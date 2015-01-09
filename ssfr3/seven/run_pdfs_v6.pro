;+
; NAME:
;   run_pdfs
;
; PURPOSE:
;   program to run through the bayes theorem by multiplying the different pdfs
;   uses the pdf lut for the various parameters
;   Takes in a measurement pdf derived from measured downwelling radiance spectra of clouds
;
;   post_pdf=prior_pdf*meas_pdf*model_pdf
;
;    runs the program for a single spectra
;    is used as a part of the retrieve_pdfs program
;    the input measurement pdf is normalized and calculated for the appropriate uncertainty
;
; CATEGORY:
;   Cloud Retrieval / Radiance /GENRA
;
; CALLING SEQUENCE:
;   run_pdfs, pars_pdf,bins, taus, ref, wp, meas_pdf, post_tau, post_ref, post_wp, H, H_ind, H_par,tau_err,ref_err,wp_err,post
;     - pars_pdf: the look up table in pdf format (tau,ref,wp,parameter,bins) - normalized input
;     - bins:     the bin value for each element in the look up table in pdf format and measurement pdf -input
;     - taus:     the array of taus for the post pdf - input
;     - ref:      the array of ref for the post pdf - input
;     - wp:       the array of water phase (0 for water and 1 for 100% ice) - input
;     - meas_pdf: the array of the measurement pdf of parameters in bin space - normalized input
;     - post_tau: the value of the retrieved tau - output
;     - post_ref: the value of the retrieved ref - output
;     - post_wp:  the value of the water phase - output
;     - H:        the value of the total normalized shannon information content
;     - H_ind:    an array of normalized shannon information content for each parameter individually, with non updating prior
;     - H_par:    an array of normalized shannon information content for each parameter, with updating prior
;     - tau_err:  an array with low and high bounds of uncertainty for the retrieved tau
;     - ref_err:  an array with low and high bounds of uncertainty for the retrieved ref
;     - wp_err:   a value of uncertainty of the retrieved wp
;     - post:     the posterior pdf in parameter space
;
; OUTPUT:
;   - retrieved products and related shannon information content in variables
;
; KEYWORDS:
;   - ic_prob:   array of probabilities for ice cloud [2,npar] 0: independent calculated, 1: updated prior
;   - li_prob:   array of probabilities for liquid cloud [2,npar] 0: independent calculated, 1: updated prior
;   - ps:        array of indices of the subset of parameter to use 
;
; DEPENDENCIES:
;   none
;   
; NEEDED FILES:
;   none
;  
; EXAMPLE:
;   run_pdfs,pars_pdf,bins, taus, ref, wp, meas_pdf,ta,re,w,H,Hi,Hm
;
; MODIFICATION HISTORY:
; Written:  Samuel LeBlanc, Monday April 22nd, 2013, Boulder, Colorado, Snow day
; Modified: by Samuel LeBlanc, Tuesday May 14th, 2013, Boulder, Colorado, hot day
;           - added keyword wp_prob that returns the probability of phase for each parameter
;             independently calculated and with the updated prior
;           by Samuel LeBlanc, Happy Thursday, May 16th, 2013, Boulder, Colorado
;           - added the ps keyword which holds the array of indices of parameters to run over
;             instead of the entire range.
;--------------------------------------------------------------------------


pro run_pdfs,pars_pdf,bins,taus,ref,wp,meas_pdf,post_tau,post_ref,post_wp,H,H_ind,H_par,tau_err,ref_err,wp_err,post,$
  ic_prob=ic_prob,li_prob=li_prob,ps=ps

if keyword_set(ps) then limp=1 else limp=0

;load the look up table pdfs
;pdf_lut

;load the measurement
;sp

; par is the array with all retrieval parameters
bins=double(bins)
; make the pdf of measurement and prior
xs=fltarr(n_elements(bins))
if limp then npar=n_elements(ps) else npar=n_elements(meas_pdf[*,0])
prio_pdf  =dblarr(n_elements(taus),n_elements(ref),n_elements(wp),npar+1)+1.d0
post      =dblarr(n_elements(taus),n_elements(ref),n_elements(wp))
post_meas =dblarr(n_elements(taus),n_elements(ref),n_elements(wp),npar)
post_ind  = post_meas
meas_jpdf = pars_pdf

for pi=0, npar-1 do begin
  p=ps[pi]
  print, p,format='(I3,".",$)' 
  for b=0, n_elements(bins[0,*])-1 do meas_jpdf[*,*,*,p,b]=meas_pdf[p,b]
  post_meas[*,*,*,pi]=total(meas_jpdf[*,*,*,p,*]*pars_pdf[*,*,*,p,*],5)
  if not finite(total(post_meas[*,*,*,pi])) then message, 'problem with post meas pdf value'

  ; now homogenize ice and liquid
   if total(post_meas[*,*,1,pi],/nan) gt 0. and finite(total(post_meas[*,*,1,pi],/nan)) eq 1 then $
    post_meas[*,*,1,pi]=post_meas[*,*,1,pi]*total(post_meas[*,*,0,pi])/total(post_meas[*,*,1,pi])

  ; now pass the prior to the solution
  prio_pdf[*,*,*,pi+1]=post_meas[*,*,*,pi]*prio_pdf[*,*,*,pi]
  post_ind[*,*,*,pi]=post_meas[*,*,*,pi]*prio_pdf[*,*,*,0]
endfor ;end par loop

post_pdf=prio_pdf[*,*,*,1:*]
if total(post_pdf[*,*,*,n_elements(ps)-1],/nan) lt 1E-25 then goto, fail
for p=0, n_elements(par)-1 do begin
  post_pdf[*,*,*,p]=post_pdf[*,*,*,p]/total(post_pdf[*,*,*,p])
  post_ind[*,*,*,p]=post_ind[*,*,*,p]/total(post_ind[*,*,*,p])
endfor
post=post_pdf[*,*,*,p-1]

;now normalize the post probabilities
post=post/total(post)
n=max(post,nmax)
ns=array_indices(post, nmax)

;calculate the shanon information content
snorm=alog(n_elements(prio_pdf[*,*,*,0]))/alog(2.)
sprio=(-1.)*total(prio_pdf[*,*,*,0]/total(prio_pdf[*,*,*,0])*alog(prio_pdf[*,*,*,0]/total(prio_pdf[*,*,*,0])+1.E-42)/alog(2.))
spost=(-1.)*total(post*alog(post+1.E-42)/alog(2.))
spostm=fltarr(npar)
spriom=fltarr(npar)
sposti=fltarr(npar)
ic_prob=fltarr(2,npar)
li_prob=fltarr(2,npar)
for p=0, npar-1 do begin
  spostm[p]=(-1.)*total(post_pdf[*,*,*,p]/total(post_pdf[*,*,*,p])$
   *alog(post_pdf[*,*,*,p]/total(post_pdf[*,*,*,p])+1.E-42)/alog(2.))
  spriom[p]=(-1.)*total(prio_pdf[*,*,*,p]/total(prio_pdf[*,*,*,p])$
   *alog(prio_pdf[*,*,*,p]/total(prio_pdf[*,*,*,p])+1.E-42)/alog(2.))
  sposti[p]=(-1.)*total(post_ind[*,*,*,p]/total(post_ind[*,*,*,p])$
   *alog(post_ind[*,*,*,p]/total(post_ind[*,*,*,p])+1.E-42)/alog(2.))
  ic_prob[0,p]=total(post_ind[*,*,1,p])/total(post_ind[*,*,*,p]) & li_prob[0,p]=total(post_ind[*,*,0,p])/total(post_ind[*,*,*,p])
  ic_prob[1,p]=total(post_pdf[*,*,1,p])/total(post_pdf[*,*,*,p]) & li_prob[1,p]=total(post_pdf[*,*,0,p])/total(post_pdf[*,*,*,p])
endfor
H=(sprio-spost)/snorm
Hm=(spriom-spostm)/snorm
Hi=(sprio-sposti)/snorm

H_par=Hm
H_ind=Hi
post_tau=taus[ns[0]]
post_ref=ref[ns[1]]
post_wp=wp[ns[2]]
;if post_ref eq 50. then stop
; now determine the effective uncertainty for tau, ref, and wp
; first set up the marginal post pdf
post_t=fltarr(n_elements(taus))
post_r=fltarr(n_elements(ref))
post_w=fltarr(n_elements(wp))
for t=0, n_elements(taus)-1 do post_t[t]=total(post[t,*,*])
for r=0, n_elements(ref)-1 do post_r[r]=total(post[*,r,*])
for w=0, n_elements(wp)-1 do post_w[w]=total(post[*,*,w])
tstd=stddev(post_t)
rstd=stddev(post_r)
wstd=stddev(post_w)
;ta=post_t[sort(post_t)]
;inta=n_elements(ta)/2
tau_err=fltarr(2)
nt=where(post_t ge post_t[ns[0]]/100.*25.)
if min(nt) eq 0 and n_elements(nt) gt 1 then nt=nt[1:*]
tau_err[0]=min(taus[nt])
tau_err[1]=max(taus[nt])
;tau_err[0]=median(ta[0:inta-1],/even)
;tau_err[1]=median(ta[inta:*],/even)  

;re=post_r[sort(post_r)]
;inre=n_elements(re)/2
ref_err=fltarr(2)
nr=where(post_r ge post_r[ns[1]]/100.*25.)
ref_err[0]=min(ref[nr])
ref_err[1]=max(ref[nr])
;ref_err[0]=median(re[0:inre-1],/even)
;ref_err[1]=median(re[inre:*],/even)

;ref_err=[ref[nl],ref[nh]]
wp_err=post_w[1]
;stop

goto,ends
fail:
nan=!values.f_nan
post_tau=nan & post_ref=nan & post_wp=nan & tau_err=[nan,nan] & ref_err=[nan,nan] & wp_err=nan
H=nan & H_par=H_par*nan & H_ind=H_ind*nan & ic_prob=ic_prob*nan & li_prob=li_prob*nan



ends:
;stop
end
