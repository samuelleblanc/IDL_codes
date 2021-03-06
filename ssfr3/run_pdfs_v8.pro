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
;           by Samuel LeBlanc, July 3rd, 2013, Boulder, Colorado
;           - changed some processing, 
;           - added a homogenization factor to account for differences between each parameter pdf
;           by Samuel LeBlanc, October 12th, 2013, Boulder Colorado
;           - changed to use new parameters methodology v1
;           - returns partial tau and ref SIC, does not return phase
;           - only takes in already phase discriminated bins, pars_pdf
;--------------------------------------------------------------------------


pro run_pdfs,pars_pdf,bins,taus,ref,wp,meas_pdf,post_tau,post_ref,post_wp,H,H_ind,H_par,tau_err,ref_err,wp_err,post,$
  ic_prob=ic_prob,li_prob=li_prob,ps=ps, post_pdf=post_pdf,h_tau, h_ref,ptot=ptot,xplot=xplot,p_likely=p_likely

if n_elements(size(pars_pdf,/dimension)) gt 4 then message, 'run_pdfs uses phase seperated lut, inpu is not phase seperated'
if n_elements(xplot) lt 1 then xplot=0

; run_pdfs,parl,binl,taul,refl,wp,meas_pdf,post_tau,post_ref,H,H_ind,H_par,terr,rerr,werr,posttm,$
;         ic_prob=icp,li_prob=lip,ps=ps,post_pdf=post_pdftm, h_tau, h_ref

if keyword_set(ps) then limp=1 else limp=0

win=1

;load the look up table pdfs
;pdf_lut

;load the measurement
;sp

; par is the array with all retrieval parameters
bins=double(bins)
; make the pdf of measurement and prior
xs=fltarr(n_elements(bins))
if limp then npar=n_elements(ps) else npar=n_elements(meas_pdf[*,0])-1
;prio_pdf  =dblarr(n_elements(taus),n_elements(ref),n_elements(wp),npar+1)+1.d0
;post      =dblarr(n_elements(taus),n_elements(ref),n_elements(wp))
;post_meas =dblarr(n_elements(taus),n_elements(ref),n_elements(wp),npar)
prio_pdf  =dblarr(n_elements(taus),n_elements(ref),npar+1)+1.d0
post      =dblarr(n_elements(taus),n_elements(ref))
post_meas =dblarr(n_elements(taus),n_elements(ref),npar)
post_ind  = post_meas
meas_jpdf = pars_pdf

if xplot then begin
  p_likely  = pars_pdf
  if win then set_plot,'win' else set_plot, 'x'
;  device, decomposed=0
;  loadct,39
;  window, 0, retain=2,xsize=1200,ysize=900
;  !p.multi=[0,4,4]
endif

for pi=0, npar-1 do begin
  p=ps[pi]
  print, p,format='(I3,".",$)' 
  if not finite(total(meas_pdf[p,*])) then message, 'problem with meas pdf values' 
  for b=0, n_elements(bins[0,*])-1 do meas_jpdf[*,*,p,b]=meas_pdf[p,b]
  
  if xplot then begin
    p_likely[*,*,pi,*]=meas_jpdf[*,*,p,*]*pars_pdf[*,*,p,*]
    post_meas[*,*,pi]=total(p_likely[*,*,p,*],4)
  endif else $
   post_meas[*,*,pi]=total(meas_jpdf[*,*,p,*]*pars_pdf[*,*,p,*],4)
;  if not finite(total(post_meas[*,*,*,pi])) then message, 'problem with post meas pdf value'
  
  if xplot and 0 then begin
    nns=where(meas_pdf[p,*] gt 0.)
    plot, bins[p,*],pars_pdf[0,0,p,*],title='Parameter :'+strtrim(p,2), yrange=[0,0.4],xstyle=0,xr=[bins[p,min(nns)],bins[p,max(nns)]]
    for t=0, n_elements(pars_pdf[*,0,0,0])-1, 20 do for r=0, n_elements(pars_pdf[0,*,0,0])-1, 10 do oplot, bins[p,*],pars_pdf[t,r,p,*]
    oplot, bins[p,*],meas_pdf[p,*],color=250
    for t=0, n_elements(pars_pdf[*,0,0,0])-1, 10 do for r=0, n_elements(pars_pdf[0,*,0,0])-1, 5 do $
      oplot, bins[p,*],p_likely[t,r,p,*], color=70
    ;for i=0,npar-1-16 do plot, findgen(10)
    ;cursor, x,y
  endif
  ; now homogenize ice and liquid
;   if total(post_meas[*,*,1,pi],/nan) gt 0. and finite(total(post_meas[*,*,1,pi],/nan)) eq 1 then $
;    post_meas[*,*,1,pi]=post_meas[*,*,1,pi]*total(post_meas[*,*,0,pi])/total(post_meas[*,*,1,pi])
endfor ;end of par loop making the likely hood function and joint-pdf (without prior)

if xplot and 0 then begin
  for i=pi, 15 do plot, findgen(10)
; cursor, x,y
stop
endif


; now homogenize all joint of each parameter to each other
;ptot=total(post_meas[*,*,0])
for pi=0, npar-1 do begin
  ;post_meas[*,*,pi]=post_meas[*,*,pi];*ptot/total(post_meas[*,*,pi])
stop
  ; now pass the prior to the solution
  prio_pdf[*,*,pi+1]=post_meas[*,*,pi]*prio_pdf[*,*,pi]
  post_ind[*,*,pi]=post_meas[*,*,pi]*prio_pdf[*,*,0]
stop
  prio_pdf[*,*,pi+1]=prio_pdf[*,*,pi+1]/total(prio_pdf[*,*,pi+1],/nan)
  post_ind[*,*,pi]=post_ind[*,*,pi]/total(post_ind[*,*,pi],/nan)
endfor ;end par loop

post_pdf=prio_pdf[*,*,1:*]
;stop
if total(post_pdf[*,*,n_elements(ps)-1],/nan) lt 1E-50 then goto, fail
;for p=0, n_elements(par)-1 do begin
;  post_pdf[*,*,*,p]=post_pdf[*,*,*,p]/total(post_pdf[*,*,*,p])
;  post_ind[*,*,*,p]=post_ind[*,*,*,p]/total(post_ind[*,*,*,p])
;endfor
post=post_pdf[*,*,pi-1]

;now normalize the post probabilities
post=post/total(post)
n=max(post,nmax)
ns=array_indices(post, nmax)

if 0 then begin ; take care of the edges
  if ns[0] ge 196 then post[nmax]=post[nmax]/2.
  if ns[1] ge 47 then post[nmax]=post[nmax]/2.
endif

;calculate the shanon information content
snorm=alog(n_elements(prio_pdf[*,*,0]))/alog(2.)
sprio=(-1.)*total(prio_pdf[*,*,0]/total(prio_pdf[*,*,0])*alog(prio_pdf[*,*,0]/total(prio_pdf[*,*,0])+1.E-42)/alog(2.))
spost=(-1.)*total(post*alog(post+1.E-42)/alog(2.))
spostm=fltarr(npar)
spriom=fltarr(npar)
sposti=fltarr(npar)
ic_prob=fltarr(2,npar)
li_prob=fltarr(2,npar)
for p=0, npar-1 do begin
  spostm[p]=(-1.)*total(post_pdf[*,*,p]/total(post_pdf[*,*,p])$
   *alog(post_pdf[*,*,p]/total(post_pdf[*,*,p])+1.E-42)/alog(2.))
  spriom[p]=(-1.)*total(prio_pdf[*,*,p]/total(prio_pdf[*,*,p])$
   *alog(prio_pdf[*,*,p]/total(prio_pdf[*,*,p])+1.E-42)/alog(2.))
  sposti[p]=(-1.)*total(post_ind[*,*,p]/total(post_ind[*,*,p])$
   *alog(post_ind[*,*,p]/total(post_ind[*,*,p])+1.E-42)/alog(2.))
  ic_prob[0,p]=total(post_ind[*,*,p]) & li_prob[0,p]=total(post_ind[*,*,p])
  ic_prob[1,p]=total(post_pdf[*,*,p]) & li_prob[1,p]=total(post_pdf[*,*,p])
endfor
H=(sprio-spost)/snorm
Hm=(spriom-spostm)/snorm
Hi=(sprio-sposti)/snorm

H_par=Hm
H_ind=Hi
post_tau=taus[ns[0]]
post_ref=ref[ns[1]]
if n_elements(ref) gt 30 then post_wp=0 else post_wp=1
;post_wp=wp[ns[2]]
;if post_ref eq 50. then stop
; now determine the effective uncertainty for tau, ref, and wp
; first set up the marginal post pdf
post_t=fltarr(n_elements(taus))
post_r=fltarr(n_elements(ref))
;post_w=fltarr(n_elements(wp))
for t=0, n_elements(taus)-1 do post_t[t]=total(post[t,*])
for r=0, n_elements(ref)-1 do post_r[r]=total(post[*,r])
;for w=0, n_elements(wp)-1 do post_w[w]=total(post[*,*])
tstd=stddev(post_t)
rstd=stddev(post_r)
;wstd=stddev(post_w)
tau_err=fltarr(2)
nt=where(post_t ge post_t[ns[0]]/100.*25.)
if min(nt) eq 0 and n_elements(nt) gt 1 then nt=nt[1:*]
tau_err[0]=min(taus[nt])
tau_err[1]=max(taus[nt])

ref_err=fltarr(2)
nr=where(post_r ge post_r[ns[1]]/100.*25.)
ref_err[0]=min(ref[nr])
ref_err[1]=max(ref[nr])
;ref_err[0]=median(re[0:inre-1],/even)
;ref_err[1]=median(re[inre:*],/even)

;ref_err=[ref[nl],ref[nh]]
;wp_err=post_w[1]
;stop

prior_t=post_t & prior_r=post_r
for t=0, n_elements(taus)-1 do prior_t[t]=total(prio_pdf[t,*,0])
for r=0, n_elements(ref)-1 do prior_r[r]=total(prio_pdf[*,r,0])
h_tau=(-1.)*total(post_t/total(post_t)*alog(post_t/total(post_t)+1.E-42)/alog(2.))
h_ref=(-1.)*total(post_r/total(post_r)*alog(post_r/total(post_r)+1.E-42)/alog(2.))
hnorm_r=alog(n_elements(prio_pdf[0,*,0]))/alog(2.)
hnorm_t=alog(n_elements(prio_pdf[*,0,0]))/alog(2.)
hprio_t=(-1.)*total(prior_t/total(prior_t)*alog(prior_t/total(prior_t)+1.E-42)/alog(2.))
hprio_r=(-1.)*total(prior_r/total(prior_r)*alog(prior_r/total(prior_r)+1.E-42)/alog(2.))
h_tau=(hprio_t-h_tau)/hnorm_t
h_ref=(hprio_r-h_ref)/hnorm_r

goto,ends
fail:
nan=!values.f_nan
post_tau=nan & post_ref=nan & post_wp=nan & tau_err=[nan,nan] & ref_err=[nan,nan] & wp_err=nan
H=nan & H_par=H_par*nan & H_ind=H_ind*nan & ic_prob=ic_prob*nan & li_prob=li_prob*nan



ends:
;stop
end
