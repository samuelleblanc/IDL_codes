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
;   run_pdfs, pars_pdf,bins, taus, ref, wp, meas_pdf, post_tau, post_ref, post_wp, H, H_ind, H_par,tau_err,ref_err,wp_err
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
;   
; OUTPUT:
;   - retrieved products and related shannon information content in variables
;
; KEYWORDS:
;   none
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
; Modified: 
;
;--------------------------------------------------------------------------


pro run_pdfs,pars_pdf,bins,taus,ref,wp,meas_pdf,post_tau,post_ref,post_wp,H,H_ind,H_par,tau_err,ref_err,wp_err

;load the look up table pdfs
;pdf_lut

;load the measurement
;sp

;get_params, wl, sp, par
; par is the array with all retrieval parameters
bins=double(bins)
; make the pdf of measurement and prior
;meas_pdf=double(bins)*0.d
xs=fltarr(n_elements(bins))
npar=n_elements(meas_pdf[*,0])
prio_pdf  =dblarr(n_elements(taus),n_elements(ref),n_elements(wp),npar+1)+1.d0
post_jpdf =dblarr(n_elements(taus),n_elements(ref),n_elements(wp),npar,1000.)
post_pdf  =dblarr(n_elements(taus),n_elements(ref),n_elements(wp),n_elements(bins))
post      =dblarr(n_elements(taus),n_elements(ref),n_elements(wp))
post_meas =dblarr(n_elements(taus),n_elements(ref),n_elements(wp),npar)
post_ind  = post_meas
for p=0, npar-1 do begin
  for t=0, n_elements(taus)-1 do begin
    for r=0, n_elements(ref)-1 do begin
      for w=0, n_elements(wp)-1 do begin
        ; now do the multiplication
        post_jpdf[t,r,w,p,*]=meas_pdf[p,*]*pars_pdf[t,r,w,p,*]
        post_meas[t,r,w,p]=total(post_jpdf[t,r,w,p,*])
        if not finite(post_meas[t,r,w,p]) then message, 'problem with inifinite parameter post pdf value'
      endfor
    endfor
  endfor

  ; now homogenize ice and liquid
  post_meas[*,*,1,p]=post_meas[*,*,1,p]*total(post_meas[*,*,0,p])/total(post_meas[*,*,1,p])

  ; now pass the prior to the solution
  prio_pdf[*,*,*,p+1]=post_meas[*,*,*,p]*prio_pdf[*,*,*,p]
  post_ind[*,*,*,p]=post_meas[*,*,*,p]*prio_pdf[*,*,*,0]
endfor ;end par loop

post_pdf=prio_pdf[*,*,*,1:*]
for p=0, n_elements(par)-1 do begin
  post_pdf[*,*,*,p]=post_pdf[*,*,*,p]/total(post_pdf[*,*,*,p])
  post_ind[*,*,*,p]=post_ind[*,*,*,p]/total(post_ind[*,*,*,p])
endfor
post=post_pdf[*,*,*,p-1]

;now normalize the post probabilities
post=post/total(post)
n=max(post,nmax)
ns=array_indices(post, nmax)
;print, 'from total: tau:'+strtrim(taus[ns[0]],2)+' ref:'+strtrim(ref[ns[1]],2)+' wp:'+strtrim(wp[ns[2]],2)

;calculate the shanon information content
snorm=alog(n_elements(prio_pdf[*,*,*,0]))/alog(2.)
sprio=(-1.)*total(prio_pdf[*,*,*,0]/total(prio_pdf[*,*,*,0])*alog(prio_pdf[*,*,*,0]/total(prio_pdf[*,*,*,0])+1.E-42)/alog(2.))
spost=(-1.)*total(post*alog(post+1.E-42)/alog(2.))
spostm=fltarr(n_elements(par))
spriom=fltarr(n_elements(par))
sposti=fltarr(n_elements(par))
for p=0, n_elements(par)-1 do begin
  spostm[p]=(-1.)*total(post_pdf[*,*,*,p]/total(post_pdf[*,*,*,p])$
   *alog(post_pdf[*,*,*,p]/total(post_pdf[*,*,*,p])+1.E-42)/alog(2.))
  spriom[p]=(-1.)*total(prio_pdf[*,*,*,p]/total(prio_pdf[*,*,*,p])$
   *alog(prio_pdf[*,*,*,p]/total(prio_pdf[*,*,*,p])+1.E-42)/alog(2.))
  sposti[p]=(-1.)*total(post_ind[*,*,*,p]/total(post_ind[*,*,*,p])$
   *alog(post_ind[*,*,*,p]/total(post_ind[*,*,*,p])+1.E-42)/alog(2.))
endfor
H=(sprio-spost)/snorm
Hm=(spriom-spostm)/snorm
Hi=(sprio-sposti)/snorm
;print, H
;print, Hm
;print, Hi

H_par=Hm
H_ind=Hi
post_tau=taus[ns[0]]
post_ref=ref[ns[1]]
post_wp=wp[ns[2]]

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
nul=min(abs(taus - (post_tau-tstd)),nl) ;low error bound
nul=min(abs(taus - (post_tau+tstd)),nh) ;high error bound
tau_err=[taus[nl],taus[nh]]
nul=min(abs(ref - (post_ref-rstd)),nl) ;low error bound
nul=min(abs(ref - (post_ref+rstd)),nh) ;high error bound
ref_err=[ref[nl],ref[nh]]
wp_err=wstd


end
