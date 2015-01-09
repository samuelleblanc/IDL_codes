; program that builds the look up table (lut) for spectral retrieval of transmitted radiance through cloud
; input is the various spectra, output is effective radius, optical depth, and number 0 to 1 
; for portion of liquid to ice water content

;@get_params_2wvl.pro
;@get_params_test.pro
;@get_params.pro
;@get_params2.pro
;@get_params3.pro
@get_params4.pro
pro build_lut_re ;,date

vv='v1'
win=1

lbl=vv+'_20120525'+['','_dref']


n=n_elements(lbl)
;abb=['2012-05-24','2012-08-04','2012-08-12','2012-08-20','2012-09-13']
re=['Constant','Proportional to z']

for i=0, n-1 do begin
  print, 'restoring:'+lbl[i]
  ;if win then restore, 'C:\Users\Samuel\Research\SSFR3\model\hires6\sp_hires6_'+lbl[i]+'.out' else $
  if win then restore, 'C:\Users\Samuel\Research\SSFR3\model\v1\sp_'+lbl[i]+'.out' else $
    restore, '/argus/roof/SSFR3/model/sp_'+lbl[i]+'.out'

  if i eq 0 then $
    spa=fltarr(n_elements(tau),n_elements(ref),n_elements(zenlambda),2,n)
  spa[*,*,*,*,i]=sp
endfor
print, 'got dref grid of:', re

;now interpol to finer sp and pars grid

if 1 then begin
  tau_hires=findgen(100)+1.
  ref_hires=findgen(60)+1.
  sp_hit=fltarr(n_elements(tau_hires),n_elements(ref),2,n_elements(zenlambda))
  sp_hi=fltarr(n_elements(tau_hires),n_elements(ref_hires),2,n_elements(zenlambda))
  sp_hiu=fltarr(n_elements(tau_hires),n_elements(ref_hires),2,n_elements(zenlambda),n)
  pars=fltarr(n_elements(tau_hires),n_elements(ref_hires),2,16)
  par_hires=fltarr(n_elements(tau_hires),n_elements(ref_hires),2,16,n)
print, 'interpolating the spectra, then calculating paramters'
for u=0, n-1 do begin
print, 'on re of:', re[u]
  for z=0,n_elements(zenlambda)-1 do $
    for w=0,1 do $
      for r=0, n_elements(ref)-1 do sp_hit[*,r,w,z]=interpol(spa[*,r,z,w,u],tau,tau_hires,/nan)
      ;for t=0, n_elements(tau)-1 do sp_hi[t,*,w,z]=interpol(sp_hit[t,*,w,z],ref,ref_hires,/nan)
   for z=0,n_elements(zenlambda)-1 do $ 
    for w=0,1 do $
      for t=0, n_elements(tau_hires)-1 do sp_hi[t,*,w,z]=interpol(sp_hit[t,*,w,z],ref,ref_hires,/nan)
  for w=0,1 do begin
      for r=0, n_elements(ref_hires)-1 do begin
        for t=0, n_elements(tau_hires)-1 do begin
          get_params,zenlambda,sp_hi[t,r,w,*],partm
          pars[t,r,w,*]=partm
        endfor
      endfor
  endfor
par_hires[*,*,*,*,u]=pars
sp_hiu[*,*,*,*,u]=sp_hi
endfor
wp=[0,1] 
 
pars=par_hires
taus=tau_hires
refs=ref_hires
date='20120525'

if win then fn='C:\Users\Samuel\Research\SSFR3\data\par_re_'+vv+'_'+date+'.out'
print, 'saving to:'+fn
save, pars,taus,refs,wp,re,filename=fn
save, pars,taus,refs,wp,re,sp_hiu,zenlambda,filename='C:\Users\Samuel\Research\SSFR3\data\sp_par_re_'+vv+'_'+date+'.out'
endif 

stop
end
