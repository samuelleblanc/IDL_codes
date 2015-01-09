; program that builds the look up table (lut) for spectral retrieval of transmitted radiance through cloud
; input is the various spectra, output is effective radius for liquid, effective radius for ice, optical depth, and ratio of ice optical depth (from 0 to 1)

@get_params_mix.pro
pro build_lut_mix_sza,lbl

snow=0

dir='/argus/roof/SSFR3/model/'
if n_elements(lbl) lt 1 then label='20120524' else label=lbl

restore, dir+'mix3_sza_'+label+'.out'
taus=tau[0:17]

tau_hires=findgen(100)+1.
refl_hires=(findgen(25)+1.)*2
refi_hires=(findgen(14)*2.5)+15.

rad_hi=fltarr(n_elements(tau_hires),n_elements(refl_hires),n_elements(refi_hires),n_elements(wp),n_elements(mus))
rad_ht=fltarr(n_elements(tau_hires),n_elements(refl),n_elements(refi),n_elements(wp),n_elements(mus))
rad_hr=fltarr(n_elements(tau_hires),n_elements(refl_hires),n_elements(refi),n_elements(wp),n_elements(mus))

for p=0, n_elements(mus)-1 do begin
  for w=0, n_elements(wp)-1 do begin
    print, p,w
    for ri=0, n_elements(refi)-1 do begin
      for rl=0, n_elements(refl)-1 do rad_ht[*,rl,ri,w,p]=interpol(rad515[*,rl,ri,w,p],tau,tau_hires,/nan)
      for t=0, n_elements(tau_hires)-1 do rad_hr[t,*,ri,w,p]=interpol(rad_ht[t,*,ri,w,p],refl,refl_hires,/nan) 
    endfor
    for t=0, n_elements(tau_hires)-1 do for rl=0,n_elements(refl_hires)-1 do rad_hi[t,rl,*,w,p]=interpol(rad_hr[t,rl,*,w,p],refi,refi_hires,/nan)
  endfor
endfor

save, rad_hi, tau_hires, refl_hires,refi_hires, wp, mus,filename=dir+'mix_hi_sza_'+label+'.out'
stop
end
