; program that builds the look up table (lut) for spectral retrieval of transmitted radiance through cloud
; input is the various spectra, output is effective radius for liquid, effective radius for ice, optical depth, and ratio of ice optical depth (from 0 to 1)

@get_params_mix.pro
pro build_lut_mix,lbl

snow=0

dir='/argus/roof/SSFR3/model/'
if n_elements(lbl) lt 1 then label='lvls_20120524' else label=lbl

restore, dir+'sp_mix3_'+label+'.out'
sps=sp
taus=tau[0:17]
get_params, zenlambda, sps[0,0,0,*,0],partm
pars=fltarr(n_elements(taus),n_elements(refl),n_elements(refi),n_elements(wp),n_elements(partm))

;tau_hires=findgen(200)+1.
;ref_hires=findgen(50)+1.
;par_hires=fltarr(n_elements(tau_hires),n_elements(ref_hires),n_elements(wp),n_elements(partm))
;par_hitm=fltarr(n_elements(tau_hires),n_elements(ref),n_elements(wp),n_elements(partm))
;par_hirm=fltarr(n_elements(tau),n_elements(ref_hires),n_elements(wp),n_elements(partm))

for w=0, n_elements(wp)-1 do begin
  for t=0, n_elements(taus)-1  do begin
    print, w,t
    for rl=0, n_elements(refl)-1 do begin
      for ri=0, n_elements(refi)-1 do begin
        get_params, zenlambda, sps[t,rl,ri,*,w],partm
        if finite(total(partm)) ne 1 then begin
          for p=0, n_elements(partm)-1 do pars[t,rl,ri,w,p]=interpol(pars[t,rl,[ri-2,ri-1],w,p],[ri-2,ri-1],[ri])
;          print, partm
;          print, reform(pars[t,rl,ri,w,*])
          if not finite(total(pars[t,rl,ri,w,*])) then $
          message, 'not finite pars' 
        endif else pars[t,rl,ri,w,*]=partm
      endfor
    endfor
  endfor
endfor

save, taus, refi,refl,wp,pars, filename=dir+'pars_mix_'+label+'.out'
stop

tau_hires=findgen(100)+1.
refl_hires=(findgen(25)+1.)*2
refi_hires=(findgen(14)*2.5)+15.

par_hires=fltarr(n_elements(tau_hires),n_elements(refl_hires),n_elements(refi_hires),n_elements(wp),n_elements(partm))
par_htm=fltarr(n_elements(tau_hires),n_elements(refl),n_elements(refi),n_elements(wp),n_elements(partm))
par_hrl=fltarr(n_elements(tau_hires),n_elements(refl_hires),n_elements(refi),n_elements(wp),n_elements(partm))

for p=0, n_elements(partm)-1 do begin
  for w=0, n_elements(wp)-1 do begin
    print, p,w
    for ri=0, n_elements(refi)-1 do begin
      for rl=0, n_elements(refl)-1 do par_htm[*,rl,ri,w,p]=interpol(pars[*,rl,ri,w,p],taus,tau_hires,/nan)
      for t=0, n_elements(tau_hires)-1 do par_hrl[t,*,ri,w,p]=interpol(par_htm[t,*,ri,w,p],refl,refl_hires,/nan) 
    endfor
    for t=0, n_elements(tau_hires)-1 do for rl=0,n_elements(refl_hires)-1 do par_hires[t,rl,*,w,p]=interpol(par_hrl[t,rl,*,w,p],refi,refi_hires,/nan)
  endfor
endfor

save, par_hires, tau_hires, refl_hires,refi_hires, wp, filename=dir+'pars_hi_mix_'+label+'.out'
stop
end
