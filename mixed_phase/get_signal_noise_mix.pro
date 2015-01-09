; program to get the singal to noise ratio of the modeled spectra

@make_meas_pdf_mix.pro
@get_params_mix.pro

pro get_signal_noise_mix

;restore files to use with make_meas_pdf
print, 'restoring files for make_meas_pdf'
restore, '/home/leblanc/SSFR3/data/dark_sample.out'
dark_s={z_dk:z_dk,zenlambda:zenlambda}
restore, '/home/leblanc/SSFR3/data/resps.out'
resps={zenlambda:zenlambda,zresp1:zresp1,zresp2:zresp2,zresp3:zresp3,zresp4:zresp4}

fn='/argus/roof/SSFR3/model/pars_pdf_mix.out'
print, 'restoring :'+fn
restore, fn
	

  print, 'restoring modeled out file'
  restore, '/argus/roof/SSFR3/model/sp_mix3_lvls_20120524.out'

  std=std*0.
  par_arr=std*0.

  wvl=zenlambda
  ;wp=[0.,1.0]
  for w=0, n_elements(wp)-1 do begin
    for t=0, n_elements(tau)-1 do begin
      for rl=0, n_elements(refl)-1 do begin
        for ri=0, n_elements(refi)-1 do begin
         get_params, wvl, reform(sp[t,rl,ri,*,w]), par
         par_arr[t,rl,ri,w,*]=par
         make_meas_pdf, reform(sp[t,rl,ri,*,w]), wvl, bins, meas_pdf, 0, std=st,dark_s=dark_s,resps=resps
         std[t,rl,ri,w,*]=st
        endfor ;refi loop
        print, t,w,rl
      endfor ;refl loop
    endfor ;tau loop
  endfor ;wp loop

save, std, par_arr,tau,refl,refi,wp,filename='~/mixed_phase/data/meas_std_mix.out'

stop
end
