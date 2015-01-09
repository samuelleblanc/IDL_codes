; program to get the singal to noise ratio of the modeled spectra

@make_meas_pdf.pro
@get_params.pro

pro get_signal_noise

;restore files to use with make_meas_pdf
print, 'restoring files for make_meas_pdf'
restore, '/home/leblanc/SSFR3/data/dark_sample.out'
dark_s={z_dk:z_dk,zenlambda:zenlambda}
restore, '/home/leblanc/SSFR3/data/resps.out'
resps={zenlambda:zenlambda,zresp1:zresp1,zresp2:zresp2,zresp3:zresp3,zresp4:zresp4}

print, 'restoring LUT v3'
restore, '/argus/roof/SSFR3/model/Pars_pdf_LUT_v3.out'
	

  print, 'restoring modeled out file'
  restore, '/argus/roof/SSFR3/model/sp_hires4_20120524.out'

  std=fltarr(24,15,2,16)
  par_arr=fltarr(24,15,2,16)

  wvl=zenlambda
  wp=[0.,1.0]
  for w=0, n_elements(wp)-1 do begin
    for t=0, n_elements(tau)-1 do begin
      for r=0, n_elements(ref)-1 do begin
         get_params, wvl, reform(sp[t,r,*,w]), par
         par_arr[t,r,w,*]=par
         make_meas_pdf, reform(sp[t,r,*,w]), wvl, bins, meas_pdf, 0, std=st,dark_s=dark_s,resps=resps
         std[t,r,w,*]=st
        print, w,t,r
      endfor ; ref loop
    endfor ;tau loop
  endfor ; wp loop

save, std, par_arr,tau,ref,wp,filename='~/SSFR3/data/meas_std.out'

stop
end
