; program to get the singal to noise ratio of the modeled spectra

@make_meas_pdf.pro
@get_params.pro

pro get_signal_noise
win=1
dw='C:\Users\Samuel\Research\SSFR3\'

;restore files to use with make_meas_pdf
print, 'restoring files for make_meas_pdf'

if win then restore, dw+'data\dark_sample.out' else $
 restore, '/home/leblanc/SSFR3/data/dark_sample.out'

dark_s={z_dk:z_dk,zenlambda:zenlambda}

if win then restore, dw+'data\resps.out' else $
 restore, '/home/leblanc/SSFR3/data/resps.out'
resps={zenlambda:zenlambda,zresp1:zresp1,zresp2:zresp2,zresp3:zresp3,zresp4:zresp4}

print, 'restoring LUT v3'
if win then restore, dw+'data\Pars_LUT_20120524.out' else $
 restore, '/argus/roof/SSFR3/model/Pars_pdf_LUT_v3.out'
	
if win then begin
 bins=fltarr(16,1000)

 for p=0, 14 do begin
  pmin=min(pars[*,*,*,p])
  pmax=max(pars[*,*,*,p])
  dp=(pmax-pmin)/550.
  if p eq 15 then dp=0.01
  bins[p,*]=findgen(1000)*dp+pmin-(dp*225.) ;make the bins larger than the extent of the modeled parameter by 22.5% on either sid
 endfor
endif


  print, 'restoring modeled out file'
if win then restore, dw+'model\hires5\sp_hires5_20120524.out' else $
  restore, '/argus/roof/SSFR3/model/sp_hires4_20120524.out'

ntau=n_elements(tau)
nref=n_elements(ref)
nw=2
np=16

  std=fltarr(ntau,nref,nw,np)
  par_arr=fltarr(ntau,nref,nw,np)

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

if win then save, std, par_arr,tau,ref,wp,filename=dw+'data\meas_std.out' else $ 
 save, std, par_arr,tau,ref,wp,filename='~/SSFR3/data/meas_std.out'

stop
end
