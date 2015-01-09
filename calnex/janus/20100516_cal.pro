@/home/leblanc/libradtran/pro/libradtran_reader.pro
pro cal_20100516

print, 'restoring'
;restore, '/home/leblanc/libradtran/output/cloud/cloud.sav'
data=libradtran_reader(file='/home/leblanc/libradtran/output/cloud/cloud.out',zout=[0,0.133,0.96],/radiance,/quiet)

restore, '/home/leblanc/CALNEX/p3/20100516/20100516_calibspcs_attcorr_post.out'
p3_utc=tmhrs & p3_zen=zspectra & p3_nad=nspectra  & p3_zl=zenlambda & p3_nl=nadlambda
restore, '/data/seven/schmidt/calnex/ship/20100516/20100516_calibspcs.out'
dir='/home/leblanc/CALNEX/clouds/'
restore, '/data/seven/schmidt/calnex/p3/20100516/20100516_SP.out

m=min(abs(19.498889-p3_utc),p31)
p3=where(p3_utc gt 19.31 and p3_utc lt 19.54)
;m=min(abs(18.96-p3_utc),p3b)
p3b=3908
m=min(abs(19.45-tmhrs),ship)
mu=1./cos(sza*!dtor)
c=1.0;0.5E-3;c=7.60172E-4 ;1.875595E-3

print, 'plotting'
set_plot, 'ps'
loadct, 39,/silent
!p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
!p.multi=0
device, /encapsulated
device, /tt_font, set_font='Helvetica Bold'
device, filename=dir+'20100516_irr_mod.ps'
device,/color,bits_per_pixel=8.
device, xsize=30, ysize=20
tvlct, 200,200,200,200
; the above cloud spectra, both upwelling and downwelling
plot, p3_zl, p3_zen[*,p31], title='Downwelling Irradiance Spectra Post-Calibration', xtitle='Wavelength (nm)', ytitle='Irradiance (W/m!E2!N)', yrange=[0,2.2]
for i=0, n_elements(p3)-1 do oplot, p3_zl, p3_zen[*,p3[i]]*mu[p3[i]]/mu[p31], color=200
oplot, p3_zl, p3_zen[*,p31]
oplot, data.wvl[2,*],data.dif_dn[2,*]+data.dir_dn[2,*], color=150
legend, ['Model', 'Measured at Model point', 'Measured values near Model Point'],textcolors=[150,0,200], /right
device,/close
spawn, 'convert '+dir+'20100516_irr_mod.ps '+dir+'20100516_cal_check_post.png '
spawn, 'rm -f '+dir+'20100516_irr_mod.ps '
stop
end






