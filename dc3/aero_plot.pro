;program to plot nicely results from aero_rad

pro aero_plot

restore, '/data/seven/DC3/SSFR3/20120621/results_utc_18.00.dat'
restore, '/data/seven/DC3/SSFR3/20120621/lambda.dat'
set_plot, 'ps'
loadct,39, /silent

    device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename='/data/seven/DC3/SSFR3/20120621/results_utc_18.00.ps'
    device, xsize=30, ysize=30
    !p.font=1 & !p.thick=5
    !p.charsize=2.0 & !x.style=1
    !y.style=1 & !z.style=1
    !y.thick=1.8 & !x.thick=1.8 

!p.multi=[0,2,2]
!y.style=1 & !x.style=1
plot, lambda, aod_wvl, title='Aerosol optical depth', xrange=[350.,1050.],xtitle='Wavelength (nm)',ytitle='Optical depth',yrange=[0.,0.1]
plot, lambda, asy_wvl, title='Aerosol spectral properties',xrange=[350.,1050.],xtitle='Wavelength (nm)', yrange=[0.4,1]
oplot, lambda, ssa_wvl, color=250
legend, ['Asymmetry parameter','Single scattering albedo'],textcolors=[0,250],box=0,/bottom,/right
plot, lambda, irr, title='Irradiance',xrange=[350.,1050.],xtitle='Wavelength (nm)',ytitle='Irradiance (W/m!U2!N nm)', yrange=[0,1.8]
oplot, lambda, irr_wvl, color=170
legend, ['Measured','Modeled'],textcolors=[0,170],box=0,/right
plot, lambda, rad, title='Zenith Radiance',xrange=[350.,1050.],xtitle='Wavelength (nm)',ytitle='Radiance (W/m!U2!N nm sr)', yrange=[0,0.1]
oplot, lambda, rad_wvl, color=170
legend, ['Measured','Modeled'],textcolors=[0,170],box=0,/right
device,/close
spawn, 'convert /data/seven/DC3/SSFR3/20120621/results_utc_18.00.ps /data/seven/DC3/SSFR3/20120621/results_utc_18.00.png'

end