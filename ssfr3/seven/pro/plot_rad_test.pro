; program to plot the results of a few radiance test

pro plot_rad_test

dir='/home/leblanc/SSFR3/plots/'

tau=[1,2,3,4,5,6,7,8,9,10,12.5,15]
rad=[0.24815,0.36184,0.411614, 0.42648, 0.423,0.4106,0.39417,0.3766,0.35923,0.3427,0.30633,0.27665]
rad2=[0.23174,0.33797,0.38367,0.4014,0.40011,0.39016,0.37621,0.36077,0.34518,0.33010,0.2962,0.268]
rads=[0.2268,0.3307,0.37747,0.39286,0.3916,0.38187,0.36822,0.35313,0.33786,0.32311,0.28997,0.2623]

fp=dir+'modeled_rad_515'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=20, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=0 & !x.margin=[6,4]

  plot, tau, rad, xtitle='Optical depth',ytitle='Radiance (W/m!U2!N nm sr)',title='Radiance at 515 nm, r!Deff!N=10 um',psym=-5
  oplot,tau, rad2, psym=-5, color=70
;  oplot,tau, rads, psym=-5, color=250

;  legend, ['mu 0.866','mu 0.85','mu 0.85 with kurudz'],textcolors=[0,70,250],box=0,/right
  legend, ['mu=0.866','mu=0.85'],textcolors=[0,70],box=0,/right


 device, /close
 spawn, 'convert '+fp+'.ps '+fp+'.png'

stop
end

