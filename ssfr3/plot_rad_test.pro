; program to plot the dependence of radiance on optical thickness 

pro plot_cloud_test

dir='C:\Users\Samuel\Research\SSFR3\plots\new\'
restore, 'C:\Users\Samuel\Research\SSFR3\model\sp_hires4_20120524.out'

 fp=dir+'rad_test'
 print, 'making plot :'+fp
 set_plot, 'ps'
  loadct, 39, /silent
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
  device, xsize=40, ysize=20
   !p.font=1 & !p.thick=5 & !p.charsize=2.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
   !p.multi=[0,2,1] & !x.margin=[6,2] & !y.margin=[3,3]


  plot, tau, sp[*,1,35,0],xtitle='Optical thickness',xr=[0,100],ytitle='Radiance (Wm!U-2!Nnm!U-1!Nsr!U-1!N)',title='Radiance at 515 nm',yr=[0,0.5]
  oplot,tau, sp[*,1,35,0],color=250
  oplot,tau, sp[*,9,35,0],color=250,linestyle=2

  plot, tau, sp[*,1,285,0],xtitle='Optical thickness',xr=[0,100],ytitle='Radiance (Wm!U-2!Nnm!U-1!Nsr!U-1!N)',title='Radiance at 1630.4 nm',yr=[0,0.07]
  oplot,tau, sp[*,1,285,0],color=250
  oplot,tau, sp[*,9,285,0],color=250,linestyle=2

  
  device, /close
  spawn, 'convert "'+fp+'.ps" "'+fp+'.png"'
stop
end
