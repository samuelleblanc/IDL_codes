; program to plot spectra of mixed phase clouds for a few different types of layers


pro plot_layers

dirin='/argus/roof/SSFR3/model/'
restore, dirin+'sp_mix2_v2.out'
sp_nml=sp
restore, dirin+'sp_mix2_lay_v2.out'
sp_lay=sp
restore, dirin+'sp_mix2_lvls_v2.out'
sp_lvl=sp


dir='~/mixed_phase/plots/'

fp=dir+'mix2_layers'
  print, 'making plot :'+fp
  set_plot, 'ps'
  loadct, 39, /silent
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
  device, xsize=40, ysize=20
   !p.font=1 & !p.thick=5 & !p.charsize=3.2 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
   !p.multi=[0,3,1] & !x.margin=[3,1] & !y.margin=[3,3] & !p.symsize=1.5
   
  plot, zenlambda, sp_nml[1,0,0,*,2],title='tau=5,ref_liq=5um,ref_ice=15um,wp=50%',xtitle='Wavelength (nm)',ytitle='Radiance (Wm!U-2!Nnm!U-1!Nsr!U-1!N)'
  oplot,zenlambda, sp_lay[1,0,0,*,2],color=70
  oplot,zenlambda, sp_lvl[1,0,0,*,2],color=250

  plot, zenlambda, sp_nml[3,1,2,*,3],title='tau=20,ref_liq=15um,ref_ice=50um,wp=75%',xtitle='Wavelength (nm)',ytitle='Radiance (Wm!U-2!Nnm!U-1!Nsr!U-1!N)'
  oplot,zenlambda, sp_lay[3,1,2,*,3],color=70
  oplot,zenlambda, sp_lvl[3,1,2,*,3],color=250

  plot, zenlambda, sp_nml[3,1,2,*,3]/max(sp_nml[3,1,2,*,3]),title='tau=20,ref_liq=15um,ref_ice=50um,wp=75%',xtitle='Wavelength (nm)',ytitle='Normalized radiance'
  oplot,zenlambda, sp_lay[3,1,2,*,3]/max(sp_lay[3,1,2,*,3]),color=70
  oplot,zenlambda, sp_lvl[3,1,2,*,3]/max(sp_lvl[3,1,2,*,3]),color=250

  device, /close
  spawn, 'convert '+fp+'.ps '+fp+'.png'
stop
end
