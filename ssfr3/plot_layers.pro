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

fp=dir+'homogeneous'
  print, 'making plot :'+fp
  set_plot, 'ps'
  loadct, 39, /silent
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
  device, xsize=20, ysize=20
   !p.font=1 & !p.thick=5 & !p.charsize=2.2 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
   !p.multi=0 & !x.margin=[3,1] & !y.margin=[3,3] & !p.symsize=1.5
  
  


  device, /close
  spawn, 'convert '+fp+'.ps '+fp+'.png'
stop
end
