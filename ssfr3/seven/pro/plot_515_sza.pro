;program to plot the sza dependence of radiance at 515

pro plot_515_sza

dir='/argus/roof/SSFR3/model/'
restore, dir+'sza_mix_std.out'

fp=dir+'sza_515'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=20, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=1.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=0 & !x.margin=[7,1.5] & !y.margin=[0.155555,0.4] & !y.omargin=[3,2]

plot, refl, rad[9,*,0,0,16],psym=-2, ytitle='Radiance (Wm!U-2!Nnm!U-1!Nsr!U-1!N)',xtitle='Effective radius (!9m!Xm)',title='Radiance at 515.7 nm, !9t!X=10',yr=[0.2,0.7],xr=[0,50]
oplot,refl, rad[9,*,0,0,17],psym=-2,color=250

restore, dir+'sp_hires4_20120524.out'
oplot,ref, sp[6,*,35,0],color=70,psym=-2


legend, ['mu=0.85','mu=0.9','mu=0.866'],textcolors=[0,250,70],box=0,/right

device, /close
spawn, 'convert '+fp+'.ps '+fp+'.png'


stop
end
