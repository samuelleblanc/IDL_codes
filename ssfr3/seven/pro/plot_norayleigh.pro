; program to plot two radiances with and without rayleigh scattering

pro plot_norayleigh
dir='/argus/roof/SSFR3/model/'
restore, dir+'no_rayleigh.out'


dir='/home/leblanc/SSFR3/plots/new/'
fp=dir+'no_ray'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=40, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=3.2 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,2,1] & !x.margin=[6,1] & !y.margin=[3,1] & !y.omargin=[0,0] & !x.omargin=[0,0]

  plot, wvl, noradt3,/nodata,xtitle='Wavelength (nm)',ytitle='Radiance (Wm!U-2!Nnm!U-1!Nsr!U-1!N)',xr=[400,1700]
  oplot,wvl, rad, color=70
  oplot,wvl,norad,color=70,linestyle=2
  oplot,wvl, radt3,color=250
  oplot,wvl,noradt3,color=250,linestyle=2

  plot, wvl, noradt3/max(noradt3),/nodata,xtitle='Wavelength (nm)',ytitle='Normalized Radiance',xr=[400,1700]
  oplot,wvl, rad/max(rad), color=70
  oplot,wvl,norad/max(norad),color=70,linestyle=2
  oplot,wvl, radt3/max(radt3),color=250
  oplot,wvl,noradt3/max(noradt3),color=250,linestyle=2

  legend, ['!9t!X=1, r!De!N=22.5 !9m!Xm','!9t!X=3, r!De!N=22.5 !9m!Xm','Without Rayleigh scattering'],$
   textcolors=[70,250,0],color=[255,255,0],linestyle=[0,0,2],box=0,/right,pspacing=1.4,charsize=2

device, /close
spawn, 'convert '+fp+'.ps '+fp+'.png'


stop
end
