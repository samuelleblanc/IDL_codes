; program to plot the different 

pro plot_sample_sp

dir='/argus/roof/SSFR3/model/'
restore, dir+'sp_mix3_lvls_20120524.out'


fp=dir+'sp_sample_mix'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=29, ysize=40
  !p.font=1 & !p.thick=5 & !p.charsize=4.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,2,4] & !x.margin=[6.2,1.6] & !y.margin=[0.15,0.4] & !y.omargin=[3,1] &!x.omargin=[0,6]
  w=0 & t=3 & ri=2 & rl=2
  tis=[' ',' ',' ',' ',' ',' ',' ',' ',' ']
  plot, zenlambda, sp[t,rl,ri,*,w],ytitle='Radiance (Wm!U-2!Nnm!U-1!Nsr!U-1!N)',/nodata,xr=[400,1700],xtickname=tis,xticks=4,xticklen=0.1
  for w=0, n_elements(wp)-1 do oplot, zenlambda, sp[t,rl,ri,*,w],color=w*25.
  w=0
  plot, zenlambda, sp[t,rl,ri,*,w]/max(sp[t,rl,ri,*,w]),ytitle='Normalized radiance',/nodata,xr=[400,1700],xtickname=tis,xticks=4,xticklen=0.1
  for w=0, n_elements(wp)-1 do oplot, zenlambda, sp[t,rl,ri,*,w]/max(sp[t,rl,ri,*,w]),color=w*25.
  legend,['!9t!X= '+string(tau[t],format='(I3)'),'r!De,liq!N= '+string(refl[rl],format='(F4.1)')+' !9m!Xm','r!De,ice!N= '+string(refi[ri],format='(F4.1)')+' !9m!Xm'],textcolors=[0,0,0],/right,box=0,charsize=2.2

  w=0 & t=6 & ri=2 & rl=5
  plot, zenlambda, sp[t,rl,ri,*,w],ytitle='Radiance (Wm!U-2!Nnm!U-1!Nsr!U-1!N)',/nodata,xr=[400,1700],xtickname=tis,xticks=4,xticklen=0.1
  for w=0, n_elements(wp)-1 do oplot, zenlambda, sp[t,rl,ri,*,w],color=w*25.
  w=0
  plot, zenlambda, sp[t,rl,ri,*,w]/max(sp[t,rl,ri,*,w]),ytitle='Normalized radiance',/nodata,xr=[400,1700],xtickname=tis,xticks=4,xticklen=0.1
  for w=0, n_elements(wp)-1 do oplot, zenlambda, sp[t,rl,ri,*,w]/max(sp[t,rl,ri,*,w]),color=w*25.
  legend,['!9t!X= '+string(tau[t],format='(I3)'),'r!De,liq!N= '+string(refl[rl],format='(F4.1)')+' !9m!Xm','r!De,ice!N= '+string(refi[ri],format='(F4.1)')+' !9m!Xm'],textcolors=[0,0,0],/right,box=0,charsize=2.2

  w=0 & t=6 & ri=5 & rl=7
  plot, zenlambda, sp[t,rl,ri,*,w],ytitle='Radiance (Wm!U-2!Nnm!U-1!Nsr!U-1!N)',/nodata,xr=[400,1700],xtickname=tis,xticks=4,xticklen=0.1
  for w=0, n_elements(wp)-1 do oplot, zenlambda, sp[t,rl,ri,*,w],color=w*25.
  w=0
  plot, zenlambda, sp[t,rl,ri,*,w]/max(sp[t,rl,ri,*,w]),ytitle='Normalized radiance',/nodata,xr=[400,1700],xtickname=tis, xticks=4,xticklen=0.1
  for w=0, n_elements(wp)-1 do oplot, zenlambda, sp[t,rl,ri,*,w]/max(sp[t,rl,ri,*,w]),color=w*25.
  legend,['!9t!X= '+string(tau[t],format='(I3)'),'r!De,liq!N= '+string(refl[rl],format='(F4.1)')+' !9m!Xm','r!De,ice!N= '+string(refi[ri],format='(F4.1)')+' !9m!Xm'],textcolors=[0,0,0],/right,box=0,charsize=2.2

  w=0 & t=12 & ri=5 & rl=7
  plot, zenlambda, sp[t,rl,ri,*,w],ytitle='Radiance (Wm!U-2!Nnm!U-1!Nsr!U-1!N)',/nodata,xr=[400,1700],xtitle='Wavelength (nm)',xticks=4,xticklen=0.1
  for w=0, n_elements(wp)-1 do oplot, zenlambda, sp[t,rl,ri,*,w],color=w*25.
  w=0
  plot, zenlambda, sp[t,rl,ri,*,w]/max(sp[t,rl,ri,*,w]),ytitle='Normalized radiance',/nodata,xr=[400,1700],xtitle='Wavelength (nm)',xticks=4,xticklen=0.1
  for w=0, n_elements(wp)-1 do oplot, zenlambda, sp[t,rl,ri,*,w]/max(sp[t,rl,ri,*,w]),color=w*25.
  legend,['!9t!X= '+string(tau[t],format='(I3)'),'r!De,liq!N= '+string(refl[rl],format='(F4.1)')+' !9m!Xm','r!De,ice!N= '+string(refi[ri],format='(F4.1)')+' !9m!Xm'],textcolors=[0,0,0],/right,box=0,charsize=2.2
  
  contour, transpose([[wp],[wp]]),[0,1],wp,levels=wp,/cell_fill,ystyle=9,ytickname=tis,position=[0.88,0.1,0.9,0.9],/normal,/noerase,xticks=1,xtickname=tis
  axis,yaxis=1,ytitle='Ice cloud optical thickness percentage (%)',yrange=[0,100]
 
 device, /close
 spawn, 'convert '+fp+'.ps '+fp+'.png'



stop
end
