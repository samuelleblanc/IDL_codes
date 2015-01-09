; program to plot the second parameter to illustrate its usefulness for phase discrimination
; put in the paper

pro plot_phase_parm

dir2='/home/leblanc/SSFR3/data/'
dir='/argus/roof/SSFR3/model/'

fn=dir+'pars_std.out'
print, 'restoring :'+fn
restore, fn

tau=tau_hires
ref=ref_hires

  fp=dir2+'phase_parm2'
  print, 'making plot :'+fp
  set_plot, 'ps'
  loadct, 39, /silent
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
  device, xsize=20, ysize=20 
   !p.font=1 & !p.thick=5 & !p.charsize=2.4 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
   !p.multi=0 & !x.margin=[7,3] & !y.margin=[3,1] & !p.symsize=1.5 ; charsize was 4.2

  yr=[min(avg[0:99,*,*,1]-std[0:99,*,*,1]),max(avg[0:99,*,*,1])]

  plot,tau,avg[*,0,0,1],/nodata,ytitle='Parameter 2!CDerivative of normalized radiance at 1200 nm (!9m!Xm!U-1!N)',$
   xtitle='Optical thickness',yrange=yr,xr=[0,100]
  rs=[4,14,24,39]
  cl=[50,130,210,250]
  tvlct,180,180,255,51 & tvlct,180,255,219,131 & tvlct,255,238,180,211 & tvlct,255,180,180,251
  clb=[51,131,211,251]
  for ri=0,n_elements(rs)-1 do begin
    polyfill, [tau[0:99],reverse(tau[0:99])],[avg[0:99,rs[ri],0,1]+std[0:99,rs[ri],0,1],reverse(avg[0:99,rs[ri],0,1]-std[0:99,rs[ri],0,1])],color=cl[ri]
    oplot, tau, avg[*,rs[ri],0,1],thick=3
    ;oplot, tau, avg[*,rs[ri],0,1], color=cl[ri]   
    polyfill, [tau[0:99],reverse(tau[0:99])],[avg[0:99,rs[ri],1,1]+std[0:99,rs[ri],1,1],reverse(avg[0:99,rs[ri],1,1]-std[0:99,rs[ri],1,1])],color=cl[ri]
    oplot, tau, avg[*,rs[ri],1,1], linestyle=2, thick=3
    ;oplot, tau, avg[*,rs[ri],1,1], color=cl[ri], linestyle=2
  endfor
  ;legend, ['R!Deff!N = '+string(ref[rs],format='(I2)')+' !9m!Xm','Liquid','Ice'],textcolors=[cl,0,0],box=0,/bottom,linestyle=[0,0,0,0,2],color=[255,255,255,0,0],pspacing=1.4
  
  legend, 'r!Deff!N = '+string(ref[rs],format='(I2)')+' !9m!Xm', textcolors=cl,box=0,/bottom
  legend, ['Liquid','Ice'],box=0,linestyle=[0,2],pspacing=1.4,position=[40,-4.0],/right

  device, /close
  spawn, 'convert "'+fp+'.ps" "'+fp+'.png"'



stop
end
