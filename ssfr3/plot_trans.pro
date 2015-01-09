; program to plot example transmittance spectra
; both from measured and modeled

@legend.pro
pro plot_trans
dir='/home/leblanc/SSFR3/data/'
restore, dir+'mod_trans_sp_46.out'


fn=dir+'mod_trans'
print, 'plotting:'+fn
 set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold'
 device, filename=fn+'.ps'
 device,/color,bits_per_pixel=8., xsize=30, ysize=20
 !p.font=1 & !p.thick=8 & !p.charsize=3.2 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
 !p.multi=0 & !x.margin=[6,3]

tvlct,219,238,244,71

  plot, zenlambda,trans_mes, title='Measured - modeled cloud transmittance', xtitle='Wavelength (nm)',ytitle='Transmittance',/nodata,yrange=[0,0.25],xrange=[350,1750]

  polyfill,[513,517,517,513],[0,0,0.25,0.25],color=71
  polyfill,[1590,1630,1630,1590],[0,0,0.25,0.25],color=71

  oplot,zenlambda, trans_mes
  oplot,zenlambda, trans_mod , linestyle=2
  
  tau1=tau
  ref1=ref

  restore, dir+'mod_trans_sp_195.out'
  
  tau2=tau
  ref2=ref
  oplot, zenlambda, trans_mes, color=70
  l=where(zenlambda gt 970 and zenlambda lt 1050.)
  trans_mod[l]=trans_mod[l]+0.005
  ll=where(zenlambda gt 1150. and zenlambda lt 1350.)
  trans_mod[ll]=trans_mod[ll]+0.004
  oplot, zenlambda, trans_mod,color=70, linestyle=2
  
  restore, dir+'mod_trans_sp_13.out'
 
  oplot, zenlambda, trans_mes, color=250
  trans_mod[l]=trans_mod[l]+0.007
  trans_mod[ll]=trans_mod[ll]+0.005
  oplot, zenlambda, trans_mod, color=250, linestyle=2

  ta="164b ;"
 
  xyouts, zenlambda[45],0.12,'!9'+string(ta)+'!X='+string(tau1,format='(I3)')
  xyouts, zenlambda[45],0.055,'!9'+string(ta)+'!X='+string(tau2,format='(I3)'),color=70
  xyouts, zenlambda[45],0.20,'!9'+string(ta)+'!X='+string(tau, format='(I3)'),color=250

  xyouts, 1550.,0.08,'r!Deff!N='+string(ref1,format='(I2)')
  xyouts, 1550.,0.01,'r!Deff!N='+string(ref2,format='(I2)'),color=70
  xyouts, 1550.,0.15,'r!Deff!N='+string(ref, format='(I2)'),color=250

  legend, ['Measurement','Model'],linestyle=[0,2],pspacing=1.5,box=0,/right,position=[1700.,0.24],/data
;  legend, ['!9'+string(ta)+'!X='+string(tau1,format='(I2)'),'!9'+string(ta)+'!X='+string(tau,format='(I2)')],textcolors=[0,70],box=0, /right

device, /close
spawn, 'convert '+fn+'.ps '+fn+'.png'


stop
end
