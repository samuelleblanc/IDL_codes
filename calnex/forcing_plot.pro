pro forcing_plot
dir='/home/leblanc/CALNEX/p3/20100519/'
restore, dir+'20100519_forcing.out'
;rad_up,no_rad_up,rad_dn, no_rad_dn, aot, wvl,forcing, eff,effp,

wvl=[340,380,440,500,675,870,1020] 

set_plot, 'ps'
  loadct, 39,/silent
  device, /encapsulated
   device, /tt_font, set_font='Helvetica Bold'
   device, filename=dir+'20100519_forcing.ps'
   device,/color,bits_per_pixel=8.
   device, xsize=40, ysize=60
   !p.font=1 & !p.thick=5
   !p.charsize=2.5 & !x.style=1
   !y.style=1 & !z.style=1
   !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,2,3]
  
  plot, wvl, forcing[*,0,0], title='Bottom of layer Forcing',ytitle='Forcing (W/m!U 2!N)',xtitle='Wavelength (nm)', yrange=[min(forcing[*,*,0]*0.9,/nan),max(forcing[*,*,0]*1.1,/nan)]
  for i=1, n_elements(forcing[0,*,0])-1 do oplot, wvl, forcing[*,i,0]
  
  plot, wvl, forcing[*,0,1], title='Top of layer Forcing',ytitle='Forcing (W/m!U 2!N)',xtitle='Wavelength (nm)', yrange=[min(forcing[*,*,1]*0.9,/nan),max(forcing[*,*,1]*1.1,/nan)]
  for i=1, n_elements(forcing[0,*,1])-1 do oplot, wvl, forcing[*,i,1]
  
  plot, wvl, eff[*,0,0], title='Bottom of layer Forcing',ytitle='Forcing Efficiency (W/m!U 2!N)',xtitle='Wavelength (nm)', yrange=[min(eff[*,*,0]*0.9,/nan),max(eff[*,*,0]*1.1,/nan)]
  for i=1, n_elements(eff[0,*,0])-1 do oplot, wvl, eff[*,i,0]
  
  plot, wvl, eff[*,0,1], title='Top of layer Forcing',ytitle='Forcing Efficiency (W/m!U 2!N)',xtitle='Wavelength (nm)', yrange=[min(eff[*,*,1]*0.9,/nan),max(eff[*,*,1]*1.1,/nan)]
  for i=1, n_elements(eff[0,*,1])-1 do oplot, wvl, eff[*,i,1]
  
  plot, wvl, effp[*,0,0], title='Bottom of layer Forcing',ytitle='Relative Forcing Efficiency(%)',xtitle='Wavelength (nm)', yrange=[min(effp[*,*,0]*0.9,/nan),max(effp[*,*,0]*1.1,/nan)]
  for i=1, n_elements(effp[0,*,0])-1 do oplot, wvl, effp[*,i,0]
  
  plot, wvl, effp[*,0,1], title='Top of layer Forcing',ytitle='Relative Forcing Efficiency(%)',xtitle='Wavelength (nm)', yrange=[min(effp[*,*,1]*0.9,/nan),max(effp[*,*,1]*1.1,/nan)]
  for i=1, n_elements(effp[0,*,1])-1 do oplot, wvl, effp[*,i,1]
  
device, /close
spawn, 'convert '+dir+'20100519_forcing.ps '+dir+'20100519_forcing.png'
spawn, 'rm -f '+dir+'20100519_forcing.ps'
stop
end