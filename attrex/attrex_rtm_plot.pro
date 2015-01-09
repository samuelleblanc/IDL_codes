; program to read in the save file to plot results of the aerosol modelling for 20111105

pro attrex_rtm_plot
restore, '/home/leblanc/ATTREX/gh/20111105/20111105_model.out'

set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8.,$
  filename='/home/leblanc/ATTREX/gh/20111105/20111105_model.ps'
 device, xsize=20, ysize=40
 !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8 & !x.margin=[7,2]
 !p.multi=[0,1,2]
plot, zenlambda,topzsp,title='Solar irradiance above the layer',xtitle='Wavelength (nm)',ytitle='Irradiance (W/m!U2!N nm)',yrange=[0,0.38]
oplot,zenlambda,topz,linestyle=2
oplot,zenlambda,topnsp,color=250
oplot,zenlambda,topn,color=250,linestyle=2

legend,['Zenith','Nadir'],textcolors=[0,250],box=0,/right
legend,['Measured','Modeled'],linestyle=[0,2],box=0,/right,pspacing=1.5,position=[0.92,0.91],/normal

plot, zenlambda,botzsp,title='Solar irradiance below the layer',xtitle='Wavelength (nm)',ytitle='Irradiance (W/m!U2!N nm)',yrange=[0,0.38]
oplot,zenlambda,botz,linestyle=2
oplot,zenlambda,botnsp,color=250
oplot,zenlambda,botn,color=250,linestyle=2

legend,['Zenith','Nadir'],textcolors=[0,250],box=0,/right
legend,['Measured','Modeled'],linestyle=[0,2],box=0,/right,pspacing=1.5,position=[0.92,0.41],/normal

device,/close
spawn,'convert /home/leblanc/ATTREX/gh/20111105/20111105_model.ps /home/leblanc/ATTREX/gh/20111105/20111105_model.png'

;now plot the absorption with the ssa and tau used to model
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8.,$
  filename='/home/leblanc/ATTREX/gh/20111105/20111105_model_abs.ps'
 device, xsize=20, ysize=40
 !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8 & !x.margin=[7,7]
 !p.multi=[0,1,2]

plot, zenlambda,topzsp-topnsp, title='Absorbed irradiance',xtitle='Wavelength (nm)',ytitle='Irradiance (W/m!U2!N nm)',yrange=[0,0.021],/nodata
oplot,zenlambda,-(topzsp-topnsp)+(botzsp-botnsp),color=80
oplot,zenlambda, (topz-topn)-(botz-botn),color=250
legend,['Measured','Modeled'],textcolors=[80,250],box=0,/right

plot, zenlambda, tau, title='Modelling parameters',xtitle='Wavelength (nm)',ytitle='Optical depth',ystyle=9
axis, yaxis=1, yrange=[0.0,1.0],color=250,ytitle='Single scattering albedo',/save
oplot, zenlambda, ssa_wvl,color=250

device,/close
spawn,'convert /home/leblanc/ATTREX/gh/20111105/20111105_model_abs.ps /home/leblanc/ATTREX/gh/20111105/20111105_model_abs.png'

end

