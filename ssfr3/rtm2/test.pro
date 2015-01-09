pro test

 !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8 & !p.multi=0 & !x.margin=[7,2]

tmhrs_big=findgen(200)
fl=indgen(200)

cod_ice=randomu(seed,200,/normal)*100.
ref_ice=randomu(seed,200,/normal)*250.

cod_liquid=randomu(seed,200,/normal)*100.
ref_liquid=randomu(seed,200,/normal)*100.
res_ice=randomu(seed,200,/normal)
res_liquid=randomu(seed,200,/normal)

dirout='/home/leblanc/ATTREX/'

print, 'plotting cod and ref time series'
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=dirout+'_rt.ps'
 device, xsize=25, ysize=20
 !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8 & !p.multi=0
 plot, tmhrs_big[fl],cod_ice[fl],title='Time series of retrieved properties',xtitle='UTC time (hours)',ytitle='Cloud optical depth',$
  ystyle=9,yrange=[0,100],xmargin=[6,11],psym=2
 oplot, tmhrs_big[fl],cod_liquid[fl],psym=2,color=250
 axis, yaxis=1,ystyle=1, yrange=[0.0,250.],/save,ytitle='Effective radius (!9m!4m)'
 oplot, tmhrs_big[fl],ref_ice[fl], psym=4
 oplot, tmhrs_big[fl],ref_liquid[fl], color=250, psym=4
 tvlct,120,120,120,200
 tvlct,160,0,0,251 
 axis,0.9, yaxis=1,ystyle=1,yrange=[0,1],/save,ytitle='Residuals',/normal,color=200
 oplot, tmhrs_big[fl],res_ice[fl], color=200, thick=0.5
 oplot, tmhrs_big[fl],res_liquid[fl], color=251, thick=0.5
 
 legend, ['Ice','Liquid Water'],textcolors=[0,250],/bottom,box=0
 legend, ['Cloud optical depth','Effective radius'],textcolors=[0,0],psym=[2,4],box=0
 
 device, /close
spawn, 'convert "'+dirout+'_rt.ps" "'+dirout+'_rt.png"'
spawn, 'rm -f "'+dirout+'_rt.ps"

end