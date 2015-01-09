; program to load the save file from compare_cod
; plots the comparison of cod to various other methods of determining the optical depth
; possibly to see the 'extra' photon path length

@legend.pro
pro plot_cod_compare, datein

dir='/home/leblanc/SSFR3/data/'
if n_elements(datein) lt 1 then date='20120926' else date=datein
l='/'
restore, dir+date+l+date+'_cod_comp.out'

fn=dir+date+l+date+'_cod_results'
set_plot,'ps'
print, 'making plot :'+fn
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fn+'.ps'
 device, xsize=40, ysize=40
 !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
 !p.multi=[0,2,2] & !x.margin=[7,2] &!x.omargin=[0,0]

 n=where(finite(tau) eq 1 and finite(area) eq 1)

 plot, area[n],tau[n],psym=2, title='Optical depth vs. water vapor absorption',xtitle='Integrated water vapor absorption',ytitle='Cloud optical depth'
 oplot, area2[n], tau[n], psym=2, color=70
 
 ao=linfit(area[n],tau[n])
 rao=correlate(area[n],tau[n])
 oplot, findgen(30), findgen(30)*ao[1]+ao[0],linestyle=2

 ao2=linfit(area2[n],tau[n])
 rao2=correlate(area2[n],tau[n])
 oplot, findgen(30), findgen(30)*ao2[1]+ao2[0],linestyle=2,color=70
 
 legend,['940 nm Band,R='+strtrim(rao,2),'1150 nm Band,R='+strtrim(rao2,2)],textcolors=[0,70],box=0,/right

 plot, cod_ar1[n],tau[n],psym=3, title='Optical depth from water vapor absorption',xtitle='Optical depth from water vapor absorption',ytitle='Cloud optical depth',xrange=[0,200]
 oplot,cod_ar2[n],tau[n],psym=3,color=70

 wo=linfit(cod_ar1[n],tau[n])
 rwo=correlate(cod_ar1[n],tau[n])
 oplot, findgen(200), findgen(200)*wo[1]+wo[0],linestyle=2

 wo2=linfit(cod_ar2[n],tau[n])
 rwo2=correlate(cod_ar2[n],tau[n])
 oplot, findgen(200), findgen(200)*wo2[1]+wo2[0],linestyle=2,color=70

 legend,['940 nm Band,R='+strtrim(rwo,2),'1150 nm Band,R='+strtrim(rwo2,2)],textcolors=[0,70],box=0

  plot, oxa[n],tau[n],psym=2, title='Optical depth vs. Oxygen-a band',xtitle='Integrated Osygen-a band absorption',ytitle='Cloud optical depth'

 ox=linfit(oxa[n],tau[n])
 rox=correlate(oxa[n],tau[n])
 oplot, findgen(30), findgen(30)*ox[1]+ox[0],linestyle=2

 legend,['Oxygen-a Band,R='+strtrim(rox,2)],textcolors=[0],box=0,/right

 plot, cod_ox[n],tau[n],psym=3, title='Optical depth from Oxygen-a band',xtitle='Optical depth from Oxygen-a band',ytitle='Cloud optical depth',xrange=[0,200]

 xc=linfit(cod_ox[n],tau[n])
 rxc=correlate(cod_ox[n],tau[n])
 oplot, findgen(200), findgen(200)*xc[1]+xc[0],linestyle=2

 legend,['Oxygen-a Band,R='+strtrim(rxc,2)],textcolors=[0],box=0

device, /close
spawn, 'convert '+fn+'.ps '+fn+'.png'
stop

end
