; program to plot the effect of removing water vapor on ice and liquid water clouds
; uses the output from janus on sp_nowv.out (no water vapor)

@legend.pro
pro plot_nowv
restore, '/argus/SSFR3/model/sp_nowv.out'

;sp=fltarr(2,2,zenlambda,2) 
; sp[1,*,*,*] - tau 25, ref 5
; sp[0,*,*,*] - tau 100, ref 25
; sp[*,0,*,*] - with water
; sp[*,1,*,*] - without water
; sp[*,*,*,0] - liquid
; sp[*,*,*,1] - ice

wvl=zenlambda

dir='/home/leblanc/SSFR3/data/'

dif=fltarr(2,n_elements(zenlambda),2)

fp=dir+'spc_nowv'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=40, ysize=40
  !p.font=1 & !p.thick=5 & !p.charsize=2.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,2,2] & !x.margin=[8,4]

 plot, wvl, sp[0,0,*,0]/max(sp[0,0,*,0]), title='Water vapor on liquid cloud', ytitle='Normalized radiance',xtitle='Wavelength (nm)'
 
 tvlct,r,g,b,/get
 v1=r[250] & v2=g[250]+150 & v3=b[250]+150
 tvlct,v1,v2,v3,251
 print, r[250],g[250],b[250]
 print, v1,v2,v3

 oplot,wvl, sp[0,0,*,0]/max(sp[0,0,*,0]),color=250
 oplot,wvl, sp[0,1,*,0]/max(sp[0,1,*,0]),color=251

 v1=r[50]-5 & v2=g[50]-5 & v3=b[50]-5
 tvlct,v1,v2,v3,51

 oplot,wvl, sp[1,0,*,0]/max(sp[1,0,*,0]),color=50
 oplot,wvl, sp[1,1,*,0]/max(sp[1,1,*,0]),color=51
 
 legend,['tau=25, ref=5um','tau=100, ref=25um','tau=25, ref=5um - No WV','tau=100, ref=25um - No WV'],textcolors=[50,250,51,251], box=0, /right

 
 plot, wvl, sp[0,0,*,1]/max(sp[0,0,*,1]), title='Water vapor on Ice cloud', ytitle='Normalized radiance',xtitle='Wavelength (nm)'
 
 oplot,wvl, sp[0,0,*,1]/max(sp[0,0,*,1]),color=250
 oplot,wvl, sp[0,1,*,1]/max(sp[0,1,*,1]),color=251

 oplot,wvl, sp[1,0,*,1]/max(sp[1,0,*,1]),color=50
 oplot,wvl, sp[1,1,*,1]/max(sp[1,1,*,1]),color=51

 legend,['tau=25, ref=5um','tau=100, ref=25um','tau=25, ref=5um - No WV','tau=100, ref=25um - No WV'],textcolors=[50,250,51,251], box=0, /right

 plot, wvl,  sp[0,1,*,0]/max(sp[0,1,*,0])-sp[0,0,*,0]/max(sp[0,0,*,0]), yrange=[0,0.4],$
  title='Liquid cloud - difference due to WV',ytitle='Difference in normalized radiance',xtitle='Wavelegnth (nm)'
 oplot,wvl,  sp[0,1,*,0]/max(sp[0,1,*,0])-sp[0,0,*,0]/max(sp[0,0,*,0]),color=250
 oplot,wvl,  sp[1,1,*,0]/max(sp[1,1,*,0])-sp[1,0,*,0]/max(sp[1,0,*,0]),color=50

dif[0,*,0]=sp[0,1,*,0]/max(sp[0,1,*,0])-sp[0,0,*,0]/max(sp[0,0,*,0])
dif[1,*,0]=sp[1,1,*,0]/max(sp[1,1,*,0])-sp[1,0,*,0]/max(sp[1,0,*,0])
 
 legend,['tau=25, ref=5um','tau=100, ref=25um'],textcolors=[50,250], box=0, /right

 plot, wvl,  sp[0,1,*,1]/max(sp[0,1,*,1])-sp[0,0,*,1]/max(sp[0,0,*,1]), yrange=[0,0.4],$
  title='Ice cloud - difference due to WV',ytitle='Difference in normalized radiance',xtitle='Wavelegnth (nm)'
 oplot,wvl,  sp[0,1,*,1]/max(sp[0,1,*,1])-sp[0,0,*,1]/max(sp[0,0,*,1]),color=250
 oplot,wvl,  sp[1,1,*,1]/max(sp[1,1,*,1])-sp[1,0,*,1]/max(sp[1,0,*,1]),color=50

dif[0,*,1]=sp[0,1,*,1]/max(sp[0,1,*,1])-sp[0,0,*,1]/max(sp[0,0,*,1])
dif[1,*,1]=sp[1,1,*,1]/max(sp[1,1,*,1])-sp[1,0,*,1]/max(sp[1,0,*,1]) 
eps=0.05
fl2=where(dif[0,*,0] lt eps and dif[1,*,0] lt eps and dif[0,*,1] lt eps and dif[1,*,1] lt eps)
oplot, wvl[fl2],dif[0,fl2,1],psym=2,color=170
oplot, wvl[fl2],dif[1,fl2,1],psym=2,color=170


eps=0.005
fl=where(dif[0,*,0] lt eps and dif[1,*,0] lt eps and dif[0,*,1] lt eps and dif[1,*,1] lt eps)

oplot, wvl[fl],dif[0,fl,1],psym=2
oplot, wvl[fl],dif[1,fl,1],psym=2

 legend,['tau=25, ref=5um','tau=100, ref=25um','valid points < 0.005','valid points < 0.05'],textcolors=[50,250,0,170], box=0, /right

device, /close
spawn, 'convert '+fp+'.ps '+fp+'.png'
;save, wvl, fl,fl2, filename=dir+'wv_fl.out'


fp=dir+'spc_raw_nowv'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=40, ysize=40
  !p.font=1 & !p.thick=5 & !p.charsize=2.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,2,2] & !x.margin=[8,4]

yr=[0,max(sp[*,*,*,0])*1.05]

 plot, wvl, sp[0,0,*,0], title='Water vapor on liquid cloud', ytitle='Radiance (W/m!U2!N nm)',xtitle='Wavelength (nm)',yr=yr

 tvlct,r,g,b,/get
 v1=r[250] & v2=g[250]+150 & v3=b[250]+150
 tvlct,v1,v2,v3,251
 print, r[250],g[250],b[250]
 print, v1,v2,v3

 oplot,wvl, sp[0,0,*,0],color=250
 oplot,wvl, sp[0,1,*,0],color=251

 v1=r[50]-5 & v2=g[50]-5 & v3=b[50]-5
 tvlct,v1,v2,v3,51

 oplot,wvl, sp[1,0,*,0],color=50
 oplot,wvl, sp[1,1,*,0],color=51

 legend,['tau=25, ref=5um - with WV','tau=100, ref=25um - with WV','tau=25, ref=5um - No WV','tau=100, ref=25um - No WV'],textcolors=[50,250,51,251], box=0, /right,charsize=2

 yr=[0,max(sp[*,*,*,1])*1.05]
 plot, wvl, sp[0,0,*,1], title='Water vapor on Ice cloud', ytitle='Radiance (W/m!U2!N nm)',xtitle='Wavelength (nm)',yr=yr
 
 oplot,wvl, sp[0,0,*,1],color=250
 oplot,wvl, sp[0,1,*,1],color=251

 oplot,wvl, sp[1,0,*,1],color=50
 oplot,wvl, sp[1,1,*,1],color=51

 legend,['tau=25, ref=5um - with WV','tau=100, ref=25um- with WV','tau=25, ref=5um - No WV','tau=100, ref=25um - No WV'],textcolors=[50,250,51,251], box=0, /right,charsize=2
;(sp[0,1,*,0]-sp[0,0,*,0])/max(sp[0,0,*,0])
 plot, wvl,  (sp[0,1,*,0]-sp[0,0,*,0]), yrange=[0,0.1],ymargin=[4,4.5],$
  title='Liquid cloud - difference due to WV!C(Rad!DNo WV!N - Rad!Dwith WV!N)',ytitle='Radiance difference (W/m!U2!N nm)',xtitle='Wavelegnth (nm)'
 oplot,wvl,  (sp[0,1,*,0]-sp[0,0,*,0]),color=250
 oplot,wvl,  (sp[1,1,*,0]-sp[1,0,*,0]),color=50

dif[0,*,0]=(sp[0,1,*,0]-sp[0,0,*,0]);/max(sp[0,0,*,0])
dif[1,*,0]=(sp[1,1,*,0]-sp[1,0,*,0]);/max(sp[1,0,*,0])
 
 legend,['tau=25, ref=5um','tau=100, ref=25um'],textcolors=[50,250], box=0, /right,charsize=2

 plot, wvl,  (sp[0,1,*,1]-sp[0,0,*,1]), yrange=[0,0.1],ymargin=[4,4.5],$
  title='Ice cloud - difference due to WV!C(Rad!DNo WV!N - Rad!Dwith WV!N)',ytitle='Radiance difference (W/m!U2!N nm)',xtitle='Wavelegnth (nm)'
 oplot,wvl,  (sp[0,1,*,1]-sp[0,0,*,1]),color=250
 oplot,wvl,  (sp[1,1,*,1]-sp[1,0,*,1]),color=50

dif[0,*,1]=(sp[0,1,*,1]-sp[0,0,*,1]);/max(sp[0,0,*,1])
dif[1,*,1]=(sp[1,1,*,1]-sp[1,0,*,1]);/max(sp[1,0,*,1])
eps=0.01
fl2=where(dif[0,*,0] lt eps and dif[1,*,0] lt eps and dif[0,*,1] lt eps and dif[1,*,1] lt eps)
oplot, wvl[fl2],dif[0,fl2,1],psym=2,color=170
oplot, wvl[fl2],dif[1,fl2,1],psym=2,color=170

eps=0.005
fl=where(dif[0,*,0] lt eps and dif[1,*,0] lt eps and dif[0,*,1] lt eps and dif[1,*,1] lt eps)

oplot, wvl[fl],dif[0,fl,1],psym=2
oplot, wvl[fl],dif[1,fl,1],psym=2

 legend,['tau=25, ref=5um','tau=100, ref=25um','valid points < 0.005','valid points < 0.01'],textcolors=[50,250,0,170], box=0, /right,charsize=2

device, /close
spawn, 'convert '+fp+'.ps '+fp+'.png'



fp=dir+'spc_raw_nowv_zoom'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=20, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=2.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=0 & !x.margin=[8,4]
  plot, wvl, sp[0,0,*,0], title='Water vapor on liquid cloud', ytitle='Radiance (W/m!U2!N nm)',xtitle='Wavelength (nm)',yr=[0,0.2],xr=[900,1150]

 tvlct,r,g,b,/get
 v1=r[250] & v2=g[250]+150 & v3=b[250]+150
 tvlct,v1,v2,v3,251
 print, r[250],g[250],b[250]
 print, v1,v2,v3

 oplot,wvl, sp[0,0,*,0],color=250
 oplot,wvl, sp[0,1,*,0],color=251

 v1=r[50]-5 & v2=g[50]-5 & v3=b[50]-5
 tvlct,v1,v2,v3,51

 oplot,wvl, sp[1,0,*,0],color=50
 oplot,wvl, sp[1,1,*,0],color=51

 legend,['tau=25, ref=5um - with WV','tau=100, ref=25um - with WV','tau=25, ref=5um - No WV','tau=100, ref=25um - No WV'],textcolors=[50,250,51,251], box=0, /right,charsize=2


device, /close
spawn, 'convert '+fp+'.ps '+fp+'.png'


stop
end
