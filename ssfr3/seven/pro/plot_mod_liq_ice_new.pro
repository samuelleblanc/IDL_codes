; program the plot the results of the models of liquid and ice water clouds
; this plots a few different optical depth and effective radius

@legend.pro
pro plot_mod_liq_ice

dir='/home/leblanc/SSFR3/data/'
restore, '/argus/roof/SSFR3/model/sp_liq_ice_new3_nstr.out'

base='mod_liq_ice_new3_nstr'
fp=dir+base
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=40, ysize=40
  !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,2,2] & !x.margin=[7,4]


  plot, zenlambda, sp[0,0,*,0]/max(sp[0,0,*,0]),xtitle='Wavelength (nm)',$
   ytitle='Normalized radiance',title='Liquid water cloud radiance'  
  for i=0, 2 do begin
   oplot,zenlambda, sp[i,0,*,0]/max(sp[i,0,*,0]), linestyle=0,color=i*120.
   oplot,zenlambda, sp[i,2,*,0]/max(sp[i,2,*,0]), linestyle=1,color=i*120.
   oplot,zenlambda, sp[i,4,*,0]/max(sp[i,4,*,0]), linestyle=2,color=i*120.
  endfor  
  legend,['ref='+strtrim(fix(ref[[0,2,4]]),2)+' um','tau='+strtrim(fix(tau[[0,1,2]]),2)],textcolors=[0,0,0,0,120,240],$ 
   box=0,linestyle=[0,1,2,0,0,0],/right,color=[0,0,0,0,120,240], pspacing=1.5


  plot, zenlambda, sp[0,0,*,1]/max(sp[0,0,*,1]),xtitle='Wavelength (nm)',$
   ytitle='Normalized radiance',title='Ice water cloud radiance'
  for i=0, 2 do begin
   oplot,zenlambda, sp[i,0,*,1]/max(sp[i,0,*,1]), linestyle=0,color=i*120.
   oplot,zenlambda, sp[i,2,*,1]/max(sp[i,2,*,1]), linestyle=1,color=i*120.
   oplot,zenlambda, sp[i,4,*,1]/max(sp[i,4,*,1]), linestyle=2,color=i*120.
  endfor
  legend,['ref='+strtrim(fix(ref[[0,2,4]]),2)+' um','tau='+strtrim(fix(tau[[0,1,2]]),2)],textcolors=[0,0,0,0,120,240],$
   box=0,linestyle=[0,1,2,0,0,0],/right,color=[0,0,0,0,120,240], pspacing=1.5

  plot, zenlambda, smooth(deriv(zenlambda,sp[0,0,*,0]/max(sp[0,0,*,0])),2),xtitle='Wavelength (nm)',$
   ytitle='Derivative',title='Liquid water cloud radiance',yrange=[-0.015,0.015]
  for i=0, 2 do begin
   oplot,zenlambda, smooth(deriv(zenlambda,sp[i,0,*,0]/max(sp[i,0,*,0])),2), linestyle=0,color=i*120.
   oplot,zenlambda, smooth(deriv(zenlambda,sp[i,2,*,0]/max(sp[i,2,*,0])),2), linestyle=1,color=i*120.
   oplot,zenlambda, smooth(deriv(zenlambda,sp[i,4,*,0]/max(sp[i,4,*,0])),2), linestyle=2,color=i*120.
  endfor
  legend,['ref='+strtrim(fix(ref[[0,2,4]]),2)+' um','tau='+strtrim(fix(tau[[0,1,2]]),2)],textcolors=[0,0,0,0,120,240],$
   box=0,linestyle=[0,1,2,0,0,0],/right,color=[0,0,0,0,120,240], pspacing=1.5


  plot, zenlambda, smooth(deriv(zenlambda,sp[0,0,*,1]/max(sp[0,0,*,1])),2),xtitle='Wavelength (nm)',$
   ytitle='Derivative',title='Ice water cloud radiance',yrange=[-0.015,0.015]
  for i=0, 2 do begin
   oplot,zenlambda, smooth(deriv(zenlambda,sp[i,0,*,1]/max(sp[i,0,*,1])),2), linestyle=0,color=i*120.
   oplot,zenlambda, smooth(deriv(zenlambda,sp[i,2,*,1]/max(sp[i,2,*,1])),2), linestyle=1,color=i*120.
   oplot,zenlambda, smooth(deriv(zenlambda,sp[i,4,*,1]/max(sp[i,4,*,1])),2), linestyle=2,color=i*120.
  endfor
  legend,['ref='+strtrim(fix(ref[[0,2,4]]),2)+' um','tau='+strtrim(fix(tau[[0,1,2]]),2)],textcolors=[0,0,0,0,120,240],$
   box=0,linestyle=[0,1,2,0,0,0],/right,color=[0,0,0,0,120,240],pspacing=1.5

device, /close
spawn, 'convert '+fp+'.ps '+fp+'.png'



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        Zoom on the derivatives, plot ice and liquid on top of each other        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
fp=dir+base+'_dsp_zoom'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=20, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=0 & !x.margin=[8,4]

  plot, zenlambda, smooth(deriv(zenlambda,sp[0,1,*,0]/max(sp[0,1,*,0])),2),xtitle='Wavelength (nm)',$
   ytitle='Derivative',title='Modeled ice/liquid water cloud derivative',yrange=[-0.015,0.015],xrange=[900,1650]
  oplot,zenlambda, smooth(deriv(zenlambda,sp[0,4,*,0]/max(sp[0,4,*,0])),2), color=120
  oplot,zenlambda, smooth(deriv(zenlambda,sp[3,1,*,0]/max(sp[3,1,*,0])),2), color=70 
  oplot,zenlambda, smooth(deriv(zenlambda,sp[3,4,*,0]/max(sp[3,4,*,0])),2), color=250
  
  oplot,zenlambda, smooth(deriv(zenlambda,sp[0,1,*,1]/max(sp[0,1,*,1])),2), linestyle=2
  oplot,zenlambda, smooth(deriv(zenlambda,sp[0,4,*,1]/max(sp[0,4,*,1])),2), color=120, linestyle=2
  oplot,zenlambda, smooth(deriv(zenlambda,sp[3,1,*,1]/max(sp[3,1,*,1])),2), color=70, linestyle=2
  oplot,zenlambda, smooth(deriv(zenlambda,sp[3,4,*,1]/max(sp[3,4,*,1])),2), color=250, linestyle=2

  legend, ['ref=10 um, tau=25 ','ref=10 um, tau=150','ref=25 um, tau=25 ','ref=25 um, tau=150'],textcolors=[0,70,120,250],box=0,/right
  legend, ['Liquid water cloud','Ice water cloud'],linestyle=[0,2],box=0,/right,/bottom,pspacing=1.5,color=[0,0]

device, /close
spawn, 'convert '+fp+'.ps '+fp+'.png'

fp=dir+base+'_zoom'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=20, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=0 & !x.margin=[6,4]
sm=2
  plot, zenlambda, smooth(sp[0,1,*,0]/max(sp[0,1,*,0]),sm),xtitle='Wavelength (nm)',$
   ytitle='Normalized radiance',title='Modeled ice/liquid water cloud spectra',xrange=[900,1700], yrange=[0,0.5]
  oplot,zenlambda, smooth(sp[0,4,*,0]/max(sp[0,4,*,0]),sm), color=200
  oplot,zenlambda, smooth(sp[3,1,*,0]/max(sp[3,1,*,0]),sm), color=70
  oplot,zenlambda, smooth(sp[3,4,*,0]/max(sp[3,4,*,0]),sm), color=250

  oplot,zenlambda, smooth(sp[0,1,*,1]/max(sp[0,1,*,1]),sm), linestyle=2
  oplot,zenlambda, smooth(sp[0,4,*,1]/max(sp[0,4,*,1]),sm), color=200, linestyle=2
  oplot,zenlambda, smooth(sp[3,1,*,1]/max(sp[3,1,*,1]),sm), color=70, linestyle=2
  oplot,zenlambda, smooth(sp[3,4,*,1]/max(sp[3,4,*,1]),sm), color=250, linestyle=2

 ; legend, ['ref=10 um, tau=25 ','ref=10 um, tau=150','ref=25 um, tau=25 ','ref=25 um, tau=150'],textcolors=[0,70,120,250],box=0,/right
 ; legend, ['Liquid water cloud','Ice water cloud'],linestyle=[0,2],box=0,/bottom,pspacing=1.5,color=[0,0]
the="164B ;"
  st='!9'+string(the)+'!X'

  legend, ['r!Deff!N=10 um, '+st+'=50 ','r!Deff!N=10 um, '+st+'=150','r!Deff!N=25 um, '+st+'=50 ','r!Deff!N=25 um, '+st+'=150'],$
   textcolors=[0,70,200,250],box=0,/right
  legend, ['Liquid','Ice'],color=[0,0],pspacing=1.5,linestyle=[0,2],box=0, position=[1620.,0.38],/data,/right


device, /close
spawn, 'convert '+fp+'.ps '+fp+'.png'


fp=dir+base+'_rad'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=20, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=0 & !x.margin=[6,4]

  plot, zenlambda, sp[1,1,*,0],xtitle='Wavelength (nm)',$
   ytitle='Radiance (W/m!U2!N nm sr)',title='Modeled ice/liquid water cloud spectra', yrange=[0,0.4]
  oplot,zenlambda, sp[1,4,*,0], color=120
  oplot,zenlambda, sp[3,1,*,0], color=70
  oplot,zenlambda, sp[3,4,*,0], color=250

  oplot,zenlambda, sp[1,1,*,1], linestyle=2
  oplot,zenlambda, sp[1,4,*,1], color=120, linestyle=2
  oplot,zenlambda, sp[3,1,*,1], color=70, linestyle=2
  oplot,zenlambda, sp[3,4,*,1], color=250, linestyle=2

  legend, ['ref=10 um, tau=50 ','ref=10 um, tau=150','ref=25 um, tau=50 ','ref=25 um, tau=150'],textcolors=[0,70,120,250],box=0,/right
  legend, ['Liquid water cloud','Ice water cloud'],linestyle=[0,2],box=0,/bottom,pspacing=1.5,color=[0,0],/right

device, /close
spawn, 'convert '+fp+'.ps '+fp+'.png'


fp=dir+base+'_rad_ex_sp'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=20, ysize=20
  !p.font=1 & !p.thick=7 & !p.charsize=2.2 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=4 & !x.thick=4
  !p.multi=0 & !x.margin=[7,7]

  plot, zenlambda, sp[1,1,*,0],xtitle='Wavelength (nm)',/nodata,xrange=[350,1700],$
   ytitle='Radiance (W/m!U2!N nm sr)',title='Modeled ice/liquid water cloud spectra', yrange=[0,0.3]
  tvlct,107,255,117,100
  polyfill,[495,505,505,495],[0,0,0.3,0.3],color=100
  polyfill,[1180,1310,1310,1180],[0,0,0.3,0.3],color=100
  polyfill,[980,1075,1075,980],[0,0,0.3,0.3],color=100;,spacing=0.2
  polyfill,[1050,1075,1075,1050],[0,0,0.3,0.3],color=100
  polyfill,[1490,1650,1650,1490],[0,0,0.3,0.3],color=100

  oplot,zenlambda, sp[1,1,*,0]
  oplot,zenlambda, sp[1,4,*,0], color=200
  oplot,zenlambda, sp[3,1,*,0], color=70
  oplot,zenlambda, sp[3,4,*,0], color=250

  oplot,zenlambda, sp[1,1,*,1], linestyle=2
  oplot,zenlambda, sp[1,4,*,1], color=200, linestyle=2
  oplot,zenlambda, sp[3,1,*,1], color=70, linestyle=2
  oplot,zenlambda, sp[3,4,*,1], color=250, linestyle=2

  the="164B ;"
  st='!9'+string(the)+'!X'
  legend, ['r!Deff!N=10 um, '+st+'=50 ','r!Deff!N=10 um, '+st+'=150','r!Deff!N=25 um, '+st+'=50 ','r!Deff!N=25 um, '+st+'=150'],$
   textcolors=[0,70,200,250],box=0,/right
  legend, ['Liquid','Ice'],color=[0,0],pspacing=1.5,linestyle=[0,2],box=0, position=[1600.,0.22],/data,/right


device, /close
spawn, 'convert '+fp+'.ps '+fp+'.png'

fp=dir+base+'_ex_sp'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=20, ysize=20
  !p.font=1 & !p.thick=7 & !p.charsize=2.2 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=4 & !x.thick=4
  !p.multi=0 & !x.margin=[7,7]

  plot, zenlambda, sp[1,1,*,0]/max(sp[1,1,*,0]),xtitle='Wavelength (nm)',/nodata,xrange=[350,1700],$
   ytitle='Normalized radiance',title='Modeled ice/liquid water cloud spectra', yrange=[0,1.0]
  tvlct,107,255,117,100
;  polyfill,[495,505,505,495],[0,0,1,1],color=100
  polyfill,[1180,1310,1310,1180],[0,0,1,1],color=100
  polyfill,[980,1075,1075,980],[0,0,1,1],color=100;,spacing=0.2
  polyfill,[1050,1075,1075,1050],[0,0,1,1],color=100
  polyfill,[1490,1650,1650,1490],[0,0,1,1],color=100

  oplot,zenlambda, sp[1,1,*,0]/max(sp[1,1,*,0])
  oplot,zenlambda, sp[1,4,*,0]/max(sp[1,4,*,0]), color=200
  oplot,zenlambda, sp[3,1,*,0]/max(sp[3,1,*,0]), color=70
  oplot,zenlambda, sp[3,4,*,0]/max(sp[3,4,*,0]), color=250

  oplot,zenlambda, sp[1,1,*,1]/max(sp[1,1,*,1]), linestyle=2
  oplot,zenlambda, sp[1,4,*,1]/max(sp[1,4,*,1]), color=200, linestyle=2
  oplot,zenlambda, sp[3,1,*,1]/max(sp[3,1,*,1]), color=70, linestyle=2
  oplot,zenlambda, sp[3,4,*,1]/max(sp[3,4,*,1]), color=250, linestyle=2

  the="164B ;"  
  st='!9'+string(the)+'!X'
  legend, ['r!Deff!N=10 um, '+st+'=50 ','r!Deff!N=10 um, '+st+'=150','r!Deff!N=25 um, '+st+'=50 ','r!Deff!N=25 um, '+st+'=150'],$
   textcolors=[0,70,200,250],box=0,/right
  legend, ['Liquid','Ice'],color=[0,0],pspacing=1.5,linestyle=[0,2],box=0, position=[1600.,0.73],/data,/right


device, /close
spawn, 'convert '+fp+'.ps '+fp+'.png'






fp=dir+base+'_dsp'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=60, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=2.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,3,1] & !x.margin=[6,4]

  plot, zenlambda, smooth(deriv(zenlambda,sp[0,1,*,0]/max(sp[0,1,*,0])),2),xtitle='Wavelength (nm)',$
   ytitle='Derivative',title='Modeled ice/liquid cloud at band 1000 nm',yrange=[-0.015,0.015],xrange=[900,1125]
  oplot,zenlambda, smooth(deriv(zenlambda,sp[0,4,*,0]/max(sp[0,4,*,0])),2), color=120
  oplot,zenlambda, smooth(deriv(zenlambda,sp[3,1,*,0]/max(sp[3,1,*,0])),2), color=70
  oplot,zenlambda, smooth(deriv(zenlambda,sp[3,4,*,0]/max(sp[3,4,*,0])),2), color=250

  oplot,zenlambda, smooth(deriv(zenlambda,sp[0,1,*,1]/max(sp[0,1,*,1])),2), linestyle=2
  oplot,zenlambda, smooth(deriv(zenlambda,sp[0,4,*,1]/max(sp[0,4,*,1])),2), color=120, linestyle=2
  oplot,zenlambda, smooth(deriv(zenlambda,sp[3,1,*,1]/max(sp[3,1,*,1])),2), color=70, linestyle=2
  oplot,zenlambda, smooth(deriv(zenlambda,sp[3,4,*,1]/max(sp[3,4,*,1])),2), color=250, linestyle=2

  legend, ['ref=10 um, tau=25 ','ref=10 um, tau=150','ref=25 um, tau=25 ','ref=25 um, tau=150'],textcolors=[0,70,120,250],box=0,/right,charsize=2.0
  legend, ['Liquid water cloud','Ice water cloud'],linestyle=[0,2],box=0,/right,/bottom,pspacing=1.5,color=[0,0],charsize=2.0

  plot, zenlambda, smooth(deriv(zenlambda,sp[0,1,*,0]/max(sp[0,1,*,0])),2),xtitle='Wavelength (nm)',$
   ytitle='Derivative',title='Modeled ice/liquid cloud at band 1200 nm',yrange=[-0.01,0.01],xrange=[1125,1400]
  oplot,zenlambda, smooth(deriv(zenlambda,sp[0,4,*,0]/max(sp[0,4,*,0])),2), color=120
  oplot,zenlambda, smooth(deriv(zenlambda,sp[3,1,*,0]/max(sp[3,1,*,0])),2), color=70
  oplot,zenlambda, smooth(deriv(zenlambda,sp[3,4,*,0]/max(sp[3,4,*,0])),2), color=250

  oplot,zenlambda, smooth(deriv(zenlambda,sp[0,1,*,1]/max(sp[0,1,*,1])),2), linestyle=2
  oplot,zenlambda, smooth(deriv(zenlambda,sp[0,4,*,1]/max(sp[0,4,*,1])),2), color=120, linestyle=2
  oplot,zenlambda, smooth(deriv(zenlambda,sp[3,1,*,1]/max(sp[3,1,*,1])),2), color=70, linestyle=2
  oplot,zenlambda, smooth(deriv(zenlambda,sp[3,4,*,1]/max(sp[3,4,*,1])),2), color=250, linestyle=2

  legend, ['ref=10 um, tau=25 ','ref=10 um, tau=150','ref=25 um, tau=25 ','ref=25 um, tau=150'],textcolors=[0,70,120,250],box=0,/right,charsize=2.0
  legend, ['Liquid water cloud','Ice water cloud'],linestyle=[0,2],box=0,/right,/bottom,pspacing=1.5,color=[0,0],charsize=2.0

  plot, zenlambda, smooth(deriv(zenlambda,sp[0,1,*,0]/max(sp[0,1,*,0])),2),xtitle='Wavelength (nm)',$
   ytitle='Derivative',title='Modeled ice/liquid cloud at band 1500 nm',yrange=[-0.002,0.002],xrange=[1400,1650]
  oplot,zenlambda, smooth(deriv(zenlambda,sp[0,4,*,0]/max(sp[0,4,*,0])),2), color=120
  oplot,zenlambda, smooth(deriv(zenlambda,sp[3,1,*,0]/max(sp[3,1,*,0])),2), color=70
  oplot,zenlambda, smooth(deriv(zenlambda,sp[3,4,*,0]/max(sp[3,4,*,0])),2), color=250

  oplot,zenlambda, smooth(deriv(zenlambda,sp[0,1,*,1]/max(sp[0,1,*,1])),2), linestyle=2
  oplot,zenlambda, smooth(deriv(zenlambda,sp[0,4,*,1]/max(sp[0,4,*,1])),2), color=120, linestyle=2
  oplot,zenlambda, smooth(deriv(zenlambda,sp[3,1,*,1]/max(sp[3,1,*,1])),2), color=70, linestyle=2
  oplot,zenlambda, smooth(deriv(zenlambda,sp[3,4,*,1]/max(sp[3,4,*,1])),2), color=250, linestyle=2

  legend, ['ref=10 um, tau=25 ','ref=10 um, tau=150','ref=25 um, tau=25 ','ref=25 um, tau=150'],textcolors=[0,70,120,250],box=0,/right,charsize=2.0
  legend, ['Liquid water cloud','Ice water cloud'],linestyle=[0,2],box=0,/right,/bottom,pspacing=1.5,color=[0,0],charsize=2.0

device, /close
spawn, 'convert '+fp+'.ps '+fp+'.png'

fp=dir+base+'_sp'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=60, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=2.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,3,1] & !x.margin=[6,4]

  plot, zenlambda, sp[0,1,*,0]/max(sp[0,1,*,0]),xtitle='Wavelength (nm)',$
   ytitle='Normalized radiance',title='Modeled ice/liquid cloud at band 1000 nm',yrange=[0,0.5],xrange=[900,1125]
  oplot,zenlambda, sp[0,4,*,0]/max(sp[0,4,*,0]), color=120
  oplot,zenlambda, sp[3,1,*,0]/max(sp[3,1,*,0]), color=70
  oplot,zenlambda, sp[3,4,*,0]/max(sp[3,4,*,0]), color=250

  oplot,zenlambda, sp[0,1,*,1]/max(sp[0,1,*,1]), linestyle=2
  oplot,zenlambda, sp[0,4,*,1]/max(sp[0,4,*,1]), color=120, linestyle=2
  oplot,zenlambda, sp[3,1,*,1]/max(sp[3,1,*,1]), color=70, linestyle=2
  oplot,zenlambda, sp[3,4,*,1]/max(sp[3,4,*,1]), color=250, linestyle=2

  legend, ['ref=10 um, tau=25 ','ref=10 um, tau=150','ref=25 um, tau=25 ','ref=25 um, tau=150'],textcolors=[0,70,120,250],box=0,/right,charsize=2.0
  legend, ['Liquid water cloud','Ice water cloud'],linestyle=[0,2],box=0,/right,/bottom,pspacing=1.5,color=[0,0],charsize=2.0

  plot, zenlambda, sp[0,1,*,0]/max(sp[0,1,*,0]),xtitle='Wavelength (nm)',$
   ytitle='Normalized radiance',title='Modeled ice/liquid cloud at band 1000 nm',yrange=[0,0.3],xrange=[1125,1400]
  oplot,zenlambda, sp[0,4,*,0]/max(sp[0,4,*,0]), color=120
  oplot,zenlambda, sp[3,1,*,0]/max(sp[3,1,*,0]), color=70
  oplot,zenlambda, sp[3,4,*,0]/max(sp[3,4,*,0]), color=250

  oplot,zenlambda, sp[0,1,*,1]/max(sp[0,1,*,1]), linestyle=2
  oplot,zenlambda, sp[0,4,*,1]/max(sp[0,4,*,1]), color=120, linestyle=2
  oplot,zenlambda, sp[3,1,*,1]/max(sp[3,1,*,1]), color=70, linestyle=2
  oplot,zenlambda, sp[3,4,*,1]/max(sp[3,4,*,1]), color=250, linestyle=2

  legend, ['ref=10 um, tau=25 ','ref=10 um, tau=150','ref=25 um, tau=25 ','ref=25 um, tau=150'],textcolors=[0,70,120,250],box=0,/right,charsize=2.0
  legend, ['Liquid water cloud','Ice water cloud'],linestyle=[0,2],box=0,/right,/bottom,pspacing=1.5,color=[0,0],charsize=2.0

  plot, zenlambda, sp[0,1,*,0]/max(sp[0,1,*,0]),xtitle='Wavelength (nm)',$
   ytitle='Normalized radiance',title='Modeled ice/liquid cloud at band 1000 nm',yrange=[0,0.1],xrange=[1400,1650]
  oplot,zenlambda, sp[0,4,*,0]/max(sp[0,4,*,0]), color=120
  oplot,zenlambda, sp[3,1,*,0]/max(sp[3,1,*,0]), color=70
  oplot,zenlambda, sp[3,4,*,0]/max(sp[3,4,*,0]), color=250

  oplot,zenlambda, sp[0,1,*,1]/max(sp[0,1,*,1]), linestyle=2
  oplot,zenlambda, sp[0,4,*,1]/max(sp[0,4,*,1]), color=120, linestyle=2
  oplot,zenlambda, sp[3,1,*,1]/max(sp[3,1,*,1]), color=70, linestyle=2
  oplot,zenlambda, sp[3,4,*,1]/max(sp[3,4,*,1]), color=250, linestyle=2

  legend, ['ref=10 um, tau=25 ','ref=10 um, tau=150','ref=25 um, tau=25 ','ref=25 um, tau=150'],textcolors=[0,70,120,250],box=0,/right,charsize=2.0
  legend, ['Liquid water cloud','Ice water cloud'],linestyle=[0,2],box=0,/right,/bottom,pspacing=1.5,color=[0,0],charsize=2.0

device, /close
spawn, 'convert '+fp+'.ps '+fp+'.png'


stop
end
