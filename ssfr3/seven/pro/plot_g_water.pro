; program to plot the g for ice and liquid water cloud particles
; input from the phase function of Baum 2011, and Mie outputs used in libradtran 1.7

pro plot_g_water

restore, '~/libradtran/ice/g_ice.out'
wvl_ice=wvl*1000.
g_ice=g
g2_ice=g2
restore, '~/libradtran/ice/g_liq.out'
wvl_liq=wvl*1000.
g_liq=g
g2_liq=g2

dir='/home/leblanc/SSFR3/plots/'

fp=dir+'g_liq_ice'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=20, ysize=20
  !p.font=1 & !p.thick=7 & !p.charsize=2.2 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=4 & !x.thick=4
  !p.multi=0 & !x.margin=[7,7]

  plot, wvl*1000., g2,xtitle='Wavelength (nm)',/nodata,xrange=[350,1700],$
   ytitle='Asymmetry parameter',title='Ice and liquid particles asymmetry parameter', yrange=[0,1.0]
  tvlct,107,255,117,100
  ;polyfill,[495,505,505,495],[0,0,0.3,0.3],color=100
  ;polyfill,[1180,1310,1310,1180],[0,0,0.3,0.3],color=100
  ;polyfill,[980,1075,1075,980],[0,0,0.3,0.3],color=100;,spacing=0.2
  ;polyfill,[1050,1075,1075,1050],[0,0,0.3,0.3],color=100
  ;polyfill,[1490,1650,1650,1490],[0,0,0.3,0.3],color=100

  oplot,wvl_liq, g2_liq
  oplot,wvl_liq, g_liq,color=250

  oplot,wvl_ice, g2_ice, linestyle=2
  oplot,wvl_ice, g_ice,color=250,linestyle=2

  ng2l=int_tabulated(wvl_liq,g2_liq)/int_tabulated(wvl_liq,wvl_liq*0.+1.)
  ngl=int_tabulated(wvl_liq,g_liq)/int_tabulated(wvl_liq,wvl_liq*0.+1.)
  ng2i=int_tabulated(wvl_ice,g2_ice)/int_tabulated(wvl_ice,wvl_ice*0.+1.)
  ngi=int_tabulated(wvl_ice,g_ice)/int_tabulated(wvl_ice,wvl_ice*0.+1.)

print, '10 - liquid',ng2l
print, '25 - liquid',ngl
print, '10 - ice',ng2i
print, '25 - ice',ngi

  the="164B ;"
  st='!9'+string(the)+'!X'
  legend, ['r!Deff!N=10 um','r!Deff!N=25 um'],$
   textcolors=[0,250],box=0,/right,/bottom
  legend, ['Liquid','Ice'],color=[0,0],pspacing=1.5,linestyle=[0,2],box=0,/bottom


device, /close
spawn, 'convert '+fp+'.ps '+fp+'.png'

stop
end
