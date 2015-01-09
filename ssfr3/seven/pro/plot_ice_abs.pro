; program to plot the ice water absorption on top of the liquid water absorption
; plots the values from the baum library

@legend.pro
pro plot_ice_abs

; get the ice data
ice=read_ascii('~/libradtran/ice/CRYSTALS_H2O-ice_Warren.csv',delimiter=',',data_start=2)
wat=read_ascii('~/libradtran/ice/H2O_absorption_Cumming_2013.csv',delimiter=',') ;'~/libradtran/ice/LIQUIDS_Water_Hale.csv',delimiter=',',data_start=2)
wat.field1[0,*]=10000000./wat.field1[0,*]/1000.
wv =read_ascii('~/libradtran/ice/H2O_vapor_absorption_Ptashnik_2004.csv',delimiter=',')
wv.field1[0,*]=10000000./wv.field1[0,*]/1000.
wv.field1[1,*]=wv.field1[1,*]/10.
read_netcdf,'/home/leblanc/libradtran/ice/ic.sol.baum.cdf.bak',ice_baum,att,stat

fn='/home/leblanc/libradtran/ice/ice_abs'
set_plot,'ps'
print, 'making plot :'+fn
loadct, 39, /silent
device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fn+'.ps'
device, xsize=20, ysize=20
 !p.font=1 & !p.thick=5 & !p.charsize=2.5 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
 !p.multi=0 & !x.margin=[7,3] &!x.omargin=[0,0]

  plot, ice.field1[0,*],ice.field1[2,*]*4.*!PI/ice.field1[0,*]*1000000.,/ylog,xrange=[0,2.5],yrange=[1E-3,1E6],$
   title='Absorption coefficient',xtitle='Wavelength (um)',ytitle='Absorption coefficient (m!U-1!N)'
  oplot,wat.field1[0,*],wat.field1[1,*],color=250
  oplot,wv.field1[0,*],wv.field1[1,*]*100000.,color=180,psym=2
oplot,ice_baum.wavelen, ice_baum.refim*4.*!PI/ice_baum.wavelen*1000000., psym=2,color=70

  legend,['Ice','Liquid','Water Vapor continuum','Baum et al., 2005'],textcolors=[0,250,180,70],box=0;,/right,/bottom

device, /close
spawn, 'convert '+fn+'.ps '+fn+'.png'

fn='/home/leblanc/libradtran/ice/ice_abs2'
set_plot,'ps'
print, 'making plot :'+fn
loadct, 39, /silent
device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fn+'.ps'
device, xsize=40, ysize=20
 !p.font=1 & !p.thick=5 & !p.charsize=2.5 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
 !p.multi=[0,2,1] & !x.margin=[7,3] &!x.omargin=[0,0]

  plot, ice.field1[0,*]*1000.,ice.field1[2,*]*4.*!PI/ice.field1[0,*]*1000000.,/ylog,xrange=[400,1700.],yrange=[1E-4,1E6],$
   xtitle='Wavelength (nm)',ytitle='Absorption coefficient (m!U-1!N)'
  oplot,wat.field1[0,*]*1000.,wat.field1[1,*],color=250
  ;oplot,wv.field1[0,*],wv.field1[1,*]*100000.,color=70,psym=1
;oplot,ice_baum.wavelen, ice_baum.refim*4.*!PI/ice_baum.wavelen*1000000., psym=2,color=70

restore, '~/libradtran/ice/WV_abs.out'
  oplot, wvl, smooth(abs*10.,10), color=70

  legend,['Ice - Warren and Brandt, 2008','Liquid - Cumming, 2013','Water Vapor - HITRAN'],$
   textcolors=[0,250,70],box=0,charsize=2.0;,/right,/bottom

restore, '~/SSFR3/data/phase/gs.out'
 plot, wvl*1000., g_ice[0,*],xtitle='Wavelength (nm)',xrange=[400,1700],ytitle='Asymmetry parameter',yrange=[0.5,1],/nodata

 n=min(abs(ref_ice-15.),ri15)
 n=min(abs(ref_ice-25.),ri25)
 n=min(abs(ref_ice-35.),ri35)

 oplot,wvl_ice*1000., g_ice[ri15,*],linestyle=2, thick=10
 oplot,wvl_ice*1000., g_ice[ri25,*],linestyle=2,color=70, thick=10
 oplot,wvl_ice*1000., g_ice[ri35,*],linestyle=2,color=250,thick=10

 n=min(abs(ref_liq-15.),rl15)
 n=min(abs(ref_liq-25.),rl25)
 n=min(abs(ref_liq-35.),rl35)

 oplot,wvl_liq, g_liq[rl15,*], thick=10
 oplot,wvl_liq, g_liq[rl25,*],color=70, thick=10
 oplot,wvl_liq, g_liq[rl35,*],color=250,thick=10

 legend,['r!De!N=15 !9m!Xm','r!De!N=25 !9m!Xm','r!De!N=35 !9m!Xm','Liquid','Ice'],linestyle=[0,0,0,0,2],color=[255,255,255,0,0],pspacing=1.4,textcolors=[0,70,250,0,0],/right,/bottom,box=0,thick=[10,10,10,10,10]


device, /close
spawn, 'convert '+fn+'.ps '+fn+'.png'

stop
end
