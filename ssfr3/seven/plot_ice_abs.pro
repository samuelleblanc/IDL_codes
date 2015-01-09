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
device, xsize=20, ysize=20
 !p.font=1 & !p.thick=5 & !p.charsize=2.5 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
 !p.multi=0 & !x.margin=[7,3] &!x.omargin=[0,0]

  plot, ice.field1[0,*]*1000.,ice.field1[2,*]*4.*!PI/ice.field1[0,*]*1000000.,/ylog,xrange=[350,2200.],yrange=[1E-4,1E6],$
   title='Absorption coefficient',xtitle='Wavelength (nm)',ytitle='Absorption coefficient (m!U-1!N)'
  oplot,wat.field1[0,*]*1000.,wat.field1[1,*],color=250
  ;oplot,wv.field1[0,*],wv.field1[1,*]*100000.,color=70,psym=1
;oplot,ice_baum.wavelen, ice_baum.refim*4.*!PI/ice_baum.wavelen*1000000., psym=2,color=70

restore, '~/libradtran/ice/WV_abs.out'
  oplot, wvl, smooth(abs*10.,10), color=70

  legend,['Ice - Warren and Brandt, 2008','Liquid - Cumming, 2013','Water Vapor - HITRAN'],$
   textcolors=[0,250,70],box=0,charsize=2.0;,/right,/bottom

device, /close
spawn, 'convert '+fn+'.ps '+fn+'.png'

stop
end
