; program to build the g for each ref of ice and liquid
; and then plot them

pro plot_g

; do ice
dir='/home/leblanc/SSFR3/data/phase/'
;restore, dir+'baum_2011_ice_512.out'
;g_ice=fltarr(n_elements(ref),n_elements(wvl))

restore, '~/libradtran/ice/g_ice.out'
ref_ice=ref & wvl_ice=wvl

;for r=0, n_elements(ref)-1 do begin
;  for w=0, n_elements(wvl)-1 do $
;    g_ice[r,w]=int_tabulated(cos(double(theta[*,0,r,w]*!dtor)),phase[*,0,r,w])
;  print, 'ice',r
;endfor
;stop

; do liquid
print, 'restoring liquid'
restore, '~/libradtran/g_liq.out'
g_liq=g
;restore, dir+'wc.mie.phase.out'
;g_liq=fltarr(n_elements(ref),n_elements(wvl))
;;wvl is in microns
ref_liq=ref & wvl_liq=wvl
;for r=0, n_elements(ref)-1 do begin
;  for w=0, n_elements(wvl)-1 do begin
;    fl=where(theta[*,0,r,w] ne -999. and phase[*,0,r,w] gt 0.2,ns)
;    if fl[ns-1] ne 0 then fl=fl[0:ns-2]
;    g_liq[r,w]=int_tabulated(cos(double(theta[fl,0,r,w]*!dtor)),phase[fl,0,r,w])
;  endfor
;  print, 'liq',r
;endfor

save, g_ice,g_liq,ref_ice,ref_liq,wvl_ice,wvl_liq,filename=dir+'gs.out'

stop

; now plot the gs
fp=dir+'g_ice_liq'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=20, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=2.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=0 & !x.margin=[6,1.5] & !y.margin=[0,0] & !y.omargin=[3,1]

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
 spawn, 'convert '+fp+'.ps '+fp+'.png'

; now plot the qscat
restore, '/home/leblanc/libradtran/wc_qscat.out'
fp=dir+'qscat_liq'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=20, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=2.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=0 & !x.margin=[6,1.5] & !y.margin=[0,0] & !y.omargin=[3,1]

 plot, wvl, qscat[0,*],xtitle='Wavelength (nm)',xrange=[400,1700],ytitle='Scattering efficiency factor',yrange=[40,110],/nodata

 oplot,wvl, qscat[0,*],linestyle=0, thick=10
 oplot,wvl, qscat[1,*],linestyle=0,color=70, thick=10
 oplot,wvl, qscat[2,*],linestyle=0,color=250,thick=10

ice=read_ascii('~/libradtran/ice/CRYSTALS_H2O-ice_Warren.csv',delimiter=',',data_start=2)
wvl_kice=ice.field1[0,*]*1000.
k_abs=ice.field1[2,*]*4.*!PI/ice.field1[0,*]*1000000.
qscat_ice=fltarr(23,n_elements(wvl_kice))
ssatm=wvl_kice
for i=0, 22 do begin
  n=where(ssa_ice[i,*] ne 1.0)
  ssatm=interpol(ssa_ice[i,n],wvl_ice[n]*1000.,wvl_kice)
  qscat_ice[i,*]=(ssatm*k_abs)/(1.-ssatm)
endfor

;k_abs_ice=interpol(k_abs,wvl_kice,wvl_ice*1000.)  
;n=where(ssa_ice eq 1.0)
;ssa_ice[n]=0.999999
;for i=0, 22 do qscat_ice[i,*]=(ssa_ice[i,*]*k_abs_ice)/(1.-ssa_ice[i,*])

; oplot,wvl_kice,smooth(qscat_ice[ri15,*]/1000.,15),linestyle=2,thick=10
; oplot,wvl_kice,smooth(qscat_ice[ri25,*]/1000.,15),linestyle=2,color=70,thick=10
; oplot,wvl_kice,smooth(qscat_ice[ri35,*]/1000.,15),linestyle=2,color=250,thick=10

 legend,['r!De!N=15 !9m!Xm','r!De!N=25 !9m!Xm','r!De!N=35 !9m!Xm'],linestyle=[0,0,0],color=[255,255,255],pspacing=1.4,textcolors=[0,70,250],/right,/bottom,box=0,thick=[10,10,10]

 device, /close
 spawn, 'convert '+fp+'.ps '+fp+'.png'


stop 
end
