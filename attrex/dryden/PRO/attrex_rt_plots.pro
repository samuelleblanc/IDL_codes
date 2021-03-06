;+
; NAME:
;   attrex_rt_plots
;
; PURPOSE:
;   Plot the calibrated and uncalibrated curves at same time as they come in for each file.
;
; CATEGORY:
;   ATTREX real time data module   
;
; CALLING SEQUENCE:
;   attrex_rt_plots, wvl, utc, spect, spect_cal,sza, extra,rnav, ssfr_temp, zcg4_temp, ncg4_temp, zcg4, ncg4
;   - where wvl is the wavelength arrays for all the spectrometers
;   - utc is the time in UTC (hours)
;   - spect is the uncalibrated spectra, with the darks taken out
;   - spect_cal is the calibrated spectra
;   - sza is the solar zenith angle. If this is empty the kurudz extrasolar irradiance won't be plotted
;   - extra is the file path to the extra solar flux
;   - rnav is the indicator if there is nav data or not
;   - ssfr_temp is the array of temperatures from the SSFR
;   - zcg4_temp is the temperature of the zenith CG4
;   - ncg4_temp is the temperature of the nadir CG4
;   - zcg4 is the Zenith CG4 long wave irradiance
;   - ncg4 is the nadir CG4 long wave irradiance
;
; OUTPUT:
;   see above
;
; KEYWORDS:
;   none
;
; DEPENDENCIES:
;   - legend.pro
; 
; NEEDED FILES:
;   - extra solar flux (usually kurudz 1nm extrasolar radiation)
;   
; EXAMPLE:
;   
;
; MODIFICATION HISTORY:
; Written:  Samuel LeBlanc, LASP CU Boulder, September 22nd, 2011
; Modified: BY Samuel LeBlanc, October 12th, 2011
;           - added plotting of CG4 irradiance time series
;           - added plotting of all the time series of the SSFr temperatures
;           
;---------------------------------------------------------------------------
@legend.pro
pro attrex_rt_plots, wvl, utc, spect, spect_cal, sza, extra, rnav, ssfr_temp, zcg4_temp, ncg4_temp, zcg4, ncg4
print, 'In the plotting phase'

print, 'retrieving extraterrestrial radiation'
ex=read_ascii(extra)

xsize=800
ysize=500
window,0,title='Raw Zenith and Nadir',xsize=xsize,ysize=ysize,retain=2
window,1,title='Zenith and Nadir',xsize=xsize,ysize=ysize,retain=2
loadct,39,/silent
tvlct,130,130,130,200
device, decomposed=0
!y.omargin=[0,0]
!x.omargin=[0,3]
!p.multi=0
for i=0l,n_elements(utc)-1 do begin
  wset,0
  title = 'Raw counts at UTC:' + strcompress(utc(i),/remove_all)
  yr=[0,2^15-1]
  plot, wvl[*,0], spect[i,*,0], xtitle = 'Wavelength (nm)', $
   ytitle = 'Zenith DN', xrange = [300,2200],$
   xstyle=1,ystyle=9, charsize = 1.5, charthick = 1.5, $
   title = title,thick=1.5,yrange=yr
  oplot,wvl[*,1],spect[i,*,1]
  axis, yaxis=1,ystyle=1, yrange=yr,/save,color=250, ytitle='Nadir DN'
  oplot,wvl[*,2],spect[i,*,2],color=250
  oplot,wvl[*,3],spect[i,*,3],color=250
  
  wset,1
  title = 'Irradiance at UTC:' + strcompress(utc(i),/remove_all)
  yr=[0,2.5]
  plot, wvl[*,0], spect_cal[i,*,0], xtitle = 'Wavelength (nm)', $
   ytitle = 'Zenith Irradiance (W/m!U2!N nm)', xrange = [300,2200],$
   xstyle=1,ystyle=9, charsize = 1.5, charthick = 1.5, $
   title = title,thick=1.5,yrange=yr,/nodata
  if rnav then begin
    oplot,ex.field1[0,*],ex.field1[1,*]*0.001*cos(sza[i]*!dtor),color=200  
    xyouts, 1600,2.3,'Model Irradiance',color=200,charsize=1.5     
  endif
  oplot,wvl[*,1],spect_cal[i,*,1]
  oplot,wvl[*,0],spect_cal[i,*,0]
  axis, yaxis=1,ystyle=1, yrange=yr,/save,color=250, ytitle='Nadir Irradiance (W/m!U2!N nm)'
  oplot,wvl[*,2],spect_cal[i,*,2],color=250
  oplot,wvl[*,3],spect_cal[i,*,3],color=250             
  
  xyouts, 1600,2.2,'Downwelling Irradiance',charsize=1.5
  xyouts, 1600,2.1,'Upwelling Irradiance',charsize=1.5,color=250

  wait,0.01       
endfor

; now plot summary plots of the temperatures and CG4 irradiances

window, 2, title='CG4', xsize=xsize, ysize=ysize
loadct,39,/silent
!p.multi=[0,1,2]
device, decomposed=0
plot, utc, zcg4, title='Time series of CG4 irradiances',yrange=[0,650.],xtitle='UTC (hours)',ytitle='Irradiance (W/m!U2!N)'
oplot, utc, ncg4,color=250
legend, ['Zenith','Nadir'],textcolors=[0,250],box=0

plot, utc, zcg4_temp, title='Time series of CG4 temperatures',yrange=[-50,80],xtitle='UTC (hours)',ytitle='Temperature (Celsius)'
oplot, utc,ncg4_temp, color=250
legend, ['Zenith','Nadir'],textcolors=[0,250],box=0

window, 3, title='SSFR temperatures', xsize=xsize, ysize=ysize*2
!p.multi=[0,2,2]
loadct, 39,/silent
device, decomposed=0
plot, utc, ssfr_temp[*,0], title='Silicon sensor temperatures',xtitle='UTC (hours)',ytitle='Temperature (Celsius)',yrange=[0,50]
oplot, utc, ssfr_temp[*,1], color=250
legend, ['Zenith','Nadir'],textcolors=[0,250],box=0

plot, utc, ssfr_temp[*,2],title='InGaAs sensor temperatures',xtitle='UTC (hours)',ytitle='Temperature (Celsius)',yrange=[-20,0]
oplot, utc, ssfr_temp[*,3],color=250
legend, ['Zenith','Nadir'],textcolors=[0,250],box=0

plot, utc, ssfr_temp[*,4],title='Box temperatures',xtitle='UTC (hours)',ytitle='Temperature (Celsius)',yrange=[-20,50]
oplot, utc, ssfr_temp[*,5],color=250
legend, ['Box','Box cooler'],textcolors=[0,250],box=0

plot, utc, ssfr_temp[*,6],title='InGaAs sensor housing temperatures',xtitle='UTC (hours)',ytitle='Temperature (Celsius)',yrange=[-20,50]
oplot, utc, ssfr_temp[*,7],color=250
legend, ['Zenith','Nadir'],textcolors=[0,250],box=0

End
