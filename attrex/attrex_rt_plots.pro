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
@attrex_retrieval.pro
pro attrex_rt_plots, wvl, utc, spect, spect_cal, sza, extra, rnav, ssfr_temp, zcg4_temp, ncg4_temp, zcg4, ncg4,alt,utcplotmin=utcplotmin,history=utch,buffer=buffer,$
                     plot_s=plot_s,plot_t=plot_t,retrieval=retrieval,verbose=verbose
;print, 'In the plotting phase'
;print, 'retrieving extraterrestrial radiation'

if n_elements(verbose) lt 1 then verbose=2

if n_elements(plot_s) lt 1 or n_elements(plot_t) lt 1 then begin
plot_s=1 ; 0=no spectra; 1=1 spectrum@beginning; 2=all spectra
plot_t=1 ; 0=no time series; 1=1 time plot@beginning; 2=full time series
endif
plot_v=1 ; vertical profile on/off
online='ssfr@130.134.190.11:/home/ssfr/public_html/' ; publish latest plots here
history=1. ; show 1 hour of history
step   =10  ; 1=show every second, 2=show every second second, ...
wait=0.00 ; waiting time between plots

;; RETRIEVALS
if retrieval gt 0 then begin
rt_wl=[870,1600]
albedo=fltarr(n_elements(rt_wl),n_elements(utc))
;for ii=0,n_elements(rt_wl)-1
  ; Si WL
  nul=min(abs(wvl[*,0]-rt_wl[0]),si_z) ; zenith
  nul=min(abs(wvl[*,2]-rt_wl[0]),si_n) ; nadir
  ; InGaAs WL
  nul=min(abs(wvl[*,1]-rt_wl[1]),in_z) ; zenith
  nul=min(abs(wvl[*,3]-rt_wl[1]),in_n) ; nadir
  albedo[0,*]=spect_cal[*,si_n,2]/spect_cal[*,si_z,0]
  albedo[1,*]=spect_cal[*,in_n,3]/spect_cal[*,in_z,1]
attrex_retrieval,sza,albedo,rt_wl,cod_ice,ref_ice,cod_liquid,ref_liquid,verbose=verbose;,residual_ice,residual_liquid
;print,cod_ice,ref_ice,cod_liquid,ref_liquid
endif




;; PLOTS
if plot_s gt 0 or plot_t gt 0 then begin

ex=read_ascii(extra)

; SETTINGS
; channels (monitor just one wavelength)
; 0: zsi  1:zir  2:nsi  3:nir
join=980
mm=min(abs(reform(wvl[*,0])-680),chz)
mm=min(abs(reform(wvl[*,2])-680),chn)
mm=min(abs(reform(wvl[*,0])-1600),ch2z)
mm=min(abs(reform(wvl[*,2])-1600),ch2n)
mm=min(abs(reform(wvl[*,0])-join),j0)
mm=min(abs(reform(wvl[*,1])-join),j1)
mm=min(abs(reform(wvl[*,2])-join),j2)
mm=min(abs(reform(wvl[*,3])-join),j3)

nn =n_elements(utc)
if n_elements(zcg4) lt nn then begin
  if verbose gt 1 then print,'No CG4',n_elements(zcg4)
  zcg4=fltarr(nn)
  ncg4=fltarr(nn)
endif

device,decomposed=0
loadct,27,/silent
nh=n_elements(utch)
if nh gt 0 then begin
  if verbose gt 1 then begin
    print,'OLD',min(utch),max(utch)
    print,'ADD',min(utc),max(utc)
  endif
  mx=max(utc)
  mn=mx-history
  mm=where(utch ge mn)
  dath =buffer[*,0]
  zenh =buffer[*,1]
  zen2h=buffer[*,2]
  zc4h =buffer[*,3]
  nadh =buffer[*,4]
  nad2h=buffer[*,5]
  nc4h =buffer[*,6]
  taul =buffer[*,7]
  refl =buffer[*,8]
  taui =buffer[*,9]
  refi =buffer[*,10]
  if verbose gt 1 then print,n_elements(utch),n_elements(dath),n_elements(zenh)
  utc_new =reform([utch[mm],utc])
  dat_new =reform([dath[mm],alt])
  zen_new =reform([zenh[mm],reform(spect_cal[*,chz,0])])
  zen2_new=reform([zen2h[mm],reform(spect_cal[*,ch2z,0])])
  zc4_new =reform([zc4h[mm],zcg4])
  nad_new =reform([nadh[mm],reform(spect_cal[*,chn,2])])
  nad2_new=reform([nad2h[mm],reform(spect_cal[*,ch2n,2])])
  nc4_new =reform([nc4h[mm],ncg4])
  taul_new=reform([taul[mm],cod_liquid])
  refl_new=reform([refl[mm],ref_liquid])
  taui_new=reform([taui[mm],cod_ice])
  refi_new=reform([refi[mm],ref_ice])
  if n_elements(zen_new) ne n_elements(dat_new) or n_elements(zen_new) ne n_elements(utc_new) then message,'wrong elements'
  ;spect_cal[*,chz,0]
  utch =utc_new
  dath =dat_new
  zenh =zen_new
  zen2h=zen2_new
  zc4h =zc4_new
  nadh =nad_new
  nad2h=nad2_new
  nc4h =nc4_new
  taul =taul_new
  refl =refl_new
  taui =taui_new
  refi =refi_new
  
endif else begin
  mx=max(utc)
  mn=mx-history
  mm=where(utc ge mn)  
  utch =utc[mm]
  dath =alt[mm]
  zenh =reform(spect_cal[mm,chz,0])
  zen2h=reform(spect_cal[mm,ch2z,0])
  zc4h =zcg4[mm]
  nadh =reform(spect_cal[mm,chn,2])
  nad2h=reform(spect_cal[mm,ch2n,2])
  nc4h =ncg4[mm]
  taul =reform(cod_liquid[mm])
  refl =reform(ref_liquid[mm])
  taui =reform(cod_ice[mm])
  refi =reform(ref_ice[mm]) 

  xsize=600
  ysize=400
  if plot_s gt 0 then begin
  window,0,title='Raw Zenith and Nadir',xsize=xsize,ysize=ysize,retain=2
  window,1,title='Zenith and Nadir',xsize=xsize,ysize=ysize,retain=2
  endif
  if plot_t gt 0 then window,2,title='TIME SERIES',xsize=xsize,ysize=ysize*2.5,xpos=0,retain=2
  if plot_v gt 0 then window,3,title='PROFILE',xsize=xsize,ysize=ysize,retain=2
endelse
if verbose gt 1 then print,'UTCH=',min(utch),max(utch)


utcmax=max(utc)
!y.omargin=[0,0]
!x.omargin=[0,3]
!p.multi=0

if 0 then begin ; temperatures

window, 3, title='SSFR temperatures', xsize=xsize, ysize=ysize*2
!p.multi=[0,2,2]
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
endif



if n_elements(utcplotmin) gt 0 then begin
  start=max([mn,utcplotmin])
  mm=min(abs(utc-start),s0)
endif else begin
  s0=0l
endelse

for i=s0,n_elements(utc)-1,step do begin
  if plot_s gt 0 then begin 
  wset,0
  title = 'Raw counts at UTC:' + strcompress(utc(i),/remove_all)
  yr=[0,2^15-1]
  if i eq s0 or plot_s gt 1 then begin
  plot, wvl[*,0], spect[i,*,0], xtitle = 'Wavelength (nm)', $
   ytitle = 'DN', xrange = [300,2200],$
   xstyle=1,ystyle=9, charsize = 1.5, charthick = 1.5, $
   title = title,thick=3,yrange=yr
  oplot,wvl[*,1],spect[i,*,1],thick=3
  ;axis, yaxis=1,ystyle=1, yrange=yr,/save,color=250, ytitle='Nadir DN'
  oplot,wvl[*,2],spect[i,*,2],color=60,thick=3
  oplot,wvl[*,3],spect[i,*,3],color=60,thick=3
  legend,['Zenith','Nadir'],textcolor=[255,60]
  wset,1
  title = 'Irradiance at UTC:' + strcompress(utc(i),/remove_all)
  yr=[0,2.5]
  plot, wvl[0:j0,0], spect_cal[i,0:j0,0], xtitle = 'Wavelength (nm)', $
   ytitle = 'Irradiance [W m!U-2!N nm!U-1!N]', xrange = [300,2200],$
   xstyle=1,ystyle=9, charsize = 1.5, charthick = 1.5, $
   title = title,thick=3,yrange=yr,/nodata
  oplot,wvl[j1:*,1],spect_cal[i,j1:*,1],thick=3
  oplot,wvl[0:j0,0],spect_cal[i,0:j0,0],thick=3
  ;axis, yaxis=1,ystyle=1, yrange=yr,/save,color=250, ytitle='Nadir Irradiance (W/m!U2!N nm)'
  oplot,wvl[0:j2,2],spect_cal[i,0:j2,2],color=30,thick=3
  oplot,wvl[j3:*,3],spect_cal[i,j3:*,3],color=30,thick=3  

  xyouts, 1500,2.2,'Downwelling Irradiance',charsize=1.5
  xyouts, 1500,2.1,'Upwelling Irradiance',charsize=1.5,color=30
  if 1 then begin           
    w=indgen(340)*5.+350
    ;w1=interpol([spect_cal[i,0:j0,0],spect_cal[i,j1:*,1]],[wvl[0:j0,0],wvl[j1:*,1]],w)
    z1=interpol(spect_cal[i,0:j0,0],wvl[0:j0,0],w)
    z2=interpol(spect_cal[i,j1:*,1],wvl[j1:*,1],w)
    z1[127:*]=0 & z2[0:126]=0
    n1=interpol(spect_cal[i,0:j2,2],wvl[0:j2,2],w)
    n2=interpol(spect_cal[i,j3:*,3],wvl[j3:*,3],w)
    n1[127:*]=0 & n2[0:126]=0
    alb=(n1+n2)/(z1+z2)
    oplot,w,alb,color=60,thick=3
    xyouts, 1500,2.0,'Albedo',charsize=1.5,color=60
  endif  
  if rnav then begin
    oplot,ex.field1[0,*],ex.field1[1,*]*0.001*cos(sza[i]*!dtor),color=200  
    xyouts, 1500,2.3,'Model Irradiance',color=200,charsize=1.5     
  endif
  if i eq s0 and n_elements(online) gt 0 then begin
    p=tvrd(true=1)
    write_png,'ssfr_spectra.png',p
    spawn,'scp ssfr_spectra.png '+online+'&',out,err
  endif
  endif ; only one plot
  endif ; plot_s gt 0

  if plot_t gt 0 then begin
  if i eq s0 or plot_t gt 1 then begin 
  wset,2
  !P.multi=[0,1,4]
  mm=min(abs(utch-utc[i]),now)
  cs=2
  plot,utch,dath*0.001,psym=3,yr=[min(dath),max(dath)]*0.001,title='Altitude',chars=cs,$
    xtit='UTC [h]',ytit='Altitude [km]'
  oplot,[utch[now],utch[now]],[min(dath),max(dath)]*0.001
  plot,utch,zenh,yr=[0,2],psym=3,title='Solar Spectral Irradiance',chars=cs,$
    xtit='UTC [h]',ytit='Irradiance [W m!U-2!N nm!U-1!N]'
  oplot,utch,nadh,psym=3,color=30
  if max(zenh) gt 0.05 then begin
    oplot,utch,nadh/zenh,thick=2,color=60
    legend,['Downwelling','Upwelling','Albedo'],textcolor=[255,30,60]
  endif else begin
    legend,['Downwelling','Upwelling'],textcolor=[255,30]
  endelse
  oplot,[utch[now],utch[now]],[0,2]
  yr=[min(zc4h),max(nc4h)]
  plot,utch,nc4h,psym=3,title='IR Broadband Irradiance',chars=cs,yr=yr,$
    xtit='UTC [h]',ytit='Irradiance [W m!U-2!N]'
  oplot,utch,zc4h,color=30
  legend,['Upwelling','Downwelling'],textcolor=[255,30]
  y0=0
  y1=max([taul,taui,refl,refi])
  plot,utch,taul,xtit='UTC [h]',ytit='COD and R!Deff!N [um]',chars=cs,yr=[y0,y1]
  oplot,utch,taui,color=30
  oplot,utch,refl,color=120
  oplot,utch,refi,color=60
  legend,['tau liq','tau ice','ref liq','ref ice'],textcolor=[255,30,120,60];,textcolor=[255,255,70,70],linesty=[0,2,0,2]
  !P.multi=0
  endif
  if i eq s0 and n_elements(online) gt 0 then begin
    p=tvrd(true=1)
    write_png,'ssfr_time_series.png',p
    spawn,'scp ssfr_time_series.png '+online+'&',out,err
  endif
  endif

  if plot_v gt 0 then begin
    wset,3
    !P.multi=[0,3,1]
    yr=[min(dath),max(dath)]*0.001
    cs=2
    ;plot,nadh/zenh,dath*0.001,yr=yr,psym=3,chars=cs
    ;legend,['Albedo']
    xr=[min([zenh-nadh,zen2h-nad2h]),max([zenh-nadh,zen2h-nad2h])]
    plot,zenh-nadh,dath*0.001,yr=yr,psym=3,chars=cs,ytit='Altitude [km]',xr=xr,xtit='F!DSW!N [W m!U-2!N nm!U-1!N]'
    oplot,[0,2],[dath[now],dath[now]]*0.001
    plots,zenh[now]-nadh[now],dath[now]*0.001,psym=4,syms=3
    legend,['net SW 680'],textcolor=[255,160]
    plot,zen2h-nad2h,dath*0.001,psym=3,chars=cs,yr=yr,xtit='F!DSW!N [W m!U-2!N nm!U-1!N]',ytit='Altitude [km]'  
    oplot,[0,2],[dath[now],dath[now]]*0.001
    plots,zen2h[now]-nad2h[now],dath[now]*0.001,psym=4,syms=3,color=120
    legend,['net SW 1600'],textcolor=[255,160]
    ;oplot,nadh,dath,color=30
    ;oplot,zenh-nadh,dath,color=60
    ;legend,['zenith','nadir','net'],textcolor=[255,30,60]
    x0=min(nc4h-zc4h) & x1=max(nc4h-zc4h)
    if x1-x0 lt 200 then x0=x1-200
    plot,nc4h-zc4h,dath*0.001,yr=yr,psym=3,chars=cs,xr=[x0,x1],xtit='F!DLW!N [W m!U-2!N]',ytit='Altitude [km]'  
    oplot,[0,500],[dath[now],dath[now]]*0.001  
    plots,nc4h[now]-zc4h[now],dath[now]*0.001,psym=4,syms=3,color=120
    ;oplot,zc4h,dath,color=30
    ;oplot,nc4h-zc4h,dath,color=60
    legend,['-net LW'];,textcolor=[255,30,60]
    if i eq s0 and n_elements(online) gt 0 then begin
      p=tvrd(true=1)
      write_png,'ssfr_profile.png',p
      spawn,'scp ssfr_profile.png '+online+'&',out,err
    endif
  endif
  !P.multi=0

  wait,wait       
endfor

buffer=reform([dath,zenh,zen2h,zc4h,nadh,nad2h,nc4h,taul,refl,taui,refi],n_elements(dath),11)
zcg4=0
ncg4=0

endif

End
