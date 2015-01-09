;+
; NAME:
;   calnex_hsrl_p3
;
; PURPOSE:
;   to find coincidences of the hsrl and p3 of flight path
;
; CATEGORY:
;   CALNEX / HSRL 
;
; CALLING SEQUENCE:
;   calnex_hsrl_p3, date
;	- date - date of day to check
;
; OUTPUT:
;   
;
; KEYWORDS:
;   SSFR, P3, CALNEX, HSRL
;
; DEPENDENCIES:
;   get_hsrl.pro - hdf hsrl reader
;
; NEEDED FILES:
;   - hsrl file
;
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
; Written:  Samuel LeBlanc, LASP CU Boulder, May 24th, 2010, Victoria Day
; Modified: June 2nd, 2010
;           -by Samuel LeBlanc
;           added windows to linux portability
;---------------------------------------------------------------------------

@get_hsrl.pro
@\\lasp-smb\leblanc\CALNEX\pro\get_hsrl.pro

pro calnex_hsrl_p3, dateinp

;set windows or linux
if !VERSION.OS eq 'linux' then linux=1 else linux=0
crd=1

if n_elements(dateinp) lt 1 then begin
  date='20100519'
endif else begin
  date=strcompress(string(dateinp),/REMOVE_ALL)
endelse

if linux then begin
  calnex_dir='/data/seven/schmidt/calnex/'
  
  case date of
    '20100516' : begin
      hsrl_path=calnex_dir+'/hsrl/20100516_L1_sub.hdf'
      SSFR_path=calnex_dir+'/p3/20100516/20100516_SP.out'
      pathout='/p3/20100516/20100516_p3_hsrl.out'
    end

    '20100519' : begin
      hsrl_path=calnex_dir+'/hsrl/20100519_L1_sub.hdf'
      SSFR_path=calnex_dir+'/p3/20100519/20100519_SP.out'
      pathout='/p3/20100519/20100519_p3_hsrl.out'
    end

    else: begin
      message, 'not a correct date'
    end
  endcase

;  set_plot, 'X'
endif else begin
  calnex_dir='\\lasp-smb\leblanc\CALNEX\'
  
  case date of
    '20100516' : begin
      hsrl_path=calnex_dir+'\hsrl\20100516_L1_sub.hdf'
      SSFR_path=calnex_dir+'\p3\20100516\20100516_SP.out'
      pathout='\p3\20100516\20100516_p3_hsrl.out'
    end

    '20100519' : begin
      hsrl_path=calnex_dir+'\hsrl\20100519_L1_sub.hdf'
      SSFR_path=calnex_dir+'\p3\20100519\20100519_SP.out'
      pathout='\p3\20100519\20100519_p3_hsrl.out'
    end

    else: begin
      message, 'not a correct date'
    end
  endcase

  set_plot, 'win'
endelse

A = FINDGEN(17) * (!PI*2/16.)
USERSYM, COS(A), SIN(A), /FILL

;get all data from ssfr and hsrl
get_hsrl,hsrl_path,utc_h,alt_h,lat_h,lon_h,gnd,z,ext532,ext1064,T,p,Nprofile,Nabove,aot
restore, SSFR_path

case date of
  '20100516' : begin
    ; subset data from ssfr to only caltech loop for only 20100516
    caltech  = where(utc gt 22.8 and utc lt 23.65 and lon lt -118.0 and lon gt -118.2 and lat lt 34.145 and lat gt 34.135 )  ;northside of loop
    caltech2 = where(utc gt 23.1 and utc lt 23.55 and lat gt 34.0 and lat lt 34.125 and lon lt -118.06)                      ; southside of loop
  end

  '20100519' : begin
    ; subset data  from ssfr for caltech loop on 20100519
    cal_time = where(utc gt 20.05 and utc lt 20.37)
    caltech  = where(utc gt 20.05 and utc lt 20.37 and lat lt 34.145 and lat gt 34.12) 			; northside of loop
    caltech2 = where(utc gt 20.05 and utc lt 20.37 and lat gt 34.09 and lat lt 34.12)			  ; southside of loop
  end

  else: message, 'date not recongnized'
endcase
print, 'filtering points'
;level leg filter
  gradient_alt1 = abs(alt[caltech]-shift(alt[caltech],1))
  gradient_alt2 = abs(alt[caltech]-shift(alt[caltech],2))

  flt =where(gradient_alt1 lt 5. and gradient_alt2 lt 5.)

  gradient2_alt1 = abs(alt[caltech2]-shift(alt[caltech2],1))
  gradient2_alt2 = abs(alt[caltech2]-shift(alt[caltech2],2))

  flt2=where(gradient2_alt1 lt 5. and gradient2_alt2 lt 5.)

; filter of high roll and pitch angles
  ang_high=5.0
  
  flt_ang  = where(abs(rol[caltech[flt]]) lt ang_high and abs(pit[caltech[flt]]) lt ang_high)
  flt_ang2 = where(abs(rol[caltech2[flt2]]) lt ang_high and abs(pit[caltech2[flt2]]) lt ang_high)
set_plot, 'ps'
loadct, 39, /silent
;device, decomposed=0
print, 'init plotting'
!x.style=1
!y.style=1
!p.thick=5.8
!p.color=0
!p.background=255
!p.charsize=1.5

t=caltech[flt[flt_ang]]
t2=caltech2[flt2[flt_ang2]]

print, 'cases'
case date of
  '20100516' : begin 

    lower_leg = where(utc[t] gt 23.1 and utc[t] lt 23.2)
    low1_leg  = where(utc[t] gt 23.2 and utc[t] lt 23.3)
    up1_leg   = where(utc[t] gt 23.3 and utc[t] lt 23.4)
    upper_leg = where(utc[t] gt 23.4 and utc[t] lt 23.5)
    Z_lower   = alt[t[lower_leg[0]]]
    Z_low1    = alt[t[low1_leg[0]]]
    Z_up1     = alt[t[up1_leg[0]]]
    Z_upper   = alt[t[upper_leg[0]]]

    leg1_s = where(utc[t2] gt 23.15 and utc[t2] lt 23.25)
    leg2_s = where(utc[t2] gt 23.25 and utc[t2] lt 23.35)
    leg3_s = where(utc[t2] gt 23.35 and utc[t2] lt 23.45)
    leg4_s = where(utc[t2] gt 23.45 and utc[t2] lt 23.55)
    leg1_s = t2[leg1_s]
    leg2_s = t2[leg2_s]
    leg3_s = t2[leg3_s]
    leg4_s = t2[leg4_s]

    Z_lower_s = alt[leg1_s[0]]
    Z_low1_s  = alt[leg2_s[0]]
    Z_up1_s   = alt[leg3_s[0]]
    Z_upper_s = alt[leg4_s[0]]
    
    leg1 = t[lower_leg]
    leg2 = t[low1_leg]
    leg3 = t[up1_leg]
    leg4 = t[upper_leg]
    
    ;make structure for all the legs
    legs = {leg1:leg1,leg2:leg2,leg3:leg3,leg4:leg4,leg1_s:leg1_s,leg2_s:leg2_s,leg3_s:leg3_s,leg4_s:leg4_s}
    

    ;make caltech loop for hsrl
    z_limits_hsrl = where(z ge z_lower and z le z_upper)
    
    cal_hsrl   = where(lon_h lt -118.0 and lon_h gt -118.2 and lat_h lt 34.145 and lat_h gt 34.135 and utc_h gt 22.8)
    cal_hsrl_s = where(lon_h lt -118.0 and lon_h gt -118.2 and lat_h gt 34.0 and lat_h lt 34.125 and utc_h gt 22.8)
    
    zbelow_attcorr=zspectra[*,leg1]*fac[leg1]*cos(sza[leg3[0]]*!dtor)/cos(sza[leg1]*!dtor)
    nbelow=nspectra[*,leg1]
    
    near={nbelow:nbelow, zbelow_attcorr:zbelow_attcorr}
    
  end ; end date of 20100516

  '20100519' : begin
    leg1 = where( utc[t] gt 20.0 and utc[t] lt 20.09) 
    leg2 = where( utc[t] gt 20.14 and utc[t] lt 20.18)
    leg3 = where( utc[t] gt 20.22 and utc[t] lt 20.27)
    leg4 = where( utc[t] gt 20.33 and utc[t] lt 20.37)
    
    leg1 = t[leg1]
    leg2 = t[leg2]
    leg3 = t[leg3]
    leg4 = t[leg4]

    leg1_s = where( utc[t2] gt 20.09 and utc[t2] lt 20.14) 
    leg2_s = where( utc[t2] gt 20.18 and utc[t2] lt 20.22) 
    leg3_s = where( utc[t2] gt 20.27 and utc[t2] lt 20.33) 
    leg4_s = [n_elements(t2)-1]
    
    leg1_s = t2[leg1_s]
    leg2_s = t2[leg2_s]
    leg3_s = t2[leg3_s]
    leg4_s = t2[leg4_s]

    ;make structure for all the legs
    legs={leg1:leg1,leg2:leg2,leg3:leg3,leg4:leg4,leg1_s:leg1_s,leg2_s:leg2_s,leg3_s:leg3_s,leg4_s:leg4_s}

    ; caltech loop for hsrl
    z_limits_hsrl = where( z ge alt[leg1[0]] and z le alt[leg2[0]]) ;modified for only the first two legs
    cal_hsrl   = where(lon_h lt -118.0 and lon_h gt -118.2 and lat_h lt 34.145 and lat_h gt 34.12)
    cal_hsrl_s = where(lon_h lt -118.0 and lon_h gt -118.2 and lat_h gt 34.09 and lat_h lt 34.12)
    
    ; to map hsrl points onto the lowest leg and also second leg to lowest leg
    near_hsrl=fltarr(n_elements(leg1))
    near_leg2=fltarr(n_elements(leg1))
    extinction=ext532[*,findgen(n_elements(leg1))]*0.0
    zabove_attcorr=zspectra[*,leg1]*0.0
    zbelow_attcorr=zspectra[*,leg1]*0.0
    nabove=nspectra[*,leg1]*0.0
    nbelow=nspectra[*,leg1]*0.0
    
    for i=0, n_elements(leg1)-1 do begin
      r=10000.0
      for j=0, n_elements(cal_hsrl)-3 do begin
        distance = map_2points(lon[leg1[i]],lat[leg1[i]],lon_h[cal_hsrl[j]],lat_h[cal_hsrl[j]])
        if distance[0] le r then begin
          near_hsrl[i]=cal_hsrl[j]
          r=distance[0]
        endif
      endfor
      
      ; get the mean weighted by time value of the extinction coefficients
      extinction[*,i]=(ext532[*,near_hsrl[i]]*utc_h[near_hsrl[i]]+ext532[*,cal_hsrl[j]]*utc_h[cal_hsrl[j]])/utc[leg1[i]]
      
      ; for second leg onto first
      r=10000.0
      for j=0, n_elements(leg2)-1 do begin
        distance = map_2points(lon[leg1[i]],lat[leg1[i]],lon[leg2[j]],lat[leg2[j]])
        if distance[0] le r then begin
          near_leg2[i]=leg2[j]
          r=distance[0]
        endif
      endfor
  
    zabove_attcorr[*,i]=zspectra[*,near_leg2[i]]*fac[near_leg2[i]]*cos(sza[leg3[0]]*!dtor)/cos(sza[near_leg2[i]]*!dtor)
    nabove[*,i]=nspectra[*,near_leg2[i]]
    
    zbelow_attcorr[*,i]=zspectra[*,leg1[i]]*fac[leg1[i]]*cos(sza[leg3[0]]*!dtor)/cos(sza[leg1[i]]*!dtor)
    nbelow[*,i]=nspectra[*,leg1[i]] 
  
    endfor  
   

;    zabove_attcorr=zspectra[*,near_leg2]*fac[near_leg2]*cos(sza[leg3[0]]*!dtor)/cos(sza[near_leg2]*!dtor)
;    nabove=nspectra[*,near_leg2]
;    
;    zbelow_attcorr=zspectra[*,leg1]*fac[leg1]*cos(sza[leg3[0]]*!dtor)/cos(sza[leg1]*!dtor)

;    nbelow=nspectra[*,leg1]    
    
    near={extinction:extinction, zabove_attcorr:zabove_attcorr, nabove:nabove, nbelow:nbelow, zbelow_attcorr:zbelow_attcorr}
        

  end

  else: message, 'not the right day!'

endcase



;;;;;;;;;;;;
;;plotting
;;;;;;;;;;;;
print, 'under plotting'
if (1) then begin
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated
 device, /tt_font, set_font='Helvetica Bold'
 device, filename='/home/leblanc/CALNEX/p3/20100519/20100519_profile.ps'
 device,/color,bits_per_pixel=8.
 device, xsize=20, ysize=20
      !p.font=1
      !p.thick=3
      !p.charsize=1.8
      !x.style=0
      !y.style=0 
      !z.style=1
      !y.thick=1.8
      !x.thick=1.8
      !p.multi=0
plot,lon[cal_time],alt[cal_time], xtitle='Longitude (degrees)', ytitle='Altitude(m)', title='P-3 Flight Profile Over Caltech';, psym=3
ngeo = n_elements(alt[t])
mxa  = max(2500.)
for i=0,ngeo-2 do begin
  cl=fix(alt[t[i]]/mxa*250)
  oplot,lon[t[i:i+1]],alt[t[i:i+1]],color=cl,psym=2,symsize=1
endfor

;oplot, lon[t2], alt[t2], psym=1
;ngeo2 = n_elements(alt[t2])
;mxa2  = max(2500.)
;for i=0,ngeo2-2 do begin
;  cl2=fix(utc[t2[i]]/mxa2*250)
;  oplot,lon[t2[i:i+1]],alt[t2[i:i+1]],color=cl2,psym=1,symsize=1
;endfor
device,/close
spawn, 'convert /home/leblanc/CALNEX/p3/20100519/20100519_profile.ps /home/leblanc/CALNEX/p3/20100519/20100519_profile.png'
spawn, 'rm /home/leblanc/CALNEX/p3/20100519/20100519_profile.ps'

endif
set_plot, 'ps'
;set_plot, 'x'
if (1) then begin
;window, 0
;plot,lon[cal_time],alt[cal_time], xtitle='longitude', ytitle='altitude', title='P-3 Flight Profile Over Caltech', psym=3
ngeo = n_elements(alt[t])
mxa  = max(2500.)
for i=0,ngeo-2 do begin
  cl=fix(utc[t[i]]/mxa*250)
 ; oplot,lon[t[i:i+1]],alt[t[i:i+1]],color=cl,psym=2,symsize=1
endfor

;oplot, lon[t2], alt[t2], psym=1
ngeo2 = n_elements(alt[t2])
mxa2  = max(2500.)
for i=0,ngeo2-2 do begin
  cl2=fix(utc[t2[i]]/mxa2*250)
 ; oplot,lon[t2[i:i+1]],alt[t2[i:i+1]],color=cl2,psym=1,symsize=1
endfor
endif
;stop

if (0) then begin
window, 1, title='lat vs. lon'
plot, lon[t], lat[t], xtitle='longitude', ytitle='latitude', title='P3 flight track', psym =3, yrange=[34.08,34.16], xrange=[-118.3,-117.9], /nodata
oplot, lon_h[cal_hsrl], lat_h[cal_hsrl], color=170, psym=2
oplot, lon[leg1], lat[leg1], color=fix(alt[leg1[0]]/mxa*250), psym=3
oplot, lon[leg2], lat[leg2], color=fix(alt[leg2[0]]/mxa*250), psym=3
oplot, lon[leg3], lat[leg3], color=fix(alt[leg3[0]]/mxa*250), psym=3
oplot, lon[leg4], lat[leg4], color=fix(alt[leg4[0]]/mxa*250), psym=3
for i=0, n_elements(cal_hsrl)-1 do begin
  xyouts, lon_h[cal_hsrl[i]], lat_h[cal_hsrl[i]], strtrim(string(utc_h[cal_hsrl[i]]),2), color=170, orientation=45.
endfor


window, 2, title='Lon vs. UTC'
plot, utc[t],lon[t], xtitle='UTC', ytitle='longitude',title='P3 flight track over time', psym=3
for i=0, ngeo-2 do begin
  cl=fix(alt[t[i]]/mxa*250)
  oplot, utc[t[i:i+1]],lon[t[i:i+1]],color=cl, psym=3
endfor
endif

if (1) then begin
;window, 3, title='Nadir Irradiance'
wl=532
mm=min(abs(nadlambda-wl),wl_i)
;plot, lon[t],nspectra[wl_i,t],xtitle='longitude', ytitle='irradiance', title='P3 Nadir irradiance at 532', psym=3 ,xrange=[-118.3,-117.9]
for i=0, ngeo-2 do begin
  cl=fix(alt[t[i]]/mxa*250)
 ; oplot, lon[t[i:i+1]],nspectra[wl_i,t[i:i+1]],color=cl, psym=3
endfor
endif

if (0) then begin
window, 4 , title='Zenith Irradiance'
wl=532
mm=min(abs(zenlambda-wl),wl_i)
plot, lon[t],zspectra[wl_i,t]*fac[t]*cos(sza[leg3[0]]*!dtor)/cos(sza[t]*!dtor),xtitle='longitude', ytitle='Attitude corrected Irradiance', title='P3 Zenith irradiance at 532', psym=3 ,xrange=[-118.3,-117.9], yrange=[1.0,1.9] 
for i=0, ngeo-2 do begin
  cl=fix(alt[t[i]]/mxa*250)
  oplot, lon[t[i:i+1]],zspectra[wl_i,t[i:i+1]]*fac[t[i:i+1]]*cos(sza[leg3[0]]*!dtor)/cos(sza[t[i:i+1]]*!dtor),color=cl, psym=3
endfor
endif

if (0) then begin
oplot, lon[t2],zspectra[wl_i,t2]*fac[t2]*cos(sza[leg3_s[0]]*!dtor)/cos(sza[t2]*!dtor), psym=1
for i=0, ngeo2-2 do begin
  cl2=fix(alt[t2[i]]/mxa*250)
  oplot, lon[t2[i:i+1]],zspectra[wl_i,t2[i:i+1]]*fac[t2[i:i+1]]*cos(sza[leg3_s[0]]*!dtor)/cos(sza[t2[i:i+1]]*!dtor),color=cl2, psym=1
endfor
endif

;window,15, title='Zenith Irradiance plus'
;plot, utc[leg1],zspectra[wl_i,leg1]*fac[leg1]*cos(sza[leg3[0]]*!dtor)/cos(sza[leg1]*!dtor), xtitle='UTC', ytitle='Attitude corrected Irradiance', title='P3 Zenith Irradiance at 532', xrange=[min(utc[t]),max(utc[t])], xstyle=0, ystyle=8, xmargin=[8,16]
;oplot, utc[leg2], zspectra[wl_i,leg2]*fac[leg2]*cos(sza[leg3[0]]*!dtor)/cos(sza[leg2]*!dtor)
;oplot, utc[leg3], zspectra[wl_i, leg3]*fac[leg3]*cos(sza[leg3[0]]*!dtor)/cos(sza[leg3]*!dtor)
;oplot, utc[leg4], zspectra[wl_i, leg4]*fac[leg4]*cos(sza[leg3[0]]*!dtor)/cos(sza[leg4]*!dtor)
;for i=0, ngeo-2 do begin
;  cl=fix(alt[t[i]]/mxa*250)
;  oplot, utc[t[i:i+1]],zspectra[wl_i,t[i:i+1]]*fac[t[i:i+1]]*cos(sza[leg3[0]]*!dtor)/cos(sza[t[i:i+1]]*!dtor),color=cl, psym=3
;endfor
print, 'using mu_not value of : ',cos(sza[leg3[0]]*!dtor)

;oplot, utc[leg1_s], zspectra[wl_i, leg1_s]*fac[leg1_s]*cos(sza[leg3_s[0]]*!dtor)/cos(sza[leg1_s]*!dtor),psym=6
;oplot, utc[leg2_s], zspectra[wl_i, leg2_s]*fac[leg2_s]*cos(sza[leg3_s[0]]*!dtor)/cos(sza[leg2_s]*!dtor),psym=6
;oplot, utc[leg3_s], zspectra[wl_i, leg3_s]*fac[leg3_s]*cos(sza[leg3_s[0]]*!dtor)/cos(sza[leg3_s]*!dtor),psym=6
;oplot, utc[leg4_s], zspectra[wl_i, leg4_s]*fac[leg4_s]*cos(sza[leg3_s[0]]*!dtor)/cos(sza[leg4_s]*!dtor),psym=6
for i=0, ngeo2-2 do begin
  cl2=fix(alt[t2[i]]/mxa*250)
;  oplot, utc[t2[i:i+1]],zspectra[wl_i,t2[i:i+1]]*fac[t2[i:i+1]]*cos(sza[leg3_s[0]]*!dtor)/cos(sza[t2[i:i+1]]*!dtor),color=cl2, psym=6
endfor



;axis, yaxis=1, ytitle='altitude', ystyle=1, yrange = [400,3000], /save
;oplot, utc, alt, color=200

;axis, max(utc[t])+0.08, yaxis=1, ytitle='mu', ystyle=1, yrange=[0.,1.],/save
;oplot, utc, cos(sza*!dtor)

if (0) then begin
window, 5, title='latitude vs. time'
plot, utc[t],lat[t], xtitle='UTC', ytitle='latitude',title='P3 flight track over time', psym=3, xrange=[22.4,23.8]
for i=0, ngeo-2 do begin
  cl=fix(alt[t[i]]/mxa*250)
  oplot, utc[t[i:i+1]],lon[t[i:i+1]],color=cl, psym=3
endfor
oplot, utc_h[cal_hsrl],lat_h[cal_hsrl], color=250, psym=2
endif

if n_elements(cal_hsrl) gt 1 then begin
;window, 6, title='HSRL extinction coefficient curtain'
dir='/home/leblanc/CALNEX/p3/20100519/'
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated
 device, /tt_font, set_font='Helvetica Bold'
 device, filename=dir+'ext_coef.ps'
 device,/color,bits_per_pixel=8.
 device, xsize=40, ysize=20
      !p.font=1
      !p.thick=5
      !p.charsize=1.8
      !x.style=1
      !y.style=1
      !z.style=1
      !y.thick=1.8
      !x.thick=1.8
      !p.multi=0

lat_cal=34.13694
lon_cal=-118.12583

dist_hsrl=fltarr(12)
for j=0, 11 do if lon_h[cal_hsrl[j]] gt lon_cal then dist_hsrl[j]=map_2points(lon_cal,lat_cal,lon_h[cal_hsrl[j]],lat_h[cal_hsrl[j]],/meters)/1000. else dist_hsrl[j]=map_2points(lon_cal,lat_cal,lon_h[cal_hsrl[j]],lat_h[cal_hsrl[j]],/meters)/(-1000.)

dist_p3=lon[cal_time]
for j=0, n_elements(cal_time)-1 do if lon[cal_time[j]] gt lon_cal then dist_p3[j]=map_2points(lon_cal,lat_cal,lon[cal_time[j]],lat[cal_time[j]],/meters)/1000. else dist_p3[j]=map_2points(lon_cal,lat_cal,lon[cal_time[j]],lat[cal_time[j]],/meters)/(-1000.)

stop

e=ext532[*,cal_hsrl[0:11]]
f=e[z_limits_hsrl,*]
t_e=transpose(e)
contour,t_e,dist_hsrl,z,nlevels=30,/cell_fill,$; title='Extinction coefficents profiles', $
 xtitle='Distance from Caltech (km)', ytitle='Altitude (m)', yrange=[0,3000],xrange=[-7.5,9.5], $; xrange=[-118.21,-118.01], $
 position=[0.12,0.18,0.45,0.9], xticks=5,min_value=min(transpose(e),/nan), max_value=0.45, xstyle=9 ;max(transpose(e),/nan)  ;,xrange=[-118.13,-118.01]

oplot,dist_p3 , alt[cal_time], color=0, thick=6;,psym=2, symsize=2, lon[cal_time]
axis,xaxis=1,xstyle=1,xrange=[-118.21,-118.01],xtitle='Longitude (degrees)'

colorbar, position=[0.18,0.91,0.9,0.93],/right,/vertical, title='km!U-1!N',range=[min(transpose(e),/nan), 0.45], format='(F5.2)'
if crd then begin

calnex_crd, date, utc_crd, ext, ext_ambient, abs

;read in the data from justin langridge
openu, lun, dir+'Cal8_amb_ext.txt',/get_lun
line=' '
i=0
amb_ext_meas=utc_crd ;time
amb_ext_calc=utc_crd ;time
readf, lun, line
while not eof(lun) do begin
  readf, lun, line
  tmp=strsplit(line, /extract,/preserve_nul)
  amb_ext_meas[i]=float(tmp[3])/1000.
  amb_ext_calc[i]=float(tmp[4])/1000.
  i=i+1
endwhile

print, 'doing the crd plotting'
;  calnex_crd, date, utc_crd, ext, ext_ambient, abs
  ext=ext_ambient/1000.
  ext=interpol(ext,utc_crd,utc)
  ctg=where(utc_crd ge min(utc[cal_time],/nan) and utc_crd le max(utc[cal_time],/nan) and amb_ext_meas ne 0.0)
  ;cc=ctg
  ;for i=0,n_elements(ctg)-1 do begin
  ;  nuls=min(abs(utc[cal_time]-utc_crd[ctg[i]]),mm)
  ;  cc[i]=mm
  ;endfor
  dist_p32=interpol(dist_p3,utc[cal_time],utc_crd[ctg]) 
  cal_time2=ctg ; cal_time[ctg]

  A = FINDGEN(17) * (!PI*2/16.)
  USERSYM, COS(A), SIN(A), /FILL
  oplot, dist_p32, alt[cal_time2], color=255,psym=8, symsize=3
  for i=0, n_elements(cal_time2)-1 do begin
    c_n=fix(255/(0.45-min(transpose(e),/nan)) * (ext[cal_time2[i]]-min(transpose(e),/nan)))
    oplot, [dist_p32[i],dist_p32[i]], [alt[cal_time2[i]],alt[cal_time2[i]]], color=c_n,psym=8, symsize=2.5
  endfor
endif

lat_h_n=findgen(60)*((lat_h[cal_hsrl[11]]-lat_h[cal_hsrl[0]])/60)+lat_h[cal_hsrl[0]]
lon_h_n=interpol(lon_h[cal_hsrl[0:11]],lat_h[cal_hsrl[0:11]],lat_h_n)
dist_hsrl_n=interpol(dist_hsrl,lat_h[cal_hsrl[0:11]],lat_h_n)
plot, dist_hsrl,lat_h[cal_hsrl[0:11]], $
 xtitle='Distance from Caltech (km)', ytitle='', xrange=[-7.5,9.5], yrange=[34.09,34.15],position=[0.55,0.18,0.9,0.355], xticks=5,$
 yticks=2, /noerase
if crd then begin
  for i=0, 156 do begin
    c_n=fix(255/(0.45-min(transpose(e),/nan)) * (ext[cal_time[i]]-min(transpose(e),/nan)))
    oplot, [dist_p3[i],dist_p3[i]], [lat[cal_time[i]],lat[cal_time[i]]], color=c_n,psym=8, symsize=2.5
  endfor
  legend,['In situ','HSRL'],box=0,/bottom,psym=[8,3],corners=c1,charsize=1.5
  USERSYM, [0,-1,0,1], [1,0,-1,0], /FILL
  legend,['In situ','HSRL'],box=0,/bottom,psym=[3,8],position=[c1[0],c1[3]],/normal,charsize=1.5
  z_lt=mean(alt[cal_time[0:156]])
  mm=min(abs(z-z_lt),z_ind)
  t_ee=interpol(t_e[*,z_ind+4],lon_h[cal_hsrl[0:11]],lon_h_n)
  USERSYM, [0,-1,0,1], [1,0,-1,0], /FILL
  for i=0,59 do begin
    c_n=fix(255/(0.45-min(transpose(e),/nan)) * (t_ee[i]-min(transpose(e),/nan)))
    oplot, [dist_hsrl_n[i],dist_hsrl_n[i]],[lat_h_n[i],lat_h_n[i]],color=c_n, psym=8, symsize=1.8
  endfor
  USERSYM, COS(A), SIN(A), /FILL
endif
xyouts, 0, lat_cal-0.01, 'Caltech', charsize=1.5
oplot, [0,0],[lat_cal,lat_cal], psym=2

plot, dist_hsrl,lat_h[cal_hsrl[0:11]], title='', $
 xtitle='', ytitle='            Latitude (degrees)', xrange=[-7.5,9.5], yrange=[34.09,34.15],position=[0.55,0.36,0.9,0.535], xticks=1,xtickname=[' ',' '],$
 yticks=2, /noerase
if crd then begin
  for i=157, 456 do begin
    c_n=fix(255/(0.45-min(transpose(e),/nan)) * (ext[cal_time[i]]-min(transpose(e),/nan)))
    oplot, [dist_p3[i],dist_p3[i]], [lat[cal_time[i]],lat[cal_time[i]]], color=c_n,psym=8, symsize=2.5
  endfor
  USERSYM, [0,-1,0,1], [1,0,-1,0], /FILL
  z_lt=mean(alt[cal_time[157:456]])
  mm=min(abs(z-z_lt),z_ind)
  t_ee=interpol(t_e[*,z_ind],lon_h[cal_hsrl[0:11]],lon_h_n)
  for i=0,59 do begin
    c_n=fix(255/(0.45-min(transpose(e),/nan)) * (t_ee[i]-min(transpose(e),/nan)))
    oplot, [dist_hsrl_n[i],dist_hsrl_n[i]],[lat_h_n[i],lat_h_n[i]],color=c_n, psym=8, symsize=1.8
  endfor
endif
xyouts, 0, lat_cal-0.01, 'Caltech', charsize=1.5
oplot, [0,0],[ lat_cal,lat_cal], psym=2

plot, dist_hsrl,lat_h[cal_hsrl[0:11]], title='', $
 xtitle='', ytitle='', xrange=[-7.5,9.5], yrange=[34.09,34.15],position=[0.55,0.54,0.9,0.715], xticks=1,xtickname=[' ',' '],$
 yticks=2, /noerase
if crd then begin
  USERSYM, COS(A), SIN(A), /FILL
  for i=457, 756 do begin
    c_n=fix(255/(0.45-min(transpose(e),/nan)) * (ext[cal_time[i]]-min(transpose(e),/nan)))
    oplot, [dist_p3[i],dist_p3[i]], [lat[cal_time[i]],lat[cal_time[i]]], color=c_n,psym=8, symsize=2.5
  endfor
  USERSYM, [0,-1,0,1], [1,0,-1,0], /FILL
  z_lt=mean(alt[cal_time[457:756]])
  mm=min(abs(z-z_lt),z_ind)
  t_ee=interpol(t_e[*,z_ind],lon_h[cal_hsrl[0:11]],lon_h_n)
  for i=1,59 do begin
    c_n=fix(255/(0.45-min(transpose(e),/nan)) * (t_ee[i]-min(transpose(e),/nan)))
    oplot, [dist_hsrl_n[i],dist_hsrl_n[i]],[lat_h_n[i],lat_h_n[i]],color=c_n, psym=8, symsize=1.8
  endfor
endif
xyouts, 0, lat_cal-0.01, 'Caltech', charsize=1.5
oplot, [0,0],[ lat_cal,lat_cal], psym=2

plot, dist_hsrl,lat_h[cal_hsrl[0:11]],$; title='Extinction coefficents map over Caltech', $
 xtitle='', ytitle='', xrange=[-7.5,9.5], yrange=[34.09,34.15],position=[0.55,0.72,0.9,0.9], xticks=1,xtickname=[' ',' '],$
 yticks=2, /noerase, xstyle=9
if crd then begin
  USERSYM, COS(A), SIN(A), /FILL
  for i=757, n_elements(cal_time)-1 do begin
    c_n=fix(255/(0.45-min(transpose(e),/nan)) * (ext[cal_time[i]]-min(transpose(e),/nan)))
    oplot, [dist_p3[i],dist_p3[i]], [lat[cal_time[i]],lat[cal_time[i]]], color=c_n,psym=8, symsize=2.5
  endfor
  USERSYM, [0,-1,0,1], [1,0,-1,0], /FILL
  z_lt=mean(alt[cal_time[757:*]])
  mm=min(abs(z-z_lt),z_ind)
  t_ee=interpol(t_e[*,z_ind],lon_h[cal_hsrl[0:11]],lon_h_n)
  for i=0,59 do begin
    c_n=fix(255/(0.45-min(transpose(e),/nan)) * (t_ee[i]-min(transpose(e),/nan)))
    oplot, [dist_hsrl_n[i],dist_hsrl_n[i]],[lat_h_n[i],lat_h_n[i]],color=c_n, psym=8, symsize=1.8
  endfor
endif
xyouts, 0, lat_cal-0.01, 'Caltech', charsize=1.5
oplot, [0,0],[ lat_cal,lat_cal], psym=2
axis,xaxis=1,xstyle=1,xrange=[-118.21,-118.01], xtitle='Longitude (degrees)'
device, /close
dir='/home/leblanc/CALNEX/p3/20100519/'
spawn, 'convert "'+dir+'ext_coef.ps" "'+dir+'ext_coef.png"'
spawn, 'rm -f "'+dir+'ext_coef.ps"'
endif

if n_elements(cal_hsrl) eq 1 then begin

window, 7, title='Extinction profile'
e=ext532[*,cal_hsrl]
f=e[z_limits_hsrl,*]
plot, f,z[z_limits_hsrl], title='Extinction coefficient over altitude at mid point', xtitle='Extinction', ytitle='Altitude'
endif

if (0) then begin

window, 8, title='flux divergence'
plot, lon[leg1], (zabove_attcorr[wl_i,*]-nabove[wl_i,*])-(zbelow_attcorr[wl_i,*]-nbelow[wl_i,*]), title='Flux divergence for '+date, ytitle='Irradiance',xtitle='Longitude', ystyle=8, xmargin=[8,8] 
axis, yaxis=1, ytitle='HSRL Extinction', yrange=[8,10.0],/save, color=250
ext=leg1*0.0
for i=0, n_elements(leg1)-1 do ext[i]=total(extinction[z_limits_hsrl,i],/nan)
oplot, lon[leg1], ext, color=250

p=tvrd(true=1)
write_png,calnex_dir+pathout+'.png', p  

endif

if (0) then begin

window, 9, title='Absorption spectrum'
plot, nadlambda,  (zabove_attcorr[*,0]-nabove[*,0])-(zbelow_attcorr[*,0]-nbelow[*,0]),title='Flux divergence spectra for '+date, ytitle='Irradiance', xtitle='Wavelenght (nm)'

p=tvrd(true=1)
write_png, calnex_dir+pathout+'_SP.png', p
endif


stop
save, legs,sza,z,alt,cal_hsrl,ext532,aot,utc, lat, lon, lat_h,lon_h,utc_h,nspectra,zspectra,nadlambda,zenlambda,near, filename=calnex_dir+pathout

stop
end
