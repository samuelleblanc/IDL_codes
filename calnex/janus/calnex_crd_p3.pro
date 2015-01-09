;+
; NAME:
;   calnex_crd_p3
;
; PURPOSE:
;   to build the file used for retrieving aerosol properties. from crd extinction coefficients
;
; CATEGORY:
;   CALNEX
;
; CALLING SEQUENCE:
;   calnex_crd_p3, date
;	- date - date of day to check
;
; OUTPUT:
;   
;
; KEYWORDS:
;   none
;
; DEPENDENCIES:
;   calnex_crd
;
; NEEDED FILES:
;   - crd file
;
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
; Written:  Samuel LeBlanc, LASP CU Boulder, July 15th, 2011, In Kingston, ON
; Modified: 
;           
;           
;---------------------------------------------------------------------------

@calnex_crd.pro

pro calnex_crd_p3, dateinp

;set windows or linux
if !VERSION.OS eq 'linux' then linux=1 else linux=0
crd=1

if n_elements(dateinp) lt 1 then begin
  date='20100519'
endif else begin
  date=strcompress(string(dateinp),/REMOVE_ALL)
endelse

if linux then begin
  calnex_dir_seb='/data/seven/schmidt/calnex/'
  calnex_dir='/home/leblanc/CALNEX/'
  crd_path=calnex_dir+'p3/'+date+'/CRDext_'
  SSFR_path=calnex_dir_seb+'p3/'+date+'/'+date+'_SP.out'
  pathout=calnex_dir+'p3/'+date+'/'+date+'_p3_crd.out'

;  set_plot, 'X'
endif else begin
message, 'not for windows'
;  calnex_dir='\\lasp-smb\leblanc\CALNEX\'
;  crd_path=calnex_dir+'\hsrl\20100516_L1_sub.hdf'
;  SSFR_path=calnex_dir+'\p3\20100516\20100516_SP.out'
;  pathout='\p3\20100516\20100516_p3_hsrl.out'
  set_plot, 'win'
endelse

A = FINDGEN(17) * (!PI*2/16.)
USERSYM, COS(A), SIN(A), /FILL

;get all data from ssfr and hsrl
calnex_crd, dateinp, time, ext, ext_ambient, abs,/no_pas
ext_ambient=ext_ambient/1000.0
restore, SSFR_path
ext_ambient=interpol(ext_ambient, time, utc)
case date of
  '20100504' : begin
    cal_time  = where(utc gt 19.8 and utc lt 21.1)
    caltech  = where(utc gt 19.8 and utc lt 21.1)
    caltech2 = where(utc gt 19.8 and utc lt 21.1)
  end
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
  '20100504' : begin
    leg1 = where( utc[t] gt 20.94 and utc[t] lt 20.99) 
    leg2 = where( utc[t] gt 20.83 and utc[t] lt 20.90)
    leg3 = where( utc[t] gt 20.73 and utc[t] lt 20.79)
    leg4 = where( utc[t] gt 20.60 and utc[t] lt 20.69)
    leg1 = t[leg1]
    leg2 = t[leg2]
    leg3 = t[leg3]
    leg4 = t[leg4]
    legs={leg1:leg1,leg2:leg2,leg3:leg3,leg4:leg4}
  end
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
  end ; end date of 20100519

  else: message, 'not the right day!'

endcase
  ; make z_heights from the altitudes of the plane between leg1 and leg2
  if leg1[n_elements(leg1)-1] gt leg2[0] then begin
    low=leg2
    high=leg1 
  endif else begin
    low=leg1
    high=leg2
  endelse    ; from last point on bottom leg to first on top leg
  
  ind_heights=[low[n_elements(low)-1],high[0]]
  ; make the reference extinction profile from the crd
  ext_profile=ext_ambient[ind_heights[0]:ind_heights[1]]
  ext_profile[0]=mean(ext_ambient[low],/nan)
  ext_profile[n_elements(ext_profile)-1]=mean(ext_ambient[high],/nan)
  if alt[ind_heights[0]] gt alt[ind_heights[1]] then begin
    z_heights=reverse(alt[ind_heights[0]:ind_heights[1]]) 
	ind_heights=reverse(ind_heights)
  endif else z_heights=alt[ind_heights[0]:ind_heights[1]]
  n_heights=n_elements(z_heights)
  z=z_heights
  ; interpol to get rid of the nans
  ext_profile=interpol(ext_profile,z,z)
  ext_ambient[leg1]=interpol(ext_ambient[leg1],utc[leg1],utc[leg1])
  ext_ambient[leg2]=interpol(ext_ambient[leg2],utc[leg2],utc[leg2])
  
  ; make sure we don't get any negative ext
  nul=where(ext_ambient[leg1] le 0.,m)
  if m gt 0 then ext_ambient[leg1[nul]]=0.
  print, m
  nul=where(ext_ambient[leg2] le 0.,m)
  if m gt 0 then ext_ambient[leg2[nul]]=0.
  print, m
  
  
  
  ; to map second leg to lowest leg, and initiate the extinction map from crd
  near_leg2=fltarr(n_elements(leg1))
  extinction=fltarr(n_heights,n_elements(leg1))
  zabove_attcorr=zspectra[*,leg1]*0.0
  zbelow_attcorr=zspectra[*,leg1]*0.0
  nabove=nspectra[*,leg1]*0.0
  nbelow=nspectra[*,leg1]*0.0
  	
    for i=0, n_elements(leg1)-1 do begin
      r=10000.0
	  
      ; for second leg onto first
      r=10000.0
      for j=0, n_elements(leg2)-1 do begin
        distance = map_2points(lon[leg1[i]],lat[leg1[i]],lon[leg2[j]],lat[leg2[j]])
        if distance[0] le r then begin
          near_leg2[i]=leg2[j]
          r=distance[0]
        endif
      endfor
      
      ; scale the reference extinction profile by the change over leg1 and leg2
      for j=0,n_heights-1 do begin
	      extinction[j,i]=ext_profile[j]*((ext_ambient[leg1[i]]/ext_profile[0])*(1-j/n_heights)+(ext_ambient[near_leg2[i]]/ext_profile[n_heights-1])*(j/n_heights))
	      ;if extinction[j,i] lt 0 then stop
      endfor
  
      zabove_attcorr[*,i]=zspectra[*,near_leg2[i]]*fac[near_leg2[i]]*cos(sza[leg3[0]]*!dtor)/cos(sza[near_leg2[i]]*!dtor)
      nabove[*,i]=nspectra[*,near_leg2[i]]
      zbelow_attcorr[*,i]=zspectra[*,leg1[i]]*fac[leg1[i]]*cos(sza[leg3[0]]*!dtor)/cos(sza[leg1[i]]*!dtor)
      nbelow[*,i]=nspectra[*,leg1[i]] 
    endfor  
near={extinction:extinction, zabove_attcorr:zabove_attcorr, nabove:nabove, nbelow:nbelow, zbelow_attcorr:zbelow_attcorr}

;;;;;;;;;;;;
;;plotting
;;;;;;;;;;;;
print, 'under plotting'
if (1) then begin
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated
 device, /tt_font, set_font='Helvetica Bold'
 device, filename='/home/leblanc/CALNEX/p3/'+date+'/'+date+'_profile.ps'
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
spawn, 'convert /home/leblanc/CALNEX/p3/'+date+'/'+date+'_profile.ps /home/leblanc/CALNEX/p3/'+date+'/'+date+'_profile.png'
spawn, 'rm /home/leblanc/CALNEX/p3/'+date+'/'+date+'_profile.ps'

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
;oplot, utc_h[cal_hsrl],lat_h[cal_hsrl], color=250, psym=2
endif

if (1) then begin
dir='/home/leblanc/CALNEX/p3/'+date+'/'

;read in the data from justin langridge
openu, lun, dir+'Cal8_amb_ext.txt',/get_lun
line=' '
i=0
amb_ext_meas=time
amb_ext_calc=time
readf, lun, line
while not eof(lun) do begin
  readf, lun, line
  tmp=strsplit(line, /extract,/preserve_nul)
  amb_ext_meas[i]=float(tmp[3])/1000.
  amb_ext_calc[i]=float(tmp[4])/1000.
  i=i+1
endwhile

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

e=extinction
t_e=transpose(e)
contour,t_e,lon[leg1],z,nlevels=30,/cell_fill, title='Extinction coefficents profiles', $
 xtitle='Longitude (degrees)', ytitle='Altitude (m)', yrange=[0,3000], xrange=[-118.21,-118.01], $
 position=[0.12,0.18,0.45,0.9], xticks=4,min_value=-0.02, max_value=0.45;max(transpose(e),/nan)  ;,xrange=[-118.13,-118.01]

oplot, lon[cal_time], alt[cal_time], color=0, thick=6;,psym=2, symsize=2
colorbar, position=[0.18,0.91,0.9,0.93],/right,/vertical, title='km!U-1!N',range=[-0.02, 0.45], format='(F5.2)'

  A = FINDGEN(17) * (!PI*2/16.)
  USERSYM, COS(A), SIN(A), /FILL
  oplot, lon[cal_time], alt[cal_time], color=255,psym=8, symsize=3
  ctg=where(amb_ext_meas[cal_time] ne 0.0)
  cal_time2=cal_time[ctg]
  for i=0, n_elements(cal_time2)-1 do begin
;    c_n=fix(255/(0.45-min(transpose(e),/nan)) * (ext_ambient[cal_time[i]]-min(transpose(e),/nan)))
    c_n=fix(255/(0.45+0.02) * (amb_ext_meas[cal_time2[i]]+0.02))
    oplot, [lon[cal_time2[i]],lon[cal_time2[i]]], [alt[cal_time2[i]],alt[cal_time2[i]]], color=c_n,psym=8, symsize=2.5
  endfor

plot, lon[leg1],lat[leg1], $
 xtitle='Longitude (degrees)', ytitle='', xrange=[-118.21,-118.01], yrange=[34.09,34.15],position=[0.55,0.18,0.9,0.355], xticks=4,$
 yticks=2, /noerase
  for i=0, 156 do begin
    c_n=fix(255/(0.45-min(transpose(e),/nan)) * (ext_ambient[cal_time[i]]-min(transpose(e),/nan)))
    oplot, [lon[cal_time[i]],lon[cal_time[i]]], [lat[cal_time[i]],lat[cal_time[i]]], color=c_n,psym=8, symsize=2.5
  endfor
  ;legend,['In situ','HSRL'],box=0,/bottom,psym=[8,3],corners=c1,charsize=1.5
  USERSYM, [0,-1,0,1], [1,0,-1,0], /FILL
  ;legend,['In situ','HSRL'],box=0,/bottom,psym=[3,8],position=[c1[0],c1[3]],/normal,charsize=1.5
  z_lt=mean(alt[cal_time[0:156]])
  mm=min(abs(z-z_lt),z_ind)
  USERSYM, [0,-1,0,1], [1,0,-1,0], /FILL
  for i=0,59 do begin
    c_n=fix(255/(0.45-min(transpose(e),/nan)) * (t_e[i]-min(transpose(e),/nan)))
    oplot, [lon[leg1[i]],lon[leg1[i]]],[lat[leg1[i]],lat[leg1[i]]],color=c_n, psym=8, symsize=1.8
  endfor
  USERSYM, COS(A), SIN(A), /FILL

xyouts, lon_cal, lat_cal-0.01, 'Caltech', charsize=1.5
oplot, [lon_cal,lon_cal],[lat_cal,lat_cal], psym=2

plot, lon[leg1],lat[leg1], title='', $
 xtitle='', ytitle='            Latitude (degrees)', xrange=[-118.21,-118.01], yrange=[34.09,34.15],position=[0.55,0.36,0.9,0.535], xticks=1,xtickname=[' ',' '],$
 yticks=2, /noerase
  for i=157, 456 do begin
    c_n=fix(255/(0.45-min(transpose(e),/nan)) * (ext_ambient[cal_time[i]]-min(transpose(e),/nan)))
    oplot, [lon[cal_time[i]],lon[cal_time[i]]], [lat[cal_time[i]],lat[cal_time[i]]], color=c_n,psym=8, symsize=2.5
  endfor
  USERSYM, [0,-1,0,1], [1,0,-1,0], /FILL
  z_lt=mean(alt[cal_time[157:456]])
  mm=min(abs(z-z_lt),z_ind)
  for i=0,59 do begin
    c_n=fix(255/(0.45-min(transpose(e),/nan)) * (t_e[i]-min(transpose(e),/nan)))
    oplot, [lon[leg1[i]],lon[leg1[i]]],[lat[leg1[i]],lat[leg1[i]]],color=c_n, psym=8, symsize=1.8
  endfor

xyouts, lon_cal, lat_cal-0.01, 'Caltech', charsize=1.5
oplot, [lon_cal,lon_cal],[ lat_cal,lat_cal], psym=2

plot, lon[leg1],lat[leg1], title='', $
 xtitle='', ytitle='', xrange=[-118.21,-118.01], yrange=[34.09,34.15],position=[0.55,0.54,0.9,0.715], xticks=1,xtickname=[' ',' '],$
 yticks=2, /noerase
  USERSYM, COS(A), SIN(A), /FILL
  for i=457, 756 do begin
    c_n=fix(255/(0.45-min(transpose(e),/nan)) * (ext_ambient[cal_time[i]]-min(transpose(e),/nan)))
    oplot, [lon[cal_time[i]],lon[cal_time[i]]], [lat[cal_time[i]],lat[cal_time[i]]], color=c_n,psym=8, symsize=2.5
  endfor
  USERSYM, [0,-1,0,1], [1,0,-1,0], /FILL
  z_lt=mean(alt[cal_time[457:756]])
  mm=min(abs(z-z_lt),z_ind)
  for i=1,59 do begin
    c_n=fix(255/(0.45-min(transpose(e),/nan)) * (t_e[i]-min(transpose(e),/nan)))
    oplot, [lon[leg1[i]],lon[leg1[i]]],[lat[leg1[i]],lat[leg1[i]]],color=c_n, psym=8, symsize=1.8
  endfor

xyouts, lon_cal, lat_cal-0.01, 'Caltech', charsize=1.5
oplot, [lon_cal,lon_cal],[ lat_cal,lat_cal], psym=2

plot, lon[leg1],lat[leg1], title='Extinction coefficents map over Caltech', $
 xtitle='', ytitle='', xrange=[-118.21,-118.01], yrange=[34.09,34.15],position=[0.55,0.72,0.9,0.9], xticks=1,xtickname=[' ',' '],$
 yticks=2, /noerase

  USERSYM, COS(A), SIN(A), /FILL
  for i=757, n_elements(cal_time)-1 do begin
    c_n=fix(255/(0.45-min(transpose(e),/nan)) * (ext[cal_time[i]]-min(transpose(e),/nan)))
    oplot, [lon[cal_time[i]],lon[cal_time[i]]], [lat[cal_time[i]],lat[cal_time[i]]], color=c_n,psym=8, symsize=2.5
  endfor
  USERSYM, [0,-1,0,1], [1,0,-1,0], /FILL
  z_lt=mean(alt[cal_time[757:*]])
  mm=min(abs(z-z_lt),z_ind)
  for i=0,59 do begin
    c_n=fix(255/(0.45-min(transpose(e),/nan)) * (t_e[i]-min(transpose(e),/nan)))
    oplot, [lon[leg1[i]],lon[leg1[i]]],[lat[leg1[i]],lat[leg1[i]]],color=c_n, psym=8, symsize=1.8
  endfor

xyouts, lon_cal, lat_cal-0.01, 'Caltech', charsize=1.5
oplot, [lon_cal,lon_cal],[ lat_cal,lat_cal], psym=2

device, /close
dir='/home/leblanc/CALNEX/p3/'+date+'/'
spawn, 'convert "'+dir+'ext_coef.ps" "'+dir+'ext_coef.png"'
spawn, 'rm -f "'+dir+'ext_coef.ps"'
endif

if (0) then begin

window, 9, title='Absorption spectrum'
plot, nadlambda,  (zabove_attcorr[*,0]-nabove[*,0])-(zbelow_attcorr[*,0]-nbelow[*,0]),title='Flux divergence spectra for '+date, ytitle='Irradiance', xtitle='Wavelenght (nm)'

p=tvrd(true=1)
write_png, calnex_dir+pathout+'_SP.png', p
endif


stop
save, legs,sza,z,alt,extinction,utc, lat, lon,nspectra,zspectra,nadlambda,zenlambda,near, filename=pathout

stop
end
