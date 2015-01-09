;+
; NAME:
;   atmosphere
;
; PURPOSE:
;   create input atmosphere file for uvspec
;
; CATEGORY:
;   calnex, atmosphere, uvspec
;
; CALLING SEQUENCE:
;   atmosphere
;
; OUTPUT:
;   plot
;   atmosphere file
;
; KEYWORDS:
;   - none
;
; DEPENDENCIES:
;   - calnex_met
;  
; NEEDED FILES:
;   - met file from flight
;   - sounding
;   - standard atmosphere
;
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
; Written:  Samuel LeBlanc, LASP CU Boulder, August 16th
; Modified: 
;---------------------------------------------------------------------------@magnus.pro
@calnex_met.pro
@magnus.pro
@\\lasp-smb\leblanc\CALNEX\pro\magnus.pro
@\\lasp-smb\leblanc\CALNEX\pro\calnex_met.pro

pro atmosphere
; create atmosphere and cloud from sounding, standard atmosphere,
; and 3D LES cloud
if !VERSION.OS eq 'linux' then linux=1 else linux=0 
date='20100504'
if linux then begin
standard_atmosphere='/home/leblanc/libradtran/libRadtran-1.5-beta/data/atmmod/afglms.dat'
file='/home/leblanc/libradtran/input/aero/atmos_aero_'+date
sounding='/home/leblanc/libradtran/input/aero/sounding_'+date+'.dat'
outfile='/home/leblanc/libradtran/input/aero/met_'+date+'.dat'
endif else begin
standard_atmosphere='\\lasp-smb\leblanc\libradtran\libRadtran-1.5-beta\data\atmmod\afglms.dat''
file='\\lasp-smb\leblanc\libradtran\input\cloud\atmos_cloud_'+date
sounding='\\lasp-smb\leblanc\libradtran\input\aero\sounding_'+date+'.dat'
outfile='\\lasp-smb\leblanc\libradtran\input\aero\met_'+date+'.dat'
endelse


 ; ATMOSPHERE PART
openr,20,standard_atmosphere
atm=fltarr(9,50)
string=''
readf,20,string
readf,20,string
readf,20,atm
close,20
h  =atm[0,*]
p  =atm[1,*]
t  =atm[2,*]
air=atm[3,*]
o3 =atm[4,*]
o2 =atm[5,*]
h2o=atm[6,*]
co2=atm[7,*]
no2=atm[8,*]


; sounding

data_sound=read_ascii(sounding, data_start=4)

hs  =data_sound.field01[1,*]*0.001   ; static height
ts  =data_sound.field01[2,*]+273.15    ; static temperature
ps  =data_sound.field01[0,*]    ; static pressure
rh  =data_sound.field01[4,*]    ; relative humidity
h2os=magnus(rh,ts)  ; relative humidity -> #/cm3


; flight for the P3

calnex_met, date=date, outfile=outfile
met=read_ascii(outfile, data_start=2)
Z_met=met.field1[0,*]
P_met=met.field1[1,*]
T_met=met.field1[2,*]
H_met=met.field1[3,*]


; put atmosphere together from sounding, and from standard atmosphere
set_plot,'x'
device,decomposed=0
loadct, 39
!p.font=1
!p.thick=5
!p.charsize=1.8
!x.style=1
!y.style=1 
!z.style=1
!y.thick=1.8
!x.thick=1.8
!p.color=0
!p.background=255
window,0,retain=2, xsize=1200, ysize=1000

; write/plot atmosphere
zzdim= 49
hh=fltarr(zzdim+1+10+5)
for i=0,zzdim do begin
  hh[i]=i*0.1
endfor
for i=1,10 do begin
  hh[zzdim+i]=5.*i
endfor
for i=1,5 do begin
  hh[zzdim+10+i]=10.*i+50.
endfor

; variables for flight:
; Z_met, P_met, T_met, H_met

; variables from sounding:
; hs, ts, ps, h2os

; variables from standard atmosphere file
; h, p, t, air, o3, o2, h2o, co2, no2

; set all heights following the height created

hh=hh[reverse(sort(hh))] ; descending order

air=interpol(air,h,hh)
o3 =interpol(o3 ,h,hh)
o2 =interpol(o2 ,h,hh)
co2=interpol(co2,h,hh)
no2=interpol(no2,h,hh)

p_atm=interpol(p,h,hh)
t_atm=interpol(t,h,hh)
h_atm=interpol(h2o,h,hh)

P_flight=interpol(P_met, z_met, hh)
t_flight=interpol(T_met, z_met, hh)
h_flight=interpol(H_met, z_met, hh)
;z_met=[0]

p_sounding=interpol(ps, hs ,hh)
t_sounding=interpol(ts, hs, hh)
h_sounding=interpol(h2os, hs, hh)

R=8.314
Av=6.0221415E23

openw,21,file+'_at.dat'
print, 'printing atmosphere file to: '+file+'_at.dat'
printf, 21, '# File generated from the combination of flight data, sounding data and standard atmosphere'
printf, 21, '#      z(km)        p(mb)          T(K)      air(cm-3)      o3(cm-3)       o2(cm-3)      h2o(cm-3)      co2(cm-3)       no2(cm-3)'
na=n_elements(hh)
for i=0, na-1 do begin

  mix_o3=o3[i]/air[i]
  mix_o2=o2[i]/air[i]
  mix_co2=co2[i]/air[i]
  mix_no2=no2[i]/air[i]
  
  if hh[i] le max(Z_met) and hh[i] ge min(Z_met) then begin
    p_good=p_flight[i]
    t_good=t_flight[i]
    h2o_good=h_sounding[i]
  endif else begin
    if hh[i] le max(hs) and hh[i] ge min(hs) then begin
      p_good=p_sounding[i]
      t_good=t_sounding[i]
      h2o_good=h_sounding[i]
    endif else begin
      p_good=p_atm[i]
      t_good=t_atm[i]
      h2o_good=h_atm[i]
    endelse
  endelse
      
  air_good = (p_good*Av)/(t_good*R*10000.)
  o3_good  = air_good*mix_o3
  o2_good  = air_good*mix_o2
  co2_good = air_good*mix_co2
  no2_good = air_good*mix_no2

  ; print to a file
  printf,21,form='(9(x,g13.6))',hh[i],p_good,t_good,air_good,o3_good,o2_good,h2o_good,co2_good,no2_good
endfor
close,21

; plot atmosphere
!p.multi=[0,2,2]
range=[0.0,10.0]
plot,hs,ts,title='Temperature',xtit='Height [km]',ytit='T [K]';,xrange=range,yrange=[260,295]
oplot,h,t,color=130,psym=2
oplot,h_met,T_met, color=250, psym=2

oplot,Z_met,T_met, color=250, psym=2
plot,hs,ps,title='Pressure',xtit='Height [km]',ytit='Pressure [hPa]';,xrange=range, yrange=[500,1050]
oplot,h,p,psym=2,color=130

oplot,Z_met,P_met, color=250, psym=2
plot,hs,h2os,title='Water Amount',xtit='Height [km]',ytit='h2o [#/cm3]';,xrange=range,yrange=[0.1E16, 5E17]
oplot,h,h2o,color=130,psym=2

oplot,Z_met,H_met, color=250, psym=2
plot,hh,o3,title='Ozone',xtit='Height [km]',ytit='Ozone [#/cm3]';,xrange=range
legend,['sounding','atm file','flight'],textcolors=[0,130, 250]

stop
end


