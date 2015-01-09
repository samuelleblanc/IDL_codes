;+
; NAME: 
;    read_iwg_dc8
;
; PURPOSE: 
;    Read DC-8 IWG data nav file which is broadcast through Iridium
;
; INPUTS: 
; OUTPUTS:
;
; COMMENTS:
;   This version is preliminary; file columns need to be verified with Dave Van Gilst.
;   Format probably as follows:
;Start_UTC,seconds
;Latitude, deg
;Longitude, deg
;GPS_Altitude, m
;Pressure_Altitude, feet
;Radar_Altitude, feet
;Ground_Speed, m/s
;True_Air_Speed, m/s
;Indicated_Air_Speed, kts
;Mach_Number, mach
;Vertical_Speed, m/s
;True_Heading, deg (0-360)
;Track_Angle, deg (0-360)
;Drift_Angle, deg
;Pitch_Angle, deg (+-180)
;Roll_Angle, deg (+-180)
;Static_Air_Temp, Celsius
;Static_Air_Temp_Experimenter, Celsius, Calculated from Rosemount 102E4AL TAT Sensor
;Dew_Point_3-Stage, Celsius, Edgetech 3-Stage Hygrometer
;TAT_Aircraft, Celsius
;TAT_Experimenter, Celsius, Rosemount 102E4AL TAT Sensor
;IR_Surf_Temp, Celsius
;Static_Pressure, mb
;Cabin_Pressure, mb
;Wind_Speed, m/s
;Wind_Direction, deg (0-360)
;Solar_Zenith_Angle, deg
;Aircraft_Sun_Elevation, deg
;Sun_Azimuth, deg
;Aircraft_Sun_Azimuth, deg
;Mixing_Ratio, g/kg, From Edgetech 3-Stage Hygrometer
;Part_Press_Water_Vapor, mb, From Edgetech 3-Stage Hygrometer
;Sat_Vapor_Press_H2O, mb
;Sat_Vapor_Press_Ice, mb
;Relative_Humidity_H2O, Percent, From Edgetech 3-Stage Hygrometer


;
; MODIFICATION HISTORY: 
;     Written by Sebastian Schmidt, 8/15/2013
;-


pro read_iwg_dc8,file,utc,lat,lon,alt,sza,tem

n=file_lines(file)
openr,ua,file,/get_lun & str=''

utc=fltarr(n) ; UTC [h]
tas=fltarr(n) ; true air speed [m/s]
lat=fltarr(n) 
lon=fltarr(n)
alt=fltarr(n)
pit=fltarr(n)
rol=fltarr(n)
hed=fltarr(n)
tem=fltarr(n) ; static temperature [K]
t1 =fltarr(n)
pst=fltarr(n) ; static pressure [hPa]
sza=fltarr(n)
saa=fltarr(n)
irr=fltarr(n)
xxx=fltarr(n)

u0 =0.
u24=0.
for i=0,n-1 do begin

  readf,ua,str
  s0=strsplit(str,'I')
  if n_elements(s0) ne 1 then message,'Wrong file format.'
  s0=strsplit(str,',',/EXTRACT,/PRESERVE_NULL)

  yy=strmid(s0[1],0,4)
  mo=strmid(s0[1],5,2)
  dd=strmid(s0[1],8,2)
  hh=strmid(s0[1],11,2)
  mm=strmid(s0[1],14,2)
  ss=strmid(s0[1],17)
  
  utc[i]=float(hh)+float(mm)/60.+float(ss)/3600.
  if utc[i] lt u0 then u24=u24+24.
  u0=utc[i]
  utc[i]=utc[i]+u24
  if strlen(s0[ 2]) gt 0 then lat[i]=float(s0[2])
  if strlen(s0[ 3]) gt 0 then lon[i]=float(s0[3])
  if strlen(s0[ 5]) gt 0 then alt[i]=float(s0[5]) ; not sure if pressure of GPS altitude

  if strlen(s0[ 9]) gt 0 then tas[i]=float(s0[9]) ; {8,9} are both speeds - which is which
  if strlen(s0[13]) gt 0 then hed[i]=float(s0[13]); {13,14,27} are all some kind of heading (maybe one is yaw?) 

  if strlen(s0[16]) gt 0 then pit[i]=float(s0[16])
  if strlen(s0[17]) gt 0 then rol[i]=float(s0[17])
  if strlen(s0[20]) gt 0 then tem[i]=float(s0[20]) ; 21 is also some kind of a temperature
  if strlen(s0[22]) gt 0 then t1 [i]=float(s0[22])
  if strlen(s0[23]) gt 0 then pst[i]=float(s0[23]) ; What is 25???
  if strlen(s0[29]) gt 0 then sza[i]=float(s0[29]) ; {30} is also something sun-related, but what?? downwelling irradiance maybe?
  if strlen(s0[30]) gt 0 then irr[i]=float(s0[30]) ; This *could* be downwelling irradiance or sun elevation relative to aircraft
  if strlen(s0[32]) gt 0 then saa[i]=float(s0[32]) ; This *could* be solar azimuth relative to aircraft

endfor
free_lun,ua

end