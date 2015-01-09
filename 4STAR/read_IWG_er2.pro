;+
; NAME: 
;    read_iwg_er2
;
; PURPOSE: 
;    Read ER-2 IWG data nav file which is broadcast through Iridium
;
; INPUTS: 
; OUTPUTS:
;
; COMMENTS:
;   This version is preliminary; file columns need to be verified with Carl Soerenson.
;
; MODIFICATION HISTORY: 
;     Written by Sebastian Schmidt, 8/15/2013
;-


pro read_iwg_er2,file,utc,lat,lon,alt,sza,ele,saa,tem

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
ele=fltarr(n)
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
  if strlen(s0[ 4]) gt 0 then alt[i]=float(s0[4]) ; {4,5} are GPS and pressure alt - which is which?

  if strlen(s0[ 9]) gt 0 then tas[i]=float(s0[9]) ; {8,9} are both speeds - which is which
  if strlen(s0[13]) gt 0 then hed[i]=float(s0[13]); {13,14,27} are all some kind of heading (maybe one is yaw?) 

  if strlen(s0[16]) gt 0 then pit[i]=float(s0[16])
  if strlen(s0[17]) gt 0 then rol[i]=float(s0[17])
  if strlen(s0[20]) gt 0 then tem[i]=float(s0[20])
  if strlen(s0[22]) gt 0 then t1 [i]=float(s0[22])
  if strlen(s0[25]) gt 0 then pst[i]=float(s0[25])
  if strlen(s0[29]) gt 0 then sza[i]=float(s0[29]) ; {30} is also something sun-related, but what?? downwelling irradiance maybe?
  if strlen(s0[30]) gt 0 then ele[i]=float(s0[30]) ; This *could* be downwelling irradiance or sun elevation relative to aircraft
  if strlen(s0[32]) gt 0 then saa[i]=float(s0[32]) ; This *could* be solar azimuth relative to aircraft
endfor
free_lun,ua

end