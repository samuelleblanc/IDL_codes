;+
; NAME:
;   ingest_nav
;
; PURPOSE:
;   to read in the nav files from the Global Hawk
;
; CATEGORY:
;   ATTREX real time data module navigation
;
; CALLING SEQUENCE:
;   ingest_nav,nav,date,pitchoff,rolloff,utc,lat,lon,alt,sza,iza,dc,rol,pit,hed,pre,tem
;    - where nav is the path to the nav file
;    - where date is the date of the flight - for SZA calculations
;    - pitchoff is the pitch offset from the cfg file
;    - rolloff is the roll offset from the cfg file
;    - utc is the time in hours
;    - lat is the latitude of the GH 
;    - lon is the longitude of the GH
;    - alt is the altitude of the GH
;    - sza is the corresponding solar zenith angle 
;    - iza is the corresponding instrument zenith angle
;    - dc is the cosine of the horizontal SSFR optical inlet plane and the direct beam of the sun
;    - rol is the roll angle
;    - pit is the pitch angle
;    - hed is the global hawk's heading
;    - pre is the ambient pressure in mbar  
;    - tem is the ambient temperature in celsius
; 
; OUTPUT:
;   see above
;
; KEYWORDS:
;   none
;
; DEPENDENCIES:
;   - zensun.pro
;   - rph2za.pro
;   - muslope.pro
; 
; NEEDED FILES:
;   - nav files (*.pos, *.att, *.met)
;   
; EXAMPLE:
;   
;
; MODIFICATION HISTORY:
; Written:  Samuel LeBlanc, LASP CU Boulder, September 20th, 2011
; Modified: 
;           
;---------------------------------------------------------------------------
@zensun.pro
@rph2za.pro
@muslope.pro

pro ingest_nav,nav,date,pitchoff,rolloff,utc,lat,lon,alt,sza,iza,dc,rol,pit,hed,pre,tem 
newline = string(10B)

if 0 then begin
print, newline+'*** Warning currently using the CalNex-P3 nav reader ***'

nav=strsplit(nav,strmid(nav,31,/reverse_offset),/extract,/regex)

pos = file_search(nav+'*.NAV', count=n_pos,/FOLD_CASE) ;mus change *.pos to *.NAV for testing
att = file_search(nav+'*.att', count=n_att,/FOLD_CASE)
met = file_search(nav+'*.met', count=n_met,/FOLD_CASE) 
if n_pos ne 1 then message,'Incorrect number of pos files.'
if n_att ne 1 then message,'Incorrect number of att files.'
if n_met ne 1 then message,'Incorrect number of met files.'

dim=file_lines(pos[0])
utc=fltarr(dim)
alt=fltarr(dim)
lat=fltarr(dim)
lon=fltarr(dim)
pit=fltarr(dim)
rol=fltarr(dim)
hed=fltarr(dim)
tem=fltarr(dim)
pre=fltarr(dim)

juld=julian_day(fix(strmid(date,0,4)),fix(strmid(date,4,2)),fix(strmid(date,6,2)))
j=0l
ut0=0.
openr,up,pos,/get_lun
openr,ua,att,/get_lun
openr,um,met,/get_lun
print,'Open: ',pos,', ',att,', and ',met,'.'
string=''
readf,up,string
uu=strsplit(string,' ',/EXTRACT)
nb=fix(uu[0])
for i=0,nb-2 do begin
  readf,up,string
endfor
readf,ua,string
uu=strsplit(string,' ',/EXTRACT)
na=fix(uu[0])
for i=0,na-2 do begin
  readf,ua,string
endfor
readf,um,string
uu=strsplit(string,' ',/EXTRACT)
nm=fix(uu[0])
for i=0,nm-2 do begin
  readf,um,string
endfor

while not eof(up) do begin
  readf,up,utc0,lat0,lon0,d0,alt0
  readf,ua,utc1,d0,d0,d0,hed0,pit0,rol0,d0,tas0
  if utc0 ne utc1 then message,'Incorrect times in pos/att files.'
  readf,um,utc2,t0,t1,t2,t3,t4,t5,t6,t7,t8,p0
  if utc0 ne utc2 then message,'Incorrect times in pos/met files.'
  utc[j]=utc0/3600.
  alt[j]=alt0
  lat[j]=lat0
  lon[j]=lon0
  pit[j]=pit0
  rol[j]=rol0
  hed[j]=hed0
  tem[j]=t0
  pre[j]=p0
  j=j+1
endwhile
free_lun,up
free_lun,ua
close, /all
print,'Read in ',j-1,' data points from nav/met files.'
dim=j
utc=utc[0:dim-1] & alt=alt[0:dim-1] & lat=lat[0:dim-1] & lon=lon[0:dim-1] & pit=pit[0:dim-1] & rol=rol[0:dim-1] & hed=hed[0:dim-1]
pre=pre[0:dim-1]
;make utc 24 plus
k=where(floor(utc) eq 0, n)
if n gt 0 then begin
  if k[0] ne 0 then utc[k:*]=utc[k:*]+24.
  kk=where(k-shift(k,1) ne 1,nn)
  if nn gt 0 then utc[kk[1]:*]=utc[kk[1]:*]+24.
endif
      

; attitude data / solar zenith angle
print,'Calculate attitude data and SZA...'
if juld eq 0 or juld gt 366 then message,'Wrong definition of Julian day!'
zensun,juld,utc,lat,lon,sza,azimuth,solfac
rph2za,pit+pitchoff,rol+rolloff,hed,ssfrzenith,ssfrazimuth
dc = muslope(sza,azimuth,ssfrzenith,ssfrazimuth) ; directional cosine
iza=ssfrzenith
endif else begin  ; start of ATTREX NAV
f=file_search(nav,count=nf,/FOLD_CASE)
if nf lt 1 then print, newline+'*** NAV file not found: '+nav+' ***'

line=''
;opten file and check the amount of lines in there
openu,lun,f,/get_lun
print, newline+'Opening NAV file :'+f
dim = file_lines(f)
utc = fltarr(dim) & lat = fltarr(dim) & lon = fltarr(dim) & sza = fltarr(dim) & iza = fltarr(dim)
dc  = fltarr(dim) & rol = fltarr(dim) & pit = fltarr(dim) & hed = fltarr(dim) & pre = fltarr(dim)
y   = fltarr(dim) & m   = fltarr(dim) & d   = fltarr(dim) & alt = fltarr(dim) & tem = fltarr(dim)
jul = fltarr(dim)
i=0

;read line by line, then seperate the values
while not eof(lun) do begin
  readf, lun, line
  tmp=strsplit(line,',',/EXTRACT,/PRESERVE_NULL)
  tmp0=strsplit(tmp[1],'-T:',/EXTRACT,/PRESERVE_NULL)
  y[i] = fix(tmp0[0]) & m[i] = fix(tmp0[1]) & d[i] = fix(tmp0[2])
  if i eq 0 then date=strjoin([tmp0[0],tmp0[1],tmp0[2]])
  jul[i] = julian_day(y[i],m[i],d[i])	  
  plus=float(jul[i]-jul[0])*24.
  utc[i] = float(tmp0[3])+float(tmp0[4])/60. + float(tmp0[5])/3600. + plus 
  lat[i] = float(tmp[2])  & lon[i] = float(tmp[3])  & alt[i] = float(tmp[4])
  hed[i] = float(tmp[13]) & rol[i] = float(tmp[17]) & pit[i] = float(tmp[16])
  tem[i] = float(tmp[20]) & pre[i] = float(tmp[23]) & sza[i] = float(tmp[29]) 
  i=i+1
endwhile
; attitude data / solar zenith angle^M
print,'Calculate attitude data and SZA...'
juld=julian_day(fix(strmid(date,0,4)),fix(strmid(date,4,2)),fix(strmid(date,6,2)))
if juld eq 0 or juld gt 366 then message,'Wrong definition of Julian day!'
zensun,juld,utc,lat,lon,sza,azimuth,solfac
rph2za,pit+pitchoff,rol+rolloff,hed,ssfrzenith,ssfrazimuth
dc = muslope(sza,azimuth,ssfrzenith,ssfrazimuth) ; directional cosine^M
iza=ssfrzenith

close,lun
free_lun,lun
endelse
end

FUNCTION BODHAINE,WV0,PZ1,PZ2
;WV0 = wavelength (in microns)
;PZ1 = Pressure of lower layer (hPa)
;PZ2 = Pressure of upper layer (hPa; PZ1 > PZ2)

num=1.0455996 - 341.29061*WV0^(-2.) - 0.90230850*WV0^(2.)
den=1 + 0.0027059889*WV0^(-2.) - 85.968563*WV0^(2.)
tauray =0.00210966*(num/den)*(PZ1-PZ2)/1013.25

return,tauray
end
