@zensun.pro
@rph2za.pro
@muslope.pro
@cfg.pro
pro nav_p3,dir,ll,date,cfg_file,utcin,alt,lat,lon,dc,sza,iza,roll=rol,pitch=pit,heading=hed,sun=sun,saa=saa,temperature=tem, offsetp=off_pitch ,offsetr=off_roll
; this routine reads in the nav/met data for the ql program
pos = file_search(dir+date+ll+'*.pos', count=n_pos,/FOLD_CASE)
att = file_search(dir+date+ll+'*.att', count=n_att,/FOLD_CASE)
met = file_search(dir+date+ll+'*.met', count=n_met,/FOLD_CASE) 
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
  readf,um,utc2,t0
  if utc0 ne utc2 then message,'Incorrect times in pos/met files.'
  utc[j]=utc0/3600.
  alt[j]=alt0
  lat[j]=lat0
  lon[j]=lon0
  pit[j]=pit0
  rol[j]=rol0
  hed[j]=hed0
  tem[j]=t0
  j=j+1
endwhile
free_lun,up
free_lun,ua
close, /all
print,'Read in ',j-1,' data points from nav/met files.'
dim=j
utc=utc[0:dim-1] & alt=alt[0:dim-1] & lat=lat[0:dim-1] & lon=lon[0:dim-1] & pit=pit[0:dim-1] & rol=rol[0:dim-1] & hed=hed[0:dim-1]

if n_elements(cfg_file) gt 0 then begin
  if n_elements(off_roll) eq 0 then begin
    rol0   =cfg(cfg_file,'rolloff') ; get roll offset
  endif else begin
    rol0   = off_roll
  endelse
  
  if n_elements(off_pitch) eq 0 then begin
    pit0   =cfg(cfg_file,'pitchoff') ; get pitch offset
  endif else begin
    pit0   = off_pitch
  endelse    
  rol0   =float(rol0)
  pit0   =float(pit0)
  dt     =cfg(cfg_file,'dt')     ; get time offset from cfg file
  dt     =float(dt)
  smootha=cfg(cfg_file,'smooth_att')
  if strcmp(smootha,'#',1) then begin
    smootha=0
  endif else begin
    smootha=fix(smootha)
    pit=smooth(pit,smootha)
    rol=smooth(rol,smootha)
    hed=smooth(hed,smootha)
  endelse
endif else begin
  rol0=0 & pit0=0 & dt=0
endelse

; attitude data / solar zenith angle
print,'Calculate attitude data and SZA...'
;lat lon
if juld eq 0 or juld gt 366 then message,'Wrong definition of Julian day!'
zensun,juld,utc,lat,lon,sza,azimuth,solfac
rph2za,pit+pit0,rol+rol0,hed,ssfrzenith,ssfrazimuth
dc = muslope(sza,azimuth,ssfrzenith,ssfrazimuth) ; directional cosine
iza=ssfrzenith


if n_elements(utcin) gt 0 then begin

  flt=where(alt gt -1 and abs(pit) lt 20. and abs(lat) le 90. and abs(lon) le 180.)
  alt=interpol(alt[flt],utc[flt],utcin)
  lat=interpol(lat[flt],utc[flt],utcin)
  lon=interpol(lon[flt],utc[flt],utcin)
  pit=interpol(pit[flt],utc[flt],utcin)
  rol=interpol(rol[flt],utc[flt],utcin)
  hed=interpol(hed[flt],utc[flt],utcin)
  dc =interpol(dc [flt],utc[flt],utcin)
  sza=interpol(sza[flt],utc[flt],utcin)
  iza=interpol(iza[flt],utc[flt],utcin)
  tem=interpol(tem[flt],utc[flt],utcin)
endif else begin
  utcin=utc
endelse

end
