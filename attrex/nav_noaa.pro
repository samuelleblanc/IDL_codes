@zensun.pro
@rph2za.pro
@muslope.pro
@cfg.pro
pro nav_noaa,dir,ll,date,cfg_file,utcin,alt,lat,lon,dc,sza,iza,roll=rol,pitch=pit,heading=hed,sun=sun,saa=saa
; this routine reads in the nav/met data for the ql program
pos = file_search(dir+date+ll+'*.pos', count=n_pos,/FOLD_CASE)
att = file_search(dir+date+ll+'*.att', count=n_att,/FOLD_CASE)
if n_pos ne 1 then message,'Incorrect number of nav files (pos).'
if n_att ne 1 then message,'Incorrect number of nav files (att).'

dim=file_lines(pos[0])
utc=fltarr(dim)
alt=fltarr(dim)
lat=fltarr(dim)
lon=fltarr(dim)
pit=fltarr(dim)
rol=fltarr(dim)
hed=fltarr(dim)

juld=julian_day(fix(strmid(date,0,4)),fix(strmid(date,4,2)),fix(strmid(date,6,2)))
j=0l
ut0=0.
openr,up,pos,/get_lun
openr,ua,att,/get_lun
print,'Open: ',pos,' and ',att,'.'
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

while not eof(up) do begin
  readf,up,utc0,lat0,lon0,d0,alt0
  readf,ua,utc1,d0,d0,d0,hed0,pit0,rol0,d0,tas0
  if utc0 ne utc1 then message,'Incorrect times in pos/att files.'
  utc[j]=utc0/3600.
  alt[j]=alt0
  lat[j]=lat0
  lon[j]=lon0
  pit[j]=pit0
  rol[j]=rol0
  hed[j]=hed0
  j=j+1
endwhile
free_lun,up
free_lun,ua
print,'Read in ',j-1,' data points from nav/met files.'
dim=j
utc=utc[0:dim-1] & alt=alt[0:dim-1] & lat=lat[0:dim-1] & lon=lon[0:dim-1] & pit=pit[0:dim-1] & rol=rol[0:dim-1] & hed=hed[0:dim-1]

if n_elements(cfg_file) gt 0 then begin
  rol0   =cfg(cfg_file,'rolloff') ; get roll offset
  pit0   =cfg(cfg_file,'pitchoff') ; get pitch offset
  rol0   =float(rol0)
  pit0   =float(pit0)
  dt     =cfg(cfg_file,'dt')     ; get time offset from cfg file
  dt     =float(dt)
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
  alt=interpol(alt,utc,utcin)
  lat=interpol(lat,utc,utcin)
  lon=interpol(lon,utc,utcin)
  pit=interpol(pit,utc,utcin)
  rol=interpol(rol,utc,utcin)
  hed=interpol(hed,utc,utcin)
  dc =interpol(dc ,utc,utcin)
  sza=interpol(sza,utc,utcin)
  iza=interpol(iza,utc,utcin)
endif else begin
  utcin=utc
endelse

end
