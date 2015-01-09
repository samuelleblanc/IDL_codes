@zensun.pro
@rph2za.pro
@muslope.pro
;@cfg.pro
pro nav_nasa,dir,date,cfg_file,utcin,alt,lat,lon,dc,sza,iza,lats,lons,label,roll=rol,pitch=pit, temperature=temperature, pressure=pressure, water=water,heading=hed,sun=sun,saa=saa,iaa=iaa
; this routine reads in the nav/met data for the ql program

juld=julian_day(fix(strmid(date,0,4)),fix(strmid(date,4,2)),fix(strmid(date,6,2)))

pattern=dir+date+'/'+date+'*nav'
nav=file_search(pattern,count=nnav,/FOLD_CASE)

dim=file_lines(nav[0])+100
utc=fltarr(dim)
alt=fltarr(dim)
lat=fltarr(dim)
lon=fltarr(dim)
pit=fltarr(dim)
rol=fltarr(dim)
hed=fltarr(dim)
temperature=fltarr(dim)
pressure=fltarr(dim)
water=fltarr(dim)
nul=fltarr(13)
null=fltarr(5)
print,'Open: ',nav
openr,up,nav[0],/get_lun
str=''

j=0L
while not eof(up) do begin
  readf,up,str
  if strmid(str,0,1) ne 'C' then goto,ende ;message,'Wrong nav data format.'
  str=strmid(str,1)
  reads,str,rec,juld0,utcs0,altft,tasknots,temp,lat0,lon0,GRDSPDKnots,head,TRK,WNS,WND,HDG,PIT0,ROL0,nul,pres,null,wat; IRS_PLAT_HDG What is this?
  if juld0 ne juld and juld0 ne juld+1 then message,'Wrong date in nav file!'
  if juld0 eq juld+1 then add=24. else add=0.
  utc[j]=utcs0/3600.+add
  alt[j]=altft*0.3048 ; ft --> m
  lat[j]=lat0
  lon[j]=lon0
  pit[j]=pit0
  rol[j]=rol0
  hed[j]=HDG ; PLAT HDG?
  temperature[j]=temp
  pressure[j]=pres
  water[j]=wat
  j=j+1
endwhile
ende:
free_lun,up
print,'Read in ',j-1,' data points from nav/met files.'
dim=j
utc=utc[0:dim-1] & alt=alt[0:dim-1] & lat=lat[0:dim-1] & lon=lon[0:dim-1] & pit=pit[0:dim-1] & rol=rol[0:dim-1] & hed=hed[0:dim-1]

rol0   =cfg(cfg_file,'rolloff') ; get roll offset
pit0   =cfg(cfg_file,'pitchoff') ; get pitch offset
rol0   =float(rol0)
pit0   =float(pit0)

; attitude data / solar zenith angle
print,'Calculate attitude data and SZA...'
;lat lon
if juld eq 0 or juld gt 366 then message,'Wrong definition of Julian day!'
zensun,juld,utc,lat,lon,sza,azimuth,solfac


rph2za,pit+pit0,rol+rol0,hed,ssfrzenith,ssfrazimuth
dc = muslope(sza,azimuth,ssfrzenith,ssfrazimuth) ; directional cosine

dt  =cfg(cfg_file,'dt')     ; get time offset from cfg file
dt  =float(dt)

if keyword_Set(saving) then begin

; write kml file for google earth
openw,uk,dir+date+'\'+date+'.kml',/get_lun
hdr1=['<?xml version="1.0" encoding="UTF-8"?>','<kml xmlns="http://earth.google.com/kml/2.2">','  <Document>',$
     '<name>Paths</name>',$
     '<description>Examples of paths.',$
     ' Note that the tessellate tag is by default set to 0.',$
     ' If you want to create tessellated lines, they must be authored (or edited) directly in KML.</description>',$
     '<Style id="yellowLineGreenPoly">',$
     '<LineStyle>','<color>7f00ffff</color>','<width>4</width>','</LineStyle>',$
     '<PolyStyle>','<color>7f00ff00</color>','</PolyStyle>','</Style>',$
     '<Placemark>','<name>Absolute Extruded</name>',$
     '<description>Transparent green wall with yellow outlines</description>',$
     '<styleUrl>#yellowLineGreenPoly</styleUrl>',$
     '<LineString>','<extrude>1</extrude>','<tessellate>1</tessellate>','<altitudeMode>absolute</altitudeMode>',$
     '<coordinates>']
hdr2=['</coordinates>','</LineString>','</Placemark>']

for i=0,n_elements(hdr1)-1 do begin
  printf,uk,hdr1[i]
endfor

for i=0,n_elements(utc)-1 do begin
  if i mod 10 eq 0 then printf,uk,lon[i],',',lat[i],',',alt[i]
endfor

for i=0,n_elements(hdr2)-1 do begin
  printf,uk,hdr2[i]
endfor

; create labels
;printf,uk,'<?xml version="1.0" encoding="UTF-8"?>'
;printf,uk,'<kml xmlns="http://earth.google.com/kml/2.2">'
;printf,uk,'<Document>'

for i=0,n_elements(lats)-1 do begin
   printf,uk,'<Placemark>'
   printf,uk,'<name>'+label[i]+'</name>'
   printf,uk,'<description>Albedo ground site</description>'
   printf,uk,'<Point> <coordinates>'
   printf,uk,lons[i],',',lats[i],',0'
   printf,uk,'</coordinates> </Point></Placemark>'
endfor

printf,uk,'</Document></kml>'

free_lun,uk
endif

; end kml file module
print, 'julian day', juld
;mm=min(abs(utc-21.0),u)
;print, 'before interpol utc:',utc[u],'sza:',sza[u],'lat:',lat[u],'lon:',lon[u]
;set_plot,'ps'
;loadct, 39, /silent
; device, /encapsulated
; device, /tt_font, set_font='Helvetica Bold'
; device, filename='/home/leblanc/arctas/nasa/20080630/nav_20080630.ps'
; device,/color,bits_per_pixel=8.
; device, xsize=45, ysize=15
;      !p.font=1
;      !p.thick=5
;      !p.charsize=2.5
;      !x.style=1
;      !y.style=1
;      !z.style=1
;      !y.thick=1.8
;      !x.thick=1.8
;      !p.multi=[0,3,1]

;plot, utc, sza, title='sza vs. utc',ytitle='sza',xtitle='utc'
;latb=lat
;lonb=lon
if n_elements(utcin) gt 0 then begin
alt=interpol(alt,utc,utcin-dt)
lat=interpol(lat,utc,utcin-dt)
lon=interpol(lon,utc,utcin-dt)
pit=interpol(pit,utc,utcin-dt)
rol=interpol(rol,utc,utcin-dt)
hed=interpol(hed,utc,utcin-dt)
dc =interpol(dc, utc,utcin-dt)
sza=interpol(sza,utc,utcin-dt)
iza=interpol(ssfrzenith,utc,utcin-dt)
saa=interpol(azimuth,utc,utcin-dt)
iaa=interpol(ssfrazimuth,utc,utcin-dt)
endif else begin
 utcin=utc
endelse
;mm=min(abs(utcin-21.0),u)
;print, 'after interpol utc:',utcin[u],'sza:',sza[u], 'lat:',lat[u],'lon:',lon[u]
;lati=lat
;loni=lon
;oplot, utcin, sza, color=70
;zensun, juld, utcin, lat, lon, sza, azimuth, solfac
;mm=min(abs(utcin-21.0),u)
;oplot, utcin,sza,color=250
;legend,['after zensun','after interpol','after interpol then zensun'], textcolors=[0,70,250],/right,/bottom,charsize=1.8

;plot, utc, latb, title='Latitude', xtitle='utc', ytitle='latitude'
;oplot, utcin, lati, color=70
;oplot, utcin, lat, color=250

;plot, utc, lonb, title='Longitude', xtitle='utc', ytitle='longitude'
;oplot, utcin, loni, color=70
;oplot, utcin, lon, color=250

;print, 'after zensun utc:',utcin[u],'sza:',sza[u],'lat:',lat[u],'lon:',lon[u]
;device,/close
;spawn, 'convert /home/leblanc/arctas/nasa/20080630/nav_20080630.ps /home/leblanc/arctas/nasa/20080630/nav_20080630.png'
;spawn, 'rm -f /home/leblanc/arctas/nasa/20080630/nav_20080630.ps'
;stop
nasa_raw_utc = utc

if keyword_set(saving) then begin
save,nasa_raw_utc,file=dir+date+'/utc_raw_nasa.sav'
save,utcin,pit,rol,hed,file=dir+date+'/pitch-n-roll_nasa.sav'
print,'NASA raw UTC saved in '+dir+date+'/utc_raw_nasa.sav'
 endif

utc=utcin

end
