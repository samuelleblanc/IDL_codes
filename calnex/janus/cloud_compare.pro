; procedure to compare multiple platforms measuring cloud data on may 16th, 2010 during CALNEX
;     - Ship
;     - P3
;     - GOES - NESDIS
;     - GOES - LARC
;     - MODIS - TERRA
;     - MODIS - AQUA
@zensun.pro
@read_modis.pro
@nesdis_reader.pro
@goes_reader.pro

pro cloud_compare
close,/all

date='20100516'
doy=julday(fix(strmid(date,4,2)),fix(strmid(date,6,2)),fix(strmid(date,0,4)))-julday(1,0,fix(strmid(date,0,4)))

if !VERSION.OS eq 'linux' then linux=1 else linux=0
if linux then begin
d='/data/seven/schmidt/calnex/'
ll='/'
ship_dir=d+'ship'+ll+date+ll
p3_dir= d+'p3'+ll+date+ll
goes_nesdis_dir= '/home/leblanc/CALNEX/goes/NESDIS/'
goes_larc_dir=d+'goes/larc/136/'
dir='/home/leblanc/CALNEX/clouds/'
endif else begin
ll='\'
dir='\\lasp-smb\leblanc\CALNEX\clouds\'

endelse


;; restore the data from various places

if (0) then begin

if not linux then message, '**** MUST BE RUN ON SEVEN ****'

; ship

; files:
; RT - retrieval products: lat, lon, reff, reffunc, status, tau, tauunc, tmhrs
print, 'getting ship'
restore, ship_dir+date+'_RT.out'
ship_lat=lat & ship_lon=lon & ship_utc=tmhrs
ship_reff=reff & ship_tau=tau 
ship_reffunc=reffunc & ship_tauunc=tauunc
; 24 hour module (time after midnight gets added as UTC>24)
nn=n_elements(ship_utc)& utc0=0. & xtra=0
for i=0L,nn-1 do begin
  if ship_utc[i] lt utc0 then xtra=i
  utc0=ship_utc[i]
endfor
ship_utc[xtra:nn-1]=ship_utc[xtra:nn-1]+24.
nan=where(ship_tau eq -9999., ct)
if ct gt 0 then ship_tau[nan]=!values.f_nan
nan=where(ship_reff eq -9999., ct)
if ct gt 0 then ship_reff[nan]=!values.f_nan
nan=where(ship_tauunc eq -9999., ct)
if ct gt 0 then ship_tauunc[nan]=!values.f_nan
nan=where(ship_reffunc eq -9999., ct)
if ct gt 0 then ship_reffunc[nan]=!values.f_nan
; get sza
utct=ship_utc
zensun,doy,utct,ship_lat,ship_lon,ship_sza
ship_mu=cos(!pi/180.*ship_sza)

max_lat=34. & min_lat= 33. & max_lon= -118. & min_lon= -120.

; P3
; there's two retrievals for the p3 (seb and sam's)
print, 'getting p3'
; sam's
; altf, latf, lonf, reff, szaf, tauf, utcf
restore, p3_dir+date+'_RI_processed.out'
sam_lat=latf & sam_lon=lonf & sam_alt=altf & sam_utc=utcf
sam_reff=reff & sam_sza=szaf & sam_tau=tauf

; seb's
; latf, lonf, refg, taug, utcf
restore, '/data/seven/schmidt/calnex/rt/clouds/16.out'
seb_lat=latf & seb_lon=lonf &  seb_utc=utcf
seb_reff=refg &  seb_tau=taug

; set utc time lapse for sam retrieval
fl=where(sam_utc gt min(seb_utc) and sam_utc lt max(seb_utc))
sam_lat=sam_lat[fl] & sam_lon=sam_lon[fl] & sam_alt=sam_alt[fl] & sam_utc=sam_utc[fl]
sam_reff=sam_reff[fl] & sam_sza=sam_sza[fl] & sam_tau=sam_tau[fl]

; possible to add wind speed & directions from .ict.wnd file in /home/leblanc/CALNEX/p3/20100516/


; GOES - NESDIS - read hdf files
print, 'getting GOES-NESDIS'
f=file_search(goes_nesdis_dir+'*'+strtrim(string(doy),2)+'*.hdf',count=ct)
f=f[sort(f)]
nesdis_reader,f[0], nes_utc, nes_lat, nes_lon, nes_tau, nes_ref, nes_lwp
for i=1, ct-1 do begin
  nesdis_reader,f[i], nes1_utc, nes1_lat, nes1_lon, nes1_tau, nes1_ref, nes1_lwp
  nes_utc=[nes_utc,nes1_utc] & nes_lat=[[[nes_lat]],[[nes1_lat]]] & nes_lon=[[[nes_lon]],[[nes1_lon]]]
  nes_tau=[[[nes_tau]],[[nes1_tau]]] & nes_ref=[[[nes_ref]],[[nes1_ref]]] & nes_lwp=[[[nes_lwp]],[[nes1_lwp]]]
endfor
nesutc=fltarr(size(nes_tau,/dimension))
for i=0, n_elements(nes_utc)-1 do nesutc[*,*,i]=nes_utc[i]
in=where(nes_lat lt max_lat and nes_lat gt min_lat and nes_lon lt max_lon and nes_lon gt min_lon)
nes_lat=nes_lat[in] & nes_lon= nes_lon[in] & nes_tau=nes_tau[in] & nes_ref=nes_ref[in] & nes_lwp=nes_lwp[in] & nes_utc=nesutc[in]


; GOES - LARC
; using .08k files (found in sebastian's directory)
print, 'getting GOES_LARC'
f=file_search(goes_larc_dir+'*'+strtrim(string(doy),2)+'*.08K',count=ct)
f=f[sort(f)]
if ct eq 0 then stop
goes_reader,f[0],larc_lat,larc_lon,larc_alb,larc_tau,larc_ref,larc_utc
for i=1,ct-1 do begin
  goes_reader,f[i],lat,lon,alb,tau,ref,utc
  larc_lat=[[[larc_lat]],[[lat]]] & larc_lon=[[[larc_lon]],[[lon]]] & larc_alb=[[[larc_alb]],[[alb]]]
  larc_tau=[[[larc_tau]],[[tau]]] & larc_ref=[[[larc_ref]],[[ref]]] & larc_utc=[larc_utc,utc]
endfor
larcutc=fltarr(size(larc_lat,/dimension))
for i=0, n_elements(larc_utc)-1 do larcutc[*,*,i]=larc_utc[i]
in=where(larc_lat lt max_lat and larc_lat gt min_lat and larc_lon lt max_lon and larc_lon gt min_lon)
larc_lat=larc_lat[in] & larc_lon= larc_lon[in] & larc_tau=larc_tau[in] & larc_ref=larc_ref[in] & larc_alb=larc_alb[in] & larc_utc=larcutc[in]

; MODIS - AQUA & TERRA
print, 'getting MODIS'
read_modis, tau_t, tau_a, reff_t, reff_a, lwp_t, lwp_a, utc_t, utc_a, lat_t, lat_a, lon_t, lon_a
stop
utct=fltarr(size(lat_t,/dimension))
for i=0, n_elements(utc_t)-1 do utct[*,i]=utc_t[i]
  in=where(lat_t lt max_lat and lat_t gt min_lat and lon_t lt max_lon and lon_t gt min_lon)
  lat_t=lat_t[in] & lon_t= lon_t[in] & tau_t=tau_t[in] & reff_t=reff_t[in] & lwp_t=lwp_t[in] & utc_t=utct[in]
utca=fltarr(size(lat_a,/dimension))
for i=0, n_elements(utc_a)-1 do utca[*,i]=utc_a[i]
  in=where(lat_a lt max_lat and lat_a gt min_lat and lon_a lt max_lon and lon_a gt min_lon)
  lat_a=lat_a[in] & lon_a= lon_a[in] & tau_a=tau_a[in] & reff_a=reff_a[in] & lwp_a=lwp_a[in] & utc_a=utca[in]

larc_lat[0]=!values.f_nan & larc_lon[0]= !values.f_nan & larc_tau[0]=!values.f_nan & larc_ref[0]=!values.f_nan & larc_alb[0]=!values.f_nan & larc_utc[0]=!values.f_nan
tau_t[0]=!values.f_nan & tau_a[0]=!values.f_nan & reff_t[0]=!values.f_nan & reff_a[0]=!values.f_nan & lwp_t[0]=!values.f_nan & lwp_a[0]=!values.f_nan & utc_t[0]=!values.f_nan & utc_a[0]=!values.f_nan
lat_t[0]=!values.f_nan & lat_a[0]=!values.f_nan & lon_t[0]=!values.f_nan & lon_a[0]=!values.f_nan
nes_utc[0]=!values.f_nan & nes_lat[0]=!values.f_nan & nes_lon[0]=!values.f_nan & nes_tau[0]=!values.f_nan & nes_ref[0]=!values.f_nan & nes_lwp[0]=!values.f_nan 
sam_lat[0]=!values.f_nan & sam_lon[0]=!values.f_nan & sam_alt[0]=!values.f_nan & sam_utc[0]=!values.f_nan
sam_reff[0]=!values.f_nan & sam_sza[0]=!values.f_nan & sam_tau[0]=!values.f_nan

save, /all, filename=dir+'20100516_clouds.out'

endif else restore, dir+'20100516_clouds.out'
if !VERSION.OS eq 'linux' then linux=1 else linux=0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ship and p3 centric measurements
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
larc_lat[0]=!values.f_nan & larc_lon[0]= !values.f_nan & larc_tau[0]=!values.f_nan & larc_ref[0]=!values.f_nan & larc_alb[0]=!values.f_nan & larc_utc[0]=!values.f_nan
tau_t[0]=!values.f_nan & tau_a[0]=!values.f_nan & reff_t[0]=!values.f_nan & reff_a[0]=!values.f_nan & lwp_t[0]=!values.f_nan & lwp_a[0]=!values.f_nan & utc_t[0]=!values.f_nan & utc_a[0]=!values.f_nan
lat_t[0]=!values.f_nan & lat_a[0]=!values.f_nan & lon_t[0]=!values.f_nan & lon_a[0]=!values.f_nan
nes_utc[0]=!values.f_nan & nes_lat[0]=!values.f_nan & nes_lon[0]=!values.f_nan & nes_tau[0]=!values.f_nan & nes_ref[0]=!values.f_nan & nes_lwp[0]=!values.f_nan 
sam_lat[0]=!values.f_nan & sam_lon[0]=!values.f_nan & sam_alt[0]=!values.f_nan & sam_utc[0]=!values.f_nan
sam_reff[0]=!values.f_nan & sam_sza[0]=!values.f_nan & sam_tau[0]=!values.f_nan
seb_lat[0]=!values.f_nan & seb_lon[0]=!values.f_nan & seb_utc[0]=!values.f_nan
seb_reff[0]=!values.f_nan & seb_tau[0]=!values.f_nan

sam_lwp=2.*sam_tau*sam_reff/3.
larc_lwp=2.*larc_tau*larc_ref/3.
ship_lwp=2.*ship_tau*ship_reff/3.
seb_lwp=2.*seb_tau*seb_reff/3.

; ship centric
loadct, 39, /silent
; p3 to ship
if linux then set_plot, 'x' else set_plot, 'win'
if (0) then begin

window, 0, xsize=900, ysize=900, title='Latitudes and Longitudes p3 and ship'

device, decomposed=0
!p.color=0 & !p.background=255
!p.font=1 & !p.thick=2.8 & !p.charsize=2.5 & !x.style=0 & !y.style=1 & !y.thick=1.8 & !x.thick=1.8
!p.multi=[0,2,2]

plot, ship_lon, ship_lat, title='lat lon of flight track and ship track',ytitle='lat', xtitle='lon', yticks=5, xticks=4
oplot, seb_lon, seb_lat, psym=2, color=70
plot, ship_utc, ship_lat, title='lat vs. utc', ytitle='lat', xtitle='utc', yticks=5, xticks=4
oplot, seb_utc, seb_lat, psym=2, color=70
plot, ship_lon, ship_utc, title='utc vs. lon', ytitle='utc', xtitle='lon', yticks=5, xticks=4
oplot, seb_lon, seb_utc, psym=2, color=70
plot, sam_utc, sam_alt, title='altitude', ytitle='alt(m)',xtitle='utc', yticks=5, xticks=4
endif
; make each point on ship equivalent to all points +- 15 min to plane
if (0) then begin
print, 'entering big loop'

r_limit=6000.0 ;6km limit
sam_ship=fltarr(n_elements(ship_utc))
seb_ship=fltarr(n_elements(ship_utc))
r_d_seb=seb_ship
r_d=fltarr(n_elements(ship_utc))
larc_ship=fltarr(n_elements(ship_utc))
nes_ship=larc_ship & a_ship=larc_ship & t_ship=larc_ship
for i=0L, n_elements(ship_utc)-1 do begin
  print, i, n_elements(ship_utc)
  k=where(sam_utc gt ship_utc[i]-0.25 and sam_utc lt ship_utc[i]+0.25, ct)
  if ct gt 0 then begin
    r=1000000.0
    for j=0, n_elements(k)-1 do begin
      distance = map_2points(ship_lon[i],ship_lat[i],sam_lon[k[j]],sam_lat[k[j]],/meters)
      if distance[0] le r then begin    ; find closest ship point
        sam_ship[i]=k[j]
        r=distance[0]
        r_d[i]=distance[0]
        if r gt r_limit then begin
          sam_ship[i]=!values.f_nan
          r_d[i]=!values.f_nan
        endif
      endif
    endfor    
  endif else begin
    sam_ship[i]=!values.f_nan
    r_d[i]=!values.f_nan
  endelse

 ;for seb's retrieval
 k=where(seb_utc gt ship_utc[i]-0.25 and seb_utc lt ship_utc[i]+0.25, ct)
  if ct gt 0 then begin
    r=1000000.0
    for j=0, n_elements(k)-1 do begin
      distance = map_2points(ship_lon[i],ship_lat[i],seb_lon[k[j]],seb_lat[k[j]],/meters)
      if distance[0] le r then begin    ; find closest ship point
        seb_ship[i]=k[j]
        r=distance[0]
        r_d_seb[i]=distance[0]
        if r gt r_limit then begin
          seb_ship[i]=!values.f_nan
          r_d_seb[i]=!values.f_nan
        endif
      endif
    endfor
  endif else begin
    seb_ship[i]=!values.f_nan
    r_d_seb[i]=!values.f_nan
  endelse
  
  ; doing the GOES imagery 
  k=where(larc_utc gt ship_utc[i]-0.25 and larc_utc lt ship_utc[i]+0.25, ct) 
  if ct gt 0 then begin ;larc loop
    m=min(abs(larc_lat[k]-ship_lat[i]) + abs(larc_lon[k]-ship_lon[i]), spot)
    larc_ship[i]=k[spot]
    ;stop
  endif else begin
    larc_ship[i]=!values.f_nan
  endelse ; end larc
  k=where(nes_utc gt ship_utc[i]-0.25 and nes_utc lt ship_utc[i]+0.25, ct) 
  if ct gt 0 then begin ;nesdis loop
    m=min(abs(nes_lat[k]-ship_lat[i]) + abs(nes_lon[k]-ship_lon[i]), spot)
    nes_ship[i]=k[spot]
  endif else begin
    nes_ship[i]=!values.f_nan
  endelse ; end nesdis
  
  ;doing MODIS Imagery
  k=where(utc_a gt ship_utc[i]-0.25 and utc_a lt ship_utc[i]+0.25, ct) 
  if ct gt 0 then begin ; AQUA loop
    m=min(abs(lat_a[k]-ship_lat[i]) + abs(lon_a[k]-ship_lon[i]), spot)
    a_ship[i]=k[spot]
  endif else begin
    a_ship[i]=!values.f_nan
  endelse ; end aqua
  k=where(utc_t gt ship_utc[i]-0.25 and utc_t lt ship_utc[i]+0.25, ct) 
  if ct gt 0 then begin ; TERRA loop
    m=min(abs(lat_t[k]-ship_lat[i]) + abs(lon_t[k]-ship_lon[i]), spot)
    t_ship[i]=k[spot]
  endif else begin
    t_ship[i]=!values.f_nan
  endelse ; end nesdis
  
endfor

;ship_lat=ship_lat[0:*:5] & ship_lon=ship_lon[0:*:5] & ship_utc=ship_utc[0:*:5]
;ship_reff=ship_reff[0:*:5] & ship_tau=ship_tau[0:*:5]
;ship_reffunc=ship_reffunc[0:*:5] & ship_tauunc=ship_tauunc[0:*:5]

save, t_ship,a_ship,nes_ship,seb_ship,sam_ship,larc_ship,r_d, filename='/home/leblanc/CALNEX/clouds/20100516_pointers_2.out'
 endif else restore, '/home/leblanc/CALNEX/clouds/20100516_pointers_2.out'

restore, '/home/mcbride/sdata/calnex/ship/20100516/20100516_wvap_doy136_6wls.out'
ship_utc=TMHRS24P
ship_tau=TAUS
ship_reff=REFFS
ship_lwp=5.*ship_tau*ship_reff/9.

ggg=read_ascii('/home/leblanc/CALNEX/clouds/visst_atlantis_matchup_2010136_allday_new.txt')
larc_tau=ggg.field01[9, *]
larc_ref=ggg.field01[11, *]
larc_lwp=5.*larc_tau*larc_ref/9.
larc_utc=ggg.field01[3,*]
ship_tau_2=ggg.field01[49,*]
ship_ref_2=ggg.field01[50,*]
ship_lwp_2=5.*ship_tau_2*ship_ref_2/9.

;window,1 , title='Retrieval comparison, Ship centric', xsize=900,ysize=900
set_plot, 'ps'
loadct, 39,/silent
!p.font=1 & !p.thick=5 & !p.charsize=4.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
!p.multi=[0,1,3]
device, /encapsulated
device, /tt_font, set_font='Helvetica Bold'
device, filename=dir+'20100519_clouds.ps'
device,/color,bits_per_pixel=8.
device, xsize=30, ysize=40

aaa=min(abs(ship_utc-19.45),check)

names=['Atlantis','P-3', 'GOES - NESDIS', 'GOES - LARC', 'MODIS - AQUA','MODIS - TERRA']
cl=[0,50,100,150,200,250]
kkk=where(seb_tau eq 0.)
seb_tau[kkk]=!values.f_nan
plot, ship_utc, ship_tau, title='Optical Depth', xtitle='UTC (h)', psym=1, yrange=[0,50], xrange=[15.,23.],/nodata
;oplot, ship_utc, sam_tau[sam_ship], color=50, psym=1
tvlct, 201,201,201,201
oplot, [ship_utc[check],ship_utc[check]],[0,60],color=201, thick=2
oplot, larc_utc, ship_tau_2, psym=1
oplot, ship_utc, seb_tau[seb_ship], color=50, psym=1
oplot, ship_utc, nes_tau[nes_ship], color=100, psym=1
oplot, larc_utc, larc_tau,  color=150, psym=1 ;larc_tau[larc_ship], color=150, psym=1
oplot, ship_utc, tau_a[a_ship], color=200, psym=1
oplot, ship_utc, tau_t[t_ship], color=250, psym=1
legend,names,textcolors=cl,charsize=1.5 ,box=0,/bottom

kkk=where(seb_reff eq 0.)
seb_reff[kkk]=!values.f_nan
plot, ship_utc, ship_reff, title='Cloud Drop Effective Radius', xtitle='UTC (h)',ytitle='Micron', psym=1, yrange=[0,20], xrange=[15.,23.], /nodata
;oplot, ship_utc, sam_reff[sam_ship], color=50, psym=1
oplot, larc_utc, ship_ref_2, psym=1
oplot, ship_utc, seb_reff[seb_ship], color=50, psym=1
oplot, ship_utc, nes_ref[nes_ship], color=100, psym=1
oplot, larc_utc, larc_ref, color=150, psym=1 ;larc_ref[larc_ship], color=150, psym=1
oplot, ship_utc, reff_a[a_ship], color=200, psym=1
oplot, ship_utc, reff_t[t_ship], color=250, psym=1
legend,names,textcolors=cl,charsize=1.5,box=0

kkk=where(seb_lwp eq 0.)
seb_lwp[kkk]=!values.f_nan
plot, ship_utc, ship_lwp, title='Cloud Liquid Water Path', xtitle='UTC (h)',ytitle='Liquid Water Path (g/m^2)', psym=1, yrange=[0,260], xrange=[15.,23.],/nodata
;oplot, ship_utc, sam_lwp[sam_ship], color=50, psym=1
oplot, larc_utc, ship_lwp_2,psym=1
oplot, ship_utc, seb_lwp[seb_ship], color=50, psym=1
oplot, ship_utc, nes_lwp[nes_ship], color=100, psym=1
oplot, larc_utc, larc_lwp, color=150, psym=1; larc_lwp[larc_ship], color=150, psym=1
oplot, ship_utc, lwp_a[a_ship], color=200, psym=1
oplot, ship_utc, lwp_t[t_ship], color=250, psym=1
legend,names,textcolors=cl,charsize=1.5,box=0, /bottom

device, /close
spawn, 'convert '+dir+'20100519_clouds.ps '+dir+'20100519_clouds_new.png'
spawn, 'rm '+dir+'20100519_clouds.ps'

;window, 2, title='modis'
;!p.multi=0
;!y.omargin=[0,0]
;contour, tau_t, lon_t, lat_t,/cell_fill

stop
end
