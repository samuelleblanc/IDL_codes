;@C:\CalNex\pro\legend.pro
;@C:\CalNex\pro\zensun.pro
;@C:\CalNex\pro\shipnav1min.pro
;@C:\CalNex\pro\read_ext.pro
;@C:\CalNex\pro\read_modis.pro

pro p3_ship_0516

path='C:\CalNex\ship\20100516\'
date='20100516'
sm  =60

device,decomposed=0
loadct,39
!p.font=0
!p.thick=2
!p.charsize=1.5
!y.thick=1.8
!x.thick=1.8
!p.color=0
!p.background=255


; *******************************************************************************
; get IDL sav files
; *******************************************************************************

restore,path+date+'_TS.out'
;tmhrs,ql_zenlambda,zpartspec,ql_nadlambda,npartspec,cg4,lat,lon,status,filename=outfile
utc=tmhrs
nl =n_elements(ql_zenlambda)
for i=0,n_elements(status)-1 do begin
  print,status[i]
  if strcmp(status[i],'Read nav',8) then begin
    pos=strpos(status[i],':')
    str=strmid(status[i],pos+1)
    if fix(str) eq 0 then print,'Warning: No nav files available - no valid retrievals!'
  endif
  if strcmp(status[i],'Saturation',8) then begin
    pos=strpos(status[i],':')
    str=strmid(status[i],pos+1)
    if fix(str) gt 0 then print,'Warning: Saturated spectra #'+str
  endif
endfor

  ; 24 hour module (time after midnight gets added as UTC>24)
  nn=n_elements(utc)
  utc0=0.
  xtra=0
  for i=0L,nn-1 do begin
    if utc[i] lt utc0 then xtra=i
    utc0=utc[i]
  endfor
  utc[xtra:nn-1]=utc[xtra:nn-1]+24.

  ; get sza
  year =fix(strmid(date,0,4))
  month=fix(strmid(date,4,2))
  day  =fix(strmid(date,6,2))
  juld=julian_day(year,month,day)
  utct=utc
  zensun,juld,utct,lat,lon,sza
  mu=cos(!pi/180.*sza)

  uu=[min(utc),max(utc)]
  ;if uu[1] gt 24 then uu[1]=24. ; Something still going wrong after 24hh

  siz=get_screen_size()

  day=juld+utc/24.
  uy =juld+uu/24.


  ;window,10,title='MAP'
  ;mlat=34.
  ;mlon=-118.
  ;map_set,mlat,mlon,/grid,limit=[mlat-4,mlon-4,mlat+4,mlon+4],/label
  ;map_continents,/usa,/hires,thick=2
  ;;map_continents,/countries,/hires,/coast,thick=2
  ;oplot,lon,lat,psym=3





restore,path+date+'_RT.out'
;tmhrs,tau,reff,tauunc,reffunc,lat,lon,status,filename = outfile


; average over 1 minute intervals
ctr=0
tt =0
rr =0
j=0
minut=fltarr(3600)
ttmin=fltarr(3600)
rrmin=fltarr(3600)
lwmin=fltarr(3600)
openw,ur,path+'SSFR_retrieval_'+date+'.txt',/get_lun
printf,ur,'julian_day cloud_optical_thickness effective_radius[um] liquid_water_path[gm-2]'
for i=0L,nn-1 do begin
  if day[i] gt uy[0] and day[i] lt uy[1] then begin
    if ctr ge 60 then begin
      tt=tt/float(ctr)
      rr=rr/float(ctr)
      lw=2./3.*tt*rr
      minut[j]=day[i]
      printf,ur,day[i],tt,rr,lw
      if tt lt 0 then stop
      ttmin[j]=tt
      rrmin[j]=rr
      lwmin[j]=lw
      j=j+1
      ctr=0
    endif else begin
      if tau[i] ge 0 and reff[i] ge 0 then begin
        tt=tt+tau[i]
        rr=rr+reff[i]
        ctr=ctr+1
      endif
    endelse
  endif
endfor
free_lun,ur

  refs=smooth(reff,sm)
  taus=smooth(tau ,sm)
  ind=where(reff gt 0.5,nrt)
  lwp=fltarr(nn)
  if nrt gt 0 then lwp[ind]=2./3.*tau[ind]*reff[ind]
  lwps=smooth(lwp,sm)
  lwpm=1000
  refm=30

if strcmp(date,'20100516') then begin
  ship='C:\CalNex\ship\20100516\Underway*.ict'
  shipnav1min,ship,utc,lat,lon,rrr,ccn
  ;extf='C:\CalNex\p3\20100516\CRDExt_NP3_20100516_RA.ict'
  ;read_ext,utc,extf,ext

  window,14,tit='T'
  !P.multi=0

  flt=where(lat gt 33.05 and lat lt 34 and lon gt -119.4 and lon lt -116 and utc gt 17.9 and utc lt 21.7)
  flx=where(lat gt 33.05 and lat lt 34 and lon gt -119.4 and lon lt -116 and utc gt 19.5 and utc lt 20)
  ref=reff
  ; ship retrieval
  plot,lon[flt],smooth(tau[flt],60),psym=3,xr=[-119.3,-118.5],yr=[0,100],xtit='Longitude [deg]',ytit='Optical Thickness', title='Comparison May 16th, 2010, Plane-Ship-Satellite'
  ;plot,lon[flx],smooth(tau[flx],60),psym=3,xr=[-119.3,-118.5],yr=[0,100],xtit='Longitude [deg]',ytit='Optical Thickness', title='Comparison May 16th, 2010, Plane-Ship-Satellite'
  utcf=utc[flt]
  lonf=lon[flt]
  latf=lat[flt]
  utcf=utc[flt]
  szaf=sza[flt]
  ref0=reff[flt]
  reff=ref0
  tauf=tau[flt]
  fly=where(utcf gt 19.0 and utcf lt 21.0)
  restore,'C:\CalNex\p3\20100516\p3_and_goes_larc_retrievals.out'
  restore,'C:\CalNex\goes\nesdis_goes.out'
  filterg=where(tau_p3_goes gt 1.)
  flf=where(utc_p3 gt 19.5 and utc_p3 lt 20)
  ; p3 retrieval
  oplot,lon_p3[flf],smooth(tau_p3[flf],10),psym=3,color=150
  ; GOES retrieval along P-3 (+/- 15 minutes, latitude/longitude matched)
  oplot,lon_p3[filterg],tau_p3_goes[filterg],psym=3,color=70
  
  ; GOES NESDIS retrieval LARC along ship track (time matched, multiple retrievals)
  oplot,xcoin[*,1],codcoin[*,1],thick=2, color=250

  ; GOES NESDIS retrieval along P3 track
  oplot, xcoin[*,0],codcoin[*,0], thick=2, color=200


  ;read_modis, latf, lonf, tau_t, tau_a, reff_t, reff_a, lwp_t, lwp_a, utc_t, utc_a, lat_t, lat_a, lon_t, lon_a
  ;oplot, lon_t, tau_t, psym=4, color=20


  legend, ['Ship Retrieval','NESDIS-ship retrieval','NESDIS-P3 Retrieval,'P3 Retrieval','LARC retrieval', 'MODIS - TERRA'],textcolor=[0,250,200,150,70,20], /right,/box, charsize=1.5
  p=tvrd(true=1)
  write_png,'C:\CalNex\p3\20100516\andy-tau.png',p



  window,15,tit='R'
  ;ship retrieval
  plot,lon[flt],smooth(ref[flt],60),psym=3,xr=[-119.3,-118.5],yr=[0,30],xtit='Longitude [deg]',ytit='Effective Radius [um]', title='Comparison May 16th, 2010, Plane-Ship-Satellite'
  ;p3 retrieval
  oplot,lon_p3[flf],smooth(ref_p3[flf],10),psym=3,color=150
  ;goes LARC retrieval (19:45)
  oplot,lon_p3[filterg],ref_p3_goes[filterg],psym=3,color=70
  ;goes NESDIS retrieval along ship track
  oplot,xcoin,refcoin,thick=2, color=250
  ;oplot, lon_t, ref_t, psym=4, color=20
  legend, ['Ship Retrieval','NESDIS retrieval','P3 Retrieval','LARC retrieval','MODIS - TERRA'],textcolor=[0,250,150,70,20], /right,/box, charsize=1.5
  p=tvrd(true=1)
  write_png,'C:\CalNex\p3\20100516\andy-ref.png',p


  window,16,tit='L'
  ; ship retrieval
  plot,lon[flt],smooth(ref[flt]*tau[flt],60),psym=3,xr=[-119.3,-118.5],yr=[0,500],xtit='Longitude [deg]',ytit='LWP [g m!E-2!N]', title='Comparison May 16th, 2010, Plane-Ship-Satellite'
  ; p3 retrieval
  oplot,lon_p3[flf],smooth(ref_p3[flf]*tau_p3[flf],10),psym=3,color=150
  ; goes retrieval along p3 (19:45)
  oplot,lon_p3[filterg],ref_p3_goes[filterg]*tau_p3_goes[filterg],psym=3,color=70
  ; goes retrieval (NESDIS) along ship track
  oplot,xcoin,refcoin*codcoin,thick=2, color=250
  ;oplot, lon_t, lwp_t, psym=4, color=20
  legend, ['Ship Retrieval','NESDIS retrieval','P3 Retrieval','LARC retrieval','MODIS - TERRA'],textcolor=[0,250,150,70,120], /right,/box, charsize=1.5
  p=tvrd(true=1)
  write_png,'C:\CalNex\p3\20100516\andy-lwp.png',p


  stop
endif


end
