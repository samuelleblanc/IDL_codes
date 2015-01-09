@../legend.pro
@../zensun.pro
;@C:\CalNex\pro\shipnav1min.pro
;@C:\CalNex\pro\read_ext.pro
@../read_modis.pro

pro comparison_may16

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

; restoring ship data

restore, '/data/seven/schmidt/calnex/ship/20100516/20100516_TS.out'
; tmhrs,ql_zenlambda,zpartspec,ql_nadlambda,npartspec,cg4,lat,lon,status,filename=outfile
nl =n_elements(ql_zenlambda)

; restore ship nav data
restore, '/home/leblanc/CALNEX/ship/20100516/sam.out'
; utc, lat , lon
utc_ship=utc
lat_ship=lat
lon_ship=lon

; restore retrieval products from ship
restore, '/data/seven/schmidt/calnex/ship/20100516/20100516_RT.out'
; tmhrs,tau,reff,tauunc,reffunc,lat,lon,status,filename = outfile

  ; get sza
  year =fix(strmid(date,0,4))
  month=fix(strmid(date,4,2))
  day  =fix(strmid(date,6,2))
  juld=julian_day(year,month,day)
  utct=utc
  zensun,juld,utct,lat,lon,sza
  mu=cos(!pi/180.*sza)

  uu=[min(utc),max(utc)]
  siz=get_screen_size()

  day=juld+utc/24.
  uy =juld+uu/24.
  
; smooth out retrievals from ship
  refs=smooth(reff,sm)
  taus=smooth(tau ,sm)
  ind=where(reff gt 0.5,nrt)
  lwp=fltarr(n_elements(refs))
  if nrt gt 0 then lwp[ind]=2./3.*tau[ind]*reff[ind]
  lwps=smooth(lwp,sm)
  lwpm=1000
  refm=30

  
; Get products from P3 and GOES LARC
restore, '/data/seven/schmidt/calnex/rt/p3_and_goes_larc_retrievals.out'
none=where(tau_p3_goes gt 0.2, count)
if count gt 0 then tau_p3_goes=tau_p3_goes[none]
if count gt 0 then ref_p3_goes=ref_p3_goes[none]

; Get poducts from GOES NESDIS on ship track
restore, '/data/seven/schmidt/calnex/goes/p3_ship_20100516.out'

  window,14,tit='T'
  !P.multi=0
; plotting of the ship
  flt=where(utc ge 17. and utc le 20.0) ;lat gt 33.05 and lat lt 34 and lon gt -119.4 and lon lt -116 and utc gt 17.5 and utc lt 21.7)
 ; flx=where(lat gt 33.05 and lat lt 34 and lon gt -119.4 and lon lt -116 and utc gt 19.5 and utc lt 20)
  ref=reff
  plot,lon[flt],smooth(tau[flt],60),psym=3,xr=[-119.3,-118.5],yr=[0,100],xtit='Longitude (degrees)',ytit='Optical Thickness', title='Comparison May 16th, 2010, Plane-Ship-Satellite'
  utcf=utc[flt] & lonf=lon[flt] & latf=lat[flt] & szaf=sza[flt] & ref0=reff[flt] & reff=ref0 & tauf=tau[flt]

  
  flf=where(utcf gt 19.5 and utcf lt 20) ; only plot the first flight leg (near time occurence of the ship)
  ; plotting of the p3
  oplot,lon_p3,smooth(tau_p3,10),psym=3,color=150
  
  ; Goes retrieval along P3 (LARC)
  oplot,lon_p3[none],tau_p3_goes,psym=3,color=70
  
  ; GOES retrieval along ship (NESDIS)
  oplot,xcoin[*,1],codcoin[*,1],thick=2, color=250
  oplot, xcoin[*,0],codcoin[*,0],thick=2, color=200
  
  utc_t=20.833
  utc_a=19.25
  
  ; for terra 
  terra_i=where(utc_p3 le utc_t + 0.01 and utc_p3 ge utc_t - 0.01)  
  read_modis, lat_p3[terra_i[0]], lon_p3[terra_i[0]], tau_t, tau_a, reff_t, reff_a, lwp_t, lwp_a, utc_t, utc_a, lat_t, lat_a, lon_t, lon_a
  oplot, lon_t, tau_t, psym=3, color=20
  
  ; for aqua
  aqua_i=where(utc_p3 le utc_a + 0.1 and utc_p3 ge utc_a - 0.1)  
  read_modis, lat_p3[aqua_i[0]], lon_p3[aqua_i[0]], tau_t, tau_a, reff_t, reff_a, lwp_t, lwp_a, utc_t, utc_a, lat_t, lat_a, lon_t, lon_a
  oplot, lon_a, tau_a, psym=3, color=40

  legend, ['Ship Retrieval','NESDIS-ship retrieval','NESDIS-P3 retrieval','P3 Retrieval','LARC retrieval', 'MODIS - TERRA','MODIS - AQUA'],textcolor=[0,250,200,150,70,20,40], /right,/box, charsize=1.5
  p=tvrd(true=1)
 write_png,'tau-compare.png',p

if (0) then begin
window,15,tit='R'
; plotting of the ship
  ref=reff
  plot,utc[flt],smooth(ref[flt],60),psym=3,xr=[17.5,21.5],yr=[0,30],xtit='Time (UTC)',ytit='Effective Radius', title='Comparison May 16th, 2010, Plane-Ship-Satellite'

  ; plotting of the p3
  oplot,utc_p3,smooth(ref_p3,10),psym=3,color=150
  
  ; Goes retrieval along P3 (LARC)
  oplot,utc_p3[none],ref_p3_goes,psym=3,color=70
  
  ; GOES retrieval along ship (NESDIS)
  oplot,tcoin[*,1],refcoin[*,1],thick=2, color=250
  oplot, tcoin[*,0],refcoin[*,0],thick=2, color=200
  
  legend, ['Ship Retrieval','NESDIS-ship retrieval','NESDIS-P3 retrieval','P3 Retrieval','LARC retrieval', 'MODIS - TERRA'],textcolor=[0,250,200,150,70,20], /right,/box, charsize=1.5

  window,16,tit='L'
	lwp=ref[flt]*tau[flt]*2./3.
  plot,utc[flt],smooth(lwp,60),psym=3,xr=[17.5,21.5],yr=[0,500],xtit='Time (UTC)',ytit='Liquid Water Path', title='Comparison May 16th, 2010, Plane-Ship-Satellite'

  ; plotting of the p3
  oplot,utc_p3,smooth(ref_p3*tau_p3*2./3.,10),psym=3,color=150
  
  ; Goes retrieval along P3 (LARC)
  oplot,utc_p3[none],ref_p3_goes*tau_p3_goes*2./3.,psym=3,color=70
  
  ; GOES retrieval along ship (NESDIS)
  oplot,tcoin[*,1],lwpcoin[*,1],thick=2, color=250
  oplot, tcoin[*,0],lwpcoin[*,0],thick=2, color=200
  
  legend, ['Ship Retrieval','NESDIS-ship retrieval','NESDIS-P3 retrieval','P3 Retrieval','LARC retrieval', 'MODIS - TERRA'],textcolor=[0,250,200,150,70,20], /right,/box, charsize=1.5

endif

  stop



end
