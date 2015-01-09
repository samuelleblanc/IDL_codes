;+
; NAME:
;   calnex_ql_ship
;
; PURPOSE:
;   Quick Look Utility for the CALNEX experiment
;
; CATEGORY:
;   CALNEX / Quick look
;
; CALLING SEQUENCE:
;   calnex_ql
;
; OUTPUT:
;
; KEYWORDS:
;   SSFR, P3, CALNEX, CG4, Quicklook
;
; DEPENDENCIES:
;   cfg.pro       ;config file cheker
;   nav_p3.pro    ;p3 nav program
;   legend.pro    ;program to make the legend
;   cg4_p3.pro    ;program to get cg4 data from the p3
;   get_cos_simple.pro  ;Program getting the cosine response correction
;   archive_p3.pro      ;Program to archive the data in the NOAA style
;   disp.pro      ;unsure about the utility of this program
;     zensun.pro  ; subprogram needed for get_cos_simple
;     muslope.pro ; subprogram needed for get_cos_simple
;     rph2za.pro  ; subprogram needed
;
; NEEDED FILES:
;   - config file for day
;
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
; Written:  Sebastian Schmidtc, LASP CU Boulder, May 12, 2010
;---------------------------------------------------------------------------
@C:\CalNex\pro\legend.pro
@C:\CalNex\pro\zensun.pro
@C:\CalNex\pro\shipnav1min.pro
pro calnex_ql_ship,date_inp
if n_elements(date_inp) lt 1 then begin
  date='20100519'
endif else begin
  date=strcompress(string(date_inp),/REMOVE_ALL)
endelse

sm=60 ; smoothing for retrievals

; windows:
dir='C:\CalNex\ship\'
ll ='\'
; linux:
;dir='/data/seven/schmidt/calnex/p3/'
;ll='/' ; directory separator

path=dir+date+ll
date=strmid(date,0,8)

device,decomposed=0
loadct,27

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
  if uu[1] gt 24 then uu[1]=24. ; Something still going wrong after 24hh

  siz=get_screen_size()
  window,1,xs=siz[0],ys=siz[1]*0.9,retain=2,tit='Quicklook'
  cs=1.5
  !p.multi=[0,2,2]

  day=juld+utc/24.
  uy =juld+uu/24.
  plot,day,zpartspec[0,*],xr=uy,yr=[0,max(zpartspec)],xtitle='DOY',ytitle='Irradiance [W m!E-2!N nm!E-1!N]',psym=3,charsize=cs,title=date,/xstyle
  oplot,utc,mu,linesty=1

  outtxt=['wavelength [nm]',strtrim(string(ql_zenlambda[0]),2)]
  outclr=[254              ,254]
  for l=1,nl-1 do begin
    oplot,day,zpartspec[l,*],color=40*l,psym=3
    outtxt=[outtxt,strtrim(string(ql_zenlambda[l]),2)]
    outclr=[outclr,40*l]
  endfor
  legend,outtxt,textcolors=outclr,charsize=1.0

  plot,day,npartspec[0,*],xr=uy,yr=[0.,max(npartspec)],xtitle='DOY',ytitle='Radiance [W m!E-2!N nm!E-1!N sr!E-1!N]',psym=3,charsize=cs,/xstyle
  for l=1,nl-1 do begin
    oplot,day,npartspec[l,*],color=40*l,psym=3
  endfor

  plot,day,mu,xr=uy,yr=[0,1],xtitle='DOY',ytitle='cos(SZA)',title='Sun Angle',psym=3,charsize=cs,/xs

  plot,day,cg4,xr=uy,yr=[300,400],xtitle='DOY',ytitle='LW Irradiance [W m!E-2!N]',charsize=cs,/xstyle

  p1=tvrd(true=1)
  write_png,path+'SSFR'+date+'_ql.png',p1


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
  window,2,xs=siz[0],ys=siz[1]*0.9,retain=2,tit='Retrievals'
  plot,day,taus, xr=uy,yr=[0,max(taus)] ,xtitle='DOY',ytitle='Cloud Optical Depth',charsize=cs,/xstyle,psym=3,title=date
  ;oplot,minut,ttmin,color=200
  plot,day,refs,xr=uy,yr=[0,refm],xtitle='DOY',ytitle='Reff [um]',charsize=cs,/xstyle,psym=3
  ;oplot,minut,rrmin,color=200
  plot,day,lwps,xr=uy,yr=[0,lwpm] ,xtitle='DOY',ytitle='LWP [g m!E-2!N]',charsize=cs,/xstyle,psym=3
  ;oplot,minut,lwmin,color=200
  plot,day,mu  ,xr=uy,yr=[0,1]        ,xtitle='DOY',ytitle='cos(SZA)',charsize=cs,/xstyle,psym=3
  p1=tvrd(true=1)
  write_png,path+'SSFR'+date+'_rt.png',p1

window,4
!P.multi=0
plot,utc,psym=3
p1=tvrd(true=1)
write_png,path+'SSFR'+date+'_hk.png',p1

goto,detail



STOP

restore,path+date+'_SP.out'
;time,wlvisz,wlnirz,wlvisn,wlnirn,zsimean_rawspectra,zinmean_rawspectra,nsimean_rawspectra,ninmean_rawspectra,zsimax_rawspectra,zinmax_rawspectra,nsimax_rawspectra,ninmax_rawspectra,zsimin_rawspectra,zinmin_rawspectra,nsimin_rawspectra,ninmin_rawspe
utcsp=time

window,4,title='Spectra',xs=siz[0],ys=siz[1]*0.9,retain=2
ns=n_elements(utcsp)
for ti=0,ns-1 do begin
plot ,wlvisz,zsi0_rawspectra[*,ti],xr=[350,1700],xtitle='Wavelength [nm]',ytitle='Counts Irradiance',/xstyle
oplot,wlnirz,zin0_rawspectra[*,ti],color=120
plot ,wlvisn,nsi0_rawspectra[*,ti],xr=[350,1700],xtitle='Wavelength [nm]',ytitle='Counts Radiance',/xstyle
oplot,wlnirn,nin0_rawspectra[*,ti],color=120

; get integration times
i0=0
for i=0,n_elements(status)-1 do begin
  if strcmp(status[i],'Integration',11) then i0=i
endfor
pos0=strpos(status[i0+1],':')+1
zsiint=float(strmid(status[i0+1],pos0))
zirint=float(strmid(status[i0+2],pos0))
nsiint=float(strmid(status[i0+3],pos0))
nirint=float(strmid(status[i0+4],pos0))


plot ,wlvisz,zsimean_rawspectra[*,ti]/zsiint/zsi[1,*],xr=[350,1700],xtitle='Wavelength [nm]',ytitle='Irradiance [W m!E-2!N nm!E-1!N]',/xstyle,title='UTC='+strcompress(string(time[ti]),/REMOVE_ALL)+' h'
oplot,wlnirz,zinmean_rawspectra[*,ti]/zirint/zir[1,*],color=120
plot ,wlvisn,nsimean_rawspectra[*,ti]/nsiint/nsi[1,*],xr=[350,1700],xtitle='Wavelength [nm]',ytitle='Radiance [W m!E-2!N nm!E-1!N sr!E-1!N]',/xstyle
oplot,wlnirn,ninmean_rawspectra[*,ti]/nirint/nir[1,*],color=120
;wait,0.15
endfor

restore,path+date+'_temp.out'
;tmhrs, nsitemp, zsitemp, nirtemp, zirtemp, bxtemp, bxcltemp
utcd=tmhrs
window,6,xsize=900,ysize=700,retain=2,tit='Temperatures'
  !P.multi=[0,2,2]
  xr=uu
  hot=where(nsitemp gt 100 or zsitemp gt 100,nhot)
  if nhot gt 0 then begin
    nsitemp[hot]=0.
    zsitemp[hot]=0.
  endif
  plot,utc,bxtemp,tit=date+' computer temperature',xtit='UTC [h]',ytit='T [C]',psym=3,xr=xr,/xstyle,yr=[10,40]
  ;oplot,utcd,zirxt,color=5
  ;oplot,utcd,nirxt,color=200
  legend,['Box'],textcolors=[255]
  plot,utc,bxcltemp,xtit='UTC [h]',ytit='T [C]',psym=3,xr=xr,/xstyle
  legend,['Ambient Cabin'],textcolors=[255]
  plot,utc,nsitemp,tit='Silicon temperatures',xtit='UTC [h]',ytit='T [C]',psym=3,xr=xr,yr=[20,max(nsitemp)+3],/xs
  oplot,utc,zsitemp,color=5,psym=3
  legend,['Nadir','Zenith'],textcolors=[255,5]
  plot,utc,nirtemp,tit='InGaAs temperature',xtit='UTC [h]',ytit='T [C]',psym=3,yr=[0,20],xr=xr,/xs
  oplot,utc,zirtemp,color=5,psym=3
  legend,['Nadir','Zenith'],textcolors=[255,5]
  p=tvrd(true=1)
  write_png,path+date+'_temps.png',p

stop


detail:
if strcmp(date,'20100516') then begin
  ship='C:\CalNex\ship\20100516\Underway*.ict'
  shipnav1min,ship,utc,lat,lon,rrr,ccn

  window,14,tit='X'
  !P.multi=0

  flt=where(lat gt 33.05 and lat lt 34 and lon gt -119.4 and lon lt -116 and utc gt 17.9 and utc lt 21.7)
  ref=reff
  plot,lon[flt],smooth(tau[flt],60),psym=3,xr=[-119.3,-118.5],yr=[0,100],xtit='Longitude [deg]',ytit='Optical Thickness'
  utcf=utc[flt]
  lonf=lon[flt]
  latf=lat[flt]
  utcf=utc[flt]
  szaf=sza[flt]
  ref0=reff[flt]
  reff=ref0
  tauf=tau[flt]
  save,utcf,lonf,latf,szaf,reff,tauf,file='C:\CalNex\ship\20100516\16.out'
  restore,'C:\CalNex\p3\20100516\16.out'
  oplot,lonf,smooth(tauf,10),psym=3,color=100
  stop

  ;plot,lon[flt],smooth(nspectra[154,flt],10),psym=3,tit='Upward Irradiance @ 865 nm',xtit='Longitude [deg]',ytit='Irradiance [W m!E-2!N nm!E-1!N]'
  ;p=tvrd(true=1)
  ;write_png,dir+date+ll+'SF'+date+'_x_'+platform+'.PNG',p
endif


end
