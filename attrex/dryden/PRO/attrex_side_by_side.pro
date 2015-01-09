@cfg.pro
@nav_noaa.pro
@legend.pro
@get_cos.pro

pro side_by_side
dir='/data/seven/schmidt/attrex/gh/'
date='20110828'
ll='/' ; directory separator

device,decomposed=0
loadct,27

; *******************************************************************************
; get information from configuration file
; *******************************************************************************
; initialize configuration file
cfg_file=dir+ll+date+ll+date+'.cfg'        ; build cfg file
if file_test(cfg_file) ne 1 then message,'Did not find configuration file.'
plot=cfg(cfg_file,'plot')
; get platform
if strcmp(strmid(plot,0,1),'y',/FOLD_CASE) then plot=1 else plot=0
;plot=0
platform=cfg(cfg_file,'platform')
print,'Process flight from '+date+' onboard '+platform+'.'
c0=cfg(cfg_file,'comment')
if not strcmp(c0,'#') then begin
  print,c0
  comment=c0
endif
; get extraterrestial flux
extra=cfg(cfg_file,'extra') ; extra-terrestial flux
; get time range
i0=cfg(cfg_file,'interval') ; look if we should only use data within a certain time window
uu0=0
if not strcmp(i0,'#') then begin
    uu=strsplit(i0,' ,',escape='#',/EXTRACT)
    uu=float(uu)
    uu0=1
    u0=uu[0] & u1=uu[1]
endif
; get broadband range
bb1=cfg(cfg_file,'bb1')
bb1=strsplit(bb1,' ,',escape='#',/EXTRACT)
bb1=float(bb1)
bb2=cfg(cfg_file,'bb2')
bb2=strsplit(bb2,' ,',escape='#',/EXTRACT)
bb2=float(bb2)
if strcmp(platform,'NOAAP3',3,/FOLD_CASE) then noaa=1 else noaa=0

; *******************************************************************************
; get spectra
; *******************************************************************************
print,'Restore SSFR data...'
restore, dir+date+ll+date+'_calibspcs.out'
print,status
utc=tmhrs
lambda=zenlambda
nn=n_elements(utc)
; make solar broadband
print,'Calculate solar broadband...'
bb1z=where(zenlambda ge bb1[0] and zenlambda le bb1[1]) & bb1n=where(nadlambda ge bb1[0] and nadlambda le bb1[1])
bb2z=where(zenlambda ge bb2[0] and zenlambda le bb2[1]) & bb2n=where(nadlambda ge bb2[0] and nadlambda le bb2[1])
dz1=0.5*(zenlambda[bb1z+1]-zenlambda[bb1z-1])           & dn1=0.5*(nadlambda[bb1n+1]-nadlambda[bb1n-1])
dz2=0.5*(zenlambda[bb2z+1]-zenlambda[bb2z-1])           & dn2=0.5*(nadlambda[bb2n+1]-nadlambda[bb2n-1])
bz1=fltarr(nn) & bn1=fltarr(nn) & bz2=fltarr(nn) & bn2=fltarr(nn)
for i=0,nn-1 do begin
  bz1[i]=total(zspectra[bb1z,i]*dz1)
  bn1[i]=total(nspectra[bb1n,i]*dn1)
  bz2[i]=total(zspectra[bb2z,i]*dz2)
  bn2[i]=total(nspectra[bb2n,i]*dn2)
endfor

; *******************************************************************************
; get NAV data
; *******************************************************************************
nav=cfg(cfg_file,'read_nav')
; get platform
if strcmp(strmid(nav,0,1),'n',/FOLD_CASE) then nav=0 else nav=1
if nav then begin
 if strcmp(platform,'NOAAP3',3,/FOLD_CASE) then begin
   nav_noaa,dir,ll,date,cfg_file,utc,alt,lat,lon,dc,sza,iza,pit=pit,rol=rol
 endif else begin
   if strcmp(platform,'NASAP3',3,/FOLD_CASE) then begin
     nav_nasa,dir,date,cfg_file,utc,alt,lat,lon,dc,sza,iza,lats,lons,label,pit=pit,rol=rol
   endif else begin
     message,'Wrong platform.'
   endelse
 endelse
endif else begin
 navf=file_search(dir+ll+date+ll+'nav.out',count=nnf)
 if nnf eq 1 then nav=1 else nav=0
endelse

; *******************************************************************************
; get cosine correction
; *******************************************************************************
wl=cfg(cfg_file,'wlql')
wl=strsplit(wl,' ,',escape='#',/EXTRACT)
wl=float(wl)
nl=n_elements(wl)
;if nav then get_cos,dir,date,cfg_file,utc,20000.,sza,dc,fac, dz,facn,dn,wl,ray
;get_cos,dir,date,cfg_file,utc,alt,sza,dc,facz,dz,facn,dn,lambda
;stop


; *******************************************************************************
; filter data
; *******************************************************************************
izamax=cfg(cfg_file,'izamax')
if strmid(izamax,0,1) eq '#' then izamax=-1 else izamax=float(izamax)
if uu0 eq 0 and nav eq 0 then message,'Please give interval in cfg file.'
if uu0 eq 0 then $
  flt=where(iza le izamax,nf) $ ; could also choose different filters
else $
  if nav and izamax gt 0 then flt=where(utc ge uu[0] and utc le uu[1] and iza le izamax,nf) else flt=where(utc ge uu[0] and utc le uu[1],nf)
; get all the data you want out of the restored spectra
nzen=n_elements(zenlambda) & nnad=n_elements(nadlambda)

nind=intarr(nl) & zind=intarr(nl)
for l=0,nl-1 do begin
  mm=min(abs(wl[l]-zenlambda),ind)
  zind[l]=ind
  mm=min(abs(wl[l]-nadlambda),ind)
  nind[l]=ind
endfor
; filter data
utf=utc[flt]
if nav then begin
  dcf=dc [flt]
  alf=alt[flt]
endif
nad=nspectra[*,flt]
nad=nad[nind,*]
zen=zspectra[*,flt]
zen=zen[zind,*]
if n_elements(sat) gt 0 then begin
  satind=where(sat[flt] gt 0,satnb)
  print,'Saturated spectra:',satnb
endif

; special processing
if strcmp(date,'20110822') or strcmp(date, '20110828') then begin
	
  !P.multi=0
  alt=11.
  juld=julian_day(fix(strmid(date,0,4)),fix(strmid(date,4,2)),fix(strmid(date,6,2)))
  lat0=37.3 & lon0=-122.0 ; (approximate Ames location)
  zensun,juld,utc,lat0,lon0,sza,azimuth,solfac
  get_cos,dir,date,cfg_file,utc,alt,sza,cos(sza*!pi/180.),zenlambda,facz,dz,nadlambda,facn,dn
  fac=fltarr(n_elements(zenlambda),n_elements(sza))
  fcn=fltarr(n_elements(nadlambda),n_elements(sza))
  case date of
   '20110822': begin
    pressure=1016.93
    utc0=20.2
   end
   '20110828': begin
    pressure=1014.56
    utc0=16.2
   end
  endcase

  tau=bodhaine(zenlambda,pressure, 0.)
  for i=0, n_elements(zenlambda)-1 do fac[i,*]=facz[*,i]*(exp(tau[i]*cos(sza*!pi/180.)))+dz[i]*(1-exp(tau[i]*cos(sza*!pi/180.)))
  for i=0, n_elements(nadlambda)-1 do fcn[i,*]=facn[*,i]*(exp(tau[i]*cos(sza*!pi/180.)))+dn[i]*(1-exp(tau[i]*cos(sza*!pi/180.)))

  openr,10,extra
  wlk=fltarr(2001) & flk=fltarr(2001)
  for i=250,2250 do begin
    readf,10,a,b
    wlk[i-250]=a & flk[i-250]=b
  endfor
  close,10
  flk=smooth(flk,10)

  ;utc0=20.2 ; plot spectra here
  mm=min(abs(utc-utc0),utcind)

  window,0,tit='Side-by-Side-Time-Series'
  yr=[0,1.8]
  plot ,utf,zspectra[zind[0],*]*fac[zind[0],*],xr=[u0,u1],xtit='UTC [h]',ytit='Measured Irradiance [W m!E-2!N nm!E-1!N]',yr=yr,/xs,/ys,tit='NASA Ames Side by Side SSFR-6 zenith nadir '+date
  oplot,utf,nspectra[nind[0],*]*fcn[nind[0],*],linesty=1,color=40
  a=nspectra[nind[0],utcind]*facn[utcind]
  ;oplot,utf,ray[*,0] ---> ray seems to be wrong!
  ;oplot,utf,flk[250]*0.001*cos(sza*!pi/180.)
  b=flk[250]*0.001*cos(sza[utcind]*!pi/180.)
  oplot,utf,zen[1,*]*fac,color=80
  oplot,utf,nad[1,*]*facn,linesty=1,color=120
  oplot,[utc0,utc0],yr
  ;oplot,utc,ray[*,2],linesty=2,color=40
  legend,['Zenith 500','Nadir 500 ...','Zenith 1600','Nadir 1600 ...'],textcolor=[255,40,80,120],/center,/right

  p=tvrd(true=1)
  write_png,dir+date+ll+'side-by-side-time-series.png',p
!p.multi=[0,2,1]
  window,1,tit='Side-by-Side-Spectra-Direct-Rayleigh-Corr'
  ;xr=[350,2150]
  xr=[350,2150]
  yr=[0,2]
  plot ,zenlambda,zspectra[*,utcind]*fac[*,utcind],xr=xr,/xs,yr=yr,/ys,xtit='Wavelength [nm]',ytit='Irradiance [W m!E-2!N nm!E-1!N]',tit='Spectra at utc='+strcompress(string(utc0),/remove_all)+' direct+rayleigh corr for '+date
  oplot,nadlambda,nspectra[*,utcind]*fcn[*,utcind],color=120


  legend,['Zenith','Nadir'],textcolors=[255,120],/right;,linesty=[0,0,0,0],colors=[255,120,70,200]
  nzspect=interpol(nspectra[*,utcind]*fcn[utcind],nadlambda,zenlambda)
  plot, zenlambda, nzspect/zspectra[*,utcind]*fac[*,utcind], xr=xr, /xs,/ys,yr=[0.5,1.5],xtit='Wavelenght [nm]',ytit='Ratio of nadir over zenith', tit='Ratio of nadir and zenith spectra'

  p=tvrd(true=1)
  write_png,dir+date+ll+'side-by-side-spectra-dir.png',p


  window,2,tit='Side-by-Side-Spectra-Diffuse-Corr'
  plot ,zenlambda,zspectra[*,utcind]*dz,xr=[350,2150],/xs,yr=[0,2.],xtit='Wavelength [nm]',ytit='Irradiance [W m!E-2!N nm!E-1!N]',tit='Spectra at utc='+strcompress(string(utc0),/remove_all)+' dif corr'
  oplot,nadlambda,nspectra[*,utcind]*dn,color=120
  ;oplot,wlm,smooth(0.001*(dd+dn),10)*0.7,color=200,thick=2
legend,['Zenith','Nadir'],textcolors=[255,120],/right
    nzspect=interpol(nspectra[*,utcind]*dn,nadlambda,zenlambda)
      plot, zenlambda, nzspect/(zspectra[*,utcind]*dz), xr=xr,yr=[0.5,1.5], /xs,/ys,xtit='Wavelenght [nm]',ytit='Ratio of nadir over zenith', tit='Ratio of nadir and zenith spectra'

 ; legend,['Zenith','Nadir'],textcolors=[255,120],/right;,linesty=[0,0,0,0],colors=[255,120,70,200]
  p=tvrd(true=1)
  write_png,dir+date+ll+'side-by-side-spectra-dif.png',p


endif

stop

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

