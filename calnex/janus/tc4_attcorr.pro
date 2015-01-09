@C:\tc4\pro\nav_er2.pro
@C:\tc4\pro\nav_dc8.pro
@C:\tc4\pro\legend.pro
@C:\tc4\pro\tc4_get_cos.pro
@C:\tc4\pro\cfg.pro

pro tc4_attcorr ; version NASA ER-2/DC-8

; settings *****************************************************
l='\'                        ; directory separator, change to '/' for unix
dir='C:\tc4\er2\'            ; base directory
date='20070717'
izamax=4                     ; maximum SSFR zenith angle - standard: 0.5 deg
wl         = 500             ; wavelength that is plotted
offsetcheck=1                ; if this is set, offset angles are checked. When
                             ; set to 0, the attcorr is made with the cfg file values
                             ; and output is actually written.
da     = 0.3                 ; angular resolution of offset check [deg]
; settings *****************************************************

; graphics *****************************************************
device,decomposed=0
loadct,27
; graphics *****************************************************

; read cfg file ************************************************
cfgf=dir+l+date+l+'tc4.cfg'        ; build cfg file
i0=cfg(cfgf,'interval') ; look if we should only use data within a certain time window
uu0=0
if not strcmp(i0,'#') then begin
    uu=strsplit(i0,' ,',escape='#',/EXTRACT)
    uu=float(uu)
    uu0=1
endif
platform=cfg(cfgf,'platform')
if strupcase(strmid(platform,0,1)) eq 'E' then er2=1 else er2=0
nav=cfg(cfgf,'read_nav')
if strcmp(strmid(nav,0,1),'n',/FOLD_CASE) then nav=0 else nav=1
; read cfg file ************************************************

; get spectra  ************************************************
print,'Restore SSFR data...'
restore, dir+date+l+date+'_calibspcs.out'
utc=tmhrs
nn    = n_elements(utc)
lamda = zenlambda
nl    = n_elements(lamda)
zs    = zspectra
ns    = fltarr(nl,nn)
mm=min(abs(wl-lamda),wlind)

print,'Interpolating spectra...'
for i=0,nn-1 do begin
  ns[*,i]=interpol(nspectra[*,i],nadlambda,lamda)
endfor
; get spectra  *************************************************

if offsetcheck then begin ; you can switch off this step, it is only thought for
                          ; comparing several pitch and roll offset values
; intermediate step: get nav data with a couple of offset angles ****
siz=get_screen_size()
window,0,tit='Offset Angles',xs=siz[0],ys=siz[1]*0.9
rol0   =cfg(cfgf,'rolloff') ; get roll offset
pit0   =cfg(cfgf,'pitchoff') ; get pitch offset
rolz   =float(rol0)
pitz   =float(pit0)
if da le 1. then begin
  pit0=[-1,0,1]*da+pitz
  rol0=[-1,0,1]*da+rolz
endif else begin
  pit0=[-2,-1,0,1,2]*da+pitz
  rol0=[-2,-1,0,1,2]*da+rolz
endelse
np=n_elements(pit0)&nr=n_elements(rol0)
!P.multi=[0,np,nr]
extra=cfg(cfgf,'extra') ; location of extraterrestial radiation file

for i=0,np-1 do begin
for j=0,nr-1 do begin
  if er2 then begin
    nav_er2,dir,date,l,cfgf,utc,alt,lat,lon,dc,sza,iza,offset=[pit0[i],rol0[j]],heading=hed ; pitch, roll
  endif else begin
    nav_dc8,dir,date,cfgf,utc,alt,lat,lon,dc,sza,iza,offset=[pit0[i],rol0[j]],heading=hed
  endelse
  get_cos,dir,date,cfgf,utc,alt,sza,dc,facz,dz,facn,dn,[wl,wl],ray
  r=rayleigh(sza,alt,zenlambda[wlind],extra)
  plot,tmhrs,zs[wlind,*]*facz,xr=uu,/xs,yr=[0,max(r)*1.2],psym=3
  flt=where(utc gt uu[0] and utc le uu[1] and abs(iza) lt izamax)
  if strcmp(date,'20070717') then begin ; optionally filter certain headings or times
    if er2 then flt=where(utc gt uu[0] and utc le uu[1] and abs(iza) lt izamax and (utc lt 15.2 or hed lt 320)) else flt=where(utc gt uu[0] and utc le uu[1] and abs(iza) lt izamax and (utc lt 16 or utc gt 17) )
  endif
  if strcmp(date,'20070731') then begin ; optionally filter certain headings or times
    flt=where(utc gt uu[0] and utc le uu[1] and abs(iza) lt izamax and (utc lt 15.2 or hed gt 280))
  endif

  oplot,tmhrs[flt],r[flt],psym=3,color=120
  ;oplot,tmhrs,r,psym=3,color=120
  legend,['pit'+string(pit0[i]),'rol'+string(rol0[j])],textcolors=[255,255],/bottom
endfor
endfor
p=tvrd(true=1)
write_png,dir+date+l+date+'_offset.png',p
; intermediate step: get nav data with a couple of offset angles ****
!P.multi=0
window,1,tit='heading'
plot,utc[flt],hed[flt],xr=uu,/xs,psym=3,tit=date
p=tvrd(true=1)
write_png,dir+date+l+date+'_heading.png',p

endif else begin

; get nav data with offset angles as specified in cfg file  ****
if er2 then begin
  nav_er2,dir,date,l,cfgf,utc,alt,lat,lon,dc,sza,iza,heading=hed
endif else begin
  nav_dc8,dir,date,cfgf,utc,alt,lat,lon,dc,sza,iza,heading=hed
endelse

; get cosine correction ****************************************
get_cos,dir,date,cfgf,utc,alt,sza,dc,facz,dz,facn,dn,[wl,wl],ray
flt=where(utc gt uu[0] and utc le uu[1] and abs(iza) lt izamax)
if strcmp(date,'20070717') then begin ; optionally filter certain headings or times
  flt=where(utc gt uu[0] and utc le uu[1] and abs(iza) lt izamax and (utc lt 15.2 or hed lt 320))
endif
if strcmp(date,'20070731') then begin ; optionally filter certain headings or times
  flt=where(utc gt uu[0] and utc le uu[1] and abs(iza) lt izamax and (utc lt 15.2 or hed gt 280))
endif

fac=facz
nadcorr=dn

nspectra = ns*nadcorr
for i=0,nl-1 do begin
  zspectra[i,*]=zs[i,*]*facz
endfor

; archive for cerberus
nadlambda=zenlambda
rol0   =cfg(cfgf,'rolloff') ; get roll offset
pit0   =cfg(cfgf,'pitchoff') ; get pitch offset
rol0   =float(rol0)
pit0   =float(pit0)
save, tmhrs, zenlambda, zspectra, nadlambda, nspectra, sat, status, rol0, pit0, nadcorr, flt, filename = dir+date+l+date+'_calibspcs_attcorr.out'
endelse ; offsetcheck

window,3
plot,utc[flt],cos(!pi/180.*sza[flt]),xr=uu,psym=3,tit=date
oplot,[min(utc),max(utc)],[0.82,0.82],linesty=1
oplot,utc[flt],nspectra[45,flt]/zspectra[45,flt],psym=3,color=40
p=tvrd(true=1)
write_png,dir+date+l+date+'_bruce.png',p

stop
end
