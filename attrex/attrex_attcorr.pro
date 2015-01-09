;+
; NAME:
;   attrex_attcorr
;
; PURPOSE:
;   Run attitude correction for ATTREX data onto the SP.out file
;
; CATEGORY:
;   ATTREX attitude correction
;
; CALLING SEQUENCE:
;   attrex_attcorr, date
;   - date is the date string in format yyyymmdd
;
; OUTPUT:
;   see above
;
; KEYWORDS:
;   none
;
; DEPENDENCIES:
;   - ingest_nav.pro - uses the bodhaine rayleigh optical depth 
; 
; NEEDED FILES:
;   - SP.out files for the flight day
;   
; EXAMPLE:
;   
;
; MODIFICATION HISTORY:
; Written:  Samuel LeBlanc, LASP CU Boulder, June 6th, 2012
; Modified: 
;           
;---------------------------------------------------------------------------
@ingest_nav.pro

pro attrex_attcorr,date

if n_elements(date) lt 1 then date='20111105'

dir='/data/seven/schmidt/attrex/gh/'
l='/'

restore, dir+date+l+date+'_SP.out'
utc_temp=utc
alt_temp=alt
lat_temp=lat
lon_temp=lon
restore, dir+date+l+date+'_met.out'

pre=interpol(pre,utc,utc_temp)
fc=fltarr(n_elements(zenlambda), n_elements(utc_temp))
tau=fac*!values.f_nan
for i=0,n_elements(pre)-1 do tau[i,*]=bodhaine(zenlambda,pre[i], 0.)
zspectra_attcorr=zspectra
utc=utc_temp
alt=alt_temp
lat=lat_temp
lon=lon_temp
for i=0, n_elements(zenlambda)-1 do begin
  fc[i,*]=fac[*,i]*(exp(tau[*,i]*cos(sza*!pi/180.)))+dz[i]*(1-exp(tau[*,i]*cos(sza*!pi/180.)))
  zspectra_attcorr[i,*]=zspectra[i,*]*fc[i,*]
endfor

save, nadlambda, zenlambda, nspectra,zspectra_attcorr, utc,sza,alt,lat,lon,pit,rol,hed,filename=dir+date+l+date+'_SP_attcorr.out'
end
