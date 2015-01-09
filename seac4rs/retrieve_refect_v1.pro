;+
; NAME:
;   retrieve_reflect_v1
;
; PURPOSE:
;   program to retrieve optical depth and effective radius from SSFR reflected irradiance
;
; CALLING SEQUENCE:
;   retrieve_reflect_v1,date
;
; INPUT:
;   - date: string in yyyymmdd format, determines which day to run the retrieval code on
;
; OUTPUT:
;   - save file of retrieved cloud properties :  retrieved_reflect_yyyymmdd_vv.out, vv refers to the version of the retrieval
;
; KEYWORDS:
;   none
;
; DEPENDENCIES:
;   - zensun.pro
;   - make_meas_std.pro
;   - make_pars_std_nas.pro
;
; NEEDED FILES:
;   - calibspc.out file from SSFR
;
; EXAMPLE:
;   IDL > .r retrieve_reflect_v1
;   IDL > retrieve_reflect_v1,'20130913'
;
;
; MODIFICATION HISTORY:
; Written:  Samuel LeBlanc, July 15th, 2014, NASA Ames, Moffett Field, CA
; Modified: 
;---------------------------------------------------------------------------

pro retrieve_reflect_v1, date

; folders of importance
dir='/home/sleblan2/4STAR/'
ll='/'
vv='v1'
datdir='/nobackup/sleblan2/SEAC4RS/er2/'


if n_elements(date) lt 1 then date='20130913'

; define the limits in time of interested parts
case date of 
 '20130913': fltime=[18.3,19.1]
 else      : message, 'date not found'
endcase


; build the lut
wvs=[515., 1628.] ;wavelengths to use for retrieval
lut=get_lut(dir+'model'+ll+'sp_v1.1_20130913_4STAR.out',wvs)

; now get the data
f=datdir+date+'_calibspcs.out'
sp=get_refl(f,lut.wvs,fltime)


; now prepare variables for saving values
; each retrieved value represents either liquid (i=0) or ice (i=1) on the second dimension
n=n_elements(sp.tmhrs)
tau_rtm=fltarr(n,2) & ref_rtm=fltarr(n,2)
tau_err=fltarr(n,2) & ref_err=fltarr(n,2)

; start loop
for i=0,n-1 do begin
  r=retrieve_cloud(sp.reflect[*,i],lut)
  tau_rtm[i,*]=[r.taul,r.taui]
  tau_err[i,*]=[r.etaul,r.etaui]
  ref_rtm[i,*]=[r.refl,r.refi]
  ref_err[i,*]=[r.erefl,r.erefi]
endfor

tmhrs=sp.tmhrs

save, tmhrs, tau_rtm, tau_err, ref_rtm, ref_err, filename=dir+date+'_retrieved_'+vv+'.out'
stop
end



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; function to build the lut for reflection
; based on the modeled irradiance file for cloud zenith transmittance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function get_lut, file,wvs

print, 'in the get_lut subfunction'
print, '..restoring file:'+file
restore, file

; find the indices for the correct wavelengths
nul=min(abs(zenlambda-wvs[0]),iv1)
nul=min(abs(zenlambda-wvs[1]),iv2)
iv=[iv1,iv2]


; build the reflectance
refl=sp_irrup[*,*,*,iv,1]/sp_irrdn[*,*,*,iv,1]

; now build a highres reflectance
print, '..interpolating to a finer grid in tau and ref'
  tau_hires=findgen(100)+1.
  ref_hires=findgen(60)+1.
  sp_hit=fltarr(n_elements(tau_hires),n_elements(ref),2,n_elements(wvs))
  sp_hi=fltarr(n_elements(tau_hires),n_elements(ref_hires),2,n_elements(wvs))
  for z=0,n_elements(wvs)-1 do $
    for w=0,1 do $
      for r=0, n_elements(ref)-1 do sp_hit[*,r,w,z]=interpol(refl[*,r,z,w],tau,tau_hires,/nan)
      ;for t=0, n_elements(tau)-1 do sp_hi[t,*,w,z]=interpol(sp_hit[t,*,w,z],ref,ref_hires,/nan)
  for z=0,n_elements(zenlambda)-1 do $
    for w=0,1 do $
      for t=0, n_elements(tau_hires)-1 do sp_hi[t,*,w,z]=interpol(sp_hit[t,*,w,z],ref,ref_hires,/nan)

tau=tau_hires
ref=ref_hires
refl=sp_hit
mu=cos(sza*!DtoR)
lut={lut,tau:tau,ref:ref,refl:refl,wvs:wvs,iv:iv,mu:mu}

return, lut
end



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; function to restore the SSFR data
; converts the full spectral data to 2 reflectance at the desired wavelengths
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function get_refl, file, wvs, fltime

print, 'in the get_refl subfunction'
print, '..restoring data file:'+file
restore, file

; get the correct wavelengths
nul=min(abs(zenlambda-wvs[0]),iv1)
nul=min(abs(zenlambda-wvs[1]),iv2)
iv=[iv1,iv2]

; subset the time array for only the period of interest
if n_elements(fltime) ne 2 then message, 'problem with time filter, needs to have only 2 elements'
fl=where(tmhrs gt fltime[0] and tmhrs le fltime[1],n)
if n lt 1 then message, 'There is no points in the time filter set' else print, 'There are '+strtrim(n,2)+' points in the filter'

; now build the reflectance fields
reflect=fltarr(n_elements(wvs),n_elements(tmhrs[fl]))
for i=0,n-1 do $
  reflect[*,i]=nspectra[iv,fl[i]]/zspectra[iv,fl[i]]

tmhrs=tmhrs[fl]
sp={sp,tmhrs:tmhrs,reflect:reflect,wvs:wvs}
return, sp
end
