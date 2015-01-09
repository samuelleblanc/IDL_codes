;+
; NAME:
;   write_input_tau_file
;
; PURPOSE:
;   write aerosol tau file for rad transfer retrieval (input for uvspec) libRadtran 1.5
;
; CATEGORY:
;   CALNEX / LibRadtran input
;
; CALLING SEQUENCE:
;   write_input, z, near
;     - z: levels of hsrl values
;     - near: structure from the calnex_hsrl_p3 (for may 16th and 19th)
;
; OUTPUT:
;   aerosol optical depth for each levels required by uvspec in a file
;
; KEYWORDS:
;   - index:   specifies the index to use from the path (which point on near.extinction to use)
;   - atm_in:  specifies the atmosphere file to use
;   - tau_out: returns the optical depth out file name
;   - alt:     returns the altitude at which tau is calculated
;   - tau_good:returns the optical thickness at different altitudes
;
; DEPENDENCIES:
;
; NEEDED FILES:
;   - AERO_TAU.DAT file
;
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
; Written:  Samuel LeBlanc, LASP CU Boulder,August 11th, 2010
; Modified:  August, 18th, 2010 by Samuel LeBlanc
;           - added tau_good and alt keywords for returning the values of tau at different altitudes
;
;- --------------------------------------------------------------------------

pro write_input_tau_file, z, near, index=index, atm_in=atm_in, tau_out=tau_out, quiet=quiet, tau_good=tau_good, alt=alt

if not keyword_set(index) then index=0 

if !VERSION.OS eq 'linux' then linux=1 else linux=0 

if linux then dir='/home/leblanc/libradtran/input/aero/' else dir='\\lasp-smb\leblanc\libradtran\input\aero\'

if not keyword_set(atm_in) then in ='atmos_aero_20100519.dat' else in=atm_in

if not keyword_set(quiet) then print, 'opening file ' + dir+in+' to read z levels'
aero_tau=read_ascii(dir+in, comment='#')
alt=aero_tau.field1[0,*] ;from 130 km down to 0km 
;print, near.extinction[*,index],z
;z=[z,30000.0,40000.0]      ;make 30km and 40km altitude go to zero
;extinction=[near.extinction,fltarr(1,81),fltarr(1,81)]
tau_good=alt*0.0
for j=1, n_elements(alt)-1 do begin
  alts=where((z/1000.0) le alt[j-1] and (z/1000.0) ge alt[j],count)
  if count gt 0 then begin
    for ii=0 , count-1 do if not finite(near.extinction[alts[ii],index]) then near.extinction[alts[ii],index]=0.0
    tau_good[j] = total(near.extinction[alts, index] * (z[alts+1]-z[alts])/1000.0,/nan)    
  endif else tau_good[j]=0.0
endfor

file='aero_tau.level'
tau_out=file
if not keyword_set(quiet) then print, 'setting up file to' + dir+file

openw, lun, dir+file ,/get_lun
printf, lun, '# Tau values for different levels'
printf, lun, '# Level (m)	tau'

for i=0, n_elements(alt)-1 do begin
	if not finite(tau_good[i]) or tau_good[i] lt 0.0 then tau_good[i]=0.0
	printf, lun, alt[i], tau_good[i]
endfor
close, lun

end
