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

pro write_input_tau_file, z, tau, atm_in,file
dir='/home/leblanc/arctas/'

if not keyword_set(atm_in) then in ='atmos_aero_20100519.dat' else in=atm_in

aero_tau=read_ascii(in, comment='#')
alt=aero_tau.field1[0,*] ;from 130 km down to 0km 

;file='aero_tau.level'
;tau_out=file
openw, lun, file ,/get_lun
printf, lun, '# Tau values for different levels'
printf, lun, '# Level (m)	tau'
fi=1
for i=0, n_elements(alt)-1 do begin
   
  if z gt alt and fi then  printf, lun, alt[i], tau else printf, lun, alt[i],0.
  if z gt alt then fi=0 else fi=1
endfor
close, lun

end
