;+
; NAME:
;   write_input_aats
;
; PURPOSE:
;   write aerosol tau file for rad transfer retrieval (input for uvspec) libRadtran 1.5
;
; CATEGORY:
;   CALNEX / LibRadtran input
;
; CALLING SEQUENCE:
;   write_input_aats, tau, top, bottom, tau_above
;     - tau
;     - top: altitude of high leg
;     - bottom: altitude of lower leg
;     - tau_above: optical depth above layer
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
;   - dirin:   the directory to save the files
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
; Written:  Samuel LeBlanc, LASP CU Boulder,August 31st, 2010, Ottawa, Ontario, Canada
; Modified: by Samuel LeBlanc, LASP CU Boulder, November 14th, 2011, Folsom Street Coffee, Boulder, CO
;         - added the dirin keyword
;         - change the tau_out keyword to also take input for a new aerosol level out file name
;- --------------------------------------------------------------------------

pro write_input_aats, tau, top, bottom, tau_above, index=index, atm_in=atm_in, tau_out=tau_out, quiet=quiet, tau_approx=tau_approx, tau_good=tau_good, alt=alt, dirin=dirin

if not keyword_set(index) then index=0 

if tau_approx then begin
  tau=round(tau*100.)/100.0
  if tau eq 0. then tau=tau+0.002
  if not keyword_set(quiet) then print, 'tau is now set to :' , tau
endif

if tau eq 0.0 then print, '**CAUTION** : tau is at zero'

if !VERSION.OS eq 'linux' then linux=1 else linux=0 

if not keyword_set(dirin) then begin
  if linux then dir='/home/leblanc/libradtran/input/aero/' else dir='\\lasp-smb\leblanc\libradtran\input\aero\'
endif else begin
  dir=dirin
endelse
if not keyword_set(atm_in) then in ='atmos_aero_20100519.dat' else in=atm_in

if not keyword_set(quiet) then print, 'opening file ' + dir+in+' to read z levels'
aero_tau=read_ascii(dir+in, comment='#')
alt=aero_tau.field1[0,*] ; from 130 km down to 0km 
tau_good=reform(alt)*0.0

middle=(top+bottom)/2.

mm=min(abs(alt-middle),t_z)
tau_good[t_z]=tau

mm=min(abs(alt-top),t_t)
ta=tau_above/5.0

for i=t_t-2, t_t-6,-1 do tau_good[i]=ta ; set up higher values of tau

if n_elements(tau_out) eq 0 then file='aero_tau.level' else file=tau_out
tau_out=file
if not keyword_set(quiet) then print, 'setting up file to' + dir+file
lun=97
openw, lun, dir+file ;,/get_lun
printf, lun, '# Tau values for different levels'
printf, lun, '# Level (m)	tau'

for i=0, n_elements(alt)-1 do begin
	if not finite(tau_good[i]) or tau_good[i] lt 0.0 then tau_good[i]=0.0
	printf, lun, alt[i], tau_good[i]
endfor
close, lun

end