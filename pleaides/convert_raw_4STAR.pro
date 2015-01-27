;+
; NAME:
;   convert_raw_4STAR
;
; PURPOSE:
;   convert raw output from libradtran to 4STAR wavelength
;
; INPUT:
;   - none in command line
;
; OUTPUT:
;   - sp_20130913_v1.out IDL save file with lut
;     - radiances: sp[tau, ref, altitudes, wavelength, phase] 
;
; KEYWORDS:
;   - none
;
; DEPENDENCIES:
;   - none
;
; NEEDED FILES:
;   - sp_*_raw.out files to be converted
;   - wavelength definition files from 4STAR
;
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
; Written:  Samuel LeBlanc, June 26th, NASA Ames, Moffet Field, CA
; Modified:
;---------------------------------------------------------------------------

pro convert_raw_4STAR

; restore the file
dir='/home5/sleblan2/4STAR/model/'
file=dir+'sp_v1.1_20130913_raw.out'
fileout=dir+'sp_v1.1_20130913_4STAR.out'
print, 'Restoring the file: '+file
restore, file
zenlambda=starwavelength(dir+'../')
spn=fltarr(n_elements(tau),n_elements(ref),2,n_elements(zenlambda),2)
spn_irrdn=spn & spn_irrup=spn

  for t=0, n_elements(tau)-1 do begin
    for r=0, n_elements(ref)-1 do begin
      if ref[r] le 30. then begin  
         for z=0,1 do begin
          spn[t,r,z,*,0]=interpol(sp[t,r,z,*,0],wvl,zenlambda)
          spn_irrdn[t,r,z,*,0]=interpol(sp_irrdn[t,r,z,*,0],wvl,zenlambda)
          spn_irrup[t,r,z,*,0]=interpol(sp_irrup[t,r,z,*,0],wvl,zenlambda)
         end
       endif

       if ref[r] ge 5. then begin
         for z=0,1 do begin
          spn[t,r,z,*,1]=interpol(sp[t,r,z,*,1],wvl,zenlambda)
          spn_irrdn[t,r,z,*,1]=interpol(sp_irrdn[t,r,z,*,1],wvl,zenlambda)
          spn_irrup[t,r,z,*,1]=interpol(sp_irrup[t,r,z,*,1],wvl,zenlambda)
         end
       endif
       print, t,r
    endfor ;ref loop
  endfor  ;tau loop

sp=spn & sp_irrdn=spn_irrdn & sp_irrup=spn_irrup
print, 'saving to:'+fileout
save, zenlambda, z,pw,ab,tau, ref, sp, sp_irrdn,sp_irrup,sza,filename=fileout
stop
end



; function to build the 4STAR wavelenght array for both nir and vis spectrometers
function starwavelength,dir

v=read_ascii(dir+'vis_4STAR_wvl.dat')
n=read_ascii(dir+'nir_4STAR_wvl.dat')
wvl=[v.field1*1000.,n.field1*1000.]
return,wvl
end
