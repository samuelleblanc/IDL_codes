;+
; NAME:
;   calnex_crd
;
; PURPOSE:
;   to get the extinction from the crd file, and compare to the retrieved values of optical depth
;   and to get absorption coefficient from PAS
;
; CATEGORY:
;   CALNEX / CRD / comparison / PAS
;
; CALLING SEQUENCE:
;   calnex_crd, date
; - date - date of day to check
;
; OUTPUT:
;   - idl save file with extinction, gamma, angstrom, RH for the caltech loops
;
; KEYWORDS:
;   
;
; DEPENDENCIES:
;   
;
; NEEDED FILES:
;   - crd file
;   - PAS file
;   - aircraft, met file
;
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
; Written:  Samuel LeBlanc, LASP CU Boulder, February 4th, 2011
; Modified: 
; 
;---------------------------------------------------------------------------
pro calnex_crd, dateinp, time, ext, ext_ambient, abs

;set windows or linux
if !VERSION.OS eq 'linux' then linux=1 else linux=0

if n_elements(dateinp) lt 1 then begin
  date='20100519'
endif else begin
  date=strcompress(string(dateinp),/REMOVE_ALL)
endelse

if linux then begin
  dir='/home/leblanc/CALNEX/p3/'+date+'/'
endif else begin
  dir='\\lasp-smb\leblanc\CALNEX\p3\'+date+'\'
endelse

; get met file
f=file_search(dir+'*'+date+'*.ict.met', count=ct)
if ct lt 1 then message, 'met file not found!'
length=[0.,0.]
openu, lun, f[0], /get_lun
readf, lun, length
free_lun, lun
close, /all
met=read_ascii(f[0],data_start= length[0])
temp=met.field01[1,*] +273.15 ;temperature (Kelvin)
RH=met.field01[6,*]    ;Relative humidity (%)

; get pos file (for altitude)
f=file_search(dir+'*'+date+'*.ict.pos', count=ct)
if ct lt 1 then message, 'pos file not found!'
length=[0.,0.]
openu, lun, f[0], /get_lun
readf, lun, length
free_lun, lun
close, /all
pos=read_ascii(f[0],data_start= length[0])
time=pos.field1[0,*]/3600. ; time data
alt=pos.field1[1,*]  ; altitude data
lat=pos.field1[2,*]  ; latitude data
lon=pos.field1[3,*]  ; longitude data

; get CRD .ict file
f=file_search(dir+'CRD*'+date+'*.ict', count=ct)
if ct lt 1 then message, 'CRD file not found!'
length=[0.,0.]
openu, lun, f[0], /get_lun
readf, lun, length
free_lun, lun
close, /all
crd=read_ascii(f[0],data_start= length[0])
ext=crd.field1[1,*]         ; dry extinction coefficient at 532nm in Mm-1
gamma=crd.field1[2,*]       ; gamma correction for extinction coefficient unitless
angstrom=crd.field1[3,*]    ; angstrom exponent for extinction coefficient



; need to interpol between times of no measurement of gamma
kk=where(gamma ne -9999.00,ct)
if ct gt 0 then gamma_int=interpol(gamma[kk],time[kk],time)
kk=where(angstrom ne -9999.0, ct)
if ct gt 0 then angstrom_int=interpol(angstrom[kk],time[kk],time)

;take out bad data
kk=where(ext eq -9999.0,ct)
if ct gt 0 then ext[kk]=!values.f_nan
kk=where(gamma eq -9999.0,ct)
if ct gt 0 then gamma[kk]=!values.f_nan
kk=where(angstrom eq -9999.0,ct)
if ct gt 0 then angstrom[kk]=!values.f_nan

;calculate the extinction coefficient at ambient RH using the gamma relation
ext_ambient=ext*(100./(100.-RH))^gamma_int

; get PAS .ict file
f=file_search(dir+'PAS*'+date+'*.ict', count=ct)
if ct lt 1 then message, 'PAS file not found!'
length=[0.,0.]
openu, lun, f[0], /get_lun
readf, lun, length
free_lun, lun
close, /all
pas=read_ascii(f[0],data_start= length[0])
abs=pas.field1[1,*]         ; dry extinction coefficient at 532nm in Mm-1
kk=where(abs eq -9999.0,ct)
if ct gt 0 then abs[kk]=!values.f_nan

end








