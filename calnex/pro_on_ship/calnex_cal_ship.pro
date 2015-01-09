;+
; NAME:
;   calnex_cal_ship
;
; PURPOSE:
;   Calibration procedure for the ship data. To be used only when primary calibration is done
;
; CATEGORY:
;   CALNEX, Ship/Atlantis,  Calibrations
;
; CALLING SEQUENCE:
;   calnex_cal_ship
;
; OUTPUT:
;       calibration file for each detector
;       plots of calibration
;
; KEYWORDS:
;       
;
; DEPENDENCIES:
;   cfg.pro
;   legend.pro
;   
; NEEDED FILES:
;   - config file for calibration of ship "calnex_ship.cfg"
;   - calibration spectras for each primary and secondary 
;   - response functions for each lamp and sphere
;
; EXAMPLE:
; 
;
; MODIFICATION HISTORY:
; Written:  Samuel LeBlanc, ATOC CU Boulder Based on calnex_cal code written by Sebastian Schmidt
; Modified: Wednesday, April 28th, 2010
;           
;-
;---------------------------------------------------------------------------

@cfg.pro
@legend.pro

pro calnex_cal_ship

path='/data/seven/schmidt/calnex/cal'
l   ='/'                         ; directory separator
cfg_file=path+l+'calnex_ship.cfg'

device,decomposed=0
loadct,27
!P.multi=0

if file_test(cfg_file) ne 1 then message,'Did not find configuration file.'

months = ["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"]

sinp                    = fix(cfg(cfg_file,'sinp'  ))    ; number of channels for each spectrum in Si
irnp                    = fix(cfg(cfg_file,'irnp'))      ; number of channels used in the IR

platform              = cfg(cfg_file,'platform')
data                  = cfg(cfg_file,'data')    ; data file extension

;for irradiance (zenith)
irradiance_nist_traceable_source = cfg(cfg_file,'irradiance_nist_traceable_source')
irradiance_primary_lamp          = cfg(cfg_file,'irradiance_primary_lamp')
irradiance_lamp                  = cfg(cfg_file,'irradiance_lamp')

;for radiance (nadir)
radiance_nist_traceable_source = cfg(cfg_file,'radiance_nist_traceable_source')
radiance_primary_lamp          = cfg(cfg_file,'radiance_primary_lamp')
radiance_sphere                = cfg(cfg_file,'radiance_sphere')

primary_cal_date      = cfg(cfg_file,'primary_cal_date')
secondary_cal_date    = cfg(cfg_file,'secondary_cal_date')

; read file location for primary, transfer, and integration times
primary_nadir_si      = strsplit(cfg(cfg_file,'primary_nadir_si')   ,', ',escape='#',/EXTRACT)
primary_nadir_ir      = strsplit(cfg(cfg_file,'primary_nadir_ir')   ,', ',escape='#',/EXTRACT)
primary_zenith_si     = strsplit(cfg(cfg_file,'primary_zenith_si')  ,', ',escape='#',/EXTRACT)
primary_zenith_ir     = strsplit(cfg(cfg_file,'primary_zenith_ir')  ,', ',escape='#',/EXTRACT)

transfer_nadir_si     = strsplit(cfg(cfg_file,'transfer_nadir_si')  ,', ',escape='#',/EXTRACT)
transfer_nadir_ir     = strsplit(cfg(cfg_file,'transfer_nadir_ir')  ,', ',escape='#',/EXTRACT)
transfer_zenith_si    = strsplit(cfg(cfg_file,'transfer_zenith_si') ,', ',escape='#',/EXTRACT)
transfer_zenith_ir    = strsplit(cfg(cfg_file,'transfer_zenith_ir') ,', ',escape='#',/EXTRACT)

secondary_nadir_si    = strsplit(cfg(cfg_file,'secondary_nadir_si') ,', ',escape='#',/EXTRACT)
secondary_nadir_ir    = strsplit(cfg(cfg_file,'secondary_nadir_ir') ,', ',escape='#',/EXTRACT)
secondary_zenith_si   = strsplit(cfg(cfg_file,'secondary_zenith_si'),', ',escape='#',/EXTRACT)
secondary_zenith_ir   = strsplit(cfg(cfg_file,'secondary_zenith_ir'),', ',escape='#',/EXTRACT)

zsi_intime_p = fix(primary_zenith_si[1])  & primary_zenith_si = primary_zenith_si[0]
zir_intime_p = fix(primary_zenith_ir[1])  & primary_zenith_ir = primary_zenith_ir[0]
nsi_intime_p = fix(primary_nadir_si[1])   & primary_nadir_si  = primary_nadir_si[0]
nir_intime_p = fix(primary_nadir_ir[1])   & primary_nadir_ir  = primary_nadir_ir[0]
zsi_intime_t = fix(transfer_zenith_si[1]) & transfer_zenith_si= transfer_zenith_si[0]
zir_intime_t = fix(transfer_zenith_ir[1]) & transfer_zenith_ir= transfer_zenith_ir[0]
nsi_intime_t = fix(transfer_nadir_si[1])  & transfer_nadir_si = transfer_nadir_si[0]
nir_intime_t = fix(transfer_nadir_ir[1])  & transfer_nadir_ir = transfer_nadir_ir[0]
zsi_intime_s = fix(secondary_zenith_si[1]) & secondary_zenith_si= secondary_zenith_si[0]
zir_intime_s = fix(secondary_zenith_ir[1]) & secondary_zenith_ir= secondary_zenith_ir[0]
nsi_intime_s = fix(secondary_nadir_si[1])  & secondary_nadir_si = secondary_nadir_si[0]
nir_intime_s = fix(secondary_nadir_ir[1])  & secondary_nadir_ir = secondary_nadir_ir[0]
if zsi_intime_p ne zsi_intime_t then message,'Primary and Transfer integration times are not equal (ZSI).'
if zir_intime_p ne zir_intime_t then message,'Primary and Transfer integration times are not equal (ZIR).'
if nsi_intime_p ne nsi_intime_t then message,'Primary and Transfer integration times are not equal (NSI).'
if nir_intime_p ne nir_intime_t then message,'Primary and Transfer integration times are not equal (NIR).'


resp_func_dir = cfg(cfg_file,'resp_func_dir')

dozenith = cfg(cfg_file,'dozenith')
donadir  = cfg(cfg_file,'donadir')
write_primary  = cfg(cfg_file,'write_primary')

; wavelengths
wlvisz=fltarr(sinp) & wlnirz=fltarr(irnp)
wlvisn=fltarr(sinp) & wlnirn=fltarr(irnp)

; definitions
bignum=20000L
darksi  =fltarr(sinp,bignum,2)          ; 0 - zen si, 1 - zen ir, 2 - nad si, 3 - nad ir
darkmsi =fltarr(sinp,2)                  ; 0 - zen, 1 - nad

darkir=fltarr(irnp,bignum,2)
darkmir=fltarr(irnp,bignum,2)

calisi  =fltarr(sinp,bignum,2)
calimsi =fltarr(sinp,2)

caliir  =fltarr(irnp,bignum,2)
calimir =fltarr(irnp,2)

resp1si =fltarr(sinp,2) ; primary response function
resp1ir =fltarr(irnp,2)
resp2si =fltarr(sinp,2) ; secondary response function
resp2ir =fltarr(irnp,2)

; make wavelengths / zenith
zlambdasi=cfg(cfg_file,'zlambdaSI') ; wavelength coefficients SI
zlambdasi=strsplit(zlambdasi,' ,',count=znlsi,escape='#',/EXTRACT) ; make string into substrings
zlambdasi=float(zlambdasi)
zlambdair=cfg(cfg_file,'zlambdaIR') ; wavelength coefficients IR
zlambdair=strsplit(zlambdair,' ,',count=znlir,escape='#',/EXTRACT) ; make string into substrings
zlambdair=float(zlambdair)
wlvisz=fltarr(sinp) & wlnirz=fltarr(irnp)
for i=0,sinp-1 do begin
   for j=0,znlsi-1 do wlvisz[i]     =wlvisz[i]     +zlambdasi[j]*float(i)^j
endfor

for i=0,irnp-1 do begin
   for j=0,znlir-1 do wlnirz[i]=wlnirz[i]+zlambdair[j]*float(i)^j
endfor

; make wavelengths / nadir
nlambdasi=cfg(cfg_file,'nlambdaSI') ; wavelength coefficients SI
nlambdasi=strsplit(nlambdasi,' ,',count=nnlsi,escape='#',/EXTRACT) ; make string into substrings
nlambdasi=float(nlambdasi)
nlambdair=cfg(cfg_file,'nlambdaIR') ; wavelength coefficients IR
nlambdair=strsplit(nlambdair,' ,',count=nnlir,escape='#',/EXTRACT) ; make string into substrings
nlambdair=float(nlambdair)
wlvisn=fltarr(sinp) & wlnirn=fltarr(irnp)
for i=0,sinp-1 do begin
   for j=0,nnlsi-1 do wlvisn[i]     =wlvisn[i]     +nlambdasi[j]*float(i)^j
endfor

for i=0,irnp-1 do begin
   for j=0,nnlir-1 do wlnirn[i]=wlnirn[i]+nlambdair[j]*float(i)^j
endfor

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PRIMARY CALIBRATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (1) process primary calibration - darks
spects = {btime: lonarr(2), bcdtimstp:  bytarr(12),intime1: long(0),$        ; this is the reference spectra structure
                     intime2: long(0), intime3: long(0), intime4: long(0),accum: long(0), shsw:long(0),$
                     zsit: ulong(0),nsit: ulong(0),zirt: ulong(0),nirt: ulong(0), zirx: ulong(0), nirx: ulong(0),xt: ulong(0),it: ulong(0),$
                     zspecsi: intarr(sinp), zspecir: intarr(sinp), nspecsi: intarr(sinp), nspecir: intarr(sinp)} 

;SSFR 7 InGaAs saves in 256 channels, but only 128 are used and NOT reversed

specn = spects
                     
if(strcmp(donadir,'yes')) then begin
    filen=primary_nadir_si+'dark'+l
    nspc_files = file_search(filen+data, count = numfiles, /FOLD_CASE)
    if (numfiles ne 1) then message,'There should be exactly 1 file for the dark.'
    openr,lunn,nspc_files[0],/get_lun
    ctn=0L
    while not eof(lunn) do begin
        readu, lunn, specn
        if(specn.intime3 ne nsi_intime_p) then message, "NSI int times don't match"
        darksi[*,ctn,1]=specn.nspecsi
        ctn = ctn + 1L
    endwhile
    ; check date
    atime  = systime(0, specn.btime[0], /utc) ; convert date
    result = strpos(atime(0), ':', 0) ; find first incidence of ':'
    day1 =  strmid(atime(0), result - 5, 1)
    day2 =  strmid(atime(0), result - 4, 1)
    if(day1 eq ' ') then begin
      day = '0' + day2
    endif else begin
      day = day1 + day2
    endelse
    mon =  strmid(atime(0), result - 9, 3)
    mon = strtrim(string(where(months eq mon) + 1),1)
    if(mon lt 10) then begin
      mon = '0' + string(mon[0])
    endif else begin
      mon = string(mon[0])
    endelse
    year = fix(strmid(atime(0), result + 7, 4)) ;get year
    mydate = strtrim(string(year,mon,day),1)
    if not strcmp(primary_cal_date,mydate,8) then message,'Primary cal date incorrect.'
    for j=0,sinp-1 do begin       ; go through channels
        darkmsi[j,1]=mean(darksi[j,0:ctn-1,1])
    endfor
    free_lun,lunn
    ;
    filen=primary_nadir_ir+'dark'+l
    nspc_files = file_search(filen+data, count = numfiles, /FOLD_CASE)
    if (numfiles ne 1) then message,'There should be exactly 1 file for the dark.'
    openr,lunn,nspc_files[0],/get_lun
    ctn=0L
    while not eof(lunn) do begin
        readu, lunn, specn
        if(specn.intime4 ne nir_intime_p) then message, "NIR int times don't match"
        darkir[*,ctn,1]=specn.nspecir[0:irnp-1]
        ctn = ctn + 1L
    endwhile
    for j=0,irnp-1 do begin       ; go through channels
        darkmir[j,1]=mean(darkir[j,0:ctn-1,1])
    endfor
    free_lun,lunn
endif
;
if(strcmp(dozenith,'yes')) then begin
filez=primary_zenith_si+'dark'+l
zspc_files = file_search(filez+data, count = numfiles, /FOLD_CASE)
if (numfiles ne 1) then message,'There should be exaclt 1 file for the dark.'
openr,lunz,zspc_files[0],/get_lun
ctz=0L
specz = spects
while not eof(lunz) do begin
    readu, lunz, specz
    if(specz.intime1 ne zsi_intime_p) then message, "ZSI int times don't match"
    darksi[*,ctz,0]=specz.zspecsi
    ctz = ctz + 1L
endwhile
for j=0,sinp-1 do begin ; go through channels
    darkmsi[j,0]=mean(darksi[j,0:ctz-1,0])
endfor
free_lun,lunz
;
filez=primary_zenith_ir+'dark'+l
zspc_files = file_search(filez+data, count = numfiles, /FOLD_CASE)
if (numfiles ne 1) then message,'There should be exaclt 1 file for the dark.'
openr,lunz,zspc_files[0],/get_lun
ctz=0L
specz = spects
while not eof(lunz) do begin
    readu, lunz, specz
    if(specz.intime2 ne zir_intime_p) then message, "ZIR int times don't match"
    darkir[*,ctz,0]=specz.zspecir[0:irnp-1]
    ctz = ctz + 1L
endwhile
for j=0,irnp-1 do begin ; go through channels
    darkmir[j,0]=mean(darkir[j,0:ctz-1,0])
endfor
free_lun,lunz
endif

; (2) process primary calibration - calibration
specn = spects
if(strcmp(donadir,'yes')) then begin
    filen=primary_nadir_si+'cal'+l
    nspc_files = file_search(filen+data, count = numfiles, /FOLD_CASE)
    if (numfiles ne 1) then message,'There should be exactly 1 file for the dark.'
    openr,lunn,nspc_files[0],/get_lun
    ctn=0L
    while not eof(lunn) do begin
        readu, lunn, specn
        if(specn.intime3 ne nsi_intime_p) then message, "NSI int times don't match"
        calisi[*,ctn,1]=specn.nspecsi
        ctn = ctn + 1L
    endwhile
    for j=0,sinp-1 do begin       ; go through channels
        calimsi[j,1]=mean(calisi[j,0:ctn-1,1])
    endfor
    free_lun,lunn
    ;
    filen=primary_nadir_ir+'cal'+l
    nspc_files = file_search(filen+data, count = numfiles, /FOLD_CASE)
    if (numfiles ne 1) then message,'There should be exactly 1 file for the dark.'
    openr,lunn,nspc_files[0],/get_lun
    ctn=0L
    while not eof(lunn) do begin
        readu, lunn, specn
        if(specn.intime4 ne nir_intime_p) then message, "NIR int times don't match"
        caliir[*,ctn,1]=specn.nspecir[0:irnp-1]
        ctn = ctn + 1L
    endwhile
    for j=0,irnp-1 do begin       ; go through channels
        calimir[j,1]=mean(caliir[j,0:ctn-1,1])
    endfor
    free_lun,lunn
endif
;
if(strcmp(dozenith,'yes')) then begin
filez=primary_zenith_si+'cal'+l
zspc_files = file_search(filez+data, count = numfiles, /FOLD_CASE)
if (numfiles ne 1) then message,'There should be exaclt 1 file for the dark.'
openr,lunz,zspc_files[0],/get_lun
ctz=0L
specz = spects
while not eof(lunz) do begin
    readu, lunz, specz
    if(specz.intime1 ne zsi_intime_p) then message, "ZSI int times don't match"
    calisi[*,ctz,0]=specz.zspecsi
    ctz = ctz + 1L
endwhile
for j=0,sinp-1 do begin ; go through channels
    calimsi[j,0]=mean(calisi[j,0:ctz-1,0])
endfor
free_lun,lunz
;
filez=primary_zenith_ir+'cal'+l
zspc_files = file_search(filez+data, count = numfiles, /FOLD_CASE)
if (numfiles ne 1) then message,'There should be exaclt 1 file for the dark.'
openr,lunz,zspc_files[0],/get_lun
ctz=0L
specz = spects
while not eof(lunz) do begin
    readu, lunz, specz
    if(specz.intime2 ne zir_intime_p) then message, "ZIR int times don't match"
    caliir[*,ctz,0]=specz.zspecir[0:irnp-1]
    ctz = ctz + 1L
endwhile
for j=0,irnp-1 do begin ; go through channels
    calimir[j,0]=mean(caliir[j,0:ctz-1,0])
endfor
free_lun,lunz
endif

; (3) read in lamp data
; For irradiance
nl=file_lines(resp_func_dir+l+irradiance_nist_traceable_source)
openr,lunl,resp_func_dir+l+irradiance_nist_traceable_source,/get_lun
pl=fltarr(nl) & pi=fltarr(nl)
for i=0,nl-1 do begin
  readf,lunl,a,b
  pl[i]=a & pi[i]=b
endfor

free_lun,lunl

print,'Warning, the lamp splining should be replaced by Planck fitting!'
lampzensi = spline(pl,pi,wlvisz) ; spline lamp response to SSFR wavelengths
lampzenir = spline(pl,pi,wlnirz)

;For Radiance measurements
nl=file_lines(resp_func_dir+l+radiance_nist_traceable_source)
openr,lunl,resp_func_dir+l+radiance_nist_traceable_source,/get_lun
pl=fltarr(nl) & pi=fltarr(nl)
for i=0,nl-1 do begin
  readf,lunl,a,b
  pl[i]=a & pi[i]=b
endfor

lampnadsi = spline(pl,pi,wlvisn) ; spline lamp response to SSFR wavelengths
lampnadir = spline(pl,pi,wlnirn)

;temp=3200.
;plzensi=fplanck(temp,wlvisz*0.001)
;plot,pl,pi,psym=1
;oplot,wlvisz,plzensi*max(pi)/max(plzensi)
;-->figure out temp of lamp
;-->recreate lamp spectra, integrate to get total irradiance


;stop

;; Get the units right
if(irradiance_nist_traceable_source eq 'radiance_3lamps_20071219.txt') then begin
    lampzensi/=1000.
    lampzenir/=1000.
    lampnadsi/=1000.
    lampnadir/=1000.
endif else begin
    lampzensi*=1E4
    lampzenir*=1E4
    lampnadsi*=1E-3
    lampnadir*=1E-3
endelse

dn_1ir = calimir - darkmir             
dn_1si = calimsi - darkmsi             ; 0 - zen, 1 - nad

;    print,'Reverse NIR wavelengths.'
;    dn_1ir[*,0] = reverse(dn_1ir[*,0])
;    dn_1ir[*,1] = reverse(dn_1ir[*,1])

resp1si[*,0] = dn_1si[*,0]/float(zsi_intime_p)/(lampzensi)
resp1ir[*,0] = dn_1ir[*,0]/float(zir_intime_p)/(lampzenir)

resp1si[*,1] = dn_1si[*,1]/float(nsi_intime_p)/(lampnadsi)
resp1ir[*,1] = dn_1ir[*,1]/float(nir_intime_p)/(lampnadir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TRANSFER CALIBRATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; (4) transfer of calibration - darks
specn = spects
if(strcmp(donadir,'yes')) then begin
    filen=transfer_nadir_si+'dark'+l
    nspc_files = file_search(filen+data, count = numfiles, /FOLD_CASE)
    if (numfiles ne 1) then message,'There should be exactly 1 file for the dark.'
    openr,lunn,nspc_files[0],/get_lun
    ctn=0L
    while not eof(lunn) do begin
        readu, lunn, specn
        if(specn.intime3 ne nsi_intime_t) then message, "NSI int times don't match"
        darksi[*,ctn,1]=specn.nspecsi
        ctn = ctn + 1L
    endwhile
    for j=0,sinp-1 do begin       ; go through channels
        darkmsi[j,1]=mean(darksi[j,0:ctn-1,1])
    endfor
    free_lun,lunn
    ;
    filen=transfer_nadir_ir+'dark'+l
    nspc_files = file_search(filen+data, count = numfiles, /FOLD_CASE)
    if (numfiles ne 1) then message,'There should be exactly 1 file for the dark.'
    openr,lunn,nspc_files[0],/get_lun
    ctn=0L
    while not eof(lunn) do begin
        readu, lunn, specn
        if(specn.intime4 ne nir_intime_t) then message, "NIR int times don't match"
        darkir[*,ctn,1]=specn.nspecir[0:irnp-1]
        ctn = ctn + 1L
    endwhile
    for j=0,irnp-1 do begin       ; go through channels
        darkmir[j,1]=mean(darkir[j,0:ctn-1,1])
    endfor
    free_lun,lunn
endif
;
if(strcmp(dozenith,'yes')) then begin
filez=transfer_zenith_si+'dark'+l
zspc_files = file_search(filez+data, count = numfiles, /FOLD_CASE)
if (numfiles ne 1) then message,'There should be exaclt 1 file for the dark.'
openr,lunz,zspc_files[0],/get_lun
ctz=0L
specz = spects
while not eof(lunz) do begin
    readu, lunz, specz
    if(specz.intime1 ne zsi_intime_t) then message, "ZSI int times don't match"
    darksi[*,ctz,0]=specz.zspecsi
    ctz = ctz + 1L
endwhile
for j=0,sinp-1 do begin ; go through channels
    darkmsi[j,0]=mean(darksi[j,0:ctz-1,0])
endfor
free_lun,lunz
;
filez=transfer_zenith_ir+'dark'+l
zspc_files = file_search(filez+data, count = numfiles, /FOLD_CASE)
if (numfiles ne 1) then message,'There should be exaclt 1 file for the dark.'
openr,lunz,zspc_files[0],/get_lun
ctz=0L
specz = spects
while not eof(lunz) do begin
    readu, lunz, specz
    if(specz.intime2 ne zir_intime_t) then message, "ZIR int times don't match"
    darkir[*,ctz,0]=specz.zspecir[0:irnp-1]
    ctz = ctz + 1L
endwhile
for j=0,irnp-1 do begin ; go through channels
    darkmir[j,0]=mean(darkir[j,0:ctz-1,0])
endfor
free_lun,lunz
endif

; (5) transfer of calibration - cal
specn = spects
if(strcmp(donadir,'yes')) then begin
    filen=transfer_nadir_si+'cal'+l
    nspc_files = file_search(filen+data, count = numfiles, /FOLD_CASE)
    if (numfiles ne 1) then message,'There should be exactly 1 file for the dark.'
    openr,lunn,nspc_files[0],/get_lun
    ctn=0L
    while not eof(lunn) do begin
        readu, lunn, specn
        if(specn.intime3 ne nsi_intime_t) then message, "NSI int times don't match"
        calisi[*,ctn,1]=specn.nspecsi
        ctn = ctn + 1L
    endwhile
    for j=0,sinp-1 do begin       ; go through channels
        calimsi[j,1]=mean(calisi[j,0:ctn-1,1])
    endfor
    free_lun,lunn
    ;
    filen=transfer_nadir_ir+'cal'+l
    nspc_files = file_search(filen+data, count = numfiles, /FOLD_CASE)
    if (numfiles ne 1) then message,'There should be exactly 1 file for the dark.'
    openr,lunn,nspc_files[0],/get_lun
    ctn=0L
    while not eof(lunn) do begin
        readu, lunn, specn
        if(specn.intime4 ne nir_intime_t) then message, "NIR int times don't match"
        caliir[*,ctn,1]=specn.nspecir[0:irnp-1]
        ctn = ctn + 1L
    endwhile
    for j=0,irnp-1 do begin       ; go through channels
        calimir[j,1]=mean(caliir[j,0:ctn-1,1])
    endfor
    free_lun,lunn
endif
;
if(strcmp(dozenith,'yes')) then begin
filez=transfer_zenith_si+'cal'+l
zspc_files = file_search(filez+data, count = numfiles, /FOLD_CASE)
if (numfiles ne 1) then message,'There should be exaclt 1 file for the dark.'
openr,lunz,zspc_files[0],/get_lun
ctz=0L
specz = spects
while not eof(lunz) do begin
    readu, lunz, specz
    if(specz.intime1 ne zsi_intime_t) then message, "ZSI int times don't match"
    calisi[*,ctz,0]=specz.zspecsi
    ctz = ctz + 1L
endwhile
for j=0,sinp-1 do begin ; go through channels
    calimsi[j,0]=mean(calisi[j,0:ctz-1,0])
endfor
free_lun,lunz
;
filez=transfer_zenith_ir+'cal'+l
zspc_files = file_search(filez+data, count = numfiles, /FOLD_CASE)
if (numfiles ne 1) then message,'There should be exaclt 1 file for the dark.'
openr,lunz,zspc_files[0],/get_lun
ctz=0L
specz = spects
while not eof(lunz) do begin
    readu, lunz, specz
    if(specz.intime2 ne zir_intime_t) then message, "ZIR int times don't match"
    caliir[*,ctz,0]=specz.zspecir[0:irnp-1]
    ctz = ctz + 1L
endwhile
for j=0,irnp-1 do begin ; go through channels
    calimir[j,0]=mean(caliir[j,0:ctz-1,0])
endfor
free_lun,lunz
endif

dn_2si = calimsi - darkmsi
dn_2ir = calimir - darkmir

;    dn_2ir[*,0] = reverse(dn_2ir[*,0])
;    dn_2ir[*,1] = reverse(dn_2ir[*,1])


transferir = dn_1ir / dn_2ir
transfersi = dn_1si / dn_2si

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SECONDARY CALIBRATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; (6) secondary calibration - darks
specn = spects
if(strcmp(donadir,'yes')) then begin
    filen=secondary_nadir_si+'dark'+l
    nspc_files = file_search(filen+data, count = numfiles, /FOLD_CASE)
    if (numfiles ne 1) then message,'There should be exactly 1 file for the dark.'
    openr,lunn,nspc_files[0],/get_lun
    ctn=0L
    while not eof(lunn) do begin
        readu, lunn, specn
        if(specn.intime3 ne nsi_intime_s) then message, "NSI int times don't match"
        darksi[*,ctn,1]=specn.nspecsi
        ctn = ctn + 1L
    endwhile
    ; check date
    atime  = systime(0, specn.btime[0], /utc) ; convert date
    result = strpos(atime(0), ':', 0) ; find first incidence of ':'
    day1 =  strmid(atime(0), result - 5, 1)
    day2 =  strmid(atime(0), result - 4, 1)
    if(day1 eq ' ') then begin
      day = '0' + day2
    endif else begin
      day = day1 + day2
    endelse
    mon =  strmid(atime(0), result - 9, 3)
    mon = strtrim(string(where(months eq mon) + 1),1)
    if(mon lt 10) then begin
      mon = '0' + string(mon[0])
    endif else begin
      mon = string(mon[0])
    endelse
    year = fix(strmid(atime(0), result + 7, 4)) ;get year
    mydate = strtrim(string(year,mon,day),1)
    if not strcmp(mydate,secondary_cal_date,8) then message,'Secondary cal date incorrect.'
    for j=0,sinp-1 do begin       ; go through channels
        darkmsi[j,1]=mean(darksi[j,0:ctn-1,1])
    endfor
    free_lun,lunn
    ;
    filen=secondary_nadir_ir+'dark'+l
    nspc_files = file_search(filen+data, count = numfiles, /FOLD_CASE)
    if (numfiles ne 1) then message,'There should be exactly 1 file for the dark.'
    openr,lunn,nspc_files[0],/get_lun
    ctn=0L
    while not eof(lunn) do begin
        readu, lunn, specn
        if(specn.intime4 ne nir_intime_s) then message, "NIR int times don't match"
        darkir[*,ctn,1]=specn.nspecir[0:irnp-1]
        ctn = ctn + 1L
    endwhile
    for j=0,irnp-1 do begin       ; go through channels
        darkmir[j,1]=mean(darkir[j,0:ctn-1,1])
    endfor
    free_lun,lunn
endif
;
if(strcmp(dozenith,'yes')) then begin
filez=secondary_zenith_si+'dark'+l
zspc_files = file_search(filez+data, count = numfiles, /FOLD_CASE)
if (numfiles ne 1) then message,'There should be exaclt 1 file for the dark.'
openr,lunz,zspc_files[0],/get_lun
ctz=0L
specz = spects
while not eof(lunz) do begin
    readu, lunz, specz
    if(specz.intime1 ne zsi_intime_s) then message, "ZSI int times don't match"
    darksi[*,ctz,0]=specz.zspecsi
    ctz = ctz + 1L
endwhile
for j=0,sinp-1 do begin ; go through channels
    darkmsi[j,0]=mean(darksi[j,0:ctz-1,0])
endfor
free_lun,lunz
;
filez=secondary_zenith_ir+'dark'+l
zspc_files = file_search(filez+data, count = numfiles, /FOLD_CASE)
if (numfiles ne 1) then message,'There should be exaclt 1 file for the dark.'
openr,lunz,zspc_files[0],/get_lun
ctz=0L
specz = spects
while not eof(lunz) do begin
    readu, lunz, specz
    if(specz.intime2 ne zir_intime_s) then message, "ZIR int times don't match"
    darkir[*,ctz,0]=specz.zspecir[0:irnp-1]
    ctz = ctz + 1L
endwhile
for j=0,irnp-1 do begin ; go through channels
    darkmir[j,0]=mean(darkir[j,0:ctz-1,0])
endfor
free_lun,lunz
endif


; (7) secondary calibration - cal
specn = spects
if(strcmp(donadir,'yes')) then begin
    filen=secondary_nadir_si+'cal'+l
    nspc_files = file_search(filen+data, count = numfiles, /FOLD_CASE)
    if (numfiles ne 1) then message,'There should be exactly 1 file for the dark.'
    openr,lunn,nspc_files[0],/get_lun
    ctn=0L
    while not eof(lunn) do begin
        readu, lunn, specn
        if(specn.intime3 ne nsi_intime_s) then message, "NSI int times don't match"
        calisi[*,ctn,1]=specn.nspecsi
        ctn = ctn + 1L
    endwhile
    for j=0,sinp-1 do begin       ; go through channels
        calimsi[j,1]=mean(calisi[j,0:ctn-1,1])
    endfor
    free_lun,lunn
    ;
    filen=secondary_nadir_ir+'cal'+l
    nspc_files = file_search(filen+data, count = numfiles, /FOLD_CASE)
    if (numfiles ne 1) then message,'There should be exactly 1 file for the dark.'
    openr,lunn,nspc_files[0],/get_lun
    ctn=0L
    while not eof(lunn) do begin
        readu, lunn, specn
        if(specn.intime4 ne nir_intime_s) then message, "NIR int times don't match"
        caliir[*,ctn,1]=specn.nspecir[0:irnp-1]
        ctn = ctn + 1L
    endwhile
    for j=0,irnp-1 do begin       ; go through channels
        calimir[j,1]=mean(caliir[j,0:ctn-1,1])
    endfor
    free_lun,lunn
endif
;
if(strcmp(dozenith,'yes')) then begin
filez=secondary_zenith_si+'cal'+l
zspc_files = file_search(filez+data, count = numfiles, /FOLD_CASE)
if (numfiles ne 1) then message,'There should be exaclt 1 file for the dark.'
openr,lunz,zspc_files[0],/get_lun
ctz=0L
specz = spects
while not eof(lunz) do begin
    readu, lunz, specz
    if(specz.intime1 ne zsi_intime_s) then message, "ZSI int times don't match"
    calisi[*,ctz,0]=specz.zspecsi
    ctz = ctz + 1L
endwhile
for j=0,sinp-1 do begin ; go through channels
    calimsi[j,0]=mean(calisi[j,0:ctz-1,0])
endfor
free_lun,lunz
;
filez=secondary_zenith_ir+'cal'+l
zspc_files = file_search(filez+data, count = numfiles, /FOLD_CASE)
if (numfiles ne 1) then message,'There should be exaclt 1 file for the dark.'
openr,lunz,zspc_files[0],/get_lun
ctz=0L
specz = spects
while not eof(lunz) do begin
    readu, lunz, specz
    if(specz.intime2 ne zir_intime_s) then message, "ZIR int times don't match"
    caliir[*,ctz,0]=specz.zspecir[0:irnp-1]
    ctz = ctz + 1L
endwhile
for j=0,irnp-1 do begin ; go through channels
    calimir[j,0]=mean(caliir[j,0:ctz-1,0])
endfor
free_lun,lunz
endif

dn2si = calimsi - darkmsi
dn2ir = calimir - darkmir


;    dn2ir[*,0] = reverse(dn2ir[*,0])
;    dn2ir[*,1] = reverse(dn2ir[*,1])


if(strcmp(dozenith,'yes')) then begin
    resp2si[*,0] = dn2si[*,0]/zsi_intime_s/(lampzensi)*transfersi[*,0]
    resp2ir[*,0] = dn2ir[*,0]/zir_intime_s/(lampzenir)*transferir[*,0]

        resp1_si_file = resp_func_dir + primary_cal_date + '_'+irradiance_primary_lamp+'_resp1_'+strcompress(string(zsi_intime_p),/REMOVE_ALL)+'_zensi.dat'
        resp1_ir_file = resp_func_dir + primary_cal_date + '_'+irradiance_primary_lamp+'_resp1_'+strcompress(string(zir_intime_p),/REMOVE_ALL)+'_zenir.dat'
        tmp=file_search(resp1_si_file,count=count)
        ;;if(count gt 0) then message, resp1_si_file + ' already exists'
        tmp=file_search(resp1_ir_file,count=count)
        ;;if(count gt 0) then message, resp1_ir_file + ' already exists'

        openw,10,resp1_si_file
        openw,11,resp1_ir_file
        for i=0,sinp-1 do begin
            printf,10,wlvisz[i],resp1si[i,0]
        endfor
         for i=0,irnp-1 do begin
            printf,11,wlnirz[i],resp1ir[i,0]
        endfor
        close,10
        close,11
endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; RESPONSE FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

if(strcmp(donadir,'yes')) then begin
    resp2si[*,1] = dn2si[*,1]/nsi_intime_s/(lampnadsi)*transfersi[*,1]
    resp2ir[*,1] = dn2ir[*,1]/nir_intime_s/(lampnadir)*transferir[*,1]
    if(strcmp(write_primary,'yes')) then begin

        resp1_si_file = resp_func_dir + primary_cal_date + '_'+radiance_primary_lamp+'_resp1_'+strcompress(string(nsi_intime_p),/REMOVE_ALL)+'_nadsi.dat'
        resp1_ir_file = resp_func_dir + primary_cal_date + '_'+radiance_primary_lamp+'_resp1_'+strcompress(string(nir_intime_p),/REMOVE_ALL)+'_nadir.dat'

        tmp=file_search(resp1_si_file,count=count)
        ;;if(count gt 0) then message, resp1_si_file + ' already exists'
        tmp=file_search(resp1_ir_file,count=count)
        ;;if(count gt 0) then message, resp1_ir_file + ' already exists'
        openw,12,resp1_si_file
        openw,13,resp1_ir_file
        for i=0,sinp-1 do begin
            printf,12,wlvisn[i],resp1si[i,1]
        endfor
        for i=0,irnp-1 do begin
            printf,13,wlnirn[i],resp1ir[i,1]
        endfor
        close,12
        close,13
    endif
endif


if(strcmp(dozenith,'yes')) then begin

    resp2_si_file = resp_func_dir + secondary_cal_date + '_'+irradiance_lamp+'_resp2_'+strcompress(string(zsi_intime_s),/REMOVE_ALL)+'_zensi.dat'
    resp2_ir_file = resp_func_dir + secondary_cal_date + '_'+irradiance_lamp+'_resp2_'+strcompress(string(zir_intime_s),/REMOVE_ALL)+'_zenir.dat'

    tmp=file_search(resp2_si_file,count=count)
    ;;if(count gt 0) then message, resp2_si_file + ' already exists'
    tmp=file_search(resp2_ir_file,count=count)
    ;;if(count gt 0) then message, resp2_ir_file + ' already exists'

    openw,10,resp2_si_file
    openw,11,resp2_ir_file
    for i=0,sinp-1 do begin
        printf,10,wlvisz[i],resp2si[i,0]
    endfor
    for i=0,irnp-1 do begin
        printf,11,wlnirz[i],resp2ir[i,0]
    endfor
    close,10
    close,11
endif


if(strcmp(donadir,'yes')) then begin

    resp2_si_file = resp_func_dir + secondary_cal_date + '_'+radiance_sphere+'_resp2_'+strcompress(string(nsi_intime_s),/REMOVE_ALL)+'_nadsi.dat'
    resp2_ir_file = resp_func_dir + secondary_cal_date + '_'+radiance_sphere+'_resp2_'+strcompress(string(nir_intime_s),/REMOVE_ALL)+'_nadir.dat'
    print,resp2_ir_file
    tmp=file_search(resp2_si_file,count=count)
    ;;if(count gt 0) then message, resp2_si_file + ' already exists'
    tmp=file_search(resp2_ir_file + ' already exists')
    ;;if(count gt 0) then message, resp2_ir_file + ' already exists'

    openw,12,resp2_si_file
    openw,13,resp2_ir_file
    for i=0,sinp-1 do begin
        printf,12,wlvisn[i],resp2si[i,1]
    endfor
    for i=0,irnp-1 do begin
        printf,13,wlnirn[i],resp2ir[i,1]
    endfor
    close,12
    close,13
endif
loadct, 39
!p.font=1 & !p.thick=1.5 & !p.charsize=2.2
!x.style=1 & !y.style=1 & !z.style=1
!y.thick=2 & !x.thick=2
  !p.background=255
  !p.color=0
if(strcmp(dozenith,'yes')) then begin
    window,0,tit='Irradiance'
    ymax = max([max(resp1si(*,0)),max(resp1ir(*,0)),max(resp2si(*,0)),max(resp2ir(*,0))])*1.04
    plot, wlvisz,resp1si(*,0),color=3,ytitle='counts/ms W!E-1!N m!E2!N nm',xtitle='Wavelength (nm)',title='Irradiance'+' p: '+irradiance_primary_lamp+' s: '+irradiance_lamp,/xs,/ys,xrange=[300,2200],yrange=[0,ymax]
    oplot,wlnirz,resp1ir(*,0),thick=1.8,color=3
    oplot,wlvisz,resp2si(*,0),thick=1.8,color=250
    oplot,wlnirz,resp2ir(*,0),thick=1.8,color=250
    legend,['Primary Cal - '+  primary_cal_date  +' - {'+strcompress(string(zsi_intime_p),/REMOVE_ALL)+','+strcompress(string(zir_intime_p),/REMOVE_ALL)+'}',$
            'Secondary Cal - '+secondary_cal_date+' - {'+strcompress(string(zsi_intime_s),/REMOVE_ALL)+','+strcompress(string(zir_intime_s),/REMOVE_ALL)+'}'],textcolors=[3,250],/left,outline_color=3
    p=tvrd(true=1)
    write_png,path+l+'irradiance.png',p
endif

if(strcmp(donadir,'yes')) then begin
    window,1,tit='Radiance'
    ymax = max([max(resp1si(*,1)),max(resp1ir(*,1)),max(resp2si(*,1)),max(resp2ir(*,1))])*1.04
    plot, wlvisn,resp1si(*,1),color=3,ytitle='counts/ms W!E-1!N m!E2!N nm',xtitle='Wavelength (nm)',title='Radiance'+' p: '+radiance_primary_lamp+' s: '+radiance_sphere,/xs,/ys,xrange=[300,2200],yrange=[0,ymax]
    oplot,wlnirn,resp1ir(*,1),thick=1.8,color=3
    oplot,wlvisn,resp2si(*,1),thick=1.8,color=250
    oplot,wlnirn,resp2ir(*,1),thick=1.8,color=250
    ;legend,['Primary Cal - '+primary_cal_date,'Secondary Cal - '+secondary_cal_date],textcolors=[3,110],/right,outline_color=3
    legend,['Primary Cal - '+  primary_cal_date  +' - {'+strcompress(string(nsi_intime_p),/REMOVE_ALL)+','+strcompress(string(nir_intime_p),/REMOVE_ALL)+'}',$
            'Secondary Cal - '+secondary_cal_date+' - {'+strcompress(string(nsi_intime_s),/REMOVE_ALL)+','+strcompress(string(nir_intime_s),/REMOVE_ALL)+'}'],textcolors=[3,250],/left,outline_color=3
    p=tvrd(true=1)
    write_png,path+l+'Radiance.png',p
endif
stop
end

function fplanck,temp,wav
; computes Planck function in erg / cm2 sec [delta lambda=1 micron] ster
; input: temp = temperature (K)
; wav = wavelength in micron

; physics constants in cgs (all cm)
k=1.380658D-16        ; Boltzmann constant (erg K; double precision)
h=6.626076D-27        ; Planck constant (erg s)
c=2.997929D10          ; velocity of light (cm/s)

wavcm=wav*1E-4        ; change wav into cm
blambda = 2*h*c^2/(wavcm^5*(exp(h*c/(wavcm*k*temp))-1))
blambda=blambda*1E-4  ; change B_lambda into per micron

return,blambda
end
