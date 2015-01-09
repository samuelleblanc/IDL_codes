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

;device,decomposed=0
loadct,39
  tvlct, r,g,bleu, /get
  r=reverse(r) & g=reverse(g) & bleu=reverse(bleu)
  tvlct,r,g,bleu
!P.multi=0
;!p.font=1 & !p.thick=2 & !p.charsize=1.5
;!x.style=1 & !y.style=1 & !z.style=1
;!y.thick=1 & !x.thick=1
;!p.color=255
;!p.background=0

if file_test(cfg_file) ne 1 then message,'Did not find configuration file.'

months = ["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"]

sinp                    = fix(cfg(cfg_file,'sinp'  ))    ; number of channels for each spectrum in Si
irnp                    = fix(cfg(cfg_file,'irnp'))      ; number of channels used in the IR

platform              = cfg(cfg_file,'platform')
data                  = cfg(cfg_file,'data')    ; data file extension

;for irradiance (zenith)
irradiance_nist_traceable_source = cfg(cfg_file,'irradiance_nist_traceable_source')
irradiance_primary_lamp          = strsplit(cfg(cfg_file,'irradiance_primary_lamp') ,', ',escape='#',/extract)
irradiance_lamp                  = strsplit(cfg(cfg_file,'irradiance_lamp') ,', ',escape='#',/extract)

;for radiance (nadir)
radiance_nist_traceable_source = cfg(cfg_file,'radiance_nist_traceable_source')
radiance_primary_lamp          = strsplit(cfg(cfg_file,'radiance_primary_lamp') ,', ',escape='#',/extract)
radiance_sphere                = strsplit(cfg(cfg_file,'radiance_sphere') ,', ',escape='#',/extract)

primary_cal_date      = strsplit(cfg(cfg_file,'primary_cal_date') ,', ',escape='#',/extract)
secondary_cal_date    = strsplit(cfg(cfg_file,'secondary_cal_date') ,', ',escape='#',/extract)
ref = cfg(cfg_file,'reference_date')

num_secondary = n_elements(secondary_cal_date)
if num_secondary gt 1 then print, 'Multiple secondary Calibration dates, Plotting all for comparison, Last cal date will be used for response function'

resp_func_dir = cfg(cfg_file,'resp_func_dir')

dozenith = cfg(cfg_file,'dozenith')
donadir  = cfg(cfg_file,'donadir')
write_primary  = cfg(cfg_file,'write_primary')

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


if(strcmp(donadir,'yes')) then begin
  if n_elements(transfer_nadir_si) ne n_elements(secondary_nadir_si) then message, 'not the same number of transfer path and secondary cal paths - nadir'
  if n_elements(primary_nadir_si) ne n_elements(secondary_nadir_si) then message, 'not the same number of primary path and secondary cal paths - nadir'
  
  nsi_intime_p=intarr(num_secondary)
  nir_intime_p=intarr(num_secondary)
  
  for ii=0, num_secondary-1 do begin
    nsi_intime_p[ii] = fix(primary_nadir_si[2*ii+1])   & primary_nadir_si[ii]  = primary_nadir_si[2*ii]
    nir_intime_p[ii] = fix(primary_nadir_ir[2*ii+1])   & primary_nadir_ir[ii]  = primary_nadir_ir[2*ii]
  endfor
  
  primary_nadir_si=primary_nadir_si[0:num_secondary-1]
  primary_nadir_ir=primary_nadir_ir[0:num_secondary-1]
  
  nsi_intime_t=intarr(num_secondary)
  nir_intime_t=intarr(num_secondary)
  
  for ii=0, num_secondary-1 do begin
    nsi_intime_t[ii] = fix(transfer_nadir_si[2*ii+1])  & transfer_nadir_si[ii] = transfer_nadir_si[2*ii]
    nir_intime_t[ii] = fix(transfer_nadir_ir[2*ii+1])  & transfer_nadir_ir[ii] = transfer_nadir_ir[2*ii]
  endfor
  
  transfer_nadir_si=transfer_nadir_si[0:num_secondary-1]
  transfer_nadir_ir=transfer_nadir_ir[0:num_secondary-1]
  
  nsi_intime_s=intarr(num_secondary)
  nir_intime_s=intarr(num_secondary)
  
  for ii=0, num_secondary-1 do begin
    nsi_intime_s[ii] = fix(secondary_nadir_si[2*ii+1])  & secondary_nadir_si[ii] = secondary_nadir_si[2*ii]
    nir_intime_s[ii] = fix(secondary_nadir_ir[2*ii+1])  & secondary_nadir_ir[ii] = secondary_nadir_ir[2*ii]
  endfor
  
  secondary_nadir_si=secondary_nadir_si[0:num_secondary-1]
  secondary_nadir_ir=secondary_nadir_ir[0:num_secondary-1]
  
  ref=where(ref eq secondary_cal_date)	;find where the secondary cal date is equal to the reference date
  ref=ref[0]
  
  for ii=0, num_secondary-1 do begin
  if nsi_intime_p[ii] ne nsi_intime_t[ii] then message,'Primary and Transfer integration times are not equal (NSI).'
  if nir_intime_p[ii] ne nir_intime_t[ii] then message,'Primary and Transfer integration times are not equal (NIR).'
  endfor
endif

if(strcmp(dozenith,'yes')) then begin
  if n_elements(transfer_zenith_si) ne n_elements(secondary_zenith_si) then message, 'not the same number of transfer path and secondary cal paths - zenith'
  if n_elements(primary_zenith_si) ne n_elements(secondary_zenith_si) then message, 'not the same number of primary path and secondary cal paths - zenith'
  
  zsi_intime_p=intarr(num_secondary)
  zir_intime_p=intarr(num_secondary)
  
  for ii=0, num_secondary-1 do begin
    zsi_intime_p[ii] = fix(primary_zenith_si[2*ii+1])  & primary_zenith_si[ii] = primary_zenith_si[2*ii]
    zir_intime_p[ii] = fix(primary_zenith_ir[2*ii+1])  & primary_zenith_ir[ii] = primary_zenith_ir[2*ii]
  endfor
  
  primary_zenith_si=primary_zenith_si[0:num_secondary-1]
  primary_zenith_ir=primary_zenith_ir[0:num_secondary-1]
  
  zsi_intime_t=intarr(num_secondary)
  zir_intime_t=intarr(num_secondary)
  
  for ii=0, num_secondary-1 do begin
    zsi_intime_t[ii] = fix(transfer_zenith_si[2*ii+1]) & transfer_zenith_si[ii]= transfer_zenith_si[2*ii]
    zir_intime_t[ii] = fix(transfer_zenith_ir[2*ii+1]) & transfer_zenith_ir[ii]= transfer_zenith_ir[2*ii]
  endfor
  
  transfer_zenith_si=transfer_zenith_si[0:num_secondary-1]
  transfer_zenith_ir=transfer_zenith_ir[0:num_secondary-1]
  
  zsi_intime_s=intarr(num_secondary)
  zir_intime_s=intarr(num_secondary)
  
  for ii=0, num_secondary-1 do begin
    zsi_intime_s[ii] = fix(secondary_zenith_si[2*ii+1]) & secondary_zenith_si[ii]= secondary_zenith_si[2*ii]
    zir_intime_s[ii] = fix(secondary_zenith_ir[2*ii+1]) & secondary_zenith_ir[ii]= secondary_zenith_ir[2*ii]
  endfor
  
  secondary_zenith_si=secondary_zenith_si[0:num_secondary-1]
  secondary_zenith_ir=secondary_zenith_ir[0:num_secondary-1]
  
  ref=where(ref eq secondary_cal_date)	;find where the secondary cal date is equal to the reference date
  ref=ref[0]
  
  for ii=0, num_secondary-1 do begin
  if zsi_intime_p[ii] ne zsi_intime_t[ii] then message,'Primary and Transfer integration times are not equal (ZSI).'
  if zir_intime_p[ii] ne zir_intime_t[ii] then message,'Primary and Transfer integration times are not equal (ZIR).'
  endfor

endif
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

resp1si_arr=fltarr(sinp,2,num_secondary)
resp1ir_arr=fltarr(irnp,2,num_secondary)
resp2si_arr=fltarr(sinp,2,num_secondary)
resp2ir_arr=fltarr(irnp,2,num_secondary)
transfersi_arr=fltarr(sinp,2,num_secondary)
transferir_arr=fltarr(irnp,2,num_secondary)

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

for gnarr=0, num_secondary-1 do begin
; (1) process primary calibration - darks
spects = {btime: lonarr(2), bcdtimstp:  bytarr(12),intime1: long(0),$        ; this is the reference spectra structure
                     intime2: long(0), intime3: long(0), intime4: long(0),accum: long(0), shsw:long(0),$
                     zsit: ulong(0),nsit: ulong(0),zirt: ulong(0),nirt: ulong(0), zirx: ulong(0), nirx: ulong(0),xt: ulong(0),it: ulong(0),$
                     zspecsi: intarr(sinp), zspecir: intarr(sinp), nspecsi: intarr(sinp), nspecir: intarr(sinp)} 

;SSFR 7 InGaAs saves in 256 channels, but only 128 are used and NOT reversed

specn = spects
                     
if(strcmp(donadir,'yes')) then begin
    filen=primary_nadir_si[gnarr]+'dark'+l
    nspc_files = file_search(filen+data, count = numfiles, /FOLD_CASE)
    if (numfiles ne 1) then message,'There should be exactly 1 file for the dark.'
    openr,lunn,nspc_files[0],/get_lun
    ctn=0L
    while not eof(lunn) do begin
        readu, lunn, specn
        if(specn.intime3 ne nsi_intime_p[gnarr]) then message, "NSI int times don't match"
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
    if not strcmp(primary_cal_date[gnarr],mydate,8) then message,'Primary cal date incorrect.'
    for j=0,sinp-1 do begin       ; go through channels
        darkmsi[j,1]=mean(darksi[j,0:ctn-1,1])
    endfor
    free_lun,lunn
    ;
    filen=primary_nadir_ir[gnarr]+'dark'+l
    nspc_files = file_search(filen+data, count = numfiles, /FOLD_CASE)
    if (numfiles ne 1) then message,'There should be exactly 1 file for the dark.'
    openr,lunn,nspc_files[0],/get_lun
    ctn=0L
    while not eof(lunn) do begin
        readu, lunn, specn
        if(specn.intime4 ne nir_intime_p[gnarr]) then message, "NIR int times don't match"
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
filez=primary_zenith_si[gnarr]+'dark'+l
zspc_files = file_search(filez+data, count = numfiles, /FOLD_CASE)
if (numfiles ne 1) then message,'There should be exaclt 1 file for the dark.'
openr,lunz,zspc_files[0],/get_lun
ctz=0L
specz = spects
while not eof(lunz) do begin
    readu, lunz, specz
    if(specz.intime1 ne zsi_intime_p[gnarr]) then message, "ZSI int times don't match"
    darksi[*,ctz,0]=specz.zspecsi
    ctz = ctz + 1L
endwhile
for j=0,sinp-1 do begin ; go through channels
    darkmsi[j,0]=mean(darksi[j,0:ctz-1,0])
endfor
free_lun,lunz
;
filez=primary_zenith_ir[gnarr]+'dark'+l
zspc_files = file_search(filez+data, count = numfiles, /FOLD_CASE)
if (numfiles ne 1) then message,'There should be exaclt 1 file for the dark.'
openr,lunz,zspc_files[0],/get_lun
ctz=0L
specz = spects
while not eof(lunz) do begin
    readu, lunz, specz
    if(specz.intime2 ne zir_intime_p[gnarr]) then message, "ZIR int times don't match"
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
    filen=primary_nadir_si[gnarr]+'cal'+l
    nspc_files = file_search(filen+data, count = numfiles, /FOLD_CASE)
    if (numfiles ne 1) then message,'There should be exactly 1 file for the dark.'
    openr,lunn,nspc_files[0],/get_lun
    ctn=0L
    while not eof(lunn) do begin
        readu, lunn, specn
        if(specn.intime3 ne nsi_intime_p[gnarr]) then message, "NSI int times don't match"
        calisi[*,ctn,1]=specn.nspecsi
        ctn = ctn + 1L
    endwhile
    for j=0,sinp-1 do begin       ; go through channels
        calimsi[j,1]=mean(calisi[j,0:ctn-1,1])
    endfor
    free_lun,lunn
    ;
    filen=primary_nadir_ir[gnarr]+'cal'+l
    nspc_files = file_search(filen+data, count = numfiles, /FOLD_CASE)
    if (numfiles ne 1) then message,'There should be exactly 1 file for the dark.'
    openr,lunn,nspc_files[0],/get_lun
    ctn=0L
    while not eof(lunn) do begin
        readu, lunn, specn
        if(specn.intime4 ne nir_intime_p[gnarr]) then message, "NIR int times don't match"
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
filez=primary_zenith_si[gnarr]+'cal'+l
zspc_files = file_search(filez+data, count = numfiles, /FOLD_CASE)
if (numfiles ne 1) then message,'There should be exaclt 1 file for the dark.'
openr,lunz,zspc_files[0],/get_lun
ctz=0L
specz = spects
while not eof(lunz) do begin
    readu, lunz, specz
    if(specz.intime1 ne zsi_intime_p[gnarr]) then message, "ZSI int times don't match"
    calisi[*,ctz,0]=specz.zspecsi
    ctz = ctz + 1L
endwhile
for j=0,sinp-1 do begin ; go through channels
    calimsi[j,0]=mean(calisi[j,0:ctz-1,0])
endfor
free_lun,lunz
;
filez=primary_zenith_ir[gnarr]+'cal'+l
zspc_files = file_search(filez+data, count = numfiles, /FOLD_CASE)
if (numfiles ne 1) then message,'There should be exaclt 1 file for the dark.'
openr,lunz,zspc_files[0],/get_lun
ctz=0L
specz = spects
while not eof(lunz) do begin
    readu, lunz, specz
    if(specz.intime2 ne zir_intime_p[gnarr]) then message, "ZIR int times don't match"
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
if(strcmp(dozenith,'yes')) then begin
resp1si[*,0] = dn_1si[*,0]/float(zsi_intime_p[gnarr])/(lampzensi)
resp1ir[*,0] = dn_1ir[*,0]/float(zir_intime_p[gnarr])/(lampzenir)
endif
if(strcmp(donadir,'yes')) then begin
resp1si[*,1] = dn_1si[*,1]/float(nsi_intime_p[gnarr])/(lampnadsi)
resp1ir[*,1] = dn_1ir[*,1]/float(nir_intime_p[gnarr])/(lampnadir)
endif
resp1ir_arr[*,*,gnarr]=resp1ir
resp1si_arr[*,*,gnarr]=resp1si

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TRANSFER CALIBRATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; (4) transfer of calibration - darks
specn = spects
if(strcmp(donadir,'yes')) then begin
    filen=transfer_nadir_si[gnarr]+'dark'+l
    nspc_files = file_search(filen+data, count = numfiles, /FOLD_CASE)
    if (numfiles ne 1) then message,'There should be exactly 1 file for the dark.'
    openr,lunn,nspc_files[0],/get_lun
    ctn=0L
    while not eof(lunn) do begin
        readu, lunn, specn
        if(specn.intime3 ne nsi_intime_t[gnarr]) then message, "NSI int times don't match"
        darksi[*,ctn,1]=specn.nspecsi
        ctn = ctn + 1L
    endwhile
    for j=0,sinp-1 do begin       ; go through channels
        darkmsi[j,1]=mean(darksi[j,0:ctn-1,1])
    endfor
    free_lun,lunn
    ;
    filen=transfer_nadir_ir[gnarr]+'dark'+l
    nspc_files = file_search(filen+data, count = numfiles, /FOLD_CASE)
    if (numfiles ne 1) then message,'There should be exactly 1 file for the dark.'
    openr,lunn,nspc_files[0],/get_lun
    ctn=0L
    while not eof(lunn) do begin
        readu, lunn, specn
        if(specn.intime4 ne nir_intime_t[gnarr]) then message, "NIR int times don't match"
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
filez=transfer_zenith_si[gnarr]+'dark'+l
zspc_files = file_search(filez+data, count = numfiles, /FOLD_CASE)
if (numfiles ne 1) then message,'There should be exaclt 1 file for the dark.'
openr,lunz,zspc_files[0],/get_lun
ctz=0L
specz = spects
while not eof(lunz) do begin
    readu, lunz, specz
    if(specz.intime1 ne zsi_intime_t[gnarr]) then message, "ZSI int times don't match"
    darksi[*,ctz,0]=specz.zspecsi
    ctz = ctz + 1L
endwhile
for j=0,sinp-1 do begin ; go through channels
    darkmsi[j,0]=mean(darksi[j,0:ctz-1,0])
endfor
free_lun,lunz
;
filez=transfer_zenith_ir[gnarr]+'dark'+l
zspc_files = file_search(filez+data, count = numfiles, /FOLD_CASE)
if (numfiles ne 1) then message,'There should be exaclt 1 file for the dark.'
openr,lunz,zspc_files[0],/get_lun
ctz=0L
specz = spects
while not eof(lunz) do begin
    readu, lunz, specz
    if(specz.intime2 ne zir_intime_t[gnarr]) then message, "ZIR int times don't match"
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
    filen=transfer_nadir_si[gnarr]+'cal'+l
    nspc_files = file_search(filen+data, count = numfiles, /FOLD_CASE)
    if (numfiles ne 1) then message,'There should be exactly 1 file for the dark.'
    openr,lunn,nspc_files[0],/get_lun
    ctn=0L
    while not eof(lunn) do begin
        readu, lunn, specn
        if(specn.intime3 ne nsi_intime_t[gnarr]) then message, "NSI int times don't match"
        calisi[*,ctn,1]=specn.nspecsi
        ctn = ctn + 1L
    endwhile
    for j=0,sinp-1 do begin       ; go through channels
        calimsi[j,1]=mean(calisi[j,0:ctn-1,1])
    endfor
    free_lun,lunn
    ;
    filen=transfer_nadir_ir[gnarr]+'cal'+l
    nspc_files = file_search(filen+data, count = numfiles, /FOLD_CASE)
    if (numfiles ne 1) then message,'There should be exactly 1 file for the dark.'
    openr,lunn,nspc_files[0],/get_lun
    ctn=0L
    while not eof(lunn) do begin
        readu, lunn, specn
        if(specn.intime4 ne nir_intime_t[gnarr]) then message, "NIR int times don't match"
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
filez=transfer_zenith_si[gnarr]+'cal'+l
zspc_files = file_search(filez+data, count = numfiles, /FOLD_CASE)
if (numfiles ne 1) then message,'There should be exaclt 1 file for the dark.'
openr,lunz,zspc_files[0],/get_lun
ctz=0L
specz = spects
while not eof(lunz) do begin
    readu, lunz, specz
    if(specz.intime1 ne zsi_intime_t[gnarr]) then message, "ZSI int times don't match"
    calisi[*,ctz,0]=specz.zspecsi
    ctz = ctz + 1L
endwhile
for j=0,sinp-1 do begin ; go through channels
    calimsi[j,0]=mean(calisi[j,0:ctz-1,0])
endfor
free_lun,lunz
;
filez=transfer_zenith_ir[gnarr]+'cal'+l
zspc_files = file_search(filez+data, count = numfiles, /FOLD_CASE)
if (numfiles ne 1) then message,'There should be exaclt 1 file for the dark.'
openr,lunz,zspc_files[0],/get_lun
ctz=0L
specz = spects
while not eof(lunz) do begin
    readu, lunz, specz
    if(specz.intime2 ne zir_intime_t[gnarr]) then message, "ZIR int times don't match"
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

transferir_arr[*,*,gnarr]=transferir
transfersi_arr[*,*,gnarr]=transfersi

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SECONDARY CALIBRATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; (6) secondary calibration - darks
specn = spects
if(strcmp(donadir,'yes')) then begin
    filen=secondary_nadir_si[gnarr]+'dark'+l
    nspc_files = file_search(filen+data, count = numfiles, /FOLD_CASE)
    if (numfiles ne 1) then message,'There should be exactly 1 file for the dark.'
    openr,lunn,nspc_files[0],/get_lun
    ctn=0L
    while not eof(lunn) do begin
        readu, lunn, specn
        if(specn.intime3 ne nsi_intime_s[gnarr]) then message, "NSI int times don't match"
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
    if not strcmp(mydate,secondary_cal_date[gnarr],8) then message,'Secondary cal date incorrect.'
    for j=0,sinp-1 do begin       ; go through channels
        darkmsi[j,1]=mean(darksi[j,0:ctn-1,1])
    endfor
    free_lun,lunn
    ;
    filen=secondary_nadir_ir[gnarr]+'dark'+l
    nspc_files = file_search(filen+data, count = numfiles, /FOLD_CASE)
    if (numfiles ne 1) then message,'There should be exactly 1 file for the dark.'
    openr,lunn,nspc_files[0],/get_lun
    ctn=0L
    while not eof(lunn) do begin
        readu, lunn, specn
        if(specn.intime4 ne nir_intime_s[gnarr]) then message, "NIR int times don't match"
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
filez=secondary_zenith_si[gnarr]+'dark'+l
zspc_files = file_search(filez+data, count = numfiles, /FOLD_CASE)
if (numfiles ne 1) then message,'There should be exaclt 1 file for the dark.'
openr,lunz,zspc_files[0],/get_lun
ctz=0L
specz = spects
while not eof(lunz) do begin
    readu, lunz, specz
    if(specz.intime1 ne zsi_intime_s[gnarr]) then message, "ZSI int times don't match"
    darksi[*,ctz,0]=specz.zspecsi
    ctz = ctz + 1L
endwhile
for j=0,sinp-1 do begin ; go through channels
    darkmsi[j,0]=mean(darksi[j,0:ctz-1,0])
endfor
free_lun,lunz
;
filez=secondary_zenith_ir[gnarr]+'dark'+l
zspc_files = file_search(filez+data, count = numfiles, /FOLD_CASE)
if (numfiles ne 1) then message,'There should be exaclt 1 file for the dark.'
openr,lunz,zspc_files[0],/get_lun
ctz=0L
specz = spects
while not eof(lunz) do begin
    readu, lunz, specz
    if(specz.intime2 ne zir_intime_s[gnarr]) then message, "ZIR int times don't match"
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
    filen=secondary_nadir_si[gnarr]+'cal'+l
    nspc_files = file_search(filen+data, count = numfiles, /FOLD_CASE)
    if (numfiles ne 1) then message,'There should be exactly 1 file for the dark.'
    openr,lunn,nspc_files[0],/get_lun
    ctn=0L
    while not eof(lunn) do begin
        readu, lunn, specn
        if(specn.intime3 ne nsi_intime_s[gnarr]) then message, "NSI int times don't match"
        calisi[*,ctn,1]=specn.nspecsi
        ctn = ctn + 1L
    endwhile
    for j=0,sinp-1 do begin       ; go through channels
        calimsi[j,1]=mean(calisi[j,0:ctn-1,1])
    endfor
    free_lun,lunn
    ;
    filen=secondary_nadir_ir[gnarr]+'cal'+l
    nspc_files = file_search(filen+data, count = numfiles, /FOLD_CASE)
    if (numfiles ne 1) then message,'There should be exactly 1 file for the dark.'
    openr,lunn,nspc_files[0],/get_lun
    ctn=0L
    while not eof(lunn) do begin
        readu, lunn, specn
        if(specn.intime4 ne nir_intime_s[gnarr]) then message, "NIR int times don't match"
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
filez=secondary_zenith_si[gnarr]+'cal'+l
zspc_files = file_search(filez+data, count = numfiles, /FOLD_CASE)
if (numfiles ne 1) then message,'There should be exaclt 1 file for the dark.'
openr,lunz,zspc_files[0],/get_lun
ctz=0L
specz = spects
while not eof(lunz) do begin
    readu, lunz, specz
    if(specz.intime1 ne zsi_intime_s[gnarr]) then message, "ZSI int times don't match"
    calisi[*,ctz,0]=specz.zspecsi
    ctz = ctz + 1L
endwhile
for j=0,sinp-1 do begin ; go through channels
    calimsi[j,0]=mean(calisi[j,0:ctz-1,0])
endfor
free_lun,lunz
;
filez=secondary_zenith_ir[gnarr]+'cal'+l
zspc_files = file_search(filez+data, count = numfiles, /FOLD_CASE)
if (numfiles ne 1) then message,'There should be exaclt 1 file for the dark.'
openr,lunz,zspc_files[0],/get_lun
ctz=0L
specz = spects
while not eof(lunz) do begin
    readu, lunz, specz
    if(specz.intime2 ne zir_intime_s[gnarr]) then message, "ZIR int times don't match"
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
  resp2si[*,0] = dn2si[*,0]/zsi_intime_s[gnarr]/(lampzensi)*transfersi[*,0]
	resp2ir[*,0] = dn2ir[*,0]/zir_intime_s[gnarr]/(lampzenir)*transferir[*,0]
	
	resp2si_arr[*,0,gnarr]=resp2si[*,0]
	resp2ir_arr[*,0,gnarr]=resp2ir[*,0]
endif

if(strcmp(donadir,'yes')) then begin
    resp2si[*,1] = dn2si[*,1]/nsi_intime_s[gnarr]/(lampzensi)*transfersi[*,1]
    resp2ir[*,1] = dn2ir[*,1]/nir_intime_s[gnarr]/(lampzenir)*transferir[*,1]
	
	resp2si_arr[*,1,gnarr]=resp2si[*,1]
	resp2ir_arr[*,1,gnarr]=resp2ir[*,1]
endif
	
endfor ; end of for loop that goes through all the transfer and secondary calibrations

if(strcmp(dozenith,'yes')) then begin
  ;  resp2si[*,0] = dn2si[*,0]/zsi_intime_s/(lampzensi)*transfersi[*,0]
  ;  resp2ir[*,0] = dn2ir[*,0]/zir_intime_s/(lampzenir)*transferir[*,0]

        resp1_si_file = resp_func_dir + primary_cal_date[num_secondary-1] + '_'+irradiance_primary_lamp[num_secondary-1]+'_resp1_'+strcompress(string(zsi_intime_p[num_secondary-1]),/REMOVE_ALL)+'_zensi.dat'
        resp1_ir_file = resp_func_dir + primary_cal_date[num_secondary-1] + '_'+irradiance_primary_lamp[num_secondary-1]+'_resp1_'+strcompress(string(zir_intime_p[num_secondary-1]),/REMOVE_ALL)+'_zenir.dat'
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
;    resp2si[*,1] = dn2si[*,1]/nsi_intime_s/(lampnadsi)*transfersi[*,1]
;    resp2ir[*,1] = dn2ir[*,1]/nir_intime_s/(lampnadir)*transferir[*,1]
    if(strcmp(write_primary,'yes')) then begin

        resp1_si_file = resp_func_dir + primary_cal_date[num_secondary-1] + '_'+radiance_primary_lamp[num_secondary-1]+'_resp1_'+strcompress(string(nsi_intime_p[num_secondary-1]),/REMOVE_ALL)+'_nadsi.dat'
        resp1_ir_file = resp_func_dir + primary_cal_date[num_secondary-1] + '_'+radiance_primary_lamp[num_secondary-1]+'_resp1_'+strcompress(string(nir_intime_p[num_secondary-1]),/REMOVE_ALL)+'_nadir.dat'

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

    resp2_si_file = resp_func_dir + secondary_cal_date[num_secondary-1] + '_'+irradiance_lamp[num_secondary-1]+'_resp2_'+strcompress(string(zsi_intime_s[num_secondary-1]),/REMOVE_ALL)+'_zensi.dat'
    resp2_ir_file = resp_func_dir + secondary_cal_date[num_secondary-1] + '_'+irradiance_lamp[num_secondary-1]+'_resp2_'+strcompress(string(zir_intime_s[num_secondary-1]),/REMOVE_ALL)+'_zenir.dat'

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

    resp2_si_file = resp_func_dir + secondary_cal_date[num_secondary-1] + '_'+radiance_sphere[num_secondary-1]+'_resp2_'+strcompress(string(nsi_intime_s[num_secondary-1]),/REMOVE_ALL)+'_nadsi.dat'
    resp2_ir_file = resp_func_dir + secondary_cal_date[num_secondary-1] + '_'+radiance_sphere[num_secondary-1]+'_resp2_'+strcompress(string(nir_intime_s[num_secondary-1]),/REMOVE_ALL)+'_nadir.dat'
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

if(strcmp(dozenith,'yes')) then begin
    ;window,0,tit='Irradiance',xsize=500,ysize=1000
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated
 device, /tt_font, set_font='Helvetica Bold'
 device, filename=path+l+'irr.ps'
 device,/color,bits_per_pixel=8.
 device, xsize=20, ysize=40
      !p.font=1 & !p.thick=5 & !p.charsize=1.8
      !x.style=0 & !y.style=1 & !z.style=1 &!y.thick=1.8
      !x.thick=1.8

	!p.multi=[0,1,2]
    ymax = max([max(resp1si_arr(*,0,*)),max(resp1ir_arr(*,0,*)),max(resp2si_arr(*,0,*)),max(resp2ir_arr(*,0,*))])*1.04
    plot, wlvisz,resp1si(*,0),color=0,ytitle='counts/ms W!E-1!N m!E2!N nm',xtitle='Wavelength (nm)',title='Irradiance'+' p: '+irradiance_primary_lamp[num_secondary-1]+' s: '+irradiance_lamp[num_secondary-1],/xs,/ys,xrange=[300,1750],yrange=[0,ymax],linestyle=2
    oplot,wlnirz,resp1ir(*,0),thick=1.8,color=0, linestyle=2
	
	legend_tit=['Primary Cal - '+  primary_cal_date[num_secondary-1]  +' - {'+strcompress(string(zsi_intime_p[num_secondary-1]),/REMOVE_ALL)+','+strcompress(string(zir_intime_p[num_secondary-1]),/REMOVE_ALL)+'}']
	color_s=findgen(num_secondary)*230/num_secondary+25
	
    for i=0,num_secondary-1 do begin
      oplot,wlvisz,resp1si_arr(*,0,i),thick=1.8,color=color_s[i], linestyle=2
      oplot,wlnirz,resp1ir_arr(*,0,i),thick=1.8,color=color_s[i], linestyle=2
      legend_tit=[legend_tit,'Primary Cal - '+primary_cal_date[i]+' - {'+strcompress(string(zsi_intime_p[i]),/REMOVE_ALL)+','+strcompress(string(zir_intime_p[i]),/REMOVE_ALL)+'}']
    endfor
	
	for i=0,num_secondary-1 do begin
      oplot,wlvisz,resp2si_arr(*,0,i),thick=1.8,color=color_s[i]
      oplot,wlnirz,resp2ir_arr(*,0,i),thick=1.8,color=color_s[i]
	  legend_tit=[legend_tit,'Secondary Cal - '+secondary_cal_date[i]+' - {'+strcompress(string(zsi_intime_s[i]),/REMOVE_ALL)+','+strcompress(string(zir_intime_s[i]),/REMOVE_ALL)+'}']
	endfor
    legend,legend_tit[1:*],textcolors=[color_s,color_s],/right,outline_color=255 ;'
	
	plot, wlvisz,resp2si_arr(*,0,ref),color=0,ytitle='%',xtitle='Wavelength (nm)',title='Percent Difference of response function !C Reference:'+ secondary_cal_date[ref]+' - Irradiance',thick=1.8,/xs,/ys,xrange=[300,1750],yrange=[-15,15], /nodata
    leg_tit='0'
    for i=0, num_secondary-1 do begin
      oplot,wlvisz,(resp2si_arr(*,0,i)-resp2si_arr(*,0,ref))*100./resp2si_arr(*,0,ref),thick=1.8,color=color_s[i]
      oplot,wlnirz,(resp2ir_arr(*,0,i)-resp2ir_arr(*,0,ref))*100./resp2ir_arr(*,0,ref),thick=1.8,color=color_s[i]
      leg_tit=[leg_tit, primary_cal_date[i]+' : '+secondary_cal_date[i]]
    endfor
    legend,['Primary : Secondary',leg_tit[1:*]],textcolors=[0,color_s],/right, outline_color=255
	
   ; p=tvrd(true=1)
   ; write_png,path+l+'irradiance.png',p
device,/close
spawn, 'convert '+path+l+'irr.ps '+path+l+'irr.png'
spawn, 'rm '+path+l+'irr.ps'

endif

if(strcmp(donadir,'yes')) then begin
  ;  window,1,tit='Radiance', xsize=500, ysize=1000
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated
 device, /tt_font, set_font='Helvetica Bold'
 device, filename=path+l+'rad.ps'
 device,/color,bits_per_pixel=8.
 device, xsize=20, ysize=40
      !p.font=1 & !p.thick=5 & !p.charsize=1.8
      !x.style=0 & !y.style=1 & !z.style=1 &!y.thick=1.8
      !x.thick=1.8

	!p.multi=[0,1,2]
    ymax = max([max(resp1si_arr(*,1,*)),max(resp1ir_arr(*,1,*)),max(resp2si_arr(*,1,*)),max(resp2ir_arr(*,1,*))])*1.04
    plot, wlvisn,resp1si(*,1),color=0,ytitle='counts/ms W!E-1!N m!E2!N nm',xtitle='Wavelength (nm)',title='Radiance',/xs,/ys,xrange=[300,1750],yrange=[0,ymax], /nodata
    oplot,wlnirz,resp1ir(*,1),thick=1.8,color=0, linestyle=2
	
	legend_tit=['Primary Cal - '+  primary_cal_date[num_secondary-1]  +' - {'+strcompress(string(nsi_intime_p[num_secondary-1]),/REMOVE_ALL)+','+strcompress(string(nir_intime_p[num_secondary-1]),/REMOVE_ALL)+'}']
	color_s=findgen(num_secondary)*230/num_secondary+25
	
    for i=0,num_secondary-1 do begin
      oplot,wlvisz,resp1si_arr(*,1,i),thick=1.8,color=color_s[i], linestyle=2
      oplot,wlnirz,resp1ir_arr(*,1,i),thick=1.8,color=color_s[i], linestyle=2
      legend_tit=[legend_tit,'Primary Cal - '+primary_cal_date[i]+' - {'+strcompress(string(nsi_intime_p[i]),/REMOVE_ALL)+','+strcompress(string(nir_intime_p[i]),/REMOVE_ALL)+'}']
    endfor
	
	;for i=0,num_secondary-1 do begin
  ;    oplot,wlvisz,resp2si_arr(*,1,i),thick=1.8,color=color_s[i]
  ;    oplot,wlnirz,resp2ir_arr(*,1,i),thick=1.8,color=color_s[i]
	;  legend_tit=[legend_tit,'Secondary Cal - '+secondary_cal_date[i]+' - {'+strcompress(string(nsi_intime_s[i]),/REMOVE_ALL)+','+strcompress(string(nir_intime_s[i]),/REMOVE_ALL)+'}']
	;endfor
    legend,legend_tit[1:*],textcolors=[color_s],/right,outline_color=255 ;'
	
	plot, wlvisz,resp2si_arr(*,1,ref),color=0,ytitle='%',xtitle='Wavelength (nm)',title='Percent Difference of response function !C Reference:'+ secondary_cal_date[ref]+' - Radiance',/xs,/ys,xrange=[300,1750],yrange=[-15,15], /nodata
    leg_tit='0'
    for i=0, num_secondary-1 do begin
      oplot,wlvisz,(resp2si_arr(*,1,i)-resp2si_arr(*,1,ref))*100./resp2si_arr(*,1,ref),thick=1.8,color=color_s[i]
      oplot,wlnirz,(resp2ir_arr(*,1,i)-resp2ir_arr(*,1,ref))*100./resp2ir_arr(*,1,ref),thick=1.8,color=color_s[i]
      leg_tit=[leg_tit, primary_cal_date[i]+' : '+secondary_cal_date[i]]
    endfor
    legend,['Primary : Secondary',leg_tit[1:*]],textcolors=[0,color_s],/right, outline_color=255

   ; p=tvrd(true=1)
   ; write_png,path+l+'Radiance.png',p
device,/close
spawn, 'convert '+path+l+'rad.ps '+path+l+'rad.png'
spawn, 'rm '+path+l+'rad.ps'
endif

if (1) then begin ; over time comparison
;window, 3, title='Change of response function over time at 940nm', xsize=500, ysize=1000
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated
 device, /tt_font, set_font='Helvetica Bold'
 device, filename=path+l+'time_cal_ship.ps'
 device,/color,bits_per_pixel=8.
 device, xsize=20, ysize=20
      !p.font=1 & !p.thick=5 & !p.charsize=2.5
      !x.style=0 & !y.style=1 & !z.style=1 &!y.thick=1.8
      !x.thick=1.8
!p.multi=0
if(strcmp(dozenith,'yes')) then begin
;zenith
kl=min(abs(940.-wlvisz),w940vz)
kl=min(abs(940.-wlnirz),w940iz)
plot, findgen(num_secondary), resp2si_arr[w940vz,0,*], title='Irradiance change of response function', ytitle='counts/ms W!E-1!N m!E2!N nm', xtitle='calibration days',color=0, charsize=1.5,thick=1.8,xstyle=0,ystyle=9, xmargin=[8,8], yrange=[min(resp2si_arr[w940vz,0,*])*0.96,max(resp2si_arr[w940vz,0,*])*1.04]
oplot,findgen(num_secondary), resp2si_arr[w940vz,0,*], color=0, psym=2
xyouts, 0,resp2si_arr[w940vz,0,0],'Pre'
xyouts, num_secondary-1,resp2si_arr[w940vz,0,num_secondary-1],'Post'

axis, yaxis=1, ytitle='counts/ms W!E-1!N m!E2!N nm', color=150, yrange=[min(resp2ir_arr[w940iz,0,*])*0.96, max(resp2ir_arr[w940iz,0,*])*1.02], ystyle=1, charsize=1.5, /save
oplot,findgen(num_secondary), resp2ir_arr[w940iz,0,*], color=150
oplot,findgen(num_secondary), resp2ir_arr[w940iz,0,*], color=150, psym=2

legend, ['Vis','NIR'],textcolors=[0,150]
k='irr'
endif

if(strcmp(donadir,'yes')) then begin
;nadir
kl=min(abs(940.-wlvisn),w940vn)
kl=min(abs(940.-wlnirn),w940in)
plot, findgen(num_secondary), resp2si_arr[w940vn,1,*], title='Radiance change of response function', ytitle='counts/ms W!E-1!N m!E2!N nm', xtitle='calibration days',color=0, charsize=1.5,thick=1.8,xstyle=0, ystyle=9, xmargin=[8,8], yrange=[min(resp2si_arr[w940vn,1,*])*0.96,max(resp2si_arr[w940vn,1,*])*1.04]
oplot,findgen(num_secondary), resp2si_arr[w940vn,1,*], color=0, psym=2
xyouts, 0,resp2si_arr[w940vn,1,0],'Pre'
xyouts, num_secondary-1,resp2si_arr[w940vn,1,num_secondary-1],'Post'

axis, yaxis=1, ytitle='counts/ms W!E-1!N m!E2!N nm', color=150, yrange=[min(resp2ir_arr[w940in,1,*])*0.96, max(resp2ir_arr[w940in,1,*])*1.02], ystyle=1, charsize=1.5, /save
oplot,findgen(num_secondary), resp2ir_arr[w940in,1,*], color=150
oplot,findgen(num_secondary), resp2ir_arr[w940in,1,*], color=150, psym=2

legend, ['Vis','NIR'],textcolors=[0,150]
k='rad'
endif
   ; p=tvrd(true=1)
   ; write_png,path+l+'time_cal_ship.png',p
device, /close
spawn, 'convert '+path+l+'time_cal_ship.ps '+path+l+'time_cal_ship_'+k+'.png'
spawn, 'rm '+path+l+'time_cal_ship.ps'

endif

if not (strcmp(donadir,'yes')) then begin  ;transfer comparison
;window, 2, title='Transfer comparison', xsize=500, ysize=500
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated
 device, /tt_font, set_font='Helvetica Bold'
 device, filename=path+l+'transfer_ship.ps'
 device,/color,bits_per_pixel=8.
 device, xsize=20, ysize=20
      !p.font=1 & !p.thick=5 & !p.charsize=2.5
      !x.style=0 & !y.style=1 & !z.style=1 &!y.thick=1.8
      !x.thick=1.8

!p.multi=0
plot, wlvisz, transfersi_arr[*,0,0]/transfersi_arr[*,0,num_secondary-1], title='Transfer change',ytitle='ratio pre-post',xtitle='Wavelength (nm)',charsize=1.5,thick=1.8,/xs,/ys,xrange=[300,1750],yrange=[0.9,1.1]
oplot,wlnirz, transferir_arr[*,0,0]/transferir_arr[*,0,num_secondary-1], color=0
oplot,wlvisn, transfersi_arr[*,1,0]/transfersi_arr[*,1,num_secondary-1], color=250
oplot,wlnirn, transferir_arr[*,1,0]/transferir_arr[*,1,num_secondary-1], color=250

legend, ['Irradiance'],textcolors=[0],/right
  ;  p=tvrd(true=1)
  ;  write_png,path+l+'transfer_ship.png',p
spawn, 'convert '+path+l+'transfer_ship.ps '+path+l+'transfer_ship.png'
spawn, 'rm '+path+l+'transfer_ship.ps'

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
