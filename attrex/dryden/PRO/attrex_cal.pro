@cfg.pro
@legend.pro
pro cal

path='/data/seven/schmidt/attrex/cal/ssfr6/'
l   ='/'                         ; directory separator
cfg_file=path+l+'ssfr6_818.cfg'
;cfg_file=path+l+'ssfr6_1276.cfg'

device,decomposed=0
loadct,27
!P.multi=0


if file_test(cfg_file) ne 1 then message,'Did not find configuration file.'

months = ["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"]

np                    = fix(cfg(cfg_file,'np'  ))    ; number of channels for each spectrum
innp_used             = fix(cfg(cfg_file,'innp_used'))    ; number of channels used in the IR

platform              = cfg(cfg_file,'platform')
data                  = cfg(cfg_file,'data')    ; data file extension

nist_traceable_source = cfg(cfg_file,'nist_traceable_source')
primary_lamp          = cfg(cfg_file,'primary_lamp')
lamp                  = cfg(cfg_file,'lamp')

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
wlvisz=fltarr(np) & wlnirz=fltarr(np)
wlvisn=fltarr(np) & wlnirn=fltarr(np)

; definitions
bignum=20000L
dark  =fltarr(np,bignum,4)
darkm =fltarr(np,4)
cali  =fltarr(np,bignum,4)
calim =fltarr(np,4)
resp1 =fltarr(np,4) ; primary response function
resp2 =fltarr(np,4) ; secondary response function

; make wavelengths / zenith
zlambdasi=cfg(cfg_file,'zlambdaSI') ; wavelength coefficients SI
zlambdasi=strsplit(zlambdasi,' ,',count=znlsi,escape='#',/EXTRACT) ; make string into substrings
zlambdasi=float(zlambdasi)
zlambdair=cfg(cfg_file,'zlambdaIR') ; wavelength coefficients IR
zlambdair=strsplit(zlambdair,' ,',count=znlir,escape='#',/EXTRACT) ; make string into substrings
zlambdair=float(zlambdair)
wlvisz=fltarr(np) & wlnirz=fltarr(np)
for i=0,np-1 do begin
   for j=0,znlsi-1 do wlvisz[i]     =wlvisz[i]     +zlambdasi[j]*float(i)^j
   for j=0,znlir-1 do wlnirz[np-1-i]=wlnirz[np-1-i]+zlambdair[j]*float(i)^j
endfor

; make wavelengths / nadir
nlambdasi=cfg(cfg_file,'nlambdaSI') ; wavelength coefficients SI
nlambdasi=strsplit(nlambdasi,' ,',count=nnlsi,escape='#',/EXTRACT) ; make string into substrings
nlambdasi=float(nlambdasi)
nlambdair=cfg(cfg_file,'nlambdaIR') ; wavelength coefficients IR
nlambdair=strsplit(nlambdair,' ,',count=nnlir,escape='#',/EXTRACT) ; make string into substrings
nlambdair=float(nlambdair)
wlvisn=fltarr(np) & wlnirn=fltarr(np)
for i=0,np-1 do begin
   for j=0,nnlsi-1 do wlvisn[i]     =wlvisn[i]     +nlambdasi[j]*float(i)^j
   for j=0,nnlir-1 do wlnirn[np-1-i]=wlnirn[np-1-i]+nlambdair[j]*float(i)^j
endfor

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PRIMARY CALIBRATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (1) process primary calibration - darks
specn = {btime: lonarr(2), bcdtimstp:  bytarr(12),intime1: long(0),$
                     intime2: long(0), intime3: long(0), intime4: long(0),accum: long(0), shsw:long(0),$
                     zsit: ulong(0),nsit: ulong(0),zirt: ulong(0),nirt: ulong(0), zirx: ulong(0), nirx: ulong(0),xt: ulong(0),it: ulong(0),$
                     zspecsi: intarr(np), zspecir: intarr(np), nspecsi: intarr(np), nspecir: intarr(np)}
if(strcmp(donadir,'yes')) then begin
    filen=primary_nadir_si+'dark'+l
    nspc_files = file_search(filen+data, count = numfiles, /FOLD_CASE)
    if (numfiles ne 1) then message,'There should be exactly 1 file for the dark.'
    openr,lunn,nspc_files[0],/get_lun
    ctn=0L
    while not eof(lunn) do begin
        readu, lunn, specn
        if(specn.intime3 ne nsi_intime_p) then message, "NSI int times don't match"
        dark[*,ctn,2]=specn.nspecsi
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
    for j=0,np-1 do begin       ; go through channels
        darkm[j,2]=mean(dark[j,0:ctn-1,2])
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
        dark[*,ctn,3]=specn.nspecir
        ctn = ctn + 1L
    endwhile
    for j=0,np-1 do begin       ; go through channels
        darkm[j,3]=mean(dark[j,0:ctn-1,3])
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
specz = {btime: lonarr(2), bcdtimstp:  bytarr(12),intime1: long(0),$
                     intime2: long(0), intime3: long(0), intime4: long(0),accum: long(0), shsw:long(0),$
                     zsit: ulong(0),nsit: ulong(0),zirt: ulong(0),nirt: ulong(0), zirx: ulong(0), nirx: ulong(0),xt: ulong(0),it: ulong(0),$
                     zspecsi: intarr(np), zspecir: intarr(np), nspecsi: intarr(np), nspecir: intarr(np)}
while not eof(lunz) do begin
    readu, lunz, specz
    if(specz.intime1 ne zsi_intime_p) then message, "ZSI int times don't match"
    dark[*,ctz,0]=specz.zspecsi
    ctz = ctz + 1L
endwhile
for j=0,np-1 do begin ; go through channels
    darkm[j,0]=mean(dark[j,0:ctz-1,0])
endfor
free_lun,lunz
;
filez=primary_zenith_ir+'dark'+l
zspc_files = file_search(filez+data, count = numfiles, /FOLD_CASE)
if (numfiles ne 1) then message,'There should be exaclt 1 file for the dark.'
openr,lunz,zspc_files[0],/get_lun
ctz=0L
specz = {btime: lonarr(2), bcdtimstp:  bytarr(12),intime1: long(0),$
                     intime2: long(0), intime3: long(0), intime4: long(0),accum: long(0), shsw:long(0),$
                     zsit: ulong(0),nsit: ulong(0),zirt: ulong(0),nirt: ulong(0), zirx: ulong(0), nirx: ulong(0),xt: ulong(0),it: ulong(0),$
                     zspecsi: intarr(np), zspecir: intarr(np), nspecsi: intarr(np), nspecir: intarr(np)}
while not eof(lunz) do begin
    readu, lunz, specz
    if(specz.intime2 ne zir_intime_p) then message, "ZIR int times don't match"
    dark[*,ctz,1]=specz.zspecir
    ctz = ctz + 1L
endwhile
for j=0,np-1 do begin ; go through channels
    darkm[j,1]=mean(dark[j,0:ctz-1,1])
endfor
free_lun,lunz
endif

; (2) process primary calibration - calibration
specn = {btime: lonarr(2), bcdtimstp:  bytarr(12),intime1: long(0),$
                     intime2: long(0), intime3: long(0), intime4: long(0),accum: long(0), shsw:long(0),$
                     zsit: ulong(0),nsit: ulong(0),zirt: ulong(0),nirt: ulong(0), zirx: ulong(0), nirx: ulong(0),xt: ulong(0),it: ulong(0),$
                     zspecsi: intarr(np), zspecir: intarr(np), nspecsi: intarr(np), nspecir: intarr(np)}
if(strcmp(donadir,'yes')) then begin
    filen=primary_nadir_si+'cal'+l
    nspc_files = file_search(filen+data, count = numfiles, /FOLD_CASE)
    if (numfiles ne 1) then message,'There should be exactly 1 file for the dark.'
    openr,lunn,nspc_files[0],/get_lun
    ctn=0L
    while not eof(lunn) do begin
        readu, lunn, specn
        if(specn.intime3 ne nsi_intime_p) then message, "NSI int times don't match"
        cali[*,ctn,2]=specn.nspecsi
        ctn = ctn + 1L
    endwhile
    for j=0,np-1 do begin       ; go through channels
        calim[j,2]=mean(cali[j,0:ctn-1,2])
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
        cali[*,ctn,3]=specn.nspecir
        ctn = ctn + 1L
    endwhile
    for j=0,np-1 do begin       ; go through channels
        calim[j,3]=mean(cali[j,0:ctn-1,3])
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
specz = {btime: lonarr(2), bcdtimstp:  bytarr(12),intime1: long(0),$
                     intime2: long(0), intime3: long(0), intime4: long(0),accum: long(0), shsw:long(0),$
                     zsit: ulong(0),nsit: ulong(0),zirt: ulong(0),nirt: ulong(0), zirx: ulong(0), nirx: ulong(0),xt: ulong(0),it: ulong(0),$
                     zspecsi: intarr(np), zspecir: intarr(np), nspecsi: intarr(np), nspecir: intarr(np)}
while not eof(lunz) do begin
    readu, lunz, specz
    if(specz.intime1 ne zsi_intime_p) then message, "ZSI int times don't match"
    cali[*,ctz,0]=specz.zspecsi
    ctz = ctz + 1L
endwhile
for j=0,np-1 do begin ; go through channels
    calim[j,0]=mean(cali[j,0:ctz-1,0])
endfor
free_lun,lunz
;
filez=primary_zenith_ir+'cal'+l
zspc_files = file_search(filez+data, count = numfiles, /FOLD_CASE)
if (numfiles ne 1) then message,'There should be exaclt 1 file for the dark.'
openr,lunz,zspc_files[0],/get_lun
ctz=0L
specz = {btime: lonarr(2), bcdtimstp:  bytarr(12),intime1: long(0),$
                     intime2: long(0), intime3: long(0), intime4: long(0),accum: long(0), shsw:long(0),$
                     zsit: ulong(0),nsit: ulong(0),zirt: ulong(0),nirt: ulong(0), zirx: ulong(0), nirx: ulong(0),xt: ulong(0),it: ulong(0),$
                     zspecsi: intarr(np), zspecir: intarr(np), nspecsi: intarr(np), nspecir: intarr(np)}
while not eof(lunz) do begin
    readu, lunz, specz
    if(specz.intime2 ne zir_intime_p) then message, "ZIR int times don't match"
    cali[*,ctz,1]=specz.zspecir
    ctz = ctz + 1L
endwhile
for j=0,np-1 do begin ; go through channels
    calim[j,1]=mean(cali[j,0:ctz-1,1])
endfor
free_lun,lunz
endif

; (3) read in lamp data
nl=file_lines(resp_func_dir+l+nist_traceable_source)
openr,lunl,resp_func_dir+l+nist_traceable_source,/get_lun
pl=fltarr(nl) & pi=fltarr(nl)
for i=0,nl-1 do begin
  readf,lunl,a,b
  pl[i]=a & pi[i]=b
endfor

free_lun,lunl

print,'Warning, the lamp splining should be replaced by Planck fitting!'
lampzensi = spline(pl,pi,wlvisz) ; spline lamp response to SSFR wavelengths
lampzenir = spline(pl,pi,wlnirz)
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
if(nist_traceable_source eq 'radiance_3lamps_20071219.txt') then begin
    lampzensi/=1000.
    lampzenir/=1000.
    lampnadsi/=1000.
    lampnadir/=1000.
endif else begin
    lampzensi*=1E4
    lampzenir*=1E4
    lampnadsi*=1E4
    lampnadir*=1E4
endelse

dn_1 = calim - darkm             ; 0 - zen si, 1 - zen ir, 2 - nad si, 3 - nad ir

if(np eq innp_used) then begin
    print,'Reverse NIR wavelengths.'
    dn_1[*,1] = reverse(dn_1[*,1])
    dn_1[*,3] = reverse(dn_1[*,3])
endif else begin
    dn_1[*,1] = [reverse(dn_1[0:innp_used-1,1]),dn_1[innp_used:*,1]]
    dn_1[*,3] = [reverse(dn_1[0:innp_used-1,3]),dn_1[innp_used:*,3]]
endelse
resp1[*,0] = dn_1[*,0]/float(zsi_intime_p)/(lampzensi)
resp1[*,1] = dn_1[*,1]/float(zir_intime_p)/(lampzenir)

resp1[*,2] = dn_1[*,2]/float(nsi_intime_p)/(lampnadsi)
resp1[*,3] = dn_1[*,3]/float(nir_intime_p)/(lampnadir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TRANSFER CALIBRATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; (4) transfer of calibration - darks
specn = {btime: lonarr(2), bcdtimstp:  bytarr(12),intime1: long(0),$
                     intime2: long(0), intime3: long(0), intime4: long(0),accum: long(0), shsw:long(0),$
                     zsit: ulong(0),nsit: ulong(0),zirt: ulong(0),nirt: ulong(0), zirx: ulong(0), nirx: ulong(0),xt: ulong(0),it: ulong(0),$
                     zspecsi: intarr(np), zspecir: intarr(np), nspecsi: intarr(np), nspecir: intarr(np)}
if(strcmp(donadir,'yes')) then begin
    filen=transfer_nadir_si+'dark'+l
    nspc_files = file_search(filen+data, count = numfiles, /FOLD_CASE)
    if (numfiles ne 1) then message,'There should be exactly 1 file for the dark.'
    openr,lunn,nspc_files[0],/get_lun
    ctn=0L
    while not eof(lunn) do begin
        readu, lunn, specn
        if(specn.intime3 ne nsi_intime_t) then message, "NSI int times don't match"
        dark[*,ctn,2]=specn.nspecsi
        ctn = ctn + 1L
    endwhile
    for j=0,np-1 do begin       ; go through channels
        darkm[j,2]=mean(dark[j,0:ctn-1,2])
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
        dark[*,ctn,3]=specn.nspecir
        ctn = ctn + 1L
    endwhile
    for j=0,np-1 do begin       ; go through channels
        darkm[j,3]=mean(dark[j,0:ctn-1,3])
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
specz = {btime: lonarr(2), bcdtimstp:  bytarr(12),intime1: long(0),$
                     intime2: long(0), intime3: long(0), intime4: long(0),accum: long(0), shsw:long(0),$
                     zsit: ulong(0),nsit: ulong(0),zirt: ulong(0),nirt: ulong(0), zirx: ulong(0), nirx: ulong(0),xt: ulong(0),it: ulong(0),$
                     zspecsi: intarr(np), zspecir: intarr(np), nspecsi: intarr(np), nspecir: intarr(np)}
while not eof(lunz) do begin
    readu, lunz, specz
    if(specz.intime1 ne zsi_intime_t) then message, "ZSI int times don't match"
    dark[*,ctz,0]=specz.zspecsi
    ctz = ctz + 1L
endwhile
for j=0,np-1 do begin ; go through channels
    darkm[j,0]=mean(dark[j,0:ctz-1,0])
endfor
free_lun,lunz
;
filez=transfer_zenith_ir+'dark'+l
zspc_files = file_search(filez+data, count = numfiles, /FOLD_CASE)
if (numfiles ne 1) then message,'There should be exaclt 1 file for the dark.'
openr,lunz,zspc_files[0],/get_lun
ctz=0L
specz = {btime: lonarr(2), bcdtimstp:  bytarr(12),intime1: long(0),$
                     intime2: long(0), intime3: long(0), intime4: long(0),accum: long(0), shsw:long(0),$
                     zsit: ulong(0),nsit: ulong(0),zirt: ulong(0),nirt: ulong(0), zirx: ulong(0), nirx: ulong(0),xt: ulong(0),it: ulong(0),$
                     zspecsi: intarr(np), zspecir: intarr(np), nspecsi: intarr(np), nspecir: intarr(np)}
while not eof(lunz) do begin
    readu, lunz, specz
    if(specz.intime2 ne zir_intime_t) then message, "ZIR int times don't match"
    dark[*,ctz,1]=specz.zspecir
    ctz = ctz + 1L
endwhile
for j=0,np-1 do begin ; go through channels
    darkm[j,1]=mean(dark[j,0:ctz-1,1])
endfor
free_lun,lunz
endif

; (5) transfer of calibration - cal
specn = {btime: lonarr(2), bcdtimstp:  bytarr(12),intime1: long(0),$
                     intime2: long(0), intime3: long(0), intime4: long(0),accum: long(0), shsw:long(0),$
                     zsit: ulong(0),nsit: ulong(0),zirt: ulong(0),nirt: ulong(0), zirx: ulong(0), nirx: ulong(0),xt: ulong(0),it: ulong(0),$
                     zspecsi: intarr(np), zspecir: intarr(np), nspecsi: intarr(np), nspecir: intarr(np)}
if(strcmp(donadir,'yes')) then begin
    filen=transfer_nadir_si+'cal'+l
    nspc_files = file_search(filen+data, count = numfiles, /FOLD_CASE)
    if (numfiles ne 1) then message,'There should be exactly 1 file for the dark.'
    openr,lunn,nspc_files[0],/get_lun
    ctn=0L
    while not eof(lunn) do begin
        readu, lunn, specn
        if(specn.intime3 ne nsi_intime_t) then message, "NSI int times don't match"
        cali[*,ctn,2]=specn.nspecsi
        ctn = ctn + 1L
    endwhile
    for j=0,np-1 do begin       ; go through channels
        calim[j,2]=mean(cali[j,0:ctn-1,2])
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
        cali[*,ctn,3]=specn.nspecir
        ctn = ctn + 1L
    endwhile
    for j=0,np-1 do begin       ; go through channels
        calim[j,3]=mean(cali[j,0:ctn-1,3])
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
specz = {btime: lonarr(2), bcdtimstp:  bytarr(12),intime1: long(0),$
                     intime2: long(0), intime3: long(0), intime4: long(0),accum: long(0), shsw:long(0),$
                     zsit: ulong(0),nsit: ulong(0),zirt: ulong(0),nirt: ulong(0), zirx: ulong(0), nirx: ulong(0),xt: ulong(0),it: ulong(0),$
                     zspecsi: intarr(np), zspecir: intarr(np), nspecsi: intarr(np), nspecir: intarr(np)}
while not eof(lunz) do begin
    readu, lunz, specz
    if(specz.intime1 ne zsi_intime_t) then message, "ZSI int times don't match"
    cali[*,ctz,0]=specz.zspecsi
    ctz = ctz + 1L
endwhile
for j=0,np-1 do begin ; go through channels
    calim[j,0]=mean(cali[j,0:ctz-1,0])
endfor
free_lun,lunz
;
filez=transfer_zenith_ir+'cal'+l
zspc_files = file_search(filez+data, count = numfiles, /FOLD_CASE)
if (numfiles ne 1) then message,'There should be exaclt 1 file for the dark.'
openr,lunz,zspc_files[0],/get_lun
ctz=0L
specz = {btime: lonarr(2), bcdtimstp:  bytarr(12),intime1: long(0),$
                     intime2: long(0), intime3: long(0), intime4: long(0),accum: long(0), shsw:long(0),$
                     zsit: ulong(0),nsit: ulong(0),zirt: ulong(0),nirt: ulong(0), zirx: ulong(0), nirx: ulong(0),xt: ulong(0),it: ulong(0),$
                     zspecsi: intarr(np), zspecir: intarr(np), nspecsi: intarr(np), nspecir: intarr(np)}
while not eof(lunz) do begin
    readu, lunz, specz
    if(specz.intime2 ne zir_intime_t) then message, "ZIR int times don't match"
    cali[*,ctz,1]=specz.zspecir
    ctz = ctz + 1L
endwhile
for j=0,np-1 do begin ; go through channels
    calim[j,1]=mean(cali[j,0:ctz-1,1])
endfor
free_lun,lunz
endif


dn_2 = calim - darkm

if(np eq innp_used) then begin
    dn_2[*,1] = reverse(dn_2[*,1])
    dn_2[*,3] = reverse(dn_2[*,3])
endif else begin
    dn_2[*,1] = [reverse(dn_2[0:innp_used-1,1]),dn_2[innp_used:*,1]]
    dn_2[*,3] = [reverse(dn_2[0:innp_used-1,3]),dn_2[innp_used:*,3]]
endelse

transfer = dn_1 / dn_2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SECONDARY CALIBRATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; (6) secondary calibration - darks
specn = {btime: lonarr(2), bcdtimstp:  bytarr(12),intime1: long(0),$
                     intime2: long(0), intime3: long(0), intime4: long(0),accum: long(0), shsw:long(0),$
                     zsit: ulong(0),nsit: ulong(0),zirt: ulong(0),nirt: ulong(0), zirx: ulong(0), nirx: ulong(0),xt: ulong(0),it: ulong(0),$
                     zspecsi: intarr(np), zspecir: intarr(np), nspecsi: intarr(np), nspecir: intarr(np)}
if(strcmp(donadir,'yes')) then begin
    filen=secondary_nadir_si+'dark'+l
    nspc_files = file_search(filen+data, count = numfiles, /FOLD_CASE)
    if (numfiles ne 1) then message,'There should be exactly 1 file for the dark.'
    openr,lunn,nspc_files[0],/get_lun
    ctn=0L
    while not eof(lunn) do begin
        readu, lunn, specn
        if(specn.intime3 ne nsi_intime_s) then message, "NSI int times don't match"
        dark[*,ctn,2]=specn.nspecsi
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
    for j=0,np-1 do begin       ; go through channels
        darkm[j,2]=mean(dark[j,0:ctn-1,2])
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
        dark[*,ctn,3]=specn.nspecir
        ctn = ctn + 1L
    endwhile
    for j=0,np-1 do begin       ; go through channels
        darkm[j,3]=mean(dark[j,0:ctn-1,3])
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
specz = {btime: lonarr(2), bcdtimstp:  bytarr(12),intime1: long(0),$
                     intime2: long(0), intime3: long(0), intime4: long(0),accum: long(0), shsw:long(0),$
                     zsit: ulong(0),nsit: ulong(0),zirt: ulong(0),nirt: ulong(0), zirx: ulong(0), nirx: ulong(0),xt: ulong(0),it: ulong(0),$
                     zspecsi: intarr(np), zspecir: intarr(np), nspecsi: intarr(np), nspecir: intarr(np)}
while not eof(lunz) do begin
    readu, lunz, specz
    if(specz.intime1 ne zsi_intime_s) then message, "ZSI int times don't match"
    dark[*,ctz,0]=specz.zspecsi
    ctz = ctz + 1L
endwhile
for j=0,np-1 do begin ; go through channels
    darkm[j,0]=mean(dark[j,0:ctz-1,0])
endfor
free_lun,lunz
;
filez=secondary_zenith_ir+'dark'+l
zspc_files = file_search(filez+data, count = numfiles, /FOLD_CASE)
if (numfiles ne 1) then message,'There should be exaclt 1 file for the dark.'
openr,lunz,zspc_files[0],/get_lun
ctz=0L
specz = {btime: lonarr(2), bcdtimstp:  bytarr(12),intime1: long(0),$
                     intime2: long(0), intime3: long(0), intime4: long(0),accum: long(0), shsw:long(0),$
                     zsit: ulong(0),nsit: ulong(0),zirt: ulong(0),nirt: ulong(0), zirx: ulong(0), nirx: ulong(0),xt: ulong(0),it: ulong(0),$
                     zspecsi: intarr(np), zspecir: intarr(np), nspecsi: intarr(np), nspecir: intarr(np)}
while not eof(lunz) do begin
    readu, lunz, specz
    if(specz.intime2 ne zir_intime_s) then message, "ZIR int times don't match"
    dark[*,ctz,1]=specz.zspecir
    ctz = ctz + 1L
endwhile
for j=0,np-1 do begin ; go through channels
    darkm[j,1]=mean(dark[j,0:ctz-1,1])
endfor
free_lun,lunz
endif


; (7) secondary calibration - cal
specn = {btime: lonarr(2), bcdtimstp:  bytarr(12),intime1: long(0),$
                     intime2: long(0), intime3: long(0), intime4: long(0),accum: long(0), shsw:long(0),$
                     zsit: ulong(0),nsit: ulong(0),zirt: ulong(0),nirt: ulong(0), zirx: ulong(0), nirx: ulong(0),xt: ulong(0),it: ulong(0),$
                     zspecsi: intarr(np), zspecir: intarr(np), nspecsi: intarr(np), nspecir: intarr(np)}
if(strcmp(donadir,'yes')) then begin
    filen=secondary_nadir_si+'cal'+l
    nspc_files = file_search(filen+data, count = numfiles, /FOLD_CASE)
    if (numfiles ne 1) then message,'There should be exactly 1 file for the dark.'
    openr,lunn,nspc_files[0],/get_lun
    ctn=0L
    while not eof(lunn) do begin
        readu, lunn, specn
        if(specn.intime3 ne nsi_intime_s) then message, "NSI int times don't match"
        cali[*,ctn,2]=specn.nspecsi
        ctn = ctn + 1L
    endwhile
    for j=0,np-1 do begin       ; go through channels
        calim[j,2]=mean(cali[j,0:ctn-1,2])
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
        cali[*,ctn,3]=specn.nspecir
        ctn = ctn + 1L
    endwhile
    for j=0,np-1 do begin       ; go through channels
        calim[j,3]=mean(cali[j,0:ctn-1,3])
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
specz = {btime: lonarr(2), bcdtimstp:  bytarr(12),intime1: long(0),$
                     intime2: long(0), intime3: long(0), intime4: long(0),accum: long(0), shsw:long(0),$
                     zsit: ulong(0),nsit: ulong(0),zirt: ulong(0),nirt: ulong(0), zirx: ulong(0), nirx: ulong(0),xt: ulong(0),it: ulong(0),$
                     zspecsi: intarr(np), zspecir: intarr(np), nspecsi: intarr(np), nspecir: intarr(np)}
while not eof(lunz) do begin
    readu, lunz, specz
    if(specz.intime1 ne zsi_intime_s) then message, "ZSI int times don't match"
    cali[*,ctz,0]=specz.zspecsi
    ctz = ctz + 1L
endwhile
for j=0,np-1 do begin ; go through channels
    calim[j,0]=mean(cali[j,0:ctz-1,0])
endfor
free_lun,lunz
;
filez=secondary_zenith_ir+'cal'+l
zspc_files = file_search(filez+data, count = numfiles, /FOLD_CASE)
if (numfiles ne 1) then message,'There should be exaclt 1 file for the dark.'
openr,lunz,zspc_files[0],/get_lun
ctz=0L
specz = {btime: lonarr(2), bcdtimstp:  bytarr(12),intime1: long(0),$
                     intime2: long(0), intime3: long(0), intime4: long(0),accum: long(0), shsw:long(0),$
                     zsit: ulong(0),nsit: ulong(0),zirt: ulong(0),nirt: ulong(0), zirx: ulong(0), nirx: ulong(0),xt: ulong(0),it: ulong(0),$
                     zspecsi: intarr(np), zspecir: intarr(np), nspecsi: intarr(np), nspecir: intarr(np)}
while not eof(lunz) do begin
    readu, lunz, specz
    if(specz.intime2 ne zir_intime_s) then message, "ZIR int times don't match"
    cali[*,ctz,1]=specz.zspecir
    ctz = ctz + 1L
endwhile
for j=0,np-1 do begin ; go through channels
    calim[j,1]=mean(cali[j,0:ctz-1,1])
endfor
free_lun,lunz
endif

dn2 = calim - darkm

if(np eq innp_used) then begin
    dn2[*,1] = reverse(dn2[*,1])
    dn2[*,3] = reverse(dn2[*,3])
endif else begin
    dn2[*,1] = [reverse(dn2[0:innp_used-1,1]),dn2[innp_used:*,1]]
    dn2[*,3] = [reverse(dn2[0:innp_used-1,3]),dn2[innp_used:*,3]]
endelse

if(strcmp(dozenith,'yes')) then begin
    resp2[*,0] = dn2[*,0]/zsi_intime_s/(lampzensi)*transfer[*,0]
    resp2[*,1] = dn2[*,1]/zir_intime_s/(lampzenir)*transfer[*,1]

        resp1_si_file = resp_func_dir + primary_cal_date + '_'+primary_lamp+'_resp1_'+strcompress(string(zsi_intime_p),/REMOVE_ALL)+'_zensi.dat'
        resp1_ir_file = resp_func_dir + primary_cal_date + '_'+primary_lamp+'_resp1_'+strcompress(string(zir_intime_p),/REMOVE_ALL)+'_zenir.dat'
        tmp=file_search(resp1_si_file,count=count)
        ;;if(count gt 0) then message, resp1_si_file + ' already exists'
        tmp=file_search(resp1_ir_file,count=count)
        ;;if(count gt 0) then message, resp1_ir_file + ' already exists'

        openw,10,resp1_si_file
        openw,11,resp1_ir_file
        for i=0,np-1 do begin
            printf,10,wlvisz[i],resp1[i,0]
            printf,11,wlnirz[i],resp1[i,1]
        endfor
        close,10
        close,11
endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; RESPONSE FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

if(strcmp(donadir,'yes')) then begin
    resp2[*,2] = dn2[*,2]/nsi_intime_s/(lampnadsi)*transfer[*,2]
    resp2[*,3] = dn2[*,3]/nir_intime_s/(lampnadir)*transfer[*,3]
    if(strcmp(write_primary,'yes')) then begin

        resp1_si_file = resp_func_dir + primary_cal_date + '_'+primary_lamp+'_resp1_'+strcompress(string(nsi_intime_p),/REMOVE_ALL)+'_nadsi.dat'
        resp1_ir_file = resp_func_dir + primary_cal_date + '_'+primary_lamp+'_resp1_'+strcompress(string(nir_intime_p),/REMOVE_ALL)+'_nadir.dat'

        tmp=file_search(resp1_si_file,count=count)
        ;;if(count gt 0) then message, resp1_si_file + ' already exists'
        tmp=file_search(resp1_ir_file,count=count)
        ;;if(count gt 0) then message, resp1_ir_file + ' already exists'
        openw,12,resp1_si_file
        openw,13,resp1_ir_file
        for i=0,np-1 do begin
            printf,12,wlvisn[i],resp1[i,2]
            printf,13,wlnirn[i],resp1[i,3]
        endfor
        close,12
        close,13
    endif
endif


if(strcmp(dozenith,'yes')) then begin

    resp2_si_file = resp_func_dir + secondary_cal_date + '_'+lamp+'_resp2_'+strcompress(string(zsi_intime_s),/REMOVE_ALL)+'_zensi.dat'
    resp2_ir_file = resp_func_dir + secondary_cal_date + '_'+lamp+'_resp2_'+strcompress(string(zir_intime_s),/REMOVE_ALL)+'_zenir.dat'

    tmp=file_search(resp2_si_file,count=count)
    ;;if(count gt 0) then message, resp2_si_file + ' already exists'
    tmp=file_search(resp2_ir_file,count=count)
    ;;if(count gt 0) then message, resp2_ir_file + ' already exists'

    openw,10,resp2_si_file
    openw,11,resp2_ir_file
    for i=0,np-1 do begin
        printf,10,wlvisz[i],resp2[i,0]
        printf,11,wlnirz[i],resp2[i,1]
    endfor
    close,10
    close,11
endif


if(strcmp(donadir,'yes')) then begin

    resp2_si_file = resp_func_dir + secondary_cal_date + '_'+lamp+'_resp2_'+strcompress(string(nsi_intime_s),/REMOVE_ALL)+'_nadsi.dat'
    resp2_ir_file = resp_func_dir + secondary_cal_date + '_'+lamp+'_resp2_'+strcompress(string(nir_intime_s),/REMOVE_ALL)+'_nadir.dat'
    print,resp2_ir_file
    tmp=file_search(resp2_si_file,count=count)
    ;;if(count gt 0) then message, resp2_si_file + ' already exists'
    tmp=file_search(resp2_ir_file + ' already exists')
    ;;if(count gt 0) then message, resp2_ir_file + ' already exists'

    openw,12,resp2_si_file
    openw,13,resp2_ir_file
    for i=0,np-1 do begin
        printf,12,wlvisn[i],resp2[i,2]
        printf,13,wlnirn[i],resp2[i,3]
    endfor
    close,12
    close,13
endif

if(strcmp(dozenith,'yes')) then begin
    window,0,tit='zenith'
    ymax = max([max(resp1(*,0)),max(resp1(*,1)),max(resp2(*,0)),max(resp2(*,1))])*1.04
    plot, wlvisz,resp1(*,0),color=3,ytitle='counts/ms W!E-1!N m!E2!N nm',xtitle='Wavelength (nm)',title='Zenith'+' p: '+primary_lamp+' s: '+lamp,charsize=1.5,thick=1.8,/xs,/ys,xrange=[300,2200],yrange=[0,ymax]
    oplot,wlnirz,resp1(*,1),thick=1.8,color=3
    oplot,wlvisz,resp2(*,0),thick=1.8,color=110
    oplot,wlnirz,resp2(*,1),thick=1.8,color=110
    legend,['Primary Cal - '+  primary_cal_date  +' - {'+strcompress(string(zsi_intime_p),/REMOVE_ALL)+','+strcompress(string(zir_intime_p),/REMOVE_ALL)+'}',$
            'Secondary Cal - '+secondary_cal_date+' - {'+strcompress(string(zsi_intime_s),/REMOVE_ALL)+','+strcompress(string(zir_intime_s),/REMOVE_ALL)+'}'],textcolors=[3,110],/right,outline_color=3
    p=tvrd(true=1)
    write_png,path+l+'zenith.png',p
endif

if(strcmp(donadir,'yes')) then begin
    window,1,tit='nadir'
    ymax = max([max(resp1(*,2)),max(resp1(*,3)),max(resp2(*,2)),max(resp2(*,3))])*1.04
    plot, wlvisn,resp1(*,2),color=3,ytitle='counts/ms W!E-1!N m!E2!N nm',xtitle='Wavelength (nm)',title='Nadir'+' p: '+primary_lamp+' s: '+lamp,charsize=1.5,thick=1.8,/xs,/ys,xrange=[300,2200],yrange=[0,ymax]
    oplot,wlnirn,resp1(*,3),thick=1.8,color=3
    oplot,wlvisn,resp2(*,2),thick=1.8,color=110
    oplot,wlnirn,resp2(*,3),thick=1.8,color=110
    ;legend,['Primary Cal - '+primary_cal_date,'Secondary Cal - '+secondary_cal_date],textcolors=[3,110],/right,outline_color=3
    legend,['Primary Cal - '+  primary_cal_date  +' - {'+strcompress(string(nsi_intime_p),/REMOVE_ALL)+','+strcompress(string(nir_intime_p),/REMOVE_ALL)+'}',$
            'Secondary Cal - '+secondary_cal_date+' - {'+strcompress(string(nsi_intime_s),/REMOVE_ALL)+','+strcompress(string(nir_intime_s),/REMOVE_ALL)+'}'],textcolors=[3,110],/right,outline_color=3
    p=tvrd(true=1)
    write_png,path+l+'nadir.png',p
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
