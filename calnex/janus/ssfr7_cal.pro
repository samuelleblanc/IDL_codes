@cfg.pro
@legend.pro
pro ssfr7_cal, cfg_file

device,decomposed=0
loadct,27
!P.multi=0

l  ='/'                         ; directory separator
if file_test(cfg_file) ne 1 then message,'Did not find configuration file.'

np     =cfg(cfg_file,'np'  )    ; number of channels for each spectrum
np     =fix(np)

innp_used = fix(cfg(cfg_file,'innp_used'))    ; number of channels used in the IR

platform = cfg(cfg_file,'platform')
lamp = cfg(cfg_file,'lamp')

data   =cfg(cfg_file,'data')    ; data file names

nist_traceable_source = cfg(cfg_file,'nist_traceable_source')
primary_lamp  = cfg(cfg_file,'primary_lamp')

primary_cal_date = cfg(cfg_file,'primary_cal_date')
secondary_cal_date = cfg(cfg_file,'secondary_cal_date')
primary_nadir = cfg(cfg_file,'primary_nadir')
primary_zenith = cfg(cfg_file,'primary_zenith')

secondary_nadir = cfg(cfg_file,'secondary_nadir')
secondary_zenith = cfg(cfg_file,'secondary_zenith')

transfer_nadir = cfg(cfg_file,'transfer_nadir')
transfer_zenith = cfg(cfg_file,'transfer_zenith')

resp_func_dir = cfg(cfg_file,'resp_func_dir')

zsi_intime = cfg(cfg_file,'zsi_intime')
zir_intime = cfg(cfg_file,'zir_intime')
nsi_intime = cfg(cfg_file,'nsi_intime')
nir_intime = cfg(cfg_file,'nir_intime')

dozenith = cfg(cfg_file,'dozenith')
donadir  = cfg(cfg_file,'donadir')
write_primary  = cfg(cfg_file,'write_primary')

wlvisz=fltarr(np) & wlnirz=fltarr(np) 
wlvisn=fltarr(np) & wlnirn=fltarr(np)

bignum=20000L
dark  =fltarr(np,bignum,4)
darkm =fltarr(np,4)
cali  =fltarr(np,bignum,4)
calim =fltarr(np,4)
resp1 =fltarr(np,4) ; primary response function
resp2 =fltarr(np,4) ; secondary response function

zlambdasi=cfg(cfg_file,'zlambdaSI') ; wavelength coefficients SI
zlambdasi=strsplit(zlambdasi,' ,',count=znlsi,escape='#',/EXTRACT) ; make string into substrings
zlambdasi=float(zlambdasi)
zlambdair=cfg(cfg_file,'zlambdaIR') ; wavelength coefficients IR
zlambdair=strsplit(zlambdair,' ,',count=znlir,escape='#',/EXTRACT) ; make string into substrings
zlambdair=float(zlambdair)

nlambdasi=cfg(cfg_file,'nlambdaSI') ; wavelength coefficients SI
nlambdasi=strsplit(nlambdasi,' ,',count=nnlsi,escape='#',/EXTRACT) ; make string into substrings
nlambdasi=float(nlambdasi)
nlambdair=cfg(cfg_file,'nlambdaIR') ; wavelength coefficients IR
nlambdair=strsplit(nlambdair,' ,',count=nnlir,escape='#',/EXTRACT) ; make string into substrings
nlambdair=float(nlambdair)

wlvisz=fltarr(np) & wlnirz=fltarr(np)
for i=0,np-1 do begin
   for j=0,znlsi-1 do wlvisz[i]     =wlvisz[i]     +zlambdasi[j]*float(i)^j
;   for j=0,znlir-1 do wlnirz[np-1-i]=wlnirz[np-1-i]+zlambdair[j]*float(i)^j
   for j=0,znlir-1 do wlnirz[i]     =wlnirz[i]     +zlambdair[j]*float(i)^j
endfor
wlvisn=fltarr(np) & wlnirn=fltarr(np)
for i=0,np-1 do begin
   for j=0,nnlsi-1 do wlvisn[i]     =wlvisn[i]     +nlambdasi[j]*float(i)^j
;   for j=0,nnlir-1 do wlnirn[np-1-i]=wlnirn[np-1-i]+nlambdair[j]*float(i)^j
   for j=0,nnlir-1 do wlnirn[i]     =wlnirn[i]     +nlambdair[j]*float(i)^j
endfor
; (1) process primary calibration - darks

specn = {btime: lonarr(2), bcdtimstp:  bytarr(12),intime1: long(0),intime2: long(0), intime3: long(0), intime4: long(0),accum: long(0), shsw:long(0),  ad0: ulong(0),ad1: ulong(0), ad2: ulong(0), ad3: ulong(0), ad4: ulong(0), ad5: ulong(0), ad6: ulong(0), ad7: ulong(0), zspecsi: intarr(np), zspecir: intarr(np), nspecsi: intarr(np), nspecir: intarr(np)}

if(strcmp(donadir,'yes')) then begin
    filen=primary_nadir+'dark/'
    nspc_files = file_search(filen+data, count = numfiles, /FOLD_CASE)
    if (numfiles ne 1) then message,'There should be exactly 1 file for the dark.'
    openr,lunn,nspc_files[0],/get_lun
    ctn=0L
    while not eof(lunn) do begin
        readu, lunn, specn
        if(specn.intime3 ne nsi_intime) then message, "NSI int times don't match"
        if(specn.intime4 ne nir_intime) then message, "NIR int times don't match"
        ;;if(specn.intime2 ne nir_intime) then message, "NIR int times don't match"
                                ;print,specn.intime1,specn.intime2,specn.intime3,specn.intime4
        dark[*,ctn,2]=specn.nspecsi &  dark[*,ctn,3]=specn.nspecir
        ctn = ctn + 1L
    endwhile
    for j=0,np-1 do begin       ; go through channels
        for i=2,3 do begin      ; go through spectrometers
            darkm[j,i]=mean(dark[j,0:ctn-1,i])
        endfor
    endfor
    free_lun,lunn
endif

if(strcmp(dozenith,'yes')) then begin
filez=primary_zenith+'dark/'
zspc_files = file_search(filez+data, count = numfiles, /FOLD_CASE)
if (numfiles ne 1) then message,'There should be exaclt 1 file for the dark.'
openr,lunz,zspc_files[0],/get_lun
ctz=0L
specz = {btime: lonarr(2), bcdtimstp:  bytarr(12),intime1: long(0),intime2: long(0), intime3: long(0), intime4: long(0),accum: long(0), shsw:long(0),  ad0: ulong(0),ad1: ulong(0), ad2: ulong(0), ad3: ulong(0), ad4: ulong(0), ad5: ulong(0), ad6: ulong(0), ad7: ulong(0),zspecsi: intarr(np), zspecir: intarr(np), nspecsi: intarr(np), nspecir: intarr(np)}
while not eof(lunz) do begin
    readu, lunz, specz
    if(specz.intime1 ne zsi_intime) then message, "ZSI int times don't match"
    if(specz.intime2 ne zir_intime) then message, "ZIR int times don't match"
    ;print,specz.intime1,specz.intime2,specz.intime3,specz.intime4
    dark[*,ctz,0]=specz.zspecsi &  dark[*,ctz,1]=specz.zspecir
    ctz = ctz + 1L
endwhile
for j=0,np-1 do begin ; go through channels
  for i=0,1 do begin ; go through spectrometers
    darkm[j,i]=mean(dark[j,0:ctz-1,i])
  endfor
endfor
free_lun,lunz
endif

; (2) process primary calibration - calibration
if(strcmp(donadir,'yes')) then begin
    filen=primary_nadir+'cal/'
    nspc_files = file_search(filen+data, count = numfiles, /FOLD_CASE)
    if (numfiles ne 1) then message,'There should be exaclt 1 file for the cal.'
    openr,lunn,nspc_files[0],/get_lun
    ctn=0L
    while not eof(lunn) do begin
        readu, lunn, specn
        if(specn.intime3 ne nsi_intime) then message, "NSI int times don't match"
        if(specn.intime4 ne nir_intime) then message, "NIR int times don't match"
        ;;if(specn.intime2 ne nir_intime) then message, "NIR int times don't match"
        cali[*,ctn,2]=specn.nspecsi &  cali[*,ctn,3]=specn.nspecir
        ctn = ctn + 1L
    endwhile
    for j=0,np-1 do begin       ; go through channels
        for i=2,3 do begin      ; go through spectrometers
            calim[j,i]=mean(cali[j,0:ctn-1,i])
        endfor
    endfor
    free_lun,lunn
endif

if(strcmp(dozenith,'yes')) then begin
filez=primary_zenith+'cal/'
zspc_files = file_search(filez+data, count = numfiles, /FOLD_CASE)
if (numfiles ne 1) then message,'There should be exaclt 1 file for the cal.'
openr,lunz,zspc_files[0],/get_lun
ctz=0L
while not eof(lunz) do begin
  readu, lunz, specz
    if(specz.intime1 ne zsi_intime) then message, "ZSI int times don't match"
    if(specz.intime2 ne zir_intime) then message, "ZIR int times don't match"
  ;print,specz.intime1,specz.intime2,specz.intime3,specz.intime4
  cali[*,ctz,0]=specz.zspecsi &  cali[*,ctz,1]=specz.zspecir
  ctz = ctz + 1L
endwhile
for j=0,np-1 do begin ; go through channels
  for i=0,1 do begin ; go through spectrometers
    calim[j,i]=mean(cali[j,0:ctz-1,i])
  endfor
endfor
free_lun,lunz
endif

; (3) read in lamp data
nl=file_lines(resp_func_dir+'/'+nist_traceable_source)
openr,lunl,resp_func_dir+'/'+nist_traceable_source,/get_lun
pl=fltarr(nl) & pi=fltarr(nl)
for i=0,nl-1 do begin
  readf,lunl,a,b
  pl[i]=a & pi[i]=b
endfor

free_lun,lunl

lampzensi = spline(pl,pi,wlvisz) ; spline lamp response to SSFR wavelengths
lampzenir = spline(pl,pi,wlnirz)

lampnadsi = spline(pl,pi,wlvisn) ; spline lamp response to SSFR wavelengths
lampnadir = spline(pl,pi,wlnirn)

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

;;ind=(innp_used-1)-indgen(innp_used)              ; reverse IR wavelengths
;;tm1=dn_1[ind,1] & tm3=dn_1[ind,3]
;;dn_1[*,1]=tm1   & dn_1[*,3]=tm3

;SSFR-7 InGaAs spectrometer wavelengths are NOT reversed in the Zeiss files...

;if(np eq innp_used) then begin
;    dn_1[*,1] = reverse(dn_1[*,1])
;    dn_1[*,3] = reverse(dn_1[*,3])
;endif else begin
;    dn_1[*,1] = [reverse(dn_1[0:innp_used-1,1]),dn_1[innp_used:*,1]]
;    dn_1[*,3] = [reverse(dn_1[0:innp_used-1,3]),dn_1[innp_used:*,3]]
;endelse
resp1[*,0] = dn_1[*,0]/zsi_intime/(lampzensi)
resp1[*,1] = dn_1[*,1]/zir_intime/(lampzenir)

resp1[*,2] = dn_1[*,2]/nsi_intime/(lampnadsi)
resp1[*,3] = dn_1[*,3]/nir_intime/(lampnadir)

; (4) transfer of calibration - darks
if(strcmp(donadir,'yes')) then begin
filen=transfer_nadir+'dark/'
nspc_files = file_search(filen+data, count = numfiles, /FOLD_CASE)
if (numfiles ne 1) then message,'There should be exaclt 1 file for the dark.'
;print, 'Opening ',nspc_files[0]
openr,lunn,nspc_files[0],/get_lun
ctn=0L
while not eof(lunn) do begin
  readu, lunn, specn
    if(specn.intime3 ne nsi_intime) then message, "NSI int times don't match"
    if(specn.intime4 ne nir_intime) then message, "NIR int times don't match"
    ;;if(specn.intime2 ne nir_intime) then message, "NIR int times don't match"
  ;print,specn.intime1,specn.intime2,specn.intime3,specn.intime4
  dark[*,ctn,2]=specn.nspecsi &  dark[*,ctn,3]=specn.nspecir
  ctn = ctn + 1L
endwhile
for j=0,np-1 do begin ; go through channels
  for i=2,3 do begin ; go through spectrometers
    darkm[j,i]=mean(dark[j,0:ctn-1,i])
  endfor
endfor
free_lun,lunn
endif

if(strcmp(dozenith,'yes')) then begin
filez=transfer_zenith+'dark/'
zspc_files = file_search(filez+data, count = numfiles, /FOLD_CASE)
if (numfiles ne 1) then message,'There should be exaclt 1 file for the dark.'
openr,lunz,zspc_files[0],/get_lun
ctz=0L
while not eof(lunz) do begin
  readu, lunz, specz
    if(specz.intime1 ne zsi_intime) then message, "ZSI int times don't match"
    if(specz.intime2 ne zir_intime) then message, "ZIR int times don't match"
  ;print,specz.intime1,specz.intime2,specz.intime3,specz.intime4
  dark[*,ctz,0]=specz.zspecsi &  dark[*,ctz,1]=specz.zspecir
  ctz = ctz + 1L
endwhile
for j=0,np-1 do begin ; go through channels
  for i=0,1 do begin ; go through spectrometers
    darkm[j,i]=mean(dark[j,0:ctz-1,i])
  endfor
endfor

free_lun,lunz
endif

; (5) transfer of calibration - calibration
if(strcmp(donadir,'yes')) then begin
filen=transfer_nadir+'cal/'
nspc_files = file_search(filen+data, count = numfiles, /FOLD_CASE)
if (numfiles ne 1) then message,'There should be exaclt 1 file for the cal.'
openr,lunn,nspc_files[0],/get_lun
ctn=0L
while not eof(lunn) do begin
  readu, lunn, specn
  if(specn.intime3 ne nsi_intime) then message, "NSI int times don't match"
  if(specn.intime4 ne nir_intime) then message, "NIR int times don't match"
  ;;if(specn.intime2 ne nir_intime) then message, "NIR int times don't match"
                                ;print,specn.intime1,specn.intime2,specn.intime3,specn.intime4
  cali[*,ctn,2]=specn.nspecsi &  cali[*,ctn,3]=specn.nspecir
  ctn = ctn + 1L
endwhile
for j=0,np-1 do begin           ; go through channels
    for i=2,3 do begin          ; go through spectrometers
        calim[j,i]=mean(cali[j,0:ctn-1,i])
    endfor
endfor
free_lun,lunn
endif

if(strcmp(dozenith,'yes')) then begin
    filez=transfer_zenith+'cal/'
    zspc_files = file_search(filez+data, count = numfiles, /FOLD_CASE)
    ;;if (numfiles ne 1) then message,'There should be exaclt 1 file for the cal.'
    openr,lunz,zspc_files[0],/get_lun
    ctz=0L
    while not eof(lunz) do begin
        readu, lunz, specz
        if(specz.intime1 ne zsi_intime) then message, "ZSI int times don't match"
        if(specz.intime2 ne zir_intime) then message, "ZIR int times don't match"
                                ;print,specz.intime1,specz.intime2,specz.intime3,specz.intime4
        cali[*,ctz,0]=specz.zspecsi &  cali[*,ctz,1]=specz.zspecir
        ctz = ctz + 1L
    endwhile
    for j=0,np-1 do begin       ; go through channels
        for i=0,1 do begin      ; go through spectrometers
            calim[j,i]=mean(cali[j,0:ctz-1,i])
        endfor
    endfor
    free_lun,lunz
endif

dn_2 = calim - darkm

;;ind=(innp_used-1)-indgen(innp_used)              ; reverse IR wavelengths
;;tm1=dn_2[ind,1] & tm3=dn_2[ind,3]
;;dn_2[*,1]=tm1   & dn_2[*,3]=tm3

;;dn_2[*,1] = reverse(dn_2[*,1])

;SSFR-7 InGaAs spectrometer wavelengths are NOT reversed in the Zeiss files...

;if(np eq innp_used) then begin
;    dn_2[*,1] = reverse(dn_2[*,1])
;    dn_2[*,3] = reverse(dn_2[*,3])
;endif else begin
;    dn_2[*,1] = [reverse(dn_2[0:innp_used-1,1]),dn_2[innp_used:*,1]]
;    dn_2[*,3] = [reverse(dn_2[0:innp_used-1,3]),dn_2[innp_used:*,3]]
;endelse

transfer = dn_1 / dn_2

; (6) transfer of calibration - darks

if(strcmp(donadir,'yes')) then begin
    filen=secondary_nadir+'dark/'
    nspc_files = file_search(filen+data, count = numfiles, /FOLD_CASE)
    if (numfiles ne 1) then message,'There should be exaclt 1 file for the dark.'
    openr,lunn,nspc_files[0],/get_lun
    ctn=0L
    while not eof(lunn) do begin
        readu, lunn, specn
        if(specn.intime3 ne nsi_intime) then message, "NSI int times don't match"
        if(specn.intime4 ne nir_intime) then message, "NIR int times don't match"
        ;;if(specn.intime2 ne nir_intime) then message, "NIR int times don't match"
                                ;print,specn.intime1,specn.intime2,specn.intime3,specn.intime4
        dark[*,ctn,2]=specn.nspecsi &  dark[*,ctn,3]=specn.nspecir
        ctn = ctn + 1L
    endwhile
    for j=0,np-1 do begin       ; go through channels
        for i=2,3 do begin      ; go through spectrometers
            darkm[j,i]=mean(dark[j,0:ctn-1,i])
        endfor
    endfor
    free_lun,lunn
endif

if(strcmp(dozenith,'yes')) then begin
    filez=secondary_zenith+'dark/'
    zspc_files = file_search(filez+data, count = numfiles, /FOLD_CASE)
    if (numfiles ne 1) then message,'There should be exaclt 1 file for the dark.'
    ctz=0L
    openr,lunz,zspc_files[0],/get_lun
    while not eof(lunz) do begin
        readu, lunz, specz
        if(specz.intime1 ne zsi_intime) then message, "ZSI int times don't match"
        if(specz.intime2 ne zir_intime) then message, "ZIR int times don't match"
        dark[*,ctz,0]=specz.zspecsi &  dark[*,ctz,1]=specz.zspecir
        ctz = ctz + 1L
    endwhile
    for j=0,np-1 do begin       ; go through channels
        for i=0,1 do begin      ; go through spectrometers
            darkm[j,i]=mean(dark[j,0:ctz-1,i])
        endfor
    endfor
    free_lun,lunz
endif

; (7) transfer of calibration - calibration
if(strcmp(donadir,'yes')) then begin
    filen=secondary_nadir+'cal/'
    nspc_files = file_search(filen+data, count = numfiles, /FOLD_CASE)
    if (numfiles ne 1) then message,'There should be exaclt 1 file for the cal.'
    openr,lunn,nspc_files[0],/get_lun
    ctn=0L
    while not eof(lunn) do begin
        readu, lunn, specn
        if(specn.intime3 ne nsi_intime) then message, "NSI int times don't match"
        if(specn.intime4 ne nir_intime) then message, "NIR int times don't match"
        ;;if(specn.intime2 ne nir_intime) then message, "NIR int times don't match"
                                ;print,specn.intime1,specn.intime2,specn.intime3,specn.intime4
        cali[*,ctn,2]=specn.nspecsi &  cali[*,ctn,3]=specn.nspecir
        ctn = ctn + 1L
    endwhile
    for j=0,np-1 do begin       ; go through channels
        for i=2,3 do begin      ; go through spectrometers
            calim[j,i]=mean(cali[j,0:ctn-1,i])
        endfor
    endfor
    free_lun,lunn
endif

if(strcmp(dozenith,'yes')) then begin
    filez=secondary_zenith+'cal/'
    zspc_files = file_search(filez+data, count = numfiles, /FOLD_CASE)
    ;;if (numfiles ne 1) then message,'There should be exaclt 1 file for the cal.'
    openr,lunz,zspc_files[0],/get_lun
    ctz=0L
    while not eof(lunz) do begin
        readu, lunz, specz
        if(specz.intime1 ne zsi_intime) then message, "ZSI int times don't match"
        if(specz.intime2 ne zir_intime) then message, "ZIR int times don't match"
        cali[*,ctz,0]=specz.zspecsi &  cali[*,ctz,1]=specz.zspecir
        ctz = ctz + 1L
    endwhile
    for j=0,np-1 do begin       ; go through channels
        for i=0,1 do begin      ; go through spectrometers
            calim[j,i]=mean(cali[j,0:ctz-1,i])
        endfor
    endfor
    free_lun,lunz
endif

dn2 = calim - darkm

;ind=(innp_used-1)-indgen(innp_used)              ; reverse IR wavelengths
;tm1=dn2[ind,1] & tm3=dn2[ind,3]
;dn2[*,1]=tm1   & dn2[*,3]=tm3 

;;dn2[*,1] = reverse(dn2[*,1])

;SSFR-7 InGaAs spectrometer wavelengths are NOT reversed in the Zeiss files...
;if(np eq innp_used) then begin
;    dn2[*,1] = reverse(dn2[*,1])
;    dn2[*,3] = reverse(dn2[*,3])
;endif else begin
;    dn2[*,1] = [reverse(dn2[0:innp_used-1,1]),dn2[innp_used:*,1]]
;    dn2[*,3] = [reverse(dn2[0:innp_used-1,3]),dn2[innp_used:*,3]]
;endelse

if(strcmp(dozenith,'yes')) then begin
    resp2[*,0] = dn2[*,0]/zsi_intime/(lampzensi)*transfer[*,0]
    resp2[*,1] = dn2[*,1]/zir_intime/(lampzenir)*transfer[*,1]
    if(strcmp(write_primary,'yes')) then begin
        resp1_si_file = resp_func_dir + primary_cal_date + '_'+primary_lamp+'_resp1_zensi.dat'
        resp1_ir_file = resp_func_dir + primary_cal_date + '_'+primary_lamp+'_resp1_zenir.dat'
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
endif

if(strcmp(donadir,'yes')) then begin
    resp2[*,2] = dn2[*,2]/nsi_intime/(lampnadsi)*transfer[*,2]
    resp2[*,3] = dn2[*,3]/nir_intime/(lampnadir)*transfer[*,3]
    if(strcmp(write_primary,'yes')) then begin
        
        resp1_si_file = resp_func_dir + primary_cal_date + '_'+primary_lamp+'_resp1_nadsi.dat'
        resp1_ir_file = resp_func_dir + primary_cal_date + '_'+primary_lamp+'_resp1_nadir.dat'

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

    resp2_si_file = resp_func_dir + secondary_cal_date + '_'+lamp+'_resp2_zensi.dat'
    resp2_ir_file = resp_func_dir + secondary_cal_date + '_'+lamp+'_resp2_zenir.dat'

    tmp=file_search(resp2_si_file,count=count)
    if(count gt 0) then message, resp2_si_file + ' already exists'
    tmp=file_search(resp2_ir_file,count=count)
    if(count gt 0) then message, resp2_ir_file + ' already exists'

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

    resp2_si_file = resp_func_dir + secondary_cal_date + '_'+lamp+'_resp2_nadsi.dat'
    resp2_ir_file = resp_func_dir + secondary_cal_date + '_'+lamp+'_resp2_nadir.dat'

    tmp=file_search(resp2_si_file,count=count)
    if(count gt 0) then message, resp2_si_file + ' already exists'
    tmp=file_search(resp2_ir_file + ' already exists')
    if(count gt 0) then message, resp2_ir_file + ' already exists'

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
    ymax = max([max(resp1(*,0)),max(resp1(*,1)),max(resp2(*,0)),max(resp2(*,1))])*1.04
    ymax = 1000.
    plot, wlvisz,resp1(*,0),color=3,ytitle='DN W!E-1!N m!E2!N nm',xtitle='Wavelength (nm)',title='Zenith',charsize=1.5,thick=1.8,/xs,/ys,xrange=[300,1700],yrange=[0,ymax]
    oplot,wlnirz,resp1(*,1),thick=1.8,color=3
    oplot,wlvisz,resp2(*,0),thick=1.8,color=110
    oplot,wlnirz,resp2(*,1),thick=1.8,color=110
    legend,['Primary Cal - '+primary_cal_date,'Secondary Cal - '+secondary_cal_date],textcolors=[3,110],/right,outline_color=3
endif

stop
if(strcmp(donadir,'yes')) then begin
    ymax = max([max(resp1(*,2)),max(resp1(*,3)),max(resp2(*,2)),max(resp2(*,3))])*1.04
    plot, wlvisn,resp1(*,2),color=3,ytitle='DN W!E-1!N m!E2!N nm',xtitle='Wavelength (nm)',title='Nadir',charsize=1.5,thick=1.8,/xs,/ys,xrange=[300,2200],yrange=[0,ymax]
    oplot,wlnirn,resp1(*,3),thick=1.8,color=3
    oplot,wlvisn,resp2(*,2),thick=1.8,color=110
    oplot,wlnirn,resp2(*,3),thick=1.8,color=110
    legend,['Primary Cal - '+primary_cal_date,'Secondary Cal - '+secondary_cal_date],textcolors=[3,110],/right,outline_color=3
endif
stop
end
