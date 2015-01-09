;+
; NAME:
;   calnex_cal
;
; PURPOSE:
;   Read in the data for calibrations, make response functions from primary calibration and secondary
;
; CATEGORY:
;   CALNEX / Calibration
;
; CALLING SEQUENCE:
;   calnex_cal
;   
; OUTPUT:
;   
;
; KEYWORDS:
;   SSFR, P3, CALNEX, Calibration
;
; DEPENDENCIES:
;   cfg.pro       ;config file cheker
;   calnex_dark.pro  ;dark spectra finder (procedure ini_dark and read_drk)
;   legend.pro ; to make legend on graph
;   
; NEEDED FILES:
;   - config file for calib
;   - all ".OSA2" files for the calibs under the proper directory (/calnex/cal/*date*/p3/(*lamp number*)/(nadir or zenith/(cal or dark))
;  
; EXAMPLE:
;   calnex_cal
;
; MODIFICATION HISTORY:
; Written:  Sebastian Schmidtc, LASP CU Boulder, date unknown
; Modified: Wedenesday, Cinco de Mayo, 2010
;          -by Samuel LeBlanc
;           Added comments   
;           Added Multiple calibration date plotting and reading from cfg file
; Modified: Thursday, May 20th, 2010
;	   -by Samuel LeBlanc
;	    Added Temperature measurements for most recent cals and print to screen
;           Added call to plotting of percent differences with addition to cfg file of reference calibration date
;---------------------------------------------------------------------------
@cfg.pro
@legend.pro
@calnex_compare_cal.pro
@errploty.pro

pro calnex_cal

path='/data/seven/schmidt/calnex/cal'
l   ='/'                         ; directory separator
;cfg_file=path+l+ 'noaap3_997.cfg';'post_cal.cfg'
cfg_file=path+l+ 'post_cal_orientation2.cfg';'post_cal.cfg'
set_plot, 'x'
device,decomposed=0
loadct,39
  tvlct, r,g,bleu, /get
  r=reverse(r) & g=reverse(g) & bleu=reverse(bleu)
  tvlct,r,g,bleu
!P.multi=0
if file_test(cfg_file) ne 1 then message,'Did not find configuration file.'

months = ["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"]

np                    = fix(cfg(cfg_file,'np'  ))    ; number of channels for each spectrum
innp_used             = fix(cfg(cfg_file,'innp_used'))    ; number of channels used in the IR

platform              = cfg(cfg_file,'platform')
data                  = cfg(cfg_file,'data')    ; data file extension

nist_traceable_source = cfg(cfg_file,'nist_traceable_source')
primary_lamp          = strsplit(cfg(cfg_file,'primary_lamp') ,', ',escape='#',/extract)	
lamp                  = strsplit(cfg(cfg_file,'lamp') , ', ',escape='#',/extract) ; array of lamps used in calibrations

primary_cal_date      = strsplit(cfg(cfg_file,'primary_cal_date') ,', ',escape='#',/extract)	; array of primary lamp
secondary_cal_date    = strsplit(cfg(cfg_file,'secondary_cal_date') ,', ',escape='#',/extract)	; to make an array of secondary cals.

ref = cfg(cfg_file,'reference_date')

lamp = lamp[sort(secondary_cal_date)]

num_secondary = n_elements(secondary_cal_date)
if num_secondary gt 1 then print, 'Multiple secondary Calibration dates, Plotting all for comparison, Last cal date will be used for response function'

; read file location for primary, transfer, and integration times
primary_nadir_si      = strsplit(cfg(cfg_file,'primary_nadir_si')   ,', ',escape='#',/EXTRACT)
primary_nadir_ir      = strsplit(cfg(cfg_file,'primary_nadir_ir')   ,', ',escape='#',/EXTRACT)
primary_zenith_si     = strsplit(cfg(cfg_file,'primary_zenith_si')  ,', ',escape='#',/EXTRACT)
primary_zenith_ir     = strsplit(cfg(cfg_file,'primary_zenith_ir')  ,', ',escape='#',/EXTRACT)

transfer_nadir_si     = strsplit(cfg(cfg_file,'transfer_nadir_si')  ,', ',escape='#',/EXTRACT)
transfer_nadir_ir     = strsplit(cfg(cfg_file,'transfer_nadir_ir')  ,', ',escape='#',/EXTRACT)
transfer_zenith_si    = strsplit(cfg(cfg_file,'transfer_zenith_si') ,', ',escape='#',/EXTRACT)
transfer_zenith_ir    = strsplit(cfg(cfg_file,'transfer_zenith_ir') ,', ',escape='#',/EXTRACT)

secondary_nadir_si    = strsplit(cfg(cfg_file,'secondary_nadir_si') ,', ',escape='#',/EXTRACT)	; to make an array of secondary cals.
secondary_nadir_ir    = strsplit(cfg(cfg_file,'secondary_nadir_ir') ,', ',escape='#',/EXTRACT)
secondary_zenith_si   = strsplit(cfg(cfg_file,'secondary_zenith_si'),', ',escape='#',/EXTRACT)
secondary_zenith_ir   = strsplit(cfg(cfg_file,'secondary_zenith_ir'),', ',escape='#',/EXTRACT)

if n_elements(transfer_nadir_si) ne n_elements(secondary_nadir_si) then message, 'not the same number of transfer path and secondary cal paths'
if n_elements(primary_nadir_si) ne n_elements(secondary_nadir_si) then message, 'not the same number of primary path and secondary cal paths'

zsi_intime_p=intarr(num_secondary)
zir_intime_p=intarr(num_secondary)
nsi_intime_p=intarr(num_secondary)
nir_intime_p=intarr(num_secondary)

for ii=0, num_secondary-1 do begin
  zsi_intime_p[ii] = fix(primary_zenith_si[2*ii+1])  & primary_zenith_si[ii] = primary_zenith_si[2*ii]
  zir_intime_p[ii] = fix(primary_zenith_ir[2*ii+1])  & primary_zenith_ir[ii] = primary_zenith_ir[2*ii]
  nsi_intime_p[ii] = fix(primary_nadir_si[2*ii+1])   & primary_nadir_si[ii]  = primary_nadir_si[2*ii]
  nir_intime_p[ii] = fix(primary_nadir_ir[2*ii+1])   & primary_nadir_ir[ii]  = primary_nadir_ir[2*ii]
endfor

primary_zenith_si=primary_zenith_si[0:num_secondary-1]
primary_zenith_ir=primary_zenith_ir[0:num_secondary-1]
primary_nadir_si=primary_nadir_si[0:num_secondary-1]
primary_nadir_ir=primary_nadir_ir[0:num_secondary-1]

zsi_intime_t=intarr(num_secondary)
zir_intime_t=intarr(num_secondary)
nsi_intime_t=intarr(num_secondary)
nir_intime_t=intarr(num_secondary)

for ii=0, num_secondary-1 do begin
  zsi_intime_t[ii] = fix(transfer_zenith_si[2*ii+1]) & transfer_zenith_si[ii]= transfer_zenith_si[2*ii]
  zir_intime_t[ii] = fix(transfer_zenith_ir[2*ii+1]) & transfer_zenith_ir[ii]= transfer_zenith_ir[2*ii]
  nsi_intime_t[ii] = fix(transfer_nadir_si[2*ii+1])  & transfer_nadir_si[ii] = transfer_nadir_si[2*ii]
  nir_intime_t[ii] = fix(transfer_nadir_ir[2*ii+1])  & transfer_nadir_ir[ii] = transfer_nadir_ir[2*ii]
endfor

transfer_zenith_si=transfer_zenith_si[0:num_secondary-1]
transfer_zenith_ir=transfer_zenith_ir[0:num_secondary-1]
transfer_nadir_si=transfer_nadir_si[0:num_secondary-1]
transfer_nadir_ir=transfer_nadir_ir[0:num_secondary-1]

zsi_intime_s=intarr(num_secondary)
zir_intime_s=intarr(num_secondary)
nsi_intime_s=intarr(num_secondary)
nir_intime_s=intarr(num_secondary)

for ii=0, num_secondary-1 do begin
  zsi_intime_s[ii] = fix(secondary_zenith_si[2*ii+1]) & secondary_zenith_si[ii]= secondary_zenith_si[2*ii]
  zir_intime_s[ii] = fix(secondary_zenith_ir[2*ii+1]) & secondary_zenith_ir[ii]= secondary_zenith_ir[2*ii]
  nsi_intime_s[ii] = fix(secondary_nadir_si[2*ii+1])  & secondary_nadir_si[ii] = secondary_nadir_si[2*ii]
  nir_intime_s[ii] = fix(secondary_nadir_ir[2*ii+1])  & secondary_nadir_ir[ii] = secondary_nadir_ir[2*ii]
endfor

secondary_zenith_si=secondary_zenith_si[0:num_secondary-1]
secondary_zenith_ir=secondary_zenith_ir[0:num_secondary-1]
secondary_nadir_si=secondary_nadir_si[0:num_secondary-1]
secondary_nadir_ir=secondary_nadir_ir[0:num_secondary-1]

; sort all the secondary file path and integration time to match secondary date array
; zsi_intime_p = zsi_intime_p[sort(secondary_cal_date)] & primary_zenith_si = primary_zenith_si[sort(secondary_cal_date)]
; zir_intime_p = zir_intime_p[sort(secondary_cal_date)] & primary_zenith_ir = primary_zenith_ir[sort(secondary_cal_date)]
; nsi_intime_p =  nsi_intime_p[sort(secondary_cal_date)] & primary_nadir_si = primary_nadir_si[sort(secondary_cal_date)]
; nir_intime_p =  nir_intime_p[sort(secondary_cal_date)] & primary_nadir_ir = primary_nadir_ir[sort(secondary_cal_date)]

; zsi_intime_s = zsi_intime_s[sort(secondary_cal_date)] & secondary_zenith_si = secondary_zenith_si[sort(secondary_cal_date)]
; zir_intime_s = zir_intime_s[sort(secondary_cal_date)] & secondary_zenith_ir = secondary_zenith_ir[sort(secondary_cal_date)]
; nsi_intime_s =  nsi_intime_s[sort(secondary_cal_date)] & secondary_nadir_si = secondary_nadir_si[sort(secondary_cal_date)]
; nir_intime_s =  nir_intime_s[sort(secondary_cal_date)] & secondary_nadir_ir = secondary_nadir_ir[sort(secondary_cal_date)]

; zsi_intime_t = zsi_intime_t[sort(secondary_cal_date)] & transfer_zenith_si = transfer_zenith_si[sort(secondary_cal_date)]
; zir_intime_t = zir_intime_t[sort(secondary_cal_date)] & transfer_zenith_ir = transfer_zenith_ir[sort(secondary_cal_date)]
; nsi_intime_t =  nsi_intime_t[sort(secondary_cal_date)] & transfer_nadir_si = transfer_nadir_si[sort(secondary_cal_date)]
; nir_intime_t =  nir_intime_t[sort(secondary_cal_date)] & transfer_nadir_ir = transfer_nadir_ir[sort(secondary_cal_date)]

; secondary_cal_date = secondary_cal_date[sort(secondary_cal_date)]	; sort in order of date

ref=where(ref eq secondary_cal_date)	;find where the secondary cal date is equal to the reference date
ref=ref[0]

for ii=0, num_secondary-1 do begin
if zsi_intime_p[ii] ne zsi_intime_t[ii] then message,'Primary and Transfer integration times are not equal (ZSI).'
if zir_intime_p[ii] ne zir_intime_t[ii] then message,'Primary and Transfer integration times are not equal (ZIR).'
if nsi_intime_p[ii] ne nsi_intime_t[ii] then message,'Primary and Transfer integration times are not equal (NSI).'
if nir_intime_p[ii] ne nir_intime_t[ii] then message,'Primary and Transfer integration times are not equal (NIR).'
endfor

resp_func_dir = cfg(cfg_file,'resp_func_dir')+l

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

resp1_arr=fltarr(np,4,num_secondary) ; array of primary response functions
transfer_arr=fltarr(np,4,num_secondary) ; array of transfers
resp2_arr=fltarr(np,4,num_secondary) ; array of desired response functions for plotting

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Temperature
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Read temperature coefficients
temp   =cfg(cfg_file,'thermistor')
npt  = file_lines(temp)
tdat = fltarr(2, npt)   & revt = fltarr(2, npt)
openr,ut,temp, /get_lun & readf,ut,tdat & free_lun,ut
for i = 0, npt - 1 do begin
    revt[0, i] = tdat[0, npt -i - 1] & revt[1, i] = tdat[1, npt -i - 1]
endfor
rt    = findgen(20000)/10000. + 0. ; interpolate thermistor data
revts = spline(revt(1, *), revt(0, *), rt) ; at fine resolution

nirxt   = fltarr(bignum)        ; temperature housing nadir NIR 
zirxt   = fltarr(bignum)        ; temperature housing zenith NIR

temps   = fltarr(bignum)        ; dummy temperature array

temp_dark_prim = fltarr(4,num_secondary)
temp_cal_prim  = fltarr(4,num_secondary)
temp_dark_tra  = fltarr(4,num_secondary)
temp_cal_tra  = fltarr(4,num_secondary)
temp_dark_sec  = fltarr(4,num_secondary)
temp_cal_sec   = fltarr(4,num_secondary)

temp_dark_prim_st = fltarr(4,num_secondary)
temp_cal_prim_st  = fltarr(4,num_secondary)
temp_dark_tra_st  = fltarr(4,num_secondary)
temp_cal_tra_st   = fltarr(4,num_secondary)
temp_dark_sec_st  = fltarr(4,num_secondary)
temp_cal_sec_st   = fltarr(4,num_secondary)

four=' 0 - ZSI, 1 - ZIR, 2 - NSI, 3 - NIR'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PRIMARY CALIBRATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

for gnarr=0, num_secondary-1 do begin
; (1) process primary calibration - darks
specn = {btime: lonarr(2), bcdtimstp:  bytarr(12),intime1: long(0),$
                     intime2: long(0), intime3: long(0), intime4: long(0),accum: long(0), shsw:long(0),$
                     zsit: ulong(0),nsit: ulong(0),zirt: ulong(0),nirt: ulong(0), zirx: ulong(0), nirx: ulong(0),xt: ulong(0),it: ulong(0),$
                     zspecsi: intarr(np), zspecir: intarr(np), nspecsi: intarr(np), nspecir: intarr(np)}   
                     
if(strcmp(donadir,'yes')) then begin
    filen=primary_nadir_si[gnarr]+'dark'+l
    nspc_files = file_search(filen+data, count = numfiles, /FOLD_CASE)
    if (numfiles ne 1) then message,'There should be exactly 1 file for the dark.'
    openr,lunn,nspc_files[0],/get_lun
    ctn=0L
    while not eof(lunn) do begin
        readu, lunn, specn
        if(specn.intime3 ne nsi_intime_p[gnarr]) then message, "NSI int times don't match"
        dark[*,ctn,2]=specn.nspecsi
        ;;Temperature determining
        ch1 = ((specn.nsit/2048.)*5.) - 5.   ;Nadir SI spect temp.
        nsit = long(ch1 * 10000.)     ;convert to integer for use as index
        if (nsit le 19999) then temps[ctn] = revts[nsit]       ;load nadir temp into array
        ctn = ctn + 1L
    endwhile
    temp_dark_prim[2,gnarr]=mean(temps[0:ctn])
    temp_dark_prim_st[2,gnarr]=stddev(temps[0:ctn])
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
    for j=0,np-1 do begin       ; go through channels
        darkm[j,2]=mean(dark[j,0:ctn-1,2])
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
        dark[*,ctn,3]=specn.nspecir
        ;; temperature determining 
        ch3 = ((specn.nirt/2048.)*5.) - 5.   ;Nadir InGaAs temp.
        temps[ctn] = ((1./3200.)*alog(20.*ch3) + (1./298.))^(-1)-273. ;nad ir tmp
        ctn = ctn + 1L
    endwhile
    temp_dark_prim[3,gnarr]=mean(temps[0:ctn])
    temp_dark_prim_st[3,gnarr]=stddev(temps[0:ctn])
    for j=0,np-1 do begin       ; go through channels
        darkm[j,3]=mean(dark[j,0:ctn-1,3])
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
specz = {btime: lonarr(2), bcdtimstp:  bytarr(12),intime1: long(0),$
                     intime2: long(0), intime3: long(0), intime4: long(0),accum: long(0), shsw:long(0),$
                     zsit: ulong(0),nsit: ulong(0),zirt: ulong(0),nirt: ulong(0), zirx: ulong(0), nirx: ulong(0),xt: ulong(0),it: ulong(0),$
                     zspecsi: intarr(np), zspecir: intarr(np), nspecsi: intarr(np), nspecir: intarr(np)}
while not eof(lunz) do begin
    readu, lunz, specz
    if(specz.intime1 ne zsi_intime_p[gnarr]) then message, "ZSI int times don't match"
    dark[*,ctz,0]=specz.zspecsi
    ;;Temperature determining
    ch0 = ((specz.zsit/2048.)*5.) - 5.   ;Nadir SI spect temp.
    zsit = long(ch0 * 10000.)     ;convert to integer for use as index
    if (zsit le 19999) then temps[ctz] = revts[zsit]       ;load nadir temp into array
    ctz = ctz + 1L
endwhile
temp_dark_prim[0,gnarr]=mean(temps[0:ctz])
temp_dark_prim_st[0,gnarr]=stddev(temps[0:ctz])
for j=0,np-1 do begin ; go through channels
    darkm[j,0]=mean(dark[j,0:ctz-1,0])
endfor
free_lun,lunz
;
filez=primary_zenith_ir[gnarr]+'dark'+l
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
    if(specz.intime2 ne zir_intime_p[gnarr]) then message, "ZIR int times don't match"
    dark[*,ctz,1]=specz.zspecir
    ;; temperature determining 
    ch2 = ((specz.zirt/2048.)*5.) - 5.   ;Zenith InGaAs temp.
    temps[ctz] = ((1./3200.)*alog(20.*ch2) + (1./298.))^(-1)-273. ;zen ir tmp
    ctz = ctz + 1L
endwhile
temp_dark_prim[1,gnarr]=mean(temps[0:ctz])
temp_dark_prim_st[1,gnarr]=stddev(temps[0:ctz])
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
    filen=primary_nadir_si[gnarr]+'cal'+l
    nspc_files = file_search(filen+data, count = numfiles, /FOLD_CASE)
    if (numfiles ne 1) then message,'There should be exactly 1 file for the dark.'
    openr,lunn,nspc_files[0],/get_lun
    ctn=0L
    while not eof(lunn) do begin
        readu, lunn, specn
        if(specn.intime3 ne nsi_intime_p[gnarr]) then message, "NSI int times don't match"
        cali[*,ctn,2]=specn.nspecsi
        ;;Temperature determining
        ch1 = ((specn.nsit/2048.)*5.) - 5.   ;Nadir SI spect temp.
        nsit = long(ch1 * 10000.)     ;convert to integer for use as index
        if (nsit le 19999) then temps[ctn] = revts[nsit]       ;load nadir temp into array
        ctn = ctn + 1L
    endwhile
    temp_cal_prim[2,gnarr]=mean(temps[0:ctn])
    temp_cal_prim_st[2,gnarr]=stddev(temps[0:ctn])
    for j=0,np-1 do begin       ; go through channels
        calim[j,2]=mean(cali[j,0:ctn-1,2])
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
        cali[*,ctn,3]=specn.nspecir
        ;; temperature determining 
        ch3 = ((specn.nirt/2048.)*5.) - 5.   ;Nadir InGaAs temp.
        temps[ctn] = ((1./3200.)*alog(20.*ch3) + (1./298.))^(-1)-273. ;nad ir tmp
        ctn = ctn + 1L
    endwhile
    temp_cal_prim[3,gnarr]=mean(temps[0:ctn])
    temp_cal_prim_st[3,gnarr]=stddev(temps[0:ctn])
    for j=0,np-1 do begin       ; go through channels
        calim[j,3]=mean(cali[j,0:ctn-1,3])
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
specz = {btime: lonarr(2), bcdtimstp:  bytarr(12),intime1: long(0),$
                     intime2: long(0), intime3: long(0), intime4: long(0),accum: long(0), shsw:long(0),$
                     zsit: ulong(0),nsit: ulong(0),zirt: ulong(0),nirt: ulong(0), zirx: ulong(0), nirx: ulong(0),xt: ulong(0),it: ulong(0),$
                     zspecsi: intarr(np), zspecir: intarr(np), nspecsi: intarr(np), nspecir: intarr(np)}
while not eof(lunz) do begin
    readu, lunz, specz
    if(specz.intime1 ne zsi_intime_p[gnarr]) then message, "ZSI int times don't match"
    cali[*,ctz,0]=specz.zspecsi
    ;;Temperature determining
    ch0 = ((specz.zsit/2048.)*5.) - 5.   ;Nadir SI spect temp.
    zsit = long(ch0 * 10000.)     ;convert to integer for use as index
    if (zsit le 19999) then temps[ctz] = revts[zsit]       ;load nadir temp into array
    ctz = ctz + 1L
endwhile
temp_cal_prim[0,gnarr]=mean(temps[0:ctz])
temp_cal_prim_st[0,gnarr]=stddev(temps[0:ctz])
for j=0,np-1 do begin ; go through channels
    calim[j,0]=mean(cali[j,0:ctz-1,0])
endfor
free_lun,lunz
;
filez=primary_zenith_ir[gnarr]+'cal'+l
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
    if(specz.intime2 ne zir_intime_p[gnarr]) then message, "ZIR int times don't match"
    cali[*,ctz,1]=specz.zspecir
    ;; temperature determining 
    ch2 = ((specz.zirt/2048.)*5.) - 5.   ;Zenith InGaAs temp.
    temps[ctz] = ((1./3200.)*alog(20.*ch2) + (1./298.))^(-1)-273. ;zen ir tmp
    ctz = ctz + 1L
endwhile
temp_cal_prim[1,gnarr]=mean(temps[0:ctz])
temp_cal_prim_st[1,gnarr]=stddev(temps[0:ctz])
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
resp1[*,0] = dn_1[*,0]/float(zsi_intime_p[gnarr])/(lampzensi)
resp1[*,1] = dn_1[*,1]/float(zir_intime_p[gnarr])/(lampzenir)
resp1_arr[*,0,gnarr]=resp1[*,0]
resp1_arr[*,1,gnarr]=resp1[*,1]

resp1[*,2] = dn_1[*,2]/float(nsi_intime_p[gnarr])/(lampnadsi)
resp1[*,3] = dn_1[*,3]/float(nir_intime_p[gnarr])/(lampnadir)
resp1_arr[*,2,gnarr]=resp1[*,2]
resp1_arr[*,3,gnarr]=resp1[*,3]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TRANSFER CALIBRATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; (4) transfer of calibration - darks
specn = {btime: lonarr(2), bcdtimstp:  bytarr(12),intime1: long(0),$
                     intime2: long(0), intime3: long(0), intime4: long(0),accum: long(0), shsw:long(0),$
                     zsit: ulong(0),nsit: ulong(0),zirt: ulong(0),nirt: ulong(0), zirx: ulong(0), nirx: ulong(0),xt: ulong(0),it: ulong(0),$
                     zspecsi: intarr(np), zspecir: intarr(np), nspecsi: intarr(np), nspecir: intarr(np)}
if(strcmp(donadir,'yes')) then begin
    filen=transfer_nadir_si[gnarr]+'dark'+l
    nspc_files = file_search(filen+data, count = numfiles, /FOLD_CASE)
    if (numfiles ne 1) then message,'There should be exactly 1 file for the dark.'
    openr,lunn,nspc_files[0],/get_lun
    ctn=0L
    while not eof(lunn) do begin
        readu, lunn, specn
        if(specn.intime3 ne nsi_intime_t[gnarr]) then message, "NSI int times don't match"
        dark[*,ctn,2]=specn.nspecsi
        ;;Temperature determining
        ch1 = ((specn.nsit/2048.)*5.) - 5.   ;Nadir SI spect temp.
        nsit = long(ch1 * 10000.)     ;convert to integer for use as index
        if (nsit le 19999) then temps[ctn] = revts[nsit]       ;load nadir temp into array
        ctn = ctn + 1L
    endwhile
    temp_dark_tra[2,gnarr]=mean(temps[0:ctn])
    temp_dark_tra_st[2,gnarr]=stddev(temps[0:ctn])
    for j=0,np-1 do begin       ; go through channels
        darkm[j,2]=mean(dark[j,0:ctn-1,2])
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
        dark[*,ctn,3]=specn.nspecir
        ;; temperature determining 
        ch3 = ((specn.nirt/2048.)*5.) - 5.   ;Nadir InGaAs temp.
        temps[ctn] = ((1./3200.)*alog(20.*ch3) + (1./298.))^(-1)-273. ;nad ir tmp
        ctn = ctn + 1L
    endwhile
    temp_dark_tra[3,gnarr]=mean(temps[0:ctn])
    temp_dark_tra_st[3,gnarr]=stddev(temps[0:ctn])
    for j=0,np-1 do begin       ; go through channels
        darkm[j,3]=mean(dark[j,0:ctn-1,3])
    endfor
    free_lun,lunn
endif
;
if(strcmp(dozenith,'yes')) then begin
filez=transfer_zenith_si[gnarr]+'dark'+l
zspc_files = file_search(filez+data, count = numfiles, /FOLD_CASE)
if (numfiles ne 1) then message,'There should be exactly 1 file for the dark.'
openr,lunz,zspc_files[0],/get_lun
ctz=0L
specz = {btime: lonarr(2), bcdtimstp:  bytarr(12),intime1: long(0),$
                     intime2: long(0), intime3: long(0), intime4: long(0),accum: long(0), shsw:long(0),$
                     zsit: ulong(0),nsit: ulong(0),zirt: ulong(0),nirt: ulong(0), zirx: ulong(0), nirx: ulong(0),xt: ulong(0),it: ulong(0),$
                     zspecsi: intarr(np), zspecir: intarr(np), nspecsi: intarr(np), nspecir: intarr(np)}
while not eof(lunz) do begin
    readu, lunz, specz
    if(specz.intime1 ne zsi_intime_t[gnarr]) then message, "ZSI int times don't match"
    dark[*,ctz,0]=specz.zspecsi
    ;;Temperature determining
    ch0 = ((specz.zsit/2048.)*5.) - 5.   ;Nadir SI spect temp.
    zsit = long(ch0 * 10000.)     ;convert to integer for use as index
    if (zsit le 19999) then temps[ctz] = revts[zsit]       ;load nadir temp into array
    ctz = ctz + 1L
endwhile
temp_dark_tra[0,gnarr]=mean(temps[0:ctz])
temp_dark_tra_st[0,gnarr]=stddev(temps[0:ctz])
for j=0,np-1 do begin ; go through channels
    darkm[j,0]=mean(dark[j,0:ctz-1,0])
endfor
free_lun,lunz
;
filez=transfer_zenith_ir[gnarr]+'dark'+l
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
    if(specz.intime2 ne zir_intime_t[gnarr]) then message, "ZIR int times don't match"
    dark[*,ctz,1]=specz.zspecir
    ;; temperature determining 
    ch2 = ((specz.zirt/2048.)*5.) - 5.   ;Zenith InGaAs temp.
    temps[ctz] = ((1./3200.)*alog(20.*ch2) + (1./298.))^(-1)-273. ;zen ir tmp
    ctz = ctz + 1L
endwhile
temp_dark_tra[1,gnarr]=mean(temps[0:ctz])
temp_dark_tra_st[1,gnarr]=stddev(temps[0:ctz])
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
    filen=transfer_nadir_si[gnarr]+'cal'+l
    nspc_files = file_search(filen+data, count = numfiles, /FOLD_CASE)
    if (numfiles ne 1) then message,'There should be exactly 1 file for the dark.'
    openr,lunn,nspc_files[0],/get_lun
    ctn=0L
    while not eof(lunn) do begin
        readu, lunn, specn
        if(specn.intime3 ne nsi_intime_t[gnarr]) then message, "NSI int times don't match"
        cali[*,ctn,2]=specn.nspecsi
        ;;Temperature determining
        ch1 = ((specn.nsit/2048.)*5.) - 5.   ;Nadir SI spect temp.
        nsit = long(ch1 * 10000.)     ;convert to integer for use as index
        if (nsit le 19999) then temps[ctn] = revts[nsit]       ;load nadir temp into array
        ctn = ctn + 1L
    endwhile
    temp_cal_tra[2,gnarr]=mean(temps[0:ctn])
    temp_cal_tra_st[2,gnarr]=stddev(temps[0:ctn])
    for j=0,np-1 do begin       ; go through channels
        calim[j,2]=mean(cali[j,0:ctn-1,2])
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
        cali[*,ctn,3]=specn.nspecir
        ;; temperature determining 
        ch3 = ((specn.nirt/2048.)*5.) - 5.   ;Nadir InGaAs temp.
        temps[ctn] = ((1./3200.)*alog(20.*ch3) + (1./298.))^(-1)-273. ;nad ir tmp
        ctn = ctn + 1L
    endwhile
    temp_cal_tra[3,gnarr]=mean(temps[0:ctn])
    temp_cal_tra_st[3,gnarr]=stddev(temps[0:ctn])
    for j=0,np-1 do begin       ; go through channels
        calim[j,3]=mean(cali[j,0:ctn-1,3])
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
specz = {btime: lonarr(2), bcdtimstp:  bytarr(12),intime1: long(0),$
                     intime2: long(0), intime3: long(0), intime4: long(0),accum: long(0), shsw:long(0),$
                     zsit: ulong(0),nsit: ulong(0),zirt: ulong(0),nirt: ulong(0), zirx: ulong(0), nirx: ulong(0),xt: ulong(0),it: ulong(0),$
                     zspecsi: intarr(np), zspecir: intarr(np), nspecsi: intarr(np), nspecir: intarr(np)}
while not eof(lunz) do begin
    readu, lunz, specz
    if(specz.intime1 ne zsi_intime_t[gnarr]) then message, "ZSI int times don't match"
    cali[*,ctz,0]=specz.zspecsi
    ;;Temperature determining
    ch0 = ((specz.zsit/2048.)*5.) - 5.   ;Nadir SI spect temp.
    zsit = long(ch0 * 10000.)     ;convert to integer for use as index
    if (zsit le 19999) then temps[ctz] = revts[zsit]       ;load nadir temp into array
    ctz = ctz + 1L
endwhile
temp_cal_tra[0,gnarr]=mean(temps[0:ctz])
temp_cal_tra_st[0,gnarr]=stddev(temps[0:ctz])
for j=0,np-1 do begin ; go through channels
    calim[j,0]=mean(cali[j,0:ctz-1,0])
endfor
free_lun,lunz
;
filez=transfer_zenith_ir[gnarr]+'cal'+l
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
    if(specz.intime2 ne zir_intime_t[gnarr]) then message, "ZIR int times don't match"
    cali[*,ctz,1]=specz.zspecir
    ;; temperature determining 
    ch2 = ((specz.zirt/2048.)*5.) - 5.   ;Zenith InGaAs temp.
    temps[ctz] = ((1./3200.)*alog(20.*ch2) + (1./298.))^(-1)-273. ;zen ir tmp
    ctz = ctz + 1L
endwhile
temp_cal_tra[1,gnarr]=mean(temps[0:ctz])
temp_cal_tra_st[1,gnarr]=stddev(temps[0:ctz])
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
transfer_arr[*,*,gnarr]=transfer

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SECONDARY CALIBRATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; (6) secondary calibration - darks
specn = {btime: lonarr(2), bcdtimstp:  bytarr(12),intime1: long(0),$
                     intime2: long(0), intime3: long(0), intime4: long(0),accum: long(0), shsw:long(0),$
                     zsit: ulong(0),nsit: ulong(0),zirt: ulong(0),nirt: ulong(0), zirx: ulong(0), nirx: ulong(0),xt: ulong(0),it: ulong(0),$
                     zspecsi: intarr(np), zspecir: intarr(np), nspecsi: intarr(np), nspecir: intarr(np)}
if(strcmp(donadir,'yes')) then begin
    filen=secondary_nadir_si[gnarr]+'dark'+l
    nspc_files = file_search(filen+data, count = numfiles, /FOLD_CASE)
    if (numfiles ne 1) then message,'There should be exactly 1 file for the dark.'
    openr,lunn,nspc_files[0],/get_lun
    ctn=0L
    while not eof(lunn) do begin
        readu, lunn, specn
        if(specn.intime3 ne nsi_intime_s[gnarr]) then message, "NSI int times don't match"
        dark[*,ctn,2]=specn.nspecsi
        ;;Temperature determining
        ch1 = ((specn.nsit/2048.)*5.) - 5.   ;Nadir SI spect temp.
        nsit = long(ch1 * 10000.)     ;convert to integer for use as index
        if (nsit le 19999) then temps[ctn] = revts[nsit]       ;load nadir temp into array
        ctn = ctn + 1L
    endwhile
    temp_dark_sec[2,gnarr]=mean(temps[0:ctn])
    temp_dark_sec_st[2,gnarr]=stddev(temps[0:ctn])
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
    for j=0,np-1 do begin       ; go through channels
        darkm[j,2]=mean(dark[j,0:ctn-1,2])
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
        dark[*,ctn,3]=specn.nspecir
        ;; temperature determining 
        ch3 = ((specn.nirt/2048.)*5.) - 5.   ;Nadir InGaAs temp.
        temps[ctn] = ((1./3200.)*alog(20.*ch3) + (1./298.))^(-1)-273. ;nad ir tmp
	;get temperatures for the IR
	ch7=((specn.nirx/2048.)*5.) - 5.
        rtem = abs((2000. * ch7)/(1. - (0.2 * ch7)))
        dem = 1.0295e-3+(2.391e-4 * alog(rtem)) + (1.568e-7 * (alog(rtem))^3)
        nirxt[ctn] = float(1./dem - 273.)

        ctn = ctn + 1L
    endwhile
    temp_dark_sec[3,gnarr]=mean(temps[0:ctn])
    temp_dark_sec_st[3,gnarr]=stddev(temps[0:ctn])
    for j=0,np-1 do begin       ; go through channels
        darkm[j,3]=mean(dark[j,0:ctn-1,3])
    endfor
    free_lun,lunn
    nirxt_dark=mean(nirxt[0:ctn])
    nirxt_dark_st=stddev(nirxt[0:ctn])
    print, 'NADIR INGAAS Temperature for Darks: ',nirxt_dark, ' +/- ' ,nirxt_dark_st 
endif
;
if(strcmp(dozenith,'yes')) then begin
filez=secondary_zenith_si[gnarr]+'dark'+l
zspc_files = file_search(filez+data, count = numfiles, /FOLD_CASE)
if (numfiles ne 1) then message,'There should be exactly 1 file for the dark.'
openr,lunz,zspc_files[0],/get_lun
ctz=0L
specz = {btime: lonarr(2), bcdtimstp:  bytarr(12),intime1: long(0),$
                     intime2: long(0), intime3: long(0), intime4: long(0),accum: long(0), shsw:long(0),$
                     zsit: ulong(0),nsit: ulong(0),zirt: ulong(0),nirt: ulong(0), zirx: ulong(0), nirx: ulong(0),xt: ulong(0),it: ulong(0),$
                     zspecsi: intarr(np), zspecir: intarr(np), nspecsi: intarr(np), nspecir: intarr(np)}
while not eof(lunz) do begin
    readu, lunz, specz
    if(specz.intime1 ne zsi_intime_s[gnarr]) then message, "ZSI int times don't match"
    dark[*,ctz,0]=specz.zspecsi
    ;;Temperature determining
    ch0 = ((specz.zsit/2048.)*5.) - 5.   ;Nadir SI spect temp.
    zsit = long(ch0 * 10000.)     ;convert to integer for use as index
    if (zsit le 19999) then temps[ctz] = revts[zsit]       ;load nadir temp into array
    ctz = ctz + 1L
endwhile
temp_dark_sec[0,gnarr]=mean(temps[0:ctz])
temp_dark_sec_st[0,gnarr]=stddev(temps[0:ctz])
for j=0,np-1 do begin ; go through channels
    darkm[j,0]=mean(dark[j,0:ctz-1,0])
endfor
free_lun,lunz
;
filez=secondary_zenith_ir[gnarr]+'dark'+l
zspc_files = file_search(filez+data, count = numfiles, /FOLD_CASE)
if (numfiles ne 1) then message,'There should be exactly 1 file for the dark.'
openr,lunz,zspc_files[0],/get_lun
ctz=0L
specz = {btime: lonarr(2), bcdtimstp:  bytarr(12),intime1: long(0),$
                     intime2: long(0), intime3: long(0), intime4: long(0),accum: long(0), shsw:long(0),$
                     zsit: ulong(0),nsit: ulong(0),zirt: ulong(0),nirt: ulong(0), zirx: ulong(0), nirx: ulong(0),xt: ulong(0),it: ulong(0),$
                     zspecsi: intarr(np), zspecir: intarr(np), nspecsi: intarr(np), nspecir: intarr(np)}
while not eof(lunz) do begin
    readu, lunz, specz
    if(specz.intime2 ne zir_intime_s[gnarr]) then message, "ZIR int times don't match"
    dark[*,ctz,1]=specz.zspecir
    ;; temperature determining 
    ch2 = ((specz.zirt/2048.)*5.) - 5.   ;Zenith InGaAs temp.
    temps[ctz] = ((1./3200.)*alog(20.*ch2) + (1./298.))^(-1)-273. ;zen ir tmp
    ; get temperatures for the IR
    ch7=((specz.zirx/2048.)*5.) - 5.
    rtem = abs((2000. * ch7)/(1. - (0.2 * ch7)))
    dem = 1.0295e-3+(2.391e-4 * alog(rtem)) + (1.568e-7 * (alog(rtem))^3)
    zirxt[ctz] = float(1./dem - 273.)

    ctz = ctz + 1L
endwhile
temp_dark_sec[1,gnarr]=mean(temps[0:ctz])
temp_dark_sec_st[1,gnarr]=stddev(temps[0:ctz])
for j=0,np-1 do begin ; go through channels
    darkm[j,1]=mean(dark[j,0:ctz-1,1])
endfor
free_lun,lunz
zirxt_dark=mean(zirxt[0:ctz])
zirxt_dark_st=stddev(zirxt[0:ctz])
print, 'ZENITH INGAAS Temperature for Darks: ',zirxt_dark, ' +/- ' ,zirxt_dark_st
endif


; (7) secondary calibration - cal
specn = {btime: lonarr(2), bcdtimstp:  bytarr(12),intime1: long(0),$
                     intime2: long(0), intime3: long(0), intime4: long(0),accum: long(0), shsw:long(0),$
                     zsit: ulong(0),nsit: ulong(0),zirt: ulong(0),nirt: ulong(0), zirx: ulong(0), nirx: ulong(0),xt: ulong(0),it: ulong(0),$
                     zspecsi: intarr(np), zspecir: intarr(np), nspecsi: intarr(np), nspecir: intarr(np)}
if(strcmp(donadir,'yes')) then begin
    filen=secondary_nadir_si[gnarr]+'cal'+l
    nspc_files = file_search(filen+data, count = numfiles, /FOLD_CASE)
    if (numfiles ne 1) then message,'There should be exactly 1 file for the dark.'
    openr,lunn,nspc_files[0],/get_lun
    ctn=0L
    while not eof(lunn) do begin
        readu, lunn, specn
        if(specn.intime3 ne nsi_intime_s[gnarr]) then message, "NSI int times don't match"
        cali[*,ctn,2]=specn.nspecsi
        ;;Temperature determining
        ch1 = ((specn.nsit/2048.)*5.) - 5.   ;Nadir SI spect temp.
        nsit = long(ch1 * 10000.)     ;convert to integer for use as index
        if (nsit le 19999) then temps[ctn] = revts[nsit]       ;load nadir temp into array
        ctn = ctn + 1L
    endwhile
    temp_cal_sec[2,gnarr]=mean(temps[0:ctn])
    temp_cal_sec_st[2,gnarr]=stddev(temps[0:ctn])
    for j=0,np-1 do begin       ; go through channels
        calim[j,2]=mean(cali[j,0:ctn-1,2])
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
        cali[*,ctn,3]=specn.nspecir
        ;; temperature determining 
        ch3 = ((specn.nirt/2048.)*5.) - 5.   ;Nadir InGaAs temp.
        temps[ctn] = ((1./3200.)*alog(20.*ch3) + (1./298.))^(-1)-273. ;nad ir tmp
	;get temperatures for the IR
	ch7=((specn.nirx/2048.)*5.) - 5.
        rtem = abs((2000. * ch7)/(1. - (0.2 * ch7)))
        dem = 1.0295e-3+(2.391e-4 * alog(rtem)) + (1.568e-7 * (alog(rtem))^3)
        nirxt[ctn] = float(1./dem - 273.)
        
	ctn = ctn + 1L
    endwhile
    temp_cal_sec[3,gnarr]=mean(temps[0:ctn])
    temp_cal_sec_st[3,gnarr]=stddev(temps[0:ctn])
    for j=0,np-1 do begin       ; go through channels
        calim[j,3]=mean(cali[j,0:ctn-1,3])
    endfor
    free_lun,lunn
    nirxt_cal=mean(nirxt[0:ctn])
    nirxt_cal_st=stddev(nirxt[0:ctn])
    print, 'NADIR INGAAS Temperature for Cals: ',nirxt_cal, ' +/- ' ,nirxt_cal_st
endif
;
if(strcmp(dozenith,'yes')) then begin
filez=secondary_zenith_si[gnarr]+'cal'+l
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
    if(specz.intime1 ne zsi_intime_s[gnarr]) then message, "ZSI int times don't match"
    cali[*,ctz,0]=specz.zspecsi
    ;;Temperature determining
    ch0 = ((specz.zsit/2048.)*5.) - 5.   ;Nadir SI spect temp.
    zsit = long(ch0 * 10000.)     ;convert to integer for use as index
    if (zsit le 19999) then temps[ctz] = revts[zsit]       ;load nadir temp into array
    ctz = ctz + 1L
endwhile
temp_cal_sec[0,gnarr]=mean(temps[0:ctz])
temp_cal_sec_st[0,gnarr]=stddev(temps[0:ctz])
for j=0,np-1 do begin ; go through channels
    calim[j,0]=mean(cali[j,0:ctz-1,0])
endfor
free_lun,lunz
;
filez=secondary_zenith_ir[gnarr]+'cal'+l
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
    if(specz.intime2 ne zir_intime_s[gnarr]) then message, "ZIR int times don't match"
    cali[*,ctz,1]=specz.zspecir
    ;; temperature determining 
    ch2 = ((specz.zirt/2048.)*5.) - 5.   ;Zenith InGaAs temp.
    temps[ctz] = ((1./3200.)*alog(20.*ch2) + (1./298.))^(-1)-273. ;zen ir tmp
    ; get temperatures for the IR
    ch7=((specz.zirx/2048.)*5.) - 5.
    rtem = abs((2000. * ch7)/(1. - (0.2 * ch7)))
    dem = 1.0295e-3+(2.391e-4 * alog(rtem)) + (1.568e-7 * (alog(rtem))^3)
    zirxt[ctz] = float(1./dem - 273.)

    ctz = ctz + 1L
endwhile
temp_cal_sec[1,gnarr]=mean(temps[0:ctz])
temp_cal_sec_st[1,gnarr]=stddev(temps[0:ctz])
for j=0,np-1 do begin ; go through channels
    calim[j,1]=mean(cali[j,0:ctz-1,1])
endfor
free_lun,lunz
zirxt_cal=mean(zirxt[0:ctz])
zirxt_cal_st=stddev(zirxt[0:ctz])
print, 'Zenith INGAAS Temperature for Cals: ',zirxt_cal, ' +/- ' ,zirxt_cal_st
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
    resp2[*,0] = dn2[*,0]/zsi_intime_s[gnarr]/(lampzensi)*transfer[*,0]
    resp2[*,1] = dn2[*,1]/zir_intime_s[gnarr]/(lampzenir)*transfer[*,1]
	
	
	
	resp2_arr[*,0,gnarr]=resp2[*,0]
	resp2_arr[*,1,gnarr]=resp2[*,1]
endif

if(strcmp(donadir,'yes')) then begin
    resp2[*,2] = dn2[*,2]/nsi_intime_s[gnarr]/(lampnadsi)*transfer[*,2]
    resp2[*,3] = dn2[*,3]/nir_intime_s[gnarr]/(lampnadir)*transfer[*,3]
	
	resp2_arr[*,2,gnarr]=resp2[*,2]
	resp2_arr[*,3,gnarr]=resp2[*,3]
endif
	
endfor ; end of for loop that goes through all the transfer and secondary calibrations

if(strcmp(dozenith,'yes')) then begin
    ;resp2[*,0] = dn2[*,0]/zsi_intime_s[num_secondary-1]/(lampzensi)*transfer[*,0]
    ;resp2[*,1] = dn2[*,1]/zir_intime_s[num_secondary-1]/(lampzenir)*transfer[*,1]

        resp1_si_file = resp_func_dir + primary_cal_date[num_secondary-1] + '_'+primary_lamp[num_secondary-1]+'_resp1_'+strcompress(string(zsi_intime_p[num_secondary-1]),/REMOVE_ALL)+'_zensi.dat'
        resp1_ir_file = resp_func_dir + primary_cal_date[num_secondary-1] + '_'+primary_lamp[num_secondary-1]+'_resp1_'+strcompress(string(zir_intime_p[num_secondary-1]),/REMOVE_ALL)+'_zenir.dat'
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

save, four, temp_dark_prim,temp_cal_prim,temp_dark_tra,temp_cal_tra,temp_dark_sec,temp_cal_sec,$
  temp_dark_prim_st,temp_cal_prim_st,temp_dark_tra_st,temp_cal_tra_st,temp_dark_sec_st,temp_cal_sec_st, filename=path+l+'calib_temps.out'

if(strcmp(donadir,'yes')) then begin
   ; resp2[*,2] = dn2[*,2]/nsi_intime_s[num_secondary-1]/(lampnadsi)*transfer[*,2]
   ; resp2[*,3] = dn2[*,3]/nir_intime_s[num_secondary-1]/(lampnadir)*transfer[*,3]
    if(strcmp(write_primary,'yes')) then begin

        resp1_si_file = resp_func_dir + primary_cal_date[num_secondary-1] + '_'+primary_lamp[num_secondary-1]+'_resp1_'+strcompress(string(nsi_intime_p[num_secondary-1]),/REMOVE_ALL)+'_nadsi.dat'
        resp1_ir_file = resp_func_dir + primary_cal_date[num_secondary-1] + '_'+primary_lamp[num_secondary-1]+'_resp1_'+strcompress(string(nir_intime_p[num_secondary-1]),/REMOVE_ALL)+'_nadir.dat'

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

    resp2_si_file = resp_func_dir + secondary_cal_date[num_secondary-1] + '_'+lamp[num_secondary-1]+'_resp2_'+strcompress(string(zsi_intime_s[num_secondary-1]),/REMOVE_ALL)+'_zensi.dat'
    resp2_ir_file = resp_func_dir + secondary_cal_date[num_secondary-1] + '_'+lamp[num_secondary-1]+'_resp2_'+strcompress(string(zir_intime_s[num_secondary-1]),/REMOVE_ALL)+'_zenir.dat'

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

    resp2_si_file = resp_func_dir + secondary_cal_date[num_secondary-1] + '_'+lamp[num_secondary-1] +'_resp2_'+strcompress(string(nsi_intime_s[num_secondary-1]),/REMOVE_ALL)+'_nadsi.dat'
    resp2_ir_file = resp_func_dir + secondary_cal_date[num_secondary-1] + '_'+lamp[num_secondary-1] +'_resp2_'+strcompress(string(nir_intime_s[num_secondary-1]),/REMOVE_ALL)+'_nadir.dat'
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
!p.thick=1.8
!p.CHARSIZE=1.5
!p.charthick=1.0
!p.multi=0
!Y.omargin=[0,0]
stop
if(strcmp(dozenith,'yes')) then begin
    window,0,tit='zenith', xsize=500, ysize=1000
    !p.multi=[0,1,2]
    ymax = max([max(resp1_arr(*,0,*)),max(resp1_arr(*,1,*)),max(resp2_arr(*,0,*)),max(resp2_arr(*,1,*))])*1.04
    plot, wlvisz,resp1(*,0),color=255,ytitle='counts/ms W!E-1!N m!E2!N nm',xtitle='Wavelength (nm)',title='Zenith'+' p: '+primary_lamp[num_secondary-1]+' s: '+lamp[num_secondary-1] ,charsize=1.5,thick=1.8,/xs,/ys,xrange=[300,2200],yrange=[0,ymax], linestyle=2 ;'
    oplot,wlnirz,resp1(*,1),thick=1.8,color=255, linestyle=2
	
	legend_tit=['Primary Cal - '+  primary_cal_date[num_secondary-1]  +' - {'+strcompress(string(zsi_intime_p[num_secondary-1]),/REMOVE_ALL)+','+strcompress(string(zir_intime_p[num_secondary-1]),/REMOVE_ALL)+'}']
	color_s=findgen(num_secondary)*230/num_secondary+25
  for i=0,num_secondary-1 do begin
    oplot,wlvisz,resp1_arr(*,0,i),thick=1.8,color=color_s[i], linestyle=2
    oplot,wlnirz,resp1_arr(*,1,i),thick=1.8,color=color_s[i], linestyle=2
  legend_tit=[legend_tit,'Primary Cal - '+primary_cal_date[i]+' - {'+strcompress(string(zsi_intime_p[i]),/REMOVE_ALL)+','+strcompress(string(zir_intime_p[i]),/REMOVE_ALL)+'}']
  endfor
  
	for i=0,num_secondary-1 do begin
    oplot,wlvisz,resp2_arr(*,0,i),thick=1.8,color=color_s[i]
    oplot,wlnirz,resp2_arr(*,1,i),thick=1.8,color=color_s[i]
	legend_tit=[legend_tit,'Secondary Cal - '+secondary_cal_date[i]+' - {'+strcompress(string(zsi_intime_s[i]),/REMOVE_ALL)+','+strcompress(string(zir_intime_s[i]),/REMOVE_ALL)+'}']
	endfor
    legend,legend_tit[1:*],textcolors=[color_s,color_s],/right,outline_color=255 ;'
	
plot, wlvisz,resp2_arr(*,0,ref),color=255,ytitle='%',xtitle='Wavelength (nm)',title='Percent Difference of response function !C Reference:'+ secondary_cal_date[ref]+' - Zenith',charsize=1.5,thick=1.8,/xs,/ys,xrange=[300,2200],yrange=[-15,15], /nodata
leg_tit='0'
for i=0, num_secondary-1 do begin
    oplot,wlvisz,(resp2_arr(*,0,i)-resp2_arr(*,0,ref))*100./resp2_arr(*,0,ref),thick=1.8,color=color_s[i]
    oplot,wlnirz,(resp2_arr(*,1,i)-resp2_arr(*,1,ref))*100./resp2_arr(*,1,ref),thick=1.8,color=color_s[i]
    leg_tit=[leg_tit, primary_cal_date[i]+' : '+secondary_cal_date[i]]
endfor
legend,['Primary : Secondary',leg_tit[1:*]],textcolors=[255,color_s],/right, outline_color=255
    p=tvrd(true=1)
    write_png,path+l+'zenith.png',p
endif

if(strcmp(donadir,'yes')) then begin
    window,1,tit='nadir', xsize=500, ysize=1000
    ymax = max([max(resp1_arr(*,2,*)),max(resp1_arr(*,3,*)),max(resp2_arr(*,2,*)),max(resp2_arr(*,3,*))])*1.04
    plot, wlvisn,resp1(*,2),color=255,ytitle='counts/ms W!E-1!N m!E2!N nm',xtitle='Wavelength (nm)',title='Nadir'+' p: '+primary_lamp[num_secondary-1]+' s: '+lamp[num_secondary-1] ,charsize=1.5,thick=1.8,/xs,/ys,xrange=[300,2200],yrange=[0,ymax], linestyle=2
	oplot,wlnirn,resp1(*,3),thick=1.8,color=255, linestyle=2
	
	legend_tit=['Primary Cal - '+  primary_cal_date[num_secondary-1]  +' - {'+strcompress(string(nsi_intime_p[num_secondary-1]),/REMOVE_ALL)+','+strcompress(string(nir_intime_p[num_secondary-1]),/REMOVE_ALL)+'}']
	color_s=findgen(num_secondary)*230/num_secondary+25
	
	for i=0,num_secondary-1 do begin
    oplot,wlvisz,resp1_arr(*,2,i),thick=1.8,color=color_s[i], linestyle=2
    oplot,wlnirz,resp1_arr(*,3,i),thick=1.8,color=color_s[i], linestyle=2
  legend_tit=[legend_tit,'Primary Cal - '+primary_cal_date[i]+' - {'+strcompress(string(nsi_intime_p[i]),/REMOVE_ALL)+','+strcompress(string(nir_intime_p[i]),/REMOVE_ALL)+'}']
  endfor	
	for i=0,num_secondary-1 do begin
    oplot,wlvisn,resp2_arr(*,2,i),thick=1.8,color=color_s[i]
    oplot,wlnirn,resp2_arr(*,3,i),thick=1.8,color=color_s[i]
	legend_tit=[legend_tit, 'Secondary Cal - '+secondary_cal_date[i]+' - {'+strcompress(string(nsi_intime_s[i]),/REMOVE_ALL)+','+strcompress(string(nir_intime_s[i]),/REMOVE_ALL)+'}']
	endfor
     legend,legend_tit[1:*],textcolors=[color_s,color_s],/right,outline_color=255

  plot, wlvisn, resp2_arr(*,2,ref),color=255,ytitle='%',xtitle='Wavelength (nm)',title='Percent Difference of response function !C Reference:'+ secondary_cal_date[ref]+' - Nadir',charsize=1.5,thick=1.8,/xs,/ys,xrange=[300,2200],yrange=[-15,15], /nodata
  leg_tit='0'
  for i=0, num_secondary-1 do begin
      oplot,wlvisn,(resp2_arr(*,2,i)-resp2_arr(*,2,ref))*100./resp2_arr(*,2,ref),thick=1.8,color=color_s[i]
      oplot,wlnirn,(resp2_arr(*,3,i)-resp2_arr(*,3,ref))*100./resp2_arr(*,3,ref),thick=1.8,color=color_s[i]
          leg_tit=[leg_tit, primary_cal_date[i]+' : '+secondary_cal_date[i]]
  endfor
  legend,['Primary : Secondary',leg_tit[1:*]],textcolors=[255,color_s],/right, outline_color=255

    p=tvrd(true=1)
    write_png,path+l+'nadir.png',p
endif

if (1) then begin ; over time comparison
  window, 7, title='Change of response function over time at 940nm', xsize=500, ysize=1000
  ;zenith
  if(strcmp(dozenith,'yes')) then begin
  kl=min(abs(940.-wlvisz),w940vz)
  kl=min(abs(940.-wlnirz),w940iz)
  plot, findgen(num_secondary), resp2_arr[w940vz,0,*], title='Zenith change of response function', ytitle='counts/ms W!E-1!N m!E2!N nm', xtitle='calibration days',color=255, charsize=1.5,thick=1.8,/xs,/ys,yrange=[10,15]
  oplot,findgen(num_secondary), resp2_arr[w940iz,1,*], color=150
  
  oplot,findgen(num_secondary), resp2_arr[w940vz,0,*], color=255, psym=2
  oplot,findgen(num_secondary), resp2_arr[w940iz,1,*], color=150, psym=2
  xyouts, 0,resp2_arr[w940vz,0,0],'Pre'
  xyouts, num_secondary-1,resp2_arr[w940vz,0,num_secondary-1],'Post'
  legend, ['Vis','NIR'],textcolors=[255,150]
  endif 
  
  ;nadir
  if(strcmp(donadir,'yes')) then begin
  kl=min(abs(940.-wlvisn),w940vn)
  kl=min(abs(940.-wlnirn),w940in)
  plot, findgen(num_secondary), resp2_arr[w940vn,2,*], title='Nadir change of response function', ytitle='counts/ms W!E-1!N m!E2!N nm', xtitle='calibration days',color=255, charsize=1.5,thick=1.8,/xs,/ys, yrange=[12,35]
  oplot,findgen(num_secondary), resp2_arr[w940in,3,*], color=150
  
  oplot,findgen(num_secondary), resp2_arr[w940vn,2,*], color=255, psym=2
  oplot,findgen(num_secondary), resp2_arr[w940in,3,*], color=150, psym=2
  xyouts, 0,resp2_arr[w940vn,2,0],'Pre'
  xyouts, num_secondary-1,resp2_arr[w940vn,2,num_secondary-1],'Post'
  legend, ['Vis','NIR'],textcolors=[255,150]
  endif
  
      p=tvrd(true=1)
      write_png,path+l+'time_cal.png',p
      
endif

if (1) then begin  ;transfer comparison
  window, 2, title='Transfer comparison', xsize=500, ysize=500
  !p.multi=0
  plot, wlvisz, transfer_arr[*,0,0]/transfer_arr[*,0,num_secondary-1], title='Transfer change',ytitle='ratio pre-post',xtitle='Wavelength (nm)',charsize=1.5,thick=1.8,/xs,/ys,xrange=[300,2200],yrange=[0.9,1.1]
  oplot,wlnirz, transfer_arr[*,1,0]/transfer_arr[*,1,num_secondary-1], color=255
  oplot,wlvisn, transfer_arr[*,2,0]/transfer_arr[*,2,num_secondary-1], color=10
  oplot,wlnirn, transfer_arr[*,3,0]/transfer_arr[*,3,num_secondary-1], color=10
  
  legend, ['Zenith','Nadir'],textcolors=[255,10],/right
      p=tvrd(true=1)
      write_png,path+l+'transfer.png',p
endif
;calnex_compare_cal, resp2_arr, wlvisz, wlnirz, wlvisn, wlnirn, n_elements(secondary_cal_date)-2,n_elements(secondary_cal_date)-1 , secondary_cal_date

if (1) then begin  ; plotting of the actual spectras from the secondary calibrations
window, 3, title='Secondary calibrations spectras',xsize=1000, ysize=1000
!p.multi=[0,2,2]
!x.omargin=[0,8]
!p.thick=1.8
spectzs = reform(resp2_arr[*,0,*]/transfer_arr[*,0,*] /resp1_arr[*,0,*])
spectzi = reform(resp2_arr[*,1,*]/transfer_arr[*,1,*] /resp1_arr[*,1,*])
spectns = reform(resp2_arr[*,2,*]/transfer_arr[*,2,*] /resp1_arr[*,2,*])
spectni = reform(resp2_arr[*,3,*]/transfer_arr[*,3,*] /resp1_arr[*,3,*])
for i=0, num_secondary-1 do begin
spectzs[*,i]=spectzs[*,i]* lampzensi
spectzi[*,i]=spectzi[*,i]* lampzenir
spectns[*,i]=spectns[*,i]* lampnadsi
spectni[*,i]=spectni[*,i]* lampnadir
endfor
join=940
mm=min(abs(join-wlvisz),szj)
mm=min(abs(join-wlvisn),snj)
mm=min(abs(join-wlnirz),izj)
mm=min(abs(join-wlnirn),inj)
spectz=[spectzs[0:szj,*],spectzi[izj:*,*]]
spectn=[spectns[0:snj,*],spectni[inj:*,*]]
wvlz=[wlvisz[0:szj],wlnirz[izj:*]]
wvln=[wlvisn[0:snj],wlnirn[inj:*]]
legend_tit=[secondary_cal_date[0]]
plot, wvlz, spectz[*,0], title='Zenith secondary calibration spectrum', xtitle='Wavelength (nm)', ytitle='Irradiance (Wm!E-2!Nnm!E-1!N)', yrange=[0,0.5], xrange=[300,2200]
for i=1, num_secondary-1 do begin
oplot, wvlz, spectz[*,i], color=color_s[i]
  legend_tit=[legend_tit,secondary_cal_date[i]]
endfor

rf=3
plot, wvlz, spectz[*,0]/spectz[*,rf]*100., title='Zenith secondary calibration difference', xtitle='Wavelength (nm)', ytitle='%', yrange=[90,110], xrange=[300,2200]
for i=1, num_secondary-1 do begin
oplot, wvlz, spectz[*,i]/spectz[*,rf]*100., color=color_s[i]
endfor

plot, wvln, spectn[*,0], title='Nadir secondary calibration spectrum', xtitle='Wavelength (nm)', ytitle='Irradiance (Wm!E-2!Nnm!E-1!N)', yrange=[0,0.5], xrange=[300,2200]
for i=1, num_secondary-1 do begin
oplot, wvln, spectn[*,i], color=color_s[i]
endfor

rf=3
plot, wvln, spectn[*,0]/spectn[*,rf]*100., title='Nadir secondary calibration difference', xtitle='Wavelength (nm)', ytitle='%', yrange=[90,110], xrange=[300,2200]
for i=1, num_secondary-1 do begin
oplot, wvln, spectn[*,i]/spectn[*,rf]*100., color=color_s[i]
endfor

legend, legend_tit, textcolors=[255,color_s[1:*]], position=[0.9,0.9], /normal
legend, legend_tit, textcolors=[255,color_s[1:*]], position=[0.9,0.4], /normal

p=tvrd(true=1)
write_png,path+l+'calib_spect.png',p
endif

if (1) then begin  ;temperature comparison
loadct, 39,/silent
!p.background=255
!p.color=0
!p.thick=4.0
!p.CHARSIZE=2.5
!p.charthick=2.0
device, decomposed=0
  window, 4, title='Primary Temperature comparison', xsize=1000, ysize=1000
  !p.multi=[0,2,2]
  !Y.omargin=[0,4]
  four=' 0 - ZSI, 1 - ZIR, 2 - NSI, 3 - NIR'
  
  plot, findgen(num_secondary), temp_cal_prim[0,*], title='Temperature for Si - Calibration', ytitle='Temp(Celsius)', xtitle='calibration days', charsize=1.5,/xs,/ys, yrange=[20,35]
  oplot, findgen(num_secondary), temp_cal_prim[2,*], color=250
  pth=!p.thick
  !p.thick=0.2
  errplot, findgen(num_secondary), temp_cal_prim[0,*]-temp_cal_prim_st[0,*],temp_cal_prim[0,*]+temp_cal_prim_st[0,*]
  errplot, findgen(num_secondary), temp_cal_prim[2,*]-temp_cal_prim_st[2,*],temp_cal_prim[2,*]+temp_cal_prim_st[2,*],color=250
  !p.thick=pth
  legend,['Zenith','Nadir'],textcolors=[0,250],box=0
  
  plot, findgen(num_secondary), temp_cal_prim[1,*], title='Temperature for InGaAs - Calibration', ytitle='Temp(Celsius)', xtitle='calibration days',color=0, charsize=1.5,/xs,/ys, yrange=[-12,-8]
  oplot, findgen(num_secondary), temp_cal_prim[3,*], color=250
  pth=!p.thick
  !p.thick=0.2
  errplot, findgen(num_secondary), temp_cal_prim[1,*]-temp_cal_prim_st[1,*],temp_cal_prim[1,*]+temp_cal_prim_st[1,*],color=0
  errplot, findgen(num_secondary), temp_cal_prim[3,*]-temp_cal_prim_st[3,*],temp_cal_prim[3,*]+temp_cal_prim_st[3,*],color=250
  !p.thick=pth
  legend,['Zenith','Nadir'],textcolors=[0,250],box=0
  
    plot, findgen(num_secondary), temp_cal_prim[0,*], title='Temperature for Si - Dark', ytitle='Temp(Celsius)', xtitle='calibration days',color=0, charsize=1.5,/xs,/ys, yrange=[20,35]
  oplot, findgen(num_secondary), temp_cal_prim[2,*], color=250
  pth=!p.thick
  !p.thick=0.2
  errplot, findgen(num_secondary), temp_cal_prim[0,*]-temp_cal_prim_st[0,*],temp_cal_prim[0,*]+temp_cal_prim_st[0,*],color=0
  errplot, findgen(num_secondary), temp_cal_prim[2,*]-temp_cal_prim_st[2,*],temp_cal_prim[2,*]+temp_cal_prim_st[2,*],color=250
  !p.thick=pth
  legend,['Zenith','Nadir'],textcolors=[0,250],box=0
  
  plot, findgen(num_secondary), temp_dark_prim[1,*], title='Temperature for InGaAs - Dark', ytitle='Temp(Celsius)', xtitle='calibration days',color=0, charsize=1.5,/xs,/ys, yrange=[-12,-8]
  oplot, findgen(num_secondary), temp_dark_prim[3,*], color=250
  pth=!p.thick
  !p.thick=0.2
  errplot, findgen(num_secondary), temp_dark_prim[1,*]-temp_dark_prim_st[1,*],temp_dark_prim[1,*]+temp_dark_prim_st[1,*],color=0
  errplot, findgen(num_secondary), temp_dark_prim[3,*]-temp_dark_prim_st[3,*],temp_dark_prim[3,*]+temp_dark_prim_st[3,*],color=250
  !p.thick=pth
  legend,['Zenith','Nadir'],textcolors=[0,250],box=0
  
  xyouts, 0.5,0.95, alignment=0.5, /normal, charsize=4.0,'Primary Temperature comparison'
      p=tvrd(true=1)
      write_png,path+l+'temperature_prim.png',p
      
  window, 5, title='Transfer Temperature comparison', xsize=1000, ysize=1000
  !p.multi=[0,2,2]
  four=' 0 - ZSI, 1 - ZIR, 2 - NSI, 3 - NIR'
  
  plot, findgen(num_secondary), temp_cal_tra[0,*], title='Temperature for Si - Calibration', ytitle='Temp(Celsius)', xtitle='calibration days',color=0, charsize=1.5,/xs,/ys, yrange=[20,35]
  oplot, findgen(num_secondary), temp_cal_tra[2,*], color=250
  pth=!p.thick
  !p.thick=0.2
  errplot, findgen(num_secondary), temp_cal_tra[0,*]-temp_cal_tra_st[0,*],temp_cal_tra[0,*]+temp_cal_tra_st[0,*],color=0
  errplot, findgen(num_secondary), temp_cal_tra[2,*]-temp_cal_tra_st[2,*],temp_cal_tra[2,*]+temp_cal_tra_st[2,*],color=250
  !p.thick=pth
  legend,['Zenith','Nadir'],textcolors=[0,250],box=0
  
  plot, findgen(num_secondary), temp_cal_tra[1,*], title='Temperature for InGaAs - Calibration', ytitle='Temp(Celsius)', xtitle='calibration days',color=0, charsize=1.5,/xs,/ys, yrange=[-12,-8]
  oplot, findgen(num_secondary), temp_cal_tra[3,*], color=250
  pth=!p.thick
  !p.thick=0.2
  errplot, findgen(num_secondary), temp_cal_tra[1,*]-temp_cal_tra_st[1,*],temp_cal_tra[1,*]+temp_cal_tra_st[1,*],color=0
  errplot, findgen(num_secondary), temp_cal_tra[3,*]-temp_cal_tra_st[3,*],temp_cal_tra[3,*]+temp_cal_tra_st[3,*],color=250
  !p.thick=pth
  legend,['Zenith','Nadir'],textcolors=[0,250],box=0
  
    plot, findgen(num_secondary), temp_cal_tra[0,*], title='Temperature for Si - Dark', ytitle='Temp(Celsius)', xtitle='calibration days',color=0, charsize=1.5,/xs,/ys, yrange=[20,35]
  oplot, findgen(num_secondary), temp_cal_tra[2,*], color=250
  pth=!p.thick
  !p.thick=0.2
  errplot, findgen(num_secondary), temp_cal_tra[0,*]-temp_cal_tra_st[0,*],temp_cal_tra[0,*]+temp_cal_tra_st[0,*],color=0
  errplot, findgen(num_secondary), temp_cal_tra[2,*]-temp_cal_tra_st[2,*],temp_cal_tra[2,*]+temp_cal_tra_st[2,*],color=250
  !p.thick=pth
  legend,['Zenith','Nadir'],textcolors=[0,250],box=0
  
  plot, findgen(num_secondary), temp_dark_tra[1,*], title='Temperature for InGaAs - Dark', ytitle='Temp(Celsius)', xtitle='calibration days',color=0, charsize=1.5,/xs,/ys, yrange=[-12,-8]
  oplot, findgen(num_secondary), temp_dark_tra[3,*], color=250
  pth=!p.thick
  !p.thick=0.2
  errplot, findgen(num_secondary), temp_dark_tra[1,*]-temp_dark_tra_st[1,*],temp_dark_tra[1,*]+temp_dark_tra_st[1,*],color=0
  errplot, findgen(num_secondary), temp_dark_tra[3,*]-temp_dark_tra_st[3,*],temp_dark_tra[3,*]+temp_dark_tra_st[3,*],color=250
  !p.thick=pth
  legend,['Zenith','Nadir'],textcolors=[0,250],box=0
  xyouts, 0.5,0.95, alignment=0.5, /normal, charsize=4.0,'Transfer Temperature comparison'
      p=tvrd(true=1)
      write_png,path+l+'temperature_tra.png',p
      
  window, 6, title='Secondary Temperature comparison', xsize=1000, ysize=1000
  !p.multi=[0,2,2]
  four=' 0 - ZSI, 1 - ZIR, 2 - NSI, 3 - NIR'
  
  plot, findgen(num_secondary), temp_cal_sec[0,*], title='Temperature for Si - Calibration', ytitle='Temp(Celsius)', xtitle='calibration days',color=0, charsize=1.5,/xs,/ys, yrange=[20,35]
  oplot, findgen(num_secondary), temp_cal_sec[2,*], color=250
  pth=!p.thick
  !p.thick=0.2
  errplot, findgen(num_secondary), temp_cal_sec[0,*]-temp_cal_sec_st[0,*],temp_cal_sec[0,*]+temp_cal_sec_st[0,*],color=0
  errplot, findgen(num_secondary), temp_cal_sec[2,*]-temp_cal_sec_st[2,*],temp_cal_sec[2,*]+temp_cal_sec_st[2,*],color=250
  !p.thick=pth
  legend,['Zenith','Nadir'],textcolors=[0,250],box=0
  
  plot, findgen(num_secondary), temp_cal_sec[1,*], title='Temperature for InGaAs - Calibration', ytitle='Temp(Celsius)', xtitle='calibration days',color=0, charsize=1.5,/xs,/ys, yrange=[-12,-8]
  oplot, findgen(num_secondary), temp_cal_sec[3,*], color=250
  pth=!p.thick
  !p.thick=0.2
  errplot, findgen(num_secondary), temp_cal_sec[1,*]-temp_cal_sec_st[1,*],temp_cal_sec[1,*]+temp_cal_sec_st[1,*],color=0
  errplot, findgen(num_secondary), temp_cal_sec[3,*]-temp_cal_sec_st[3,*],temp_cal_sec[3,*]+temp_cal_sec_st[3,*],color=250
  !p.thick=pth
  legend,['Zenith','Nadir'],textcolors=[0,250],box=0
  
    plot, findgen(num_secondary), temp_cal_sec[0,*], title='Temperature for Si - Dark', ytitle='Temp(Celsius)', xtitle='calibration days',color=0, charsize=1.5,/xs,/ys, yrange=[20,35]
  oplot, findgen(num_secondary), temp_cal_sec[2,*], color=250
  pth=!p.thick
  !p.thick=0.2
  errplot, findgen(num_secondary), temp_cal_sec[0,*]-temp_cal_sec_st[0,*],temp_cal_sec[0,*]+temp_cal_sec_st[0,*],color=0
  errplot, findgen(num_secondary), temp_cal_sec[2,*]-temp_cal_sec_st[2,*],temp_cal_sec[2,*]+temp_cal_sec_st[2,*],color=250
  !p.thick=pth
  legend,['Zenith','Nadir'],textcolors=[0,250],box=0
  
  plot, findgen(num_secondary), temp_dark_sec[1,*], title='Temperature for InGaAs - Dark', ytitle='Temp(Celsius)', xtitle='calibration days',color=0, charsize=1.5,/xs,/ys, yrange=[-12,-8]
  oplot, findgen(num_secondary), temp_dark_sec[3,*], color=250
  pth=!p.thick
  !p.thick=0.2
  errplot, findgen(num_secondary), temp_dark_sec[1,*]-temp_dark_sec_st[1,*],temp_dark_sec[1,*]+temp_dark_sec_st[1,*],color=0
  errplot, findgen(num_secondary), temp_dark_sec[3,*]-temp_dark_sec_st[3,*],temp_dark_sec[3,*]+temp_dark_sec_st[3,*],color=250
  !p.thick=pth
  legend,['Zenith','Nadir'],textcolors=[0,250],box=0
  xyouts, 0.5,0.95, alignment=0.5, /normal, charsize=4.0,'Secondary Temperature comparison'
      p=tvrd(true=1)
      write_png,path+l+'temperature_sec.png',p
      
endif

stop
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; need to get ratio of differences of calibrations


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
