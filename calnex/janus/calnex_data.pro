;+
; NAME:
;   calnex_data
;
; PURPOSE:
;   Read in the data for one day, save as an IDL out format
;
; CATEGORY:
;   CALNEX / Data
;
; CALLING SEQUENCE:
;   calnex_data, date
;   where date is in format yyyymmdd
;   
; OUTPUT:
;   "_calibspecs.out" calibrated spectras for the day
;   "_temp.out" temperature data for the day
;   raw and calibrated spectras for each time stamp in a window, and some selected
;   prints saturated spectra information
;
; KEYWORDS:
;   SSFR, P3, CALNEX, CG4, Data
;
; DEPENDENCIES:
;   cfg.pro       ;config file cheker
;   calnex_dark.pro  ;dark spectra finder (procedure ini_dark and read_drk)
;   
; NEEDED FILES:
;   - config file for day
;   - all ".OSA2" files for the day under the proper directory (/calnex/p3/*date*/)
;  
; EXAMPLE:
;   calnex_data, 20100421
;
; MODIFICATION HISTORY:
; Written:  Sebastian Schmidtc, LASP CU Boulder, date unknown
; Modified: Friday, April 23rd, 2010
;          -by Samuel LeBlanc
;           Added comments  
;           Added date keyword in call  
;           Added spawn chmod for output files    
;---------------------------------------------------------------------------
 @cfg.pro
 @calnex_dark.pro
pro calnex_data, date_inp

set_plot, 'x'
device,decomposed=0
loadct,27,/silent
!P.multi=0

months = ["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"]
stop_checking = 0

indir  = '/data/seven/schmidt/calnex/p3'           ; directory
if n_elements(date_inp) lt 1 then begin            ; date
  date='20100421'
endif else begin
  date=strcompress(string(date_inp),/REMOVE_ALL)
endelse
               ; date
l      = '/'                      ; directory separator
dir    = indir + l + date
outdir = dir

cfg_file=dir+l+date+'.cfg'        ; build cfg file
if file_test(cfg_file) ne 1 then message,'Did not find configuration file.'

if (n_elements(doplot) eq 0) then begin
    doplot = (strcmp(strmid(cfg(cfg_file,'plot'),0,1),'y'))
endif

if (doplot ge 1) then begin
    xsize=500
    ysize=300
    window,0,title='Raw Zenith',xsize=xsize,ysize=ysize,retain=2
    window,1,title='Raw Nadir',xsize=xsize,ysize=ysize,retain=2
    window,2,title='Zenith',xsize=xsize,ysize=ysize,retain=2
    window,3,title='Nadir',xsize=xsize,ysize=ysize,retain=2
endif


; Read parameters from configuration file
np     =cfg(cfg_file,'np'  )    ; number of channels for each spectrum
np     =fix(np)
;
data   =cfg(cfg_file,'data')    ; data file names
data   =dir+l+data              ; append file name to path
;
dark   =cfg(cfg_file,'dark')    ; get dark mode (I or T)
darki  =dir+l+'darki.dat'       ; file name dark file I
darkt  =dir+l+'darkt.dat'       ; file name dark file T
darkmin=cfg(cfg_file,'darkmin') ; required minimum length of dark cycle
darkmin=fix(darkmin)
;
temp   =cfg(cfg_file,'thermistor') ; thermistor coefficients
;temp   =dir+l+temp              ; append file name to path
;
zrespsi =cfg(cfg_file,'zresponseSI') ; response function Si
zrespir =cfg(cfg_file,'zresponseIR') ; response function IR
nrespsi =cfg(cfg_file,'nresponseSI') ; response function Si
nrespir =cfg(cfg_file,'nresponseIR') ; response function IR
;respsi =dir+l+'cal'+l+respsi      ; append file name to path
;respir =dir+l+'cal'+l+respir      ; append file name to path
respdir =cfg(cfg_file,'respdir') ; directory where response functions are located
;
zlambdasi=cfg(cfg_file,'zlambdaSI') ; wavelength coefficients SI
zlambdasi=strsplit(zlambdasi,' ,',escape='#',/EXTRACT) ; make string into substrings
znlsi=n_elements(zlambdasi)
zlambdasi=float(zlambdasi)
zlambdair=cfg(cfg_file,'zlambdaIR') ; wavelength coefficients IR
zlambdair=strsplit(zlambdair,' ,',escape='#',/EXTRACT) ; make string into substrings
znlir=n_elements(zlambdair)
zlambdair=float(zlambdair)

nlambdasi=cfg(cfg_file,'nlambdaSI') ; wavelength coefficients SI
nlambdasi=strsplit(nlambdasi,' ,',escape='#',/EXTRACT) ; make string into substrings
nnlsi=n_elements(nlambdasi)
nlambdasi=float(nlambdasi)
nlambdair=cfg(cfg_file,'nlambdaIR') ; wavelength coefficients IR
nlambdair=strsplit(nlambdair,' ,',escape='#',/EXTRACT) ; make string into substrings
nnlir=n_elements(nlambdair)
nlambdair=float(nlambdair)

zsi0    =cfg(cfg_file,'zSIfirst') ; first wavelength Si
zjoin   =cfg(cfg_file,'zJOIN')    ; joinder wavelength Si - IR
zir1    =cfg(cfg_file,'zIRlast')  ; last wavelength IR
zsi0=float(zsi0) & zjoin=float(zjoin) & zir1=float(zir1)
zsmooth =cfg(cfg_file,'zsmooth')  ; over how many channels to smooth
zsmooth =fix(zsmooth)

nsi0    =cfg(cfg_file,'zSIfirst') ; first wavelength Si
njoin   =cfg(cfg_file,'zJOIN')    ; joinder wavelength Si - IR
nir1    =cfg(cfg_file,'zIRlast')  ; last wavelength IR
nsi0=float(nsi0) & njoin=float(njoin) & nir1=float(nir1)
nsmooth =cfg(cfg_file,'nsmooth')  ; over how many channels to smooth
nsmooth =fix(nsmooth)

platform = cfg(cfg_file,'platform')

; Determine the number of data files and put it in the string array spc_files
spc_files = file_search(data, count = numfiles, /FOLD_CASE)
if (numfiles eq 0) then message,'No data files found.'

; Read temperature coefficients
npt  = file_lines(temp)
tdat = fltarr(2, npt)   & revt = fltarr(2, npt)
openr,ut,temp, /get_lun & readf,ut,tdat & free_lun,ut
for i = 0, npt - 1 do begin
    revt[0, i] = tdat[0, npt -i - 1] & revt[1, i] = tdat[1, npt -i - 1]
endfor
rt    = findgen(20000)/10000. + 0. ; interpolate thermistor data
revts = spline(revt(1, *), revt(0, *), rt) ; at fine resolution

; Initialize: get darks
print, 'Process ',data
print, 'Number of files: ', numfiles
if dark eq 'I' then print,'Dark mode: Interpolation' else  print,'Dark mode: Temperature'

ini_dark,spc_files,np,darkmin,dark,darku,darki,darkt,aa,bb,zz,utd0,utd1,date

; Initialize: get response functions
cd,respdir,current=prevdir
;print, zrespsi,zrespir,nrespsi,nrespir

zrespsi1 = file_search(zrespsi,count=zrsi,/FOLD_CASE)
zrespir1 = file_search(zrespir,count=zrir,/FOLD_CASE)

nrespsi1 = file_search(nrespsi,count=nrsi,/FOLD_CASE)
nrespir1 = file_search(nrespir,count=nrir,/FOLD_CASE)


if zrsi lt 1 then message,'Did not find zenith si response functions.' ; Exit program
if zrir lt 1 then message,'Did not find zenith ir response functions.' ; Exit program
if nrsi lt 1 then message,'Did not find nadir si response functions.'  ; Exit program
if nrir lt 1 then message,'Did not find nadir ir response functions.'  ; Exit program

if zrsi gt 1 then begin      ; found more than one SI response function
  resp_date = lonarr(zrsi)
  for ii=0,zrsi-1 do begin
    tmp = strsplit(zrespsi1[ii],'_',/extract)
    resp_date[ii] = long(tmp[0])
  endfor
  resp_index = max(where(resp_date le date)) ; use the latest response function, in the standard setting
  print, "Using zenith Si response function ", zrespsi1[resp_index]
  zrespsi1 = respdir+zrespsi1[resp_index]
endif
if nrsi gt 1 then begin      ; found more than one SI response function
  resp_date = lonarr(nrsi)
  for ii=0,nrsi-1 do begin
    tmp = strsplit(nrespsi1[ii],'_',/extract)
    resp_date[ii] = long(tmp[0])
  endfor
  resp_index = max(where(resp_date le date)) ; use the latest response function, in the standard setting
  print, "Using nadir Si response function ", nrespsi1[resp_index]
  nrespsi1 = respdir+nrespsi1[resp_index]
endif

if zrir gt 1 then begin      ; found more than one IR response function
  resp_date = lonarr(zrir)
  for ii=0,zrir-1 do begin
    tmp = strsplit(zrespir1[ii],'_',/extract)
    resp_date[ii] = long(tmp[0])
  endfor
  resp_index = max(where(resp_date le date)) ; use the latest response function, in the standard setting
  print, "Using zenith InGaAs response function ", zrespir1[resp_index]
  zrespir1 = respdir+zrespir1[resp_index]
endif
if nrir gt 1 then begin      ; found more than one IR response function
  resp_date = lonarr(nrir)
  for ii=0,nrir-1 do begin
    tmp = strsplit(nrespir1[ii],'_',/extract)
    resp_date[ii] = long(tmp[0])
  endfor
  resp_index = max(where(resp_date le date)) ; use the latest response function, in the standard setting
  print, "Using nadir InGaAs response function ", nrespir1[resp_index]
  nrespir1 = respdir+nrespir1[resp_index]
endif

; Read zenith response functions into "si" and "ir", respectively
zsi = fltarr(2, 256)            & zir = fltarr(2, 256)
openr,zsilun,zrespsi1,/get_lun  & openr,zirlun,zrespir1,/get_lun
readf,zsilun,zsi                & readf,zirlun,zir
free_lun,zsilun                 & free_lun,zirlun

; Read nadir response functions into "si" and "ir", respectively
nsi = fltarr(2, 256)             & nir = fltarr(2, 256)
openr,nsilun,nrespsi1,/get_lun   & openr,nirlun,nrespir1,/get_lun
readf,nsilun,nsi                 & readf,nirlun,nir
free_lun,nsilun                  & free_lun,nirlun

cd,prevdir

; Initialize: get wavelengths
wlvisz=fltarr(np) & wlnirz=fltarr(np)
for i=0,np-1 do begin
   for j=0,znlsi-1 do wlvisz[i]     =wlvisz[i]     +zlambdasi[j]*float(i)^j
   for j=0,znlir-1 do wlnirz[np-1-i]=wlnirz[np-1-i]+zlambdair[j]*float(i)^j
endfor
wlvisn=fltarr(np) & wlnirn=fltarr(np)
for i=0,np-1 do begin
   for j=0,nnlsi-1 do wlvisn[i]     =wlvisn[i]     +nlambdasi[j]*float(i)^j
   for j=0,nnlir-1 do wlnirn[np-1-i]=wlnirn[np-1-i]+nlambdair[j]*float(i)^j
endfor

; Initialize: get wavelength indices
zind=where(wlvisz ge zsi0)         ; get index of first Si wavelength
zindsi1=zind[0]
zind=where(wlvisz lt zjoin,ct)     ; get index of last Si wavelength
zindsi2=zind[ct-1]
zind=where(wlnirz gt zjoin)        ; get index of first IR wavelength
zindin1=zind[0]
zind=where(wlnirz le zir1,ct)      ; get index of last IR wavelength
zindin2=zind[ct-1]
znumsi   =zindsi2-zindsi1+1
znumir   =zindin2-zindin1+1
znlambda=znumsi+znumir

nind=where(wlvisz ge nsi0)         ; get index of first Si wavelength
nindsi1=nind[0]
nind=where(wlvisz lt njoin,ct)     ; get index of last Si wavelength
nindsi2=nind[ct-1]
nind=where(wlnirz gt njoin)        ; get index of first IR wavelength
nindin1=nind[0]
nind=where(wlnirz le nir1,ct)      ; get index of last IR wavelength
nindin2=nind[ct-1]
numsi   =nindsi2-nindsi1+1
numir   =nindin2-nindin1+1
nnlambda=numsi+numir

; Initialize: get wavelengths of joined spectra
zenlambda=fltarr(znlambda)
zenlambda[0:zindsi2-zindsi1]=wlvisz[zindsi1:zindsi2]
zenlambda[zindsi2-zindsi1+1:znlambda-1]=wlnirz[zindin1:zindin2]

nadlambda=fltarr(nnlambda)
nadlambda[0:nindsi2-nindsi1]=wlvisn[nindsi1:nindsi2]
nadlambda[nindsi2-nindsi1+1:nnlambda-1]=wlnirn[nindin1:nindin2]

; Get interval
i0=cfg(cfg_file,'interval') ; look if we should only use data within a certain time window
uu0=0
if not strcmp(i0,'#') then begin
    uu=strsplit(i0,' ,',escape='#',/EXTRACT)
    uu=float(uu)
    uu0=1
    u0=uu[0] & u1=uu[1]
endif else begin
  u0=0 & u1=48. ; two days
endelse

; Initialize variables / allocate memory
bignum  = 60000L ; defines maximum spectrum number - increase if needed

tmhrs   = dblarr(bignum)        ; time array to hold all UTC times
zjoined  = fltarr(znlambda)        ; joined spectrum
zej      = fltarr(znlambda)        ; joined error spectrum
njoined  = fltarr(nnlambda)        ; joined spectrum
nej      = fltarr(nnlambda)        ; joined error spectrum
zspectra = fltarr(znlambda,bignum) ; time series of joined spectrum
nspectra = fltarr(nnlambda,bignum) ; time series of joined spectrum
zes      = fltarr(znlambda,bignum) ; time series of joined error spectrum
nes      = fltarr(nnlambda,bignum) ; time series of joined error spectrum
sitemp  = fltarr(bignum)        ; array to hold the SI spec. temp
bxtemp  = fltarr(bignum)       ; array to hold the computer temp
bxcltemp= fltarr(bignum)       ; array to hold the inner box temp
nirtemp = fltarr(bignum)        ; array to hold the Nadir InGaAs spec. temp
zirtemp = fltarr(bignum)        ; array to hold the Zenith InGaAs spec. temp
nirxt   = fltarr(bignum)        ; temperature housing nadir NIR
zirxt   = fltarr(bignum)        ; temperature housing zenith NIR
nsitemp = fltarr(bignum)        ; array to hold the Nadir Si spec. temp
zsitemp = fltarr(bignum)        ; array to hold the Zenith Si spec. temp
sat     = intarr(bignum)

ctr = 0L                        ; Counter for nondark spectra
darkcount=0                     ; Counter for dark cycles
slast = 0
for ind = 0,numfiles -1 do begin ; Primary Loop
                                ; print,spc_files[ind]
    print, "Opening ",spc_files(ind)
    print,format='(a1,$)',"."
    openr, lun, spc_files(ind), /get_lun
    while not eof(lun) do begin ; Read individual spectra
        spect = {btime: lonarr(2), bcdtimstp:  bytarr(12),intime1: long(0),$
                     intime2: long(0), intime3: long(0), intime4: long(0),accum: long(0), shsw:long(0),$
                     zsit: ulong(0),nsit: ulong(0),zirt: ulong(0),nirt: ulong(0), zirx: ulong(0), nirx: ulong(0),xt: ulong(0),it: ulong(0),$
                     zspecsi: intarr(np), zspecir: intarr(np), nspecsi: intarr(np), nspecir: intarr(np)}


        readu, lun, spect
        ;print,spect.intime1,spect.intime2,spect.intime3,spect.intime4
        ;stop

        atime  = systime(0, spect.btime(0), /utc) ; convert date

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
        if fix(strmid(date,6,2))+1 eq fix(strmid(mydate,6,2)) then begin
          plus=24.
        endif else begin
          plus=0.
          if((mydate ne date) and (not stop_checking)) then begin
            print, "Skipping ",spc_files(ind),date,' ',mydate
            continue
          endif else begin
            stop_checking = 1
          endelse
        endelse

        hh = strmid(atime(0), result - 2, 2) & mm = strmid(atime(0), result + 1, 2) & ss = strmid(atime(0), result + 4, 2)
        utc = double(hh) + double(mm)/60. + double(ss)/3600. ; put hr in decimal form
        ;if utc lt utc0 then t0=24. ; add one day starting utc=24.
        ;utc0=utc
        tmhrs[ctr]=utc+plus
        if tmhrs[ctr] gt u1 then begin
          free_lun,lun
          goto,jump_out
        endif
        ;if ctr gt 500 then message,'Test'

        if(ctr ge 1) then begin
            if(abs(tmhrs(ctr)-tmhrs(ctr-1)) ge 1) then begin
                ;stop
            endif
        endif

        if (spect.shsw eq 0) then begin ; shutter open
           ; Get analogue channels and convert to voltage / temperature
            ch0 = ((spect.zsit/2048.)*5.) - 5.   ;Zenith SI spect. temp.
            ch1 = ((spect.nsit/2048.)*5.) - 5.   ;Nadir SI spect temp.
            ch2 = ((spect.zirt/2048.)*5.) - 5.   ;Zenith InGaAs temp.
            ch3 = ((spect.nirt/2048.)*5.) - 5.   ;Nadir InGaAs temp.
            ch4 = ((spect.it  /2048.)*5.) - 5.   ;Box temperature
            ch5 = ((spect.xt  /2048.)*5.) - 5.   ;Box Cooler Temperature
            ch6 = ((spect.zirx/2048.)*5.) - 5.   ;temperature ZIR housing
            ch7 = ((spect.nirx/2048.)*5.) - 5.   ;temperature NIR housing
            ;;;;;;;;;;;;;;;;; Convert Voltage to Temp ;;;;;;;;;;;;;;;;;;;;

            zsit = long(ch0 * 10000.)     ;convert to integer for use as index
            nsit = long(ch1 * 10000.)     ;convert to integer for use as index

            if (zsit le 19999) then begin
              zsitemp[ctr] = revts[zsit] ;load zenith temp into array
            endif
            if (nsit le 19999) then begin
              nsitemp[ctr] = revts[nsit]       ;load nadir temp into array
            endif

            zirtemp[ctr] = ((1./3200.)*alog(20.*ch2) + (1./298.))^(-1)-273. ;zen ir tmp
            nirtemp[ctr] = ((1./3200.)*alog(20.*ch3) + (1./298.))^(-1)-273. ;nad ir tmp
            ;;;;;;;;;;;; Third thermistor internal to the computer ;;;;;;;;;;;;;

            rtem = abs((2000. * ch4)/(1. - (0.2 * ch4)))
            dem = 1.0295e-3+(2.391e-4 * alog(rtem)) + (1.568e-7 * (alog(rtem))^3)
            bxtemp[ctr] = float(1./dem - 273.)
            rtem = abs((2000. * ch5)/(1. - (0.2 * ch5)))
            dem = 1.0295e-3+(2.391e-4 * alog(rtem)) + (1.568e-7 * (alog(rtem))^3)
            bxcltemp[ctr] = float(1./dem - 273.)
            ; new temperatures
            rtem = abs((2000. * ch6)/(1. - (0.2 * ch6)))
            dem = 1.0295e-3+(2.391e-4 * alog(rtem)) + (1.568e-7 * (alog(rtem))^3)
            zirxt[ctr] = float(1./dem - 273.)
            rtem = abs((2000. * ch7)/(1. - (0.2 * ch7)))
            dem = 1.0295e-3+(2.391e-4 * alog(rtem)) + (1.568e-7 * (alog(rtem))^3)
            nirxt[ctr] = float(1./dem - 273.)



            ; Define dark coefficients aa and bb when time is up
            if(tmhrs[ctr] gt utd1) then begin
                darkcount=darkcount+1
                read_drk,np,darkmin,dark,darku,aa,bb,zz,utd0,utd1
                print,tmhrs[ctr],utd0,utd1
            endif
                                ; Calculate darks
            if dark eq 'T' then begin
                message,'T option currently not allowed/implemented!'
                zvisdark=aa[0,*]+bb[0,*]*bxtemp[ctr] & znirdark=aa[2,*]+bb[2,*]*bxtemp[ctr]
                nvisdark=aa[1,*]+bb[1,*]*bxtemp[ctr] & nnirdark=aa[3,*]+bb[3,*]*bxtemp[ctr]
            endif else begin
                if utd1 lt 99 then begin ; not last dark
                    factor=(tmhrs[ctr]-utd0)/(utd1-utd0)
                    ;factor=0.   ; please remove later
                    if factor lt 0. then factor=0. & if factor gt 1. then factor=1.
                    zvisdark=aa[0,*]*(1.-factor)+bb[0,*]*factor ; factor determines how much of the previous
                    znirdark=aa[2,*]*(1.-factor)+bb[2,*]*factor ; and next dark is used for the current spectrum
                    nvisdark=aa[1,*]*(1.-factor)+bb[1,*]*factor ; factor determines how much of the previous
                    nnirdark=aa[3,*]*(1.-factor)+bb[3,*]*factor ; and next dark is used for the current spectrum
                endif else begin
                    zvisdark=bb[0,*]
                    znirdark=bb[2,*]
                    nvisdark=bb[1,*]
                    nnirdark=bb[3,*]
                endelse
            endelse

            ; Spectra: subtract darks, divide by integration time
            zsispectrum=(spect.zspecsi-zvisdark)/spect.intime1 ; Si spectrum
            zirspectrum=(spect.zspecir-znirdark)/spect.intime2 ; IR spectrum
            nsispectrum=(spect.nspecsi-nvisdark)/spect.intime3 ; Si spectrum
            nirspectrum=(spect.nspecir-nnirdark)/spect.intime4 ; IR spectrum

            zsierr     =zz[0,*]/spect.intime1
            zirerr     =zz[2,*]/spect.intime2
            nsierr     =zz[1,*]/spect.intime3
            nirerr     =zz[3,*]/spect.intime4

            rawzsispectrum=(spect.zspecsi-zvisdark)
            rawzirspectrum=(spect.zspecir-znirdark)
            rawnsispectrum=(spect.nspecsi-nvisdark)
            rawnirspectrum=(spect.nspecir-nnirdark)

            max_dn = 2^15-1
            if(n_elements(where(spect.zspecsi eq max_dn)) gt 1) then sat[ctr]=sat[ctr]+n_elements(where(spect.zspecsi eq max_dn))-1
            if not sat[ctr] then if(n_elements(where(spect.zspecir eq max_dn)) gt 1) then sat[ctr]=sat[ctr]+n_elements(where(spect.zspecir eq max_dn))-1
            if not sat[ctr] then if(n_elements(where(spect.nspecsi eq max_dn)) gt 1) then sat[ctr]=sat[ctr]+n_elements(where(spect.nspecsi eq max_dn))-1
            ;if(n_elements(where(spect.nspecsi eq max_dn)) gt 1) then begin
            ;  print, 'Warning: SATURATION IN NADIR SI ... continue? (y/n)'
            ;  str=''
            ;  read,form='(a)',str
            ;  if strcmp(str,'n',/FOLD_CASE) then stop
            ;endif
            if(n_elements(where(spect.nspecir eq max_dn)) gt 1) then message, 'SATURATION IN NADIR IR'

            ; Reversal of wavelengths for IR spectra ONLY
            zirreverse=fltarr(np) & nirreverse=fltarr(np)
            rawzirreverse=fltarr(np) & rawnirreverse=fltarr(np)
            zirerrrev=fltarr(np) & nirerrrev=fltarr(np)
            for k=0,np-1 do begin
                zirreverse[k] = zirspectrum[255-k]
                nirreverse[k] = nirspectrum[255-k]
                zirerrrev [k] = zirerr     [255-k]
                nirerrrev [k] = nirerr     [255-k]

                rawzirreverse[k] = rawzirspectrum[255-k]
                rawnirreverse[k] = rawnirspectrum[255-k]
            endfor

            ; Divide by the spectrometer response functions
            zsispectrum = zsispectrum/zsi(1,*)
            zirspectrum = zirreverse /zir(1,*)
            nsispectrum = nsispectrum/nsi(1,*)
            nirspectrum = nirreverse /nir(1,*)
            zsierr      = zsierr     /zsi[1,*]
            zirerr      = zirerrrev  /zir[1,*]
            nsierr      = nsierr     /nsi[1,*]
            nirerr      = nirerrrev  /nir[1,*]

            ; Join Si and IR spectra
            zjoined[0:zindsi2-zindsi1         ] = zsispectrum[zindsi1:zindsi2]
            zjoined[zindsi2-zindsi1+1:znlambda-1] = zirspectrum[zindin1:zindin2]
            zej    [0:zindsi2-zindsi1         ] = zsierr[zindsi1:zindsi2]
            zej    [zindsi2-zindsi1+1:znlambda-1] = zirerr[zindin1:zindin2]

            njoined[0:nindsi2-nindsi1         ] = nsispectrum[nindsi1:nindsi2]
            njoined[nindsi2-nindsi1+1:nnlambda-1] = nirspectrum[nindin1:nindin2]
            nej    [0:nindsi2-nindsi1         ] = nsierr[nindsi1:nindsi2]
            nej    [nindsi2-nindsi1+1:nnlambda-1] = nirerr[nindin1:nindin2]

            if zsmooth gt 1 then zjoined=smooth(zjoined,zsmooth)
            if nsmooth gt 1 then njoined=smooth(njoined,nsmooth)
            zspectra[*,ctr]=zjoined
            nspectra[*,ctr]=njoined
            zes     [*,ctr]=zej
            nes     [*,ctr]=nej

            if(doplot ge 1) then begin
                ;wset,4
                ;tmp=interpol(nspectra[*,ctr],nadlambda,zenlambda)
                ;albedo = tmp/zspectra[*,ctr]
                ;title = '
                ;title = platform + ' Albedo ' + atime + ' UTC:' + strcompress(tmhrs(ctr),/remove_all)
                ;plot,zenlambda,albedo,yrange=[0,1],xtitle = 'Wavelength (nm)', $
                ;  ytitle = 'Albedo', xrange = [300,2200],$
                ;  xstyle=1, /ystyle, charsize = 1.5, charthick = 1.5, $
                ;  title = title,color=3,thick=1.5

                wset,0
                title = platform + ' Raw Zenith ' + atime + ' UTC:' + strcompress(tmhrs(ctr),/remove_all)
                ymax = max([max(rawzsispectrum),max(rawzirreverse(0:200))])*1.04
                ymin = 0
                if(spect.shsw eq 1) then begin
                    ymin = -225
                    ymax = 225
                endif
                plot, wlvisz, rawzsispectrum, xtitle = 'Wavelength (nm)', $
                  ytitle = 'DN', xrange = [300,2200],$
                  xstyle=1, /ystyle, charsize = 1.5, charthick = 1.5, $
                  title = title,color=3,thick=1.5,yrange=[ymin,ymax]
                oplot,wlnirz,rawzirreverse
                if(doplot ge 2) then begin
                    if((ctr MOD doplot) eq 0) then begin
                        filestr = dir + platform + '_Raw_Zenith_UTC_' + strcompress(tmhrs(ctr),/remove_all)+'.png'
                        rgb=tvrd(true=1)
                        write_png,filestr,rgb
                    endif
                endif
                wset,1
                title = platform + ' Raw Nadir ' + atime + ' UTC:' + strcompress(tmhrs(ctr),/remove_all)
                ymax = max([max(rawnsispectrum),max(rawnirreverse(0:200))])*1.04
                if(spect.shsw eq 1) then begin
                    ymin = -225
                    ymax = 225
                endif
                plot, wlvisn, rawnsispectrum, xtitle = 'Wavelength (nm)', $
                  ytitle = 'DN', xrange = [300,2200],$
                  xstyle=1, /ystyle, charsize = 1.5, charthick = 1.5, $
                  title = title, color=3,thick=1.5,yrange=[ymin,ymax]
                oplot,wlnirn,rawnirreverse
                if(doplot ge 2) then begin
                    if((ctr MOD doplot) eq 0) then begin
                        filestr = dir + platform + '_Raw_Nadir_UTC_' + strcompress(tmhrs(ctr),/remove_all)+'.png'
                        rgb=tvrd(true=1)
                        write_png,filestr,rgb
                    endif
                endif
                wset,2
                title = platform + ' Zenith ' + atime + ' UTC:' + strcompress(tmhrs(ctr),/remove_all)
                ymax = max(zsispectrum(0:200))*1.04
                if(spect.shsw eq 1) then begin
                    ymin = -.1
                    ymax = .1
                endif
                plot, wlvisz, zsispectrum, xtitle = 'Wavelength (nm)', $
                  ytitle = 'Irradiance (Wm!E-2!Nnm!E-1!N)', xrange = [300,2200],$
                  xstyle=1, /ystyle, charsize = 1.5, charthick = 1.5, $
                  title = title, color=3,thick=1.5,yrange=[ymin,ymax]
                oplot, wlnirz, zirspectrum
                if(doplot ge 2) then begin
                    if((ctr MOD doplot) eq 0) then begin
                        filestr = dir + platform + '_Zenith_UTC_' + strcompress(tmhrs(ctr),/remove_all)+'.png'
                        rgb=tvrd(true=1)
                        write_png,filestr,rgb
                    endif
                endif
                wset,3
                title = platform + ' Nadir ' + atime + ' UTC:' + strcompress(tmhrs(ctr),/remove_all)
                ymax = max(nsispectrum(0:200))*1.04
                if(spect.shsw eq 1) then begin
                    ymin = -.1
                    ymax = .1
                endif
                plot, wlvisn, nsispectrum, xtitle = 'Wavelength (nm)', $
                  ytitle = 'Irradiance (Wm!E-2!Nnm!E-1!N)', xrange = [300,2200],$
                  xstyle=1, /ystyle, charsize = 1.5, charthick = 1.5, $
                  title = title, color=3,thick=1.5,yrange=[ymin,ymax]
                oplot, wlnirn, nirspectrum
                if(doplot ge 2) then begin
                    if((ctr MOD doplot) eq 0) then begin
                        filestr = dir + platform + '_Nadir_UTC_' + strcompress(tmhrs(ctr),/remove_all)+'.png'
                        rgb=tvrd(true=1)
                        write_png,filestr,rgb
                    endif
                endif
            endif
            ctr = ctr + 1L      ; count up spectrum number
        endif                   ; shutter open
    ;endif else begin
    ;    if(tmhrs[ctr] ge timeMax) then stop
    ;endelse


        slast = spect.shsw
    endwhile ; while not eof(lun) - runs until all spectra from file are read
    free_lun, lun               ; ...and closes file afterwards
endfor                          ; primary loop - go through files
print

jump_out:

tmhrs   = tmhrs  [0:ctr-1]
zspectra = zspectra[*,0:ctr-1]
nspectra = nspectra[*,0:ctr-1]
zes      = zes[*,0:ctr-1]
nes      = nes[*,0:ctr-1]
nsitemp  = nsitemp [0:ctr-1]
zsitemp  = zsitemp [0:ctr-1]
nirtemp  = nirtemp [0:ctr-1]
zirtemp  = zirtemp [0:ctr-1]
bxtemp  = bxtemp [0:ctr-1]


; prepare satus string array to be saved with IDL out file
flight_date   =strmid(date,0,4)+' '+strmid(date,4,2)+' '+strmid(date,6,2)
systime       =systime(/julian)
caldat,systime,mm,dd,yyyy
pro_date =strcompress(string(yyyy,format='(I4.4)'),/remove_all)+' '+strcompress(string(mm,format='(I2.2)'),/remove_all)+' '+strcompress(string(dd,format='(I2.2)'),/remove_all)
platform=cfg(cfg_file,'platform')
satstat=where(sat gt 0,satcount)
status = ['Flight date         : '+flight_date,$
          'Processing date     : '+pro_date,$
          'Platform            : '+platform,$
          'Integration times   : ',$
          '         '+string(spect.intime1)+string(spect.intime2)+string(spect.intime3)+string(spect.intime4),$
          'Response functions  :',$
          'Zenith Si           : '+zrespsi1,$
          'Zenith InGaAs       : '+zrespir1,$
          'Nadir Si            : '+nrespsi1,$
          'Nadir InGaAs        : '+nrespir1,$
          'Dark Mode           : '+dark,$
          'Saturation          : '+string(satcount)+' spectra /'+string(ctr),$
          'Joinder Zenith      : '+string(zjoin),$
          'Joinder Nadir       : '+string(njoin),$
          'Number data files   : '+string(numfiles),$
          'Number of darks     : '+string(darkcount),$
          'Data file location  : '+data]
nstat=n_elements(status)
for ii=0,nstat-1 do print,status[ii]

if doplot then begin
  wdelete,0
  wdelete,1
  wdelete,2
  wdelete,3
endif


; write variables to an IDL save fileq
; tmhrs   - UTC time in decimal hours
; lambda   - an array containing all the wavelengths observed
; spectra - spectrum time series at all wavelengths observed

; zer is the mean/min/max spectral error resulting from the dark currents for zenith
zer=fltarr(znlambda,3)
for i=0,znlambda-1 do begin
  zer[i,0]=max(zes[i,*])
  zer[i,1]=mean(zes[i,*])
  zer[i,2]=min(zes[i,*])
endfor

; ner is the mean/min/max spectral error resulting from the dark currents for nadir
ner=fltarr(nnlambda,3)
for i=0,nnlambda-1 do begin
  ner[i,0]=max(nes[i,*])
  ner[i,1]=mean(nes[i,*])
  ner[i,2]=min(nes[i,*])
endfor

outfile = strcompress(outdir +l+strcompress(date,/remove_all)+'_calibspcs_post.out',/REMOVE_ALL)
save, tmhrs, zenlambda, zspectra, zer, nadlambda, nspectra, ner, sat, status, filename = outfile
spawn, 'chmod a+w '+outfile

outfile = strcompress(outdir+l+strcompress(date,/remove_all)+'_temp_post.out',/REMOVE_ALL)
save, tmhrs, nsitemp, zsitemp, nirtemp, zirtemp, bxtemp, bxcltemp, nirxt, zirxt,filename = outfile
spawn, 'chmod a+w '+outfile

stop
end
