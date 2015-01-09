;+
; NAME:
;   calnex_test_cal
;
; PURPOSE:
;   Read in the data for one day, plot irradiance with different response functions
;   Compare the irradiance measured by the Silicon and the irradiance measured by the InGaAs
;
; CATEGORY:
;   CALNEX / calibrations
;
; CALLING SEQUENCE:
;   calnex_test_cal
;   
; OUTPUT:
;   plots
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
;   calnex_test_cal
;
; MODIFICATION HISTORY:
; Written:  Samuel LeBlanc, LASP CU Boulder, June 13th, 2011
; Modified:    
;---------------------------------------------------------------------------
 @cfg.pro
 @calnex_dark.pro
pro calnex_test_cal

months = ["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"]
stop_checking = 0

indir  = '/data/seven/schmidt/calnex/p3'           ; directory
date='20100519'            ; date

l      = '/'                      ; directory separator
dir    = indir + l + date
outdir = dir

cfg_file=dir+l+date+'.cfg'        ; build cfg file
if file_test(cfg_file) ne 1 then message,'Did not find configuration file.'

if (n_elements(doplot) eq 0) then begin
    doplot = (strcmp(strmid(cfg(cfg_file,'plot'),0,1),'y'))
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

; Use multiple response functions

print, 'Found '+string(zrsi)+' zrsi'
if zrsi gt 1 then begin      ; found more than one SI response function
  resp_date = strarr(zrsi)
  for ii=0,zrsi-1 do begin
    tmp = strsplit(zrespsi1[ii],'_',/extract)
    resp_date[ii] = tmp[0]
  endfor
  zrespsi1 = respdir+zrespsi1[sort(resp_date)]  ; for multiple possible response functions, sort them by date
endif
if nrsi gt 1 then begin      ; found more than one SI response function
  resp_date = strarr(nrsi)
  for ii=0,nrsi-1 do begin
    tmp = strsplit(nrespsi1[ii],'_',/extract)
    resp_date[ii] = tmp[0]
  endfor
  nrespsi1 = respdir+nrespsi1[sort(resp_date)]  ; for multiple possible response functions, sort them by date
endif

print, 'Found '+string(zrir)+' zrir'
if zrir gt 1 then begin      ; found more than one IR response function
  resp_date = strarr(zrir)
  for ii=0,zrir-1 do begin
    tmp = strsplit(zrespir1[ii],'_',/extract)
    resp_date[ii] = tmp[0]
  endfor
  zrespir1 = respdir+zrespir1[sort(resp_date)]  ; for multiple possible response functions, sort them by date
endif
if nrir gt 1 then begin      ; found more than one IR response function
  resp_date = strarr(nrir)
  for ii=0,nrir-1 do begin
    tmp = strsplit(nrespir1[ii],'_',/extract)
    resp_date[ii] = tmp[0]
  endfor
  nrespir1 = respdir+nrespir1[sort(resp_date)]  ; for multiple possible response functions, sort them by date
endif

; Read zenith response functions into "si" and "ir", respectively
zsi = fltarr(zrsi, 2, 256)
tm=fltarr(2,256)
for i=0, zrsi-1 do begin
  PRINT, 'Opening :', zrespsi1[i]
  openr,zsilun,zrespsi1[i],/get_lun  
  readf,zsilun,tm
  zsi[i,*,*]=tm                
  free_lun,zsilun                
endfor
zir = fltarr(zrir,2, 256)
for i=0, zrir-1 do begin
  PRINT, 'Opening :', zrespir1[i]
  openr,zirlun,zrespir1[i],/get_lun
  readf,zirlun,tm
  zir[i,*,*]=tm
  free_lun,zirlun
endfor

; Read nadir response functions into "si" and "ir", respectively
nsi = fltarr(nrsi, 2, 256)
for i=0, nrsi-1 do begin
  openr,nsilun,nrespsi1[i],/get_lun  
  readf,nsilun,tm
  nsi[i,*,*]=tm                
  free_lun,nsilun                
endfor
nir = fltarr(nrir,2, 256)
for i=0, nrir-1 do begin
  openr,nirlun,nrespir1[i],/get_lun
  readf,nirlun,tm
  nir[i,*,*]=tm
  free_lun,nirlun
endfor

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

; Initialize: get wavelengths of joined spectra --- this is not used
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
bignum  = 30000L ; defines maximum spectrum number - increase if needed

zirspect_cals=fltarr(zrir,n_elements(wlnirz), bignum)  ;initialize all the various possible spectras, with various response functions
zsispect_cals=fltarr(zrsi,n_elements(wlvisz), bignum)
nirspect_cals=fltarr(nrir,n_elements(wlnirn), bignum)
nsispect_cals=fltarr(nrsi,n_elements(wlvisn), bignum)


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Read in all the *.OSA2 files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

            ; the spectra with various different response functions
            ; These are named 'spect_cals'
            for k=0, zrir-1 do begin
              zirspect_cals[k,*,ctr]=zirreverse /zir(k,1,*)
              zirerr      = zirerrrev  /zir[k,1,*]
            endfor
            for k=0, zrsi-1 do begin
              zsispect_cals[k,*,ctr]=zsispectrum/zsi(k,1,*)
              zsierr      = zsierr     /zsi[k,1,*]
            endfor
            for k=0, nrir-1 do begin
              nirspect_cals[k,*,ctr]=nirreverse /nir(k,1,*) 
              nirerr      = nirerrrev  /nir[k,1,*]
            endfor
            for k=0, nrsi-1 do begin
              nsispect_cals[k,*,ctr]=nsispectrum/nsi(k,1,*) 
              nsierr      = nsierr     /nsi[k,1,*]
            endfor 
            nsispectrum = nsispectrum/nsi(0,1,*) ;regular spectra with first response function
            nirspectrum = nirreverse /nir(0,1,*)              
            zsispectrum = zsispectrum/zsi(0,1,*)
            zirspectrum = zirreverse /zir(0,1,*)
            
            ; Join Si and IR spectra --- not used
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

            ctr = ctr + 1L      ; count up spectrum number
        endif                   ; shutter open

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
nirspect_cals=nirspect_cals[*,*,0:ctr-1]
nsispect_cals=nsispect_cals[*,*,0:ctr-1]
zirspect_cals=zirspect_cals[*,*,0:ctr-1]
zsispect_cals=zsispect_cals[*,*,0:ctr-1]


; get the proper wavelength indices for plotting, set 4 wavelengths for plotting, then the wlsi_920 is the lowest, and the wlsi_1020 is the highest
wvl=[1020.,1000.,980.,900.]
kk=min(abs(wlnirz-wvl[0]),wlir_1020)
kk=min(abs(wlvisz-wvl[0]),wlsi_1020)
kk=min(abs(wlnirz-wvl[1]),wlir_1000)
kk=min(abs(wlvisz-wvl[1]),wlsi_1000)
kk=min(abs(wlnirz-wvl[2]),wlir_980)
kk=min(abs(wlvisz-wvl[2]),wlsi_980)
kk=min(abs(wlnirz-wvl[3]),wlir_920)
kk=min(abs(wlvisz-wvl[3]),wlsi_920)

wlir_in=indgen(wlir_1020-wlir_920)+wlir_920   ; build the wavelength indices for the ir zenith
wlsi_inn=indgen(wlsi_1020-wlsi_920)+wlsi_920  ; build the wavelength indices for the si zenith
n_wl=n_elements(wlir_in)
wlsi_in=fltarr(n_wl)


; plotting of the different irradiances (measured by ingaas vs. by si)

;plotting the time trace of the irradiance at a few different wavelengths and the silicon and ingaas
   set_plot, 'ps'
  loadct, 39,/silent
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8.
  device, filename=dir+l+'test_cal.ps'
  device, xsize=80, ysize=60
  !p.font=1 & !p.thick=5 & !p.charsize=1.8 & !x.style=1
  !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !x.margin=[6,3] & !y.margin=[3,2]
  !p.multi=[0,4,3]
  
  for k=0, zrir-1 do begin
    plot, zsispect_cals[k,wlsi_1020,*], psym=3, title='time trace of irradiance at cal:'+resp_date[k], ytitle='irradiance'
    oplot, zirspect_cals[k,wlir_1020,*], psym=3,color=30
    oplot, zsispect_cals[k,wlsi_1000,*], psym=3,color=70
    oplot, zirspect_cals[k,wlir_1000,*], psym=3, color=90
    oplot, zsispect_cals[k,wlsi_980,*], psym=3,color=130
    oplot, zirspect_cals[k,wlir_980,*], psym=3, color=150 
    oplot, zsispect_cals[k,wlsi_920,*], psym=3,color=200
    oplot, zirspect_cals[k,wlir_920,*], psym=3, color=250 
  
    legend, ['1020 nm SI','1020 nm IR','1000 nm SI','1000 nm IR','980 nm SI','980 nm IR', '900 nm SI', '900 nm IR'], textcolors=[0,30,70,90,130,150,200,250], box=0
  endfor

  device, /close
  spawn, 'convert '+dir+l+'test_cal.ps '+dir+l+'test_cal_time_sec.png'
  spawn, 'rm -f '+dir+l+'test_cal.ps'
  
  
 ;plotting all irradiance of ingaas vs. silicon at a few wavelengths
   set_plot, 'ps'
  loadct, 39,/silent
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8.
  device, filename=dir+l+'test_cal.ps'
  device, xsize=80, ysize=60
  !p.font=1 & !p.thick=5 & !p.charsize=1.8 & !x.style=1
  !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !x.margin=[6,3] & !y.margin=[3,2]
  !p.multi=[0,4,3]
  sl=fltarr(zrir,4)
  in=fltarr(zrir,4)
  sl_e=sl
  in_e=in
  
  for k=0, zrir-1 do begin
    plot, zsispect_cals[k,wlsi_1020,*], zirspect_cals[k,wlir_1020,*], title='Zenith Calibration for: '+resp_date[k], xtitle='SI irradiance', ytitle='InGaAs irradiance', psym=3, xrange=[0, max(zsispect_cals[k,wlsi_1020,*])]
    oplot, zsispect_cals[k,wlsi_1000,*], zirspect_cals[k,wlir_1000,*], psym=3, color=70
    oplot, zsispect_cals[k,wlsi_980,*], zirspect_cals[k,wlir_980,*], psym=3, color=130
    oplot, zsispect_cals[k,wlsi_920,*], zirspect_cals[k,wlir_920,*], psym=3, color=250
    a=linfit(zsispect_cals[k,wlsi_1020,*], zirspect_cals[k,wlir_1020,*],sigma=ae)
    b=linfit(zsispect_cals[k,wlsi_1000,*], zirspect_cals[k,wlir_1000,*],sigma=be)
    c=linfit(zsispect_cals[k,wlsi_980,*], zirspect_cals[k,wlir_980,*],sigma=ce)
    d=linfit(zsispect_cals[k,wlsi_920,*], zirspect_cals[k,wlir_920,*],sigma=de) 
    
    sl[k,0]=a[1] & sl[k,1]=b[1] & sl[k,2]=c[1] & sl[k,3]=d[1]
    in[k,0]=a[0] & in[k,1]=b[0] & in[k,2]=c[0] & in[k,3]=d[0]
    sl_e[k,0]=ae[1] & sl_e[k,1]=be[1] & sl_e[k,2]=ce[1] & sl_e[k,3]=de[1]
    in_e[k,0]=ae[0] & in_e[k,1]=be[0] & in_e[k,2]=ce[0] & in_e[k,3]=de[0]
    
    legend, ['1020 nm','1000 nm','980 nm', '900 nm'], textcolors=[0,70,130,250], box=0
    legend, ['slope:'+string(a[1],format='(F9.3)')+' int:'+string(a[0],format='(F6.3)'), 'slope:'+string(b[1],format='(F9.3)')+' int:'+string(b[0],format='(F5.2)'),$
     'slope:'+string(c[1],format='(F9.3)')+' int:'+string(c[0],format='(F6.3)'),'slope:'+string(d[1],format='(F9.3)')+' int:'+string(d[0],format='(F5.2)'), 'mean slope:'+string(mean([a[1],b[1],c[1],d[1]]),format='(F9.3)')+' int:'+string(mean([a[0],b[0],c[0],d[0]]),format='(F5.2)')],$
     textcolors=[0,70,130,250,0], box=0, /right, /bottom
  endfor
  device, /close
  spawn, 'convert '+dir+l+'test_cal.ps '+dir+l+'test_cal_sec.png'
  spawn, 'rm -f '+dir+l+'test_cal.ps'

  
; plot the slopes and intercepts of all the irradiance measured by the silicon and ingaas over the overlapping wavelengths 
  set_plot, 'ps'
  loadct, 39,/silent
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8.
  device, filename=dir+l+'test_cal.ps'
  device, xsize=40, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=1.8 & !x.style=1
  !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !x.margin=[6,3] & !y.margin=[3,2]
  !p.multi=[0,2,1]
  sl=fltarr(zrir,n_wl)   ; build the slopes and intercept arrays (sl and in) as well as the standard deviations arrays (sle and ine)
  sle=fltarr(zrir,n_wl)
  in=fltarr(zrir,n_wl)
  ine=fltarr(zrir,n_wl)
  
  for k=0, zrir-1 do begin   ; loop through all the various response functions
    for ii=0, n_wl-1 do begin   ; loop through all the wavelengths
      temp=fltarr(n_elements(zsispect_cals[k,wlsi_inn[ii],*]),n_wl)  ; setup a temp array to hold the interpolated calibrated spectra 
      for jj=0L, n_elements(zsispect_cals[k,wlsi_inn[ii],*])-1L do begin ; loop through all the points during that day
        temp[jj,*]=interpol(zsispect_cals[k,wlsi_inn[0]:wlsi_inn[n_elements(wlsi_inn)-1],jj],wlvisz[wlsi_inn],wlnirz[wlir_in])  ; interpol the silicon spectra to the ingaas wavelengths
      endfor
 
      filt=where(temp[*,ii]/zirspect_cals[k,wlir_in[ii],*] le 1.1 and temp[*,ii]/zirspect_cals[k,wlir_in[ii],*] ge 0.9, ct) ; setup up a filter to take out the larger than 10% deviation points
      if ct lt 1 then begin
      a=[!values.f_nan,!values.f_nan] ; if all the points are larger than 10% then set the slope and intercept to a NaN
      ae=a
      endif else a=linfit(temp[filt,ii], zirspect_cals[k,wlir_in[ii],filt],sigma=ae)  ; else get the slope and intercept and standard deviations
      sl[k,ii]=a[1] & sle[k,ii]=ae[1] ; set them to the arrays
      in[k,ii]=a[0] & ine[k,ii]=ae[0]    
    endfor
  endfor
  
 ; now plot the slope of the silicon-to-ingaas values vs. wavelengths of the ingaas
  plot, wlnirz[wlir_in], sl[0,*], title='Slope of Si and InGaAs irradiances', xtitle='Wavelength (nm)', ytitle='Slope', yrange=[0.8,1.2], xrange=[840.,1040.], ystyle=8, xmargin=[6,6]
  errplot,wlnirz[wlir_in], sl[0,*]- sle[0,*], sl[0,*]+sle[0,*] ;plot the standard deviation
  cl=intarr(zrir)
  b=fltarr(zrir)
  cl[0]=0
  b[0]=mean(sl[0,11:*],/nan) ;calculate the mean slope of all wavelengths - should be modified to include only the pertinent wavelengths
  print, resp_date[0],b[0]
  for k=1,zrir-1 do begin ;loop through all the other spectra with various response functions
    cl[k]=k*255/(zrir-1)
    oplot, wlnirz[wlir_in], sl[k,*], color=cl[k];,psym=2
    errplot,wlnirz[wlir_in], sl[k,*]- sle[k,*], sl[k,*]+sle[k,*],color=cl[k]
    b[k]=mean(sl[k,11:*],/nan)
    print, resp_date[k], b[k]
  endfor 
  axis, yaxis=1,ystyle=1, yrange=[0.2,max(temp[2000,*])*1.05],ytitle='Irradiance', /save ; over plot the measured irradiance at on particular point
  OPLOT, wlnirz[wlir_in], zirspect_cals[n_elements(resp_date)-1,wlir_in,2000], linestyle=1 
  OPLOT, wlvisz[wlsi_in], temp[2000,*], linestyle=3
  legend, resp_date, textcolors=cl, box=0
  
  ; now plot the same for the intercept
    plot, wlnirz[wlir_in], in[0,*], title='Intercept of Si and InGaAs irradiances', xtitle='Wavelength (nm)', ytitle='Intercept', psym=2, yrange=[-0.05,0.05], xrange=[840.,1040.]
  errplot,wlnirz[wlir_in], in[0,*]- ine[0,*], in[0,*]+ine[0,*]
  cl=intarr(zrir)
  for k=1,zrir-1 do begin
    cl[k]=k*255/(zrir-1)
    oplot, wlnirz[wlir_in], in[k,*], color=cl[k],psym=2
    errplot,wlnirz[wlir_in], in[k,*]- ine[k,*], in[k,*]+ine[k,*],color=cl[k]
  endfor 
  legend, resp_date, textcolors=cl, box=0
device, /close
  spawn, 'convert '+dir+l+'test_cal.ps '+dir+l+'test_cal_slope_sec.png'
  spawn, 'rm -f '+dir+l+'test_cal.ps'

stop
end
