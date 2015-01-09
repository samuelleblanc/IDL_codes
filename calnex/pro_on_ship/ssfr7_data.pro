@cfg.pro
@ssfr7_dark.pro
@read_cg4_knorr.pro
@read_nav_knorr.pro

pro ssfr7_data,indir,inputdate,doplot                     ; input: dir

if(doplot) then begin
    device,decomposed=0
    loadct,27,/silent
    !P.multi=0
endif

months = ["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"]
stop_checking = 0

l  ='/'                         ; directory separator
dir = indir + l + inputdate
outdir = dir

inputdate=strmid(inputdate,0,8)

cfg_file=dir+l+'ssfr7.cfg'        ; build cfg file (Knorr specific)
if file_test(cfg_file) ne 1 then message,'Did not find configuration file.'

qlwl=cfg(cfg_file,'wlql')
qlwl=strsplit(qlwl,' ,',escape='#',/EXTRACT)
qlwl=float(qlwl)
nqlwl=n_elements(qlwl)

if (n_elements(doplot) eq 0) then begin
    doplot = (strcmp(strmid(cfg(cfg_file,'plot'),0,1),'y'))
endif

if (doplot ge 1) then begin
    xsize=600
    ysize=400
    window,0,title='Raw Zenith',xsize=xsize,ysize=ysize,retain=2
    window,1,title='Raw Nadir',xsize=xsize,ysize=ysize,retain=2
    window,2,title='Zenith',xsize=xsize,ysize=ysize,retain=2
    window,3,title='Nadir',xsize=xsize,ysize=ysize,retain=2
    ;;window,4,title='Joined',xsize=xsize,ysize=ysize,retain=2
endif


; Read parameters from configuration file
sinp     =cfg(cfg_file,'sinp'  )    ; number of channels for each si spectrum
sinp     =fix(sinp)

innp     =cfg(cfg_file,'innp'  )    ; number of channels for each si spectrum
innp     =fix(innp)
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
temp   =dir+l+temp              ; append file name to path
;
zrespsi =cfg(cfg_file,'zresponseSI') ; response function Si
zrespir =cfg(cfg_file,'zresponseIR') ; response function IR
nrespsi =cfg(cfg_file,'nresponseSI') ; response function Si
nrespir =cfg(cfg_file,'nresponseIR') ; response function IR
;respsi =dir+l+'cal'+l+respsi      ; append file name to path
;respir =dir+l+'cal'+l+respir      ; append file name to path
respdir =dir+l+'../../cal'+l          ; append file name to path
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

read_nav = (cfg(cfg_file,'read_nav') eq 'yes')
do_retrieval = (cfg(cfg_file,'do_retrieval') eq 'yes')

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
;print, 'Zenith Response function SI: ',respdir+zrespsi
;print, '                         IR: ',respdir+zrespir
;print, 'Nadir  Response function SI: ',respdir+nrespsi
;print, '                         IR: ',respdir+nrespir

;No Dark files?  Unique situation - likely only occurs with quick test data
if(not (dark eq 'NONE')) then begin
    ini_dark,spc_files,sinp,innp,darkmin,dark,darku,darki,darkt,siaa,sibb,inaa,inbb,utd0,utd1,inputdate
endif

; Initialize: get response functions
cd,respdir,current=prevdir
;print, zrespsi,zrespir,nrespsi,nrespir

zrespsi1 = file_search(zrespsi,count=zrsi,/FOLD_CASE)
zrespir1 = file_search(zrespir,count=zrir,/FOLD_CASE)

nrespsi1 = file_search(nrespsi,count=nrsi,/FOLD_CASE)
nrespir1 = file_search(nrespir,count=nrir,/FOLD_CASE)

cd,prevdir

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
  resp_index = max(where(resp_date le inputdate)) ; use the latest response function, in the standard setting
  print, "Using zenith Si response function ", zrespsi1[resp_index]
  zrespsi1 = respdir+zrespsi1[resp_index]
endif else begin
    print, "Using zenith Si response function ", zrespsi1
    zrespsi1 = respdir+zrespsi1
endelse
if nrsi gt 1 then begin ; found more than one SI response function
  resp_date = lonarr(nrsi)
  for ii=0,nrsi-1 do begin
    tmp = strsplit(nrespsi1[ii],'_',/extract)
    resp_date[ii] = long(tmp[0])
  endfor
  resp_index = max(where(resp_date le inputdate)) ; use the latest response function, in the standard setting
  print, "Using nadir Si response function ", nrespsi1[resp_index]
  nrespsi1 = respdir+nrespsi1[resp_index]
endif else begin
    print, "Using nadir Si response function ", nrespsi1
    nrespsi1 = respdir+nrespsi1
endelse

if zrir gt 1 then begin      ; found more than one IR response function
  resp_date = lonarr(zrir)
  for ii=0,zrir-1 do begin
    tmp = strsplit(zrespir1[ii],'_',/extract)
    resp_date[ii] = long(tmp[0])
  endfor
  resp_index = max(where(resp_date le inputdate)) ; use the latest response function, in the standard setting
  print, "Using zenith InGaAs response function ", zrespir1[resp_index]
  zrespir1 = respdir+zrespir1[resp_index]
endif else begin
    print, "Using zenith InGaAs response function ", zrespir1
    zrespir1 = respdir+zrespir1
endelse
if nrir gt 1 then begin      ; found more than one IR response function
  resp_date = lonarr(nrir)
  for ii=0,nrsi-1 do begin
    tmp = strsplit(nrespir1[ii],'_',/extract)
    resp_date[ii] = long(tmp[0])
  endfor
  resp_index = max(where(resp_date le inputdate)) ; use the latest response function, in the standard setting
  print, "Using nadir InGaAs response function ", nrespir1[resp_index]
  nrespir1 = respdir+nrespir1[resp_index]
endif else begin
    print, "Using nadir InGaAs response function ", nrespir1
    nrespir1 = respdir+nrespir1
endelse

; Read zenith response functions into "si" and "ir", respectively
zsi = fltarr(2, sinp)            & zir = fltarr(2, innp)
openr,zsilun,zrespsi1,/get_lun  & openr,zirlun,zrespir1,/get_lun
readf,zsilun,zsi                & readf,zirlun,zir
free_lun,zsilun                 & free_lun,zirlun

; Read nadir response functions into "si" and "ir", respectively
nsi = fltarr(2, sinp)             & nir = fltarr(2, innp)
openr,nsilun,nrespsi1,/get_lun   & openr,nirlun,nrespir1,/get_lun
readf,nsilun,nsi                 & readf,nirlun,nir
free_lun,nsilun                  & free_lun,nirlun

; Initialize: get wavelengths
wlvisz=fltarr(sinp) & wlnirz=fltarr(innp)
for i=0,sinp-1 do begin
    for j=0,znlsi-1 do wlvisz[i]     =wlvisz[i]     +zlambdasi[j]*float(i)^j
endfor
for i=0,innp-1 do begin
;    for j=0,znlir-1 do wlnirz[innp-1-i]=wlnirz[innp-1-i]+zlambdair[j]*float(i)^j
    for j=0,znlir-1 do wlnirz[i]     =wlnirz[i]     +zlambdair[j]*float(i)^j
endfor

wlvisn=fltarr(sinp) & wlnirn=fltarr(innp)
for i=0,sinp-1 do begin
   for j=0,nnlsi-1 do wlvisn[i]     =wlvisn[i]     +nlambdasi[j]*float(i)^j
endfor
for i=0,innp-1 do begin
;    for j=0,nnlir-1 do wlnirn[innp-1-i]=wlnirn[innp-1-i]+nlambdair[j]*float(i)^j
    for j=0,nnlir-1 do wlnirn[i]    =wlnirn[i]     +nlambdair[j]*float(i)^j
endfor
;stop
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

; Initialize variables / allocate memory
bignum  = 60000L ; defines maximum spectrum number - increase if needed

tmhrs   = dblarr(bignum)        ; time array to hold all UTC times
zjoined  = fltarr(znlambda)        ; joined spectrum
njoined  = fltarr(nnlambda)        ; joined spectrum
zspectra = fltarr(znlambda,bignum) ; time series of joined spectrum
nspectra = fltarr(nnlambda,bignum) ; time series of joined spectrum
sitemp  = fltarr(bignum)        ; array to hold the SI spec. temp
bxtemp  = fltarr(bignum)       ; array to hold the computer temp
bxcltemp= fltarr(bignum)       ; array to hold the inner box temp
nirtemp = fltarr(bignum)        ; array to hold the Nadir InGaAs spec. temp
zirtemp = fltarr(bignum)        ; array to hold the Zenith InGaAs spec. temp
nsitemp = fltarr(bignum)        ; array to hold the Nadir Si spec. temp
zsitemp = fltarr(bignum)        ; array to hold the Zenith Si spec. temp
sat = intarr(bignum)
sat4 = bytarr(4,bignum)
rawzsispectra = fltarr(sinp,bignum)
rawzirspectra = fltarr(innp,bignum)
rawnsispectra = fltarr(sinp,bignum)
rawnirspectra = fltarr(innp,bignum)

ctr = 0L                        ; Counter for nondark spectra
darkcount=0                     ; Counter for dark cycles
slast = 0
for ind = 0, numfiles -1 do begin ; Primary Loop
                                ; print,spc_files[ind]
    print, "Opening ",spc_files(ind)
    print,format='(a1,$)',"."
    openr, lun, spc_files(ind), /get_lun
    while not eof(lun) do begin ; Read individual spectra
        spect = {btime: lonarr(2), bcdtimstp:  bytarr(12),intime1: long(0),$
                 intime2: long(0), intime3: long(0), intime4: long(0),$
                 accum: long(0), shsw:long(0), ad0: ulong(0),ad1: ulong(0),$
                 ad2: ulong(0), ad3: ulong(0), ad4: ulong(0), ad5: ulong(0),$
                 ad6: ulong(0), ad7: ulong(0),$
                 zspecsi: intarr(sinp), zspecir: intarr(sinp), nspecsi: intarr(sinp),$
                 nspecir: intarr(sinp)}
        
                 
        readu, lun, spect
        ;print, ctr, ind, spect.shsw
        ;if spect.shsw eq 0 then stop
        spect={btime:spect.btime, zspecir:spect.zspecir[0:innp-1], nspecir:spect.nspecir[0:innp-1], bcdtimstp:spect.bcdtimstp, $
              intime1:spect.intime1,intime2:spect.intime2, intime3:spect.intime3, intime4:spect.intime4, $
              accum: spect.accum, shsw:spect.shsw, ad0: spect.ad0,ad1: spect.ad1,$
                 ad2: spect.ad2, ad3: spect.ad3, ad4:spect.ad4, ad5: spect.ad5,$
                 ad6: spect.ad6, ad7: spect.ad7,$
                 zspecsi: spect.zspecsi, nspecsi: spect.nspecsi}

;        print,spect.intime1,spect.intime2,spect.intime3,spect.intime4
;        stop

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

        datadate = strtrim(string(year,mon,day),1)

        if((datadate ne inputdate) and (not stop_checking)) then begin
            print, "Skipping ",spc_files(ind),'    ',inputdate,'   ',datadate
            ;;stop
            continue
        endif else begin
            stop_checking = 1
        endelse

        hh = strmid(atime(0), result - 2, 2) & mm = strmid(atime(0), result + 1, 2) & ss = strmid(atime(0), result + 4, 2)
        utc = double(hh) + double(mm)/60. + double(ss)/3600. ; put hr in decimal form
        ;if utc lt utc0 then t0=24. ; add one day starting utc=24.
        ;utc0=utc
        tmhrs[ctr]=utc;+t0
        ;if( tmhrs[ctr] ge timeMin and tmhrs[ctr] le timeMax)then begin
        if(ctr ge 1) then begin
            if(abs(tmhrs(ctr)-tmhrs(ctr-1)) ge 1) then begin
                ;stop
            endif
        endif

        if (spect.shsw eq 0) then begin ; shutter open
           ; Get analogue channels and convert to voltage / temperature
            ch0 = ((spect.ad0/2048.)*5.) - 5. ;Zenith SI spect. temp.
            ch1 = ((spect.ad1/2048.)*5.) - 5. ;Nadir SI spect temp.
            ch2 = ((spect.ad2/2048.)*5.) - 5. ;Zenith InGaAs temp.
            ch3 = ((spect.ad3/2048.)*5.) - 5. ;Nadir InGaAs temp.
            ch4 = (((spect.ad4/2048.)*5.) - 5.) ;Box temperature
            ch5 = (((spect.ad5/2048.)*5.) - 5.) ;Box Cooler Temperature
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
            bxtemp[ctr] = float(1./dem - 273.) ;holds internal temp
            rtem = abs((2000. * ch5)/(1. - (0.2 * ch5)))
            dem = 1.0295e-3+(2.391e-4 * alog(rtem)) + (1.568e-7 * (alog(rtem))^3)
            bxcltemp[ctr] = float(1./dem - 273.) ;holds internal temp

            if(not(dark eq 'NONE')) then begin
                ;; Define dark coefficients aa and bb when time is up
                if(tmhrs[ctr] gt utd1) then begin
                    darkcount=darkcount+1
                    read_drk,sinp,innp,darkmin,dark,darku,siaa,sibb,inaa,inbb,utd0,utd1
                endif
                                ; Calculate darks
                if dark eq 'T' then begin
                    message,'Verboten :-( '
                    zvisdark=siaa[0,*]+sibb[0,*]*bxtemp[ctr]
                    nvisdark=siaa[1,*]+sibb[1,*]*bxtemp[ctr]
                    znirdark=inaa[0,*]+inbb[0,*]*bxtemp[ctr]
                    nnirdark=inaa[1,*]+inbb[1,*]*bxtemp[ctr]
                endif else begin
                    if utd1 lt 99 then begin ; not last dark
                        factor=(tmhrs[ctr]-utd0)/(utd1-utd0)
                        ;;factor=0.   ; please remove later
                        if factor lt 0. then factor=0. & if factor gt 1. then factor=1.
                        zvisdark=siaa[0,*]*(1.-factor)+sibb[0,*]*factor ; factor determines how much of the previous
                        znirdark=inaa[0,*]*(1.-factor)+inbb[0,*]*factor ; and next dark is used for the current spectrum
                        nvisdark=siaa[1,*]*(1.-factor)+sibb[1,*]*factor ; factor determines how much of the previous
                        nnirdark=inaa[1,*]*(1.-factor)+inbb[1,*]*factor ; and next dark is used for the current spectrum
                    endif else begin
                        zvisdark=sibb[0,*]
                        znirdark=inbb[0,*]
                        nvisdark=sibb[1,*]
                        nnirdark=inbb[1,*]
                    endelse
                endelse

                ;; Spectra: subtract darks, divide by integration time
                zsispectrum=(spect.zspecsi-zvisdark)/spect.intime1 ; Si spectrum
                zirspectrum=(spect.zspecir-znirdark)/spect.intime2 ; IR spectrum
                nsispectrum=(spect.nspecsi-nvisdark)/spect.intime3 ; Si spectrum
                nirspectrum=(spect.nspecir-nnirdark)/spect.intime4 ; IR spectrum

                rawzsispectrum=(spect.zspecsi-zvisdark)
                rawzirspectrum=(spect.zspecir-znirdark)
                rawnsispectrum=(spect.nspecsi-nvisdark)
                rawnirspectrum=(spect.nspecir-nnirdark)
            endif else begin
                zsispectrum=(spect.zspecsi)
                zirspectrum=(spect.zspecir)
                nsispectrum=(spect.nspecsi)
                nirspectrum=(spect.nspecir)

                rawzsispectrum=(spect.zspecsi)
                rawzirspectrum=(spect.zspecir)
                rawnsispectrum=(spect.nspecsi)
                rawnirspectrum=(spect.nspecir)
            endelse


            max_dn = 2^15-1
            if(n_elements(where(spect.zspecsi eq max_dn)) gt 1) then sat[ctr]=sat[ctr]+n_elements(where(spect.zspecsi eq max_dn))-1
            if not sat[ctr] then if(n_elements(where(spect.zspecir eq max_dn)) gt 1) then sat[ctr]=sat[ctr]+n_elements(where(spect.zspecir eq max_dn))-1
            if not sat[ctr] then if(n_elements(where(spect.nspecsi eq max_dn)) gt 1) then sat[ctr]=sat[ctr]+n_elements(where(spect.nspecsi eq max_dn))-1
            
            sat4[*,ctr] = [(n_elements(where([spect.zspecsi,max_dn] eq max_dn)) gt 1),$
                           (n_elements(where([spect.zspecir,max_dn] eq max_dn)) gt 1),$
                           (n_elements(where([spect.nspecsi,max_dn] eq max_dn)) gt 1),$
                           (n_elements(where([spect.nspecir,max_dn] eq max_dn)) gt 1)]

            ; Reversal of wavelengths for IR spectra ONLY
            ;SSFR-7 InGaAs spectrometer wavelengths are NOT reversed in the Zeiss files...

;            zirreverse=fltarr(innp) & nirreverse=fltarr(innp)
;            rawzirreverse=fltarr(innp) & rawnirreverse=fltarr(innp)

;            tmp_innp = 128 ;IR for Knorr only has 128 elements (to 1700 nm)
;            for k=0,tmp_innp-1 do begin
;                zirreverse[k] = zirspectrum[tmp_innp-1-k]
;                nirreverse[k] = nirspectrum[tmp_innp-1-k]

;                rawzirreverse[k] = rawzirspectrum[tmp_innp-1-k]
;                rawnirreverse[k] = rawnirspectrum[tmp_innp-1-k]
;            endfor


            ; Divide by the spectrometer response functions
            zsispectrum = zsispectrum/zsi(1,*)
;            zirspectrum = zirreverse /zir(1,*)
            zirspectrum = zirspectrum /zir(1,*)
            nsispectrum = nsispectrum/nsi(1,*)
;            nirspectrum = nirreverse /nir(1,*)
            nirspectrum = nirspectrum /nir(1,*)

            ; Join Si and IR spectra
            zjoined[0:zindsi2-zindsi1         ] = zsispectrum[zindsi1:zindsi2]
            zjoined[zindsi2-zindsi1+1:znlambda-1] = zirspectrum[zindin1:zindin2]

            njoined[0:nindsi2-nindsi1         ] = nsispectrum[nindsi1:nindsi2]
            njoined[nindsi2-nindsi1+1:nnlambda-1] = nirspectrum[nindin1:nindin2]

            if zsmooth gt 1 then zjoined=smooth(zjoined,zsmooth)
            if nsmooth gt 1 then njoined=smooth(njoined,nsmooth)
            zspectra[*,ctr]=zjoined
            nspectra[*,ctr]=njoined

            rawzsispectra[*,ctr] = rawzsispectrum
            rawzirspectra[*,ctr] = rawzirspectrum
            rawnsispectra[*,ctr] = rawnsispectrum
            rawnirspectra[*,ctr] = rawnirspectrum

            if(doplot ge 1 and tmhrs[ctr]) then begin
                ;;wset,4
                ;;title = platform + ' Joined ' + atime + ' UTC:' + strcompress(tmhrs(ctr),/remove_all)
                ;;ymax = max(rawzsispectrum)*1.04
                ;;plot,zenlambda,zspectra[*,ctr],xtitle = 'Wavelength (nm)', $
                ;;  ytitle = 'Irradiance', xrange = [300,1630],$
                ;;  xstyle=1, /ystyle, charsize = 1.5, charthick = 1.5, $
                ;;  title = title,color=3,thick=1.5

                wset,0
                title = platform + ' Raw Zenith ' + atime + ' UTC:' + strcompress(tmhrs(ctr),/remove_all)
;                ymax = max([max(rawzsispectrum),max(rawzirreverse)])*1.04
                ymax = max([max(rawzsispectrum),max(rawzirspectrum)])*1.04
                ymin = 0
                if(spect.shsw eq 1) then begin
                    ymin = -225
                    ymax = 225
                endif
                plot, wlvisz, rawzsispectrum, xtitle = 'Wavelength (nm)', $
                  ytitle = 'DN', xrange = [300,zir1],$
                  xstyle=1, /ystyle, charsize = 1.5, charthick = 1.5, $
                  title = title,color=3,thick=1.5,yrange=[ymin,ymax]
;                oplot,wlnirz,rawzirreverse
                oplot,wlnirz,rawzirspectrum
                if(doplot ge 2) then begin
                    if((ctr MOD doplot) eq 0) then begin
                        filestr = dir + platform + '_Raw_Zenith_UTC_' + strcompress(tmhrs(ctr),/remove_all)+'.png'
                        rgb=tvrd(true=1)
                        write_png,filestr,rgb
                    endif
                endif
                wset,1
                title = platform + ' Raw Nadir ' + atime + ' UTC:' + strcompress(tmhrs(ctr),/remove_all)
;                ymax = max([max(rawnsispectrum),max(rawnirreverse)])*1.04
                ymax = max([max(rawnsispectrum),max(rawnirspectrum)])*1.04
                ymin = 0
                if(spect.shsw eq 1) then begin
                    ymin = -225
                    ymax = 225
                endif
                plot, wlvisn, rawnsispectrum, xtitle = 'Wavelength (nm)', $
                  ytitle = 'DN', xrange = [300,nir1],$
                  xstyle=1, /ystyle, charsize = 1.5, charthick = 1.5, $
                  title = title, color=3,thick=1.5,yrange=[ymin,ymax]
;                oplot,wlnirn,rawnirreverse
                oplot,wlnirn,rawnirspectrum
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
                  ytitle = 'Irradiance (Wm!E-2!Nnm!E-1!N)', xrange = [300,nir1],$
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
                ymax = max(nsispectrum(25:200))*1.04 ;skip lambda < 390 nm
                if(spect.shsw eq 1) then begin
                    ymin = -.1
                    ymax = .1
                endif
                plot, wlvisn, nsispectrum, xtitle = 'Wavelength (nm)', $
                  ytitle = 'Radiance (Wm!E-2!Nnm!E-1!Nsr!E-1!N)', xrange = [380,nir1],$
                  xstyle=1, /ystyle, charsize = 1.5, charthick = 1.5, $
                  title = title, color=3,thick=1.5,yrange=[ymin,ymax]
                oplot, wlnirn, nirspectrum
;                stop
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

tmhrs   = tmhrs  [0:ctr-1]
zspectra = zspectra[*,0:ctr-1]
nspectra = nspectra[*,0:ctr-1]
nsitemp  = nsitemp [0:ctr-1]
zsitemp  = zsitemp [0:ctr-1]
nirtemp  = nirtemp [0:ctr-1]
zirtemp  = zirtemp [0:ctr-1]
bxtemp  = bxtemp [0:ctr-1]
sat  = sat[0:ctr-1]

rawzsispectra = rawzsispectra[*,0:ctr-1]
rawzirspectra = rawzirspectra[*,0:ctr-1]
rawnsispectra = rawnsispectra[*,0:ctr-1]
rawnirspectra = rawnirspectra[*,0:ctr-1]

; read in and process the CG-4 data
tmhrs_all = tmhrs
zenlambda_all = zenlambda
zspectra_all = zspectra
nadlambda_all = nadlambda
nspectra_all = nspectra

tmhrs_24plus=tmhrs_all
ntimes = n_elements(tmhrs_all)
utc0=0.
xtra=0
for ii=0L,ntimes-1 do begin
    if tmhrs_all[ii] lt utc0 then xtra=ii
    utc0=tmhrs_all[ii]
endfor
tmhrs_24plus[xtra:ntimes-1]=tmhrs_24plus[xtra:ntimes-1]+24.
  

numspectra = [1000,20000]

; read in and process the CG-4 data
cg4serial = cfg(cfg_file,'cg4serial')
cg4pattern =cfg(cfg_file,'cg4pattern') ; read path for CG4 files from CFG
; read in and process the NAV data
navpattern =cfg(cfg_file,'navpattern')
;bode
cg4dir = cg4pattern + l + inputdate + l +'*.CG4'
;eode
cg4calis0  =cfg(cfg_file,'cg4cali') ; read CG4 calibration factors from CFG
cg4calis1  =strsplit(cg4calis0,' ,',escape='#',/EXTRACT)
ncg4cali=n_elements(cg4calis1)
if ncg4cali ne 3 then message,'Did not get the right calibration coefficients for CG4.'
cg4cali=float(cg4calis1)
;read_cg4_knorr,cg4pattern,cg4cali,cg4utc,cg4zenw,cg4brtm
;read_cg4_knorr,cg4dir,cg4cali,cg4utc,cg4zenw,cg4brtm
read_cg4_knorr,cg4dir,cg4serial,cg4cali,cg4utc,cg4z,cg4t; cg4utc=utc from CG-4, cg4z=net irradiance, cg4t=CG-4 temperature
cg4dn=5.6704E-8*(cg4t+273.15)^4 + cg4z ; convert CG-4 net flux to downward irradiance with Stefan-Boltzmann law
;cg4all=interpol(cg4zenw,cg4utc,tmhrs_all) ; interpolate CG4 W/m2 to SSFR times

cg4utc_24plus=cg4utc
ntimes = n_elements(cg4utc)
utc0=0.
xtra=0
for ii=0L,ntimes-1 do begin
    if cg4utc[ii] lt utc0 then xtra=ii
    utc0=cg4utc[ii]
endfor
cg4utc_24plus[xtra:ntimes-1]=cg4utc_24plus[xtra:ntimes-1]+24.


cg4all=interpol(cg4dn,cg4utc_24plus,tmhrs_24plus) ; interpolate CG4 W/m2 to SSFR times
cg4=cg4all

lutdir = cfg(cfg_file,'lutdir')
;lutdir = './ship/lutdir/'

; read in nav data
;navlocation = navpattern + l + inputdate + l +'*.nav'
navlocation = navpattern + l 
if(read_nav) then begin
    read_nav_knorr,navlocation,inputdate,utcnav,latnav,lonnav
    inds = where(finite(latnav) eq 1)
    lat_all=interpol(latnav[inds],utcnav[inds],tmhrs_24plus) ; interpolate lat/lon to SSFR-7 times
    lon_all=interpol(lonnav[inds],utcnav[inds],tmhrs_24plus)
    lat = lat_all
    lon = lon_all
endif else begin
    numobs = n_elements(tmhrs_24plus)
    lat = fltarr(numobs)
    lon = fltarr(numobs)
    deflat = float(cfg(cfg_file,'default_lat'))
    deflon = float(cfg(cfg_file,'default_lon'))
    lat[*] = deflat
    lon[*] = deflon
    utcnav = tmhrs_24plus
endelse

if(do_retrieval) then begin
    print,'Starting the cloud parameter retrievals'
    retrieve_cloud_parms,lutdir,inputdate,nspectra,nadlambda,tmhrs_24plus,lat,lon,tau,reff,tauunc,reffunc
endif

; prepare satus string array to be saved with IDL out file
flight_date   =strmid(inputdate,0,4)+' '+strmid(inputdate,4,2)+' '+strmid(inputdate,6,2)
systime       =systime(/julian)
caldat,systime,mm,dd,yyyy
pro_date =strcompress(string(yyyy,format='(I4.4)'),/remove_all)+' '+strcompress(string(mm,format='(I2.2)'),/remove_all)+' '+strcompress(string(dd,format='(I2.2)'),/remove_all)
platform=cfg(cfg_file,'platform')
satstat=where(sat gt 0,satcount)
status = ['Flight date         : '+flight_date,$
          'Processing date     : '+pro_date,$
          'Platform            : '+platform,$
          'Response functions  :',$
          'Zenith Si           : '+zrespsi1,$
          'Zenith InGaAs       : '+zrespir1,$
          'Nadir Si            : '+nrespsi1,$
          'Nadir InGaAs        : '+nrespir1,$
          'Dark Mode           : '+dark,$
          'Saturation          : '+string(satcount)+' spectra /'+string(ctr),$
          'Integration times   : ',$
          'Zenith Si           : '+string(spect.intime1),$
          'Zenith InGaAs       : '+string(spect.intime2),$
          'Nadir Si            : '+string(spect.intime3),$
          'Nadir InGaAs        : '+string(spect.intime4),$
          'Joinder Zenith      : '+string(zjoin),$
          'Joinder Nadir       : '+string(njoin),$
          'Number data files   : '+string(numfiles),$
          'Number of darks     : '+string(darkcount),$
          'Data file location  : '+data,$
          'Read nav            : '+string(fix(read_nav)),$
          'Do cloud retrieval  : '+string(fix(do_retrieval))]

if(do_retrieval) then begin
    if(read_nav eq 0) then begin
        status = [status,$
                  'Used default lat    : '+string(deflat),$
                  'Used default lon    : '+string(deflon)]
    endif else begin
        status = [status,$
                  'Used nav data']
    endelse        
endif

    
nstat=n_elements(status)
for ii=0,nstat-1 do print,status[ii]

; write variables to an IDL save fileq
; tmhrs   - UTC time in decimal hours
; lambda   - an array containing all the wavelengths observed
; spectra - spectrum time series at all wavelengths observed

; Save temperature data
outfile = strcompress(outdir+l+strcompress(inputdate,/remove_all)+'_temp.out',/REMOVE_ALL)
save, tmhrs, nsitemp, zsitemp, nirtemp, zirtemp, bxtemp, bxcltemp, filename = outfile
spawn, 'chmod a+w '+outfile
spawn,'gzip -9f '+outfile

; Save calibrated spectra
outfile = strcompress(outdir +l+strcompress(inputdate,/remove_all)+'_calibspcs.out',/REMOVE_ALL)
save, tmhrs, zenlambda, zspectra, nadlambda, nspectra,cg4,lat,lon,status, filename = outfile
spawn, 'chmod a+w '+outfile
spawn,'gzip -9f '+outfile

; Save cloud retrieval data
if(do_retrieval) then begin
    outfile = strcompress(outdir +l+strcompress(inputdate,/remove_all)+'_RT.out',/REMOVE_ALL)
    save, tmhrs,tau,reff,tauunc,reffunc,lat,lon,status,filename = outfile
    spawn, 'chmod a+w '+outfile
    spawn,'gzip -9f '+outfile
    ;stuff for debugging
;    cdfdata = {tmhrs:tmhrs,tau:tau,reff:reff,tauunc:tauunc,reffunc:reffunc,status:status}
;    cdffile = outfile+'.cdf'
;    write_netcdf,cdfdata,cdffile
endif

;; Pull out the indices of the archived wavelengths specified in cfg file
zenwl_indices = fltarr(n_elements(qlwl))
for ii=0,n_elements(qlwl)-1 do begin
    tmp=min(abs(zenlambda - qlwl[ii]),ind)
    zenwl_indices[ii] = ind
endfor
nadwl_indices = fltarr(n_elements(qlwl))
for ii=0,n_elements(qlwl)-1 do begin
    tmp=min(abs(nadlambda - qlwl[ii]),ind)
    nadwl_indices[ii] = ind
endfor
ql_zenlambda = zenlambda[zenwl_indices]
ql_nadlambda = nadlambda[nadwl_indices]

; Save 1HZ time series at selected wls
outfile = strcompress(outdir +l+strcompress(inputdate,/remove_all)+'_TS.out',/REMOVE_ALL)
zpartspec=zspectra[zenwl_indices,*]
npartspec=nspectra[nadwl_indices,*]
save,sat,tmhrs,ql_zenlambda,zpartspec,ql_nadlambda,npartspec,cg4,lat,lon,status,filename=outfile
spawn, 'chmod a+w '+outfile
spawn,'gzip -9f '+outfile


; Save full unjoined instantaneous/mean/max/min spectra every 10 minutes
numsec=fix(cfg(cfg_file,'numsec'))
ntimes = ctr/numsec + (ctr MOD numsec gt 0)
time_indices = indgen(ntimes, /double)*numsec
ntime_indices = n_elements(time_indices)

zsimean_rawspectra = fltarr(sinp,ntime_indices)
zinmean_rawspectra = fltarr(innp,ntime_indices)
nsimean_rawspectra = fltarr(sinp,ntime_indices)
ninmean_rawspectra = fltarr(innp,ntime_indices)
zsi0_rawspectra = fltarr(sinp,ntime_indices)
zin0_rawspectra = fltarr(innp,ntime_indices)
nsi0_rawspectra = fltarr(sinp,ntime_indices)
nin0_rawspectra = fltarr(innp,ntime_indices)
zsimax_rawspectra = fltarr(sinp,ntime_indices)
zinmax_rawspectra = fltarr(innp,ntime_indices)
nsimax_rawspectra = fltarr(sinp,ntime_indices)
ninmax_rawspectra = fltarr(innp,ntime_indices)
zsimin_rawspectra = fltarr(sinp,ntime_indices)
zinmin_rawspectra = fltarr(innp,ntime_indices)
nsimin_rawspectra = fltarr(sinp,ntime_indices)
ninmin_rawspectra = fltarr(innp,ntime_indices)
zsiinst_rawspectra = fltarr(sinp,ntime_indices)
zininst_rawspectra = fltarr(innp,ntime_indices)
nsiinst_rawspectra = fltarr(sinp,ntime_indices)
nininst_rawspectra = fltarr(innp,ntime_indices)

for ind1=0d,ntime_indices-2 do begin
    ;;for ii=0d,nqlwl-1 do begin
    zsimean_rawspectra[*,ind1]=total(rawzsispectra[*,time_indices[ind1]:time_indices[ind1+1]],2)/numsec
    zinmean_rawspectra[*,ind1]=total(rawzirspectra[*,time_indices[ind1]:time_indices[ind1+1]],2)/numsec
    nsimean_rawspectra[*,ind1]=total(rawnsispectra[*,time_indices[ind1]:time_indices[ind1+1]],2)/numsec
    ninmean_rawspectra[*,ind1]=total(rawnirspectra[*,time_indices[ind1]:time_indices[ind1+1]],2)/numsec
    
    zsi0_rawspectra[*,ind1]=rawzsispectra[*,time_indices[ind1]]
    zin0_rawspectra[*,ind1]=rawzirspectra[*,time_indices[ind1]]
    nsi0_rawspectra[*,ind1]=rawnsispectra[*,time_indices[ind1]]
    nin0_rawspectra[*,ind1]=rawnirspectra[*,time_indices[ind1]]
    
    zsimax_rawspectra[*,ind1]=max(rawzsispectra[*,time_indices[ind1]:time_indices[ind1+1]],DIMENSION=2)
    zinmax_rawspectra[*,ind1]=max(rawzirspectra[*,time_indices[ind1]:time_indices[ind1+1]],DIMENSION=2)
    nsimax_rawspectra[*,ind1]=max(rawnsispectra[*,time_indices[ind1]:time_indices[ind1+1]],DIMENSION=2)
    ninmax_rawspectra[*,ind1]=max(rawnirspectra[*,time_indices[ind1]:time_indices[ind1+1]],DIMENSION=2)
    zsimin_rawspectra[*,ind1]=min(rawzsispectra[*,time_indices[ind1]:time_indices[ind1+1]],DIMENSION=2)
    zinmin_rawspectra[*,ind1]=min(rawzirspectra[*,time_indices[ind1]:time_indices[ind1+1]],DIMENSION=2)
    nsimin_rawspectra[*,ind1]=min(rawnsispectra[*,time_indices[ind1]:time_indices[ind1+1]],DIMENSION=2)
    ninmin_rawspectra[*,ind1]=min(rawnirspectra[*,time_indices[ind1]:time_indices[ind1+1]],DIMENSION=2)
    zsiinst_rawspectra[*,ind1] = rawzsispectra[*,time_indices[ind1]]
    zininst_rawspectra[*,ind1] = rawzirspectra[*,time_indices[ind1]]
    nsiinst_rawspectra[*,ind1] = rawnsispectra[*,time_indices[ind1]]
    nininst_rawspectra[*,ind1] = rawnirspectra[*,time_indices[ind1]]
endfor

;; Handle the remainder
lastind = ind1
numremainderpoints = n_elements(tmhrs[time_indices[ntime_indices-1]:*])
zsimean_rawspectra[*,lastind]=total(rawzsispectra[*,time_indices[lastind]:*],2)/numremainderpoints
zinmean_rawspectra[*,lastind]=total(rawzirspectra[*,time_indices[lastind]:*],2)/numremainderpoints
nsimean_rawspectra[*,lastind]=total(rawnsispectra[*,time_indices[lastind]:*],2)/numremainderpoints
ninmean_rawspectra[*,lastind]=total(rawnirspectra[*,time_indices[lastind]:*],2)/numremainderpoints
zsimax_rawspectra[*,lastind]=max(rawzsispectra[*,time_indices[lastind]:*],DIMENSION=2)
zinmax_rawspectra[*,lastind]=max(rawzirspectra[*,time_indices[lastind]:*],DIMENSION=2)
nsimax_rawspectra[*,lastind]=max(rawnsispectra[*,time_indices[lastind]:*],DIMENSION=2)
ninmax_rawspectra[*,lastind]=max(rawnirspectra[*,time_indices[lastind]:*],DIMENSION=2)
zsimin_rawspectra[*,lastind]=min(rawzsispectra[*,time_indices[lastind]:*],DIMENSION=2)
zinmin_rawspectra[*,lastind]=min(rawzirspectra[*,time_indices[lastind]:*],DIMENSION=2)
nsimin_rawspectra[*,lastind]=min(rawnsispectra[*,time_indices[lastind]:*],DIMENSION=2)
ninmin_rawspectra[*,lastind]=min(rawnirspectra[*,time_indices[lastind]:*],DIMENSION=2)
zsiinst_rawspectra[*,lastind] = rawzsispectra[*,time_indices[lastind]]
zininst_rawspectra[*,lastind] = rawzirspectra[*,time_indices[lastind]]
nsiinst_rawspectra[*,lastind] = rawnsispectra[*,time_indices[lastind]]
nininst_rawspectra[*,lastind] = rawnirspectra[*,time_indices[lastind]]

outfile = strcompress(outdir +l+strcompress(inputdate,/remove_all)+'_SP.out',/REMOVE_ALL)
time = tmhrs[time_indices]
save,time,wlvisz,wlnirz,wlvisn,wlnirn,$
     zsi0_rawspectra,zin0_rawspectra,nsi0_rawspectra,nin0_rawspectra,$
     zsimean_rawspectra,zinmean_rawspectra,nsimean_rawspectra,ninmean_rawspectra,$
     zsimax_rawspectra,zinmax_rawspectra,nsimax_rawspectra,ninmax_rawspectra,$
     zsimin_rawspectra,zinmin_rawspectra,nsimin_rawspectra,ninmin_rawspectra,$
     zsi,zir,nsi,nir,$
     numremainderpoints,status,filename=outfile
spawn, 'chmod a+w '+outfile
spawn,'gzip -9f '+outfile

end
