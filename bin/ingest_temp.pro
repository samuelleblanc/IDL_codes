;+
; NAME:
;   ingest_temp
;
; PURPOSE:
;   to change the ssfr thermistor voltages to celsius, with the cfg file
;
; CATEGORY:
;   Real time data module temperatures
;
; CALLING SEQUENCE:
;   ingest_temp, cfg_file, ch, temp, ssfr3=ssfr3
;    - where cfg_file is the cfg file path
;    - ch is the channel configuration of SSFR for the temperatures
;    - temp is the output temperatures in Celsius
;   
; OUTPUT:
;   see above
;
; KEYWORDS:
;   - ssfr3
;    runs the program for ssfr3 temperatures
;
; DEPENDENCIES:
;   - cfg.pro
; 
; NEEDED FILES:
;   - .cfg file 
;   
; EXAMPLE:
;   
;
; MODIFICATION HISTORY:
; Written:  Samuel LeBlanc, LASP CU Boulder, September 19th, 2011
; Modified: Monday, April 23rd, 2012
;           by Samuel LeBlanc
;           added the SSFR3 keyword, so that this program can be used with SSFR3 temperature data
;           
;---------------------------------------------------------------------------

@cfg.pro
pro ingest_temp, cfg_file, ch, temp, ssfr3=ssfr3

temp_file   =cfg(cfg_file,'thermistor') ; thermistor coefficients
cnt=n_elements(ch[*,0]) ; count number of elements in the ch array

; Read temperature coefficients
npt  = file_lines(temp_file)
tdat = fltarr(2, npt)   & revt = fltarr(2, npt)
openr,ut,temp_file, /get_lun & readf,ut,tdat & free_lun,ut
for i = 0, npt - 1 do begin
  revt[0, i] = tdat[0, npt -i - 1] & revt[1, i] = tdat[1, npt -i - 1]
endfor
rt    = findgen(20000)/10000. + 0. ; interpolate thermistor data
revts = spline(revt(1, *), revt(0, *), rt) ; at fine resolution

if keyword_set(ssfr3) then begin 
  temp = fltarr(cnt,3)
  ztmp = fltarr(cnt) ;zenith temperature
  ntmp = fltarr(cnt) ;nadir temperature
  itmp = fltarr(cnt) ;internatl temperature
  
  for i=0, cnt-1 do begin
    ;convert voltage to temp
    zitv = long(ch[i,0] * 10000.)     ;convert to integer for use as index
    nitv = long(ch[i,1] * 10000.)     ;convert to integer for use as index
    
    if (zitv le 19999) then begin  
      ztmp[i] = revts(zitv)       ;load zenith temp into array
    endif
    if (nitv le 19999) then begin
      ntmp[i] = revts(nitv)       ;load nadir temp into array
    endif
     
    rtem = abs((2000. * ch[i,2])/(1. - (0.2 * ch[i,2])))
    dem = 1.0295e-3 + (2.391e-4 * alog(rtem)) + (1.568e-7 * (alog(rtem))^3)
    intv = float(1./dem)
    itmp[i] = float(intv - 273.)         ;holds internal temp 
  endfor
endif else begin
  zirtemp=fltarr(cnt) & nirtemp  =fltarr(cnt)
  zsitemp=fltarr(cnt) & nsitemp  =fltarr(cnt)
  bxtemp =fltarr(cnt) & bxcltemp =fltarr(cnt)
  zirxt  =fltarr(cnt) & nirxt    =fltarr(cnt)
  temp   =fltarr(cnt,8)
  
  ; Get analogue channels and convert to voltage / temperature
              ;ch0    ;Zenith SI spect. temp.
              ;ch1    ;Nadir SI spect temp.
              ;ch2    ;Zenith InGaAs temp.
              ;ch3    ;Nadir InGaAs temp.
              ;ch4    ;Box temperature
              ;ch5    ;Box Cooler Temperature
              ;ch6    ;temperature ZIR housing
              ;ch7    ;temperature NIR housing
  
  ;;;;;;;;;;;;;;;;; Convert Voltage to Temp ;;;;;;;;;;;;;;;;;;;;
  for i=0, cnt-1 do begin
    zsit = long(ch[i,0] * 10000.)     ;convert to integer for use as index
    nsit = long(ch[i,1] * 10000.)     ;convert to integer for use as index
  
    if (zsit le 19999) then zsitemp[i] = revts[zsit] ;load zenith temp into array
    if (nsit le 19999) then nsitemp[i] = revts[nsit] ;load nadir temp into array
    
    zirtemp[i] = ((1./3200.)*alog(20.*ch[i,2]) + (1./298.))^(-1)-273. ;zen ir tmp
    nirtemp[i] = ((1./3200.)*alog(20.*ch[i,3]) + (1./298.))^(-1)-273. ;nad ir tmp
    ;;;;;;;;;;;; Third thermistor internal to the computer ;;;;;;;;;;;;;
  
    rtem = abs((2000. * ch[i,4])/(1. - (0.2 * ch[i,4])))
    dem = 1.0295e-3+(2.391e-4 * alog(rtem)) + (1.568e-7 * (alog(rtem))^3)
    bxtemp[i] = float(1./dem - 273.)
    rtem = abs((2000. * ch[i,5])/(1. - (0.2 * ch[i,5])))
    dem = 1.0295e-3+(2.391e-4 * alog(rtem)) + (1.568e-7 * (alog(rtem))^3)
    bxcltemp[i] = float(1./dem - 273.)
  ;  new temperatures
    rtem = abs((2000. * ch[i,6])/(1. - (0.2 * ch[i,6])))
    dem = 1.0295e-3+(2.391e-4 * alog(rtem)) + (1.568e-7 * (alog(rtem))^3)
    zirxt[i] = float(1./dem - 273.)
    rtem = abs((2000. * ch[i,7])/(1. - (0.2 * ch[i,7])))
    dem = 1.0295e-3+(2.391e-4 * alog(rtem)) + (1.568e-7 * (alog(rtem))^3)
    nirxt[i] = float(1./dem - 273.)
    
    temp[i,0]=zsitemp[i]
    temp[i,1]=nsitemp[i]
    temp[i,2]=zirtemp[i]
    temp[i,3]=nirtemp[i]
    temp[i,4]=bxtemp[i]
    temp[i,5]=bxcltemp[i]
    temp[i,6]=zirxt[i]
    temp[i,7]=nirxt[i]
  endfor
endelse
end
