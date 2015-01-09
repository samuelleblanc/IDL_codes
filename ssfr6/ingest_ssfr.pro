;+
; NAME:
;   ingest_ssfr
;
; PURPOSE:
;   Read in the data from a single OSA2 file ingest it and return the raw spectra and darks, as well as temperature and integration times
;
; CATEGORY:
;   ATTREX real time data module   
;
; CALLING SEQUENCE:
;   ingest_ssfr,ssfr,date,ssfr_utc,raw,drk,ch,int,n_drk,err,n_sat,n_ctr
;    - where date is in format yyyymmdd
;    - ssfr is the osa2 file name
;    - ssfr_utc is the uts time on the ssfr data
;    - raw is the raw spectra (zsi,zir,nsi,nir)
;    - drk is the drk spectra
;    - ch is the various temperature measurement channels
;    - int is the integration times 
;    - n_drk is the number of darks present in this file
;    - err is the standard deviation of the dark spectra
;    - n_sat is the number of saturated points
;    - n_ctr is the number of spectra
;   
; OUTPUT:
;   see above
;
; KEYWORDS:
;   none
;
; DEPENDENCIES:
;   none  
; 
; NEEDED FILES:
;   - .OSA2 file 
;   
; EXAMPLE:
;   
;
; MODIFICATION HISTORY:
; Written:  Samuel LeBlanc and Sebastian Schmidt, LASP CU Boulder, September 19th, 2011
; Modified: 
;           
;---------------------------------------------------------------------------


pro ingest_ssfr,ssfr,date,ssfr_utc,raw,drk,ch,int,n_drk,err,n_sat,n_ctr
newline = string(10B)
print,newline+'Processing '+ssfr

ctr  = 0L ; counter of total spectra
n_ctr= 0L ; counter of spectra only
n_drk= 0L ; counter of darks only

np=256
months = ["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"]
stop_checking=0

bn=200
tmhrs   =fltarr(bn)
ch      =fltarr(bn,8)
raw     =fltarr(bn,np,4)
drk     =fltarr(np,4)
err     =fltarr(np,4)
drk_tmp =fltarr(bn,np,4)
int     =fltarr(4)
shut_op =fltarr(bn)
; 0: zsi  1:zir  2:nsi  3:nir

max_dn=(2^15)-1
openr,lun,ssfr, /get_lun
while not eof(lun) do begin ; Read individual spectra
  spect = {btime: lonarr(2), bcdtimstp:  bytarr(12),intime1: long(0),$
           intime2: long(0), intime3: long(0), intime4: long(0),accum: long(0), shsw:long(0),$
           zsit: ulong(0),nsit: ulong(0),zirt: ulong(0),nirt: ulong(0), zirx: ulong(0), nirx: ulong(0),xt: ulong(0),it: ulong(0),$
           zspecsi: intarr(np), zspecir: intarr(np), nspecsi: intarr(np), nspecir: intarr(np)}

  readu, lun, spect

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
  if(mon lt 10) then mon = '0' + string(mon[0]) else mon = string(mon[0])
  year = fix(strmid(atime(0), result + 7, 4)) ;get year
  mydate = strtrim(string(year,mon,day),1)
  if fix(strmid(date,6,2))+1 eq fix(strmid(mydate,6,2)) then plus=24. else begin
    if fix(strmid(date,6,2))+2 eq fix(strmid(mydate,6,2)) then plus=48. else plus=0.
   ; if((mydate ne date) and (not stop_checking)) then begin
   ;   print, "Skipping ",ssfr,' ',date,' ',mydate
   ;   continue
   ; endif else begin
   ;   stop_checking = 1
   ; endelse
   ;print, 'SSFR .OSA2 date is set to:'+mydate+' while set date is:'+date
  endelse

  hh = strmid(atime(0), result - 2, 2) & mm = strmid(atime(0), result + 1, 2) & ss = strmid(atime(0), result + 4, 2)
  utc = double(hh) + double(mm)/60. + double(ss)/3600. ; put hr in decimal form
  tmhrs[ctr]=utc+plus


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
    ch[n_ctr,*]=[ch0,ch1,ch2,ch3,ch4,ch5,ch6,ch7]
	
	;;;;;;;;;;;;;;;;;;; get raw spectra - don't substract darks;;;;;;;;;;;;
    ; 0: zsi  1:zir  2:nsi  3:nir
  	raw[n_ctr,*,0]=spect.zspecsi
  	raw[n_ctr,*,1]=reverse(spect.zspecir)
  	raw[n_ctr,*,2]=spect.nspecsi
  	raw[n_ctr,*,3]=reverse(spect.nspecir)
  
  	int[0]=spect.intime1
  	int[1]=spect.intime2
  	int[2]=spect.intime3
  	int[3]=spect.intime4
  	
  	n_ctr=n_ctr+1
  	shut_op[n_ctr]=ctr
  endif else begin ; shutter closed, acquire darks
    drk_tmp[n_drk,*,0]=spect.zspecsi
	  drk_tmp[n_drk,*,1]=reverse(spect.zspecir)
	  drk_tmp[n_drk,*,2]=spect.nspecsi
	  drk_tmp[n_drk,*,3]=reverse(spect.nspecir)	

    n_drk=n_drk+1
  endelse
  ctr=ctr+1L
endwhile

; now cut the variables to the correct length
if n_ctr gt 0 then begin ;if there was a time where there were some measurements with the shutter open
shut_op  = shut_op[1:n_ctr]
nnn      = where(shift(shut_op,-1)-shut_op le 1 and shift(shut_op,1)-shut_op ge -1,ns) ; takes out one spectrum before and after the darks
if ns gt 0 then shut_op = shut_op[nnn]               ; take out the indices that are right before and right after a dark.
ch       = ch[shut_op,*]
raw      = raw[0:n_ctr,*,*]
if ns gt 0 then raw = raw[nnn,*,*]
;drk_tmp  = drk_tmp[0:n_drk,*,*]
ssfr_utc = tmhrs[shut_op]

nul=where(raw eq max_dn,n_sat)
endif 

drk_tmp  = drk_tmp[0:n_drk,*,*]
; now process the darks to get one mean dark and a standard deviation of the darks
if n_drk gt 0 then begin
  for i=0, np-1 do begin
    drk[i,0]=mean(drk_tmp[*,i,0],/nan)
	  drk[i,1]=mean(drk_tmp[*,i,1],/nan)
  	drk[i,2]=mean(drk_tmp[*,i,2],/nan)
  	drk[i,3]=mean(drk_tmp[*,i,3],/nan)
	
  	err[i,0]=stddev(drk_tmp[*,i,0],/nan)
  	err[i,1]=stddev(drk_tmp[*,i,1],/nan)
	  err[i,2]=stddev(drk_tmp[*,i,2],/nan)
	  err[i,3]=stddev(drk_tmp[*,i,3],/nan)
  endfor
endif
free_lun,lun
if n_ctr gt 0 then begin
  if ns gt 0 then n_ctr=ns
  print,'SSFR .OSA2 date is set to:'+mydate+' while set date is:'+date
  print,'  UTC SSFR=',tmhrs[n_ctr-1],'  #total Spectra=',ctr,'  #Spectra=',n_ctr,'  #Darks  =',n_drk
endif
;stop
end
