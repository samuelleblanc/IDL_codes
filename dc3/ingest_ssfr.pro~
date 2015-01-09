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
;   ingest_ssfr,ssfr,date,ssfr_utc,raw,drk,ch,int,n_drk,err,n_sat,n_ctr,ssfr3=ssfr3,avg=avg,drktime=drktime
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
;   - ssfr3, sets the ingest_ssfr to read *.OSA files (from SSFR3 only)
;   - avg, returns the raw averages of the spectra
;   - drktime, sets the program to search for dark at specified times (every 15 minutes, for 1 minute, on the hour,works with ssfr3 keyword)
;
; DEPENDENCIES:
;   none  
; 
; NEEDED FILES:
;   - .OSA2 file (or *.OSA with the SSFR3 keyword)
;   
; EXAMPLE:
;   
;
; MODIFICATION HISTORY:
; Written:  Samuel LeBlanc and Sebastian Schmidt, LASP CU Boulder, September 19th, 2011
; Modified: April 17th, 2012, LASP CU Boulder
;           - by Samuel LeBlanc
;           added the avg keyword that returns raw averages
;           added the ssfr3 keyword that makes this program work with SSFR3 data
; Modified: May 17th, 2012, Thursday, LASP CU Boulder
;           - by Samuel LeBlanc
;           added the drktime keyword for analyzing the ssfr3 data from the rooftop.
;           
;---------------------------------------------------------------------------


pro ingest_ssfr,ssfr,date,ssfr_utc,raw,drk,ch,int,n_drk,err,n_sat,n_ctr,ssfr3=ssfr3,avg=avg,drktime=drktime
newline = string(10B)
print,newline+'Processing '+ssfr

if not keyword_set(ssfr3) then ssfr=0
if not keyword_set(avg) then avg=0
if not keyword_set(drktime) then drktime=0

ctr  = 0L ; counter of total spectra
n_ctr= 0L ; counter of spectra only
n_drk= 0L ; counter of darks only

np=256
months = ["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"]
stop_checking=0

bn=200
tmhrs   =fltarr(bn)
if ssfr3 then ch = fltarr(bn,3) else ch =fltarr(bn,8)
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
  if ssfr3 then begin
     spect = {btime: lonarr(2), bcdtimstp:  bytarr(12), intime1: long(0), $
            intime2: long(0), intime3: long(0), intime4: long(0), $
            accum: long(0), ztemp: ulong(0), ntemp: ulong(0), itemp: ulong(0), $
            zspecsi: intarr(np), zspecir: intarr(np), nspecsi: intarr(np), nspecir: intarr(np)}
  
  endif else begin
  spect = {btime: lonarr(2), bcdtimstp:  bytarr(12),intime1: long(0),$
           intime2: long(0), intime3: long(0), intime4: long(0),accum: long(0), shsw:long(0),$
           zsit: ulong(0),nsit: ulong(0),zirt: ulong(0),nirt: ulong(0), zirx: ulong(0), nirx: ulong(0),xt: ulong(0),it: ulong(0),$
           zspecsi: intarr(np), zspecir: intarr(np), nspecsi: intarr(np), nspecir: intarr(np)}
  endelse
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
  if ssfr3 then begin 
    ssfr3_sh=1
    if drktime then begin
      mmmp=double(mm)+double(ss+10.0)/60.
      mmml=double(mm)+double(ss-10.0)/60.
      if mmml ge 0.0 and mmmp le 1.0 then ssfr3_sh=0
      if mmml ge 15.0 and mmmp le 16.0 then ssfr3_sh=0
      if mmml ge 30.0 and mmmp le 31.0 then ssfr3_sh=0
      if mmml ge 45.0 and mmmp le 46.0 then ssfr3_sh=0
    endif 
  endif else ssfr3_sh=spect.shsw

  if ssfr3_sh then begin ; shutter open
  
    if ssfr3 then begin 
      ch0=10.*(float(spect.ztemp)/4096.)
      ch1=10.*(float(spect.ntemp)/4096.)
      ch2=spect.itemp*(10./4096.)
      ch[n_ctr,*]=[ch0,ch1,ch2]
  	
      raw[n_ctr,*,0]=spect.zspecsi
      raw[n_ctr,*,1]=[reverse(spect.zspecir[0:127]),spect.zspecir[128:*]]
      raw[n_ctr,*,2]=spect.nspecsi
      raw[n_ctr,*,3]=[reverse(spect.nspecir[0:127]),spect.nspecir[128:*]]
    
    endif else begin
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
    endelse
    int[0]=spect.intime1
    int[1]=spect.intime2
    int[2]=spect.intime3
    int[3]=spect.intime4
  
    if drktime then begin
      op=1
      if mmmp ge 0.0 and mmml le 1.0 then op=0
      if mmmp ge 15.0 and mmml le 16.0 then op=0
      if mmmp ge 30.0 and mmml le 31.0 then op=0
      if mmmp ge 45.0 and mmml le 46.0 then op=0
      if op then begin
        n_ctr=n_ctr+1 & shut_op[n_ctr]=ctr
      endif
    endif else begin  	
      n_ctr=n_ctr+1
      shut_op[n_ctr]=ctr
    endelse
  endif else begin ; shutter closed, acquire darks
    if ssfr3 then begin
      ;ch0=10.*(float(spect.ztemp)/4096.)
      ;ch1=10.*(float(spect.ntemp)/4096.)
      ;ch2=spect.itemp*(10./4096.)
      ;ch[n_ctr,*]=[ch0,ch1,ch2]

      drk_tmp[n_ctr,*,0]=spect.zspecsi
      drk_tmp[n_ctr,*,1]=[reverse(spect.zspecir[0:127]),spect.zspecir[128:*]]
      drk_tmp[n_ctr,*,2]=spect.nspecsi
      drk_tmp[n_ctr,*,3]=[reverse(spect.nspecir[0:127]),spect.nspecir[128:*]]
    endif else begin
      drk_tmp[n_drk,*,0]=spect.zspecsi
      drk_tmp[n_drk,*,1]=reverse(spect.zspecir)
      drk_tmp[n_drk,*,2]=spect.nspecsi
      drk_tmp[n_drk,*,3]=reverse(spect.nspecir)	
    endelse
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

  if avg then begin ; part of program that makes the raw averages
    avgs=drk
    for i=0, np-1 do begin
      avgs[i,0]=mean(raw[*,i,0],/nan)
      avgs[i,1]=mean(raw[*,i,1],/nan)
      avgs[i,2]=mean(raw[*,i,2],/nan)
      avgs[i,3]=mean(raw[*,i,3],/nan)
    endfor
    raw=avgs
  endif
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
