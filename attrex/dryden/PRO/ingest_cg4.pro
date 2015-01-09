;+
; NAME:
;   ingest_cg4
;
; PURPOSE:
;   To take in the .CG4 files and read them
;
; CATEGORY:
;   ATTREX real time data module CG4
;
; CALLING SEQUENCE:
;   ingest_cg4, cg4, config, cg4_utc, cg4_zen, cg4_nad, cg4_zent, cg4_nadt
;    - where cg4 if the file path to the cg4 file
;    - cg4_utc is the time in hours
;    - cg4_zen is the zenith facing cg4 irradiance
;    - cg4_nad is the nadir facing cg4 irradiance
;    - cg4_zent is the zenith detector temperature
;    - cg4_nadt is the nadir detector temperature
;    - config if the file path for the configuration file
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
;   - .CG4 file 
;   
; EXAMPLE:
;   
;
; MODIFICATION HISTORY:
; Written:  Samuel LeBlanc, LASP CU Boulder, September 20th, 2011
; Modified: 
;           
;---------------------------------------------------------------------------

pro ingest_cg4, cg4, config, cg4_utc, cg4_zen, cg4_nad, cg4_zent, cg4_nadt

file =cg4
nf=1

bn=400

; conversion coefficients
cal=float(strsplit(cfg(config,'cg4cal'),' ,',escape='#',/EXTRACT))
tem=float(strsplit(cfg(config,'cg4tem'),' ,',escape='#',/EXTRACT))
date=cfg(config,'date')

secs=lonarr(bn)
zenv=fltarr(bn) ; detector voltage / signal
nadv=fltarr(bn)
zent=fltarr(bn) ; detector temperature
nadt=fltarr(bn)
zenr=fltarr(bn) ; rear case temperature
nadr=fltarr(bn)
zens=fltarr(bn) ; data system temperature
nads=fltarr(bn)
zenI=fltarr(bn) ; Irradiance
nadI=fltarr(bn)
times=fltarr(bn,6) ;UTC time in year, month, day, hour, minutes, seconds
o=5.6704E-8 ;sigma
newline = string(10B)

i=0L
for f=0,nf-1 do begin
openr,uu,file[f],/get_lun
print,newline+'Open:',file[f]
while not eof(uu) do begin
  ; data structure of CG4
            block = {time1: lonarr(2), cnt:lonarr(1),$
                 status1:bytarr(1), pad1:bytarr(3), serial1:lonarr(1),sys_serial1:intarr(1),version1:bytarr(1),gain1:bytarr(1),$
                 voltage1:lonarr(1),temperature1:lonarr(1),reartemp1:lonarr(1),systemp1:lonarr(1),caltemp1:lonarr(1),$
                 status2:bytarr(1), pad2:bytarr(3), serial2:lonarr(1),sys_serial2:intarr(1),version2:bytarr(1),gain2:bytarr(1),$
                 voltage2:lonarr(1),temperature2:lonarr(1),reartemp2:lonarr(1),systemp2:lonarr(1),caltemp2:lonarr(1)}

  readu,uu,block
  
  ;zenith is the serial1
  ;nadir is the serial2
  
  ;check the correct date
  dtemp=bin_date(systime(0,block.time1[0],/utc))
  if fix(date) gt fix(string(dtemp[0],format='(I04)')+string(dtemp[1],format='(I02)')+string(dtemp[2],format='(I02)')) then begin
    print, 'Not correct date for CG4: '+string(dtemp[0],format='(I04)')+string(dtemp[1],format='(I02)')+string(dtemp[2],format='(I02)')+' config date:'+date
    secs=!values.f_nan
    times[0,*]=[dtemp]
    zenv=!values.f_nan
    nadv=!values.f_nan
    zent=!values.f_nan
    nadt=!values.f_nan
    zenI=!values.f_nan
    nadI=!values.f_nan
    i=1
    goto, out ;goto the next file
  endif

; Calibration constant for each cg4
for j=0, n_elements(cal)/4-1 do begin
  if fix(block.serial1) eq fix(cal[j*4]) then begin
    cal1=[cal[j*4+1],cal[j*4+2],cal[j*4+3]] ; setting up cals in k0,k1,k2 format
    tem1=[tem[j*7+1],tem[j*7+2]]
  endif
  if fix(block.serial2) eq fix(cal[j*4]) then begin
    cal2=[cal[j*4+1],cal[j*4+2],cal[j*4+3]] ; setting up cals in k0,k1,k2 format
    tem2=[tem[j*7+1],tem[j*7+2]]
  endif 
endfor

if n_elements(cal1) ne 3 then message, 'Serial '+string(block.serial1)+' was not found in Configuration file'
if n_elements(cal2) ne 3 then message, 'Serial '+string(block.serial2)+' was not found in Configuration file'


  secs[i]=block.cnt mod 86400 ; count from beginning of the day
  if i gt 0 then if secs[i] lt secs[i-1] and secs[i-1]-secs[i] gt 86000  then secs[i]=secs[i]+86400

  zenv[i]=(block.voltage1 - '800000'XL)*1.25/float('800000'XL)/float(block.gain1)
  nadv[i]=(block.voltage2 - '800000'XL)*1.25/float('800000'XL)/float(block.gain2)

  zent[i]=((block.temperature1 - '800000'XL)*1.25/float('800000'XL))/float(block.gain1)
  nadt[i]=((block.temperature2 - '800000'XL)*1.25/float('800000'XL))/float(block.gain2)

  zent[i]=tem1[0]+tem1[1]*1e3*zent[i]
  nadt[i]=tem2[0]+tem2[1]*1e3*nadt[i]
  
  zenI[i]=cal1[0] + zenv[i]*1.0E6*cal1[1] +cal1[2]*o*(zent[i]+273.15)^4.0      ;for downwelling Irradiance
  nadI[i]=cal2[0] + nadv[i]*1.0E6*cal2[1] +cal2[2]*o*(nadt[i]+273.15)^4.0      ;for Upwelling Irradiance
  
  times[i,*]=bin_date(systime(0,block.time1[0],/utc))

  i+=1L
endwhile
out:
free_lun,uu
endfor

secs=secs[0:i-1]
times=times[0:i-1,*]
zenv=zenv[0:i-1]
nadv=nadv[0:i-1]
zent=zent[0:i-1]
nadt=nadt[0:i-1]
zenI=zenI[0:i-1]
nadI=nadI[0:i-1]

dim=i

utc=times[*,3]+times[*,4]/60.+times[*,5]/3600.
kkk=where(julday(times[*,1],times[*,2],times[*,0],0,0,0) ne julday(times[0,1],times[0,2],times[0,0],0,0,0),count)
if count gt 0 then begin
  utc[kkk]=utc[kkk]+24.
  kkkp=where(julday(times[kkk,1],times[kkk,2],times[kkk,0],0,0,0) ne julday(times[kkk[0],1],times[kkk[0],2],times[kkk[0],0],0,0,0),countp)
  if countp gt 0 then utc[kkk[kkkp]]=utc[kkk[kkkp]]+24.
endif
count=0 ;wrong_n=where(nadI gt 1000.0 or zenI gt 1000.0 or zenI lt 0.0 or nadI lt 0.0,count)
if count gt 0 then begin
  zenv[wrong_n]=-9999.
  nadv[wrong_n]=-9999.
  zent[wrong_n]=-9999.
  nadt[wrong_n]=-9999.
  zenI[wrong_n]=-9999.
  nadI[wrong_n]=-9999.
endif
cg4_utc=utc & cg4_zen=zenI & cg4_nad=nadI & cg4_zent=zent & cg4_nadt=nadt

end
