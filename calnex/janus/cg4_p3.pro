;+
; NAME:
;   cg4_p3
;
; PURPOSE:
;   program to get cg4 data from the p3
;
; CATEGORY:
;   CALNEX / CG4
;
; CALLING SEQUENCE:
;   cg4_p3,path,datein,utcin,zenI, nadI,zent,nadt
;   
;   where:
;     path  : path to files of the day
;     datein: Date to match data
;     utcin : UTC timing data to match
;     zenI  : zenith Irradiance
;     nadI  : nadir Irradiance
;     zent  : zenith temperatures
;     nadt  : nadir temperatures
;   
; OUTPUT:
;     zenI and nadI 
;     zent and nadt
;
; KEYWORDS:
;   CG4, P3, CALNEX, , Data
;
; DEPENDENCIES:
;   cfg.pro       ;config file cheker
;   
; NEEDED FILES:
;   - config file for day
;   - cg4 data in *.CG4 format
;  
; EXAMPLE:
; 
;
; MODIFICATION HISTORY:
; Written:  Sebastian Schmidtc, LASP CU Boulder, date unknown
; Modified: wednesday April 21st, 2010
;          -by Samuel LeBlanc
;           added comments
;           change the output format of this file to just give up and down irradiances
;           added the new calibration constants for irradiance, reading from the config file
; Modified: Thursday, April 22nd, 2010
;         -by Samuel LeBlanc
;           added outputs of temperature for zenith and nadir
;           change the reading sequence to correctly match the cg4 files
;           The calibration constant need voltage in uV (multiplied by 1.0E6)
;           added correct time retrieval from the cg4 data
; Modified: Friday, April 23rd, 2010
;         - by Samuel LeBlanc
;           added new date input, and date checking for interpol procedure
;           added new filtering for outrageous value sof irradiances, changed to -9999
;---------------------------------------------------------------------------
pro cg4_p3,path,datein,utcin,zenI,nadI,zent,nadt

pattern=path+'*.CG4'
files =file_search(pattern,count=nf)
config=path+'*.cfg'
config=file_search(config,count=nc)
config=config[0]
if nc ne 1 then message,'Configuration file not found.'
bn=40000L

yearin=float(strmid(datein,0,4))
monthin=float(strmid(datein,4,2))
dayin=float(strmid(datein,6,2))

; conversion coefficients
cal=float(strsplit(cfg(config,'cg4cal'),' ,',escape='#',/EXTRACT))
tem=float(strsplit(cfg(config,'cg4tem'),' ,',escape='#',/EXTRACT))

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

i=0L
for f=0,nf-1 do begin
openr,uu,files[f],/get_lun
print,'Open:',files[f]
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


date_n=where((times[*,0] ge yearin) and (times[*,1] ge monthin) and (times[*,2] ge dayin),count)
if count lt 1 then message, 'Could not find correct date in CG4 data'
utc=times[date_n,3]+times[date_n,4]/60.+times[date_n,5]/3600.
kkk=where(julday(times[date_n,1],times[date_n,2],times[date_n,0],0,0,0) ne julday(times[date_n[0],1],times[date_n[0],2],times[date_n[0],0],0,0,0),count)
if count gt 0 then utc[kkk]=utc[kkk]+24.
wrong_n=where(nadI[date_n] gt 1000.0 or zenI[date_n] gt 1000.0 or zenI[date_n] lt 0.0 or nadI[date_n] lt 0.0,count)
if n_elements(utcin) gt 0 then begin
  zenv=interpol(zenv[date_n],utc,utcin)
  nadv=interpol(nadv[date_n],utc,utcin)
  zent=interpol(zent[date_n],utc,utcin)
  nadt=interpol(nadt[date_n],utc,utcin)
  zenI=interpol(zenI[date_n],utc,utcin)
  nadI=interpol(nadI[date_n],utc,utcin)
endif
zenv[wrong_n]=-9999.
nadv[wrong_n]=-9999.
zent[wrong_n]=-9999.
nadt[wrong_n]=-9999.
zenI[wrong_n]=-9999.
nadI[wrong_n]=-9999.

;
;plot=1 & iscg4=1
;if plot and iscg4 then begin 
;  set_plot, 'X'
;  window, 9, title='CG4 Temperatures'
;  plot, utcin, zent+273.15, title='CG4 Temperatures',ytitle='Temperatures (Kelvin)',xtitle='UTC [h]', psym = 1
;  oplot, utcin, nadt+273.15, psym = 3
;  legend, ['Zenith','Nadir'], psym=[1,3]
;
;  window, 10, title='CG4 Irradiances'
;  plot, utcin, zenI, title='CG4 Irradiances',ytitle='Irradiance [W m!E-2!N]',xtitle='UTC [h]',linestyle =1
;  oplot, utcin, nadI, linestyle=3
;  legend, ['Downwelling', 'Upwelling'],linestyle=[1,3]
;  
;    window, 11, title='CG4 Voltages'
;  plot, utcin, zenv, title='CG4 Voltages',ytitle='Votlage',xtitle='UTC [h]',linestyle =1
;  oplot, utcin, nadv, linestyle=3
;  legend, ['Downwelling', 'Upwelling'],linestyle=[1,3]
;endif
;stop
;print, 'CG4 timing not correct, do not trust these yet'
end
