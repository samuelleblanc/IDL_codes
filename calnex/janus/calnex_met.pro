;+
; NAME:
;   calnex_met
;
; PURPOSE:
;   read in the data from the calnex aircraft files
;
; CATEGORY:
;   calnex, met
;
; CALLING SEQUENCE:
;   calnex_met,date=date)
;
; OUTPUT:
;   structure that contains all the data from the aircraft files (p3)
;
; KEYWORDS:
;   - date:  date of file
;   - outfile file to write output to
;
; DEPENDENCIES:
;
; NEEDED FILES:
;   - output file from aircraft
;
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
; Written:  Samuel LeBlanc, LASP CU Boulder, August 13th
; Modified: 
;---------------------------------------------------------------------------
@magnus.pro
@\\lasp-smb\leblanc\CALNEX\pro\magnus.pro
pro calnex_met, date=date, outfile=outfile

if not keyword_set(date) then date='20100519'

if !VERSION.OS eq 'linux' then linux=1 else linux=0

if linux then begin
ll='/'
dir='/home/leblanc/CALNEX/p3/'+date+ll
endif else begin
ll='\'
dir='\\lasp-smb\leblanc\CALNEX\p3\'+date+ll
endelse

if not keyword_set(outfile) then outfile=dir+'atm_'+date+'.dat'

; get met file
f=file_search(dir+'*'+date+'*.ict.met', count=ct)
if ct lt 1 then message, 'met file not found!'
length=[0.,0.]
openu, lun, f[0], /get_lun
readf, lun, length
free_lun, lun
close, /all
met=read_ascii(f[0],data_start= length[0])
temp=met.field01[1,*] +273.15 ;temperature (Kelvin)
RH=met.field01[6,*]    ;Relative humidity (%)
pressure=met.field01[10,*]  ;pressure (milibars)
h2o=magnus(RH, temp)
;get nav file
f=file_search(dir+'*'+date+'*.ict.pos', count=ct)
if ct lt 1 then message, 'pos file not found!'
length=[0.,0.]
openu, lun, f[0], /get_lun
readf, lun, length
free_lun, lun
close, /all
pos=read_ascii(f[0],data_start= length[0])
alt=pos.field1[4,*]*0.001    ;altitude in km
time=pos.field1[0,*]/3600.   ;time in UTC hours

; now get only the desired time span
case date of
  '20100504':begin
    utc_range=[19.8,21.0]
  end
  '20100519':begin
    utc_range=[20.05,20.37]
  end
  '20100516':begin
    utc_range=[22.8,23.55]
  end
  else: message, 'date not recongnized'
endcase
fl=where(time ge utc_range[0] and time le utc_range[1])

temp=temp[fl]
pressure=pressure[fl]
h2o=h2o[fl]
alt=alt[fl]

alts=findgen(600)/20.

openw, lut, outfile, /get_lun
printf, lut, '# Atmosphere file written from WP-3 flight track near CalTech'
printf, lut, '# Z (km)  Pressure(mb)    T(K)  H2O(#/cm3)'

;interpol routine for in between the lowest leg and highest lef of the flight section

low_alt=min(alt,bottom)
high_alt=max(alt,top)

for i=n_elements(alts)-1,0,-1 do begin
  if alts[i] ge low_alt and alts[i] le high_alt then begin
    p=interpol(pressure[sort(alt)],alt[sort(alt)],alts[i])
    t=interpol(temp[sort(alt)],alt[sort(alt)],alts[i])
    h=interpol(h2o[sort(alt)],alt[sort(alt)],alts[i])
    printf, lut, alts[i], p, t, h
  endif 
endfor
free_lun, lut
close, /all

end