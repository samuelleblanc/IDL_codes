;+
; NAME:
;   read_cpl
;
; PURPOSE:
;   to read the quick optical file from CPL
;
; CATEGORY:
;   ATTREX, clouds properties
;
; CALLING SEQUENCE:
;   read_cpl, file, utc, h_sat, h_grd, nlay, type_code, lay_topht, lay_botht, tau, tau_e, sp_use, sp_use_e
;   - file:      file name to read in 
;   - utc:       time in utc of the CPL data
;   - h_sat:     height of the saturation (meters ASL)
;   - h_grd:     height of the ground (meters ASL)
;   - nlay:      numbers of layer
;   - type_code: layer type (0 for cirrus, 1 for PBL,-1 for neither present)
;   - lay_topht: height of the top of the layer (meters ASL)
;   - lay_botht: height of the bottom of the layer (meters ASL)
;   - tau:       layer optical depth estimate per wl, 
;   - tau_e:     layer optical depth from error profile per wl
;   - sp_use:    extinction-to-backscatter (S ratio) used for layer per wl
;   - sp_use_e:  S ratio for error profile for layer per wl
;
; OUTPUT:
;   values of optical depth per layer
;
; KEYWORDS:
;   - none
;
; DEPENDENCIES:
;   - none
;   
; NEEDED FILES:
;   - CPL quick optical text file
;
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
; Written:  Samuel LeBlanc, LASP CU Boulder, June 4th , 2012
; Modified: 
;
;---------------------------------------------------------------------------

pro read_cpl, file, utc, h_sat, h_grd, nlay, type_code, lay_topht, lay_botht, tau, tau_e

f=file_search(file, count=n)
if n lt 1 then message, 'CPL file not found'
num=file_lines(f[0])/3

;set up the arrays for all the possible data
sortie_arr   = intarr(num)
year_arr     = intarr(num)
djday_arr    = dblarr(num)
hr_arr       = intarr(num)
minu_arr     = intarr(num)
sec_arr      = fltarr(num)
lat_arr      = fltarr(num)
lon_arr      = fltarr(num)
pitch_arr    = fltarr(num)
roll_arr     = fltarr(num)
heading_arr  = fltarr(num)
plnht_arr    = fltarr(num)
zcode_arr    = intarr(num,3)
vsmo_arr     = intarr(num)
hsmo_arr     = intarr(num)
saturate_arr = fltarr(num,4)
grd_ht_arr   = fltarr(num)
nlay_arr     = intarr(num)
type_code_arr= intarr(num)
lay_topht_arr= fltarr(num)
lay_botht_arr= fltarr(num)
tau_cal1_arr = fltarr(num,3)
tau_cal1e_arr= fltarr(num,3)
sp_use_arr   = fltarr(num,3)
sp_use_e_arr = fltarr(num,3)
s_source_arr = intarr(num,3)
proctype_arr = intarr(num,3)

openr, lun, f[0], /get_lun
;set up the reading variables
line1='' & line2='' & line3=''
zcode=intarr(3) & saturate=fltarr(4) & tau_cal1=fltarr(3) & tau_cal1e=fltarr(3)
sp_use=fltarr(3)& sp_use_e=fltarr(3) & s_source=intarr(3) & proctype=intarr(3)
i=0
while not eof(lun) do begin
;read all 3 lines, then parse out the desired data
  readf, lun, line1
  readf, lun, line2
  readf, lun, line3

  reads, line1, sortie,year,djday,hr,minu,sec,lat,lon,pitch,roll,heading,plnht,zcode,format= '(I6.5,I5,F10.5,3I3,F7.2,F8.2,3F7.2,F7.0,1X,3I2)'
  reads, line2, vsmo,hsmo,saturate,gnd_ht,nlay,type_code,lay_topht,lay_botht,tau_cal1,format= '(4X,2I3,5F7.0,I3,I3,2F7.0,3F7.3)'
  reads, line3, tau_cal1e,sp_use,sp_use_e,s_source,proctype, format= '(4X,3F7.3,6F7.2,1X,6I2)'
  
  ;now put the variables in the correct array
  sortie_arr[i]   =sortie    & year_arr[i]      =year      & djday_arr[i]     =djday     & hr_arr[i]        =hr       & minu_arr[i]       =minu    & sec_arr[i]  =sec
  lat_arr[i]      =lat       & lon_arr[i]       =lon       & pitch_arr[i]     =pitch     & roll_arr[i]      =roll     & heading_arr[i]    =heading & plnht_arr[i]=plnht
  zcode_arr[i,*]  =zcode     & vsmo_arr[i]      =vsmo      & hsmo_arr[i]      =hsmo      & saturate_arr[i,*]=saturate & grd_ht_arr[i]     =gnd_ht  & nlay_arr[i] =nlay
  type_code_arr[i]=type_code & lay_topht_arr[i] =lay_topht & lay_botht_arr[i] =lay_botht & tau_cal1_arr[i,*]=tau_cal1 & tau_cal1e_arr[i,*]=tau_cal1e
  sp_use_arr[i,*]   =sp_use  & sp_use_e_arr[i,*]=sp_use_e  & s_source_arr[i,*]=s_source  & proctype_arr[i,*]=proctype
i=i+1
end
tau   = tau_cal1_arr & tau_e = tau_cal1e_arr & lay_topht=lay_topht_arr & lay_botht=lay_botht_arr & type_code=type_code_arr & nlay=nlay_arr
h_grd = grd_ht_arr   & h_sat = saturate_arr

utc=(djday_arr-float(floor(djday_arr[0])))*24.
;stop
end
