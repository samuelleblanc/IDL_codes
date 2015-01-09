;+
; NAME:
;   rtm_aero_unique
;
; PURPOSE:
;   to present results from the unique runs in arctas_rtm_aero
;
; CATEGORY:
;   Aerosol retrieval, ARCTAS, unique
;
; CALLING SEQUENCE:
;   rtm_aero_unique
; 
;
; OUTPUT:
;   plots
;   values in a save file
;
; KEYWORDS:
;   - none
;
; DEPENDENCIES:
;   
;   
; NEEDED FILES:
;   - rtm_unique file
;
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
; Written:  Samuel LeBlanc, LASP CU Boulder, March 24th, 2011, Ottawa, Ontario, Canada
; Modified: 
;---------------------------------------------------------------------------


pro rtm_aero_unique

dir='/home/leblanc/libradtran/output/aero/'
dirout='/home/leblanc/arctas/'

;put in manually the original optical depth measured at index 56
tau_original=[0.705700,0.675900,0.568800,0.503500,0.481900,0.377400,0.308200,0.238100,0.190500,0.137200,0.0923000,0.0535000,0.0379000]
wvl_original=[353.500,380.000,451.200,499.400,520.400,605.800,675.100,779.100,864.500,1019.10,1241.30,1558.50,2139.10]

dat=read_ascii(dir+'rtm_unique_20080709_v1.txt',data_start=1)
wvl=dat.field01[2,*]
num=n_elements(wvl)
lat=fltarr(13,num/13)
lon=lat & ssa=lat & asy=lat & asy2=lat & albedo=lat & tau_rtm=lat & tau_input=lat & wvl=lat

for ii=0,num/13-1  do begin
  for i=0,12 do begin
   lat[i,ii]=dat.field01[0,(ii*13)+i]
   lon[i,ii]=dat.field01[1,(ii*13)+i]
   wvl[i,ii]=dat.field01[2,(ii*13)+i]
   ssa[i,ii]=dat.field01[3,(ii*13)+i]
   asy[i,ii]=dat.field01[4,(ii*13)+i]
   asy2[i,ii]=dat.field01[5,(ii*13)+i]
   albedo[i,ii]=dat.field01[6,(ii*13)+i]
   tau_rtm[i,ii]=dat.field01[8,(ii*13)+i]
   tau_input[i,ii]=dat.field01[9,(ii*13)+i] * tau_original[i]
  endfor
endfor

save, lat, lon, wvl, ssa, asy, asy2, albedo, tau_rtm, tau_input, tau_original, filename=dirout+'rtm_unique.out'

end