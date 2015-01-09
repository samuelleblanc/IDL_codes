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
dirout='/home/leblanc/rtm_aero/data/'

;put in manually the original optical depth measured at index 56
;tau_original=[0.705700,0.675900,0.568800,0.503500,0.481900,0.377400,0.308200,0.238100,0.190500,0.137200,0.0923000,0.0535000,0.0379000]
wvl_original=[353.500,380.000,451.200,499.400,520.400,605.800,675.100,779.100,864.500,1019.10,1241.30,1558.50,2139.10]

dat=read_ascii(dirout+'rtm_unique_20080709_v3.txt',data_start=1)
wvl=dat.field01[2,*]
num=n_elements(wvl)
lat=fltarr(13,num/13)
lon=lat & ssa=lat & asy=lat & asy2=lat & albedo=lat & tau_rtm=lat & tau_input=lat & wvl=lat & tau_original=lat

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
   tau_original[i,ii]=dat.field01[10,(ii*13)+i]
   tau_input[i,ii]=dat.field01[9,(ii*13)+i] * tau_original[i,ii]
   
  endfor
endfor
nul=where(ssa lt 0.01, ct) 
if ct gt 0 then begin
 tau_rtm[nul]=!values.f_nan
 tau_input[nul]=!values.f_nan
endif

save, lat, lon, wvl, ssa, asy, asy2, albedo, tau_rtm, tau_input, tau_original, filename=dirout+'rtm_unique_v3.out'

set_plot, 'ps'
loadct, 39,/silent

   device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8.
   device, filename=dirout+'rtm_unique.ps'
   device, xsize=20, ysize=20
   !p.font=1 & !p.thick=5
   !p.charsize=1.8 & !x.style=1
   !y.style=1 & !z.style=1
   !y.thick=1.8 & !x.thick=1.8
   !x.margin=[6,4]
   !p.multi=0
   cl=findgen(13)*255/13
   plot, tau_rtm[0,*],tau_input[0,*], title='Retrieved optical depth from varrying optical depth input',ytitle='Input optical depth', xtitle='Retrieved optical depth', psym=4, color=cl[0], yrange=[0,2.],xrange=[0,1.5]
   oplot, [tau_original[0],tau_original[0]],[0,2],color=cl[0], thick=1
   for i=1, 12 do begin
    oplot, [tau_original[i],tau_original[i]],[0,2],color=cl[i], thick=1
	oplot, tau_rtm[i,*],tau_input[i,*], psym=4, color=cl[i]
   endfor
   
   legend, string(wvl_original,format='(F6.1)'),textcolors=cl, box=0
   device, /close
spawn, 'convert '+dirout+'rtm_unique.ps '+dirout+'rtm_unique_v3.png'
spawn, 'rm -f '+dirout+'rtm_unique.ps'
   
stop
end