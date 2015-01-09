;+
; NAME:
;   SKS_std
;
; PURPOSE:
;  Determine the standard deviation of a set of meaasured values
;
; CATEGORY:
;   SKS data analysis. Standard deviation
;
; CALLING SEQUENCE:
;   SKS_std, date
;    - where date is in format yyyymmdd (optional)
;   
; OUTPUT:
;   - plots
;
; KEYWORDS:
;   none
;
; DEPENDENCIES:
;   - legend.pro
;
; NEEDED FILES:
;   - yyyymmdd_SP.out files
;   
; EXAMPLE:
;   
;
; MODIFICATION HISTORY:
; Written:  Samuel LeBlanc, October 16th, 2012, Ottawa, Canada
; Modified: 
;           
;---------------------------------------------------------------------------


pro SKS_std, date

if n_elements(date) lt 1 then date='20120507'

dir='C:\Users\Samuel\Research\SKS\data'
l='\'
restore, dir+l+date+l+date+'_SP.out'

;make arrays of standard deviation for both the darks and spectra
nad=n_elements(nadlambda)
zen=n_elements(zenlambda)

nadstd=fltarr(nad)
zenstd=fltarr(zen)

stddark =fltarr(256,4)
stdspect=fltarr(256,4)

shutop=where(shut eq 1 and shift(shut,1) eq 1 and shift(shut,-1) eq 1,cs)

for i=0, nad-1 do nadstd[i]=stddev(nspectra[*,i])
for i=0, zen-1 do zenstd[i]=stddev(zspectra[*,i])
for i=0, 255 do begin
  for j=0, 3 do begin
    stddark[i,j]=stddev(dark[*,i,j])/mean(dark[*,i,j])
    stdspect[i,j]=stddev(spect[shutop,i,j])/mean(spect[shutop,i,j])
  endfor
endfor

;now plot the results vs. wavelengths
set_plot, 'ps'
 loadct, 39, /silent
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., xsize=20, ysize=45
  device, filename=dir+l+date+l+'std.ps'
  !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,1,3]

  plot, zenlambda, zenstd, title='Silicon Stability in Radiance',xtitle='Wavelength (nm)',$
   ytitle='Standard deviation of Radiance (W/m!U2!N nm sr)',yrange=[0.001,10.0],/ylog,xrange=[300.,900.]
  oplot, nadlambda, nadstd, color=250
  legend,['Spectrometer 1','Spectrometer 2'],textcolors=[0,250],box=0, /right
  plot, wvl[*,0],stdspect[*,0]*100., title='Percent Silicon deviation', xtitle='Wavelength (nm)',$
   ytitle='Standard deviation (%)',/ylog,yrange=[0.01,100.],xrange=[300.,900.]
  oplot, wvl[*,2],stdspect[*,2]*100.,color=250
  legend,['Spectrometer 1','Spectrometer 2'],textcolors=[0,250],box=0, /right
    plot, wvl[*,0],stddark[*,0]*100., title='Percent Darks Silicon deviation', xtitle='Wavelength (nm)',$
	       ytitle='Standard deviation (%)',xrange=[300.,900.]
         oplot, wvl[*,2],stddark[*,2]*100.,color=250
	   legend,['Spectrometer 1','Spectrometer 2'],textcolors=[0,250],box=0, /right
  
  device, /close
  spawn, 'convert '+dir+l+date+l+'std.ps '+dir+l+date+l+'std.png'
  stop

end
