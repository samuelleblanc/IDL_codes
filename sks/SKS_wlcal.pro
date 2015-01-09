;+
; NAME:
;   SKS_wlcal
;
; PURPOSE:
;   Program to compare wavelength calibration of SKS to measured Hg spectra
;
; CATEGORY:
;   SKS calibration wavelength
;
; CALLING SEQUENCE:
;   SKS_wlcal, dir
;   - dir is the directory with the *.SKS file to check out
;   
; OUTPUT:
;   Wavelength calibration
;
; KEYWORDS:
;   none
;
; DEPENDENCIES:
;   - read_SKS.pro
; 
; NEEDED FILES:
;   - *.SKS files
;   
; EXAMPLE:
;   
;
; MODIFICATION HISTORY:
; Written:  Samuel LeBlanc, Ocotber 14th, 2012, Ottawa, Canada
; Modified: 
;           
;---------------------------------------------------------------------------

@read_SKS.pro

pro SKS_wlcal,dir

if n_elements(dir) lt 1 then dir='C:\Users\Samuel\Research\SKS\data\wvl\'

l='\'

file=file_search(dir+'*.SKS',count=n)
if n lt 1 then message, 'No file found in:'+dir

;read the SKS file
read_SKS, file[0], spect, eos,shutter,inttime,temps,utc,count,nd,darks

;build the dark
dark=fltarr(256,4)
sp=fltarr(256,4)
op=where(shutter[*,0] eq 1,no)
if no lt 1 then message, 'There is no spectra with the shutters opened in this file'
for i=0, 255 do begin
  for j=0, 3 do begin
    dark[i,j]=mean(darks[*,i,j])
    ; build the average spectra
    sp[i,j]=mean(spect[op,i,j])-dark[i,j]
  endfor
endfor

;now normalize sp
for i=0, 3 do sp[*,i]=sp[*,i]/max(sp[*,i])

; make silicon wavelength array just to get first try at it
wvl=findgen(256)*3.1+300.
ind=findgen(256)

;list of the mercury emission lines
hg=[313.2,334.1,365.0,404.6,435.8,546.1,576.9]
hg=[365.0,404.6,435.8,546.1,577.3];,697.,765.,811.,912.]


; now plot the resultant spectra with the mercury wavelengths

set_plot, 'win'
device, decomposed=0
loadct, 39, /silent
window, 0, retain=2
plot,sp[*,0], title='Wavenlength calibration of silicon using Mercury lamp for radiance', xtitle='Wavelength (index)', $
  ytitle='Normalized radiance';,/ylog, yrange=[0.00001,1]
for i=0, n_elements(hg)-1 do begin
  nul=min(abs(wvl-hg[i]),nv)
  oplot, [ind[nv],ind[nv]],[0.00001,1],color=250
endfor

; now set up for makeing gaussians around selected areas

center=hg*0.0
fwhm=hg*0.0
for i=0, n_elements(hg)-1 do begin
  cursor,x1,y1
  wait,0.2
  cursor,x2,y2
  fn=where(ind ge x1 and ind le x2)
  res=gaussfit(ind[fn],sp[fn,0],A,nterms=3)
  xx=indgen(101)*(x2-x1)/100.+x1
  ff=a[0]*exp(-0.5*((xx-a[1])/a[2])^2)
  fwhmn=2*SQRT(2*ALOG(2))*A[2]
  an=a[1]
  oplot,xx,ff,thick=2,color=70
  print,an
  print,fwhmn
  center[i]=an
  fwhm[i]=fwhmn
endfor

window, 1, retain=1
plot, hg, center, xtitle='Wavelength (nm)', ytitle='Wavelenght (index)', title='Index to wavelength correlation for radiance', psym=2

; now make the polynomial fit to the data
fit=poly_fit(center,hg,3);, measure_errors=fwhm)

; now print the fit line and oplot it
print, fit
lambda=fltarr(256)
for i=0, 255 do for j=0,3 do lambda[i]=lambda[i]+fit[j]*float(i)^j
oplot, lambda, ind, color=250


; now repeat the calibration procedure for irradiance

set_plot, 'win'
device, decomposed=0
loadct, 39, /silent
window, 2, retain=2
plot,sp[*,2], title='Wavenlength calibration of silicon using Mercury lamp for irradiance', xtitle='Wavelength (index)', $
  ytitle='Normalized radiance' ;, /ylog,yrange=[0.00001,1]
for i=0, n_elements(hg)-1 do begin 
  nul=min(abs(wvl-hg[i]),nv) 
  oplot, [ind[nv],ind[nv]],[0.00001,1],color=250 
endfor 
       
; now set up for makeing gaussians around selected areas 
       
center=hg*0.0 
fwhm=hg*0.0 
for i=0, n_elements(hg)-1 do begin 
  cursor,x1,y1 
  wait,0.2 
  cursor,x2,y2 
  fn=where(ind ge x1 and ind le x2) 
  res=gaussfit(ind[fn],sp[fn,2],A,nterms=3) 
  xx=indgen(101)*(x2-x1)/100.+x1 
  ff=a[0]*exp(-0.5*((xx-a[1])/a[2])^2) 
  fwhmn=2*SQRT(2*ALOG(2))*A[2] 
  an=a[1] 
  oplot,xx,ff,thick=2,color=70 
  print,an 
  print,fwhmn 
  center[i]=an 
  fwhm[i]=fwhmn 
endfor 
				   
window, 3, retain=1 
plot, hg, center, xtitle='Wavelength (nm)', ytitle='Wavelenght (index)', title='Index to wavelength correlation for irradiance',$
 psym=2
				   
; now make the polynomial fit to the data 
fit=poly_fit(center,hg,3);, measure_errors=fwhm) 
				   
; now print the fit line and oplot it 
print, fit   
lambda=fltarr(256) 
 for i=0, 255 do for j=0,3 do lambda[i]=lambda[i]+fit[j]*float(i)^j 
 oplot, lambda, ind, color=250 



stop
end
