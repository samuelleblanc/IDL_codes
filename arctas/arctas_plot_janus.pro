;; procedure to plot the results of arctas retrieval 
;; this uses the results from the janus super computer
;; does a few more tests on the results

@errploty.pro
pro arctas_plot_janus
date='20080408'
dir='/home/leblanc/arctas/nasa/'+date+'/'

; get the result of the retrievals
  f=file_search(dir+'rtm_hist_'+date+'_wvl????.txt')
  f=f[sort(f)]
  print, f
    
  wvl0353=read_ascii(f[0], data_start=1)
  wvl0380=read_ascii(f[1], data_start=1)
  wvl0452=read_ascii(f[2], data_start=1)
  wvl0499=read_ascii(f[3], data_start=1)
  wvl0519=read_ascii(f[4], data_start=1)
  wvl0605=read_ascii(f[5], data_start=1)
  wvl0675=read_ascii(f[6], data_start=1)
  wvl0779=read_ascii(f[7], data_start=1)
  wvl0864=read_ascii(f[8], data_start=1)
  wvl1019=read_ascii(f[9], data_start=1)
  wvl1241=read_ascii(f[10], data_start=1)
  wvl1558=read_ascii(f[11], data_start=1)
  wvl2139=read_ascii(f[12], data_start=1)
  
  
  print, 'getting retrieval error'
;get errors in retrievals
error=fltarr(6,13)
openr, 95, '/home/leblanc/arctas/nasa/rtm_error.out'
line=' '
readf, 95, line
readf, 95, error
close,/all
error=error*0.01
  
;;;; loop to verify that the g and g_hat match within epsilon ;;;;;
if 1 then begin
fl=where(abs(wvl0353.field01[3,*]-wvl0353.field01[4,*]) lt 0.02 and finite(wvl0353.field01[3,*]) eq 1 and finite(wvl0353.field01[4,*]) eq 1,complement=cm)
wvl0353.field01[*,cm]=replicate(!values.f_nan,16,n_elements(cm));wvl0353.field01[*,fl]
fl=where(abs(wvl0380.field01[3,*]-wvl0380.field01[4,*]) lt 0.02 and finite(wvl0380.field01[3,*]) eq 1 and finite(wvl0380.field01[4,*]) eq 1,complement=cm)
wvl0380.field01[*,cm]=replicate(!values.f_nan,16,n_elements(cm));wvl0380.field01[*,fl]
fl=where(abs(wvl0452.field01[3,*]-wvl0452.field01[4,*]) lt 0.02 and finite(wvl0452.field01[3,*]) eq 1 and finite(wvl0452.field01[4,*]) eq 1,complement=cm)
wvl0452.field01[*,cm]=replicate(!values.f_nan,16,n_elements(cm));wvl0452.field01[*,fl]
fl=where(abs(wvl0499.field01[3,*]-wvl0499.field01[4,*]) lt 0.02 and finite(wvl0499.field01[3,*]) eq 1 and finite(wvl0499.field01[4,*]) eq 1,complement=cm)
wvl0499.field01[*,cm]=replicate(!values.f_nan,16,n_elements(cm));wvl0499.field01[*,fl]
fl=where(abs(wvl0519.field01[3,*]-wvl0519.field01[4,*]) lt 0.02 and finite(wvl0519.field01[3,*]) eq 1 and finite(wvl0519.field01[4,*]) eq 1,complement=cm)
wvl0519.field01[*,cm]=replicate(!values.f_nan,16,n_elements(cm));wvl0519.field01[*,fl]
fl=where(abs(wvl0605.field01[3,*]-wvl0605.field01[4,*]) lt 0.02 and finite(wvl0605.field01[3,*]) eq 1 and finite(wvl0605.field01[4,*]) eq 1,complement=cm)
wvl0605.field01[*,cm]=replicate(!values.f_nan,16,n_elements(cm));wvl0605.field01[*,fl]
fl=where(abs(wvl0675.field01[3,*]-wvl0675.field01[4,*]) lt 0.02 and finite(wvl0675.field01[3,*]) eq 1 and finite(wvl0675.field01[4,*]) eq 1,complement=cm)
wvl0675.field01[*,cm]=replicate(!values.f_nan,16,n_elements(cm));wvl0675.field01[*,fl]
fl=where(abs(wvl0779.field01[3,*]-wvl0779.field01[4,*]) lt 0.02 and finite(wvl0779.field01[3,*]) eq 1 and finite(wvl0779.field01[4,*]) eq 1,complement=cm)
wvl0779.field01[*,cm]=replicate(!values.f_nan,16,n_elements(cm));wvl0779.field01[*,fl]
fl=where(abs(wvl0864.field01[3,*]-wvl0864.field01[4,*]) lt 0.02 and finite(wvl0864.field01[3,*]) eq 1 and finite(wvl0864.field01[4,*]) eq 1,complement=cm)
wvl0864.field01[*,cm]=replicate(!values.f_nan,16,n_elements(cm));wvl0864.field01[*,fl]
fl=where(abs(wvl1019.field01[3,*]-wvl1019.field01[4,*]) lt 0.02 and finite(wvl1019.field01[3,*]) eq 1 and finite(wvl1019.field01[4,*]) eq 1,complement=cm)
wvl1019.field01[*,cm]=replicate(!values.f_nan,16,n_elements(cm));wvl1019.field01[*,fl]
endif
; disregard any values above 1000 nm
;fl=where(abs(wvl1241.field01[3,*]-wvl1241.field01[4,*]) lt 0.02 and finite(wvl1241.field01[3,*]) eq 1 and finite(wvl1241.field01[4,*]) eq 1)
;fl=[0]
;wvl1241.field01=wvl1241.field01[*,fl]
;fl=where(abs(wvl1558.field01[3,*]-wvl1558.field01[4,*]) lt 0.02 and finite(wvl1558.field01[3,*]) eq 1 and finite(wvl1558.field01[4,*]) eq 1)
;fl=[0]
;wvl1558.field01=wvl1558.field01[*,fl]
;fl=where(abs(wvl2139.field01[3,*]-wvl2139.field01[4,*]) lt 0.02 and finite(wvl2139.field01[3,*]) eq 1 and finite(wvl2139.field01[4,*]) eq 1)
;wvl2139.field01=wvl2139.field01[*,fl]  

; get the average value of the values at each wavelength
ssa=[mean(wvl0353.field01[2,*],/nan),mean(wvl0380.field01[2,*],/nan),mean(wvl0452.field01[2,*],/nan),mean(wvl0499.field01[2,*],/nan),$
 mean(wvl0519.field01[2,*],/nan),mean(wvl0605.field01[2,*],/nan),mean(wvl0675.field01[2,*],/nan),mean(wvl0779.field01[2,*],/nan),$
 mean(wvl0864.field01[2,*],/nan),mean(wvl1019.field01[2,*],/nan)]
asy=[mean(wvl0353.field01[3,*],/nan),mean(wvl0380.field01[3,*],/nan),mean(wvl0452.field01[3,*],/nan),mean(wvl0499.field01[3,*],/nan),$
 mean(wvl0519.field01[3,*],/nan),mean(wvl0605.field01[3,*],/nan),mean(wvl0675.field01[3,*],/nan),mean(wvl0779.field01[3,*],/nan),$
 mean(wvl0864.field01[3,*],/nan),mean(wvl1019.field01[3,*],/nan)]
asy2=[mean(wvl0353.field01[4,*],/nan),mean(wvl0380.field01[4,*],/nan),mean(wvl0452.field01[4,*],/nan),mean(wvl0499.field01[4,*],/nan),$
 mean(wvl0519.field01[4,*],/nan),mean(wvl0605.field01[4,*],/nan),mean(wvl0675.field01[4,*],/nan),mean(wvl0779.field01[4,*],/nan),$
 mean(wvl0864.field01[4,*],/nan),mean(wvl1019.field01[4,*],/nan)]
alb=[mean(wvl0353.field01[5,*],/nan),mean(wvl0380.field01[5,*],/nan),mean(wvl0452.field01[5,*],/nan),mean(wvl0499.field01[5,*],/nan),$
 mean(wvl0519.field01[5,*],/nan),mean(wvl0605.field01[5,*],/nan),mean(wvl0675.field01[5,*],/nan),mean(wvl0779.field01[5,*],/nan),$
 mean(wvl0864.field01[5,*],/nan),mean(wvl1019.field01[5,*],/nan)]
tau=[mean(wvl0353.field01[11,*],/nan),mean(wvl0380.field01[11,*],/nan),mean(wvl0452.field01[11,*],/nan),mean(wvl0499.field01[11,*],/nan),$
 mean(wvl0519.field01[11,*],/nan),mean(wvl0605.field01[11,*],/nan),mean(wvl0675.field01[11,*],/nan),mean(wvl0779.field01[11,*],/nan),$
 mean(wvl0864.field01[11,*],/nan),mean(wvl1019.field01[11,*],/nan)]/[mean(wvl0353.field01[7,*],/nan),mean(wvl0380.field01[7,*],/nan),$
 mean(wvl0452.field01[7,*],/nan),mean(wvl0499.field01[7,*],/nan),mean(wvl0519.field01[7,*],/nan),mean(wvl0605.field01[7,*],/nan),$
 mean(wvl0675.field01[7,*],/nan),mean(wvl0779.field01[7,*],/nan),mean(wvl0864.field01[7,*],/nan),mean(wvl1019.field01[7,*],/nan)]
 
wvl=[353,380,452,499,519,605,675,779,864,1019] 
 
;restore, '/home/leblanc/arctas/nasa/'+date+'/'+date+'_spectra_save.out'
;dtau_aats=dtau_aats*0.0 +0.01
;dtau_aats ; errors in tau_aats

; now plot the averaged data
 set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold'
 device, filename=dir+'rtm_results.ps'
 device,/color,bits_per_pixel=8., xsize=40, ysize=40
  !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,2,2]
 
 plot,wvl,ssa,yrange=[0,1],ytitle='Single scattering albedo',xtitle='Wavelength (nm)'
 plot,wvl,asy,yrange=[0,1],ytitle='Asymmetry parameter', xtitle='Wavelength (nm)'
 plot,wvl,asy2,yrange=[0,1],ytitle='Asymmetry parameter 2',xtitle='Wavelength (nm)'
 plot,wvl,tau,ytitle='Optical depth',xtitle='Wavelength (nm)'
 
  device, /close
  spawn, 'convert "'+dir+'rtm_results.ps" "'+dir+'rtm_results.png" ' ;'
  spawn, 'rm -f "'+dir+'rtm_results.ps"'

  ; now plot all the different spectras possible with the valid values
   set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold'
 device, filename=dir+'rtm_results_all.ps'
 device,/color,bits_per_pixel=8., xsize=40, ysize=40
  !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,2,2]
  
  plot, wvl, ssa,yrange=[0,1],ytitle='Single scattering albedo',xtitle='Wavelength (nm)'
  for i=0, n_elements(wvl0499.field01[0,*])-1 do begin
    spectrum=[wvl0353.field01[2,i],wvl0380.field01[2,i],wvl0452.field01[2,i],wvl0499.field01[2,i],$
     wvl0519.field01[2,i],wvl0605.field01[2,i],wvl0675.field01[2,i],wvl0779.field01[2,i],$
     wvl0864.field01[2,i],wvl1019.field01[2,i]]
	oplot, wvl,spectrum,psym=2
  endfor
  
    plot, wvl, asy,yrange=[0,1],ytitle='Asymmetry parameter',xtitle='Wavelength (nm)'
  for i=0, n_elements(wvl0499.field01[0,*])-1 do begin
    spectrum=[wvl0353.field01[3,i],wvl0380.field01[3,i],wvl0452.field01[3,i],wvl0499.field01[3,i],$
     wvl0519.field01[3,i],wvl0605.field01[3,i],wvl0675.field01[3,i],wvl0779.field01[3,i],$
     wvl0864.field01[3,i],wvl1019.field01[3,i]]
	oplot, wvl,spectrum,psym=2
  endfor
  
      plot, wvl, asy2,yrange=[0,1],ytitle='Asymmetry parameter 2',xtitle='Wavelength (nm)'
  for i=0, n_elements(wvl0499.field01[0,*])-1 do begin
    spectrum=[wvl0353.field01[4,i],wvl0380.field01[4,i],wvl0452.field01[4,i],wvl0499.field01[4,i],$
     wvl0519.field01[4,i],wvl0605.field01[4,i],wvl0675.field01[4,i],wvl0779.field01[4,i],$
     wvl0864.field01[4,i],wvl1019.field01[4,i]]
	oplot, wvl,spectrum,psym=2
  endfor
  
      plot, wvl, tau,yrange=[0,1],ytitle='optical depth',xtitle='Wavelength (nm)'
  for i=0, n_elements(wvl0499.field01[0,*])-1 do begin
  spectrum=[wvl0353.field01[11,i],wvl0380.field01[11,i],wvl0452.field01[11,i],wvl0499.field01[11,i],$
 wvl0519.field01[11,i],wvl0605.field01[11,i],wvl0675.field01[11,i],wvl0779.field01[11,i],$
 wvl0864.field01[11,i],wvl1019.field01[11,i]]/[wvl0353.field01[7,i],wvl0380.field01[7,i],$
 wvl0452.field01[7,i],wvl0499.field01[7,i],wvl0519.field01[7,i],wvl0605.field01[7,i],$
 wvl0675.field01[7,i],wvl0779.field01[7,i],wvl0864.field01[7,i],wvl1019.field01[7,i]]
  oplot, wvl, spectrum,psym=2
  endfor
  
   device, /close
  spawn, 'convert "'+dir+'rtm_results_all.ps" "'+dir+'rtm_results_all.png" ' ;'
  spawn, 'rm -f "'+dir+'rtm_results_all.ps"'
end

  