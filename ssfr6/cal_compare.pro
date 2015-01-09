;+
; NAME:
;   cal_compare
;
; PURPOSE:
;   To compare different calibrations done in lab
;   useful for determining dependence of SSFR on fiber optic mating angles
;   all directories are hardcoded
;
; CATEGORY:
;   SSFR calibration comparison
;
; CALLING SEQUENCE:
;   cal_compare
;   
; OUTPUT:
;   outputs plots of the calibrations
;   - absolute values overplotted to each other
;   - relative velus overplotted
;
; KEYWORDS:
;   none
;
; DEPENDENCIES:
;   - ingest_ssfr.pro
;   - legend.pro  
;   - get_wvl.pro
;   - cfg.pro
; 
; NEEDED FILES:
;   - .OSA2 file 
;   
; EXAMPLE:
;   
;
; MODIFICATION HISTORY:
; Written:  Samuel LeBlanc, LASP CU Boulder, March 22nd, 2012
; Modified: 
;           
;---------------------------------------------------------------------------

@ingest_ssfr.pro
@get_wvl.pro
@cfg.pro
@legend.pro
pro cal_compare
  dir='/home/leblanc/SSFR6/cal/20120322/'
  head_cal=['12/nadir/cal/spc00000.OSA2','3/nadir/cal/spc00000.OSA2','6/nadir/cal/spc00000.OSA2','9/nadir/cal/spc00000.OSA2']
  head_drk=['12/nadir/dark/spc00000.OSA2','3/nadir/dark/spc00000.OSA2','6/nadir/dark/spc00000.OSA2','9/nadir/dark/spc00000.OSA2']
  ssfrz_cal=['12_ssfr/zenith/cal/spc00000.OSA2','3_ssfr/zenith/cal/spc00000.OSA2','6_ssfr/zenith/cal/spc00000.OSA2','9_ssfr/zenith/cal/spc00000.OSA2']
  ssfrn_cal=['12_ssfr/nadir/cal/spc00000.OSA2','3_ssfr/nadir/cal/spc00000.OSA2','6_ssfr/nadir/cal/spc00000.OSA2','9_ssfr/nadir/cal/spc00000.OSA2']
  ssfrz_drk=['12_ssfr/zenith/dark/spc00000.OSA2','3_ssfr/zenith/dark/spc00000.OSA2','6_ssfr/zenith/dark/spc00000.OSA2','9_ssfr/zenith/dark/spc00000.OSA2']
  ssfrn_drk=['12_ssfr/nadir/dark/spc00000.OSA2','3_ssfr/nadir/dark/spc00000.OSA2','6_ssfr/nadir/dark/spc00000.OSA2','9_ssfr/nadir/dark/spc00000.OSA2']
  fiber_cal=['12/nadir/cal/spc00000.OSA2','fiber_small_roll/cal/spc00000.OSA2']
  fiber_drk=['12/nadir/dark/spc00000.OSA2','fiber_small_roll/dark/spc00000.OSA2']
  head_name=['12','3','6','9']
  ssfr_name=head_name
  fiber_name=['Loose bends','Tight bends']
  date='20120322'
 
  dir2='/home/leblanc/SSFR6/cal/20120323/'
  date2='20120323'
  ssfrz2_cal=['12_ssfr2/zenith/cal/spc00000.OSA2','3_ssfr2/zenith/cal/spc00000.OSA2','6_ssfr2/zenith/cal/spc00000.OSA2','9_ssfr2/zenith/cal/spc00000.OSA2']
  ssfrn2_cal=['12_ssfr2/nadir/cal/spc00000.OSA2','3_ssfr2/nadir/cal/spc00000.OSA2','6_ssfr2/nadir/cal/spc00000.OSA2','9_ssfr2/nadir/cal/spc00000.OSA2']
  ssfrz2_drk=['12_ssfr2/zenith/dark/spc00000.OSA2','3_ssfr2/zenith/dark/spc00000.OSA2','6_ssfr2/zenith/dark/spc00000.OSA2','9_ssfr2/zenith/dark/spc00000.OSA2']
  ssfrn2_drk=['12_ssfr2/nadir/dark/spc00000.OSA2','3_ssfr2/nadir/dark/spc00000.OSA2','6_ssfr2/nadir/dark/spc00000.OSA2','9_ssfr2/nadir/dark/spc00000.OSA2']
  fiber2_cal=['9_ssfr2/nadir/cal/spc00000.OSA2','fiber_small/cal/spc00000.OSA2']
  fiber2_drk=['9_ssfr2/nadir/dark/spc00000.OSA2','fiber_small/dark/spc00000.OSA2']
 
  ; get the wavelength arrays for SSFR6
  cfg_file='/data/seven/schmidt/attrex/cal/ssfr6/ssfr6.cfg'; '/data/seven/schmidt/attrex/cal/ssfr6/ssfr6.cfg '
  get_wvl, cfg_file, wvl, zenlambda, nadlambda, indices
  ing=indgen(indices[1,3]-indices[0,3])+indices[0,3]
  sil=indgen(indices[1,2]-indices[0,2])+indices[0,2]
  ingz=indgen(indices[1,1]-indices[0,1])+indices[0,1]
  silz=indgen(indices[1,0]-indices[0,0])+indices[0,0]
  
  
  ; head
  head_spc = fltarr(n_elements(nadlambda),4)
  head_raw = fltarr(n_elements(nadlambda),4)
  head_dark= fltarr(n_elements(nadlambda),4)
  
  ; get the average cals
  for i=0,3 do begin
  ingest_ssfr,dir+head_cal[i],date,ssfr_utc,raw,drk,ch,int,n_drk,err,n_sat,n_ctr
  ingest_ssfr,dir+head_drk[i],date,ssfr_utc,rawt,drk,ch,int,n_drk,err,n_sat,n_ctr
  for j=0,indices[1,2]-indices[0,2]-1 do begin
    head_raw[j,i]=mean(raw[*,sil[j],2],/nan)  
	head_dark[j,i]=drk[sil[j],2]
  endfor
  for j=0,indices[1,3]-indices[0,3]-1 do begin
    head_raw[j+indices[1,2]-indices[0,2],i]=mean(raw[*,ing[j],3],/nan) 
	head_dark[j+indices[1,2]-indices[0,2],i]=drk[ing[j],3]
  endfor
  endfor
  head_spc=head_raw-head_dark
  
  ;now plot the results
  set_plot, 'ps'
  loadct, 39,/silent
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=dir+date+'_head.ps'
   device, xsize=40, ysize=20
   !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8 & !p.multi=[0,2,1]
  
  plot, nadlambda,head_spc[*,0],xtitle='Wavelength (nm)',ytitle='Counts',title='Different mating angles to the optical head' 
  oplot, nadlambda,head_spc[*,1], color=50
  oplot, nadlambda,head_spc[*,2], color=150
  oplot, nadlambda,head_spc[*,3], color=250
  legend,head_name, box=0,/right,linestyle=[0,0,0,0],textcolors=[0,50,150,250],color=[0,50,150,250]
  
  plot, nadlambda,head_spc[*,0]/head_spc[*,0]*100.,yrange=[80,110],xtitle='Wavelength (nm)',ytitle='Percent difference(%)',title='Mating angles at the Light Collector'
  oplot, nadlambda,head_spc[*,1]/head_spc[*,0]*100., color=50
  oplot, nadlambda,head_spc[*,2]/head_spc[*,0]*100., color=150
  oplot, nadlambda,head_spc[*,3]/head_spc[*,0]*100., color=250
  legend,head_name, box=0,/right,linestyle=[0,0,0,0],textcolors=[0,50,150,250],color=[0,50,150,250]
  
  device, /close
spawn, 'convert '+dir+date+'_head.ps '+dir+date+'_head.png'
spawn, 'rm -f '+dir+date+'_head.ps'

  ; zenith ssfr
  ssfrz_spc = fltarr(n_elements(nadlambda),4)
  ssfrz_raw = fltarr(n_elements(nadlambda),4)
  ssfrz_dark= fltarr(n_elements(nadlambda),4)
  
  ; get the average cals
  for i=0,3 do begin
  ingest_ssfr,dir+ssfrz_cal[i],date,ssfr_utc,raw,drk,ch,int,n_drk,err,n_sat,n_ctr
  ingest_ssfr,dir+ssfrz_drk[i],date,ssfr_utc,rawt,drk,ch,int,n_drk,err,n_sat,n_ctr
  for j=0,indices[1,0]-indices[0,0]-1 do begin
    ssfrz_raw[j,i]=mean(raw[*,silz[j],0],/nan)  
    ssfrz_dark[j,i]=drk[silz[j],0]
  endfor
  for j=0,indices[1,1]-indices[0,1]-1 do begin
    ssfrz_raw[j+indices[1,0]-indices[0,0],i]=mean(raw[*,ingz[j],1],/nan) 
    ssfrz_dark[j+indices[1,0]-indices[0,0],i]=drk[ingz[j],1]
  endfor
  endfor
  ssfrz_spc=ssfrz_raw-ssfrz_dark
  
  ;now plot the results
  set_plot, 'ps'
  loadct, 39,/silent
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=dir+date+'_ssfrz.ps'
   device, xsize=40, ysize=20
   !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8 & !p.multi=[0,2,1]
  
  plot, nadlambda,ssfrz_spc[*,0],xtitle='Wavelength (nm)',ytitle='Counts',title='Different mating angles to the SSFR-zenith' 
  oplot, nadlambda,ssfrz_spc[*,1], color=50
  oplot, nadlambda,ssfrz_spc[*,2], color=150
  oplot, nadlambda,ssfrz_spc[*,3], color=250
  legend,ssfr_name, box=0,/right,linestyle=[0,0,0,0],textcolors=[0,50,150,250],color=[0,50,150,250]
  
  plot, nadlambda,ssfrz_spc[*,0]/ssfrz_spc[*,0]*100.,yrange=[80,110],xtitle='Wavelength (nm)',ytitle='Percent difference(%)',title='Mating angles at the Zenith Spectrometer' 
  oplot, nadlambda,ssfrz_spc[*,1]/ssfrz_spc[*,0]*100., color=50
  oplot, nadlambda,ssfrz_spc[*,2]/ssfrz_spc[*,0]*100., color=150
  oplot, nadlambda,ssfrz_spc[*,3]/ssfrz_spc[*,0]*100., color=250
  legend,head_name, box=0,/right,linestyle=[0,0,0,0],textcolors=[0,50,150,250],color=[0,50,150,250]
  
  device, /close
spawn, 'convert '+dir+date+'_ssfrz.ps '+dir+date+'_ssfrz.png'
spawn, 'rm -f '+dir+date+'_ssfrz.ps'

  ; nadir ssfr
  ssfrn_spc = fltarr(n_elements(zenlambda),4)
  ssfrn_raw = fltarr(n_elements(zenlambda),4)
  ssfrn_dark= fltarr(n_elements(zenlambda),4)
  
  ; get the average cals
  for i=0,3 do begin
  ingest_ssfr,dir+ssfrn_cal[i],date,ssfr_utc,raw,drk,ch,int,n_drk,err,n_sat,n_ctr
  ingest_ssfr,dir+ssfrn_drk[i],date,ssfr_utc,rawt,drk,ch,int,n_drk,err,n_sat,n_ctr
  for j=0,indices[1,2]-indices[0,2]-1 do begin
    ssfrn_raw[j,i]=mean(raw[*,sil[j],2],/nan)  
    ssfrn_dark[j,i]=drk[sil[j],2]
  endfor
  for j=0,indices[1,3]-indices[0,3]-1 do begin
    ssfrn_raw[j+indices[1,2]-indices[0,2],i]=mean(raw[*,ing[j],3],/nan) 
    ssfrn_dark[j+indices[1,2]-indices[0,2],i]=drk[ing[j],3]
  endfor
  endfor
  ssfrn_spc=ssfrn_raw-ssfrn_dark
  
  ;now plot the results
  set_plot, 'ps'
  loadct, 39,/silent
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=dir+date+'_ssfrn.ps'
   device, xsize=40, ysize=20
   !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8 & !p.multi=[0,2,1]
  
  plot, nadlambda,ssfrn_spc[*,0],xtitle='Wavelength (nm)',ytitle='Counts',title='Changes in mating angles to the SSFR-nadir' 
  oplot, nadlambda,ssfrn_spc[*,1], color=50
  oplot, nadlambda,ssfrn_spc[*,2], color=150
  oplot, nadlambda,ssfrn_spc[*,3], color=250
  legend,ssfr_name, box=0,/right,linestyle=[0,0,0,0],textcolors=[0,50,150,250],color=[0,50,150,250]
  
  plot, nadlambda,ssfrn_spc[*,0]/ssfrn_spc[*,0]*100.,yrange=[80,110],xtitle='Wavelength (nm)',ytitle='Percent difference(%)',title='Mating angles at the Nadir Spectrometer' 
  oplot, nadlambda,ssfrn_spc[*,1]/ssfrn_spc[*,0]*100., color=50
  oplot, nadlambda,ssfrn_spc[*,2]/ssfrn_spc[*,0]*100., color=150
  oplot, nadlambda,ssfrn_spc[*,3]/ssfrn_spc[*,0]*100., color=250
  legend,ssfr_name, box=0,/right,linestyle=[0,0,0,0],textcolors=[0,50,150,250],color=[0,50,150,250]
  
  device, /close
spawn, 'convert '+dir+date+'_ssfrn.ps '+dir+date+'_ssfrn.png'
spawn, 'rm -f '+dir+date+'_ssfrn.ps'

  ; fiber
  fiber_spc = fltarr(n_elements(nadlambda),2)
  fiber_raw = fltarr(n_elements(nadlambda),2)
  fiber_dark= fltarr(n_elements(nadlambda),2)
  
  ; get the average cals
  for i=0,1 do begin
  ingest_ssfr,dir+fiber_cal[i],date,ssfr_utc,raw,drk,ch,int,n_drk,err,n_sat,n_ctr
  ingest_ssfr,dir+fiber_drk[i],date,ssfr_utc,rawt,drk,ch,int,n_drk,err,n_sat,n_ctr
  for j=0,indices[1,2]-indices[0,2]-1 do begin
    fiber_raw[j,i]=mean(raw[*,sil[j],2],/nan)  
    fiber_dark[j,i]=drk[sil[j],2]
  endfor
  for j=0,indices[1,3]-indices[0,3]-1 do begin
    fiber_raw[j+indices[1,2]-indices[0,2],i]=mean(raw[*,ing[j],3],/nan) 
    fiber_dark[j+indices[1,2]-indices[0,2],i]=drk[ing[j],3]
  endfor
  endfor
  fiber_spc=fiber_raw-fiber_dark
  
  ;now plot the results
  set_plot, 'ps'
  loadct, 39,/silent
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=dir+date+'_fiber.ps'
   device, xsize=40, ysize=20
   !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8 & !p.multi=[0,2,1]
  
  plot, nadlambda,fiber_spc[*,0],xtitle='Wavelength (nm)',ytitle='Counts',title='Different fiber optics routing' 
  oplot, nadlambda,fiber_spc[*,1], color=250
  legend,fiber_name, box=0,/right,linestyle=[0,0],textcolors=[0,250],color=[0,250]
  
  plot, nadlambda,fiber_spc[*,0]/fiber_spc[*,0]*100.,yrange=[80,110],xtitle='Wavelength (nm)',ytitle='Percent difference(%)',title='Bending radius of the Fiber 1' 
  oplot, nadlambda,fiber_spc[*,1]/fiber_spc[*,0]*100., color=250
  legend,fiber_name, box=0,/right,linestyle=[0,0],textcolors=[0,250],color=[0,250]
  
  device, /close
spawn, 'convert '+dir+date+'_fiber.ps '+dir+date+'_fiber.png'
spawn, 'rm -f '+dir+date+'_fiber.ps'

  ; zenith ssfr
  ssfrz2_spc = fltarr(n_elements(nadlambda),4)
  ssfrz2_raw = fltarr(n_elements(nadlambda),4)
  ssfrz2_dark= fltarr(n_elements(nadlambda),4)
  
  ; get the average cals
  for i=0,3 do begin
  ingest_ssfr,dir2+ssfrz2_cal[i],date2,ssfr_utc,raw,drk,ch,int,n_drk,err,n_sat,n_ctr
  ingest_ssfr,dir2+ssfrz2_drk[i],date2,ssfr_utc,rawt,drk,ch,int,n_drk,err,n_sat,n_ctr
  for j=0,indices[1,0]-indices[0,0]-1 do begin
    ssfrz2_raw[j,i]=mean(raw[*,silz[j],0],/nan)  
    ssfrz2_dark[j,i]=drk[silz[j],0]
  endfor
  for j=0,indices[1,1]-indices[0,1]-1 do begin
    ssfrz2_raw[j+indices[1,0]-indices[0,0],i]=mean(raw[*,ingz[j],1],/nan) 
    ssfrz2_dark[j+indices[1,0]-indices[0,0],i]=drk[ingz[j],1]
  endfor
  endfor
  ssfrz2_spc=ssfrz2_raw-ssfrz2_dark
  
  ;now plot the results
  set_plot, 'ps'
  loadct, 39,/silent
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=dir+date+'_ssfrz.ps'
   device, xsize=40, ysize=20
   !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8 & !p.multi=[0,2,1]
  
  plot, nadlambda,ssfrz2_spc[*,0],xtitle='Wavelength (nm)',ytitle='Counts',title='Different mating angles to the SSFR-zenith' 
  oplot, nadlambda,ssfrz2_spc[*,1], color=50
  oplot, nadlambda,ssfrz2_spc[*,2], color=150
  oplot, nadlambda,ssfrz2_spc[*,3], color=250
  legend,ssfr_name, box=0,/right,linestyle=[0,0,0,0],textcolors=[0,50,150,250],color=[0,50,150,250]
  
  plot, nadlambda,ssfrz2_spc[*,0]/ssfrz_spc[*,0]*100.,yrange=[80,110],xtitle='Wavelength (nm)',ytitle='Percent difference(%)',title='Mating angles at the Zenith Spectrometer (Fiber 2)' 
  oplot, nadlambda,ssfrz2_spc[*,1]/ssfrz_spc[*,0]*100., color=50
  oplot, nadlambda,ssfrz2_spc[*,2]/ssfrz_spc[*,0]*100., color=150
  oplot, nadlambda,ssfrz2_spc[*,3]/ssfrz_spc[*,0]*100., color=250
  legend,head_name, box=0,/right,linestyle=[0,0,0,0],textcolors=[0,50,150,250],color=[0,50,150,250]
  
  device, /close
spawn, 'convert '+dir+date+'_ssfrz.ps '+dir+date+'_ssfrz2.png'
spawn, 'rm -f '+dir+date+'_ssfrz.ps'

  ; nadir ssfr
  ssfrn2_spc = fltarr(n_elements(zenlambda),4)
  ssfrn2_raw = fltarr(n_elements(zenlambda),4)
  ssfrn2_dark= fltarr(n_elements(zenlambda),4)
  
  ; get the average cals
  for i=0,3 do begin
  ingest_ssfr,dir2+ssfrn2_cal[i],date2,ssfr_utc,raw,drk,ch,int,n_drk,err,n_sat,n_ctr
  ingest_ssfr,dir2+ssfrn2_drk[i],date2,ssfr_utc,rawt,drk,ch,int,n_drk,err,n_sat,n_ctr
  for j=0,indices[1,2]-indices[0,2]-1 do begin
    ssfrn2_raw[j,i]=mean(raw[*,sil[j],2],/nan)  
    ssfrn2_dark[j,i]=drk[sil[j],2]
  endfor
  for j=0,indices[1,3]-indices[0,3]-1 do begin
    ssfrn2_raw[j+indices[1,2]-indices[0,2],i]=mean(raw[*,ing[j],3],/nan) 
    ssfrn2_dark[j+indices[1,2]-indices[0,2],i]=drk[ing[j],3]
  endfor
  endfor
  ssfrn2_spc=ssfrn2_raw-ssfrn2_dark
  
  ;now plot the results
  set_plot, 'ps'
  loadct, 39,/silent
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=dir+date+'_ssfrn.ps'
   device, xsize=40, ysize=20
   !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8 & !p.multi=[0,2,1]
  
  plot, nadlambda,ssfrn2_spc[*,0],xtitle='Wavelength (nm)',ytitle='Counts',title='Changes in mating angles to the SSFR-nadir' 
  oplot, nadlambda,ssfrn2_spc[*,1], color=50
  oplot, nadlambda,ssfrn2_spc[*,2], color=150
  oplot, nadlambda,ssfrn2_spc[*,3], color=250
  legend,ssfr_name, box=0,/right,linestyle=[0,0,0,0],textcolors=[0,50,150,250],color=[0,50,150,250]
  
  plot, nadlambda,ssfrn2_spc[*,0]/ssfrn_spc[*,0]*100.,yrange=[80,110],xtitle='Wavelength (nm)',ytitle='Percent difference(%)',title='Mating angles at the Nadir Spectrometer (Fiber 2)' 
  oplot, nadlambda,ssfrn2_spc[*,1]/ssfrn_spc[*,0]*100., color=50
  oplot, nadlambda,ssfrn2_spc[*,2]/ssfrn_spc[*,0]*100., color=150
  oplot, nadlambda,ssfrn2_spc[*,3]/ssfrn_spc[*,0]*100., color=250
  legend,ssfr_name, box=0,/right,linestyle=[0,0,0,0],textcolors=[0,50,150,250],color=[0,50,150,250]
  
  device, /close
spawn, 'convert '+dir+date+'_ssfrn.ps '+dir+date+'_ssfrn2.png'
spawn, 'rm -f '+dir+date+'_ssfrn.ps'

; fiber 2
  fiber2_spc = fltarr(n_elements(nadlambda),2)
  fiber2_raw = fltarr(n_elements(nadlambda),2)
  fiber2_dark= fltarr(n_elements(nadlambda),2)
  
  ; get the average cals
  for i=0,1 do begin
  ingest_ssfr,dir2+fiber2_cal[i],date2,ssfr_utc,raw,drk,ch,int,n_drk,err,n_sat,n_ctr
  ingest_ssfr,dir2+fiber2_drk[i],date2,ssfr_utc,rawt,drk,ch,int,n_drk,err,n_sat,n_ctr
  for j=0,indices[1,2]-indices[0,2]-1 do begin
    fiber2_raw[j,i]=mean(raw[*,sil[j],2],/nan)  
    fiber2_dark[j,i]=drk[sil[j],2]
  endfor
  for j=0,indices[1,3]-indices[0,3]-1 do begin
    fiber2_raw[j+indices[1,2]-indices[0,2],i]=mean(raw[*,ing[j],3],/nan) 
    fiber2_dark[j+indices[1,2]-indices[0,2],i]=drk[ing[j],3]
  endfor
  endfor
  fiber2_spc=fiber2_raw-fiber2_dark
  
  ;now plot the results
  set_plot, 'ps'
  loadct, 39,/silent
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=dir+date+'_fiber.ps'
   device, xsize=40, ysize=20
   !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8 & !p.multi=[0,2,1]
  
  plot, nadlambda,fiber2_spc[*,0],xtitle='Wavelength (nm)',ytitle='Counts',title='Different fiber optics routing' 
  oplot, nadlambda,fiber2_spc[*,1], color=250
  legend,fiber_name, box=0,/right,linestyle=[0,0],textcolors=[0,250],color=[0,250]
  
  plot, nadlambda,fiber2_spc[*,0]/fiber_spc[*,0]*100.,yrange=[80,110],xtitle='Wavelength (nm)',ytitle='Percent difference(%)',title='Bending radius of the Fiber 2' 
  oplot, nadlambda,fiber2_spc[*,1]/fiber_spc[*,0]*100., color=250
  legend,fiber_name, box=0,/right,linestyle=[0,0],textcolors=[0,250],color=[0,250]
  
  device, /close
spawn, 'convert '+dir+date+'_fiber.ps '+dir+date+'_fiber2.png'
spawn, 'rm -f '+dir+date+'_fiber.ps'
end
