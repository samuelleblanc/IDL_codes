; program to plot a sample retrieval of boulder cloud properties from 20120525
; compare to GOES determined at 15.75 UTC of tau=20 and ref=8.8 water cloud
; save from other program

pro plot_sample_retr

dir='/argus/roof/SSFR3/retrieved/' ;'/home/leblanc/SSFR3/data/'
;dir='C:\Users\Samuel\Research\SSFR3\data\'
lbls=['20120523',$
      '20120525',$
      '20120602',$
      '20120806',$
      '20120813',$
      '20120816',$
      '20120820',$
      '20120824',$
      '20120912',$
      '20130110',$
      '20130111']

nlbl=n_elements(lbls)

p=1

  restore, dir+'retrieved_pdf_'+lbls[p]+'_limp_v2.out' ;'_limp.out'
  tau_rtm1=tau_rtm & ref_rtm1=ref_rtm & wp_rtm1=wp_rtm & tau_err1=tau_err & ref_err1=ref_err & wp_err1=wp_err
  restore, dir+'retrieved_'+lbls[p]+'_pat.out' ;_sp20.out'
  tau_rtm2=tau_rtm & ref_rtm2=ref_rtm & wp_rtm2=wp_rtm & tau_err2=tau_err & ref_err2=ref_err & wp_err2=wp_err
  restore, dir+'retrieved_'+lbls[p]+'_2wvl.out' ;_sp20.out'
  tau_rtm3=tau_rtm & ref_rtm3=ref_rtm & wp_rtm3=wp_rtm & tau_err3=tau_err & ref_err3=ref_err & wp_err3=wp_err

  nans2=where(wp_rtm2 eq 1.,ct2)
  nans3=where(wp_rtm3 eq 1.,ct3)
  if ct2 gt 0 then begin
    tau_rtm2[nans2]=!values.f_nan
    ref_rtm2[nans2]=!values.f_nan
  endif
  if ct3 gt 0 then begin
    tau_rtm3[nans3]=!values.f_nan
    ref_rtm3[nans3]=!values.f_nan
  endif
;  ref_rtm2=ref_rtm2+2.
;  ref_rtm3=ref_rtm3+2.
;  ref_err2=ref_err2+2.
;  ref_err3=ref_err3+2.
  nans1=where(ref_rtm1 le 2.,ct1)
  nans2=where(ref_rtm2 le 2.,ct2)
  nans3=where(ref_rtm3 le 2.,ct3)
  if ct1 gt 0 then begin
    tau_rtm1[nans1]=!values.f_nan
    ref_rtm1[nans1]=!values.f_nan
  endif
  if ct2 gt 0 then begin
    tau_rtm2[nans2]=!values.f_nan
    ref_rtm2[nans2]=!values.f_nan
  endif
  if ct3 gt 0 then begin
    tau_rtm3[nans3]=!values.f_nan
    ref_rtm3[nans3]=!values.f_nan
  endif
  fhi=where(finite(tau_rtm1) eq 1)
  fhm=where(finite(tau_rtm2) eq 1)
  fhc=where(finite(tau_rtm3) eq 1)

  fp=dir+'sample_retr_tmhrs'+lbls[p]
  print, 'making plot :'+fp
  set_plot, 'ps'
  loadct, 39, /silent
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
  device, xsize=30, ysize=15 ;xsize was 30
   !p.font=1 & !p.thick=5 & !p.charsize=2.4 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
   !p.multi=[0,1,2] & !x.margin=[6,3] & !y.margin=[3,1] & !p.symsize=1.5 ; charsize was 4.2
  tvlct,225,90,90,253
  tvlct,90,225,90,133
  tvlct,90,90,225,40
tvlct, 150,0,150,40
tvlct, 150,170,0,253
tvlct, 255,125,0,153
  
usersym, [ -1, 1, 1, -1, -1 ], [ 1, 1, -1, -1, 1 ], /fill

plot, tmhrs, tau_rtm1, /nodata, ytitle='Optical depth',yrange=[0,160],ymargin=[1.3,1],xtickname=[' ',' ',' ',' '],xticklen=0.1 ;4 was -2
  oplot, tmhrs[fhi],tau_rtm1[fhi], psym=-4, color=40
  errplot, tmhrs[fhi],tau_err1[0,fhi],tau_err1[1,fhi],color=40
  oplot, tmhrs[fhm],tau_rtm2[fhm], psym=-1, color=253
  errplot, tmhrs[fhm],tau_err2[0,fhm],tau_err2[1,fhm],color=253
  oplot, tmhrs[fhc],tau_rtm3[fhc], psym=-5, color=153
  errplot, tmhrs[fhc],tau_err3[0,fhc],tau_err3[1,fhc],color=153
  plots, 15.75,20.,color=50,psym=8

  plot, tmhrs, ref_rtm1, /nodata, xtitle='UTC (H)',ytitle='Effective radius (!9m!Xm)',yrange=[0,25],ymargin=[3,-0.8],xticklen=0.1 ;4 was -2
  oplot, tmhrs[fhi],ref_rtm1[fhi], psym=-4, color=40
  errplot, tmhrs[fhi],ref_err1[0,fhi],ref_err1[1,fhi],color=40
  oplot, tmhrs[fhm],ref_rtm2[fhm], psym=-1, color=253
  errplot, tmhrs[fhm],ref_err2[0,fhm],ref_err2[1,fhm],color=253
  oplot, tmhrs[fhc],ref_rtm3[fhc], psym=-5, color=153
  errplot, tmhrs[fhc],ref_err3[0,fhc],ref_err3[1,fhc],color=153
  plots, 15.75,8.8,color=50,psym=8
  legend,['Multi-parameter','Slope','2-Wavelengths'],textcolors=[40,253,153],box=0,charsize=2.0
  legend,['GOES'],textcolors=[50],color=[50],box=0,charsize=2.0,psym=8,/right,/fill,position=[15.39,21.7]
  device, /close
  spawn, 'convert "'+fp+'.ps" "'+fp+'.png"'

stop
end
