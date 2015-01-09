; program to plot the dependance of modeled ice cloud radiances to changes in sza and ref larger than 25

@legend.pro
pro plot_mod_ice
dir='/home/leblanc/SSFR3/data/'
restore, '/argus/SSFR3/model/sp_ice_sza.out'

fp=dir+'mod_ice_sza'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=40, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,2,1] & !x.margin=[6,4]

  plot, zenlambda, sp[0,*]/max(sp[0,*]),title='Ice cloud spectra at different sza',xtitle='Wavelength (nm)',ytitle='Normalized radiance'
  oplot,zenlambda, sp[1,*]/max(sp[1,*]),color=70
  oplot,zenlambda, sp[2,*]/max(sp[2,*]),color=180
  oplot,zenlambda, sp[3,*]/max(sp[3,*]),color=250

  legend,['tau='+strtrim(fix(tau),2),'ref='+strtrim(fix(ref),2)+' um','sza='+strtrim(fix(szas),2)],textcolors=[0,0,0,70,180,250],box=0,/right
  

  plot, zenlambda, smooth(deriv(zenlambda,sp[0,*]/max(sp[0,*])),2),title='Ice cloud spectra derivative at different sza',$
   xtitle='Wavelength (nm)',ytitle='Derivative',yrange=[-0.015,0.015]
  oplot,zenlambda, smooth(deriv(zenlambda,sp[1,*]/max(sp[1,*])),2),color=70
  oplot,zenlambda, smooth(deriv(zenlambda,sp[2,*]/max(sp[2,*])),2),color=180
  oplot,zenlambda, smooth(deriv(zenlambda,sp[3,*]/max(sp[3,*])),2),color=250

  legend,['sza='+strtrim(fix(szas),2)],textcolors=[0,70,180,250],box=0,/right

device, /close
spawn, 'convert '+fp+'.ps '+fp+'.png'

fp=dir+'mod_ice_sza_zoom'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=60, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,3,1] & !x.margin=[6,4]

  plot, zenlambda, smooth(deriv(zenlambda,sp[0,*]/max(sp[0,*])),2),title='Ice cloud spectra derivative at different sza',$
   xtitle='Wavelength (nm)',ytitle='Derivative',yrange=[-0.015,0.015], xrange=[900,1125]
  oplot,zenlambda, smooth(deriv(zenlambda,sp[1,*]/max(sp[1,*])),2),color=70
  oplot,zenlambda, smooth(deriv(zenlambda,sp[2,*]/max(sp[2,*])),2),color=180
  oplot,zenlambda, smooth(deriv(zenlambda,sp[3,*]/max(sp[3,*])),2),color=250

  legend,['sza='+strtrim(fix(szas),2)],textcolors=[0,70,180,250],box=0,/right

  plot, zenlambda, smooth(deriv(zenlambda,sp[0,*]/max(sp[0,*])),2),title='Ice cloud spectra derivative at different sza',$
   xtitle='Wavelength (nm)',ytitle='Derivative',yrange=[-0.01,0.01], xrange=[1125,1400]
  oplot,zenlambda, smooth(deriv(zenlambda,sp[1,*]/max(sp[1,*])),2),color=70
  oplot,zenlambda, smooth(deriv(zenlambda,sp[2,*]/max(sp[2,*])),2),color=180
  oplot,zenlambda, smooth(deriv(zenlambda,sp[3,*]/max(sp[3,*])),2),color=250

  legend,['sza='+strtrim(fix(szas),2)],textcolors=[0,70,180,250],box=0,/right

  plot, zenlambda, smooth(deriv(zenlambda,sp[0,*]/max(sp[0,*])),2),title='Ice cloud spectra derivative at different sza',$
   xtitle='Wavelength (nm)',ytitle='Derivative',yrange=[-0.005,0.005], xrange=[1400,1650]
  oplot,zenlambda, smooth(deriv(zenlambda,sp[1,*]/max(sp[1,*])),2),color=70
  oplot,zenlambda, smooth(deriv(zenlambda,sp[2,*]/max(sp[2,*])),2),color=180
  oplot,zenlambda, smooth(deriv(zenlambda,sp[3,*]/max(sp[3,*])),2),color=250

  legend,['sza='+strtrim(fix(szas),2)],textcolors=[0,70,180,250],box=0,/right

device, /close
spawn, 'convert '+fp+'.ps '+fp+'.png'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;  do the same but now with changing ref and changing habits    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

restore, '/argus/SSFR3/model/sp_ice_ref.out'
fp=dir+'mod_ice_ref'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=40, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,2,1] & !x.margin=[6,4]

  plot, zenlambda, sp[0,*]/max(sp[0,*]),title='Ice cloud spectra at different ref',xtitle='Wavelength (nm)',ytitle='Normalized radiance'
  oplot,zenlambda, sp[1,*]/max(sp[1,*]),color=70
  oplot,zenlambda, sp[2,*]/max(sp[2,*]),color=180
  oplot,zenlambda, sp[3,*]/max(sp[3,*]),color=250

  legend,['tau='+strtrim(fix(tau),2),'ref='+strtrim(fix(ref),2)+' um'],textcolors=[0,0,70,180,250],box=0,/right


  plot, zenlambda, smooth(deriv(zenlambda,sp[0,*]/max(sp[0,*])),2),title='Ice cloud spectra derivative at different ref',$
   xtitle='Wavelength (nm)',ytitle='Derivative',yrange=[-0.015,0.015]
  oplot,zenlambda, smooth(deriv(zenlambda,sp[1,*]/max(sp[1,*])),2),color=70
  oplot,zenlambda, smooth(deriv(zenlambda,sp[2,*]/max(sp[2,*])),2),color=180
  oplot,zenlambda, smooth(deriv(zenlambda,sp[3,*]/max(sp[3,*])),2),color=250

  legend,['ref='+strtrim(fix(ref),2)+' um'],textcolors=[0,70,180,250],box=0,/right

device, /close
spawn, 'convert '+fp+'.ps '+fp+'.png'


fp=dir+'mod_ice_ref_zoom'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=60, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,3,1] & !x.margin=[6,4]

  plot, zenlambda, smooth(deriv(zenlambda,sp[0,*]/max(sp[0,*])),2),title='Ice cloud spectra derivative at different ref',$
   xtitle='Wavelength (nm)',ytitle='Derivative',yrange=[-0.015,0.015], xrange=[900,1125]
  oplot,zenlambda, smooth(deriv(zenlambda,sp[1,*]/max(sp[1,*])),2),color=70
  oplot,zenlambda, smooth(deriv(zenlambda,sp[2,*]/max(sp[2,*])),2),color=180
  oplot,zenlambda, smooth(deriv(zenlambda,sp[3,*]/max(sp[3,*])),2),color=250

  legend,['ref='+strtrim(fix(ref),2)+' um'],textcolors=[0,70,180,250],box=0,/right

  plot, zenlambda, smooth(deriv(zenlambda,sp[0,*]/max(sp[0,*])),2),title='Ice cloud spectra derivative at different ref',$
   xtitle='Wavelength (nm)',ytitle='Derivative',yrange=[-0.01,0.01], xrange=[1125,1400]
  oplot,zenlambda, smooth(deriv(zenlambda,sp[1,*]/max(sp[1,*])),2),color=70
  oplot,zenlambda, smooth(deriv(zenlambda,sp[2,*]/max(sp[2,*])),2),color=180
  oplot,zenlambda, smooth(deriv(zenlambda,sp[3,*]/max(sp[3,*])),2),color=250

  legend,['ref='+strtrim(fix(ref),2)+' um'],textcolors=[0,70,180,250],box=0,/right

  plot, zenlambda, smooth(deriv(zenlambda,sp[0,*]/max(sp[0,*])),2),title='Ice cloud spectra derivative at different ref',$
   xtitle='Wavelength (nm)',ytitle='Derivative',yrange=[-0.005,0.005], xrange=[1400,1650]
  oplot,zenlambda, smooth(deriv(zenlambda,sp[1,*]/max(sp[1,*])),2),color=70
  oplot,zenlambda, smooth(deriv(zenlambda,sp[2,*]/max(sp[2,*])),2),color=180
  oplot,zenlambda, smooth(deriv(zenlambda,sp[3,*]/max(sp[3,*])),2),color=250

  legend,['ref='+strtrim(fix(ref),2)+' um'],textcolors=[0,70,180,250],box=0,/right

device, /close
spawn, 'convert '+fp+'.ps '+fp+'.png'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;  do the same for the mixture of liquid water and ice clouds   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

restore, '/argus/SSFR3/model/sp_ice_liq_mix_new.out'
fp=dir+'mod_ice_mix'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=40, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,2,1] & !x.margin=[6,4]

  plot, zenlambda, sp[0,*]/max(sp[0,*]),title='Ice/liquid water cloud mixtures spectra',xtitle='Wavelength (nm)',$
   ytitle='Normalized radiance'
  for i=1, 10 do oplot, zenlambda, sp[i,*]/max(sp[i,*]),color=i*25

  legend,['ice portion',strtrim(fix(ice),2)+'%'],textcolors=[0,ice*2.5],box=0,/right


  plot, zenlambda, smooth(deriv(zenlambda,sp[0,*]/max(sp[0,*])),2),title='Ice/liquid water cloud mixtures derivative',$
   xtitle='Wavelength (nm)',ytitle='Derivative',yrange=[-0.015,0.015]
  for i=1, 10 do oplot,zenlambda, smooth(deriv(zenlambda,sp[i,*]/max(sp[i,*])),2),color=i*25

  legend,['ice portion',strtrim(fix(ice),2)+'%'],textcolors=[0,ice*2.5],box=0,/right

device, /close
spawn, 'convert '+fp+'.ps '+fp+'.png'

fp=dir+'mod_ice_mix_zoom'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=60, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,3,1] & !x.margin=[6,4]

  plot, zenlambda, smooth(deriv(zenlambda,sp[0,*]/max(sp[0,*])),2),title='Ice/liquid cloud spectra derivative mixtures',$
   xtitle='Wavelength (nm)',ytitle='Derivative',yrange=[-0.015,0.015], xrange=[900,1125]
  for i=1, 10 do oplot,zenlambda, smooth(deriv(zenlambda,sp[i,*]/max(sp[i,*])),2),color=i*25
  legend,['ice portion',strtrim(fix(ice),2)+'%'],textcolors=[0,ice*2.5],box=0,/right

  plot, zenlambda, smooth(deriv(zenlambda,sp[0,*]/max(sp[0,*])),2),title='Ice/liquid cloud spectra derivative mixtures',$
   xtitle='Wavelength (nm)',ytitle='Derivative',yrange=[-0.01,0.01], xrange=[1125,1400]
  for i=1, 10 do oplot,zenlambda, smooth(deriv(zenlambda,sp[i,*]/max(sp[i,*])),2),color=i*25
  legend,['ice portion',strtrim(fix(ice),2)+'%'],textcolors=[0,ice*2.5],box=0,/right


  plot, zenlambda, smooth(deriv(zenlambda,sp[0,*]/max(sp[0,*])),2),title='Ice/liquid cloud spectra derivative mixtures',$
   xtitle='Wavelength (nm)',ytitle='Derivative',yrange=[-0.005,0.005], xrange=[1400,1650]
  for i=1, 10 do oplot,zenlambda, smooth(deriv(zenlambda,sp[i,*]/max(sp[i,*])),2),color=i*25
  legend,['ice portion',strtrim(fix(ice),2)+'%'],textcolors=[0,ice*2.5],box=0,/right

device, /close
spawn, 'convert '+fp+'.ps '+fp+'.png'

fp=dir+'mod_ice_mix_sp_zoom'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=60, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,3,1] & !x.margin=[6,4]

  plot, zenlambda, sp[0,*]/max(sp[0,*]),title='Ice/liquid cloud spectra mixtures',$
   xtitle='Wavelength (nm)',ytitle='Derivative',yrange=[0,0.9], xrange=[900,1125]
  for i=1, 10 do oplot,zenlambda, sp[i,*]/max(sp[i,*]),color=i*25
  legend,['ice portion',strtrim(fix(ice),2)+'%'],textcolors=[0,ice*2.5],box=0,/right

  plot, zenlambda, sp[0,*]/max(sp[0,*]),title='Ice/liquid cloud spectra mixtures',$
   xtitle='Wavelength (nm)',ytitle='Derivative',yrange=[0,0.7], xrange=[1125,1400]
  for i=1, 10 do oplot,zenlambda, sp[i,*]/max(sp[i,*]),color=i*25
  legend,['ice portion',strtrim(fix(ice),2)+'%'],textcolors=[0,ice*2.5],box=0,/right


  plot, zenlambda, sp[0,*]/max(sp[0,*]),title='Ice/liquid cloud spectra mixtures',$
   xtitle='Wavelength (nm)',ytitle='Derivative',yrange=[0,0.5], xrange=[1400,1650]
  for i=1, 10 do oplot,zenlambda, sp[i,*]/max(sp[i,*]),color=i*25
  legend,['ice portion',strtrim(fix(ice),2)+'%'],textcolors=[0,ice*2.5],box=0,/right

device, /close
spawn, 'convert '+fp+'.ps '+fp+'.png'


stop
end
