; program to plot the different exmaples of spectra
; trying to link the spectra to liquid water cloud, ice wter cloud, and mixtures

@legend.pro
pro plot_spc_cat

ns=18
sps=fltarr(393,ns)
dsps=fltarr(393,ns)
utcs=fltarr(ns)
lbl=strarr(ns)
tau=fltarr(ns)*!values.f_nan
ref=fltarr(ns)*!values.f_nan

dir='/home/leblanc/SSFR3/data/'

 restore, dir+'../cloudy.out'

restore, dir+'20120806_sp_ex.out'
  sps[*,0]=sp & dsps[*,0]=smooth(dsp,2) & utcs[0]=utc & lbl[0]='8/06-1'
  t=where(totday eq 20120806 and tottmhrs eq utc) & tau[0]=tottau[t] & ref[0]=totref[t]
restore, dir+'20120912_sp_ex.out'
  sps[*,1]=sp & dsps[*,1]=smooth(dsp,2) & utcs[1]=utc & lbl[1]='9/12-1'
  t=where(totday eq 20120912 and tottmhrs eq utc) & tau[1]=tottau[t] & ref[1]=totref[t]
restore, dir+'20120523_sp_ex.out'
  sps[*,2]=sp & dsps[*,2]=smooth(dsp,2) & utcs[2]=utc & lbl[2]='5/23-1'
  t=where(totday eq 20120523 and tottmhrs eq utc) & tau[2]=tottau[t] & ref[2]=totref[t]
restore, dir+'20120816_sp_ex.out'
  sps[*,3]=sp & dsps[*,3]=smooth(dsp,2) & utcs[3]=utc & lbl[3]='8/16-1'
  t=where(totday eq 20120816 and tottmhrs eq utc) & tau[3]=tottau[t] & ref[3]=totref[t]
restore, dir+'20120813_sp_ex.out'
  sps[*,4]=sp & dsps[*,4]=smooth(dsp,2) & utcs[4]=utc & lbl[4]='8/13-1'
  t=where(totday eq 20120813 and tottmhrs eq utc) & tau[4]=tottau[t] & ref[4]=totref[t]
restore, dir+'20130110_sp_ex.out'
  sps[*,5]=sp & dsps[*,5]=smooth(dsp,2) & utcs[5]=utc & lbl[5]='1/10-1'
restore, dir+'20130111_sp_ex.out'
  sps[*,6]=sp & dsps[*,6]=smooth(dsp,2) & utcs[6]=utc & lbl[6]='1/11-1'
restore, dir+'2013011002_sp_ex.out'
  sps[*,7]=sp & dsps[*,7]=smooth(dsp,2) & utcs[7]=utc & lbl[7]='1/10-2'
restore, dir+'2013011102_sp_ex.out'
  sps[*,8]=sp & dsps[*,8]=smooth(dsp,2) & utcs[8]=utc & lbl[8]='1/11-2'
restore, dir+'2013011003_sp_ex.out'
  sps[*,9]=sp & dsps[*,9]=smooth(dsp,2) & utcs[9]=utc & lbl[9]='1/10-3'
restore, dir+'2013011103_sp_ex.out'
  sps[*,10]=sp & dsps[*,10]=smooth(dsp,2) & utcs[10]=utc & lbl[10]='1/11-3'
restore, dir+'2012081302_sp_ex.out'
  sps[*,11]=sp & dsps[*,11]=smooth(dsp,2) & utcs[11]=utc & lbl[11]='8/13-2'
  t=where(totday eq 20120813 and tottmhrs eq utc) & tau[11]=tottau[t] & ref[11]=totref[t]
restore, dir+'2012091202_sp_ex.out'
  sps[*,12]=sp & dsps[*,12]=smooth(dsp,2) & utcs[12]=utc & lbl[12]='9/12-2'
  t=where(totday eq 20120912 and tottmhrs eq utc) & tau[12]=tottau[t] & ref[12]=totref[t]
restore, dir+'2012080605_sp_ex.out'
  sps[*,13]=sp & dsps[*,13]=smooth(dsp,2) & utcs[13]=utc & lbl[13]='8/06-5'
  t=where(totday eq 20120806 and tottmhrs eq utc) & tau[13]=tottau[t] & ref[13]=totref[t]
restore, dir+'2012080604_sp_ex.out'
  sps[*,14]=sp & dsps[*,14]=smooth(dsp,2) & utcs[14]=utc & lbl[14]='8/06-4'
  t=where(totday eq 20120806 and tottmhrs eq utc) & tau[14]=tottau[t] & ref[14]=totref[t]
restore, dir+'2012080603_sp_ex.out'
  sps[*,15]=sp & dsps[*,15]=smooth(dsp,2) & utcs[15]=utc & lbl[15]='8/06-3'
  t=where(totday eq 20120806 and tottmhrs eq utc) & tau[15]=tottau[t] & ref[15]=totref[t]
restore, dir+'2012091203_sp_ex.out'
  sps[*,16]=sp & dsps[*,16]=smooth(dsp,2) & utcs[16]=utc & lbl[16]='9/12-3'
  t=where(totday eq 20120912 and tottmhrs eq utc) & tau[16]=tottau[t] & ref[16]=totref[t]
restore, dir+'2012091204_sp_ex.out'
  sps[*,17]=sp & dsps[*,17]=smooth(dsp,2) & utcs[17]=utc & lbl[17]='9/12-4'
  t=where(totday eq 20120912 and tottmhrs eq utc) & tau[17]=tottau[t] & ref[17]=totref[t]
zenlambda=wl
liq=[0,1,3,4,12,13,14]
mix=[2,11,15,16,17]
ice=[5,6,7,8,9,10]

save, wl,sps, dsps, liq, mix, ice, utcs, lbl, tau, ref, filename=dir+'phase_example.out'
stop

fp=dir+'spc_cat2'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=60, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=3.2 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,3,1] & !x.margin=[8,4]

  plot, zenlambda,sps[*,liq[0]]/max(sps[*,liq[0]]),xtitle='Wavelength (nm)',title='Liquid cloud',$
   ytitle='Normalized Radiance',xrange=[900,1700],yrange=[0,0.5]
  clliq=intarr(n_elements(liq))
  for i=0, n_elements(liq)-1 do begin
    clliq[i]=i*255/n_elements(liq)
    oplot,zenlambda,sps[*,liq[i]]/max(sps[*,liq[i]]),color=clliq[i]
  endfor
  legend, lbl[liq]+' tau='+strtrim(tau[liq],2)+' ref='+strtrim(ref[liq],2),textcolors=clliq,/right,charsize=2.0,box=0

  plot, zenlambda,sps[*,mix[0]]/max(sps[*,mix[0]]),xtitle='Wavelength (nm)',title='Mixed phase cloud',$
   ytitle='Normalized Radiance',xrange=[900,1700],yrange=[0,0.5]
  clmix=intarr(n_elements(mix))
  for i=0, n_elements(mix)-1 do begin
    clmix[i]=i*255/n_elements(mix)
    oplot,zenlambda,sps[*,mix[i]]/max(sps[*,mix[i]]),color=clmix[i]
  endfor
  legend, lbl[mix]+' tau='+strtrim(tau[mix],2)+' ref='+strtrim(ref[mix],2),textcolors=clmix,/right,charsize=2.0,box=0
  
  plot, zenlambda,sps[*,ice[0]]/max(sps[*,ice[0]]),xtitle='Wavelength (nm)',title='Ice cloud',$
   ytitle='Normalized Radiance',xrange=[900,1700],yrange=[0,0.5]
  clice=intarr(n_elements(ice))
  for i=0, n_elements(ice)-1 do begin
    clice[i]=i*255/n_elements(ice)
    oplot,zenlambda,sps[*,ice[i]]/max(sps[*,ice[i]]),color=clice[i]
  endfor
  legend, lbl[ice]+' tau='+strtrim(tau[ice],2)+' ref='+strtrim(ref[ice],2),textcolors=clice,/right,charsize=2.0,box=0
device, /close
spawn, 'convert '+fp+'.ps '+fp+'.png'

fp=dir+'spc_dps_cat2'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=60, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=3.2 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,3,1] & !x.margin=[8,4]

  plot, zenlambda,dsps[*,liq[0]],xtitle='Wavelength (nm)',title='Liquid cloud',$
   ytitle='Derivative',xrange=[900,1700],yrange=[-0.01,0.01]
  clliq=intarr(n_elements(liq))
  for i=0, n_elements(liq)-1 do begin
    clliq[i]=i*255/n_elements(liq)
    oplot,zenlambda,dsps[*,liq[i]],color=clliq[i]
  endfor
  legend, lbl[liq]+' tau='+strtrim(tau[liq],2)+' ref='+strtrim(ref[liq],2),textcolors=clliq,/right,charsize=2.0,box=0

  plot, zenlambda,dsps[*,mix[0]],xtitle='Wavelength (nm)',title='Mixed phase cloud',$
   ytitle='Derivative',xrange=[900,1700],yrange=[-0.01,0.01]
  clmix=intarr(n_elements(mix))
  for i=0, n_elements(mix)-1 do begin
    clmix[i]=i*255/n_elements(mix)
    oplot,zenlambda,dsps[*,mix[i]],color=clmix[i]
  endfor
  legend, lbl[mix]+' tau='+strtrim(tau[mix],2)+' ref='+strtrim(ref[mix],2),textcolors=clmix,/right,charsize=2.0,box=0

  plot, zenlambda,dsps[*,ice[0]],xtitle='Wavelength (nm)',title='Ice cloud',$
   ytitle='Derivative',xrange=[900,1700],yrange=[-0.01,0.01]
  clice=intarr(n_elements(ice))
  for i=0, n_elements(ice)-1 do begin
    clice[i]=i*255/n_elements(ice)
    oplot,zenlambda,dsps[*,ice[i]],color=clice[i]
  endfor
  legend, lbl[ice]+' tau='+strtrim(tau[ice],2)+' ref='+strtrim(ref[ice],2),textcolors=clice,/right,charsize=2.0,box=0

device, /close
spawn, 'convert '+fp+'.ps '+fp+'.png'


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; now apply the filter to the data

restore, '/home/leblanc/SSFR3/data/nowv_fl.out'
; now make the filter compatible with measurements
fl=fl+15
fl2=fl2+15

fp=dir+'spc_cat2_fl'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=60, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=3.2 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,3,1] & !x.margin=[8,4]

  plot, zenlambda[fl],sps[fl,liq[0]]/max(sps[*,liq[0]]),xtitle='Wavelength (nm)',title='Liquid cloud',$
   ytitle='Normalized Radiance',xrange=[900,1700],yrange=[0,0.5],psym=2
  clliq=intarr(n_elements(liq))
  for i=0, n_elements(liq)-1 do begin
    clliq[i]=i*255/n_elements(liq)
    oplot,zenlambda[fl],sps[fl,liq[i]]/max(sps[*,liq[i]]),color=clliq[i],psym=2
  endfor
  legend, lbl[liq]+' tau='+strtrim(tau[liq],2)+' ref='+strtrim(ref[liq],2),textcolors=clliq,/right,charsize=2.0,box=0

  plot, zenlambda[fl],sps[fl,mix[0]]/max(sps[*,mix[0]]),xtitle='Wavelength (nm)',title='Mixed phase cloud',$
   ytitle='Normalized Radiance',xrange=[900,1700],yrange=[0,0.5],psym=2
  clmix=intarr(n_elements(mix))
  for i=0, n_elements(mix)-1 do begin
    clmix[i]=i*255/n_elements(mix)
    oplot,zenlambda[fl],sps[fl,mix[i]]/max(sps[*,mix[i]]),color=clmix[i],psym=2
  endfor
  legend, lbl[mix]+' tau='+strtrim(tau[mix],2)+' ref='+strtrim(ref[mix],2),textcolors=clmix,/right,charsize=2.0,box=0

  plot, zenlambda[fl],sps[fl,ice[0]]/max(sps[*,ice[0]]),xtitle='Wavelength (nm)',title='Ice cloud',$
   ytitle='Normalized Radiance',xrange=[900,1700],yrange=[0,0.5],psym=2
  clice=intarr(n_elements(ice))
  for i=0, n_elements(ice)-1 do begin
    clice[i]=i*255/n_elements(ice)
    oplot,zenlambda[fl],sps[fl,ice[i]]/max(sps[*,ice[i]]),color=clice[i],psym=2
  endfor
  legend, lbl[ice]+' tau='+strtrim(tau[ice],2)+' ref='+strtrim(ref[ice],2),textcolors=clice,/right,charsize=2.0,box=0
device, /close
spawn, 'convert '+fp+'.ps '+fp+'.png'

fp=dir+'spc_cat2_fl2'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=60, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=3.2 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,3,1] & !x.margin=[8,4]

  plot, zenlambda[fl2],sps[fl2,liq[0]]/max(sps[*,liq[0]]),xtitle='Wavelength (nm)',title='Liquid cloud',$
   ytitle='Normalized Radiance',xrange=[900,1700],yrange=[0,0.5],psym=2
  clliq=intarr(n_elements(liq))
  for i=0, n_elements(liq)-1 do begin
    clliq[i]=i*255/n_elements(liq)
    oplot,zenlambda[fl2],sps[fl2,liq[i]]/max(sps[*,liq[i]]),color=clliq[i],psym=2
  endfor
  legend, lbl[liq]+' tau='+strtrim(tau[liq],2)+' ref='+strtrim(ref[liq],2),textcolors=clliq,/right,charsize=2.0,box=0

  plot, zenlambda[fl2],sps[fl2,mix[0]]/max(sps[*,mix[0]]),xtitle='Wavelength (nm)',title='Mixed phase cloud',$
   ytitle='Normalized Radiance',xrange=[900,1700],yrange=[0,0.5],psym=2
  clmix=intarr(n_elements(mix))
  for i=0, n_elements(mix)-1 do begin
    clmix[i]=i*255/n_elements(mix)
    oplot,zenlambda[fl2],sps[fl2,mix[i]]/max(sps[*,mix[i]]),color=clmix[i],psym=2
  endfor
  legend, lbl[mix]+' tau='+strtrim(tau[mix],2)+' ref='+strtrim(ref[mix],2),textcolors=clmix,/right,charsize=2.0,box=0

  plot, zenlambda[fl2],sps[fl2,ice[0]]/max(sps[*,ice[0]]),xtitle='Wavelength (nm)',title='Ice cloud',$
   ytitle='Normalized Radiance',xrange=[900,1700],yrange=[0,0.5],psym=2
  clice=intarr(n_elements(ice))
  for i=0, n_elements(ice)-1 do begin
    clice[i]=i*255/n_elements(ice)
    oplot,zenlambda[fl2],sps[fl2,ice[i]]/max(sps[*,ice[i]]),color=clice[i],psym=2
  endfor
  legend, lbl[ice]+' tau='+strtrim(tau[ice],2)+' ref='+strtrim(ref[ice],2),textcolors=clice,/right,charsize=2.0,box=0
device, /close
spawn, 'convert '+fp+'.ps '+fp+'.png'


fp=dir+'spc_dps_cat2_fl'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=60, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=3.2 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,3,1] & !x.margin=[8,4]

  plot, zenlambda[fl],dsps[fl,liq[0]],xtitle='Wavelength (nm)',title='Liquid cloud',$
   ytitle='Derivative',xrange=[900,1700],yrange=[-0.01,0.01],psym=2
  clliq=intarr(n_elements(liq))
  for i=0, n_elements(liq)-1 do begin
    clliq[i]=i*255/n_elements(liq)
    oplot,zenlambda[fl],dsps[fl,liq[i]],color=clliq[i],psym=2
  endfor
  legend, lbl[liq]+' tau='+strtrim(tau[liq],2)+' ref='+strtrim(ref[liq],2),textcolors=clliq,/right,charsize=2.0,box=0

  plot, zenlambda[fl],dsps[fl,mix[0]],xtitle='Wavelength (nm)',title='Mixed phase cloud',$
   ytitle='Derivative',xrange=[900,1700],yrange=[-0.01,0.01],psym=2
  clmix=intarr(n_elements(mix))
  for i=0, n_elements(mix)-1 do begin
    clmix[i]=i*255/n_elements(mix)
    oplot,zenlambda[fl],dsps[fl,mix[i]],color=clmix[i],psym=2
  endfor
  legend, lbl[mix]+' tau='+strtrim(tau[mix],2)+' ref='+strtrim(ref[mix],2),textcolors=clmix,/right,charsize=2.0,box=0

  plot, zenlambda[fl],dsps[fl,ice[0]],xtitle='Wavelength (nm)',title='Ice cloud',$
   ytitle='Derivative',xrange=[900,1700],yrange=[-0.01,0.01],psym=2
  clice=intarr(n_elements(ice))
  for i=0, n_elements(ice)-1 do begin
    clice[i]=i*255/n_elements(ice)
    oplot,zenlambda[fl],dsps[fl,ice[i]],color=clice[i],psym=2
  endfor
  legend, lbl[ice]+' tau='+strtrim(tau[ice],2)+' ref='+strtrim(ref[ice],2),textcolors=clice,/right,charsize=2.0,box=0

device, /close
spawn, 'convert '+fp+'.ps '+fp+'.png'

fp=dir+'spc_dps_cat2_fl2'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=60, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=3.2 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,3,1] & !x.margin=[8,4]

  plot, zenlambda[fl2],dsps[fl2,liq[0]],xtitle='Wavelength (nm)',title='Liquid cloud',$
   ytitle='Derivative',xrange=[900,1700],yrange=[-0.004,0.004],psym=2
  clliq=intarr(n_elements(liq))
  for i=0, n_elements(liq)-1 do begin
    clliq[i]=i*255/n_elements(liq)
    oplot,zenlambda[fl2],dsps[fl2,liq[i]],color=clliq[i],psym=2
  endfor
  legend, lbl[liq]+' tau='+strtrim(tau[liq],2)+' ref='+strtrim(ref[liq],2),textcolors=clliq,/right,charsize=2.0,box=0,/bottom

  plot, zenlambda[fl2],dsps[fl2,mix[0]],xtitle='Wavelength (nm)',title='Mixed phase cloud',$
   ytitle='Derivative',xrange=[900,1700],yrange=[-0.004,0.004],psym=2
  clmix=intarr(n_elements(mix))
  for i=0, n_elements(mix)-1 do begin
    clmix[i]=i*255/n_elements(mix)
    oplot,zenlambda[fl2],dsps[fl2,mix[i]],color=clmix[i],psym=2
  endfor
  legend, lbl[mix]+' tau='+strtrim(tau[mix],2)+' ref='+strtrim(ref[mix],2),textcolors=clmix,/right,charsize=2.0,box=0,/bottom

  plot, zenlambda[fl2],dsps[fl2,ice[0]],xtitle='Wavelength (nm)',title='Ice cloud',$
   ytitle='Derivative',xrange=[900,1700],yrange=[-0.004,0.004],psym=2
  clice=intarr(n_elements(ice))
  for i=0, n_elements(ice)-1 do begin
    clice[i]=i*255/n_elements(ice)
    oplot,zenlambda[fl2],dsps[fl2,ice[i]],color=clice[i],psym=2
  endfor
  legend, lbl[ice]+' tau='+strtrim(tau[ice],2)+' ref='+strtrim(ref[ice],2),textcolors=clice,/right,charsize=2.0,box=0,/bottom

device, /close
spawn, 'convert '+fp+'.ps '+fp+'.png'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; now do the same plots but with model data

restore, '/argus/SSFR3/model/sp_liq_ice_new3.out'
fp=dir+'spc_mod_cat2'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=60, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=3.2 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,3,1] & !x.margin=[8,4]


  plot, zenlambda, sp[0,1,*,0]/max(sp[0,1,*,0]),xtitle='Wavelength (nm)',$
   ytitle='Normalized radiance',title='Modeled liquid cloud spectra',xrange=[900,1700], yrange=[0,0.5]
  oplot,zenlambda, sp[0,4,*,0]/max(sp[0,4,*,0]), color=120
  oplot,zenlambda, sp[3,1,*,0]/max(sp[3,1,*,0]), color=70
  oplot,zenlambda, sp[3,4,*,0]/max(sp[3,4,*,0]), color=250

  legend, ['ref=10 um, tau=25 ','ref=10 um, tau=150','ref=25 um, tau=25 ','ref=25 um, tau=150'],textcolors=[0,70,120,250],box=0,/right,charsize=2

  restore, '/argus/SSFR3/model/sp_ice_liq_mix_new.out'
  plot, zenlambda, sp[0,*]/max(sp[0,*]),title='Ice/liquid water cloud mixtures spectra',xtitle='Wavelength (nm)',$
   ytitle='Normalized radiance',xrange=[900,1700],yrange=[0,0.5]
  for i=1, 10 do oplot, zenlambda, sp[i,*]/max(sp[i,*]),color=i*25

  legend,['ice portion',strtrim(fix(ice),2)+'%'],textcolors=[0,ice*2.5],box=0,/right,charsize=2


  restore, '/argus/SSFR3/model/sp_liq_ice_new3.out'
  plot, zenlambda, sp[0,1,*,1]/max(sp[0,1,*,1]), xtitle='Wavelength (nm)',$
   ytitle='Normalized radiance',title='Modeled ice cloud spectra',xrange=[900,1700], yrange=[0,0.5]
  oplot,zenlambda, sp[0,4,*,1]/max(sp[0,4,*,1]), color=120
  oplot,zenlambda, sp[3,1,*,1]/max(sp[3,1,*,1]), color=70
  oplot,zenlambda, sp[3,4,*,1]/max(sp[3,4,*,1]), color=250

  legend, ['ref=10 um, tau=25 ','ref=10 um, tau=150','ref=25 um, tau=25 ','ref=25 um, tau=150'],textcolors=[0,70,120,250],box=0,/right,charsize=2

  device, /close
  spawn, 'convert '+fp+'.ps '+fp+'.png'


  restore, '/argus/SSFR3/model/sp_liq_ice_new3.out'
fl=fl-15
fl2=fl2-15
fp=dir+'spc_mod_cat2_fl'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=60, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=3.2 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,3,1] & !x.margin=[8,4]

  plot, zenlambda[fl], sp[0,1,fl,0]/max(sp[0,1,*,0]),xtitle='Wavelength (nm)',$
   ytitle='Normalized radiance',title='Modeled liquid cloud spectra',xrange=[900,1700], yrange=[0,0.5],psym=2
  oplot,zenlambda[fl], sp[0,4,fl,0]/max(sp[0,4,*,0]), color=120,psym=2
  oplot,zenlambda[fl], sp[3,1,fl,0]/max(sp[3,1,*,0]), color=70, psym=2
  oplot,zenlambda[fl], sp[3,4,fl,0]/max(sp[3,4,*,0]), color=250,psym=2

  legend, ['ref=10 um, tau=25 ','ref=10 um, tau=150','ref=25 um, tau=25 ','ref=25 um, tau=150'],textcolors=[0,70,120,250],box=0,/right,charsize=2

  restore, '/argus/SSFR3/model/sp_ice_liq_mix_new.out'
  plot, zenlambda[fl], sp[0,fl]/max(sp[0,*]),title='Ice/liquid water cloud mixtures spectra',xtitle='Wavelength (nm)',$
   ytitle='Derivative',psym=2,xrange=[900,1700],yrange=[0,0.5]
  for i=1, 10 do oplot, zenlambda[fl], sp[i,fl]/max(sp[i,*]),color=i*25,psym=2

  legend,['ice portion',strtrim(fix(ice),2)+'%'],textcolors=[0,ice*2.5],box=0,/right,charsize=2


  restore, '/argus/SSFR3/model/sp_liq_ice_new3.out'
  plot, zenlambda[fl], sp[0,1,fl,1]/max(sp[0,1,*,1]), xtitle='Wavelength (nm)',$
   ytitle='Normalized radiance',title='Modeled ice cloud spectra',xrange=[900,1700], yrange=[0,0.5],psym=2
  oplot,zenlambda[fl], sp[0,4,fl,1]/max(sp[0,4,*,1]), color=120,psym=2
  oplot,zenlambda[fl], sp[3,1,fl,1]/max(sp[3,1,*,1]), color=70, psym=2
  oplot,zenlambda[fl], sp[3,4,fl,1]/max(sp[3,4,*,1]), color=250,psym=2

  legend, ['ref=10 um, tau=25 ','ref=10 um, tau=150','ref=25 um, tau=25 ','ref=25 um, tau=150'],textcolors=[0,70,120,250],box=0,/right,charsize=2

  device, /close
  spawn, 'convert '+fp+'.ps '+fp+'.png'





fp=dir+'spc_mod_cat2_fl2'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=60, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=3.2 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,3,1] & !x.margin=[8,4]


  plot, zenlambda[fl2], sp[0,1,fl2,0]/max(sp[0,1,*,0]),xtitle='Wavelength (nm)',$
   ytitle='Normalized radiance',title='Modeled liquid cloud spectra',xrange=[900,1700], yrange=[0,0.5],psym=2
  oplot,zenlambda[fl2], sp[0,4,fl2,0]/max(sp[0,4,*,0]), color=120,psym=2
  oplot,zenlambda[fl2], sp[3,1,fl2,0]/max(sp[3,1,*,0]), color=70, psym=2
  oplot,zenlambda[fl2], sp[3,4,fl2,0]/max(sp[3,4,*,0]), color=250,psym=2

  legend, ['ref=10 um, tau=25 ','ref=10 um, tau=150','ref=25 um, tau=25 ','ref=25 um, tau=150'],textcolors=[0,70,120,250],box=0,/right,charsize=2

  restore, '/argus/SSFR3/model/sp_ice_liq_mix_new.out'
  plot, zenlambda[fl2], sp[0,fl2]/max(sp[0,*]),title='Ice/liquid water cloud mixtures spectra',xtitle='Wavelength (nm)',$
   ytitle='Derivative',psym=2,xrange=[900,1700],yrange=[0,0.5]
  for i=1, 10 do oplot, zenlambda[fl2], sp[i,fl2]/max(sp[i,*]),color=i*25,psym=2

  legend,['ice portion',strtrim(fix(ice),2)+'%'],textcolors=[0,ice*2.5],box=0,/right,charsize=2


  restore, '/argus/SSFR3/model/sp_liq_ice_new3.out'
  plot, zenlambda[fl2], sp[0,1,fl2,1]/max(sp[0,1,*,1]), xtitle='Wavelength (nm)',$
   ytitle='Normalized radiance',title='Modeled ice cloud spectra',xrange=[900,1700], yrange=[0,0.5],psym=2
  oplot,zenlambda[fl2], sp[0,4,fl2,1]/max(sp[0,4,*,1]), color=120,psym=2
  oplot,zenlambda[fl2], sp[3,1,fl2,1]/max(sp[3,1,*,1]), color=70, psym=2
  oplot,zenlambda[fl2], sp[3,4,fl2,1]/max(sp[3,4,*,1]), color=250,psym=2

  legend, ['ref=10 um, tau=25 ','ref=10 um, tau=150','ref=25 um, tau=25 ','ref=25 um, tau=150'],textcolors=[0,70,120,250],box=0,/right,charsize=2

  device, /close
  spawn, 'convert '+fp+'.ps '+fp+'.png'


restore, '/argus/SSFR3/model/sp_liq_ice_new3.out'
fp=dir+'spc_dsp_mod_cat2'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=60, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=3.2 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,3,1] & !x.margin=[8,4]


  plot, zenlambda, smooth(deriv(zenlambda,sp[0,1,*,0]/max(sp[0,1,*,0])),2),xtitle='Wavelength (nm)',$
   ytitle='Derivative',title='Modeled liquid cloud spectra',xrange=[900,1700], yrange=[-0.01,0.01]
  oplot,zenlambda, smooth(deriv(zenlambda,sp[0,4,*,0]/max(sp[0,4,*,0])),2), color=120
  oplot,zenlambda, smooth(deriv(zenlambda,sp[3,1,*,0]/max(sp[3,1,*,0])),2), color=70
  oplot,zenlambda, smooth(deriv(zenlambda,sp[3,4,*,0]/max(sp[3,4,*,0])),2), color=250

  legend, ['ref=10 um, tau=25 ','ref=10 um, tau=150','ref=25 um, tau=25 ','ref=25 um, tau=150'],textcolors=[0,70,120,250],box=0,/right,charsize=2

  restore, '/argus/SSFR3/model/sp_ice_liq_mix_new.out'
  plot, zenlambda, smooth(deriv(zenlambda,sp[0,*]/max(sp[0,*])),2),title='Ice/liquid water cloud mixtures spectra',xtitle='Wavelength (nm)',$
   ytitle='Derivative',yrange=[-0.01,0.01],xrange=[900,1700]
  for i=1, 10 do oplot, zenlambda, smooth(deriv(zenlambda,sp[i,*]/max(sp[i,*])),2),color=i*25

  legend,['ice portion',strtrim(fix(ice),2)+'%'],textcolors=[0,ice*2.5],box=0,/right,charsize=2


  restore, '/argus/SSFR3/model/sp_liq_ice_new3.out'
  plot, zenlambda, smooth(deriv(zenlambda,sp[0,1,*,1]/max(sp[0,1,*,1])),2), xtitle='Wavelength (nm)',$
   ytitle='Derivative',title='Modeled ice cloud spectra',xrange=[900,1700], yrange=[-0.01,0.01]
  oplot,zenlambda, smooth(deriv(zenlambda,sp[0,4,*,1]/max(sp[0,4,*,1])),2), color=120
  oplot,zenlambda, smooth(deriv(zenlambda,sp[3,1,*,1]/max(sp[3,1,*,1])),2), color=70
  oplot,zenlambda, smooth(deriv(zenlambda,sp[3,4,*,1]/max(sp[3,4,*,1])),2), color=250

  legend, ['ref=10 um, tau=25 ','ref=10 um, tau=150','ref=25 um, tau=25 ','ref=25 um, tau=150'],textcolors=[0,70,120,250],box=0,/right,charsize=2

  device, /close
  spawn, 'convert '+fp+'.ps '+fp+'.png'

  restore, '/argus/SSFR3/model/sp_liq_ice_new3.out'

fp=dir+'spc_dsp_mod_cat2_fl'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=60, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=3.2 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,3,1] & !x.margin=[8,4]


  df=smooth(deriv(zenlambda,sp[0,1,*,0]/max(sp[0,1,*,0])),2)
  plot, zenlambda[fl], df[fl],xtitle='Wavelength (nm)',$
   ytitle='Derivative',title='Modeled liquid cloud spectra',xrange=[900,1700], yrange=[-0.01,0.01],psym=2
  df=smooth(deriv(zenlambda,sp[0,4,*,0]/max(sp[0,4,*,0])),2)
  oplot,zenlambda[fl], df[fl], color=120,psym=2
  df=smooth(deriv(zenlambda,sp[3,1,*,0]/max(sp[3,1,*,0])),2)
  oplot,zenlambda[fl], df[fl], color=70, psym=2
  df=smooth(deriv(zenlambda,sp[3,4,*,0]/max(sp[3,4,*,0])),2)
  oplot,zenlambda[fl], df[fl], color=250,psym=2

  legend, ['ref=10 um, tau=25 ','ref=10 um, tau=150','ref=25 um, tau=25 ','ref=25 um, tau=150'],textcolors=[0,70,120,250],box=0,/right,charsize=2

  restore, '/argus/SSFR3/model/sp_ice_liq_mix_new.out'
  df=smooth(deriv(zenlambda,sp[0,*]/max(sp[0,*])),2)
  plot, zenlambda[fl], df[fl],title='Ice/liquid water cloud mixtures spectra',xtitle='Wavelength (nm)',$
   ytitle='Normalized radiance',psym=2,yrange=[-0.01,0.01],xrange=[900,1700]
  for i=1, 10 do begin
    df=smooth(deriv(zenlambda,sp[i,*]/max(sp[i,*])),2)
    oplot, zenlambda[fl], df[fl],color=i*25,psym=2
  endfor
  legend,['ice portion',strtrim(fix(ice),2)+'%'],textcolors=[0,ice*2.5],box=0,/right,charsize=2


  restore, '/argus/SSFR3/model/sp_liq_ice_new3.out'
  df=smooth(deriv(zenlambda,sp[0,1,*,1]/max(sp[0,1,*,1])),2)
  plot, zenlambda[fl], df[fl], xtitle='Wavelength (nm)',$
   ytitle='Normalized radiance',title='Modeled ice cloud spectra',xrange=[900,1700], yrange=[-0.01,0.01],psym=2
  df=smooth(deriv(zenlambda,sp[0,4,*,1]/max(sp[0,4,*,1])),2)
  oplot,zenlambda[fl], df[fl], color=120,psym=2
  df=smooth(deriv(zenlambda,sp[3,1,*,1]/max(sp[3,1,*,1])),2)
  oplot,zenlambda[fl], df[fl], color=70, psym=2
  df=smooth(deriv(zenlambda,sp[3,4,*,1]/max(sp[3,4,*,1])),2)
  oplot,zenlambda[fl], df[fl], color=250,psym=2

  legend, ['ref=10 um, tau=25 ','ref=10 um, tau=150','ref=25 um, tau=25 ','ref=25 um, tau=150'],textcolors=[0,70,120,250],box=0,/right,charsize=2

  device, /close
  spawn, 'convert '+fp+'.ps '+fp+'.png'


fp=dir+'spc_dsp_mod_cat2_fl2'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=60, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=3.2 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,3,1] & !x.margin=[8,4]


  df=smooth(deriv(zenlambda,sp[0,1,*,0]/max(sp[0,1,*,0])),2)
  plot, zenlambda[fl2], df[fl2],xtitle='Wavelength (nm)',$
   ytitle='Derivative',title='Modeled liquid cloud spectra',xrange=[900,1700], yrange=[-0.01,0.01],psym=2
  df=smooth(deriv(zenlambda,sp[0,4,*,0]/max(sp[0,4,*,0])),2)
  oplot,zenlambda[fl2], df[fl2], color=120,psym=2
  df=smooth(deriv(zenlambda,sp[3,1,*,0]/max(sp[3,1,*,0])),2)
  oplot,zenlambda[fl2], df[fl2], color=70, psym=2
  df=smooth(deriv(zenlambda,sp[3,4,*,0]/max(sp[3,4,*,0])),2)
  oplot,zenlambda[fl2], df[fl2], color=250,psym=2

  legend, ['ref=10 um, tau=25 ','ref=10 um, tau=150','ref=25 um, tau=25 ','ref=25 um, tau=150'],textcolors=[0,70,120,250],box=0,/right,charsize=2

  restore, '/argus/SSFR3/model/sp_ice_liq_mix_new.out'
  df=smooth(deriv(zenlambda,sp[0,*]/max(sp[0,*])),2)
  plot, zenlambda[fl2], df[fl2],title='Ice/liquid water cloud mixtures spectra',xtitle='Wavelength (nm)',$
   ytitle='Normalized radiance',psym=2,yrange=[-0.01,0.01],xrange=[900,1700]
  for i=1, 10 do begin
    df=smooth(deriv(zenlambda,sp[i,*]/max(sp[i,*])),2)
    oplot, zenlambda[fl2], df[fl2],color=i*25,psym=2
  endfor
  legend,['ice portion',strtrim(fix(ice),2)+'%'],textcolors=[0,ice*2.5],box=0,/right,charsize=2


  restore, '/argus/SSFR3/model/sp_liq_ice_new3.out'
  df=smooth(deriv(zenlambda,sp[0,1,*,1]/max(sp[0,1,*,1])),2)
  plot, zenlambda[fl2], df[fl2], xtitle='Wavelength (nm)',$
   ytitle='Normalized radiance',title='Modeled ice cloud spectra',xrange=[900,1700], yrange=[-0.01,0.01],psym=2
  df=smooth(deriv(zenlambda,sp[0,4,*,1]/max(sp[0,4,*,1])),2)
  oplot,zenlambda[fl2], df[fl2], color=120,psym=2
  df=smooth(deriv(zenlambda,sp[3,1,*,1]/max(sp[3,1,*,1])),2)
  oplot,zenlambda[fl2], df[fl2], color=70, psym=2
  df=smooth(deriv(zenlambda,sp[3,4,*,1]/max(sp[3,4,*,1])),2)
  oplot,zenlambda[fl2], df[fl2], color=250,psym=2

  legend, ['ref=10 um, tau=25 ','ref=10 um, tau=150','ref=25 um, tau=25 ','ref=25 um, tau=150'],textcolors=[0,70,120,250],box=0,/right,charsize=2

  device, /close
  spawn, 'convert '+fp+'.ps '+fp+'.png'

stop

end
