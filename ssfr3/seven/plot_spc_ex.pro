; program to plot examples of spectra gathered under liquid water clouds and ice water clouds
; restores the yyyymmdd_sp_ex.out files produced by spc_compare.pro
; simple quick program

@legend.pro
@zensun.pro
pro plot_spc_ex
dir='/home/leblanc/SSFR3/data/'

fp=dir+'spc_ex'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=20, ysize=40
  !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,1,2] & !x.margin=[8,4]

  restore, dir+'20120602_sp_ex.out'
  dspi=dsp
  spi=sp
  utci=utc
  plot, wl, sp/max(sp), title='Measured radiance of clouds',xtitle='Wavelength (nm)',ytitle='Normalized radiance'
  
  restore, dir+'20120806_sp_ex.out'
  oplot, wl, sp/max(sp), color=70
  spa=sp & dspa=dsp & utca=utc

  restore, dir+'20120912_sp_ex.out'
  oplot, wl, sp/max(sp), color=100
  spb=sp & dspb=dsp & utcb=utc

  restore, dir+'20120523_sp_ex.out'
  oplot, wl, sp/max(sp), color=180
  spc=sp & dspc=dsp & utcc=utc

  restore, dir+'20120816_sp_ex.out'
  oplot, wl, sp/max(sp), color=210
  spd=sp & dspd=dsp & utcd=utc
  
  restore, dir+'20120813_sp_ex.out'
  oplot, wl, sp/max(sp), color=250
  spe=sp & dspe=dsp & utce=utc

  restore, dir+'20130110_sp_ex.out'
  oplot, wl, sp/max(sp), color=230
  spf=sp & dspf=dsp & utcf=utc

  restore, dir+'20130111_sp_ex.out'
  oplot, wl, sp/max(sp), color=150
  spg=sp & dspg=dsp & utcg=utc

  restore, dir+'2013011002_sp_ex.out'
  oplot, wl, sp/max(sp), color=10
  sph=sp & dsph=dsp & utch=utc

  restore, dir+'2013011102_sp_ex.out'
  oplot, wl, sp/max(sp), color=50
  spi=sp & dspi=dsp & utci=utc

  restore, dir+'2013011003_sp_ex.out'
  oplot, wl, sp/max(sp), color=10,linestyle=2
  spj=sp & dspj=dsp & utcj=utc

  restore, dir+'2013011103_sp_ex.out'
  oplot, wl, sp/max(sp), color=50,linestyle=2
  spk=sp & dspk=dsp & utck=utc

  restore, dir+'2012081302_sp_ex.out'
  oplot, wl, sp/max(sp), color=85,linestyle=2
  spl=sp & dspl=dsp & utcl=utc

  restore, dir+'2012091202_sp_ex.out'
  oplot, wl, sp/max(sp), color=130,linestyle=2
  spm=sp & dspm=dsp & utcm=utc

  restore, dir+'2012080605_sp_ex.out'
  oplot, wl, sp/max(sp), color=195,linestyle=2
  spn=sp & dspn=dsp & utcn=utc


 ;legend,['Ice','Liquid'],textcolors=[0,70],box=0,/right
  legend, ['20120602','20120806','20120912','20120523','20120816','20120813','20130110','20130111','2/10_2','2/11_2','2/10_3','2/11_3','8/13_2','9/12_2','8/06_5'],$
   textcolors=[0,70,100,180,210,250,230,150,10,50,10,50,85,130,195],/right,box=0

  plot, wl, smooth(dspi,2), title='Derivative of radiance of clouds',xtitle='Wavelength (nm)', ytitle='Derivative of radiance'
  oplot,wl, smooth(dspa,2), color=70
  oplot,wl, smooth(dspb,2), color=100  
  oplot,wl, smooth(dspc,2), color=180
  oplot,wl, smooth(dspd,2), color=210
  oplot,wl, smooth(dspe,2), color=250
  oplot,wl, smooth(dspf,2), color=230
  oplot,wl, smooth(dspg,2), color=150
  oplot,wl, smooth(dsph,2), color=10
  oplot,wl, smooth(dspi,2), color=50
  oplot,wl, smooth(dspj,2), color=10,linestyle=2
  oplot,wl, smooth(dspk,2), color=50,linestyle=2
  oplot,wl, smooth(dspl,2), color=85,linestyle=2
  oplot,wl, smooth(dspm,2), color=130,linestyle=2
  oplot,wl, smooth(dspn,2), color=195,linestyle=2

;  legend,['Ice','Liquid'],textcolors=[0,70],box=0
  legend, ['20120602','20120806','20120912','20120523','20120816','20120813','20130110','20130111','2/10_2','2/11_2','2/10_3','2/11_3','8/13_2','9/12_2','8/06_5'],$
   textcolors=[0,70,100,180,210,250,230,150,10,50,10,50,85,130,195],/right,box=0

  restore, dir+'../cloudy.out'
  u=where(totday eq 20120602 and tottmhrs eq utci)
  v=where(totday eq 20120806 and tottmhrs eq utca)
  vb=where(totday eq 20120912 and tottmhrs eq utcb)
  vc=where(totday eq 20120523 and tottmhrs eq utcc)
  vd=where(totday eq 20120816 and tottmhrs eq utcd)  
  ve=where(totday eq 20120813 and tottmhrs eq utce)
  vf=where(totday eq 20120813 and tottmhrs eq utcl)
  vg=where(totday eq 20120912 and tottmhrs eq utcm)
  vh=where(totday eq 20120806 and tottmhrs eq utcn)
  lti='tau='+string(tottau[[u,v,vb,vc,vd,ve,vf,vg,vh]])
  lr='ref='+string(totref[[u,v,vb,vc,vd,ve,vf,vg,vh]])
  legend, lti, textcolors=[0,70,100,180,210,250,85,130,195],box=0
  legend, lr, textcolors=[0,70,100,180,210,250,85,130,195],box=0,/right,/bottom

  ;legend,['tau='+strtrim(tottau[u],2),'ref='+strtrim(totref[u],2)+'um','tau='+strtrim(tottau[v],2),'ref='+strtrim(totref[v],2)+'um'],textcolors=[0,0,70,70],box=0, /right

 device, /close
 spawn, 'convert '+fp+'.ps '+fp+'.png'

fp=dir+'spc_exi_zoom'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=20, ysize=40
  !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,1,2] & !x.margin=[8,4]
  plot, wl, spi/max(spi), title='Measured radiance of clouds',xtitle='Wavelength (nm)',ytitle='Normalized radiance',xrange=[900,1650],yrange=[0,0.5]
  oplot,wl, spa/max(spa), color=70
  oplot,wl, spb/max(spb), color=100
  oplot,wl, spc/max(spc), color=180
  oplot,wl, spd/max(spd), color=210
  oplot,wl, spe/max(spe), color=250
  oplot,wl, spf/max(spf), color=230
  oplot,wl, spg/max(spg), color=150
  oplot,wl, sph/max(sph), color=10
  oplot,wl, spi/max(spi), color=50
  oplot,wl, spj/max(spj), color=10,linestyle=2
  oplot,wl, spk/max(spk), color=50,linestyle=2
  oplot,wl, spl/max(spl), color=85,linestyle=2
  oplot,wl, spm/max(spm), color=130,linestyle=2
  oplot,wl, spn/max(spn), color=195,linestyle=2
 
  legend, ['20120602','20120806','20120912','20120523','20120816','20120813','20130110','20130111','2/10_2','2/11_2','2/10_3','2/11_3','8/13_2','9/12_2','8/06_5'],$
   textcolors=[0,70,100,180,210,250,230,150,10,50,10,50,85,130,195],/right,box=0
;  legend,['Ice','Liquid'],textcolors=[0,70],box=0,/right

  plot, wl, smooth(dspi,2), title='Derivative of radiance of clouds',xtitle='Wavelength (nm)', ytitle='Derivative of radiance',xrange=[900,1650],yrange=[-0.01,0.01]
  oplot,wl, smooth(dspa,2), color=70
  oplot,wl, smooth(dspb,2), color=100
  oplot,wl, smooth(dspc,2), color=180
  oplot,wl, smooth(dspd,2), color=210
  oplot,wl, smooth(dspe,2), color=250 
  oplot,wl, smooth(dspf,2), color=230
  oplot,wl, smooth(dspg,2), color=150
  oplot,wl, smooth(dsph,2), color=10
  oplot,wl, smooth(dspi,2), color=50
  oplot,wl, smooth(dspj,2), color=10,linestyle=2
  oplot,wl, smooth(dspk,2), color=50,linestyle=2
  oplot,wl, smooth(dspl,2), color=85,linestyle=2
  oplot,wl, smooth(dspm,2), color=130,linestyle=2
  oplot,wl, smooth(dspn,2), color=195,linestyle=2
; legend,['Ice','Liquid'],textcolors=[0,70],box=0
  legend, ['20120602','20120806','20120912','20120523','20120816','20120813','20130110','20130111','2/10_2','2/11_2','2/10_3','2/11_3','8/13_2','9/12_2','8/06_5'],$
   textcolors=[0,70,100,180,210,250,230,150,10,50,10,50,85,130,195],/right,box=0 
device, /close
 spawn, 'convert '+fp+'.ps '+fp+'.png'


fp=dir+'spc_exi_zooms'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=60, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=2.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,3,1] & !x.margin=[8,4]

  plot, wl, smooth(dspi,2), title='Derivative of radiance of clouds',xtitle='Wavelength (nm)', ytitle='Derivative of radiance',xrange=[900,1125],yrange=[-0.015,0.015]
  oplot,wl, smooth(dspa,2), color=70
  oplot,wl, smooth(dspb,2), color=100
  oplot,wl, smooth(dspc,2), color=180
  oplot,wl, smooth(dspd,2), color=210
  oplot,wl, smooth(dspe,2), color=250
  oplot,wl, smooth(dspf,2), color=230
  oplot,wl, smooth(dspg,2), color=150
  oplot,wl, smooth(dsph,2), color=10
  oplot,wl, smooth(dspi,2), color=50
  oplot,wl, smooth(dspj,2), color=10,linestyle=2
  oplot,wl, smooth(dspk,2), color=50,linestyle=2
  oplot,wl, smooth(dspl,2), color=85,linestyle=2
  oplot,wl, smooth(dspm,2), color=130,linestyle=2
  oplot,wl, smooth(dspn,2), color=195,linestyle=2


; legend,['Ice','Liquid'],textcolors=[0,70],box=0
  legend, ['20120602','20120806','20120912','20120523','20120816','20120813','20130110','20130111','2/10_2','2/11_2','2/10_3','2/11_3','8/13_2','9/12_2','8/06_5'],$
   textcolors=[0,70,100,180,210,250,230,150,10,50,10,50,85,130,195],/right,box=0

  plot, wl, smooth(dspi,2), title='Derivative of radiance of clouds',xtitle='Wavelength (nm)', ytitle='Derivative of radiance',xrange=[1125,1400],yrange=[-0.01,0.01]
  oplot,wl, smooth(dspa,2), color=70
  oplot,wl, smooth(dspb,2), color=100
  oplot,wl, smooth(dspc,2), color=180
  oplot,wl, smooth(dspd,2), color=210
  oplot,wl, smooth(dspe,2), color=250
  oplot,wl, smooth(dspf,2), color=230
  oplot,wl, smooth(dspg,2), color=150
  oplot,wl, smooth(dsph,2), color=10
  oplot,wl, smooth(dspi,2), color=50
  oplot,wl, smooth(dspj,2), color=10,linestyle=2
  oplot,wl, smooth(dspk,2), color=50,linestyle=2
  oplot,wl, smooth(dspl,2), color=85,linestyle=2
  oplot,wl, smooth(dspm,2), color=130,linestyle=2
  oplot,wl, smooth(dspn,2), color=195,linestyle=2

; legend,['Ice','Liquid'],textcolors=[0,70],box=0
  legend, ['20120602','20120806','20120912','20120523','20120816','20120813','20130110','20130111','2/10_2','2/11_2','2/10_3','2/11_3','8/13_2','9/12_2','8/06_5'],$
   textcolors=[0,70,100,180,210,250,230,150,10,50,10,50,85,130,195],/right,box=0

  plot, wl, smooth(dspi,2), title='Derivative of radiance of clouds',xtitle='Wavelength (nm)', ytitle='Derivative of radiance',xrange=[1400,1650],yrange=[-0.002,0.002]
  oplot,wl, smooth(dspa,2), color=70
  oplot,wl, smooth(dspb,2), color=100
  oplot,wl, smooth(dspc,2), color=180
  oplot,wl, smooth(dspd,2), color=210
  oplot,wl, smooth(dspe,2), color=250
  oplot,wl, smooth(dspf,2), color=230
  oplot,wl, smooth(dspg,2), color=150 
  oplot,wl, smooth(dsph,2), color=10
  oplot,wl, smooth(dspi,2), color=50
  oplot,wl, smooth(dspj,2), color=10,linestyle=2
  oplot,wl, smooth(dspk,2), color=50,linestyle=2
  oplot,wl, smooth(dspl,2), color=85,linestyle=2
  oplot,wl, smooth(dspm,2), color=130,linestyle=2
  oplot,wl, smooth(dspn,2), color=195,linestyle=2
; legend,['Ice','Liquid'],textcolors=[0,70],box=0
  legend, ['20120602','20120806','20120912','20120523','20120816','20120813','20130110','20130111','2/10_2','2/11_2','2/10_3','2/11_3','8/13_2','9/12_2','8/06_5'],$
   textcolors=[0,70,100,180,210,250,230,150,10,50,10,50,85,130,195],/right,box=0
 device, /close
 spawn, 'convert '+fp+'.ps '+fp+'.png'


fp=dir+'spc_exi_sp_zooms'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=60, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=2.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,3,1] & !x.margin=[8,4]

  plot, wl, spi/max(spi), title='Measured cloud radiance',xtitle='Wavelength (nm)', ytitle='Normalized radiance',xrange=[900,1125],yrange=[0,0.5]
  oplot,wl, spa/max(spa), color=70
  oplot,wl, spb/max(spb), color=100
  oplot,wl, spc/max(spc), color=180
  oplot,wl, spd/max(spd), color=210
  oplot,wl, spe/max(spe), color=250
  oplot,wl, spf/max(spf), color=230
  oplot,wl, spg/max(spg), color=150
  oplot,wl, sph/max(sph), color=10
  oplot,wl, spi/max(spi), color=50
  oplot,wl, spj/max(spj), color=10,linestyle=2
  oplot,wl, spk/max(spk), color=50,linestyle=2
  oplot,wl, spl/max(spl), color=85,linestyle=2
  oplot,wl, spm/max(spm), color=130,linestyle=2
  oplot,wl, spn/max(spn), color=195,linestyle=2
  legend, ['20120602','20120806','20120912','20120523','20120816','20120813','20130110','20130111','2/10_2','2/11_2','2/10_3','2/11_3','8/13_2','9/12_2','8/06_5'],$
   textcolors=[0,70,100,180,210,250,230,150,10,50,10,50,85,130,195],/right,box=0

  plot, wl, spi/max(spi), title='Measured cloud radiance',xtitle='Wavelength (nm)', ytitle='Normalized radiance',xrange=[1125,1400],yrange=[0,0.3]
  oplot,wl, spa/max(spa), color=70
  oplot,wl, spb/max(spb), color=100
  oplot,wl, spc/max(spc), color=180
  oplot,wl, spd/max(spd), color=210
  oplot,wl, spe/max(spe), color=250
  oplot,wl, spf/max(spf), color=230
  oplot,wl, spg/max(spg), color=150
  oplot,wl, sph/max(sph), color=10
  oplot,wl, spi/max(spi), color=50
  oplot,wl, spj/max(spj), color=10,linestyle=2
  oplot,wl, spk/max(spk), color=50,linestyle=2
  oplot,wl, spl/max(spl), color=85,linestyle=2
  oplot,wl, spm/max(spm), color=130,linestyle=2
  oplot,wl, spn/max(spn), color=195,linestyle=2
  legend, ['20120602','20120806','20120912','20120523','20120816','20120813','20130110','20130111','2/10_2','2/11_2','2/10_3','2/11_3','8/13_2','9/12_2','8/06_5'],$
   textcolors=[0,70,100,180,210,250,230,150,10,50,10,50,85,130,195],/right,box=0  

  plot, wl, spi/max(spi), title='Measured cloud radiance',xtitle='Wavelength (nm)', ytitle='Normalized radiance',xrange=[1400,1650],yrange=[0,0.1]
  oplot,wl, spa/max(spa), color=70
  oplot,wl, spb/max(spb), color=100
  oplot,wl, spc/max(spc), color=180
  oplot,wl, spd/max(spd), color=210
  oplot,wl, spe/max(spe), color=250
  oplot,wl, spf/max(spf), color=230
  oplot,wl, spg/max(spg), color=150
  oplot,wl, sph/max(sph), color=10
  oplot,wl, spi/max(spi), color=50
  oplot,wl, spj/max(spj), color=10,linestyle=2
  oplot,wl, spk/max(spk), color=50,linestyle=2
  oplot,wl, spl/max(spl), color=85,linestyle=2
  oplot,wl, spm/max(spm), color=130,linestyle=2
  oplot,wl, spn/max(spn), color=195,linestyle=2
  legend, ['20120602','20120806','20120912','20120523','20120816','20120813','20130110','20130111','2/10_2','2/11_2','2/10_3','2/11_3','8/13_2','9/12_2','8/06_5'],$
   textcolors=[0,70,100,180,210,250,230,150,10,50,10,50,85,130,195],/right,box=0



 device, /close
 spawn, 'convert '+fp+'.ps '+fp+'.png'



lat=40.007916667
lon=-105.26825
zensun, totday,tottmhrs,lat, lon, szas, azi, solfac
print, tottmhrs[[u,v]]
print, szas[[u,v]]
print, totwater[[u,v]]
print, totpres[[u,v]]


stop
end
