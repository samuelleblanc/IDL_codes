; program to plot the measurement pdf for two different combination of tau ref and phase


pro plot_meas_pdf
dir='~/SSFR3/plots/'

print, 'restoring liquid water case, tau=100, ref=15 um'
restore, '~/SSFR3/data/meas_pdf_liq.out'
meas_pdf_liq=meas_pdf

print, 'restoring ice water case, tau=3, ref=40 um'
restore, '~/SSFR3/data/meas_pdf_ice.out'
meas_pdf_ice=meas_pdf

fp=dir+'sample_meas_pdf'
  print, 'making plot :'+fp
  set_plot, 'ps'
  loadct, 39, /silent
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
  device, xsize=20, ysize=15
   !p.font=1 & !p.thick=5 & !p.charsize=2.1 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
   !p.multi=0 & !x.margin=[7,2] & !y.margin=[5,1] & !p.symsize=1.5

  p=5
  plot, bins[p,*],meas_pdf_ice[p,*],xtitle='Parameter 6!CNormalized radiance near 1600 nm',ytitle='Probability density',/nodata,xrange=[0,0.12]
  oplot,bins[p,*],meas_pdf_liq[p,*],color=250
  oplot,bins[p,*],meas_pdf_ice[p,*],color=70
  legend,['Liquid, !9t!X=100, r!De!N=15 !9m!Xm','Ice, !9t!X=3, r!De!N=40 !9m!Xm'],textcolors=[250,70],box=0
 device, /close
 spawn,'convert '+fp+'.ps '+fp+'.png'

stop
end
