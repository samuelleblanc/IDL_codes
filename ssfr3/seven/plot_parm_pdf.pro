; program to plot the pdf of a sample measurement pdf
; loads from sample run of retrieve_pdfs - 20120523

pro plot_parm_pdf

dir='/home/leblanc/SSFR3/'
restore, dir+'data/meas_pdf_ex.out'

  fp=dir+'plots/parm_pdf_11'
  print, 'making plot :'+fp
  set_plot, 'ps'
  loadct, 39, /silent
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
  device, xsize=20, ysize=20
   !p.font=1 & !p.thick=5 & !p.charsize=2.4 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
   !p.multi=0 & !x.margin=[5,3] & !y.margin=[4,1] & !p.symsize=1.5 ; charsize was 4.2

  plot, bins[10,*],meas_pdf[10,*],xrange=[-0.00165,-0.0015], ytitle='Normalized pdf',xtitle='Parameter 11!CSlope of normalized radiance at 550-680 nm (nm!U-1!N)'

  device, /close
  spawn, 'convert "'+fp+'.ps" "'+fp+'.png"'

stop
end
