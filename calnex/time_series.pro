pro time_series

restore, '/data/seven/schmidt/calnex/p3/20100516/20100516_SP.out'


set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated
 device, /tt_font, set_font='Helvetica Bold'
 device, filename='/home/leblanc/CALNEX/clouds/time_series.ps'
 device,/color,bits_per_pixel=8.
 device, xsize=40, ysize=30
      !p.font=1
      !p.thick=3
      !p.charsize=2.5
      !x.style=0
      !y.style=1 
      !z.style=1
      !y.thick=1.8
      !x.thick=1.8
      !p.multi=0;[0,1,2]
      ;!y.omargin=[1,3]
      !x.charsize=1.5
      !y.charsize=1.5

tvlct, 220,220,220,200

  mm=min(abs(500-zenlambda),wvlz)
  fl=where(utc gt 18.2 and utc lt 21.5)
  plot, utc[fl], zspectra[wvlz,fl], title='Downwelling Irradiance at 500nm', xtitle='UTC (H)', ytitle='Irradiance (W/m!E2!N)', yrange=[0,2.5], ystyle=8, xmargin=[8,10]
  
  turns=where(abs(rol[fl]) gt 5.) 
  for i=0, n_elements(turns)-1 do begin
    oplot, [utc[fl[turns[i]]], utc[fl[turns[i]]]], [0,2.5], color=200
  endfor
  oplot, utc[fl], zspectra[wvlz,fl]
  oplot, [19.45,19.45], [0,2.5], color=250

restore, '/data/seven/schmidt/calnex/p3/20100516/20100516_calibspcs_attcorr.out'

oplot, utc[fl], zspectra[wvlz,fl],color=150

  axis,yaxis=1,ytitle='Altitude (m)', color=70, yrange=[0,3000], /save
  oplot, utc[fl], alt[fl], color=70

  legend,['Change of heading', 'Point of interest', 'Altitude', 'Attitude corrected'],textcolors=[200,250,70,150],/right

 ; plot, utc[fl], alt[fl], title='Flight altitude', xtitle='UTC (H)', ytitle='Altitude (m)'

 device, /close
spawn, 'convert /home/leblanc/CALNEX/clouds/time_series.ps /home/leblanc/CALNEX/clouds/time_series.png'
spawn, 'rm -f /home/leblanc/CALNEX/clouds/time_series.ps'

end