;program to plot cloud optical depth comparison for the high park fire 
; during times of northerly and southerly winds (from a single day)
; at the peak of high park fire
; for 20120616

pro plot_high_park

dir='/home/leblanc/SSFR3/data/'
l='/'
date='20120620'

print, 'restoring :'+dir+date+l+date+'_cod_comp.out'
restore, dir+date+l+date+'_cod_comp.out'

print, 'restoring the relationships'
restore, dir+'relations.out'


;make a filter for northerly winds and southerly winds

;a=where(tmhrs gt 11.0 and tmhrs lt 15.0) ;from north
;b=where(tmhrs gt 18.0 and tmhrs lt 21.0) ;from south

a=where( tmhrs gt 12. and tmhrs lt 14.)
b=where(tmhrs gt 15. and tmhrs lt 18.)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; cod from oxa and water ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

fn=dir+date+'_cod_from_path_fire'
print, 'plotting:'+fn
 set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold'
 device, filename=fn+'.ps'
 device,/color,bits_per_pixel=8., xsize=20, ysize=20
 !p.font=1 & !p.thick=7 & !p.charsize=2.3 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
 !p.multi=0 & !x.margin=[6,3]

  tvlct,250,128,000,247
  tvlct,255,191,108,248
  tvlct,115,230,000,101
  alpha="141B  ;"

  r = FINDGEN(17) * (!PI*2/16.)
  USERSYM, COS(r), SIN(r), /FILL

  plot, tau[b], p_to_t(oxa[b],px),psym=2, title='Optical depth from different methods',ytitle='Predicted optical depth',$
   xrange=[0,200],yrange=[0,200],xtitle='Cloud optical depth',/nodata
  oplot, tau[b],p_to_t(oxa[b],px),psym=8,color=101,symsize=0.2
  oplot, tau[b],p_to_t(area[b],p),psym=8,color=248,symsize=0.2
  oplot, tau[b],p_to_t(area2[b],p2),psym=8,color=247,symsize=0.2

  oplot, tau[a],p_to_t(oxa[a],px),psym=8,color=101,symsize=0.4
  oplot, tau[a],p_to_t(area[a],p),psym=8,color=248,symsize=0.4
  oplot, tau[a],p_to_t(area2[a],p2),psym=8,color=247,symsize=0.4
  oplot, findgen(150),linestyle=2

  legend, ['Oxygen-!9'+string(alpha)+'!X','940 nm band','1150 nm band'],textcolors=[101,248,247],box=0,/right,/bottom
  legend, ['Downwind of fire','Upwind of fire'],psym=[8,8],symsize=[0.4,0.2],box=0,pspacing=1
  legend, ['1:1    '],box=0,/right
 device, /close
 spawn, 'convert '+fn+'.ps '+fn+'.png'

;;;;;;; plot the histogram of the difference in tau ;;;;;;;;;

fn=dir+date+'_cod_histogram_fire'
print, 'plotting:'+fn
 set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold'
 device, filename=fn+'.ps'
 device,/color,bits_per_pixel=8., xsize=20, ysize=20
 !p.font=1 & !p.thick=10 & !p.charsize=2.3 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
 !p.multi=0 & !x.margin=[6,3]

  tvlct,250,128,000,247
  tvlct,255,191,108,248
  tvlct,115,230,000,101
  alpha="141B  ;"

  dtx=tau[a]-p_to_t(oxa[a],px)
  dtw=tau[a]-p_to_t(area[a],p)
  dtw2=tau[a]-p_to_t(area2[a],p2)

  bins=findgen(50)*8.-200.+4.
  hx=histogram(dtx,nbins=50,min=-300.,max=200.)
  hx=float(hx)/float(max(hx))
  hw=histogram(dtw,nbins=50,min=-300.,max=200.)
  hw=float(hw)/float(max(hw))
  hw2=histogram(dtw2,nbins=50,min=-300.,max=200.)
  hw2=float(hw2)/float(max(hw2))

  plot, bins, hx,psym=10, title='Differences in optical depth histogram',ytitle='Normalized frequency',$
   xrange=[-300,200],yrange=[0,1],xtitle='Cloud optical depth differences',/nodata
  oplot, bins,hx,psym=10,color=101
  oplot, bins,hw,psym=10,color=248
  oplot, bins,hw2,psym=10,color=247

    dtxb=tau[b]-p_to_t(oxa[b],px)
  dtwb=tau[b]-p_to_t(area[b],p)
  dtw2b=tau[b]-p_to_t(area2[b],p2)

  binsb=findgen(50)*10.-300.+5.
  hxb=histogram(dtxb,nbins=50,min=-300.,max=200.)
  hxb=float(hxb)/float(max(hxb))
  hwb=histogram(dtwb,nbins=50,min=-300.,max=200.)
  hwb=float(hwb)/float(max(hwb))
  hw2b=histogram(dtw2b,nbins=50,min=-300.,max=200.)
  hw2b=float(hw2b)/float(max(hw2b))
  oplot, binsb,hxb,psym=10,color=101,thick=15
  oplot, binsb,hwb,psym=10,color=248,thick=15
  oplot, binsb,hw2b,psym=10,color=247,thick=15

  legend, ['Downwind of fire','Upwind of fire'],linestyle=[0,0],thick=[10,15],box=0,pspacing=1

  legend, ['Oxygen-!9'+string(alpha)+'!X','940 nm band','1150 nm band'],textcolors=[101,248,247],box=0,/right
 device, /close
 spawn, 'convert '+fn+'.ps '+fn+'.png'

;;;;;;; plot the timetrace of each tau ;;;;;;;;;;;;;;;;;;;

fn=dir+date+'_cod_tmhrs_fire'
print, 'plotting:'+fn
 set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold'
 device, filename=fn+'.ps'
 device,/color,bits_per_pixel=8., xsize=20, ysize=20
 !p.font=1 & !p.thick=10 & !p.charsize=2.3 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
 !p.multi=0 & !x.margin=[6,3]

  tvlct,250,128,000,247
  tvlct,255,191,108,248
  tvlct,115,230,000,101
  alpha="141B  ;"

  plot, tmhrs, tau,psym=3, title='Optical depth over time',ytitle='Cloud optical depth',$
   xrange=[10.5,21.5],xtitle='UTC (Hours)',/nodata,yrange=[0,150]
  tvlct,77,210,255,71
  tvlct,255,77,121,251
  polyfill,[11.0,15.0,15.0,11.0],[0,0,150,150],color=251
  polyfill,[18.0,21.0,21.0,18.0],[0,0,150,150],color=71
  
  oplot, tmhrs,tau,psym=3
  oplot, tmhrs,p_to_t(oxa,px),psym=3,color=101
  oplot, tmhrs,p_to_t(area,p),psym=3,color=248
  oplot, tmhrs,p_to_t(area2,p2),psym=3,color=247

  legend, ['Traditional method','Oxygen-!9'+string(alpha)+'!X','940 nm band','1150 nm band'],textcolors=[0,101,248,247],box=0,/right
  legend, ['Downwind of the fire','Upwind of the fire'],textcolors=[251,71],box=0
 device, /close
 spawn, 'convert '+fn+'.ps '+fn+'.png'


stop
end

;;;; function to transform integrated areas(x) to optical depth with the arguments (a)
function p_to_t, x, a

return, a[1]+(a[2]*alog(x+a[0]))
end


