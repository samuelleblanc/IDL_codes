; program to make a plot showing a case of homogeneous clouds and heterogeneous clouds. 
; 

@read_skywatch.pro
@cgimage.pro
pro plot_homogeneous
dir='/home/leblanc/SSFR3/skywatch/'


  ;get data
  pa=read_skywatch(instrument='pyra',name=dir+'pyra_12_05_23.dat')
  pg=read_skywatch(instrument='pyrg',name=dir+'pyrg_12_05_23.dat')
  cl=read_skywatch(instrument='ceil',name=dir+'ceil_12_05_23.dat')

  ; get image
  img_homo=read_image(dir+'homo.jpg')
  img_hete=read_image(dir+'hete.jpg')

  ; set the ranges of homogeneous cloudy scenes and heterogeneous
  tmhrs=pa.sec_of_day/60./60.
  tmhrsc=cl.sec_of_day/60./60.
  
  homo=[21.,24.]
  hete=[16.,19.]
  fhomo=where(tmhrs gt homo[0] and tmhrs lt homo[1])
  fhete=where(tmhrs gt hete[0] and tmhrs lt hete[1])
  fcho=where(tmhrsc gt homo[0] and tmhrsc lt homo[1] and cl.cloud_base1 gt 1000.)
  fche=where(tmhrsc gt hete[0] and tmhrsc lt hete[1])

  fp=dir+'homogeneous'
  print, 'making plot :'+fp
  set_plot, 'ps'
  loadct, 39, /silent
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
  device, xsize=40, ysize=32
   !p.font=1 & !p.thick=5 & !p.charsize=4.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
   !p.multi=[0,2,4] & !x.margin=[8,-1] & !y.margin=[0,0.1] & !p.symsize=1.5

  empty=[' ',' ',' ',' ',' ',' ',' ',' ']


  plot, findgen(10),/nodata,yticks=0,xticks=0,title='Heterogeneous',ymargin=[-2.5,2.5],xtickname=empty,ytickname=empty
  cgimage,img_hete, /overplot ;title='Heterogeneous',position=[0.21,0.02,0.98,0.8]
  xyouts, 0.5,0.5,'2012/05/23 18.0 UTC',/data,color=255,charsize=1.8
  plot, findgen(10),/nodata,yticks=0,xticks=0,title='Homogeneous',ymargin=[-2.5,2.5],xmargin=[2,5],xtickname=empty,ytickname=empty
  cgimage,img_homo, /overplot ;title='Homogeneous',position=[0.05,0.02,0.9,0.8]
  xyouts, 0.5,0.5,'2012/05/23 23.0 UTC',/data,color=255,charsize=1.8
  plot, tmhrs[fhete],pa.irradiance[fhete],ytitle='Pyranometer!CIrradiance (Wm!U-2!N)',yrange=[0,1300],xtickname=empty,ymargin=[-0.5,2.7]
  plot, tmhrs[fhomo],pa.irradiance[fhomo],yrange=[0,1300],xtickname=empty,ytickname=empty,ymargin=[-0.5,2.7],xmargin=[2,5]
  plot, tmhrs[fhete],pg.irradiance[fhete],yrange=[300,400],ytitle='Pyrgeometer!CIrradiance (Wm!U-2!N)',xtickname=empty,ymargin=[1.5,0.7]
  plot, tmhrs[fhomo],pg.irradiance[fhomo],yrange=[300,400],xtickname=empty,ytickname=empty,xmargin=[2,5],ymargin=[1.5,0.7]
  plot, tmhrsc[fche],cl.cloud_base1[fche]/1000.,yrange=[0,7.6],ytitle='Cloud base height (km)', xtitle='UTC (H)',ymargin=[3,-1.3],psym=2;,xtickname=empty
  plot, tmhrsc[fcho],cl.cloud_base1[fcho]/1000.,yrange=[0,7.6],ytickname=empty, xtitle='UTC (H)',ymargin=[3,-1.3],xmargin=[2,5],psym=2;,xtickname=empty


; xyouts, '2012/05/23 18.0 UTC',5,5,/data,color=250
; xyouts, '2012/05/23 23.0 UTC',5,5,/data,color=250 

device, /close
spawn, 'convert '+fp+'.ps '+fp+'.png'

stop
end
