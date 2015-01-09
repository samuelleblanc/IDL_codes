; program to plot the absorbed area in the gas absorption bands (water vapor and oxygen-a)
; plots the comparison of plane-parrallel modeled area to the measured area

@legend.pro
pro plot_area

date='20120806'
dir='/home/leblanc/SSFR3/data/'
restore, dir+'cloudy.out'

n=where(totday eq double(date))

restore,dir+'area_comp_'+date+'.out'

case date of 
  '20120602': begin
    r1=[30,55] & m1=[30,55]
    r2=[25,80] & m2=[35,100]
    r3=[2.5,6] & m3=[4,6]
    rx=[9,14] & mx=[3,5.5]
  end
  '20120813': begin
    r1=[34,46] & m1=[35,52]
    r2=[40,65] & m2=[30,80]
    r3=[3,5.5] & m3=[4.5,6]
    rx=[3,4.5] & mx=[11,13]
  end
  '20120525': begin
    r1=[25,45] & m1=[35,45]
    r2=[35,55] & m2=[20,40]
    r3=[2,5] & m3=[4.5,5.5]
    rx=[3,4] & mx=[11,13.5]
  end
  '20120523': begin
    r1=[40,100] & m1=[30,55]
    r2=[50,170] & m2=[10,80]
    r3=[4,12] & m3=[3,7]
    rx=[3.5,6] & mx=[9,15]
  end
  '20120912': begin
    r1=[55,90] & m1=[30,55]
    r2=[70,140] & m2=[10,80]
    r3=[7,14] & m3=[3.5,6]
    rx=[3.5,6] & mx=[9,15]
  end
  '20120806': begin
    r1=[30,80] & m1=[30,55]
    r2=[35,105] & m2=[10,80]
    r3=[0,10] & m3=[3.5,6]
    rx=[2,6] & mx=[9,15]
  end
  '20120816': begin
    r1=[30,60] & m1=[30,55]
    r2=[35,75] & m2=[20,60]
    r3=[3,8] & m3=[4.5,6]
    rx=[3,6] & mx=[9,15]
  end
  '20120820': begin
    r1=[20,50] & m1=[30,55]
    r2=[30,70] & m2=[15,70]
    r3=[2,6] & m3=[4,6]
    rx=[3,4.5] & mx=[9,15]
  end
  '20120824': begin
    r1=[20,70] & m1=[30,55]
    r2=[20,120] & m2=[10,80]
    r3=[2,9] & m3=[4,6]
    rx=[3,5] & mx=[9,15]
  end


endcase

fp=dir+'area_comp_'+date
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=40, ysize=40
  !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,2,2] & !x.margin=[6,4]

 u1=where(totarea[n] gt 0 and totarea[n] lt 100)
 ta1=totarea[n[u1]]
 ar1=area[u1]

 plot, ta1,ar1, title='Modeled-Measured comparison @ 940 nm WV band',$
  xtitle='Measured path integrated attenuation',ytitle='Modeled path integrated attenuation', psym=2, yrange=m1,xrange=r1
 u1=linfit(ta1,ar1)
 x1=correlate(ta1,ar1)
 oplot, r1,r1*u1[1]+u1[0],linestyle=2, color=250
 legend, ['y='+strtrim(u1[1],2)+'x+'+strtrim(u1[0],2),'R='+strtrim(x1,2)],textcolors=[250,250],box=0

 u2=where(totarea2[n] gt 0 and totarea2[n] lt 170)
 ta2=totarea2[n[u2]]
 ar2=area2[u2]

 plot, ta2,ar2, title='Modeled-Measured comparison @ 1150 nm WV band',$
  xtitle='Measured path integrated attenuation',ytitle='Modeled path integrated attenuation', psym=2, yrange=m2,xrange=r2
 u2=linfit(ta2,ar2)
 x2=correlate(ta2,ar2)
 oplot, r2,r2*u2[1]+u2[0],linestyle=2, color=250
 legend, ['y='+strtrim(u2[1],2)+'x+'+strtrim(u2[0],2),'R='+strtrim(x2,2)],textcolors=[250,250],box=0

 u3=where(totarea3[n] gt 0 and totarea3[n] lt 20)
 ta3=totarea3[n[u3]]
 ar3=area3[u3]

 plot, ta3,ar3, title='Modeled-Measured comparison @ 810 nm WV band',$
  xtitle='Measured path integrated attenuation',ytitle='Modeled path integrated attenuation', psym=2, yrange=m3,xrange=r3
 u3=linfit(ta3,ar3)
 x3=correlate(ta3,ar3)
 oplot, r3,r3*u3[1]+u3[0],linestyle=2, color=250
 legend, ['y='+strtrim(u3[1],2)+'x+'+strtrim(u3[0],2),'R='+strtrim(x3,2)],textcolors=[250,250],box=0

 ux=where(totoxa[n] gt 0 and totoxa[n] lt 20)
 tax=totoxa[n[ux]]
 arx=oxa[ux]

 plot, tax,arx, title='Modeled-Measured comparison @ Oxygen-A band',$
  xtitle='Measured path integrated attenuation',ytitle='Modeled path integrated attenuation', psym=2, yrange=mx,xrange=rx
 ux=linfit(tax,arx)
 xx=correlate(tax,arx)
 oplot, rx,rx*ux[1]+ux[0],linestyle=2, color=250
 legend, ['y='+strtrim(ux[1],2)+'x+'+strtrim(ux[0],2),'R='+strtrim(xx,2)],textcolors=[250,250],box=0

device, /close
spawn, 'convert '+fp+'.ps '+fp+'.png'


stop
end
