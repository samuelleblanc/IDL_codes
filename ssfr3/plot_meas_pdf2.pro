; program to plot the measurement pdf for two different combination of tau ref and phase


pro plot_meas_pdf
dir='~/SSFR3/plots/'
dir='C:\Users\Samuel\Research\SSFR3\'


print, 'restoring ice water case, tau=5, ref=25 um'
;restore, '~/SSFR3/data/meas_pdf_liq.out'
restore,dir+'data\meas_pdf_t5_r25_i.out'
meas_pdf1=meas_pdf
bin1=bini

restore,dir+'data\meas_pdf_t40_r7_l.out' 
meas_pdf2=meas_pdf
bin2=binl

restore,dir+'data\meas_pdf_t20_r50_i.out' 
meas_pdf3=meas_pdf
bin3=bini

;normalize with integral to compare each pdf
for p=0,n_elements(bin1[*,0])-1 do begin
 ; k1=int_tabulated(bin1[p,*],meas_pdf1[p,*])
  k1=total(meas_pdf1[p,*])*(bin1[p,1]-bin1[p,0])
  meas_pdf1[p,*]=meas_pdf1[p,*]/k1
  ;k2=int_tabulated(bin2[p,*],meas_pdf2[p,*])
  k2=total(meas_pdf2[p,*])*(bin2[p,1]-bin2[p,0])
  meas_pdf2[p,*]=meas_pdf2[p,*]/k2
  ;k3=int_tabulated(bin3[p,*],meas_pdf3[p,*])
  k3=total(meas_pdf3[p,*])*(bin3[p,1]-bin3[p,0])
  meas_pdf3[p,*]=meas_pdf3[p,*]/k3
endfor
stop

fp=dir+'plots\p2\sample_meas_pdf2'
  print, 'making plot :'+fp
  set_plot, 'ps'
  loadct, 39, /silent
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
  device, xsize=20, ysize=13
   !p.font=1 & !p.thick=5 & !p.charsize=2.1 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
   !p.multi=0 & !x.margin=[7,2] & !y.margin=[4,1] & !p.symsize=1.5 & !y.omargin=[0,0] & !x.omargin=[0,0]

  tvlct,0,150,0,150
  p=1
  del='!9'+string("266B)+'!X' ;"
bar='!9'+string("244B)+'!X' ;"
ds='!U'+del+'!N'+bar+'!D'+del+'!9l!X!I'
dds='!U'+del+'!e2!X!N'+bar+'!D'+del+'!9l!X!e2!X!I'

lls=ds+'1.2!N'


  xtit='!9h!X!D2!N -   ' ;!CDerivative of normalized radiance at 1200 nm'
  plot, bin1[p,*],meas_pdf1[p,*],xtitle=xtit,ytitle='Probability density',/nodata,xrange=[-3.5,0.5],yr=[0,23]
  oplot,bin1[p,*],meas_pdf1[p,*],color=50
  oplot,bin2[p,*],meas_pdf2[p,*],color=250
  oplot,bin3[p,*],meas_pdf3[p,*],color=150
  xyouts, -1.4,-5.5,/data,lls,charsize=3.3
  legend,['A - !9t!X=40, r!De!N=7 !9m!Xm, liquid','B - !9t!X=20, r!De!N=50 !9m!Xm, ice','C - !9t!X=5, r!De!N=25 !9m!Xm, ice'],$
   textcolors=[250,150,50],box=0
 device, /close
 spawn,'convert '+fp+'.ps '+fp+'.png'

stop
end
