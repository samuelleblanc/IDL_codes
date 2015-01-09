; program to plot the ratio of modeled to measured area for all data points.
; uses the output of the compiled models from compile_area_comp
; and the meausrement from cloudy.out output from plot_multi_area

@legend.pro
pro plot_mod_meas

dir='/home/leblanc/SSFR3/data/'
restore, '/argus/SSFR3/goes/goes_cld_top.out'

dates=date

restore, dir+'../cloudy.out'
restore,'/argus/SSFR3/model/cloudy_model4.out'

days=totday[uniq(totday)]
totop=totday*0.
ii=0
for i=0, n_elements(days)-1 do begin
  n=where(totday eq days[i])
  u=where(dates eq long(days[i]))
  tops=interpol(top[u],tmhrs[u],tottmhrs[n])
  totop[ii:ii+n_elements(tops)-1]=tops
  ii=ii+n_elements(tops)
endfor
stop

n=where(totvalid eq 1 and totop lt 7.5 and totop gt 0. and totarea lt 70 and totarea gt 0)
fp=dir+'area_mod_meas4'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=40, ysize=40
  !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,2,2] & !x.margin=[6,4]

plot, totarea[n], mtotarea[n], psym=3, title='940 nm WV modeled-measured comparison',xtitle='Measured attenuation',$
 ytitle='Modeled attenuation',yrange=[20,60],xrange=[20,65]

a1=linfit(totarea[n],mtotarea[n])
r1=correlate(totarea[n],mtotarea[n])
oplot, findgen(200),a1[0]+findgen(200)*a1[1],linestyle=2,color=250
legend, ['R!U2!N='+strtrim(r1,2)],textcolors=[250],box=0

plot, totarea2[n], mtotarea2[n], psym=3, title='1150 nm WV modeled-measured comparison',xtitle='Measured attenuation',$ 
 ytitle='Modeled attenuation',yrange=[30,90],xrange=[30,80]
a2=linfit(totarea2[n],mtotarea2[n])
r2=correlate(totarea2[n],mtotarea2[n])
oplot, findgen(200),a2[0]+findgen(200)*a2[1],linestyle=2,color=250
legend, ['R!U2!N='+strtrim(r2,2)],textcolors=[250],box=0

plot, totarea3[n], mtotarea3[n], psym=3, title='820 nm WV modeled-measured comparison',xtitle='Measured attenuation',$ 
 ytitle='Modeled attenuation',yrange=[2,8],xrange=[2,8]
a3=linfit(totarea3[n],mtotarea3[n])
r3=correlate(totarea3[n],mtotarea3[n])
oplot, findgen(200),a3[0]+findgen(200)*a3[1],linestyle=2,color=250
legend, ['R!U2!N='+strtrim(r3,2)],textcolors=[250],box=0

plot, totoxa[n], mtotoxa[n], psym=3, title='Oxygen-A modeled-measured comparison',xtitle='Measured attenuation',$ 
 ytitle='Modeled attenuation',yrange=[4,12],xrange=[3,6]
ax=linfit(totoxa[n],mtotoxa[n])
rx=correlate(totoxa[n],mtotoxa[n])
oplot, findgen(200),ax[0]+findgen(200)*ax[1],linestyle=2,color=250
legend, ['R!U2!N='+strtrim(rx,2)],textcolors=[250],box=0

device, /close
spawn, 'convert '+fp+'.ps '+fp+'.png'

stop

end
