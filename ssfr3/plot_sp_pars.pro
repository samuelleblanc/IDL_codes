; program to plot a sample spectra with all the regions of each parameter highlighted
; uses measured spectra as based

pro plot_sp_pars

dir='/home/leblanc/SSFR3/data/'
dir='C:\Users\Samuel\Research\SSFR3\data\'

restore,dir+'2012052303_sp_ex.out'
wvl=wl

fn=dir+'sp_pars'
set_plot,'ps'
print, 'making plot :'+fn
loadct, 39, /silent
device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fn+'.ps'
device, xsize=20, ysize=20
 !p.font=1 & !p.thick=8 & !p.charsize=2.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
 !p.multi=0 & !x.margin=[7,3] &!x.omargin=[0,0]

 sp=sp/max(sp)

 plot, wvl,sp, xtitle='Wavelength (nm)',ytitle='Normalized radiance',xrange=[350.,1700.]
 clrs=findgen(15)/14.*254

;par 1
 nul=min(abs(wvl-1000.),i01)
 nul=min(abs(wvl-1100.),i11)
 oplot, wvl[i01:i11],sp[i01:i11],color=clrs[0],thick=120 ;psym=7,color=clrs[0], symsize=2
 oplot, wvl[i01:i11],sp[i01:i11],color=255,thick=15
;par 2
 nul=min(abs(wvl-1200.),id2)
 plots,wvl[id2],sp[id2],psym=1,color=clrs[1],symsize=2

;par 3
 nul=min(abs(wvl-1500.),id3)
 plots,wvl[id3],sp[id3],psym=1,color=clrs[2],symsize=2

;par 4
 nul=min(abs(wvl-1200.),i04) 
 nul=min(abs(wvl-1237.),i14)
 plots, wvl[i04],sp[i04],psym=7,color=clrs[3],symsize=2
 plots, wvl[i14],sp[i14],psym=7,color=clrs[3],symsize=2

;par 5
 nul=min(abs(wvl-1245.),i05)
 nul=min(abs(wvl-1270.),i15) 
 oplot, wvl[i05:i15],sp[i05:i15], thick=90,color=clrs[4]

;par 6
 nul=min(abs(wvl-1565.),i06)
 nul=min(abs(wvl-1640.),i16)  
 oplot, wvl[i06:i16],sp[i06:i16], thick=90,color=clrs[5] 

;par 7
 nul=min(abs(wvl-1000.),i07)
 nul=min(abs(wvl-1050.),i17)  
 oplot, wvl[i07:i17],sp[i07:i17], thick=80,color=clrs[6]

;par 8
 nul=min(abs(wvl-1490.),i08)
 nul=min(abs(wvl-1600.),i18) 
 oplot, wvl[i08:i18],sp[i08:i18], thick=50,color=clrs[7]

;par 9
 nul=min(abs(wvl-1000.),i09)
 nul=min(abs(wvl-1080.),i19) 
 oplot, wvl[i09:i19],sp[i09:i19], thick=40,color=clrs[8]

;par 10
 nul=min(abs(wvl-1200.),i010)
 nul=min(abs(wvl-1310.),i110) 
 oplot, wvl[i010:i110],sp[i010:i110], thick=50,color=clrs[9]

;par 11
 nul=min(abs(wvl-550.),i011)
 nul=min(abs(wvl-680.),i111)
 ll=linfit(wvl[i011:i111],sp[i011:i111]) 
 oplot, wvl[i011:i111],ll[0]+ll[1]*wvl[i011:i111], thick=80,color=clrs[10]

;par 12
 nul=min(abs(wvl-1000.),id12) 
 plots,wvl[id12],sp[id12],psym=1,color=clrs[11]

;par 13
 nul=min(abs(wvl-1040.),id13)
 plots,wvl[id13],sp[id13],psym=1,color=clrs[12]

;par 14 
 nul=min(abs(wvl-1065.),id14)
 plots,wvl[id14],sp[id14],psym=1,color=clrs[13]

;par 15 
 nul=min(abs(wvl-1565.),i015)
 nul=min(abs(wvl-1634.),i115)
 lv=linfit(wvl[i015:i115],sp[i015:i115])
 oplot,wvl[i015:i115],lv[0]+lv[1]*wvl[i015:i115],thick=50,color=clrs[14]

 oplot,wvl,sp
 plots,wvl[id2],sp[id2],psym=1,color=clrs[1],symsize=2
 plots,wvl[id3],sp[id3],psym=1,color=clrs[2],symsize=2
 plots, wvl[i04],sp[i04],psym=7,color=clrs[3],symsize=2
 plots, wvl[i14],sp[i14],psym=7,color=clrs[3],symsize=2
 plots,wvl[id12],sp[id12],psym=1,color=clrs[11],symsize=2.3
 plots,wvl[id13],sp[id13],psym=1,color=clrs[12],symsize=2.3
 plots,wvl[id14],sp[id14],psym=1,color=clrs[13],symsize=2.3

neta="150B ;'"
estr='!9'+string(neta)+'!X'
legend,estr+'='+string(indgen(15)+1,format='(I2)'),textcolors=clrs,box=0,/right,charsize=1.8

device, /close
spawn, 'convert "'+dir+'.ps" "'+dir+'.png"'
stop
end
