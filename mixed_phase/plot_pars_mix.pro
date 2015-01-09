; program to plot all 15 parameters as they vary with tau and ref for ice and water

@legend.pro
@C:/
pro plot_pars_mix

win=1

if not win then dir='/home/leblanc/mixed_phase/plots/' else $
 dir='C:\Users\Samuel\Research\mixed_phase\plots\'

;restore pars lut
if not win then fn='/argus/roof/SSFR3/model/pars_mix_std.out' else $
  fn='C:\Users\Samuel\Research\mixed_phase\data\pars_mix_std.out'
print, 'restoring '+fn
restore, fn
stdp=std

;restore measurment pars
if 0 then begin
if win then fn='C:\Users\Samuel\Research\mixed_phase\data\meas_mix_std.out' else $
fn='~/SSFR3/data/meas_std.out'
print, 'restoring '+fn
restore, fn
nn=where(std[*,*,*,14] gt 0.1)
nni=array_indices(std[*,*,*,14],nn)
std[nni[0,*],nni[1,*],nni[2,*],14]=std[nni[0,*],nni[1,*],nni[2,*],14]*0.+0.1
stop
meas_std=stdp
mt=fltarr(200,15,2,16)
for p=0,15 do begin
  for r=0,13 do begin
    mt[*,r,0,p]=interpol(std[*,r,0,p],tau,tau_hires,/nan)
    mt[*,r,1,p]=interpol(std[*,r,1,p],tau,tau_hires,/nan)
  endfor
  for t=0,199 do begin
    meas_std[t,*,0,p]=interpol(mt[t,*,0,p],ref,ref_hires,/nan)
    meas_std[t,*,1,p]=interpol(mt[t,*,1,p],ref,ref_hires,/nan)
  endfor
endfor	
std=sqrt(stdp*stdp+meas_std*meas_std*2.)

std[*,*,*,14]=std[*,*,*,14]/2.

save, std, avg,tau_hires,ref_hires,filename='~/SSFR3/data/Pars_std_meas_lut_snow.out'
endif
;stop

;restore the rad_mu at 515 nm
if win then restore, 'C:\Users\Samuel\Research\mixed_phase\data\sza_pdf_mix.out' else $
 restore, '/argus/roof/SSFR3/model/sza_pdf_mix.out' 
rad=rad*1000.
r=where(rad lt 0.)
rad[r]=0.
; now set up plotting

if 1 then begin
fp=dir+'pars_mix_tau'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=40, ysize=32
  !p.font=1 & !p.thick=5 & !p.charsize=4.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,4,5] & !x.margin=[6,1.5] & !y.margin=[0.155555,0.4] & !y.omargin=[3,1]

tvlct,220,220,220,201
tvlct,250,200,200,251
tvlct,200,200,250,71


;tau=tau_hires
 for p=0, 16 do begin

taus=tau

;AVG             FLOAT     = Array[100, 25, 14, 11, 17]
; tau=rad[*,14,0,0,18]
 xr=[1,100]
 ;xr=[0,0.6]
  yr=[min(avg[0:99,[4,9],[0,6],*,p]-std[0:99,[4,9],[0,6],*,p],/nan)*0.98,max(avg[0:99,[4,9],[0,6],*,p]+std[0:99,[4,9],[0,6],*,p],/nan)*1.02]
  if p gt 13 then plot, tau,avg[*,14,0,0,p],xtitle='Optical thickness',xrange=xr,ytitle='!9h!X!D'+string(p+1,format='(I2)')+'!N',yr=yr, xticklen=0.1,/nodata else $
   plot, tau,avg[*,14,0,0,p],xtickname=[' ',' ',' ',' ',' ',' '],xrange=xr,ytitle='!9h!X!D'+string(p+1,format='(I2)')+'!N',yr=yr, xticklen=0.1,/nodata

 ;tau=rad[*,4,0,0,18]
  polyfill,[tau[0:99],reverse(tau[0:99])],[avg[0:99,4,0,0,p]-std[0:99,4,0,0,p],reverse(avg[0:99,4,0,0,p]+std[0:99,4,0,0,p])],color=201
  oplot,tau,avg[*,4,0,0,p],thick=7
 ;tau=rad[*,9,6,0,18]
  polyfill,[tau[0:99],reverse(tau[0:99])],[avg[0:99,9,6,0,p]-std[0:99,9,6,0,p],reverse(avg[0:99,9,6,0,p]+std[0:99,9,6,0,p])],color=201
  oplot,tau,avg[*,9,6,0,p],linestyle=2,thick=7 
 
 ;tau=rad[*,4,0,4,18]
  polyfill,[tau[0:99],reverse(tau[0:99])],[avg[0:99,4,0,4,p]-std[0:99,4,0,4,p],reverse(avg[0:99,4,0,4,p]+std[0:99,4,0,4,p])],color=71
  oplot,tau,avg[*,4,0,4,p],color=70,thick=7
 ;tau=rad[*,9,6,4,18]
  polyfill,[tau[0:99],reverse(tau[0:99])],[avg[0:99,9,6,4,p]-std[0:99,9,6,4,p],reverse(avg[0:99,9,6,4,p]+std[0:99,9,6,4,p])],color=71
  oplot,tau,avg[*,9,6,4,p],color=70,linestyle=2,thick=7

 ;tau=rad[*,4,0,10,18]
  polyfill,[tau[0:99],reverse(tau[0:99])],[avg[0:99,4,0,10,p]-std[0:99,4,0,10,p],reverse(avg[0:99,4,0,10,p]+std[0:99,4,0,10,p])],color=251
  oplot,tau,avg[*,4,0,10,p],color=250,thick=7
 ;tau=rad[*,9,6,10,18]
  polyfill,[tau[0:99],reverse(tau[0:99])],[avg[0:99,9,6,10,p]-std[0:99,9,6,10,p],reverse(avg[0:99,9,6,10,p]+std[0:99,9,6,10,p])],color=251
  oplot,tau,avg[*,9,6,10,p],color=250,linestyle=2,thick=7
 endfor

; now put in the radiace at 515 nm
p=16
;avg=rad
;std=stdr
  yr=[min(rad[0:99,[4,9],[0,6],*,p]-stdr[0:99,[4,9],[0,6],*,p],/nan)*0.98,max(rad[0:99,[4,9],[0,6],*,p]+stdr[0:99,[4,9],[0,6],*,p],/nan)*1.02]
  plot, tau,rad[*,14,0,0,p],xtitle='Optical thickness',xrange=[1,100],ytitle='!9h!X!D'+string(p+1,format='(I2)')+'!N',yr=yr, xticklen=0.1,/nodata 

  polyfill,[tau[0:99],reverse(tau[0:99])],[rad[0:99,4,0,0,p]-stdr[0:99,4,0,0,p],reverse(rad[0:99,4,0,0,p]+stdr[0:99,4,0,0,p])],color=201
  oplot,tau,rad[*,4,0,0,p],thick=7
  polyfill,[tau[0:99],reverse(tau[0:99])],[rad[0:99,9,6,0,p]-stdr[0:99,9,6,0,p],reverse(rad[0:99,9,6,0,p]+stdr[0:99,9,6,0,p])],color=201
  oplot,tau,rad[*,9,6,0,p],linestyle=2,thick=7

  polyfill,[tau[0:99],reverse(tau[0:99])],[rad[0:99,4,0,4,p]-stdr[0:99,4,0,4,p],reverse(rad[0:99,4,0,4,p]+stdr[0:99,4,0,4,p])],color=71
  oplot,tau,rad[*,4,0,4,p],color=70,thick=7
  polyfill,[tau[0:99],reverse(tau[0:99])],[rad[0:99,9,6,4,p]-stdr[0:99,9,6,4,p],reverse(rad[0:99,9,6,4,p]+stdr[0:99,9,6,4,p])],color=71
  oplot,tau,rad[*,9,6,4,p],color=70,linestyle=2,thick=7

  polyfill,[tau[0:99],reverse(tau[0:99])],[rad[0:99,4,0,10,p]-stdr[0:99,4,0,10,p],reverse(rad[0:99,4,0,10,p]+stdr[0:99,4,0,10,p])],color=251
  oplot,tau,rad[*,4,0,10,p],color=250,thick=7
  polyfill,[tau[0:99],reverse(tau[0:99])],[rad[0:99,9,6,10,p]-stdr[0:99,9,6,10,p],reverse(rad[0:99,9,6,10,p]+stdr[0:99,9,6,10,p])],color=251
  oplot,tau,rad[*,9,6,10,p],color=250,linestyle=2,thick=7

 legend,['0% ice','50% ice','100% ice','r!De,liq!N=10 !9m!Xm - r!De,ice!N=15 !9m!Xm','r!De,liq!N=20 !9m!Xm - r!De,ice!N=30 !9m!Xm'],textcolors=[0,70,250,0,0],color=[255,255,255,0,0],linestyle=[0,0,0,0,2],pspacing=1.2,box=0,position=[0.65,0.18],/normal,charsize=2.6

 device, /close
 spawn, 'convert '+fp+'.ps '+fp+'.png'
endif

if 1 then begin
fp=dir+'pars_mix_refl'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=40, ysize=32
  !p.font=1 & !p.thick=5 & !p.charsize=4.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,4,5] & !x.margin=[6,1.5] & !y.margin=[0.15,0.4] & !y.omargin=[4,1]
 
tvlct,220,220,220,201
tvlct,250,200,200,251
tvlct,200,200,250,71

  for p=0, 16 do begin

; dependence on refl
    xr=[1,50]
    yr=[min(avg[[2,19],*,[0,6],*,p]-std[[2,19],*,[0,6],*,p],/nan)*0.98,max(avg[[2,19],*,[0,6],*,p]+std[[2,19],*,[0,6],*,p],/nan)*1.02]
    if p gt 13 then plot, refl,avg[2,*,0,0,p],xtitle='Liquid!Ceffective radius (!9m!Xm)',xrange=xr,ytitle='!9h!X!D'+string(p+1,format='(I2)')+'!N',yr=yr, xticklen=0.1,/nodata else $
     plot, refl,avg[2,*,0,0,p],xtickname=[' ',' ',' ',' ',' ',' '],xrange=xr,ytitle='!9h!X!D'+string(p+1,format='(I2)')+'!N',yr=yr, xticklen=0.1,/nodata
   
    polyfill,[refl,reverse(refl)],[reform(avg[2,*,0,0,p]-std[2,*,0,0,p]),reverse(reform(avg[2,*,0,0,p]+std[2,*,0,0,p]))],color=201
    oplot,refl,avg[2,*,0,0,p],thick=7 
    polyfill,[refl,reverse(refl)],[reform(avg[19,*,6,0,p]-std[19,*,6,0,p]),reverse(reform(avg[19,*,6,0,p]+std[19,*,6,0,p]))],color=201
    oplot,refl,avg[19,*,6,0,p],linestyle=2,thick=7 
  
    polyfill,[refl,reverse(refl)],[reform(avg[2,*,0,4,p]-std[2,*,0,4,p]),reverse(reform(avg[2,*,0,4,p]+std[2,*,0,4,p]))],color=71
    oplot,refl,avg[2,*,0,4,p],color=70,thick=7 
    polyfill,[refl,reverse(refl)],[reform(avg[19,*,6,4,p]-std[19,*,6,4,p]),reverse(reform(avg[19,*,6,4,p]+std[19,*,6,4,p]))],color=71
    oplot,refl,avg[19,*,6,4,p],color=70,linestyle=2,thick=7  
   
    polyfill,[refl,reverse(refl)],[reform(avg[2,*,0,10,p]-std[2,*,0,10,p]),reverse(reform(avg[2,*,0,10,p]+std[2,*,0,10,p]))],color=251
    oplot,refl,avg[2,*,0,10,p],color=250,thick=7 
    polyfill,[refl,reverse(refl)],[reform(avg[19,*,6,10,p]-std[19,*,6,10,p]),reverse(reform(avg[19,*,6,10,p]+std[19,*,6,10,p]))],color=251
    oplot,refl,avg[19,*,6,10,p],color=250,linestyle=2,thick=7 

  endfor

 ; now put in the radiace at 515 nm
p=16
;avg=rad
;std=stdr
  yr=[min(rad[[2,19],*,[0,6],*,p]-stdr[[2,19],*,[0,6],*,p],/nan)*0.98,max(rad[[2,19],*,[0,6],*,p]+stdr[[2,19],*,[0,6],*,p],/nan)*1.02]
  plot, refl,rad[2,*,0,0,p],xtitle='Liquid!Ceffective radius (!9m!Xm)',xrange=xr,ytitle='!9h!X!D'+string(p+1,format='(I2)')+'!N',yr=yr, xticklen=0.1,/nodata

  polyfill,[refl,reverse(refl)],[reform(rad[2,*,0,0,p]-stdr[2,*,0,0,p]),reverse(reform(rad[2,*,0,0,p]+stdr[2,*,0,0,p]))],color=201
  oplot,refl,rad[2,*,0,0,p],thick=7
  polyfill,[refl,reverse(refl)],[reform(rad[19,*,6,0,p]-stdr[19,*,6,0,p]),reverse(reform(rad[19,*,6,0,p]+stdr[19,*,6,0,p]))],color=201
  oplot,refl,rad[19,*,6,0,p],linestyle=2,thick=7

  polyfill,[refl,reverse(refl)],[reform(rad[2,*,0,4,p]-stdr[2,*,0,4,p]),reverse(reform(rad[2,*,0,4,p]+stdr[2,*,0,4,p]))],color=71
  oplot,refl,rad[2,*,0,4,p],color=70,thick=7
  polyfill,[refl,reverse(refl)],[reform(rad[19,*,6,4,p]-stdr[19,*,6,4,p]),reverse(reform(rad[19,*,6,4,p]+stdr[19,*,6,4,p]))],color=71
  oplot,refl,rad[19,*,6,4,p],color=70,linestyle=2,thick=7

  polyfill,[refl,reverse(refl)],[reform(rad[2,*,0,10,p]-stdr[2,*,0,10,p]),reverse(reform(rad[2,*,0,10,p]+stdr[2,*,0,10,p]))],color=251
  oplot,refl,rad[2,*,0,10,p],color=250,thick=7
  polyfill,[refl,reverse(refl)],[reform(rad[19,*,6,10,p]-stdr[19,*,6,10,p]),reverse(reform(rad[19,*,6,10,p]+stdr[19,*,6,10,p]))],color=251
  oplot,refl,rad[19,*,6,10,p],color=250,linestyle=2,thick=7

 legend,['0% ice','50% ice','100% ice','!9t!X=3 - r!De,ice!N=15 !9m!Xm','!9t!x=20 - r!De,ice!N=30 !9m!Xm'],textcolors=[0,70,250,0,0],color=[255,255,255,0,0],linestyle=[0,0,0,0,2],pspacing=1.2,box=0,position=[0.65,0.18],/normal,charsize=2.6


 device, /close
 spawn, 'convert '+fp+'.ps '+fp+'.png'
 
 
endif


if 1 then begin
fp=dir+'pars_mix_refi'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=40, ysize=32
  !p.font=1 & !p.thick=5 & !p.charsize=4.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,4,5] & !x.margin=[6,1.5] & !y.margin=[0.15,0.4] & !y.omargin=[4,1]
  
tvlct,220,220,220,201
tvlct,250,200,200,251
tvlct,200,200,250,71
 
  for p=0, 16 do begin
  xr=[15,50]

; dependence on refi 
    yr=[min(avg[[2,19],[4,9],*,*,p]-std[[2,19],[4,9],*,*,p],/nan)*0.98,max(avg[[2,19],[4,9],*,*,p]+std[[2,19],[4,9],*,*,p],/nan)*1.02]
    if p gt 13 then plot, refi,avg[2,14,*,0,p],xtitle='Ice!Ceffective radius (!9m!Xm)',xrange=xr,ytitle='!9h!X!D'+string(p+1,format='(I2)')+'!N',yr=yr, xticklen=0.1,/nodata,xticks=4 else $
     plot, refi,avg[2,14,*,0,p],xtickname=[' ',' ',' ',' ',' ',' '],xrange=xr,ytitle='!9h!X!D'+string(p+1,format='(I2)')+'!N',yr=yr, xticklen=0.1,/nodata,xticks=4 
    
    polyfill,[refi,reverse(refi)],[reform(avg[2,4,*,0,p]-std[2,4,*,0,p]),reverse(reform(avg[2,4,*,0,p]+std[2,4,*,0,p]))],color=201 
    oplot,refi,avg[2,4,*,0,p],thick=7  
    polyfill,[refi,reverse(refi)],[reform(avg[19,9,*,0,p]-std[19,9,*,0,p]),reverse(reform(avg[19,9,*,0,p]+std[19,9,*,0,p]))],color=201 
    oplot,refi,avg[19,9,*,0,p],linestyle=2,thick=7  
   
    polyfill,[refi,reverse(refi)],[reform(avg[2,4,*,4,p]-std[2,4,*,4,p]),reverse(reform(avg[2,4,*,4,p]+std[2,4,*,4,p]))],color=71 
    oplot,refi,avg[2,4,*,4,p],color=70,thick=7  
    polyfill,[refi,reverse(refi)],[reform(avg[19,9,*,4,p]-std[19,9,*,4,p]),reverse(reform(avg[19,9,*,4,p]+std[19,9,*,4,p]))],color=71 
    oplot,refi,avg[19,9,*,4,p],color=70,linestyle=2,thick=7   
    
    polyfill,[refi,reverse(refi)],[reform(avg[2,4,*,10,p]-std[2,4,*,10,p]),reverse(reform(avg[2,4,*,10,p]+std[2,4,*,10,p]))],color=251 
    oplot,refi,avg[2,4,*,10,p],color=250,thick=7  
    polyfill,[refi,reverse(refi)],[reform(avg[19,9,*,10,p]-std[19,9,*,10,p]),reverse(reform(avg[19,9,*,10,p]+std[19,9,*,10,p]))],color=251 
    oplot,refi,avg[19,9,*,10,p],color=250,linestyle=2,thick=7 

  endfor
p=16
;avg=rad
;std=stdr
  yr=[min(rad[[2,19],[4,9],*,*,p]-stdr[[2,19],[4,9],*,*,p],/nan)*0.98,max(rad[[2,19],[4,9],*,*,p]+stdr[[2,19],[4,9],*,*,p],/nan)*1.02]
  plot, refi,rad[2,4,*,0,p],xtitle='Ice!Ceffective radius (!9m!Xm)',xrange=xr,ytitle='!9h!X!D'+string(p+1,format='(I2)')+'!N',yr=yr, xticklen=0.1,/nodata,xticks=4

  polyfill,[refi,reverse(refi)],[reform(rad[2,4,*,0,p]-stdr[2,4,*,0,p]),reverse(reform(rad[2,4,*,0,p]+stdr[2,4,*,0,p]))],color=201
  oplot,refi,rad[2,4,*,0,p],thick=7
  polyfill,[refi,reverse(refi)],[reform(rad[19,9,*,0,p]-stdr[19,9,*,0,p]),reverse(reform(rad[19,9,*,0,p]+stdr[19,9,*,0,p]))],color=201
  oplot,refi,rad[19,9,*,0,p],linestyle=2,thick=7

  polyfill,[refi,reverse(refi)],[reform(rad[2,4,*,4,p]-stdr[2,4,*,4,p]),reverse(reform(rad[2,4,*,4,p]+stdr[2,4,*,4,p]))],color=71
  oplot,refi,rad[2,4,*,4,p],color=70,thick=7
  polyfill,[refi,reverse(refi)],[reform(rad[19,9,*,4,p]-stdr[19,9,*,4,p]),reverse(reform(rad[19,9,*,4,p]+stdr[19,9,*,4,p]))],color=71
  oplot,refi,rad[19,9,*,4,p],color=70,linestyle=2,thick=7
 
  polyfill,[refi,reverse(refi)],[reform(rad[2,4,*,10,p]-stdr[2,4,*,10,p]),reverse(reform(rad[2,4,*,10,p]+stdr[2,4,*,10,p]))],color=251
  oplot,refi,rad[2,4,*,10,p],color=250,thick=7 
  polyfill,[refi,reverse(refi)],[reform(rad[19,9,*,10,p]-stdr[19,9,*,10,p]),reverse(reform(rad[19,9,*,10,p]+stdr[19,9,*,10,p]))],color=251
  oplot,refi,rad[19,9,*,10,p],color=250,linestyle=2,thick=7 
 
 legend,['0% ice','50% ice','100% ice','!9t!X=3 - r!De,liq!N=10 !9m!Xm','!9t!x=20 - r!De,liq!N=20 !9m!Xm'],textcolors=[0,70,250,0,0],color=[255,255,255,0,0],linestyle=[0,0,0,0,2],pspacing=1.2,box=0,position=[0.65,0.18],/normal,charsize=2.6

 device, /close 
 spawn, 'convert '+fp+'.ps '+fp+'.png'


endif

stop
end
