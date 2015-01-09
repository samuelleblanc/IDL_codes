; program to plot all 15 parameters as they vary with tau and ref for ice and water

pro plot_pars

dir='/home/leblanc/SSFR3/plots/new/'
sno='_snow'
;sno=''
;restore pars lut
fn='/argus/roof/SSFR3/model/pars_std_v3'+sno+'.out'
print, 'restoring '+fn
restore, fn
stdp=std

;restore measurment pars
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

save, std, avg,tau_hires,ref_hires,filename='~/SSFR3/data/Pars_std_meas_lut'+sno+'_v4.out'

;stop

; now set up plotting

fp=dir+'pars'+sno+'_v4'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=40, ysize=26
  !p.font=1 & !p.thick=5 & !p.charsize=4.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,4,4] & !x.margin=[6,1.5] & !y.margin=[0.155555,0.4] & !y.omargin=[3,1]

tvlct,220,220,220,201
tvlct,250,200,200,251
tvlct,200,200,250,71


tau=tau_hires
 for p=0, 14 do begin
 
  yr=[min(avg[0:99,[14,24,34],*,p]-std[0:99,[14,24,34],*,p],/nan)*0.98,max(avg[0:99,[14,24,34],*,p]+std[0:99,[14,24,34],*,p],/nan)*1.02]
  if p gt 10 then plot, tau,avg[*,14,0,p],xtitle='Optical thickness',xrange=[1,100],ytitle='!9h!X!D'+string(p+1,format='(I2)')+'!N',yr=yr, xticklen=0.1,/nodata else $
   plot, tau,avg[*,14,0,p],xtickname=[' ',' ',' ',' ',' ',' '],xrange=[1,100],ytitle='!9h!X!D'+string(p+1,format='(I2)')+'!N',yr=yr, xticklen=0.1,/nodata

  polyfill,[tau[0:99],reverse(tau[0:99])],[avg[0:99,14,0,p]-std[0:99,14,0,p],reverse(avg[0:99,14,0,p]+std[0:99,14,0,p])],color=201
  oplot,tau,avg[*,14,0,p],thick=7
  polyfill,[tau[0:99],reverse(tau[0:99])],[avg[0:99,14,1,p]-std[0:99,14,1,p],reverse(avg[0:99,14,1,p]+std[0:99,14,1,p])],color=201
  oplot,tau,avg[*,14,1,p],linestyle=2,thick=7 
 
  polyfill,[tau[0:99],reverse(tau[0:99])],[avg[0:99,24,0,p]-std[0:99,24,0,p],reverse(avg[0:99,24,0,p]+std[0:99,24,0,p])],color=71
  oplot,tau,avg[*,24,0,p],color=70,thick=7
  polyfill,[tau[0:99],reverse(tau[0:99])],[avg[0:99,24,1,p]-std[0:99,24,1,p],reverse(avg[0:99,24,1,p]+std[0:99,24,1,p])],color=71
  oplot,tau,avg[*,24,1,p],color=70,linestyle=2,thick=7

  polyfill,[tau[0:99],reverse(tau[0:99])],[avg[0:99,34,0,p]-std[0:99,34,0,p],reverse(avg[0:99,34,0,p]+std[0:99,34,0,p])],color=251
  oplot,tau,avg[*,34,0,p],color=250,thick=7
  polyfill,[tau[0:99],reverse(tau[0:99])],[avg[0:99,34,1,p]-std[0:99,34,1,p],reverse(avg[0:99,34,1,p]+std[0:99,34,1,p])],color=251
  oplot,tau,avg[*,34,1,p],color=250,linestyle=2,thick=7
 endfor

 legend,['r!De!N=15 !9m!Xm','r!De!N=25 !9m!Xm','r!De!N=35 !9m!Xm','Liquid','Ice'],textcolors=[0,70,250,0,0],color=[255,255,255,0,0],linestyle=[0,0,0,0,2],pspacing=1.2,box=0,position=[0.8,0.2],/normal,charsize=2.6

 device, /close
 spawn, 'convert '+fp+'.ps '+fp+'.png'

stop
end
