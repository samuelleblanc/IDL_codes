; program that plots the dependance of each factor on the measured area (integrated attenuation)
; makes some sort of lut with three dependance

@colorbar.pro
@legend.pro
pro plot_meas_table

dir='/home/leblanc/SSFR3/data/'

;restore, dir+'area_table_mod.out'
restore, dir+'cloudy.out'

; put the measurements into subset of each values
tau=(findgen(8)+1.)*25.
ref=(findgen(5)+1.)*5.
base=(findgen(4))*2000.
water=findgen(5)*2.+14.

area=fltarr(n_elements(base),n_elements(tau),n_elements(ref),n_elements(water),4)

taul=[tau-(tau[1]-tau[0])/2.,tau[n_elements(tau)-1]+(tau[1]-tau[0])/2.]
refl=[ref-(ref[1]-ref[0])/2.,ref[n_elements(ref)-1]+(ref[1]-ref[0])/2.]
basel=[base-(base[1]-base[0])/2.,base[n_elements(base)-1]+(base[1]-base[0])/2.]
waterl=[water-(water[1]-water[0])/2.,water[n_elements(water)-1]+(water[1]-water[0])/2.]

for b=0, n_elements(base)-1 do begin
print, b
  for t=0, n_elements(tau)-1 do begin
    for r=0, n_elements(ref)-1 do begin
      for w=0, n_elements(water)-1 do begin
        n=where(totbase ge basel[b] and totbase lt basel[b+1] and totref ge refl[r] and totref lt refl[r+1] $
            and tottau ge taul[t] and tottau lt taul[t+1] and totwater ge waterl[w] and totwater lt waterl[w+1],ns)
        if ns gt 1 then begin
           area[b,t,r,w,0]=mean(totarea[n])
           area[b,t,r,w,1]=mean(totarea2[n])
           area[b,t,r,w,2]=mean(totarea3[n])
           area[b,t,r,w,3]=mean(totoxa[n])
        endif else begin
           area[b,t,r,w,0]=-999.
           area[b,t,r,w,1]=-999.
           area[b,t,r,w,2]=-999.
           area[b,t,r,w,3]=-999.
        endelse
      endfor  ;water loop
    endfor ;ref loop
  endfor ;tau loop
endfor ;base loop

fp=dir+'area_table_meas_wv1'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=20, ysize=45
  !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,1,3] & !x.margin=[6,9]
u=where(area[*,*,*,*,0] ne -999.)
ar=area[*,*,*,*,0]
yr=[min(ar[u],/nan),max(ar[u],/nan)]
llr=findgen(n_elements(ref))
ltr='Ref:'+string(ref,format='(I2)')+' um'
plot, tau, area[0,*,0,1,0],psym=-2,title='modeled integrated attenuation for 940 nm WV', xtitle='Cloud optical depth',$
 ytitle='Integrated attenuation', yrange=yr
for i=1, n_elements(base)-1 do begin
  for j=1, n_elements(ref)-1 do oplot, tau, area[i,*,j,1,0], linestyle=j,psym=-2, color=i*254/n_elements(base)
endfor
legend,ltr,linestyle=llr,box=0
colorbar, minrange=min(base/1000.),maxrange=max(base/1000.), title='Cloud base height (km)',/vertical,/right,$
 format='(F4.2)', position=[0.90,0.70,0.92,0.98]

ltw='Water:'+string(water,format='(I2)')+' mm'
llw=findgen(n_elements(water))
plot, tau, area[1,*,0,0,0],psym=-2,title='modeled integrated attenuation for 940 nm WV', xtitle='Cloud optical depth',$
 ytitle='Integrated attenuation',yrange=yr
for i=0, n_elements(ref)-1 do begin
  for j=0, n_elements(water)-1 do oplot, tau, area[1,*,i,j,0], linestyle=j,psym=-2, color=i*254/n_elements(ref)
endfor
legend, ltw,linestyle=llw,box=0
colorbar, minrange=min(ref),maxrange=max(ref), title='Effective radius (um)',/vertical,/right,$
 format='(F5.2)', position=[0.90,0.36,0.92,0.65]

ltb='Base:'+string(base/1000.,format='(I2)')+' km'
llb=findgen(n_elements(base))
plot, tau, area[1,*,1,0,0],psym=-2,title='modeled integrated attenuation for 940 nm WV', xtitle='Cloud optical depth',$
 ytitle='Integrated attenuation',yrange=yr
for i=0, n_elements(water)-1 do begin
  for j=0, n_elements(base)-1 do oplot, tau, area[j,*,1,i,0], linestyle=j,psym=-2, color=i*254/n_elements(water)
endfor
legend, ltb, linestyle=llb, box=0
colorbar, minrange=min(water),maxrange=max(water), title='Precipitable water vapor (mm)',/vertical,/right,$
 format='(F5.2)', position=[0.90,0.05,0.92,0.31]

device, /close
spawn, 'convert '+fp+'.ps '+fp+'.png'

fp=dir+'area_table_meas_wv2'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=20, ysize=45
  !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,1,3] & !x.margin=[6,9]
u=where(area[*,*,*,*,1] ne -999.)
ar=area[*,*,*,*,1]
yr=[min(ar[u],/nan),max(ar[u],/nan)]

llr=findgen(n_elements(ref))
ltr='Ref:'+string(ref,format='(I2)')+' um'
plot, tau, area[0,*,0,1,1],psym=-2,title='Measured integrated attenuation for 1150 nm WV', xtitle='Cloud optical depth',$
 ytitle='Integrated attenuation', yrange=yr
for i=0, n_elements(base)-1 do $
 for j=0, n_elements(ref)-1 do $
  oplot, tau, area[i,*,j,1,1], linestyle=j,psym=-2, color=i*254/n_elements(base)
legend,ltr,linestyle=llr,box=0
colorbar, minrange=min(base/1000.),maxrange=max(base/1000.), title='Cloud base height (km)',/vertical,/right,$
 format='(F4.2)', position=[0.90,0.70,0.92,0.98]

ltw='Water:'+string(water,format='(I2)')+' mm'
llw=findgen(n_elements(water))
plot, tau, area[1,*,0,0,1],psym=-2,title='Measured integrated attenuation for 1150 nm WV', xtitle='Cloud optical depth',$
 ytitle='Integrated attenuation',yrange=yr
for i=0, n_elements(ref)-1 do $
 for j=0, n_elements(water)-1 do $
  oplot, tau, area[1,*,i,j,1], linestyle=j, psym=-2, color=i*254/n_elements(ref)
legend, ltw,linestyle=llw,box=0
colorbar, minrange=min(ref),maxrange=max(ref), title='Effective radius (um)',/vertical,/right,$
 format='(F5.2)', position=[0.90,0.36,0.92,0.65]

ltb='Base:'+string(base/1000.,format='(I2)')+' km'
llb=findgen(n_elements(base))
plot, tau, area[0,*,1,0,1],psym=-2,title='Measured integrated attenuation for 1150 nm WV', xtitle='Cloud optical depth',$
 ytitle='Integrated attenuation',yrange=yr
for i=0, n_elements(water)-1 do $
 for j=0, n_elements(base)-1 do $
  oplot, tau, area[j,*,1,i,1], linestyle=j, psym=-2, color=i*254/n_elements(water)
legend, ltb, linestyle=llb, box=0
colorbar, minrange=min(water),maxrange=max(water), title='Precipitable water vapor (mm)',/vertical,/right,$
 format='(F5.2)', position=[0.90,0.05,0.92,0.31]

device, /close
spawn, 'convert '+fp+'.ps '+fp+'.png'

fp=dir+'area_table_meas_wv3'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=20, ysize=45
  !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,1,3] & !x.margin=[6,9]

u=where(area[*,*,*,*,2] ne -999.)
ar=area[*,*,*,*,2]
yr=[min(ar[u],/nan),max(ar[u],/nan)]

plot, tau, area[0,*,0,1,2],psym=-2,title='Measured integrated attenuation for 820 nm WV', xtitle='Cloud optical depth',$
 ytitle='Integrated attenuation', yrange=yr
for i=0, n_elements(base)-1 do $
 for j=0, n_elements(ref)-1 do $
  oplot, tau, area[i,*,j,1,2], linestyle=j, psym=-2, color=i*254/n_elements(base)
legend,ltr,linestyle=llr,box=0
colorbar, minrange=min(base/1000.),maxrange=max(base/1000.), title='Cloud base height (km)',/vertical,/right,$
 format='(F4.2)', position=[0.90,0.70,0.92,0.98]

plot, tau, area[1,*,0,0,2],psym=-2,title='Measured integrated attenuation for 820 nm WV', xtitle='Cloud optical depth',$
 ytitle='Integrated attenuation',yrange=yr
for i=0, n_elements(ref)-1 do $
 for j=0, n_elements(water)-1 do $
  oplot, tau, area[1,*,i,j,2], linestyle=j, psym=-2, color=i*254/n_elements(ref)
legend, ltw,linestyle=llw,box=0
colorbar, minrange=min(ref),maxrange=max(ref), title='Effective radius (um)',/vertical,/right,$
 format='(F5.2)', position=[0.90,0.36,0.92,0.65]

plot, tau, area[0,*,1,0,2],psym=-2,title='Measured integrated attenuation for 820 nm WV', xtitle='Cloud optical depth',$
 ytitle='Integrated attenuation',yrange=yr
for i=0, n_elements(water)-1 do $
 for j=0, n_elements(base)-1 do $
  oplot, tau, area[j,*,1,i,2], linestyle=j, psym=-2, color=i*254/n_elements(water)
legend, ltb, linestyle=llb, box=0
colorbar, minrange=min(water),maxrange=max(water), title='Precipitable water vapor (mm)',/vertical,/right,$
 format='(F5.2)', position=[0.90,0.05,0.92,0.31]

device, /close
spawn, 'convert '+fp+'.ps '+fp+'.png'

fp=dir+'area_table_meas_oxa'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=20, ysize=45
  !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,1,3] & !x.margin=[6,9]

u=where(area[*,*,*,*,3] ne -999.)
ar=area[*,*,*,*,3]
yr=[min(ar[u],/nan),max(ar[u],/nan)]
yr=[3,5]

plot, tau, area[0,*,0,0,3],psym=-2,title='Measured integrated attenuation for Oxygen-A', xtitle='Cloud optical depth',$
 ytitle='Integrated attenuation', yrange=yr
for i=0, n_elements(base)-1 do $
 for j=0, n_elements(ref)-1 do $
  oplot, tau, area[i,*,j,0,3], linestyle=j, psym=-2, color=i*254/n_elements(base)
legend,ltr,linestyle=llr,box=0
colorbar, minrange=min(base/1000.),maxrange=max(base/1000.), title='Cloud base height (km)',/vertical,/right,$
 format='(F4.2)', position=[0.90,0.70,0.92,0.98]

plot, tau, area[0,*,0,0,3],psym=-2,title='Measured integrated attenuation for Oxygen-A', xtitle='Cloud optical depth',$
 ytitle='Integrated attenuation',yrange=yr
for i=0, n_elements(ref)-1 do $
 for j=0, n_elements(water)-1 do $
  oplot, tau, area[0,*,i,j,3], linestyle=j, psym=-2, color=i*254/n_elements(ref)
legend, ltw,linestyle=llw,box=0
colorbar, minrange=min(ref),maxrange=max(ref), title='Effective radius (um)',/vertical,/right,$
 format='(F5.2)', position=[0.90,0.36,0.92,0.65]

plot, tau, area[0,*,0,0,3],psym=-2,title='Measured integrated attenuation for Oxygen-A', xtitle='Cloud optical depth',$
 ytitle='Integrated attenuation',yrange=yr
for i=0, n_elements(water)-1 do $
 for j=0, n_elements(base)-1 do $
  oplot, tau, area[j,*,0,i,3], linestyle=j, psym=-2, color=i*254/n_elements(water)
legend, ltb, linestyle=llb, box=0
colorbar, minrange=min(water),maxrange=max(water), title='Precipitable water vapor (mm)',/vertical,/right,$
 format='(F5.2)', position=[0.90,0.05,0.92,0.31]

device, /close
spawn, 'convert '+fp+'.ps '+fp+'.png'

stop
end
