; program to plot the results of the parameters for a mixed phase models
; runs through the mixed phase modeled radiance

@get_params.pro

pro plot_mix_par

dir='/argus/roof/SSFR3/model/'
print, 'restoring :'+dir+'sp_mix2_v2.out'
restore, dir+'sp_mix2_v2.out'

; sp tau, ref_liq, ref_ice, wp
; tau, refl, refi, wp
; SP              FLOAT     = Array[6, 3, 3, 378, 5]

ntau=n_elements(tau)
nrefi=n_elements(refi)
nrefl=n_elements(refl)
nwp=n_elements(wp)

get_params, zenlambda, sp[0,0,0,*,0],partm

par=fltarr(ntau,nrefl,nrefi,nwp,n_elements(partm))
for t=0, ntau-1 do begin
  for rl=0, nrefl-1 do begin
    for ri=0, nrefi-1 do begin
      for w=0, nwp-1 do begin
        get_params, zenlambda, sp[t,rl,ri,*,w],partm
        par[t,rl,ri,w,*]=partm
      endfor
    endfor
  endfor
endfor

dir='/home/leblanc/SSFR3/plots/mix/'

for p=0, n_elements(partm)-1 do begin
fp=dir+'mix_par'+string(p,format='(I02)')
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=30, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=0 & !x.margin=[6,19]
 lbl=strarr(nwp)
 cl=intarr(nwp)

 yr=[min(par[*,*,*,*,p],/nan),max(par[*,*,*,*,p],/nan)]

 plot, tau, par[*,0,0,0,p],/nodata,title='parameter:'+strtrim(p+1,2), xtitle='Optical depth',ytitle='Parameter',yr=yr
 for w=0, nwp-1 do begin
   cl[w]=(w+1)*250/nwp
   lbl[w]=string(wp[w]*100,format='(I3)')
   oplot,tau, par[*,0,0,w,p], color=(w+1)*250/nwp
   oplot,tau, par[*,0,2,w,p],linestyle=2,psym=-1, color=(w+1)*250/nwp
   oplot,tau, par[*,2,0,w,p],linestyle=3,psym=-2, color=(w+1)*250/nwp
   oplot,tau, par[*,2,2,w,p],linestyle=4,psym=-5, color=(w+1)*250/nwp
 endfor

 tx=string(refl[[0,0,2,2]],format='(I2)')+' um liq, '+string(refi[[0,2,0,2]],format='(I2)')+' um ice'
 legend, lbl+'% ice', textcolors=cl,box=0,position=[0.8,0.9],/normal
 legend, tx, linestyle=[0,2,3,4],box=0,/bottom,pspacing=1.5,position=[0.7,0.5],/normal
 
 device, /close
 spawn, 'convert '+fp+'.ps '+fp+'.png'

endfor

stop
end
