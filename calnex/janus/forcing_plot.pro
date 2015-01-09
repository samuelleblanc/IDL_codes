pro forcing_plot
dir='/home/leblanc/CALNEX/p3/20100519/'
restore, dir+'20100519_forcing.out'
;rad_up,no_rad_up,rad_dn, no_rad_dn, aot, wvl,forcing, eff,effp,

wvl=[340,380,440,500,675,870,1020] 

set_plot, 'ps'
  loadct, 39,/silent
  device, /encapsulated
   device, /tt_font, set_font='Helvetica Bold'
   device, filename=dir+'20100519_forcing.ps'
   device,/color,bits_per_pixel=8.
   device, xsize=60, ysize=40
   !p.font=1 & !p.thick=5
   !p.charsize=3.5 & !x.style=1
   !y.style=1 & !z.style=1
   !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,3,2]
  
 ; plot, wvl, forcing[*,0,0], title='Bottom of layer Forcing',ytitle='Forcing (W/m!U 2!N)',xtitle='Wavelength (nm)', yrange=[min(forcing[*,*,0]*0.9,/nan),max(forcing[*,*,0]*1.1,/nan)]
 ; for i=1, n_elements(forcing[0,*,0])-1 do oplot, wvl, forcing[*,i,0]
  
 ; plot, wvl, forcing[*,0,1], title='Top of layer Forcing',ytitle='Forcing (W/m!U 2!N)',xtitle='Wavelength (nm)', yrange=[min(forcing[*,*,1]*0.9,/nan),max(forcing[*,*,1]*1.1,/nan)]
 ; for i=1, n_elements(forcing[0,*,1])-1 do oplot, wvl, forcing[*,i,1]
  
 ; plot, wvl, eff[*,0,0], title='Bottom of layer Forcing',ytitle='Forcing Efficiency (W/m!U 2!N)',xtitle='Wavelength (nm)', yrange=[min(eff[*,*,0]*0.9,/nan),max(eff[*,*,0]*1.1,/nan)]
 ; for i=1, n_elements(eff[0,*,0])-1 do oplot, wvl, eff[*,i,0]
  
 ; plot, wvl, eff[*,0,1], title='Top of layer Forcing',ytitle='Forcing Efficiency (W/m!U 2!N)',xtitle='Wavelength (nm)', yrange=[min(eff[*,*,1]*0.9,/nan),max(eff[*,*,1]*1.1,/nan)]
 ; for i=1, n_elements(eff[0,*,1])-1 do oplot, wvl, eff[*,i,1]
  
  plot, wvl, effp[*,0,1], title='Top of layer forcing',ytitle='Relative forcing efficiency(%)',xtitle='Wavelength (nm)', yrange=[min(effp[*,*,1]*0.9,/nan),max(effp[*,*,1]*1.1,/nan)]
  for i=1, n_elements(effp[0,*,1])-1 do oplot, wvl, effp[*,i,1]
  
  plot, wvl, ssa[*,0], title='Single scattering albedo', xtitle='Wavelength (nm)', yrange=[0,1]
  for i=1, n_elements(ssa[0,*])-1 do oplot, wvl, ssa[*,i]
  
  plot, wvl, asy[*,0], title='Asymmetry parameter', xtitle='Wavelength (nm)', yrange=[0,1]
  for i=1, n_elements(asy[0,*])-1 do oplot, wvl, asy[*,i]
    
  plot, wvl, effp[*,0,0], title='Bottom of layer forcing',ytitle='Relative forcing efficiency(%)',xtitle='Wavelength (nm)', yrange=[min(effp[*,*,0]*0.9,/nan),max(effp[*,*,0]*1.1,/nan)]
  for i=1, n_elements(effp[0,*,0])-1 do oplot, wvl, effp[*,i,0]
    
  plot, wvl, aot[*,0], title='Optical depth', xtitle='Wavelength (nm)', yrange=[0,1]
  for i=1, n_elements(aot[0,*])-1 do oplot, wvl, aot[*,i]
  
  plot, wvl, albedo[*,0], title='Surface albedo', xtitle='Wavelength (nm)', yrange=[0,1]
  for i=1, n_elements(albedo[0,*])-1 do oplot, wvl, albedo[*,i]
  
device, /close
spawn, 'convert '+dir+'20100519_forcing.ps '+dir+'20100519_forcing.png'
spawn, 'rm -f '+dir+'20100519_forcing.ps'

wvlo=wvl
effpo=effp
restore, '/home/leblanc/arctas/nasa/20080709/20080709_forcing.out'
effp_arctas=effp
wvl_arctas=wvl_arr
effp=effpo
wvl=wvlo

restore, '/home/leblanc/CALNEX/forcing_err.out'
eff_err_arctas=fltarr(n_elements(wvl_arctas),2)
eff_err_arctas[*,0]=interpol(eff_err[*,0],wvl,wvl_arctas)
eff_err_arctas[*,1]=interpol(eff_err[*,1],wvl,wvl_arctas)
effp_m_arctas=fltarr(n_elements(wvl_arctas),2)
effp_s_arctas=fltarr(n_elements(wvl_arctas),2)
for i=0, n_elements(wvl_arctas)-1 do begin
  effp_m_arctas[i,0]=mean(effp_arctas[i,8:*,0],/nan)
  effp_m_arctas[i,1]=mean(effp_arctas[i,8:*,1],/nan)
  effp_s_arctas[i,0]=stddev(effp_arctas[i,8:*,0],/nan)
  effp_s_arctas[i,1]=stddev(effp_arctas[i,8:*,1],/nan)
endfor
effp_m=fltarr(n_elements(wvl),2)
effp_s=fltarr(n_elements(wvl),2)
for i=0, n_elements(wvl)-1 do begin
  effp_m[i,0]=mean(effp[i,*,0],/nan)
  effp_m[i,1]=mean(effp[i,*,1],/nan);+3.
  effp_s[i,0]=stddev(effp[i,*,0],/nan)-5.
  effp_s[i,1]=stddev(effp[i,*,1],/nan)-5.
endfor
;if (0) do begin
set_plot, 'ps'
  loadct, 39,/silent
  device, /encapsulated
   device, /tt_font, set_font='Helvetica Bold'
   device, filename=dir+'20100519_effp.ps'
   device,/color,bits_per_pixel=8.
   device, xsize=40, ysize=20
   !p.font=1 & !p.thick=5
   !p.charsize=2.0 & !x.style=1
   !y.style=1 & !z.style=1
   !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,2,1] & !y.omargin=[0,1]
  f=file_search('/home/leblanc/libradtran/output/aero/rtm_taumod_20100519_wvl*.txt')
  f=f[sort(f)]
  wvl0500=read_ascii(f[3], data_start=1)
  lat_cal=34.13694
  lon_cal=-118.12583
  lon=wvl0500.field01[1,*]
  lat=wvl0500.field01[0,*]
  dist_p3=lon
  for j=0, n_elements(lon)-1 do if lon[j] gt lon_cal then dist_p3[j]=map_2points(lon_cal,lat_cal,lon[j],lat[j],/meters)/1000. else dist_p3[j]=map_2points(lon_cal,lat_cal,lon[j],lat[j],/meters)/(-1000.)

  plot, dist_p3, effp[3,*,1],ytitle='Relative forcing efficiency(%)',xmargin=[7,4],$
   xtitle='Distance from Caltech (km)', yrange=[-70,10],/nodata,xstyle=9, xticks=4, xtickformat='(F8.3)'
  axis,xaxis=1,xstyle=1,xrange=[min(lon),max(lon)],xtitle='Longitude (degrees)',/save
  !p.thick=10
  tvlct, 230,230,230,230
  tvlct,210,210,210,210
  t=effp[3,*,1]
  b=effp[3,*,0]
  k=finite(t)
  kl=finite(b)
  kk=where( k eq 1)
  kkl=where(kl eq 1)
  k=where(k eq 1)
  kl=where(kl eq 1)
  kis=indgen(25)
  polyfill,[reform(wvl0500.field01[1,kk[kis]]),reform(wvl0500.field01[1,reverse(kk[kis])])],[t[k[kis]]-eff_err[3,1],t[reverse(k[kis])]+eff_err[3,1]],color=230
  polyfill,[reform(wvl0500.field01[1,kkl[kis]]),reform(wvl0500.field01[1,reverse(kkl[kis])])],[b[kl[kis]]-eff_err[3,0],b[reverse(kl[kis])]+eff_err[3,0]],color=210
  polyfill,[reform(wvl0500.field01[1,kk[kis]]),reform(wvl0500.field01[1,reverse(kk[kis])])],[t[k[kis]]-eff_err[3,1],t[reverse(k[kis])]+eff_err[3,1]],color=230, /line_fill, spacing=0.2
  not_nan=where(finite(effp[3,*,1]) eq 1)
  oplot, wvl0500.field01[1,not_nan], effp[3,not_nan,1],linestyle=2, psym=-1
  oplot, wvl0500.field01[1,not_nan], effp[3,not_nan,0], linestyle=0, psym=-1
xyouts, -118.098, -5, 'Top of Layer'
xyouts, -118.095, -20, 'Bottom of Layer'

  
  
n=52
  ; effp[*,52,1]
  plot, wvl, effp_m[*,1],ytitle='Relative forcing efficiency(%)',$
   xmargin=[7,4],xtitle='Wavelength (nm)', yrange=[-70,10],/nodata
 !p.thick=10
  tvlct, 200,200,200,200
  tvlct, 230,230,230,230
  tvlct, 180,180,255,231
  tvlct, 160,160,255,201

  polyfill, [wvl,reverse(wvl)],[effp_m[*,1]+effp_s[*,1],reverse(effp_m[*,1]-effp_s[*,1])],color=230
  polyfill, [wvl,reverse(wvl)],[effp_m[*,0]+effp_s[*,0],reverse(effp_m[*,0]-effp_s[*,0])],color=200 
  polyfill, [wvl_arctas[0:9],reverse(wvl_arctas[0:9])],[effp_m_arctas[0:9,1]+effp_s_arctas[0:9,1],reverse(effp_m_arctas[0:9,1]-effp_s_arctas[0:9,1])],color=231
  polyfill, [wvl_arctas[0:9],reverse(wvl_arctas[0:9])],[effp_m_arctas[0:9,0]+effp_s_arctas[0:9,0],reverse(effp_m_arctas[0:9,0]-effp_s_arctas[0:9,0])],color=201
  polyfill, [wvl,reverse(wvl)],[effp_m[*,1]+effp_s[*,1],reverse(effp_m[*,1]-effp_s[*,1])],color=230,/line_fill, spacing=0.11,thick=4
 ; jens' data
  openr, 98, '/home/leblanc/CALNEX/redemann_2006/jens.dat'
  je=fltarr(11,5)
  readf, 98, je
  close, 98

  mm=[mean(je[1:*,0]),mean(je[1:*,1]),mean(je[1:*,2]),mean(je[1:*,3])]
  std=[stddev(je[1:*,0]),stddev(je[1:*,1]),stddev(je[1:*,2]),stddev(je[1:*,3])]
  tvlct, 235,179,80,201
  polyfill, [reform([[je[0,0:2]],[1020.]]),reverse(reform([[je[0,0:2]],[1020.]]))], [mm-std,reverse(mm+std)],/line_fill, spacing=0.1, color=201,thick=4.;linestyle=2
  oplot, je[0,*], mm,color=199,psym=-1

  oplot, wvl, effp_m[*,1],linestyle=2,psym=-1
  tvlct, 196,255,175,151


  polyfill, [wvl,reverse(wvl)],[effp_m[*,0]+effp_s[*,0],reverse(effp_m[*,0]-effp_s[*,0])],color=200, /line_fill, spacing=0.2
  oplot, je[0,*], mm,color=199,psym=-1
  
  oplot, wvl, effp_m[*,0], linestyle=0,psym=-1
  
  oplot, wvl_arctas,effp_m_arctas[*,1], linestyle=2, color=50,psym=-1
  oplot, wvl_arctas,effp_m_arctas[*,0], linestyle=0, color=50,psym=-1
  
  tvlct, 175,247,255,71

;legend,['Top of Layer', 'Bottom of Layer'], linestyle=[2,0],pspacing=1.5,/right,/bottom,box=0
; green strong aborption
tvlct, 155,187,89, 72
openr, 98, '/home/leblanc/CALNEX/schmidt_2010/green-fe.dat'
gn=fltarr(4,12)
readf, 98, gn
close,98

oplot, gn[0,*],gn[1,*], linestyle=0, color=72,psym=-1 ;BOL
oplot, gn[0,*],gn[3,*], linestyle=2, color=72,psym=-1 ;TOL

; red aged aerosol layer
tvlct, 192,80,77,73
openr, 98, '/home/leblanc/CALNEX/schmidt_2010/red-fe.dat'
rd=fltarr(5,12)
readf, 98, rd
close,98

oplot, rd[0,*],rd[1,*], linestyle=0, color=73,psym=-1 ;BOL
oplot, rd[0,*],rd[3,*], linestyle=2, color=73,psym=-1 ;TOL

legend,['ARCTAS','CalNex','Fresh aerosols - MILAGRO','Aged aerosols - MILAGRO','INTEX-NA'],textcolors=[50,0,72,73,201], /right,/bottom,box=0
;legend,['ARCTAS','Fresh aerosols - MILAGRO','Aged aerosols - MILAGRO'],textcolors=[50,72,73], /right,/bottom,box=0
device, /close
spawn, 'convert '+dir+'20100519_effp.ps '+dir+'20100519_eff_time_spect.png'
spawn, 'rm -f '+dir+'20100519_effp.ps'

stop
end
