@errploty.pro
pro calnex_plot
date='20100519'
dir='/home/leblanc/libradtran/output/aero/'


if (1) then begin
; get tau modified
f=file_search(dir+'rtm_taumod_'+date+'_wvl*.txt')
f=f[sort(f)]
print, f
wvl0340=read_ascii(f[0], data_start=1)
wvl0380=read_ascii(f[1], data_start=1)
wvl0440=read_ascii(f[2], data_start=1)
wvl0500=read_ascii(f[3], data_start=1)
wvl0675=read_ascii(f[4], data_start=1)
wvl0870=read_ascii(f[5], data_start=1)
wvl1020=read_ascii(f[6], data_start=1)
;stop
wvls=[340,380,440,500,675,870,1020]
endif

;get errors in retrievals
error=fltarr(6,13)
openr, 95, '/home/leblanc/arctas/nasa/rtm_error.out'
line=' '
readf, 95, line
readf, 95, error
close,/all
err=fltarr(5,n_elements(wvls[*]))
for n=1,5 do begin
  error[n,*]=error[n,*]*0.01
  err[n-1,*]=interpol(error[n,*],error[0,*],wvls[*])
endfor

dir = '/home/leblanc/CALNEX/p3/'+date+'/'
restore, '/home/leblanc/CALNEX/p3/'+date+'/'+date+'_spectra_save.out'
restore, '/home/leblanc/CALNEX/forcing_err_calnex.out'
if 1 then begin
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated
 device, /tt_font, set_font='Helvetica Bold'
 device, filename=dir+'rtm_results_time.ps'
 device,/color,bits_per_pixel=8.
 device, xsize=40, ysize=40
      !p.font=1
      !p.thick=5
      !p.charsize=2.0
      !x.style=1
      !y.style=1 
      !z.style=1
      !y.thick=1.8
      !x.thick=1.8
      !p.multi=[0,2,2]

ind=[2,3,11,5]
name=['Single scattering albedo','Asymmetry parameter','Optical depth','Albedo']
nn=[0,1,2,4,3]
!y.omargin=[0,4]
rangs=[[0.7,1],[0.55,1],[0.0,0.6],[0.0,0.3]]
;wvls=[340,380,440,500,675,870,1020]
for i=0,3 do begin
plot, wvl0340.field01[1,*],  wvl0340.field01[ind[i],*], title=name[i], xtitle='Longitude (degrees)', yrange=rangs[*,i], xrange=[-118.14,-118.10], xticks=4, xtickformat='(F8.3)'
oplot, wvl0380.field01[1,*], wvl0380.field01[ind[i],*], color=30
oplot, wvl0440.field01[1,*], wvl0440.field01[ind[i],*], color=70
oplot, wvl0500.field01[1,*], wvl0500.field01[ind[i],*], color=110
oplot, wvl0675.field01[1,*], wvl0675.field01[ind[i],*], color=150
oplot, wvl0870.field01[1,*], wvl0870.field01[ind[i],*], color=190
oplot, wvl1020.field01[1,*], wvl1020.field01[ind[i],*], color=230

!p.thick=1.5
errplot, wvl0340.field01[1,*], wvl0340.field01[ind[i],*]*(1-err[nn[i],0]),wvl0340.field01[ind[i],*]*(1+err[nn[i],0]),color=0
errplot, wvl0380.field01[1,*], wvl0380.field01[ind[i],*]*(1-err[nn[i],1]),wvl0380.field01[ind[i],*]*(1+err[nn[i],1]),color=30
errplot, wvl0440.field01[1,*], wvl0440.field01[ind[i],*]*(1-err[nn[i],2]),wvl0440.field01[ind[i],*]*(1+err[nn[i],2]),color=70
errplot, wvl0500.field01[1,*], wvl0500.field01[ind[i],*]*(1-err[nn[i],3]),wvl0500.field01[ind[i],*]*(1+err[nn[i],3]),color=110
errplot, wvl0675.field01[1,*], wvl0675.field01[ind[i],*]*(1-err[nn[i],4]),wvl0675.field01[ind[i],*]*(1+err[nn[i],4]),color=150
errplot, wvl0870.field01[1,*], wvl0870.field01[ind[i],*]*(1-err[nn[i],5]),wvl0870.field01[ind[i],*]*(1+err[nn[i],5]),color=190
errplot, wvl1020.field01[1,*], wvl1020.field01[ind[i],*]*(1-err[nn[i],6]),wvl1020.field01[ind[i],*]*(1+err[nn[i],6]),color=230
!p.thick=5

endfor

legend,strtrim(string(wvls),2)+'nm',textcolors=[0,30,70,110,150,190,230],position=[0.48,0.9],/normal

xyouts, 0.5,0.95, alignment=0.5, /normal, charsize=4.0, 'Aerosol Parameters Retrieval for CalNex '+date
device, /close
spawn, 'convert "'+dir+'rtm_results_time.ps" "'+dir+'rtm_results_time.png"'
spawn, 'rm -f "'+dir+'rtm_results_time.ps"'
endif

nn=[0,1,2,4,3]

if (1) then begin
restore, '/home/leblanc/CALNEX/p3/20100519/20100519_forcing.out'
eff_err=eff_err*100.
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated
 device, /tt_font, set_font='Helvetica Bold'
 device, filename=dir+'rtm_forcing_time.ps'
 device,/color,bits_per_pixel=8.
 device, xsize=20, ysize=20
      !p.font=1 & !p.thick=5
      !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1
      !y.thick=1.8 & !x.thick=1.8
      !p.multi=0
plot, wvl0340.field01[1,*], effp[0,*,0], title='Aerosol direct forcing for CalNex 20100519', xtitle='Longitude (degrees)', ytitle='Relative forcing efficiency (%)',yrange=[-60,5], xrange=[-118.14,-118.10], xticks=4, xtickformat='(F8.3)'
oplot, wvl0380.field01[1,*], effp[1,*,0], color=30
oplot, wvl0440.field01[1,*], effp[2,*,0], color=70
oplot, wvl0500.field01[1,*], effp[3,*,0], color=110
oplot, wvl0675.field01[1,*], effp[4,*,0], color=150
oplot, wvl0870.field01[1,*], effp[5,*,0], color=190
oplot, wvl1020.field01[1,*], effp[6,*,0], color=230

!p.thick=1.5
errplot, wvl0340.field01[1,*], effp[0,*,0]-eff_err[0,0],effp[0,*,0]+eff_err[0,0], color=0
errplot, wvl0380.field01[1,*], effp[1,*,0]-eff_err[1,0],effp[1,*,0]+eff_err[1,0], color=30
errplot, wvl0440.field01[1,*], effp[2,*,0]-eff_err[2,0],effp[2,*,0]+eff_err[2,0], color=70
errplot, wvl0500.field01[1,*], effp[3,*,0]-eff_err[3,0],effp[3,*,0]+eff_err[3,0], color=110
errplot, wvl0675.field01[1,*], effp[4,*,0]-eff_err[4,0],effp[4,*,0]+eff_err[4,0], color=150
errplot, wvl0870.field01[1,*], effp[5,*,0]-eff_err[5,0],effp[5,*,0]+eff_err[5,0], color=190
errplot, wvl1020.field01[1,*], effp[6,*,0]-eff_err[6,0],effp[6,*,0]+eff_err[6,0], color=230
!p.thick=5

oplot, wvl0340.field01[1,*], effp[0,*,1], linestyle=2
oplot, wvl0380.field01[1,*], effp[1,*,1], color=30, linestyle=2
oplot, wvl0440.field01[1,*], effp[2,*,1], color=70, linestyle=2
oplot, wvl0500.field01[1,*], effp[3,*,1], color=110, linestyle=2
oplot, wvl0675.field01[1,*], effp[4,*,1], color=150, linestyle=2
oplot, wvl0870.field01[1,*], effp[5,*,1], color=190, linestyle=2
oplot, wvl1020.field01[1,*], effp[6,*,1], color=230, linestyle=2

!p.thick=1.5
errplot, wvl0340.field01[1,*], effp[0,*,1]-eff_err[0,1],effp[0,*,1]+eff_err[0,1], color=0
errplot, wvl0380.field01[1,*], effp[1,*,1]-eff_err[1,1],effp[1,*,1]+eff_err[1,1], color=30
errplot, wvl0440.field01[1,*], effp[2,*,1]-eff_err[2,1],effp[2,*,1]+eff_err[2,1], color=70
errplot, wvl0500.field01[1,*], effp[3,*,1]-eff_err[3,1],effp[3,*,1]+eff_err[3,1], color=110
errplot, wvl0675.field01[1,*], effp[4,*,1]-eff_err[4,1],effp[4,*,1]+eff_err[4,1], color=150
errplot, wvl0870.field01[1,*], effp[5,*,1]-eff_err[5,1],effp[5,*,1]+eff_err[5,1], color=190
errplot, wvl1020.field01[1,*], effp[6,*,1]-eff_err[6,1],effp[6,*,1]+eff_err[6,1], color=230
!p.thick=5

Legend, ['Top of layer', 'Bottom of layer'], linestyle=[2,0], color=[0,0],/right,/bottom
legend,strtrim(string(wvls),2)+'nm',textcolors=[0,30,70,110,150,190,230], box=0, margin=0 ,position=[0.22,0.38],/normal
device, /close
spawn, 'convert "'+dir+'rtm_forcing_time.ps" "'+dir+'rtm_forcing_time.png"'
spawn, 'rm -f "'+dir+'rtm_forcing_time.ps"'
endif

if 0 then begin
set_plot, 'ps'
  loadct, 39,/silent
  device, /encapsulated
   device, /tt_font, set_font='Helvetica Bold'
   device, filename=dir+'20100519_effp_time.ps'
   device,/color,bits_per_pixel=8.
   device, xsize=20, ysize=20
   !p.font=1 & !p.thick=5
   !p.charsize=2.0 & !x.style=1
   !y.style=1 & !z.style=1
   !y.thick=1.8 & !x.thick=1.8
  !p.multi=0 & !y.omargin=[0,0]

  plot, wvl0500.field01[1,*], effp[3,*,1], title='Aerosol direct forcing at 500 nm',ytitle='Relative forcing efficiency(%)',xmargin=[7,4],$
   xtitle='Longitude (degrees)', yrange=[-70,5],/nodata, xticks=4, xtickformat='(F8.3)',xrange=[-118.14,-118.10]
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
kis=indgen(16)+5
  polyfill,[reform(wvl0500.field01[1,kk[kis]]),reform(wvl0500.field01[1,reverse(kk[kis])])],[t[k[kis]]-eff_err[3,1],t[reverse(k[kis])]+eff_err[3,1]],color=230
  polyfill,[reform(wvl0500.field01[1,kkl[kis]]),reform(wvl0500.field01[1,reverse(kkl[kis])])],[b[kl[kis]]-eff_err[3,0],b[reverse(kl[kis])]+eff_err[3,0]],color=210
  polyfill,[reform(wvl0500.field01[1,kk[kis]]),reform(wvl0500.field01[1,reverse(kk[kis])])],[t[k[kis]]-eff_err[3,1],t[reverse(k[kis])]+eff_err[3,1]],color=230, /line_fill, spacing=0.2
;errplot, wvl0500.field01[1,*], effp[3,*,1]-eff_err[3,1],effp[3,*,1]+eff_err[3,1],color=230
;errplot, wvl0500.field01[1,*], effp[3,*,0]-eff_err[3,0],effp[3,*,0]+eff_err[3,0],color=210
;ln=wvl0500.field01[1,*]
  oplot, wvl0500.field01[1,*], effp[3,*,1],linestyle=2, psym=-1
;  errplot, wvl0500.field01[1,*], effp[3,*,1]-eff_err[3,1],effp[3,*,1]+eff_err[3,1]

  oplot, wvl0500.field01[1,*], effp[3,*,0], linestyle=0, psym=-1
;  errplot, wvl0500.field01[1,*], effp[3,*,0]-eff_err[3,0],effp[3,*,0]+eff_err[3,0]
;legend,['Top of Layer', 'Bottom of Layer'], linestyle=[2,0], box=0, pspacing=1.5,/bottom,/right
xyouts, -118.130, -15, 'Top of Layer'
xyouts, -118.120, -49, 'Bottom of Layer'

device, /close
spawn, 'convert '+dir+'20100519_effp_time.ps '+dir+'20100519_effp_time.png'
spawn, 'rm -f '+dir+'20100519_effp_time.ps
endif
stop

   !p.font=1 & !p.thick=5
   !p.charsize=2.0 & !x.style=1
   !y.style=1 & !z.style=1
   !y.thick=1.8 & !x.thick=1.8
  !p.multi=0
  loadct, 39,/silent
  
if 0 then begin ; plotting of forcing for only calnex at n
n=52
set_plot, 'ps'

  device, /encapsulated
   device, /tt_font, set_font='Helvetica Bold'
   device, filename=dir+'20100519_effp.ps'
   device,/color,bits_per_pixel=8.
   device, xsize=20, ysize=20
   !p.font=1 & !p.thick=5
   !p.charsize=2.0 & !x.style=1
   !y.style=1 & !z.style=1
   !y.thick=1.8 & !x.thick=1.8
  !p.multi=0
n=52
  ; effp[*,52,1]
  eff_err=eff_err*100.
  plot, wvls, effp[*,n,1], title='Aerosol direct forcing spectra',ytitle='Relative forcing efficiency(%)',$
   xmargin=[7,4],xtitle='Wavelength (nm)', yrange=[-50,5]
 !p.thick=10
  tvlct, 200,200,200,200
  tvlct, 230,230,230,230
  tvlct, 180,180,255,231
  tvlct, 160,160,255,201
  
  polyfill, [wvls,reverse(wvls)],[effp[*,n,1]+eff_err[*,1],reverse(effp[*,n,1]-eff_err[*,1])],color=230
  polyfill, [wvls,reverse(wvls)],[effp[*,n,0]+eff_err[*,0],reverse(effp[*,n,0]-eff_err[*,0])],color=200
  polyfill, [wvls,reverse(wvls)],[effp[*,n,1]+eff_err[*,1],reverse(effp[*,n,1]-eff_err[*,1])],color=230,/line_fill,spacing=0.1,thick=4.
  oplot, wvls, effp[*,n,1],linestyle=2
  oplot, wvls, effp[*,n,0],linestyle=0 

  legend,['Top of Layer', 'Bottom of Layer'], linestyle=[2,0], box=0, pspacing=1.5,/bottom,/right
  device, /close
  spawn, 'convert '+dir+'20100519_effp.ps '+dir+'20100519_effp.png'
  spawn, 'rm -f '+dir+'20100519_effp.ps'
endif



 ;#lat  lon     ssa   asy       asy2    albedo          correction    tau modification  


crd=0

if 1 then begin
;#lat  lon     ssa   asy       asy2    albedo          correction    tau modification        flux divergence  model down  model up  tau
ind=[2,3,4,5,11,8]
name=['Single scattering albedo','Asymmetry parameter','ASY2','Albedo','Optical depth']
wvl=[340,380,440,500,675,870,1020]

; plot the time trace (at 500nm) on a single plot, similarly to the wavelength trace
set_plot, 'ps'
 device, /encapsulated
 device, /tt_font, set_font='Helvetica Bold'
 device, filename=dir+'rtm_results_spc'+'for_time.ps'
 device,/color,bits_per_pixel=8.
 device, xsize=60, ysize=20
 !x.style=1
 !p.charsize=3.8
 !y.omargin=[0,3]
!p.multi=[0,3,1]
nn=[0,1,2,3,4,5]
i=0 ;ssa

spectrum=wvl0380.field01[ind[i],*]
lon=wvl0380.field01[1,*]
lat=wvl0380.field01[0,*]
lat_cal=34.13694
lon_cal=-118.12583
dist_p3=lon
for j=0, n_elements(lon)-1 do if lon[j] gt lon_cal then dist_p3[j]=map_2points(lon_cal,lat_cal,lon[j],lat[j],/meters)/1000. else dist_p3[j]=map_2points(lon_cal,lat_cal,lon[j],lat[j],/meters)/(-1000.)
stop
if crd then begin
  title_top='Along flight-path asymmetry parameter at 380 nm'
  nodata=1
  sty=1
endif else begin
  title_top='Along flight-path-retrieved properties at 380 nm'
  nodata=0
  sty=8
endelse
title_top=''
plot, dist_p3, spectrum, title=title_top, xtitle='Distance from Caltech (km)', yrange=[0,1], ystyle=sty,$
 xmargin=[8,4], xticks=5, xtickformat='(F8.3)',nodata=nodata, psym=-1,xstyle=9;, xrange=[-118.14,-118.10]
 tvlct, 230,230,230,230
if not crd then errplot, lon, spectrum*(1-err[nn[i],3]), spectrum*(1+err[nn[i],3]),color=230, thick=2.5
tvlct, 21,205,0,150
xyouts, 0.03,0.505,'g', orientation=90, /normal, color=150
if not crd then begin
  xyouts, 0.03,0.4,'!9a!4',orientation=90, /normal, color=70
  xyouts, 0.03,0.45, '!9'+string(118B)+'!4',orientation=90,/normal
endif

i=1 ;asy
spectrum=wvl0380.field01[ind[i],*]
oplot, dist_p3, spectrum, color=150, psym=-1
tvlct, 211,255,190,151
errplot, dist_p3, spectrum*(1-err[nn[i],3]), spectrum*(1+err[nn[i],3]), color=151, thick=2.5

; get met file
f=file_search('/home/leblanc/CALNEX/p3/'+date+'/*'+date+'*.ict.met', count=ct)
if ct lt 1 then message, 'met file not found!'
length=[0.,0.]
openu, lun, f[0], /get_lun
readf, lun, length
free_lun, lun
close, /all
met=read_ascii(f[0],data_start= length[0])
temp=met.field01[1,*] +273.15 ;temperature (Kelvin)
RH=met.field01[6,*]    ;Relative humidity (%)


if not crd then begin
  i=3 ;albedo
  spectrum=wvl0380.field01[ind[i],*]
  oplot, dist_p3, spectrum, color=70, psym=-1
  tvlct, 150,222,255,71
  errplot, dist_p3, spectrum*(1-err[nn[i],3])*0.9, spectrum*(1+err[nn[i],3])*1.1,color=71, thick=2.5
  
  i=4 ;taumod
  axis, yaxis=1,ystyle=1, yrange=[0.0,1.0],/save,color=250
  xyouts, 0.34,0.45,'!9t!4',orientation=90, /normal, color=250
  spectrum=wvl0380.field01[ind[i],*]
  oplot, dist_p3, spectrum,color=250,psym=-1
  tvlct,255,180,180,251
  errplot, dist_p3, spectrum*(1-err[nn[i],3]), spectrum*(1+err[nn[i],3]),color=251, thick=2.5
endif else begin
  restore, '/home/leblanc/rtm_aero/size/mie_output_caltech.out'
 ; restore, '/home/leblanc/CALNEX/aerosol_size/20100519/asy_380.out'
  oplot, dist_p3, asy[i_below[*],1], color=150, linestyle=2
  xyouts, dist_p3[35], asy[i_below[35],1]-0.05, 'Mie scattering calculations', color=150, charsize=1.8
  xyouts, dist_p3[35], spectrum[35]+0.05, 'Retrieved', color=150, charsize=1.8
 ; axis, yaxis=1,ystyle=1, yrange=[0.0,100.],/save, ytitle='Relative Humidity (%)'
 ; oplot, lon, RH[i_above[0]:i_above[74]]
 ; oplot, lon, RH[i_below[0]:i_below[74]], linestyle=2
endelse

xyouts, 0.26,0.25,'380 nm',/normal,charsize=2.0
axis,xaxis=1,xstyle=1,xrange=[min(lon),max(lon)],xtitle='Longitude (degrees)'

; plot again for different wavelength
if crd then begin
  title_top='Along flight-path asymmetry parameter at 500 nm'
  nodata=1
endif else begin
  title_top='Along flight-path-retrieved properties at 500 nm'
  nodata=0
endelse
title_top=''
i=0 ;ssa
spectrum=wvl0500.field01[ind[i],*]
lon=wvl0500.field01[1,*]
plot, dist_p3, spectrum, title=title_top, xtitle='Distance from Caltech (km)', yrange=[0,1], ystyle=sty,$
 xmargin=[8,4], xticks=5, xtickformat='(F8.3)',nodata=nodata, psym=-1;, xrange=[-118.14,-118.10]
 tvlct, 230,230,230,230
if not crd then errplot, lon, spectrum*(1-err[nn[i],3]), spectrum*(1+err[nn[i],3]),color=230, thick=2.5
tvlct, 21,205,0,150
xyouts, 0.36,0.505,'g', orientation=90, /normal, color=150
if not crd then begin
    xyouts, 0.36,0.4,'!9a!4',orientation=90, /normal, color=70
    xyouts, 0.36,0.45, '!9'+string(118B)+'!4',orientation=90,/normal
endif

i=1 ;asy
spectrum=[wvl0340.field01[ind[i],n],wvl0380.field01[ind[i],n],wvl0440.field01[ind[i],n],wvl0500.field01[ind[i],n],wvl0675.field01[ind[i],n],wvl0870.field01[ind[i],n],wvl1020.field01[ind[i],n]]
spectrum=wvl0500.field01[ind[i],*]
oplot, dist_p3, spectrum, color=150,psym=-1
tvlct, 211,255,190,151
errplot, dist_p3, spectrum*(1-err[nn[i],3]), spectrum*(1+err[nn[i],3]), color=151, thick=2.5

if crd then begin
 restore, '/home/leblanc/rtm_aero/size/mie_output_caltech.out'
 ;restore, '/home/leblanc/CALNEX/aerosol_size/20100519/asy_500.out'
  oplot, dist_p3, asy[i_below[0]:i_below[74],3], color=150, linestyle=2
  xyouts, dist_p3[55], asy[i_below[55],3]-0.05, 'Mie scattering calculations', color=150, charsize=1.8
  xyouts, dist_p3[55], spectrum[55]+0.05, 'Retrieved', color=150, charsize=1.8
  filen='rtm_asy_time.png'
  ;axis, yaxis=1,ystyle=1, yrange=[0.0,100.],/save, ytitle='Relative Humidity (%)'
  ;oplot, lon, RH[i_above[0]:i_above[74]]
  ;oplot, lon, RH[i_below[0]:i_below[74]], linestyle=2

  his_asy=histogram(asy[i_below[0]:i_below[74],3],nbins=50,max=1.0,min=0)
  his_rasy=histogram(spectrum, nbins=50,max=1.0,min=0)
  plot, findgen(50)*0.02-0.01, his_rasy/total(his_rasy), title='Distribution of asymmetry parameter',ytitle='Normalized counts', xtitle='Asymmetry parameter',$
    xrange=[0.4,1.0],psym=10, yrange=[0.01,1],/ylog
  oplot, findgen(50)*0.02-0.01, his_asy/total(his_asy),psym=10,color=250
  legend, ['Retrieved','Mie scattering calculations'],/right,textcolors=[0,250],box=0,charsize=1.8
endif else begin
  i=3 ;albedo
  spectrum=wvl0500.field01[ind[i],*]
  oplot, dist_p3, spectrum, color=70,psym=-1
  tvlct, 150,222,255,71
  errplot, dist_p3, spectrum*(1-err[nn[i],3])*0.9, spectrum*(1+err[nn[i],3])*1.1,color=71, thick=2.5
  
  i=4 ;taumod
  axis, yaxis=1,ystyle=1, yrange=[0.0,1.0],/save,color=250
  xyouts, 0.68,0.45,'!9t!4',orientation=90, /normal, color=250
  spectrum=wvl0500.field01[ind[i],*]
  oplot, dist_p3, spectrum,color=250,psym=-1
  tvlct,255,180,180,251
  errplot, dist_p3, spectrum*(1-err[nn[i],3]), spectrum*(1+err[nn[i],3]),color=251, thick=2.5
  filen='rtm_for_time.png'
endelse

xyouts, 0.59,0.25,'500 nm',/normal,charsize=2.0
axis,xaxis=1,xstyle=1,xrange=[min(lon),max(lon)],xtitle='Longitude (degrees)'

device, /close
spawn, 'convert '+dir+'rtm_results_spc'+'for_time.ps '+dir+filen
spawn, 'rm -f '+dir+'rtm_results_spc'+'for_time.ps'
stop

for n=0, n_elements(wvl0340.field01[0,*])-1 do begin
;n=45
nn=[0,1,2,3,4]
set_plot, 'ps'
 device, /encapsulated
 device, /tt_font, set_font='Helvetica Bold'
 device, filename=dir+'rtm_results_spc'+strtrim(string(n),2)+'.ps'
 device,/color,bits_per_pixel=8.
 device, xsize=60, ysize=20
 !x.style=1
 !p.charsize=3.8
tvlct, 210,210,210,210 
print, n, n_elements(wvl0340.field01[0,*])
 ;stop
 !y.omargin=[0,3]
!p.multi=[0,3,1]
i=0 ;ssa
wvl=[340,380,440,500,675,870,1020]
error=err
spectrum=[wvl0340.field01[ind[i],n],wvl0380.field01[ind[i],n],wvl0440.field01[ind[i],n],wvl0500.field01[ind[i],n],wvl0675.field01[ind[i],n],wvl0870.field01[ind[i],n],wvl1020.field01[ind[i],n]]
plot, wvl, spectrum, title='Retrieved properties spectra', xtitle='Wavelength (nm)',xrange=[340,1020], yrange=[0,1], ystyle=8, xmargin=[8,4]
;errplot, wvl, spectrum*(1-err[nn[i],*]), spectrum*(1+err[nn[i],*])
;stop
sst=spectrum*(1-error[nn[i],*])
k=where(sst le 0.0,op)
if op gt 0 then sst[k]=0.
sstt=spectrum*(1+error[nn[i],*])
k=where(sstt ge 1.0,op)
if op gt 0 then sstt[k]=1.0
polyfill, [wvl,reverse(wvl)],[sst,reverse(sstt)],color=210;, /line_fill, spacing=0.13
oplot, wvl, spectrum
sssa=spectrum
ssta=sst & sstta=sstt

tvlct, 21,205,0,150
xyouts, 0.02,0.505,'g', orientation=90, /normal, color=150
xyouts, 0.02,0.4,'!9a!4',orientation=90, /normal, color=70
xyouts, 0.02,0.45, '!9'+string(118B)+'!4',orientation=90,/normal
i=1 ;asy

spectrum=[wvl0340.field01[ind[i],n],wvl0380.field01[ind[i],n],wvl0440.field01[ind[i],n],wvl0500.field01[ind[i],n],wvl0675.field01[ind[i],n],wvl0870.field01[ind[i],n],wvl1020.field01[ind[i],n]]
;oplot, wvl, spectrum, title=name[i]+' spectrum', xtitle='wavelength (nm)',xrange=[300,1020], yrange=[0,1];,/nodata
sst=spectrum*(1-error[nn[i],*])
k=where(sst le 0.0,op)
if op gt 0 then sst[k]=0.
sstt=spectrum*(1+error[nn[i],*])
k=where(sstt ge 1.0,op)
if op gt 0 then sstt[k]=1.0
kf=where(finite(spectrum) eq 0, ofk)
tvlct, 211,255,190,151
if (ofk) le 0 then begin
  polyfill, [wvl,reverse(wvl)],[sst,reverse(sstt)],color=151; , /line_fill, spacing=0.12
endif else begin
kfp=where(finite(spectrum) ne 0, ofp)
if ofp le 0 then kfp=[1]
wvl_f=wvl[kfp]
sst_f=sst[kfp]
sstt_f=sstt[kfp]
polyfill, [wvl_f,reverse(wvl_f)],[sst_f,reverse(sstt_f)],color=151;, /line_fill, spacing=0.12
endelse
oplot, wvl, spectrum, color=150
;errplot, wvl, spectrum*(1-err[nn[i],*]), spectrum*(1+err[nn[i],*]), color=150
;i=2;asy2

;spectrum=[wvl0340.field01[ind[i],n],wvl0380.field01[ind[i],n],wvl0440.field01[ind[i],n],wvl0500.field01[ind[i],n],wvl0675.field01[ind[i],n],wvl0870.field01[ind[i],n],wvl1020.field01[ind[i],n]]
;oplot, wvl, spectrum, color=70
;errplot, wvl, spectrum*(1-err[nn[i],*]), spectrum*(1+err[nn[i],*]),color=70
;legend,['ASY','ASY2'],textcolors=[150,70],/right

i=3 ;albedo

spectrum=[wvl0340.field01[ind[i],n],wvl0380.field01[ind[i],n],wvl0440.field01[ind[i],n],wvl0500.field01[ind[i],n],wvl0675.field01[ind[i],n],wvl0870.field01[ind[i],n],wvl1020.field01[ind[i],n]]
;plot, wvl, spectrum, title=name[i]+' spectrum', xtitle='wavelength (nm)',xrange=[300,1020], yrange=[0,1]
sst=spectrum*(1-error[nn[i],*])*0.9
k=where(sst le 0.0,op)
if op gt 0 then sst[k]=0.
sstt=spectrum*(1+error[nn[i],*])*1.1
k=where(sstt ge 1.0,op)
if op gt 0 then sstt[k]=1.0
kf=where(finite(spectrum) eq 0, ofk)
tvlct, 150,222,255,71
if (ofk) le 0 then begin
  polyfill, [wvl,reverse(wvl)],[sst,reverse(sstt)],color=71 ;, /line_fill, spacing=0.11
endif else begin
kfp=where(finite(spectrum) ne 0, ofp)
if ofp le 0 then kfp=[1]
wvl_f=wvl[kfp]
sst_f=sst[kfp]
sstt_f=sstt[kfp]
polyfill, [wvl_f,reverse(wvl_f)],[sst_f,reverse(sstt_f)],color=71;, /line_fill, spacing=0.11
endelse
oplot, wvl, spectrum, color=70
;errplot, wvl, spectrum*(1-err[nn[i],*]), spectrum*(1+err[nn[i],*]),color=70
i=4 ;taumod

;plotting mie scattering code output.
;restore, '/home/leblanc/CALNEX/p3/'+date+'/'+date+'_spectra_save.out'
;restore, '/home/leblanc/rtm_aero/size/mie_output_cst_m.out'
;wvl=[380,440,500,675,870,1020]
;oplot, wvl, asy[i_above[n],*], color=150, linestyle=2
;oplot, wvl, ssa[i_above[n],*], linestyle=2
;oplot, wvl, asy[i_below[n],*], color=150, linestyle=4
;oplot, wvl, ssa[i_below[n],*], linestyle=4

spectrum=[wvl0340.field01[ind[i],n],wvl0380.field01[ind[i],n],wvl0440.field01[ind[i],n],wvl0500.field01[ind[i],n],wvl0675.field01[ind[i],n],wvl0870.field01[ind[i],n],wvl1020.field01[ind[i],n]]
;plot, wvl, (spectrum-1.0)*100., title=name[i]+' spectrum', xtitle='wavelength (nm)',xrange=[300,1020], yrange=[-20,20], ytitle='% deviation of initial guess'
;oplot, [min(wvl),max(wvl)], [0,0], linestyle=1
axis, yaxis=1,ystyle=1, yrange=[0.0,1.0],/save,color=250
xyouts, 0.34,0.45, '!9t!4',orientation=90,/normal, color=250
sst=spectrum*(1.-error[nn[i],*])
k=where(sst le 0.0,op)
if op gt 0 then sst[k]=0.
sstt=spectrum*(1.+error[nn[i],*])
k=where(sstt ge 1.0,op)
if op gt 0 then sstt[k]=1.0
kf=where(finite(spectrum) eq 0, ofk)
tvlct,255,180,180,251
if (ofk) le 0 then begin
  polyfill, [wvl,reverse(wvl)],[sst,reverse(sstt)],color=251, /line_fill, spacing=0.1
endif else begin
kfp=where(finite(spectrum) ne 0, ofp)
if ofp le 0 then kfp=[1]
wvl_f=wvl[kfp]
sst_f=sst[kfp]
sstt_f=sstt[kfp]
polyfill, [wvl_f,reverse(wvl_f)],[sst_f,reverse(sstt_f)],color=251, /line_fill, spacing=0.1
endelse
oplot, wvl, spectrum,color=250
;errplot, wvl, spectrum*(1-err[nn[i],*]), spectrum*(1+err[nn[i],*]),color=250
;stop

;legend,name[[0,1,4,3]],textcolors=[0,150,250,70],box=0,charsize=1.8,/bottom


i=5
spectrum=[wvl0340.field01[ind[i],n],wvl0380.field01[ind[i],n],wvl0440.field01[ind[i],n],wvl0500.field01[ind[i],n],wvl0675.field01[ind[i],n],wvl0870.field01[ind[i],n],wvl1020.field01[ind[i],n]]
ts=6
s_corr=[wvl0340.field01[ts,n],wvl0380.field01[ts,n],wvl0440.field01[ts,n],wvl0500.field01[ts,n],wvl0675.field01[ts,n],wvl0870.field01[ts,n],wvl1020.field01[ts,n]]
spectrum=spectrum/s_corr
plot, wvl, spectrum, linestyle=2, title='Flux divergence (absorption)', xtitle='Wavelength (nm)', yrange=[0,0.15],ytitle='Spectral irradiance (W/m!U2!N/nm)',xrange=[300,1020]
oplot, wvl, spectrum, psym=2
wvlo=wvl
restore, '/home/leblanc/CALNEX/p3/'+date+'/'+date+'_spectra_save.out'
oplot, wvl, -(nabove[*,n]-zabove[*,n])+(nbelow[*,n]-zbelow[*,n]), color=250
legend,['Model','Measured'],textcolors=[0,250],box=0,charsize=1.8
oplot, wvlo, spectrum, psym=2
plot, wvl, zabove[*,n], title='Measured spectral irradiance', xtitle='Wavelength (nm)', ytitle='Spectral irradiance (W/m!U2!N/nm)', yrange=[0,2.0]
oplot, wvl, zbelow[*,n], color=250
oplot, wvl, nabove[*,n], color=0, linestyle=2
oplot, wvl, nbelow[*,n], color=250, linestyle=2
;legend,['Zenith','Nadir','Above','Below'],/right,color=[0,0,0,250],linestyle=[0,1,0,0],box=0,charsize=1.8

legend, ['Downwelling','Upwelling'], color=[0,0],linestyle=[0,1],pspacing=1.5,corners=c1,/right,position=[0.9,0.75],box=0,/normal,charsize=1.8
legend,['|','|'], color=[250,250],box=0,/right,pspacing=1.5,linestyle=[0,1],/normal,position=[c1[1]+0.262,c1[3]],charsize=1.8
xyouts,0.9,0.75,/normal,'Top | Bottom',alignment=0.485,charsize=1.8

xyouts, 0.5,0.96, alignment=0.5, /normal, charsize=3.0, 'Aerosol retrieval spectrum for CalNex !C'+date+' at Longitude: '+strtrim(string(wvl0340.field01[1,n]),2)
device, /close
spawn, 'convert "'+dir+'rtm_results_spc'+strtrim(string(n),2)+'.ps" "'+dir+'rtm_results_find_spc'+strtrim(string(n,format='(I03)'),2)+'.png"'
spawn, 'rm -f "'+dir+'rtm_results_spc'+strtrim(string(n),2)+'.ps"
endfor
endif
end
