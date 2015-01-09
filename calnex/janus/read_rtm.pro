pro read_rtm


flag_1=0 ; do wavelengths
flag_2=1 ; do tau

dir= '\\lasp-smb\leblanc\CALNEX\p3\20100519\'
;tau_10=read_ascii(dir+'rtm_out_20100519.txt', data_start=1)
;tau_11=read_ascii(dir+'rtm_out_20100519_1.1.txt', data_start=1)
;tau_09=read_ascii(dir+'rtm_out_20100519_0.9.txt', data_start=1)
if flag_2 then begin


  tau=read_ascii(dir+'rtm_out_20100519_tau_mod_wvl1020.txt', data_start=1)
  set_plot, 'ps'
  loadct, 39
  !p.font=1
  !p.thick=5
  !p.charsize=1.8
  !x.style=1
  !y.style=1 
  !z.style=1
  !y.thick=1.8
  !x.thick=1.8
  !p.multi=0
   device, /encapsulated
    device, /tt_font, set_font='Helvetica Bold'
    device, filename=dir+'rtm_20100519_tau_mod_1020.ps'
    device,/color,bits_per_pixel=8.
    device, xsize=30, ysize=20
  
  plot, tau.field1[0,*], tau.field1[1,*], title='Aerosol Retrieval Over CalTech (May 19th) @ 1020nm', yrange=[0.0,1.0], xtitle='longitude (degrees)', ystyle=8, xmargin=[4,8] 
  oplot, tau.field1[0,*],tau.field1[2,*], color=70
  oplot, tau.field1[0,*],tau.field1[3,*], color=150
  oplot, tau.field1[0,*],tau.field1[4,*], color=200
  axis, yaxis=1, ytitle='Tau multiplication factor (from HSRL)', yrange=[0.6,1.4],/save, color=250
  oplot, tau.field1[0,*],tau.field1[6,*], color=250
  
  legend, ['Single Scattering Albedo','Asymmetry','Asymmetry #2','effective surface albedo','tau multiplier'],textcolors=[0,70,150,200,250], position=[-118.075,1.0]
  device, /close
  spawn, 'convert "'+dir+'rtm_20100519_tau_mod_1020.ps" "'+dir+'rtm_20100519_tau_mod_1020.png"'


endif

if flag_1 then begin

w1020=read_ascii(dir+'rtm_out_20100519_tau_mod_wvl1020.txt', data_start=1)
w870=read_ascii(dir+'rtm_out_20100519_tau_mod_wvl0870.txt', data_start=1)
w675=read_ascii(dir+'rtm_out_20100519_tau_mod_wvl0675.txt', data_start=1)
w500=read_ascii(dir+'rtm_out_20100519_tau_mod_wvl0500.txt', data_start=1)
w440=read_ascii(dir+'rtm_out_20100519_tau_mod_wvl0440.txt', data_start=1)
w380=read_ascii(dir+'rtm_out_20100519_tau_mod_wvl0380.txt', data_start=1)
w340=read_ascii(dir+'rtm_out_20100519_tau_mod_wvl0340.txt', data_start=1)

;lon , ssa, asy, asy2, albedo, correction, tau mod
wvl=[1020.,870.,675.,500.,440.,380.,340.]

set_plot, 'ps'
loadct, 39
!p.font=1
!p.thick=5
!p.charsize=1.8
!x.style=1
!y.style=1 
!z.style=1
!y.thick=1.8
!x.thick=1.8
!p.multi=[0,2,2]
 device, /encapsulated
  device, /tt_font, set_font='Helvetica Bold'
  device, filename=dir+'rtm_20100519_wvl_tau_mod.ps'
  device,/color,bits_per_pixel=8.
  device, xsize=60, ysize=40


plot, w1020.field1[0,*], w1020.field1[1,*], title='Single Scattering albedo', yrange=[0.6,1.0], xtitle='longitude (degrees)'
oplot, w870.field1[0,*], w870.field1[1,*], color=30
oplot, w675.field1[0,*], w675.field1[1,*], color=70
oplot, w500.field1[0,*], w500.field1[1,*], color=120
oplot, w440.field1[0,*], w440.field1[1,*], color=180
oplot, w380.field1[0,*], w380.field1[1,*], color=210
oplot, w340.field1[0,*], w340.field1[1,*], color=250

legend, ['1020nm', '870nm','675nm','500nm','440nm','380nm','340nm'], textcolors=[0,30,70,120,180,210,250], /bottom, /right

plot, w1020.field1[0,*], w1020.field1[2,*], title='Asymmetry Parameter', yrange=[0.4,1.0], xtitle='longitude (degrees)'
oplot, w870.field1[0,*], w870.field1[2,*], color=30
oplot, w675.field1[0,*], w675.field1[2,*], color=70
oplot, w500.field1[0,*], w500.field1[2,*], color=120
oplot, w440.field1[0,*], w440.field1[2,*], color=180
oplot, w380.field1[0,*], w380.field1[2,*], color=210
oplot, w340.field1[0,*], w340.field1[2,*], color=250

legend, ['1020nm', '870nm','675nm','500nm','440nm','380nm','340nm'], textcolors=[0,30,70,120,180,210,250], /bottom, /right

for i=0, n_elements(w1020.field1[3,*])-1 do begin
  if w1020.field1[3,i] eq 0.97 or w1020.field1[3,i] eq 0.5 then w1020.field1[3,i]=!values.f_nan
  if w870.field1[3,i] eq 0.97 or w870.field1[3,i] eq 0.5 then w870.field1[3,i]=!values.f_nan
  if w675.field1[3,i] eq 0.97 or w675.field1[3,i] eq 0.5 then w675.field1[3,i]=!values.f_nan
  if w500.field1[3,i] eq 0.97 or w500.field1[3,i] eq 0.5 then w500.field1[3,i]=!values.f_nan
  if w440.field1[3,i] eq 0.97 or w440.field1[3,i] eq 0.5 then w440.field1[3,i]=!values.f_nan
  if w380.field1[3,i] eq 0.97 or w380.field1[3,i] eq 0.5 then w380.field1[3,i]=!values.f_nan
  if w340.field1[3,i] eq 0.97 or w340.field1[3,i] eq 0.5 then w340.field1[3,i]=!values.f_nan
endfor


plot, w1020.field1[0,*], w1020.field1[3,*], title='Asymmetry Parameter Second method', yrange=[0.4,1.0], xtitle='longitude (degrees)'
oplot, w870.field1[0,*], w870.field1[3,*], color=30
oplot, w675.field1[0,*], w675.field1[3,*], color=70
oplot, w500.field1[0,*], w500.field1[3,*], color=120
oplot, w440.field1[0,*], w440.field1[3,*], color=180
oplot, w380.field1[0,*], w380.field1[3,*], color=210
oplot, w340.field1[0,*], w340.field1[3,*], color=250

legend, ['1020nm', '870nm','675nm','500nm','440nm','380nm','340nm'], textcolors=[0,30,70,120,180,210,250], /bottom, /right

plot, w1020.field1[0,*], w1020.field1[6,*], title='Optical thickness correction factor', yrange=[0.6,1.4], xtitle='longitude (degrees)'
oplot, w870.field1[0,*], w870.field1[6,*], color=30
oplot, w675.field1[0,*], w675.field1[6,*], color=70
oplot, w500.field1[0,*], w500.field1[6,*], color=120
oplot, w440.field1[0,*], w440.field1[6,*], color=180
oplot, w380.field1[0,*], w380.field1[6,*], color=210
oplot, w340.field1[0,*], w340.field1[6,*], color=250

legend, ['1020nm', '870nm','675nm','500nm','440nm','380nm','340nm'], textcolors=[0,30,70,120,180,210,250], /top, /right

;plot, tau_10.field1[0,*], tau_10.field1[2,*], title='Asymmetry parameter for original tau', yrange=[0,1.0], xtitle='longitude (degrees)'
;oplot, tau_10.field1[0,*], tau_10.field1[2,*], color= 70
;oplot, tau_10.field1[0,*], tau_10.field1[3,*], color= 250
;legend,['ASY','ASY2'], textcolors=[70,250], /bottom
;
;plot, tau_11.field1[0,*], tau_11.field1[2,*], title='Asymmetry parameter for tau + 10%', yrange=[0,1.0], xtitle='longitude (degrees)'
;oplot, tau_11.field1[0,*], tau_11.field1[2,*], color= 70
;oplot, tau_11.field1[0,*], tau_11.field1[3,*], color= 250
;legend,['ASY','ASY2'], textcolors=[70,250], /bottom
;
;plot, tau_09.field1[0,*], tau_09.field1[2,*], title='Asymmetry parameter for tau - 10%', yrange=[0,1.0], xtitle='longitude (degrees)'
;oplot, tau_09.field1[0,*], tau_09.field1[2,*], color= 70
;oplot, tau_09.field1[0,*], tau_09.field1[3,*], color= 250
;legend,['ASY','ASY2'], textcolors=[70,250], /bottom
;
;plot, tau_09.field1[0,*], tau_10.field1[3,*]-tau_10.field1[2,*], title='Difference in Asymmetry parameter variations', yrange=[-0.5,0.5], xtitle='longitude (degrees)'
;oplot,  tau_09.field1[0,*], tau_10.field1[3,*]-tau_10.field1[2,*], color=70
;oplot,  tau_09.field1[0,*], tau_09.field1[3,*]-tau_09.field1[2,*], color=150
;oplot,  tau_09.field1[0,*], tau_11.field1[3,*]-tau_11.field1[2,*], color=250
;legend,['tau','tau -10%', 'tau +10%'], textcolors=[70,150,250], /bottom, /right
device, /close
spawn, 'convert "'+dir+'rtm_20100519_wvl_tau_mod.ps" "'+dir+'rtm_20100519_wvl_tau_mod.png"'

!p.multi=0
 device, /encapsulated
  device, /tt_font, set_font='Helvetica Bold'
  device, filename=dir+'rtm_20100519_wvl_l.ps'
  device,/color,bits_per_pixel=8.
  device, xsize=20, ysize=20

mm=min(abs(-118.106 - w1020.field1[0,*]), n)
plot, wvl, [w1020.field1[1,n],w870.field1[1,n],w675.field1[1,n],w500.field1[1,n],w440.field1[1,n],w380.field1[1,n],w340.field1[1,n]], title='Aerosol', yrange=[0.0,1.0], xtitle='Wavelength (nm)'
oplot, wvl, [w1020.field1[2,n],w870.field1[2,n],w675.field1[2,n],w500.field1[2,n],w440.field1[2,n],w380.field1[2,n],w340.field1[2,n]], color=70
oplot, wvl, [w1020.field1[3,n],w870.field1[3,n],w675.field1[3,n],w500.field1[3,n],w440.field1[3,n],w380.field1[3,n],w340.field1[3,n]], color=150
oplot, wvl, [w1020.field1[4,n],w870.field1[4,n],w675.field1[4,n],w500.field1[4,n],w440.field1[4,n],w380.field1[4,n],w340.field1[4,n]], color=250
oplot, wvl, [w1020.field1[6,n],w870.field1[6,n],w675.field1[6,n],w500.field1[6,n],w440.field1[6,n],w380.field1[6,n],w340.field1[6,n]], color=200
legend,['SSA','ASY','ASY2','Albedo','Tau multiplier'],textcolors=[0,70,150,250,200], /right
device, /close
spawn, 'convert "'+dir+'rtm_20100519_wvl_l.ps" "'+dir+'rtm_20100519_wvl_l.png"'

endif




end