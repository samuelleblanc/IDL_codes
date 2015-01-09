@errploty.pro
pro arctas_plot
date='20080709'
dir='/home/leblanc/libradtran/output/aero/'


if (1) then begin
; get tau modified
f=file_search(dir+'rtm_angstrom_'+date+'_wvl*.txt')
f=f[sort(f)]
print, f


;dir='/home/leblanc/arctas/nasa/'+date+'/'
wvl0353=read_ascii(f[0], data_start=1)
wvl0380=read_ascii(f[1], data_start=1)
wvl0452=read_ascii(f[2], data_start=1)
wvl0499=read_ascii(f[3], data_start=1)
wvl0519=read_ascii(f[4], data_start=1)
wvl0605=read_ascii(f[5], data_start=1)
wvl0675=read_ascii(f[6], data_start=1)
wvl0779=read_ascii(f[7], data_start=1)
wvl0864=read_ascii(f[8], data_start=1)
wvl1019=read_ascii(f[9], data_start=1)
wvl1241=read_ascii(f[10], data_start=1)
wvl1558=read_ascii(f[11], data_start=1)
wvl2139=read_ascii(f[12], data_start=1)
endif

if (1) then begin
; get non tau modified
f=file_search(dir+'rtm_'+date+'_wvl*.txt')
f=f[sort(f)]

dir='/home/leblanc/arctas/nasa/'+date+'/'
wvl0353_1=read_ascii(f[0], data_start=1)
wvl0380_1=read_ascii(f[1], data_start=1)
wvl0452_1=read_ascii(f[2], data_start=1)
wvl0499_1=read_ascii(f[3], data_start=1)
wvl0519_1=read_ascii(f[4], data_start=1)
wvl0605_1=read_ascii(f[5], data_start=1)
wvl0675_1=read_ascii(f[6], data_start=1)
wvl0779_1=read_ascii(f[7], data_start=1)
wvl0864_1=read_ascii(f[8], data_start=1)
wvl1019_1=read_ascii(f[9], data_start=1)
wvl1241_1=read_ascii(f[10], data_start=1)
wvl1558_1=read_ascii(f[11], data_start=1)
wvl2139_1=read_ascii(f[12], data_start=1)
endif

dir = '/home/leblanc/arctas/nasa/'+date+'/'
restore, '/home/leblanc/arctas/nasa/'+date+'/'+date+'_spectra_seb.out'
;dtau_aats ; errors in tau_aats
if (1) then begin
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated
 device, /tt_font, set_font='Helvetica Bold'
 device, filename=dir+'rtm_tau.ps'
 device,/color,bits_per_pixel=8.
 device, xsize=40, ysize=40
      !p.font=1
      !p.thick=5
      !p.charsize=2.5
      !x.style=0
      !y.style=1 
      !z.style=1
      !y.thick=1.8
      !x.thick=1.8
      !p.multi=0
      t=11
	  
      restore, '/home/leblanc/arctas/nasa/'+date+'/'+date+'_spectra_seb.out'

  plot, wvl0353_1.field01[t,*], wvl0353.field01[t,*],yrange=[0,1.], xrange=[0,1.], psym=2, title='Retrieved tau vs. AATS measured tau for '+date, xtitle='AATS optical thickness', ytitle='Retrieved optical thickness Varied by angstrom method '
	oplot, wvl0380_1.field01[t,*],wvl0380.field01[t,*], color=30, psym=2
	oplot, wvl0452_1.field01[t,*],wvl0452.field01[t,*], color=50, psym=2
	oplot, wvl0499_1.field01[t,*],wvl0499.field01[t,*], color=70, psym=2
	oplot, wvl0519_1.field01[t,*],wvl0519.field01[t,*], color=90, psym=2
	oplot, wvl0605_1.field01[t,*],wvl0605.field01[t,*], color=110, psym=2
	oplot, wvl0675_1.field01[t,*],wvl0675.field01[t,*], color=130, psym=2
	oplot, wvl0779_1.field01[t,*],wvl0779.field01[t,*], color=150, psym=2
	oplot, wvl0864_1.field01[t,*],wvl0864.field01[t,*], color=170, psym=2
	oplot, wvl1019_1.field01[t,*],wvl1019.field01[t,*], color=190, psym=2
	oplot, wvl1241_1.field01[t,*],wvl1241.field01[t,*], color=210, psym=2
	oplot, wvl1558_1.field01[t,*],wvl1558.field01[t,*], color=230, psym=2
	oplot, wvl2139_1.field01[t,*],wvl2139.field01[t,*], color=250, psym=2
    
    ; now put the error bars from aats
    !p.thick=0.2
  errploty, wvl0353.field01[t,*], wvl0353_1.field01[t,*]-dtau_aats[*,0], wvl0353_1.field01[t,*]+dtau_aats[*,0], color=0
  errploty, wvl0380.field01[t,*], wvl0380_1.field01[t,*]-dtau_aats[*,1], wvl0380_1.field01[t,*]+dtau_aats[*,1], color=30
  errploty, wvl0452.field01[t,*], wvl0452_1.field01[t,*]-dtau_aats[*,2], wvl0452_1.field01[t,*]+dtau_aats[*,2], color=50
  errploty, wvl0499.field01[t,*], wvl0499_1.field01[t,*]-dtau_aats[*,3], wvl0499_1.field01[t,*]+dtau_aats[*,3], color=70
  errploty, wvl0519.field01[t,*], wvl0519_1.field01[t,*]-dtau_aats[*,4], wvl0519_1.field01[t,*]+dtau_aats[*,4], color=90
  errploty, wvl0605.field01[t,*], wvl0605_1.field01[t,*]-dtau_aats[*,5], wvl0605_1.field01[t,*]+dtau_aats[*,5], color=110
  errploty, wvl0675.field01[t,*], wvl0675_1.field01[t,*]-dtau_aats[*,6], wvl0675_1.field01[t,*]+dtau_aats[*,6], color=130
  errploty, wvl0779.field01[t,*], wvl0779_1.field01[t,*]-dtau_aats[*,7], wvl0779_1.field01[t,*]+dtau_aats[*,7], color=150
  errploty, wvl0864.field01[t,*], wvl0864_1.field01[t,*]-dtau_aats[*,8], wvl0864_1.field01[t,*]+dtau_aats[*,8], color=170
  errploty, wvl1019.field01[t,*], wvl1019_1.field01[t,*]-dtau_aats[*,9], wvl1019_1.field01[t,*]+dtau_aats[*,9], color=190
  errploty, wvl1241.field01[t,*], wvl1241_1.field01[t,*]-dtau_aats[*,10], wvl1241_1.field01[t,*]+dtau_aats[*,10], color=210
  errploty, wvl1558.field01[t,*], wvl1558_1.field01[t,*]-dtau_aats[*,11], wvl1558_1.field01[t,*]+dtau_aats[*,11], color=230
  errploty, wvl2139.field01[t,*], wvl2139_1.field01[t,*]-dtau_aats[*,12], wvl2139_1.field01[t,*]+dtau_aats[*,12], color=250
      
	plots, [0,1],[0,1], linestyle=2
	
	legend,['353nm','380nm','452nm','499nm','519nm','605nm','675nm','779nm','864nm','1019nm','1241nm','1558nm','2139nm'],textcolors=[0,30,50,70,90,110,130,150,170,190,210,230,250]
;	legend, ['779nm','864nm','1019nm','1241nm','1558nm','2139nm'], textcolors=[150,170,190,210,230,250], /right

device, /close

spawn, 'convert "'+dir+'rtm_tau.ps" "'+dir+'rtm_tau_angstrom.png"'
spawn, 'rm -f "'+dir+'rtm_tau.ps"'
endif

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
      !p.charsize=2.5
      !x.style=0
      !y.style=1 
      !z.style=1
      !y.thick=1.8
      !x.thick=1.8
      !p.multi=[0,2,2]

ind=[2,3,4,5]
name=['SSA','ASY','ASY2','Albedo']
!y.omargin=[2,6]
for i=0,3 do begin
plot, wvl0353.field01[0,*], wvl0353.field01[ind[i],*], title=name[i], xtitle='latitude', yrange=[0,1];, xrange=[57.06,57.08]
oplot, wvl0380.field01[0,*],wvl0380.field01[ind[i],*], color=30
oplot, wvl0452.field01[0,*],wvl0452.field01[ind[i],*], color=50
oplot, wvl0499.field01[0,*],wvl0499.field01[ind[i],*], color=70
oplot, wvl0519.field01[0,*],wvl0519.field01[ind[i],*], color=90
oplot, wvl0605.field01[0,*],wvl0605.field01[ind[i],*], color=110
oplot, wvl0675.field01[0,*],wvl0675.field01[ind[i],*], color=130
oplot, wvl0779.field01[0,*],wvl0779.field01[ind[i],*], color=150
oplot, wvl0864.field01[0,*],wvl0864.field01[ind[i],*], color=170
oplot, wvl1019.field01[0,*],wvl1019.field01[ind[i],*], color=190
oplot, wvl1241.field01[0,*],wvl1241.field01[ind[i],*], color=210
oplot, wvl1558.field01[0,*],wvl1558.field01[ind[i],*], color=230
oplot, wvl2139.field01[0,*],wvl2139.field01[ind[i],*], color=250
endfor

legend,['353nm','380nm','452nm','499nm','519nm','605nm','675nm'],textcolors=[0,30,50,70,90,110,130]
legend, ['779nm','864nm','1019nm','1241nm','1558nm','2139nm'], textcolors=[150,170,190,210,230,250], /right

xyouts, 0.5,0.95, alignment=0.5, /normal, charsize=4.0, 'Aerosol Retrieval Latitudes for ARCTAS '+date
device, /close
spawn, 'convert "'+dir+'rtm_results_time.ps" "'+dir+'rtm_results_time.png"'
spawn, 'rm -f "'+dir+'rtm_results_time.ps"'
endif

if 1 then begin
;#lat  lon     ssa   asy       asy2    albedo          correction    tau modification        flux divergence  model down  model up  tau
ind=[2,3,4,5,7,8]
name=['SSA','Asymmetry parameter','ASY2','Albedo','Tau modification']

;for n=0, n_elements(wvl0353.field01[0,*])-1 do begin
n=0
set_plot, 'ps'
 device, /encapsulated
 device, /tt_font, set_font='Helvetica Bold'
 device, filename=dir+'rtm_results_spc'+strtrim(string(n),2)+'.ps'
 device,/color,bits_per_pixel=8.
 device, xsize=40, ysize=30
 !x.style=1
 print, n_elements(wvl0353.field01[0,*])
 ;stop
 !y.omargin=[2,5]
!p.multi=[0,3,2]
i=0 ;ssa
spectrum=[wvl0353.field01[ind[i],n],wvl0380.field01[ind[i],n],wvl0452.field01[ind[i],n],wvl0499.field01[ind[i],n],wvl0519.field01[ind[i],n],wvl0605.field01[ind[i],n],wvl0675.field01[ind[i],n],wvl0779.field01[ind[i],n],wvl0864.field01[ind[i],n],wvl1019.field01[ind[i],n],wvl1241.field01[ind[i],n],wvl1558.field01[ind[i],n],wvl2139.field01[ind[i],n]]
 wvl=[353,380,452,499,519,605,675,779,864,1019,1241,1558,2139]
plot, wvl, spectrum, title=name[i]+' spectrum for lat: '+strtrim(string(wvl0353.field01[0,n]),2), xtitle='wavelength',xrange=[300,2200], yrange=[0,1]

i=1 ;asy
spectrum=[wvl0353.field01[ind[i],n],wvl0380.field01[ind[i],n],wvl0452.field01[ind[i],n],wvl0499.field01[ind[i],n],wvl0519.field01[ind[i],n],wvl0605.field01[ind[i],n],wvl0675.field01[ind[i],n],wvl0779.field01[ind[i],n],wvl0864.field01[ind[i],n],wvl1019.field01[ind[i],n],wvl1241.field01[ind[i],n],wvl1558.field01[ind[i],n],wvl2139.field01[ind[i],n]]
plot, wvl, spectrum, title=name[i]+' spectrum for lat: '+strtrim(string(wvl0353.field01[0,n]),2), xtitle='wavelength',xrange=[300,2200], yrange=[0,1],/nodata
oplot, wvl, spectrum, color=150

i=2;asy2
spectrum=[wvl0353.field01[ind[i],n],wvl0380.field01[ind[i],n],wvl0452.field01[ind[i],n],wvl0499.field01[ind[i],n],wvl0519.field01[ind[i],n],wvl0605.field01[ind[i],n],wvl0675.field01[ind[i],n],wvl0779.field01[ind[i],n],wvl0864.field01[ind[i],n],wvl1019.field01[ind[i],n],wvl1241.field01[ind[i],n],wvl1558.field01[ind[i],n],wvl2139.field01[ind[i],n]]
oplot, wvl, spectrum, color=70
legend,['ASY','ASY2'],textcolors=[150,70],/right

i=3 ;albedo
spectrum=[wvl0353.field01[ind[i],n],wvl0380.field01[ind[i],n],wvl0452.field01[ind[i],n],wvl0499.field01[ind[i],n],wvl0519.field01[ind[i],n],wvl0605.field01[ind[i],n],wvl0675.field01[ind[i],n],wvl0779.field01[ind[i],n],wvl0864.field01[ind[i],n],wvl1019.field01[ind[i],n],wvl1241.field01[ind[i],n],wvl1558.field01[ind[i],n],wvl2139.field01[ind[i],n]]
plot, wvl, spectrum, title=name[i]+' spectrum for lat: '+strtrim(string(wvl0353.field01[0,n]),2), xtitle='wavelength',xrange=[300,2200], yrange=[0,1]

i=4 ;taumod
spectrum=[wvl0353.field01[ind[i],n],wvl0380.field01[ind[i],n],wvl0452.field01[ind[i],n],wvl0499.field01[ind[i],n],wvl0519.field01[ind[i],n],wvl0605.field01[ind[i],n],wvl0675.field01[ind[i],n],wvl0779.field01[ind[i],n],wvl0864.field01[ind[i],n],wvl1019.field01[ind[i],n],wvl1241.field01[ind[i],n],wvl1558.field01[ind[i],n],wvl2139.field01[ind[i],n]]
plot, wvl, (spectrum-1.0)*100., title=name[i]+' spectrum for lat: '+strtrim(string(wvl0353.field01[0,n]),2), xtitle='wavelength',xrange=[300,2200], yrange=[-18,18], ytitle='% deviation of AATS'
oplot, [min(wvl),max(wvl)], [0,0], linestyle=1
!p.thick=0.5
errplot, wvl, -dtau_aats[n,*]/tau_aats[n,*]*100.,  +dtau_aats[n,*]/tau_aats[n,*]*100.
!p.thick=5
i=5
absorp=[wvl0353.field01[ind[i],n],wvl0380.field01[ind[i],n],wvl0452.field01[ind[i],n],wvl0499.field01[ind[i],n],wvl0519.field01[ind[i],n],wvl0605.field01[ind[i],n],wvl0675.field01[ind[i],n],wvl0779.field01[ind[i],n],wvl0864.field01[ind[i],n],wvl1019.field01[ind[i],n],wvl1241.field01[ind[i],n],wvl1558.field01[ind[i],n],wvl2139.field01[ind[i],n]]
plot, wvl, absorp, title='Flux Divergence (absorption)', xtitle='wavelength', yrange=[0,0.350],ytitle='Irradiance (w/m^2)',xrange=[300,2200]
oplot, wvl, absorp, psym=2

restore, '/home/leblanc/arctas/nasa/'+date+'/'+date+'_spectra_seb.out'
oplot, wvl, -(nabove[*,n]-zabove[*,n])+(nbelow[*,n]-zbelow[*,n]), color=250
legend,['model','measured'],textcolors=[0,250], /right

plot, wvl, zabove[*,n], title='Zenith Irradiance', xtitle='wavelength', ytitle='Irradiance (W/m^2)', yrange=[0,1.7]
oplot, wvl, zbelow[*,n], color=70
oplot, wvl, nabove[*,n], color=0, linestyle=2
oplot, wvl, nbelow[*,n], color=70, linestyle=2
legend,['Zenith','Nadir','Above','Below'],/right,color=[0,0,0,70],linestyle=[0,1,0,0]

xyouts, 0.5,0.95, alignment=0.5, /normal, charsize=4.0, 'Aerosol Retrieval Spectrum for ARCTAS '+date
device, /close
spawn, 'convert "'+dir+'rtm_results_spc'+strtrim(string(n),2)+'.ps" "'+dir+'rtm_results_find_spc'+strtrim(string(n,format='(I03)'),2)+'.png"'
spawn, 'rm -f "'+dir+'rtm_results_spc'+strtrim(string(n),2)+'.ps"
;endfor
endif
end
