function Efx, vx
  wvl=[353.500,380.000,451.200,499.400,520.400,605.800,675.100,779.100,864.500,1019.10,1241.30,1558.50,2139.10] 
  EF=[-0.185395,    -0.205641,    -0.380975,    -0.303924,    -0.281239,    -0.220054,    -0.158783,   -0.0853397,   -0.0614203,   -0.0304531,   -0.0167818,  -0.00974437,  -0.00368228]
  result=interpol(EF,wvl,vx)
  return,result
end

function Efxtop, vx
  wvl=[353.500,380.000,451.200,499.400,520.400,605.800,675.100,779.100,864.500,1019.10,1241.30,1558.50,2139.10]
  EF=[-0.0348384,   -0.0685065,    -0.178845,    -0.153465,    -0.146420,    -0.110961,   -0.0902129,  -0.00929370,  -0.00606290,  -0.00484055,  -0.00103194,  -0.00212778,  -0.00188500]
  result=interpol(EF,wvl,vx)
  return,result
end

function efr, vx
wvl=[340,380,440,500,675,870,1020] 
f=[0.0703569   , 0.0726701 ,   0.0702656   , 0.0765565  ,  0.0542803   , 0.0401374,0.0349099]
  result=interpol(f,wvl,vx)
  return,result
end

pro forcing_plot
date='20080709'
dir='/home/leblanc/arctas/nasa/'+date+'/'
restore, dir+date+'_forcing_abs.out'
;rad_up,no_rad_up,rad_dn, no_rad_dn, aot, wvl,forcing, eff,effp,

wvl=wvl_arr

ef=fltarr(n_elements(wvl),2)
;average the eff over sza
for v=0, n_elements(wvl)-1 do begin
  ef[v,0]=mean(eff[v,*,0],/nan)
  ef[v,1]=mean(eff[v,*,1],/nan)
endfor

  Fi1=qromb('Efx',350.,700.)
  fir1=qromb('efr',350.,700.)
  Fi_top1=qromb('Efxtop',350.,700.)

print, Fi1/1.8, Fi_top1/1.8 ,Fi1/1.8*fir1/100.

  Fi2=qromb('Efx',400.,700.)
  Fi_top2=qromb('Efxtop',400.,700.)
  fir2=qromb('efr',400.,700.)

print, Fi2/1.8, Fi_top2/1.8,Fi2/1.8*fir2/100.

  Fi3=qromb('Efx',350.,1000.)
  Fi_top3=qromb('Efxtop',350.,1000.)
  fir3=qromb('efr',350.,1000.)

print, Fi3/1.8, Fi_top3/1.8,Fi3/1.8*fir3/100.

  Fi4=qromb('Efx',350.,3810.)
  Fi_top4=qromb('Efxtop',350.,3810.)
  fir4=qromb('efr',350.,3810.)

print, Fi4/1.8, Fi_top4/1.8,Fi4/1.8*fir4/100.
stop


set_plot, 'ps'
  loadct, 39,/silent
  device, /encapsulated
   device, /tt_font, set_font='Helvetica Bold'
   device, filename=dir+date+'_forcing.ps'
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
  
  plot, wvl, effp[*,0,1], title='Top of layer Forcing',ytitle='Relative Forcing Efficiency(%)',xtitle='Wavelength (nm)', yrange=[min(effp[*,*,1]*0.9,/nan),max(effp[*,*,1]*1.1,/nan)]
  for i=1, n_elements(effp[0,*,1])-1 do oplot, wvl, effp[*,i,1]
  
  plot, wvl, ssa[*,0], title='Single Scattering Albedo', xtitle='Wavelength (nm)', yrange=[0,1]
  for i=1, n_elements(ssa[0,*])-1 do oplot, wvl, ssa[*,i]
  
  plot, wvl, asy[*,0], title='Asymmetry parameter', xtitle='Wavelength (nm)', yrange=[0,1]
  for i=1, n_elements(asy[0,*])-1 do oplot, wvl, asy[*,i]
    
  plot, wvl, effp[*,0,0], title='Bottom of layer Forcing',ytitle='Relative Forcing Efficiency(%)',xtitle='Wavelength (nm)', yrange=[min(effp[*,*,0]*0.9,/nan),max(effp[*,*,0]*1.1,/nan)]
  for i=1, n_elements(effp[0,*,0])-1 do oplot, wvl, effp[*,i,0]
    
  plot, wvl, aot[*,0], title='Optical Depth', xtitle='Wavelength (nm)', yrange=[0,1]
  for i=1, n_elements(aot[0,*])-1 do oplot, wvl, aot[*,i]
  
  plot, wvl, albedo[*,0], title='Surface Albedo', xtitle='Wavelength (nm)', yrange=[0,1]
  for i=1, n_elements(albedo[0,*])-1 do oplot, wvl, albedo[*,i]
  
device, /close
spawn, 'convert '+dir+date+'_forcing.ps '+dir+date+'_forcing_abs.png'
spawn, 'rm -f '+dir+date+'_forcing.ps'

restore, '/home/leblanc/CALNEX/forcing_err_calnex.out'
eff_err=eff_err*110.
eff_errp=fltarr(n_elements(wvl),2)
wvlo=[340,380,440,500,675,870,1020] 
eff_errp[*,0]=interpol(eff_err[*,0],wvlo,wvl)
eff_errp[*,1]=interpol(eff_err[*,1],wvlo,wvl)

wvl=reverse(wvl_arr) 
effp_m=fltarr(n_elements(wvl),2)
effp_s=fltarr(n_elements(wvl),2)
for i=0, n_elements(wvl)-1 do begin
  effp_m[i,0]=mean(effp[i,*,0],/nan)
  effp_m[i,1]=mean(effp[i,*,1],/nan)
  effp_s[i,0]=stddev(effp[i,*,0],/nan)
  effp_s[i,1]=stddev(effp[i,*,1],/nan)
endfor

effp_m[9,0]=effp_m[9,0]*0.6
effp_m[10,0]=effp_m[10,0]*0.6
effp_m[11,0]=effp_m[11,0]*0.5
effp_m[12,0]=effp_m[12,0]*0.4

;n=137
n=2 ;biomass burn
wvl=reverse(wvl)
set_plot, 'ps'
  loadct, 39,/silent
  device, /encapsulated
   device, /tt_font, set_font='Helvetica Bold'
   device, filename=dir+date+'_effp.ps'
   device,/color,bits_per_pixel=8.
   device, xsize=20, ysize=20
   !p.font=1 & !p.thick=5
   !p.charsize=1.8 & !x.style=1
   !y.style=1 & !z.style=1
   !y.thick=1.8 & !x.thick=1.8
  !p.multi=0
!y.omargin=[0,3]
  ; effp[*,n,1]
  plot, wvl, effp[*,n,1], title='Aerosol Direct Forcing Spectra',ytitle='Relative Forcing Efficiency(%/AOT!D500!N)',xmargin=[7,4],xtitle='Wavelength (nm)', yrange=[-70,10],xrange=[340,1020.],/nodata
 !p.thick=10
  tvlct, 230,230,230,230
  polyfill, [wvl[0:9],reverse(wvl[0:9])],[effp_m[0:9,1]+eff_errp[0:9,1],reverse(effp_m[0:9,1]-eff_errp[0:9,1])], color=230
  oplot, wvl, effp_m[*,1],linestyle=2
;  errplot, wvl, effp_m[*,1]-eff_errp[*,1],effp_m[*,1]+eff_errp[*,1]
  tvlct, 196,255,175,151
  ;for i=0, n_elements(effp[0,*,1])-1 do oplot, wvl, effp[*,i,1],color=151
;  tvlct, 0, 150, 0, 150
;  oplot, wvl, effp[*,52,1],color=150
  tvlct, 200,200,200,200
  polyfill, [wvl[0:9],reverse(wvl[0:9])],[effp_m[0:9,0]+eff_errp[0:9,0],reverse(effp_m[0:9,0]-eff_errp[0:9,0])], color=200
  oplot, wvl, effp_m[*,0], linestyle=0
;  errplot, wvl, effp_m[*,0]-eff_errp[*,0],effp_m[*,0]+eff_errp[*,0]
  tvlct, 175,247,255,71
  ;for i=0, n_elements(effp[0,*,0])-1 do oplot, wvl, effp[*,i,0],color=71
;  tvlct, 0,17 ,200,70
;  oplot, wvl, effp[*,52,0], color=70
;  oplot, wvl, effp[*,52,1],color=150

;legend,['Top of Layer', 'Bottom of Layer']
; green strong aborption
tvlct, 155,187,89, 72
openr, 98, '/home/leblanc/CALNEX/schmidt_2010/green-fe.dat'
gn=fltarr(4,12)
readf, 98, gn
close,98

;oplot, gn[0,*],gn[1,*], linestyle=0, color=72 ;BOL
;oplot, gn[0,*],gn[3,*], linestyle=2, color=72 ;TOL

; red aged aerosol layer
tvlct, 192,80,77,73
openr, 98, '/home/leblanc/CALNEX/schmidt_2010/red-fe.dat'
rd=fltarr(5,12)
readf, 98, rd
close,98

;oplot, rd[0,*],rd[1,*], linestyle=0, color=73 ;BOL
;oplot, rd[0,*],rd[3,*], linestyle=2, color=73 ;TOL

;legend,['ARCTAS','Strongly absorbing aerosols!U[1]!N','Slightly aged aerosols!U[1]!N'],textcolors=[0,72,73], /right,/bottom
legend,['Top of Layer', 'Bottom of Layer'], linestyle=[2,0],pspacing=1.5,box=0,/right,/bottom

device, /close
spawn, 'convert '+dir+date+'_effp.ps '+dir+date+'_effp_no_ce_abs.png'
spawn, 'rm -f '+dir+date+'_effp.ps'


stop
end
