; program to determine the water vapor influence by a second method
; uses the derivative to find the width of the water vapor in a much finer resolution than available by SSFR
; used for testing

@zensun.pro
@legend.pro
pro wv_2nd

dir='/home/leblanc/SSFR3/data/'

; get spectra
restore, dir+'sp_at_wv.out'
print, 'restoring spectra file: '+dir+'sp_at_wv3.out'


;get the top of atmosphere irradiance to calculate the transmittance
print, 'getting extra terrestrial irradiance'
F_o=read_ascii('/home/leblanc/libradtran/libRadtran-1.6-beta/data/solar_flux/kurudz_1.0nm.dat', comment_symbol='#',data_start=12)
vis=read_ascii('/home/leblanc/libradtran/vis_1nm.dat')
nir=read_ascii('/home/leblanc/libradtran/nir_1nm.dat')

F_o.field1[1,*]=F_o.field1[1,*]/1000.
vis.field1[1,*]=vis.field1[1,*]/total(vis.field1[1,*])
nir.field1[1,*]=nir.field1[1,*]/total(nir.field1[1,*])
fvis=fltarr(n_elements(F_o.field1[1,*]))
fnir=fvis
for i=7,n_elements(f_o.field1[0,*])-8 do for j=-7,7 do fvis[i]=fvis[i]+f_o.field1[1,i+j]*vis.field1[1,j]
for i=15,n_elements(f_o.field1[0,*])-16 do for j=-15,15 do fnir[i]=fnir[i]+f_o.field1[1,i+j]*nir.field1[1,j]
foz=[interpol(fvis,F_o.field1[0,*],zenlambda[0:193]),interpol(fnir,F_o.field1[0,*],zenlambda[194:*])]
fon=[interpol(fvis,F_o.field1[0,*],nadlambda[0:193]),interpol(fnir,F_o.field1[0,*],nadlambda[194:*])]

;build the sza array
print, 'building the sza arrays'
;nd=184
;doy=findgen(nd)+2456048.5
caldat,doys,month,day,year
lat=40.007916667
lon=-105.26825
nd=n_elements(doys)
sza=fltarr(nd)
for i=0, nd-1 do begin
  doyt=julian_day(year[i],month[i],day[i])
  zensun, doyt, (doys[i]-floor(doys[i]))*24.,lat, lon, szat, azimuth, solfac
  sza[i]=szat
endfor


disd=fltarr(nd)
lims=fltarr(nd,2)
v2=fltarr(nd)

lam=findgen(460.)/2.+1020. ;wavelength arrays for much higher resolution


set_plot, 'x'
device, decomposed=0
loadct, 39
!p.multi=[0,1,2]
window, 0, retain=2, xsize=600,ysize=900
window, 1, retain=2, xsize=600,ysize=900
n=where(zenlambda ge 1020. and zenlambda le 1250.)
for i=0, nd-1 do begin
wset,0
  print, 'doing :',i,'/',nd-1
  spz0=spz[*,i]*!PI/cos(sza[i]*!dtor)/foz
  spn0=spn[*,i]*!PI/cos(sza[i]*!dtor)/fon
  drad=deriv(spz0)
  ddrad=deriv(drad)

  ; the area of the second water vapor band is determined by indexes from 197 to 237 (1000 - 1250 nm)
  plot, zenlambda, spz0, xrange=[1000,1250], title='Transmittance',xtitle='Wavelength (nm)'
  ;print, 'plotted spz0' 
  plot, zenlambda, drad, xrange=[1000,1250],title='Derivative of Trans',xtitle='Wavelength (nm)'
  ;print, 'splinning'
  ;now spline to a much higher resolution
  ;sp=spline(zenlambda[n],spz0[n],lam)
  sp=interpol(drad[n],zenlambda[n],lam,/spline)
  sps=interpol(smooth(drad[n],3),zenlambda[n],lam,/spline)
  sd=interpol(ddrad[n],zenlambda[n],lam,/spline)
  oplot, lam, sd,color=200
  oplot, lam,sp,color=70 ; overplot the spline measurement
  oplot, lam,sps,color=251 
 
  ;now find the width
  ; start by the lowest deriv
  ;n=indgen(41)+197
  nul=min(sp,nmin)
  
  ; find the first decreasing value starting from the minimum
  ns=where(sp-shift(sp,1) gt 0.)
  j=max(ns[where(ns lt nmin)])+1
  ;l=ns[where(ns-shift(ns,-1) ne -1)]  
  ;j=max(l[where(l lt j-1)])
  ;stop
  ; decrement until found a local max
  ;while (j gt 0) do if sp[j] gt sp[j-1] then continue else j=j-1
  if j gt 1 then start=lam[j] else start=!values.f_nan

  ; now start with the highest deriv
  nul=max(sp,nmax)
  ms=where(sp-shift(sp,-1) lt 0.)
  k=min(ms[where(ms gt nmax)])

  ;increment until found local min
;  while (k lt n_elements(n)-1) do if sp[k] lt sp[k+1] then continue else k=k+1
  if k lt n_elements(lam)-1 then fin=lam[k] else fin=!values.f_nan
;stop
  ; now plot the values found 
  oplot, [start,start],[min(drad),max(drad)],linestyle=2, color=130
  oplot, [fin,fin],[min(drad),max(drad)],linestyle=2, color=130

  disd[i]=fin-start
  if disd[i] lt 0 then disd[i] = !values.f_nan
  lims[i,0]=start
  lims[i,1]=fin
;stop
;;; save a plot of a single spectra
if i eq 3 then begin
  fn='/home/leblanc/SSFR3/data/example_1150nm'
  set_plot,'ps'
  print, 'making plot :'+fn
   loadct, 39, /silent
   device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fn+'.ps'
   device, xsize=20, ysize=40
   !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
   !p.multi=[0,1,2] & !x.margin=[7,2] &!x.omargin=[0,0]
  plot, zenlambda, spz0, xrange=[1000,1250], title='Transmittance',ytitle='Transmittance',xtitle='Wavelength (nm)'
  plot, zenlambda, drad, xrange=[1000,1250],title='Derivative of Trans',ytitle='Derivative',xtitle='Wavelength (nm)'
  oplot, lam,sp,color=70 ; overplot the spline measurement
  oplot, [start,start],[min(drad),max(drad)],linestyle=2, color=130
  oplot, [fin,fin],[min(drad),max(drad)],linestyle=2, color=130
  legend, ['Raw derivative','Splined derivative','Limits of the band'],textcolors=[0,70,130],box=0

  device, /close
  spawn, 'convert '+fn+'.ps '+fn+'.png'

  set_plot, 'x'
  device, decomposed=0
  loadct, 39
  !p.multi=[0,1,2] & !p.thick=1
  window, 0, retain=2, xsize=600,ysize=900
  window, 1, retain=2, xsize=600, ysize=900
endif
wset,1
spi=interpol(spz0[n],zenlambda[n],lam,/spline)
ind=indgen(450)
nul=min(abs(lam-1070.),j)
nul=min(abs(lam-1240.),k)
in=where(ind le j or ind ge k)
spnorm=spi/interpol(spi[in],lam[in],lam,/spline)

plot, lam, spnorm, title='Normalized Transmitted radiance', xtitle='Wavelength (nm)', ytitle='Normalized radiance'
plot, lam, deriv(spnorm),title='Derivatice of radiance', xtitle='Wavelength (nm)', ytitle='Derivative'

v2[i]=int_tabulated(lam[j:k],1.-spnorm[j:k])


wait,0.2
endfor

r=correlate(disd,water)

!p.multi=0
window,1, retain=2
plot, disd
window,2,retain=2
plot, disd, water, psym=2
print, r

stop
end
