; program to determine the water vapor influence by a second method
; uses the derivative to find the width of the water vapor in a much finer resolution than available by SSFR
; used for testing

@zensun.pro
pro wv_2nd_cl

dir='/home/leblanc/SSFR3/data/'

; get spectra
restore, dir+'sp_at_cl.out'
print, 'restoring spectra file: '+dir+'sp_at_cl.out'


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
area=fltarr(nd)
lims=fltarr(nd,2)
v2=area

lam=findgen(4500.)/20.+1020. ;wavelength arrays for much higher resolution


set_plot, 'x'
device, decomposed=0
loadct, 39
!p.multi=[0,1,2]
window, 0, retain=2, xsize=600,ysize=800
window, 1, retain=2, xsize=600,ysize=800
n=where(zenlambda ge 1020. and zenlambda le 1250.)
for i=0, nd-1 do begin
  print, 'doing :',i,'/',nd-1
  spz0=spz[*,i]*!PI/cos(sza[i]*!dtor)/foz
  spz0=spz0/max(spz0[n])
  spn0=spn[*,i]*!PI/cos(sza[i]*!dtor)/fon
  drad=deriv(spz0)
  ;ddrad=deriv(drad)
  wset,0
  ; the area of the second water vapor band is determined by indexes from 197 to 237 (1000 - 1250 nm)
  plot, zenlambda, spz0, xrange=[1000,1250], title='Transmittance',xtitle='Wavelength (nm)'
  ;print, 'plotted spz0' 
  plot, zenlambda, drad, xrange=[1000,1250],title='Derivative of Trans',xtitle='Wavelength (nm)'
  ;print, 'splinning'
  ;now spline to a much higher resolution
  ;sp=spline(zenlambda[n],spz0[n],lam)
  sp=interpol(drad[n],zenlambda[n],lam,/spline)
  sps=interpol(smooth(drad[n],3),zenlambda[n],lam,/spline)
 ; sd=interpol(ddrad[n],zenlambda[n],lam,/spline)
  ;oplot, lam, sd,color=200
  oplot, lam,sp,color=70 ; overplot the spline measurement
  oplot, lam,sps,color=251 
 
  ;now find the width
  ; start by the lowest deriv
  ;n=indgen(41)+197
  ;nul=min(sd,nmin)
  nul=min(sp,nmin) 
 
  ; find the first decreasing value starting from the minimum
  ;ns=where(sd-shift(sd,1) gt 0.)
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
;  if disd[i] lt 0 then disd[i] = !values.f_nan
  if k lt j then k=2500
spi=interpol(spz0[n],zenlambda[n],lam,/spline)
  area[i]=int_tabulated(lam[j:k],interpol(spi[[j,k]],lam[[j,k]],lam[j:k])-spi[j:k])

  lims[i,0]=start
  lims[i,1]=fin

wset,1
;spi=interpol(spz0[n],zenlambda[n],lam,/spline)
ind=indgen(4500)
nul=min(abs(lam-1070.),j)
nul=min(abs(lam-1240.),k)
in=where(ind le j or ind ge k)
spnorm=spi/interpol(spi[in],lam[in],lam,/spline)

plot, lam, spnorm, title='Normalized Transmitted radiance', xtitle='Wavelength (nm)', ytitle='Normalized radiance'
plot, lam, deriv(spnorm),title='Derivatice of radiance', xtitle='Wavelength (nm)', ytitle='Derivative'

v2[i]=int_tabulated(lam[j:k],1.-spnorm[j:k])

;stop
wait,0.2
endfor

is=where(disd gt 100. and disd lt 120.)

r=correlate(disd[is],water[is])
ra=correlate(area[is],water[is])

!p.multi=0
window,2, retain=2
plot, disd,title='Width vs. time'
window,3,retain=2
plot, disd[is], water[is], psym=2, title='Width vs. water'
print, r,ra
window,4, retain=2
plot, area[is], water[is], psym=2, title='Area vs. water'
window,5, retain=2
plot, v2, water, psym=2, title='Second area vs. water'


;save, disd,water, area, lims,doys, cod,filename=dir+'water_disd.out'
stop
end
