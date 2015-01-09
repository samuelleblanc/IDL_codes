;+
; NAME:
;   calc_water_oxy_abs
;
; PURPOSE:
;   to calculate the water vapor absorption band integrate area for two different water vapor bands
;   to also calculate the oxygen-a band integrated area
;   used for the photon path length analysis
;   only does calculates these values from a single sprectum
;   automatically calculated the transmitted radiance/irradiance
;
; CATEGORY:
;   Photon path length, water vapor and oxygen-a absorption calculations
;
; CALLING SEQUENCE:
;   calc_water_oxy,spectra,lambda,sza,w1_area,w2_area,oxy_area,disd,irr=irr
;     - spectra  - the measured spectrum (input)
;     - lambda   - the corresponding wavelength array for the measured spectrum (input)
;     - sza      - the corresponding solar zenith angle (input)
;     - w1_area  - the resulting integrated water vapor transmitted absorption for the 1st band (output)
;     - w2_area  - same as w1_area, but the second water vapor band (output)
;     - oxy_area - the resulting integrated transmitted oxygen-a band absorption (output)
;     - disd     - The 2nd water vapor band width in nm (output)
;
; OUTPUT:
;       integrated water vapor band area (for 1st and 2nd)
;       integrated oxygen-a band area
;
; KEYWORDS:
;   - irr - does the calculations for irradiance instead of radiance
;   - w2_areav2 - second way to calculate the water vapor band 2
;   - solfac - the sun-earth distance factor
;   - trans - indicates that the input spectrum is in transmittance already
;   - v2 - the second version of the area/width values
;   - doplot - to make plots of each calculations to 'x'
;
; DEPENDENCIES:
;   - kurudz_1.0nm.dat - extra terrestrial irradiance
;   - the line width SSFR function files
;
; EXAMPLE:
; 
;
; MODIFICATION HISTORY:
; Written:  Samuel LeBlanc, LASP/ATOC CU Boulder, November 12th, 2012
; Modified: By Samuel LeBlanc, November 20th, 2012
;           - added the doplot keyword
;
;---------------------------------------------------------------------------


pro calc_water_oxy_abs,spectra,lambda,sza,w1_area,w2_area,oxy_area,disd,irr=irr,w2_areav2=w2_areav2,solfac=solfac, trans=trans,v2=v2,doplot=doplot

if keyword_set(doplot) then doplot=1 else doplot=0
if keyword_set(trans)  then trans=1  else trans=0

v2=fltarr(3) ; 0 - area1, 1 - area2, 2 - oxa

;get the top of atmosphere irradiance to calculate the transmittance
;print, 'getting extra terrestrial irradiance'
if not trans then begin
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
  fo=[interpol(fvis,F_o.field1[0,*],lambda[0:193]),interpol(fnir,F_o.field1[0,*],lambda[194:*])]
endif

; prepare the limits of the calculations
nul=min(abs(lambda-750.),fi)
nul=min(abs(lambda-775.),la)
wvs=fltarr(2)

; normalize the spetra to tranmittance
sp=spectra
if not trans then begin 
  if not keyword_set(solfac) then sp0=spectra*!PI/cos(sza*!dtor)/fo else sp0=spectra*!PI/solfac/fo
endif else sp0=sp

drad0=deriv(sp0)


;now start the calculations
drad=smooth(drad0/max(drad0[149:204]),2)
nmins=where(drad[149:169] lt 0.2 and drad[149:169] gt -0.2 and shift(drad[149:169],1) gt -0.2 and shift(drad[149:169],1) lt 0.2)
ns=indgen(21)+149
nmin=max(nmins)
nmin=ns[nmin]

nmaxs=where(drad[193:204] lt 0.2 and drad[193:204] gt -0.2 and shift(drad[193:204],1) gt -0.2 and shift(drad[193:204],1) lt 0.2)
ns=indgen(12)+193
nmax=min(nmaxs)
nmax=ns[nmax]
  
dist=lambda[nmax]-lambda[nmin]
narea=int_tabulated(lambda[nmin:nmax],interpol(sp0[[nmin,nmax]],lambda[[nmin,nmax]],lambda[nmin:nmax])-$
   sp0[nmin:nmax])  ;transmitted area
area=int_tabulated(lambda[nmin:nmax],interpol(sp[[nmin,nmax]],lambda[[nmin,nmax]],lambda[nmin:nmax])-$
   sp[nmin:nmax])   ;integrated radiance area
wvs[0]=lambda[nmin]
wvs[1]=lambda[nmax]

;for the second water vapor band
nwv=[198,212,220,232]
dr=drad
nmins=where(drad0[nwv[0]:nwv[1]]/max(drad0[nwv[0]:nwv[3]]) gt (shift(drad0[nwv[0]:nwv[1]]/max(drad0[nwv[0]:nwv[3]]),1)-0.035))
ns=indgen(nwv[1]-nwv[0]+1)+nwv[0]
nmin=max(nmins)
nmin=ns[nmin]

nmaxs=where(drad0[nwv[2]:nwv[3]] lt 0.2 and drad0[nwv[2]:nwv[3]] gt -0.2 and shift(drad0[nwv[2]:nwv[3]],1) gt -0.2 and shift(drad0[nwv[2]:nwv[3]],1) lt 0.2)
ns=indgen(nwv[3]-nwv[2]+1)+nwv[2]
nmax=min(nmaxs)
nmax=ns[nmax]

narea2=int_tabulated(lambda[nmin:nmax],interpol(sp0[[nmin,nmax]],lambda[[nmin,nmax]],lambda[nmin:nmax])-$
 sp0[nmin:nmax])
area2=int_tabulated(lambda[nmin:nmax],interpol(sp[[nmin,nmax]],lambda[[nmin,nmax]],lambda[nmin:nmax])-$
 sp[nmin:nmax])

i1=[196,197,198,232,233,234]
spn1=sp[196:234]/interpol(sp[i1],lambda[i1],lambda[196:234],/spline)
i1ns=indgen(n_elements(spn1))+196
i1n=where(i1ns le nmin or i1ns ge nmax)
spn1=sp[196:234]/interpol(sp[i1ns[i1n]],lambda[i1ns[i1n]],lambda[196:234],/spline)
v2[0]=int_tabulated(lambda[198:232],1.-spn1[2:n_elements(spn1)-3])

;for the oxygen a band
oxar=-sp[fi:la]+interpol(sp[[fi,la]],lambda[[fi,la]],lambda[fi:La])
radoxa=int_tabulated(lambda[fi:la],oxar)
noxar=-sp0[fi:la]+interpol(sp0[[fi,la]],lambda[[fi,la]],lambda[fi:La])
nradoxa=int_tabulated(lambda[fi:la],noxar)

;second way of determining absorption from oxaygen-a band, by normalizing to a value outside of the band
ix=[fi-5,fi-4,fi-3,fi-2,fi-1,la+1,la+2,la+3,la+4,la+5]
spn=sp[fi-5:la+5]/interpol(sp[ix],lambda[ix],lambda[fi-5:la+5],/spline)
v2[2]=int_tabulated(lambda[fi-1:la+1],1.-spn[4:n_elements(spn)-5])

; for the water vapor width of the second band
; make new high reslution splined spectra
lam=findgen(4300.)/20.+1020.
n=where(lambda ge 1020. and lambda le 1230.)
spi=interpol(drad0[n],lambda[n],lam,/spline)

;determine the start of the band
nul=min(spi,nmin)
ns=where(spi-shift(spi,1) gt 0.)
j=max(ns[where(ns lt nmin)])+1
if j gt 1 and j lt n_elements(lam) then start=lam[j] else start=!values.f_nan

;determine the end of the band
nul=max(spi,nmax)
ms=where(spi-shift(spi,-1) lt 0.)
k=min(ms[where(ms gt nmax)])
if k lt n_elements(lam)-1 and k gt 1 then fin=lam[k] else fin=!values.f_nan
if k le j then fin=!values.f_nan 

;now set the distance
disd=fin-start

;another method of finding the area2
sp0hi=interpol(sp0[n],lambda[n],lam,/spline)
if k gt j then areav2=int_tabulated(lam[j:k],-sp0hi[j:k]+interpol(sp0hi[[j,k]],lam[[j,k]],lam[j:k])) else areav2=!values.f_nan

;mehtod using the normalisation
spi=interpol(sp0[n],lambda[n],lam,/spline)
ind=indgen(4300)
in=where(ind le j or ind ge k)
spnorm=spi/interpol(spi[in],lam[in],lam,/spline)

;plot, lam, spnorm, title='Normalized Transmitted radiance', xtitle='Wavelength (nm)', ytitle='Normalized radiance'
;plot, lam, deriv(spnorm),title='Derivatice of radiance', xtitle='Wavelength (nm)', ytitle='Derivative'

if k gt j then v2[1]=int_tabulated(lam[j:k],1.-spnorm[j:k]) else v2[1]=!values.f_nan

if doplot then begin
  wset,0

  ;plot the first water vapor band
  plot, lambda[196:234],spn1,title='940 nm Water vapor band',xtitle='Wavelength (nm)',ytitle='Normalized radiance'
  oplot,lambda[[198,198]],[0,2],linestyle=2,color=70
  oplot,lambda[[232,232]],[0,2],linestyle=2,color=70
  oplot, lambda[196:234], lambda[196:234]*0.+1.,linestyle=2,color=250

  ; plot the second water vapor band
  plot, lam,spnorm,title='1150 nm Water vapor band',xtitle='Wavelength (nm)',ytitle='Normalized radiance'
  oplot, lam[[j,j]],[0,2],linestyle=2, color=70
  oplot, lam[[k,k]],[0,2],linestyle=2,color=70
  oplot, lam, lam*0.+1.,linestyle=2,color=250

  ;plot the oxygen-a band
  plot, lambda[fi-5:la+5],spn,title='Oxygen-a band',xtitle='Wavelength (nm)',ytitle='Normalized radiance' 
  oplot, lambda[[fi-1,fi-1]],[0,2],linestyle=2,color=70
  oplot, lambda[[la+1,la+1]],[0,2],linestyle=2,color=70
  oplot, lambda[fi-5:la+5], lambda[fi-5:la+5]*0.+1., linestyle=2,color=250

  wset,1

  ;plot the raw spectra before normalization
  ;plot the first water vapor band
  plot, lambda[196:234],sp[196:234],title='Raw 940 nm Water vapor band',xtitle='Wavelength (nm)',ytitle='Radiance'
  oplot,lambda[196:234],interpol(sp[i1ns[i1n]],lambda[i1ns[i1n]],lambda[196:234],/spline),color=70

  ; plot the second water vapor band
  plot, lam,spi,title='Raw 1150 nm Water vapor band',xtitle='Wavelength (nm)',ytitle='Radiance'
  oplot, lam,interpol(spi[in],lam[in],lam,/spline), color=70

  ;plot the oxygen-a band
  plot, lambda[fi-5:la+5],sp[fi-5:la+5],title='Raw Oxygen-a band',xtitle='Wavelength (nm)',ytitle='Radiance'
  oplot, lambda[fi-5:la+5],interpol(sp[ix],lambda[ix],lambda[fi-5:la+5],/spline),color=70

  wait, 0.02
endif

;set the values to be returned
w1_area=narea
w2_area=narea2
w2_areav2=areav2
oxy_area=nradoxa

;stop
end

