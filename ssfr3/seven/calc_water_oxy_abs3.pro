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
;           - ported from calc_water_oxy_abs3
;           - changed the integration method to normalized transmission
;---------------------------------------------------------------------------


pro calc_water_oxy_abs,spectra,lambda,sza,w1_area,w2_area,oxy_area,disd,irr=irr,w2_areav2=w2_areav2,solfac=solfac, trans=trans,v2=v2,doplot=doplot,tt=tt

if keyword_set(doplot) then doplot=1 else doplot=0
if keyword_set(trans)  then trans=1  else trans=0
if keyword_set(tt)     then tt=1     else tt=0

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

; normalize the spetra to tranmittance
if tt then sp=spectra/mean(spectra[44:46]) else sp=spectra
if not trans then begin 
  if not keyword_set(solfac) then sp0=spectra*!PI/cos(sza*!dtor)/fo else sp0=spectra*!PI/solfac/fo
endif else sp0=sp

;set the limits
;i940=[163,195]  ;from 883 - 993 nm
;i1150=[209,228] ;from 1077- 1176(1199) nm
;i810=[135,151]  ;from 793 - 844 nm
;iox=[120,130]   ;from 750 - 775 nm
i940=[152,206]
i1150=[211,242]
i810=[133,153]
iox=[120,131]



;v2[0]=int_tabulated(lambda[i940],sp[i940])-int_tabulated(lambda[i940[0]:i940[1]],sp[i940[0]:i940[1]])
;v2[1]=int_tabulated(lambda[i1150],sp[i1150])-int_tabulated(lambda[i1150[0]:i1150[1]],sp[i1150[0]:i1150[1]])
;v2[2]=int_tabulated(lambda[iox],sp[iox])-int_tabulated(lambda[iox[0]:iox[1]],sp[iox[0]:iox[1]])
;av2  =int_tabulated(lambda[i810],sp[i810])-int_tabulated(lambda[i810[0]:i810[1]],sp[i810[0]:i810[1]])

sps=smooth(sp,3)
spi0 =interpol(sps[i940],lambda[i940],lambda[i940[0]:i940[1]])
v2[0]=int_tabulated(lambda[i940[0]:i940[1]],1.-sps[i940[0]:i940[1]]/spi0)
spi1 =interpol(sp[i1150],lambda[i1150],lambda[i1150[0]:i1150[1]])
v2[1]=int_tabulated(lambda[i1150[0]:i1150[1]],1.-sp[i1150[0]:i1150[1]]/spi1)
spi2 =interpol(sp[iox],lambda[iox],lambda[iox[0]:iox[1]])
v2[2]=int_tabulated(lambda[iox[0]:iox[1]],1.-sp[iox[0]:iox[1]]/spi2)
spi3 =interpol(sp[i810],lambda[i810],lambda[i810[0]:i810[1]])
av2  =int_tabulated(lambda[i810[0]:i810[1]],1.-sp[i810[0]:i810[1]]/spi3)

;set the values to be returned
w1_area=v2[0]
w2_area=v2[1]
w2_areav2=av2
oxy_area=v2[2]

disd=0.

if doplot then begin
  wset,0
  plot,lambda[i940[0]-5:i940[1]+5],sps[i940[0]-5:i940[1]+5], title='940 nm band :'+strtrim(v2[0],2)
  oplot,lambda[i940],sps[i940], color=250

  plot,lambda[i1150[0]-5:i1150[1]+5],sp[i1150[0]-5:i1150[1]+5], title='1150 nm band :'+strtrim(v2[1],2)
  oplot,lambda[i1150],sp[i1150], color=250

  plot,lambda[iox[0]-5:iox[1]+5],sp[iox[0]-5:iox[1]+5], title='Oxygen-a band :'+strtrim(v2[2],2)
  oplot,lambda[iox],sp[iox], color=250

  wset, 1
  spii0=[sps[i940[0]-5:i940[0]-1],spi0,sps[i940[1]+1:i940[1]+5]]
  plot,lambda[i940[0]-5:i940[1]+5],sps[i940[0]-5:i940[1]+5]/spii0, title='Normalized 940 nm band :'+strtrim(v2[0],2)
  oplot,lambda[i940],[1,1], color=250

  spii1=[sp[i1150[0]-5:i1150[0]-1],spi1,sp[i1150[1]+1:i1150[1]+5]]
  plot,lambda[i1150[0]-5:i1150[1]+5],sp[i1150[0]-5:i1150[1]+5]/spii1, title='Normalized 1150 nm band :'+strtrim(v2[1],2)
  oplot,lambda[i1150],[1,1], color=250

  spii2=[sp[iox[0]-5:iox[0]-1],spi2,sp[iox[1]+1:iox[1]+5]]
  plot,lambda[iox[0]-5:iox[1]+5],sp[iox[0]-5:iox[1]+5]/spii2, title='Normalized Oxygen-a band :'+strtrim(v2[2],2)
  oplot,lambda[iox],[1,1], color=250

  wait, 0.02
endif
;stop
end

