; program to run the cloud transmission retrieval technique descirbed by Patrick
; based on the retrieve_cloud_params from calnex

pro cloud_rtm, sza, spectra, lambda, tau, ref, irr=irr

if keyword_set(irr) then irr=1 else irr=0

;restore the lut from file
restore, '/home/leblanc/DC3_SEAC4RS/library/CLD_LUT.out'

; now interpolate the measured spectra to the proper wavelengths
sp=interpol(spectra,lambda,wvls)

; change the measured spectra to transmission
if irr then sp=sp*cos(sza*!dtor)/sun else sp=sp*!DPI*cos(sza*!dtor)/sun

; calculate the slope and value at 515 for the measurement
slp=sp[1:*]/sp[1]
tmp=linfit(wvls[1:*],slp)
slope=tmp[1]

r515=sp[0]
deltarad=0.05 ; set the uncertainty


; now interpolate the resulting lut to the proper sza

if irr then begin ; set the correct model to look at
  mslo=sloirr
  m515=irr500
endif else begin
  mslo=slorad
  m515=rad500
endelse

;prepare variables to store interpolate values
ms = fltarr(n_elements(taus),n_elements(refs)) ;model slope
ma = fltarr(n_elements(taus),n_elements(refs)) ;model value at 515

for i=0,n_elements(taus)-1 do begin
  for j=0, n_elements(refs)-1 do begin
    ms[i,j]=interpol(mslo[i,j,*],szas,sza)
    ma[i,j]=interpol(m515[i,j,*],szas,sza)
  endfor
endfor

; now determine the errors in each value of the look up table
a_err=((ma-r515)/ma)^2.0
s_err=((ms-slope)/ms)^2.0
total_err=sqrt((a_err+s_err)/2.0)

; now find the location of the minimum error
nul=min(total_err,i_min)

iref=floor(i_min/n_elements(taus))
itau=i_min-n_elements(taus)*iref

;now set the tau ad ref
tau=taus[itau]
ref=refs[iref]

end
