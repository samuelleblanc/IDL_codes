; program that takes in a spectrum of zenith pointing radiance
; then calculates various parameters from that parameter that would be linked to 
; ref, tau, (ice or liquid) water content, phase

pro get_params, wvl, sp, par ;wvl is the wavelength array of the spectrum, sp is the spectrum, par is an array of different parameters

; first normalize sp and calculate its derivative
par=fltarr(2)

; program to make test paramaters all the ones determined from the MODIS wavelengths
nul=min(abs(wvl-515.),iw)
par[0]=sp[iw]

nul=min(abs(wvl-1625.),iw)
par[1]=sp[iw]

end 
