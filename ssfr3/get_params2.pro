; program that takes in a spectrum of zenith pointing radiance
; then calculates various parameters from that parameter that would be linked to 
; ref, tau, (ice or liquid) water content, phase

pro get_params, wvl, spc, par, maxwvl=maxwvl ;wvl is the wavelength array of the spectrum, sp is the spectrum, par is an array of different parameters

; first normalize sp and calculate its derivative
; normalized to the mean of 560 nm to 580 nm  
nul=min(abs(wvl-560.),mm)
nul=min(abs(wvl-580.),mn)
sp=spc/mean(spc[mm:mn]) ;max(spc,mm)
dsp=smooth(deriv(wvl/1000.,sp),2)
maxwvl=wvl[mm]
par=fltarr(16)

; for first parameter:
; Difference between a slope of the points at 1000 nm and 1077 nm to all the measured points in between
nul=min(abs(wvl-1000.),io)
nul=min(abs(wvl-1077.),in)

ln=linfit(wvl[[io,in]],sp[[io,in]])
par[0]=total(sp[io:in]-(ln[0]+ln[1]*wvl[io:in]))

;for second parameter
; The derivative value at 1193 nm (more negative - more ice)
nul=min(abs(wvl-1193.),ii)
par[1]=dsp[ii]

; for third parameter
; The derivative value at 1493 nm (more positive - more liquid, for small reff)
nul=min(abs(wvl-1493.),ii)
par[2]=dsp[ii]

; for fourth parameter
; ratio of 1198 & 1236 nm, when 1236 nm is larger - liquid, when 1198 nm is larger - ice
nul=min(abs(wvl-1198.),io)
nul=min(abs(wvl-1236.),in)
par[3]=sp[io]/sp[in]

; for fifth parameter
; Average normalized radiance at around 1248 nm  to 1270 nm- proportional to water content ?
nul=min(abs(wvl-1248.),io)
nul=min(abs(wvl-1270.),in)
par[4]=mean(sp[io:in])

; for sixth parameter
; Average normalized radiance at around 1565 nm to 1644 nm - proportional to tau?
nul=min(abs(wvl-1565.),io)
nul=min(abs(wvl-1644.),in)
par[5]=mean(sp[io:in])

; for seventh parameter
; Average normalized radiance at around 1000 nm to 1050 nm - proportional to what? 
nul=min(abs(wvl-1000.),io)
nul=min(abs(wvl-1050.),in)
par[6]=mean(sp[io:in])

; for eigth parameter
; Difference between a slope of the points at 1493 nm and 1600 nm to all the measured points in between
nul=min(abs(wvl-1493.),io)
nul=min(abs(wvl-1600.),in)

ln=linfit(wvl[[io,in]],sp[[io,in]])
par[7]=total(sp[io:in]-(ln[0]+ln[1]*wvl[io:in]))

; for ninth parameter
; the slope of the derivative for all pints between 1000 nm and 1077 nm
nul=min(abs(wvl-1000.),io)
nul=min(abs(wvl-1077.),in)

ln=linfit(wvl[io:in],dsp[io:in])
par[8]=ln[1]

; for tenth parameter
; the slope of the derivative for all points between 1200 nm to 1300 nm
nul=min(abs(wvl-1200.),io)
nul=min(abs(wvl-1300.),in)

ln=linfit(wvl[io:in],dsp[io:in])
par[9]=ln[1]


; for eleventh parameter
; the slope of the visible wavelengths between 550nm to 680 nm
nul=min(abs(wvl-550.),io)
nul=min(abs(wvl-680.),in)

ln=linfit(wvl[io:in]/1000.,sp[io:in])
par[10]=ln[1]

; for twelth paratemer
; normalized radiance at 1000 nm
nul=min(abs(wvl-1000.),io)
par[11]=sp[io]

; for the thirteenth parameter
; normalized radiance at 1040 nm
nul=min(abs(wvl-1040.),io)
par[12]=sp[io]

; for the fourteenth parameter
; normalized radiance at 1065 nm
nul=min(abs(wvl-1065.),io)
par[13]=sp[io] 

par[12]=par[11]/par[13]

; for the fifteenth parameter
; the slope between 1565 and 1634 nm 
; the same parameter used by Mcbride et al., 2011
nul=min(abs(wvl-1565.),io)
nul=min(abs(wvl-1634.),in)

ln=linfit(wvl[io:in],spc[io:in]/spc[io])
par[14]=ln[1]
if sp[io] lt 0.003 then par[14]=0.03

; for sixteenth parameter
; simply the maximum radiance
; used for testing
par[15]=max(sp)


end 
