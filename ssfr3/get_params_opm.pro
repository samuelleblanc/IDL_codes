; program that takes in a spectrum of zenith pointing radiance
; then calculates various parameters from that parameter that would be linked to 
; ref, tau, (ice or liquid) water content, phase


; ******** Optimized for monte carlo **********
;
;




function get_params, wvl, spc, maxwvl=maxwvl ;wvl is the wavelength array of the spectrum, sp is the spectrum, par is an array of different parameters

; first normalize sp and calculate its derivative
sp=spc/max(spc,mm)
;nul=min(abs(wvl-1000.),io)
io=181
sp2=spc/spc[io]
dsp=smooth(deriv(wvl/1000.,sp2),2)
maxwvl=wvl[mm]
par=fltarr(16)

; for first parameter:
; Difference between a slope of the points at 1000 nm and 1077 nm to all the measured points in between
;nul=min(abs(wvl-1000.),io)
io=181
;nul=min(abs(wvl-1077.),in)
in=194

ln=linfit(wvl[[io,in]],sp2[[io,in]])
par[0]=total(sp2[io:in]-(ln[0]+ln[1]*wvl[io:in]))

;for second parameter
; The derivative value at 1193 nm (more negative - more ice)
;nul=min(abs(wvl-1193.),ii)
ii=212
par[1]=dsp[ii]

; for third parameter
; The derivative value at 1493 nm (more positive - more liquid, for small reff)
;nul=min(abs(wvl-1493.),ii)
ii=262
par[2]=dsp[ii]

; for fourth parameter
; ratio of 1198 & 1236 nm, when 1236 nm is larger - liquid, when 1198 nm is larger - ice
;nul=min(abs(wvl-1198.),io)
io=213
;nul=min(abs(wvl-1236.),in)
in=219
par[3]=sp[io]/sp[in]

; for fifth parameter
; Average normalized radiance at around 1248 nm  to 1270 nm- proportional to water content ?
;nul=min(abs(wvl-1248.),io)
;nul=min(abs(wvl-1270.),in)
io=221
in=225
par[4]=mean(sp[io:in])

; for sixth parameter
; Average normalized radiance at around 1565 nm to 1644 nm - proportional to tau?
;nul=min(abs(wvl-1565.),io)
;nul=min(abs(wvl-1644.),in)
io=274
in=287
par[5]=mean(sp[io:in])

; for seventh parameter
; Average normalized radiance at around 1000 nm to 1050 nm - proportional to what? 
;nul=min(abs(wvl-1000.),io)
;nul=min(abs(wvl-1050.),in)
io=181
in=189
par[6]=mean(sp[io:in])

; for eigth parameter
; Difference between a slope of the points at 1493 nm and 1600 nm to all the measured points in between
;nul=min(abs(wvl-1493.),io)
;nul=min(abs(wvl-1600.),in)
io=262
in=280

ln=linfit(wvl[[io,in]],sp2[[io,in]])
par[7]=total(sp2[io:in]-(ln[0]+ln[1]*wvl[io:in]))

; for ninth parameter
; the slope of the derivative for all pints between 1000 nm and 1077 nm
;nul=min(abs(wvl-1000.),io)
;nul=min(abs(wvl-1077.),in)
io=181
in=194

ln=linfit(wvl[io:in],dsp[io:in])
par[8]=ln[1]

; for tenth parameter
; the slope of the derivative for all points between 1200 nm to 1300 nm
;nul=min(abs(wvl-1200.),io)
;nul=min(abs(wvl-1300.),in)
io=214
in=230

ln=linfit(wvl[io:in],dsp[io:in])
par[9]=ln[1]


; for eleventh parameter
; the slope of the visible wavelengths between 550nm to 680 nm
;nul=min(abs(wvl-550.),io)
;nul=min(abs(wvl-680.),in)
;nul=min(abs(wvl-530.),io)
;nul=min(abs(wvl-610.),in)
io=39
in=64

ln=linfit(wvl[io:in]/1000.,sp[io:in])
par[10]=ln[1]

; for twelth paratemer
; normalized radiance at 1000 nm
;nul=min(abs(wvl-1040.),io)
io=188
par[11]=sp[io]

; for the thirteenth parameter
; normalized radiance at 1040 nm
;nul=min(abs(wvl-1000.),io)
;nul=min(abs(wvl-1065.),in)
io=181
in=192
par[12]=sp[io]/sp[in]

; for the fourteenth parameter
; normalized radiance at 1065 nm
;nul=min(abs(wvl-600.),io) 
;nul=min(abs(wvl-870.),in) 
io=61
in=144
par[13]=sp[io]/sp[in] 

;nul=min(abs(wvl-1065.),io)
;par[13]=sp[io] 

;par[12]=par[11]/par[13]


; for the fifteenth parameter
; the slope between 1565 and 1634 nm 
; the same parameter used by Mcbride et al., 2011
;nul=min(abs(wvl-1565.),io)
;nul=min(abs(wvl-1634.),in)
io=274
in=286

ln=linfit(wvl[io:in],spc[io:in]/spc[io])
par[14]=ln[1]
;if sp[io] lt 0.003 then par[14]=0.03
if par[14] gt 0.03 then par[14]=0.03

;if abs(ln[1]) gt 0.2 or not finite(ln[1]) then par[14]=0.2

; for sixteenth parameter
; simply the maximum radiance
; used for testing
;nul=min(abs(wvl-515.),in)
in=35
par[15]=spc[in] ;max(sp)

return, par
end 
