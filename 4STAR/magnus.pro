FUNCTION MAGNUS,relh,tk     ; relh = rel.hum.[%]; tk = temperature [K]
;uses Magnus Teton approximation to convert water vapor RH% to #/cm3

t=tk-273.15
; Magnus-Teton approximation for saturation vapor pressure (in hPa)

mt    = 10.^((7.5*t)/(t+237.3)+0.7858) ;temperature (t)in Celsius  --> saturation vapor pressure
ph2o  = relh/100.*mt   ; actual water vapor pressure
;get number density of water (#/cm3)
rstar = 8.314              ; J/mol/K
Na    = 6.02297*10^23.     ; molecules/mol
h2o=(ph2o*100.)/(rstar*tk) ; mol/m3
h2o=h2o*Na/1.0e6           ; #/cm3

return,h2o
end
