FUNCTION BODHAINE,WV0,PZ1,PZ2
;WV0 = wavelength (in microns)
;PZ1 = Pressure of lower layer (hPa)
;PZ2 = Pressure of upper layer (hPa; PZ1 > PZ2)

num=1.0455996 - 341.29061*WV0^(-2.) - 0.90230850*WV0^(2.)
den=1 + 0.0027059889*WV0^(-2.) - 85.968563*WV0^(2.)
tauray =0.00210966*(num/den)*(PZ1-PZ2)/1013.25

return,tauray
end
