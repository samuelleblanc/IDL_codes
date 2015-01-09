pro errors
; tool that reads in the error values and spits out a sav file
openr, 97, '/home/leblanc/libradtran/output/aero/rtm_errors_1.dat'
line=' '
readf, 97, line
wvl=fltarr(13)
ssa=fltarr(13)
asy=fltarr(13)
asy2=fltarr(13)
albedo=fltarr(13)
tau=fltarr(13)
ssa_err=ssa
asy_err=asy
asy2_err=asy2
albedo_err=albedo
tau_err=tau
g=fltarr(11,13)

readf,97,g
;for i=0,12 do readf, 97, wvl[i],ssa[i],asy[i],asy2[i],albedo[i],tau[i],ssa_err[i],asy_err[i],asy2_err[i],albedo_err[i],tau_err[i]
close, 97

stop

end
