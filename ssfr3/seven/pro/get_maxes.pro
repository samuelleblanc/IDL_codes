; program to get the maximum radiance wavelength of the modeled files
; 

pro get_maxes


restore, '/argus/roof/SSFR3/model/sp_hires4_20120524.out'

mwvl=fltarr(n_elements(tau),n_elements(ref),2)

for t=0, n_elements(tau)-1 do begin
  for r=0, n_elements(ref)-1 do begin
    for w=0,1 do begin
     nul=max(sp[t,r,*,w],n)
     mwvl[t,r,w]=zenlambda[n]
    endfor
  endfor
endfor
   
print, max(mwvl), min(mwvl),mean(mwvl),median(mwvl),stddev(mwvl)



stop
end
