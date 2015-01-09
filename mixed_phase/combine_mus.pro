; program to combine the results of the different mus

pro combine_mus,lbl

if n_elements(lbl) lt 1 then lbl='20120524'
f=file_search('/argus/roof/SSFR3/model/sp_mix3_sza_'+lbl+'_mu??.out')
nf=n_elements(f)
if nf eq 0 then message, 'no files'
f=f[sort(f)]

restore, f[0]
rad515=fltarr(n_elements(tau),n_elements(refl),n_elements(refi),n_elements(wp),nf)
wvl=515.
for s=0,nf- 1 do begin
  print, 'restoring: '+f[s]
  restore, f[s]
  rad515[*,*,*,*,s]=sp[*,*,*,0,*]
endfor

mus=cos(szas*!dtor)
print, 'saving'
save, tau, refl,refi, mus, wp, szas,wvl,rad515, filename='/argus/roof/SSFR3/model/mix3_sza_'+lbl+'.out'

stop
end
