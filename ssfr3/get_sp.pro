; program to restore all spectra and collecting all taken at 6pm
; in order to compare to soundings

pro get_sp


dir='/argus/SSFR3/data/'
f=file_search(dir+'*/out/*calibs*',count=nf)
f=f[sort(f)]

nd=184
spz=fltarr(393,nd)
spn=fltarr(393,nd)

doy=findgen(nd)+2456048.5
caldat,doy,month,day,year
date=strarr(nd)

u=0
for i=0, nd-1 do begin
  date[i]=string(year[i],format='(I04)')+string(month[i],format='(I02)')+string(day[i],format='(I02)')
  print, date[i]
  n=strpos(f[u],date[i])
  if n lt 0 then continue ;if not found go to next date 
  print, 'restoring: '+f[u]
  restore, f[u]
  nul=min(abs(tmhrs-24.0),ind)
  spz[*,i]=zspectra[*,ind]
  spn[*,i]=nspectra[*,ind]
  u=u+1
endfor


save, spz,spn,nadlambda,zenlambda,date,filename='/home/leblanc/SSFR3/data/sp_at_sounding.out'
stop
end
