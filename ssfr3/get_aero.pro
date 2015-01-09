; program to go through all the sunphotometer data to get the optical depth
; at 6pm everyday
; and save to file to compare to sounding

@read_skywatch.pro
pro get_aero

dir='/argus/SSFR3/data/tau/'
f=file_search(dir+'sun_tau_*.dat',count=nf)
f=f[sort(f)]

nd=184
tau=fltarr(1024,nd)

doy=findgen(nd)+2456048.5
caldat,doy,month,day,year
date=strarr(nd)

u=0
for i=0, nd-1 do begin
  date[i]=string(year[i],format='(I04)')+string(month[i],format='(I02)')+string(day[i],format='(I02)')
  dd=string(year[i]-2000,format='(I02)')+'_'+string(month[i],format='(I02)')+'_'+string(day[i],format='(I02)')
  print, date[i]
  n=strpos(f[u],dd)
  if n lt 0 then continue ;if not found go to next date 
  print, 'reading: '+f[u]
  data=read_skywatch(name=f[u],instrument='tau')
  nul=min(abs(data.sec_of_day/3600.-18.0),ind)
  tau[*,i]=data.tau[*,ind]
  u=u+1
endfor

wvl=data.wavelength

save, tau,wvl,date,filename='/home/leblanc/SSFR3/data/aero_at_sounding.out'
stop
end
