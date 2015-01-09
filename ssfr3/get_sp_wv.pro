; program to restore all spectra and collecting all taken at the same time as the measurement of precipitable water vapor from GPS stations
; Only open those with clear days

pro get_sp_wv

dir='/argus/SSFR3/data/'
f=file_search(dir+'*/out/*calibs*',count=nf)
f=f[sort(f)]

; load the pw file to get the proper times
;pw=read_ascii('/home/leblanc/SSFR3/wx/DSRCnrt_2012.plt')
;pw=read_ascii('/home/leblanc/SSFR3/wx/SA60nrt_2012.plt')
pw=read_ascii('/home/leblanc/SSFR3/wx/SA67nrt_2012.plt')

;filter for the days of clear skies
;fl_cl=[13,23,38,39,40,41,51,52,53,64,70,71,72,80,109,$
;       116,117,126,127,130,131,135,136,151,154,166,169,170]

fl_cl=[8,13,14,130,131,136,154,166,169]

doy_cl=float(fl_cl)+julday(05,01,2012)-julday(01,00,2012)

;fl=where(floor(pw.field1[0,*]) eq floor(doy_cl))

big=60000
spz=fltarr(393,big)
spn=fltarr(393,big)
doys=fltarr(big)
water=fltarr(big)
water_err=fltarr(big)
tau=fltarr(1024,big)

nd=184
doy=findgen(nd)+julday(05,01,2012)
caldat,doy,month,day,year
date=strarr(nd)

u=0
z=0
for i=0, nd-1 do begin
  
  date[i]=string(year[i],format='(I04)')+string(month[i],format='(I02)')+string(day[i],format='(I02)')
  print, date[i]
  n=strpos(f[u],date[i])
  if n lt 0 then continue ;if not found go to next date 
  nul=where(i eq fl_cl,nf)
  ;stop
  if nf lt 1 then begin
    u=u+1
    continue
  endif
  print, 'restoring: '+f[u]
  restore, f[u]

  ; now find the times of measured spectra that corresponds to the pw estimates
  hrs=doy[i]-julday(01,00,2012)+tmhrs/24.0
  npw=where(pw.field01[0,*] gt doy[i]-julday(01,00,2012)+14.5/24.0 and pw.field01[0,*] lt doy[i]-julday(01,00,2012)+23.5/24.,ns)
  ;stop
  if ns gt 0 then begin
    data=read_skywatch(name='/argus/SSFR3/data/tau/sun_tau_'+string(year[i]-2000.,format='(I02)')+'_'+string(month[i],format='(I02)')+'_'+string(day[i],format='(I02)')+'.dat',instrument='tau')
    print, 'processing',ns,' pw points'
    for j=0, ns-1 do begin
      nul=min(abs(hrs-pw.field01[0,npw[j]]),ind)
      nul=min(abs(doy[i]-julday(01,00,2012)+6./24.+data.sec_of_day/3600./24.-pw.field01[0,npw[j]]),ita)
      tau[*,z]=data.tau[*,ita]
;stop
      for v=0, 255 do spz[v,z]=mean(zspectra[v,(ind-60):(ind+60)],/nan)
      for v=0, 255 do spn[v,z]=mean(nspectra[v,(ind-60):(ind+60)],/nan)
;stop
      doys[z]=pw.field01[0,npw[j]]
      water[z]=pw.field01[1,npw[j]]
      water_err[z]=pw.field01[2,npw[j]]
      z=z+1
    endfor
  endif

  u=u+1
endfor

spz=spz[*,0:z-1]
spn=spn[*,0:z-1]
doys=doys[0:z-1]
water=water[0:z-1]
water_err=water_err[0:z-1]
tau=tau[*,0:z-1]
wvl=data.wavelength
nw=where(water eq -9.9,nn)
if nn gt 0 then water[nw]=!values.f_nan

save, spz,spn,nadlambda,zenlambda,date,doys,water, water_err,tau,wvl,filename='/home/leblanc/SSFR3/data/sp_at_wv3.out'
stop
end
