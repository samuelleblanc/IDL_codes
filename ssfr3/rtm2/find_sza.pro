; program to find the correct measurements linked to a specific sza
; and correct for the differences in solfac

@zensun.pro
pro find_sza
dir='/media/usbdisk3/DC3/SSFR3/'
date='20120814'
dirout='/home/leblanc/DC3_SEAC4RS/SSFR3/'
lat=40.007916667
lon=-105.26825

;start by finding the appropriate save files
f=file_search(dir+date+'/out/'+date+'_TS_*.out',count=nf)
if nf le 0 then message, 'No files found'
f=f[sort(f)]

; get the values of sza used
dateref='20120810'
ntime=20
time=findgen(ntime)/5.+14. ; UTC time of interest

doyref=julian_day(fix(strmid(dateref,0,4)),fix(strmid(dateref,4,2)),fix(strmid(dateref,6,2)))
zensun, doyref, time,lat, lon, szaref, azimuth, solfacref

; now open up the files to find all the selected times
doy=julian_day(fix(strmid(date,0,4)),fix(strmid(date,4,2)),fix(strmid(date,6,2)))

nadsp=fltarr(ntime,393)
zensp=fltarr(ntime,393)
times=fltarr(ntime)
sols=fltarr(ntime)

cont=1
i=0
jj=0
while cont do begin
  restore, f[i]
  print, 'restoring '+f[i]
  zensun, doy, tmhrs_big, lat, lon, sza, azimuth, solfac
  szamax=max(sza,/nan)
  szamin=min(sza,/nan)
  ns=where(szaref le szamax and szaref ge szamin,nsza)
  if nsza gt 0 then begin
    print, 'Found '+string(nsza)+' points in this file'
    for j=0, nsza-1 do begin    
      if jj ge ntime then begin
        cont=0
        continue
      endif
      nul=min(abs(szaref[jj]-sza),t,/nan)
      if nul lt 0.5 then begin ;check if that values hasn't already been processed
        nadsp[jj,*]=nad_spect[t,*]*solfac[t]/solfacref[jj]
        zensp[jj,*]=zen_spect[t,*]*solfac[t]/solfacref[jj]
        times[jj,*]=tmhrs_big[t]
        sols[jj]=solfac[t]
        jj++
      endif

    endfor
  endif else if szamax lt max(szaref) then cont=1 else cont=0
  i++
endwhile

print, 'Now saving to '+dir+date+'/'+date+'_sample.out and '+dirout+'/'+date+'/'+date+'_sample.out'
save, nadsp,zensp,times,sols,zenlambda,nadlambda,filename=dir+date+'/'+date+'_sample.out'
spawn , 'mkdir '+dirout+'/'+date
spawn, 'cp '+dir+date+'/'+date+'_sample.out '+dirout+'/'+date+'/'+date+'_sample.out'
stop
end