; program to run the transmission cloud retrieval over all time in a day
; uses the data from the roof top
; uses the calibspcs save files

@zensun.pro
@cloud_rtm.pro

pro run_retrieval, date

if n_elements(date) lt 1 then date='20120623'

dir='/argus/SSFR3/data/'+date+'/out/'
restore, dir+date+'_calibspcs.out'

lat=40.007916667
lon=-105.26825
doy=julian_day(fix(strmid(date,0,4)),fix(strmid(date,4,2)),fix(strmid(date,6,2)))
zensun, doy, tmhrs, lat, lon, sza, azimuth, solfac

doyref=270
zensun, doyref, tmhrs, lat, lon, szaref, azimuthref, solfacref

n=n_elements(tmhrs)
tau=fltarr(n)
ref=fltarr(n)

for i=0, n-1 do begin
  sp=zspectra[*,i]*solfac[i]/solfacref[i]
  cloud_rtm,sza[i],sp,zenlambda,t,r
  tau[i]=t
  ref[i]=r
  if (i mod 60) eq 0 then print, tmhrs[i],t,r 
endfor


save, tau,ref,tmhrs,filename=dir+date+'_cld_parms.out'
end
