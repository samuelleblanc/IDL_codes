pro shipnav1min,path,utcin,lat1,lon1,rrr1,ccn1

files=file_search(path,count=nf)

utc=fltarr(3600)
lat=fltarr(3600)
lon=fltarr(3600)
rrr=fltarr(3600)
ccn=fltarr(3600)
j=0
for i=0,nf-1 do begin
  openr,us,files[i],/get_lun
  str=''
  readf,us,str
  while not eof(us) do begin
    readf,us,minute0,lat0,lon0,x,x,x,x,x,x,rain_rate0,x,ccn0
    utc[j]=minute0/3600.
    lat[j]=lat0
    lon[j]=lon0
    rrr[j]=rain_rate0
    ccn[j]=ccn0
    j=j+1
  endwhile
  free_lun,us
endfor

utc1=utc[0:j-1]
lat1=interpol(lat[0:j-1],utc1,utcin)
lon1=interpol(lon[0:j-1],utc1,utcin)
rrr1=interpol(rrr[0:j-1],utc1,utcin)
ccn1=interpol(ccn[0:j-1],utc1,utcin)

end