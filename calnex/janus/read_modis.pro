pro read_modis, tau_t, tau_a, reff_t, reff_a, lwp_t, lwp_a, utc_t, utc_a, lat_t, lat_a, lon_t, lon_a

dir='/data/seven/schmidt/calnex/modis/20100516/'
file_t=dir+'MOD06_L2.A2010136.1915.051.2010137025613.hdf'
file_aa=dir+'MYD03.A2010136.2050.005.2010137163504.hdf'      ;geolocation
file_tt=dir+'MOD03.A2010136.1915.005.2010137015856.hdf'      ;geolocation
file_a=dir+'MYD06_L2.A2010136.2050.051.2010137181931.hdf'

modis,file_a,tau_a,reff_a
geo,file_aa,lat_a,lon_a,time_a
print,'AQUA DONE.'

modis,file_t,tau_t,reff_t
geo, file_tt, lat_t, lon_t, time_t
print, 'TERRA DONE'

lwp_t=2.*tau_t*reff_t/3.
lwp_a=2.*tau_a*reff_a/3.

utc_t=(time_t-julday(5,16,2010))*24.d
utc_a=(time_a-julday(5,16,2010))*24.d
end


pro geo,file,latmodis,lonmodis,time ; gives the geolocation of the file
id=hdf_sd_start(file)
index=hdf_sd_nametoindex(id,'Latitude') ;2030
varid=hdf_sd_select(id,index)
hdf_sd_getdata,varid,latmodis
HDF_SD_AttrInfo, varid, 2, data = fill
nan=where(latmodis eq fill[0], ct)
if ct gt 0 then latmodis[nan]=!values.F_NAN

index=hdf_sd_nametoindex(id,'Longitude')
varid=hdf_sd_select(id,index)
hdf_sd_getdata,varid,lonmodis
HDF_SD_AttrInfo, varid, 2, data = fill
nan=where(lonmodis eq fill[0], ct)
if ct gt 0 then lonmodis[nan]=!values.F_NAN

index=hdf_sd_nametoindex(id,'EV start time') ;203
varid=hdf_sd_select(id,index)
hdf_sd_getdata,varid,timemodis
timemodis=timemodis-7.0
time=fltarr(n_elements(lonmodis[0,*]))
timemodis=[timemodis,timemodis[n_elements(timemodis)-1]+abs(timemodis[n_elements(timemodis)-1]-timemodis[n_elements(timemodis)-2])]
for i=0, n_elements(timemodis)-2 do begin
  for j=0,9 do begin
    time[i*10+j]=timemodis[i] + float(j)*(timemodis[i+1]-timemodis[i])/10.
  endfor
endfor
time=time/3600.D/24.d + julday(1,1,1993) ;convert to idl julian day utc
end


pro modis,file,taumodis,reffmodis
id=hdf_sd_start(file)
index=hdf_sd_nametoindex(id,'Cloud_Optical_Thickness')
varid=hdf_sd_select(id,index)
hdf_sd_getdata,varid,taumodis
HDF_SD_AttrInfo, varid, 1, data = fill
HDF_SD_AttrInfo, varid, 4, data = scale
nan=where(taumodis eq fill[0], ct)
taumodis=taumodis*scale[0]
if ct gt 0 then taumodis[nan]=!values.F_NAN

index=hdf_sd_nametoindex(id,'Cloud_Effective_Radius')
varid=hdf_sd_select(id,index)
hdf_sd_getdata,varid,reffmodis
HDF_SD_AttrInfo, varid, 1, data = fill
HDF_SD_AttrInfo, varid, 4, data = scale
nan=where(reffmodis eq fill[0], ct)
reffmodis=reffmodis*scale[0]
if ct gt 0 then reffmodis[nan]=!values.F_NAN
;units micron
end
