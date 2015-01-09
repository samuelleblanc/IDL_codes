pro read_modis, latin, lonin, tau_t, tau_a, reff_t, reff_a, lwp_t, lwp_a, utc_t, utc_a, lat_t, lat_a, lon_t, lon_a
dir='C:\CalNex\MODIS\'
file_t=dir+'MOD06_L2.A2010136.1915.051.2010137025613.hdf'
file_aa=dir+'MYD03.A2010136.2050.005.2010137163504.hdf'
file_tt=dir+'MOD03.A2010136.1915.005.2010137015856.hdf'
file_a=dir+'MYD06_L2.A2010136.2050.051.2010137121931.hdf'

aqua,file_a,latmodis,lonmodis,surmodis,taumodis,fracmodis,forcingmodis,reffmodis ; aqua
geo,file_aa,latmodis,lonmodis

terra,file_t,lat_t,lon_t,sur_t,tau_t,frac_t,forcing_t,reff_t ;terra
geo, file_tt, lat_t, lon_t

same_geo=where(lat_t ge latin - 0.25 and lat_t le latin +0.25 and lon_t ge lonin - 0.25 and lon_t le lonin + 0.25)

;terra output at location
lat_t=lat_t[same_geo]
lon_t=lon_t[same_geo]
tau_t=tau_t[same_geo]
reff_t=reff_t[same_geo]
lwp_t=2.*tau_t*reff_t/3.
utc_t=20.833
;aqua outpu at location
same_a=where(latmodis ge latin -0.25 and latmodis le latin +0.25 and lonmodis ge lonin-0.25 and lonmodis le lonin+0.25)
lat_a=latmodis[same_a]
lon_a=lonmodis[same_a]
tau_a=taumodis[same_a]
reff_a=reffmodis[same_a]
lwp_a=2.*tau_a*reff_a/3.
utc_a=19.25
end


pro geo,file,latmodis,lonmodis
id=hdf_sd_start(file)
index=hdf_sd_nametoindex(id,'Latitude')
varid=hdf_sd_select(id,index)
hdf_sd_getdata,varid,latmodis
index=hdf_sd_nametoindex(id,'Longitude')
varid=hdf_sd_select(id,index)
hdf_sd_getdata,varid,lonmodis
end



pro aqua,file,latmodis,lonmodis,surmodis,taumodis,fracmodis,forcingmodis,reffmodis ; aqua
id=hdf_sd_start(file)
index=hdf_sd_nametoindex(id,'Latitude')
varid=hdf_sd_select(id,index)
hdf_sd_getdata,varid,latmodis
index=hdf_sd_nametoindex(id,'Longitude')
varid=hdf_sd_select(id,index)
hdf_sd_getdata,varid,lonmodis
index=hdf_sd_nametoindex(id,'Surface_Type')
varid=hdf_sd_select(id,index)
hdf_sd_getdata,varid,surmodis
index=hdf_sd_nametoindex(id,'Cloud_Optical_Thickness')
varid=hdf_sd_select(id,index)
hdf_sd_getdata,varid,taumodis
index=hdf_sd_nametoindex(id,'Cloud_Fraction')
varid=hdf_sd_select(id,index)
hdf_sd_getdata,varid,fracmodis
index=hdf_sd_nametoindex(id,'Spectral_Cloud_Forcing')
varid=hdf_sd_select(id,index)
hdf_sd_getdata,varid,forcingmodis
index=hdf_sd_nametoindex(id,'Cloud_Effective_Radius')
varid=hdf_sd_select(id,index)
hdf_sd_getdata,varid,reffmodis

taumodis =taumodis *0.01
reffmodis=reffmodis*0.01
end

pro terra,file,latmodis,lonmodis,surmodis,taumodis,fracmodis,forcingmodis,reffmodis
id=hdf_sd_start(file)
index=hdf_sd_nametoindex(id,'Latitude')
varid=hdf_sd_select(id,index)
hdf_sd_getdata,varid,latmodis
index=hdf_sd_nametoindex(id,'Longitude')
varid=hdf_sd_select(id,index)
hdf_sd_getdata,varid,lonmodis
index=hdf_sd_nametoindex(id,'Surface_Type')
varid=hdf_sd_select(id,index)
hdf_sd_getdata,varid,surmodis
index=hdf_sd_nametoindex(id,'Cloud_Optical_Thickness')
varid=hdf_sd_select(id,index)
hdf_sd_getdata,varid,taumodis
index=hdf_sd_nametoindex(id,'Cloud_Fraction')
varid=hdf_sd_select(id,index)
hdf_sd_getdata,varid,fracmodis
index=hdf_sd_nametoindex(id,'Spectral_Cloud_Forcing')
varid=hdf_sd_select(id,index)
hdf_sd_getdata,varid,forcingmodis
index=hdf_sd_nametoindex(id,'Cloud_Effective_Radius')
varid=hdf_sd_select(id,index)
hdf_sd_getdata,varid,reffmodis

taumodis =taumodis *0.01
reffmodis=reffmodis*0.01
end

