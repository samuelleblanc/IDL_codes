pro aqua,f, latmodis,lonmodis,surmodis,taumodis,fracmodis,forcingmodis,reffmodis
id=hdf_sd_start(f)
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

pro terra,f,latmodis,lonmodis,surmodis,taumodis,fracmodis,forcingmodis,reffmodis
id=hdf_sd_start(f)
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

