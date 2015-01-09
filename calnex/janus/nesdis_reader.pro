pro nesdis_reader, filein, utc, lat, lon, tau, ref, lwp

  hr = strmid(filein,9,2, /reverse_offset)
  mn = strmid(filein,7,2, /reverse_offset)
  sc = strmid(filein,5,2, /reverse_offset)
  utc= float(hr)+ float(mn)/60. + float(sc)/3600.

hdfid=hdf_sd_start(filein)

index=hdf_sd_nametoindex(hdfid,'pixel_latitude')
varid=hdf_sd_select(hdfid,index)
hdf_sd_getdata,varid,lat
HDF_SD_AttrInfo, varid, 6, data = scale
HDF_SD_AttrInfo, varid, 7, data = offset
HDF_SD_AttrInfo, varid, 8, data = fill
nan=where(lat eq fill[0], ct)
lat=scale[0]*(lat-offset[0])
if ct gt 0 then lat[nan]=!values.F_NAN

index=hdf_sd_nametoindex(hdfid,'pixel_longitude')
varid=hdf_sd_select(hdfid,index)
hdf_sd_getdata,varid,lon
HDF_SD_AttrInfo, varid, 6, data = scale
HDF_SD_AttrInfo, varid, 7, data = offset
HDF_SD_AttrInfo, varid, 8, data = fill
nan=where(lon eq fill[0], ct)
lon=scale[0]*(lon-offset[0])
if ct gt 0 then lon[nan]=!values.F_NAN

index=hdf_sd_nametoindex(hdfid,'experimental_goes_day_cloud_optical_depth_vis')
varid=hdf_sd_select(hdfid,index)
hdf_sd_getdata,varid,tau
HDF_SD_AttrInfo, varid, 8, data = fill
nan=where(tau eq fill[0], ct)
if ct gt 0 then tau[nan]=!values.F_NAN
 
index=hdf_sd_nametoindex(hdfid,'experimental_goes_day_cloud_particle_effective_radius')
varid=hdf_sd_select(hdfid,index)
hdf_sd_getdata,varid,ref
HDF_SD_AttrInfo, varid, 6, data = scale
HDF_SD_AttrInfo, varid, 7, data = offset
HDF_SD_AttrInfo, varid, 8, data = fill
nan=where(ref eq fill[0], ct)
ref=scale[0]*(ref)+offset[0]
if ct gt 0 then ref[nan]=!values.F_NAN
; units micron

index=hdf_sd_nametoindex(hdfid,'experimental_goes_day_cloud_liquid_water_path')
varid=hdf_sd_select(hdfid,index)
hdf_sd_getdata,varid,lwp
HDF_SD_AttrInfo, varid, 6, data = scale
HDF_SD_AttrInfo, varid, 7, data = offset
HDF_SD_AttrInfo, varid, 8, data = fill
nan=where(lwp eq fill[0], ct)
lwp=scale[0]*(lwp)+offset[0]
if ct gt 0 then lwp[nan]=!values.F_NAN
; units g/m^2

hdf_sd_end,hdfid
end
