pro test
hsrl='/home/seven/schmidt/gom/20060913/20060913_HSRL_subset.hdf'
get_hsrl,hsrl,utc,alt,lat,lon,gnd,z,ext532,ext1064,T,p,Nprofile,Nabove
nk =n_elements(utc)
tau532=fltarr(nk)
for k=0,nk-1 do begin
  ind=where(ext532[*,k] ge 0,ni)
  if ni ge 1 then tau532[k]=int_tabulated(z[ind],ext532[ind,k],/sort)*0.001
endfor
tau1064=fltarr(nk)
for k=0,nk-1 do begin
  ind=where(ext1064[*,k] ge 0,ni)
  if ni ge 1 then tau1064[k]=int_tabulated(z[ind],ext1064[ind,k],/sort)*0.001
endfor
window,0,retain=2
plot,utc,tau532
oplot,utc,tau1064,color=70
stop
end

pro get_hsrl,hsrl,utc,alt,lat,lon,gnd,z,ext532,ext1064,T,p,Nprofile,Nabove,aot
hdfid=hdf_sd_start(hsrl)

index=hdf_sd_nametoindex(hdfid,'gps_time')
varid=hdf_sd_select(hdfid,index)
hdf_sd_getdata,varid,utc

index=hdf_sd_nametoindex(hdfid,'gps_alt')
varid=hdf_sd_select(hdfid,index)
hdf_sd_getdata,varid,alt

index=hdf_sd_nametoindex(hdfid,'gps_lat')
varid=hdf_sd_select(hdfid,index)
hdf_sd_getdata,varid,lat

index=hdf_sd_nametoindex(hdfid,'gps_lon')
varid=hdf_sd_select(hdfid,index)
hdf_sd_getdata,varid,lon

index=hdf_sd_nametoindex(hdfid,'DEM_altitude')
varid=hdf_sd_select(hdfid,index)
hdf_sd_getdata,varid,gnd

index=hdf_sd_nametoindex(hdfid,'Altitude')
varid=hdf_sd_select(hdfid,index)
hdf_sd_getdata,varid,z

index=hdf_sd_nametoindex(hdfid,'532_ext')
varid=hdf_sd_select(hdfid,index)
hdf_sd_getdata,varid,ext532

index=hdf_sd_nametoindex(hdfid,'1064_ext')
varid=hdf_sd_select(hdfid,index)
hdf_sd_getdata,varid,ext1064

index=hdf_sd_nametoindex(hdfid,'Temperature')
varid=hdf_sd_select(hdfid,index)
hdf_sd_getdata,varid,T

index=hdf_sd_nametoindex(hdfid,'Pressure')
varid=hdf_sd_select(hdfid,index)
hdf_sd_getdata,varid,p

index=hdf_sd_nametoindex(hdfid,'Number_Density')
varid=hdf_sd_select(hdfid,index)
hdf_sd_getdata,varid,Nprofile

index=hdf_sd_nametoindex(hdfid,'Sum_Number_Density_Above_HSRL')
varid=hdf_sd_select(hdfid,index)
hdf_sd_getdata,varid,Nabove

index=hdf_sd_nametoindex(hdfid,'AOT_hi')
varid=hdf_sd_select(hdfid,index)
hdf_sd_getdata,varid,aot

hdf_sd_end,hdfid
end
