; program to load calibspcs.out files and change them to netcdf files
@write_netcdf.pro

pro out2cdf,file,fileout
restore, file
d={nadlambda:nadlambda,nspectra:nspectra,sat:sat,status:status,tmhrs:tmhrs,zenlambda:zenlambda,zspectra:zspectra}
write_netcdf,d,fileout,st
end
