; program to run through a set of retrieved MODIS L3 surface albedo files to save the proper values
; uses the pixels selected from find_MODIS_pixel

pro get_MODIS_albedo
dir='/argus/sat/modis/Boulder/'
f=file_search(dir+'MOD09GA*h09v04*.hdf')
f=f[sort(f)]
nf=n_elements(f)
f2=file_search(dir+'MOD09GA*h09v05*.hdf')
f2=f2[sort(f2)]

wvl=[645.,859.,469.,555.,1240.,1640.,2130.]
albedo=fltarr(nf,7)
doy=[010,011]

;n=1396800 ;index retrieved from find_MODIS_pixel
ns=[1199,1164]*2
ns2=[0,528]*2

obj=Obj_New('NCDF_DATA',f[0])
for i=0, nf-1 do begin
  ;obj -> OpenFile, f[i]
  for j=0, 6 do begin
    print, f[i]
    obj -> OpenFile, f[i]
    var=obj->readvariable('sur_refl_b0'+string(j+1,format='(I1)')+'_1')
    var=var/10000./10000.
    print, f2[i]
    obj -> OpenFile, f2[i]
    var2=obj->readvariable('sur_refl_b0'+string(j+1,format='(I1)')+'_1')
    var2=var2/10000./10000.
    print, 'sur_refl_b0'+string(j+1,format='(I1)')+'_1'
    
    albedo[i,j]=(var[ns[0],ns[1]]+0.86*(var[ns[0],ns[1]-2]+var[ns[0],ns[1]+2])+0.5*(var[ns[0],ns[1]-4]+var[ns[0],ns[1]+4])+$
                0.6*(var[ns[0]-2,ns[1]-2]+var[ns[0]-2,ns[1]+2])+0.86*(var[ns[0]-2,ns[1]])+0.5*var[ns[0]-4,ns[1]]+$
 ;0.)/6.28
               0.86*(var2[ns2[0],ns2[1]])+0.6*(var2[ns2[0],ns2[1]-2]+var2[ns2[0],ns2[1]+2])+0.5*var2[ns2[0]+2,ns2[1]])/8.84
    print, i,j,var[ns[0],ns[1]],var2[ns2[0],ns2[1]],albedo[i,j]
    if finite(albedo[i,j]) ne 1 then stop
  endfor
endfor
Obj_Destroy, obj

fl=where(albedo gt 1.0, ns)
if ns gt 0 then albedo[fl]=!values.f_nan

save, wvl, albedo,doy,filename=dir+'modis_surface_albedo_avg_snow.out'
stop
end
