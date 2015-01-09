;program to read in the cloud top and bottom height from the GOES netcdf pixel level files retrieved from larc 
; at: http://cloudsgate2.larc.nasa.gov/prod/ under goest-east, visst-pixel-netcdf, 2012, ...

pro read_cld_top

dir='/argus/SSFR3/goes/'

;boulder's lat and lon
latb=40.007915497
lonb=-105.268249512

;get a list of netcdf files to read
file=file_search(dir+'*.PX.*')
nf=n_elements(file)

print, 'found '+nf+' files'

cod=fltarr(nf)
def=fltarr(nf)
lwp=fltarr(nf)
top=fltarr(nf)
bot=fltarr(nf)

for i=0, nf-1 do begin
  print, 'opening file:'+file[i]
  fileid=ncdf_open(file[i])
  ncdf_varget,fileid,15,lat
  ncdf_varget,fileid,16,lon
  ncdf_varget,fileid,28,sz ;particle size (microns)
  ncdf_varget,fileid,29,path ; liquid water path (g/m^2)
  ncdf_varget,fileid,27,tau ; visible optical depth
  ncdf_varget,fileid,34,up ; top cloud height (km)
  ncdf_varget,fileid,36,dn ; bottom cloud height (km)
  ncdf_close,fileid
  

  ; now get the closest point
  nul=min(abs(lat-latb)+abs(lon-lonb),ns,/nan)
  cod[i]=tau[ns]
  def[i]=sz[ns]
  lwp[i]=path[ns]
  top[i]=up[ns]
  bot[i]=dn[ns]
 print, cod[i],def[i],lwp[i],top[i],bot[i]
endfor

save, cod,def,lwp,top,bot,file,filename='./goes_cld_top.out'

stop
end

