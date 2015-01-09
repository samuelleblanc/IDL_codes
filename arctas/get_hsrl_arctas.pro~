pro get_hsrl_arctas,dir,date,uth,lat,lon,n,z_o,dz,z_n,ext532
hsrl=findfile(dir+date+'/HSRL*.ICT')
n_hsrl=n_elements(hsrl)
if strlen(hsrl[0]) gt 1 and n_hsrl ge 1 then begin
  if n_hsrl gt 1 then message,'More than one HSRL file!'
  openr,u_hsrl,hsrl[0],/get_lun
  string=' '
  readf,u_hsrl,number
  for i=0,number-2 do begin
    readf,u_hsrl,string
  endfor
  dim=file_lines(hsrl[0])-1-number
  uth=fltarr((dim+1)/2)
  n=fltarr((dim+1)/2)
  z_o=n & dz=n & z_n=n
  hr=n & mins=n & sec=n
  lat=n & lon=n 
  ext532=fltarr((dim+1)/2, 400)
  for i=0,(dim-1)/2 do begin
    readf,u_hsrl,d1,d2,d3,u1,u2,u3,u4,u5,u6,u7,u8,u9,d
    if u1 ne -9999.0 then n[i]=u1 else n[i]=!values.f_nan
    if u2 ne -9999.0 then z_o[i]=u2 else z_o[i]=!values.f_nan
    if u3 ne -9999.0 then dz[i]=u3 else dz[i]=!values.f_nan
    if u4 ne -9999.0 then z_n[i]=u4 else z_n[i]=!values.f_nan
    if u5 ne -9999.0 then hr[i]=u5 else hr[i]=!values.f_nan
    if u6 ne -9999.0 then mins[i]=u6 else mins[i]=!values.f_nan
    if u7 ne -9999.0 then sec[i]=u7 else sec[i]=!values.f_nan
    if u8 ne -9999.0 then lat[i]=u8 else lat[i]=!values.f_nan
    if u9 ne -9999.0 then lon[i]=u9 else lon[i]=!values.f_nan
    uth[i]=hr[i]+mins[i]/60.+sec[i]/3600.
    
    fl=fltarr(n[i])
    readf,u_hsrl,fl
    for j=0, n[i]-1 do $
     if fl[j] ne -9999.0 then ext532[i,j]=fl[j] else ext532[i,j]=!values.f_nan
  endfor
  free_lun,u_hsrl
  print,'Processed',(dim-1)/2,' data points from HSRL-File.'
endif else message, 'No HSRL file'

end
