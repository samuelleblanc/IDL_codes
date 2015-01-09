pro get_aats_all,dir,date,uth,lambda,tauh,cld,dtauh, coefficient
aats=findfile(dir+date+'/AATS*.ict')
n_aats=n_elements(aats)
lambda=[353.5,380.0,451.2,499.4,520.4,605.8,675.1,779.1,864.5,1019.1,1241.3,1558.5,2139.1]
if strlen(aats[0]) gt 1 and n_aats ge 1 then begin
  if n_aats gt 1 then message,'More than one AATS file!'
  openr,u_aats,aats[0],/get_lun
  string=' '
  readf,u_aats,number
;stop
  for i=0,number-1 do begin
    readf,u_aats,string
  endfor
  dim=file_lines(aats[0])-1-number
  uth=fltarr(dim)
  cld=fltarr(dim)
  coefficient=fltarr(dim,3)

  tauh=fltarr(dim,13)
  dtauh=fltarr(dim,13)
  for i=0,dim-2-number do begin
    readf,u_aats,utc0,dum,d,d,d,d,d,d,d,d,d,d,cld_flg,a2,a1,a0,$
          u1,u2,u3,u4,u5,u6,u7,u8,u9,u10,u11,u12,u13,$
          d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13
    uth[i]=utc0
    cld[i]=cld_flg
    if u1 ne -9999.0 then tauh[i,0]=u1 else tauh[i,0]=!values.f_nan
    if u2 ne -9999.0 then tauh[i,1]=u2 else tauh[i,1]=!values.f_nan
    if u3 ne -9999.0 then tauh[i,2]=u3 else tauh[i,2]=!values.f_nan
    if u4 ne -9999.0 then tauh[i,3]=u4 else tauh[i,3]=!values.f_nan
    if u5 ne -9999.0 then tauh[i,4]=u5 else tauh[i,4]=!values.f_nan
    if u6 ne -9999.0 then tauh[i,5]=u6 else tauh[i,5]=!values.f_nan
    if u7 ne -9999.0 then tauh[i,6]=u7 else tauh[i,6]=!values.f_nan
    if u8 ne -9999.0 then tauh[i,7]=u8 else tauh[i,7]=!values.f_nan
    if u9 ne -9999.0 then tauh[i,8]=u9 else tauh[i,8]=!values.f_nan
    if u10 ne -9999.0 then tauh[i,9]=u10 else tauh[i,9]=!values.f_nan
    if u11 ne -9999.0 then tauh[i,10]=u11 else tauh[i,10]=!values.f_nan
    if u12 ne -9999.0 then tauh[i,11]=u12 else tauh[i,11]=!values.f_nan
    if u13 ne -9999.0 then tauh[i,12]=u13 else tauh[i,12]=!values.f_nan

    if d1 ne -9999.0 then dtauh[i,0]=d1 else dtauh[i,0]=!values.f_nan
    if d2 ne -9999.0 then dtauh[i,1]=d2 else dtauh[i,1]=!values.f_nan
    if d3 ne -9999.0 then dtauh[i,2]=d3 else dtauh[i,2]=!values.f_nan
    if d4 ne -9999.0 then dtauh[i,3]=d4 else dtauh[i,3]=!values.f_nan
    if d5 ne -9999.0 then dtauh[i,4]=d5 else dtauh[i,4]=!values.f_nan
    if d6 ne -9999.0 then dtauh[i,5]=d6 else dtauh[i,5]=!values.f_nan
    if d7 ne -9999.0 then dtauh[i,6]=d7 else dtauh[i,6]=!values.f_nan
    if d8 ne -9999.0 then dtauh[i,7]=d8 else dtauh[i,7]=!values.f_nan
    if d9 ne -9999.0 then dtauh[i,8]=d9 else dtauh[i,8]=!values.f_nan
    if d10 ne -9999.0 then dtauh[i,9]=d10 else dtauh[i,9]=!values.f_nan
    if d11 ne -9999.0 then dtauh[i,10]=d11 else dtauh[i,10]=!values.f_nan
    if d12 ne -9999.0 then dtauh[i,11]=d12 else dtauh[i,11]=!values.f_nan
    if d13 ne -9999.0 then dtauh[i,12]=d13 else dtauh[i,12]=!values.f_nan
    
    if a0 ne -9999.0 then coefficient[i,0]=a0 else coefficient[i,0]=!values.f_nan
    if a1 ne -9999.0 then coefficient[i,1]=a1 else coefficient[i,1]=!values.f_nan
    if a2 ne -9999.0 then coefficient[i,2]=a2 else coefficient[i,2]=!values.f_nan


  endfor
  free_lun,u_aats
  print,'Processed',dim,' data points from AATS-File.'
endif else message, 'No AATS file'

end
