; program that retrieves opticla depth and effective radius (diameter) from a set of albedos
; made for online processing during attrex

pro attrex_retrieval,sza,albedo,rt_wl,cod_ice,ref_ice,cod_liquid,ref_liquid,residual_ice,residual_liquid

lib_dir='/localhome/leblanc/data/attrex/pro/lib/'

mu              = cos(sza*!dtor)
cod_ice         = fltarr(n_elements(sza))
ref_ice         = fltarr(n_elements(sza))
cod_liquid      = fltarr(n_elements(sza))
ref_liquid      = fltarr(n_elements(sza))
residual_ice    = fltarr(n_elements(sza))
residual_liquid = fltarr(n_elements(sza))
print, 'Restoring ice library at: '+lib_dir+'ATTREX_icelib.dat'
restore, lib_dir+'ATTREX_icelib.dat'
print, 'Restoring liquid water library at: '+lib_dir+'ATTREX_liquidcld.dat'
restore, lib_dir+'ATTREX_liquidcld.dat'

for i=0, n_elements(sza)-1 do begin
  fail=0
  if mu[i] lt 0.05 then fail=1
  kk=where(albedo[*,i] gt 1.0,nn)	  
  if nn gt 0 then fail=1
  kk=where(albedo[*,i] le 0.0,nn)
  if nn gt 0 then fail=1
  if fail then begin
    print, 'Retrieval failed'
    cod_ice[i]    = !values.f_nan
    ref_ice[i]    = !values.f_nan
    cod_liquid[i] = !values.f_nan
    ref_liquid[i] = !values.f_nan
    residual_ice[i]= !values.f_nan
    residual_liquid[i]= !values.f_nan
;stop
  endif else begin 
    nul=min(abs(mu-MUSHIRES),/nan,mu_index)
    table_vis=albedo870cube[mu_index,*,*]
    table_nir=albedo1600cube[mu_index,*,*]
    
    residual_ice[i]=min((albedo[0,i]-table_vis)^2.0 + (albedo[1,i]-table_nir)^2.0,index)
    tn= index mod n_elements(taushires)
    dn= floor(index/n_elements(taushires))
    
    cod_ice[i]=taushires[tn]
    ref_ice[i]=deffshires[dn]/2.0
    
    table_vis=ALBEDOLIQUID870[mu_index,*,*] ;liquid water
    table_nir=ALBEDOLIQUID1600[mu_index,*,*] ;liquid water
    
    residual_liquid[i]=min((albedo[0,i]-table_vis)^2.0 + (albedo[1,i]-table_nir)^2.0,index)
    tn= index mod n_elements(TAUSHIRESLIQ)
    dn= floor(index/n_elements(TAUSHIRESLIQ))
    
    cod_liquid[i]=TAUSHIRESLIQ[tn]
    ref_liquid[i]=REFFSHIRESLIQ[dn]
  endelse
endfor
end

