;+
; NAME:
;   ssfr3_retrieval
;
; PURPOSE:
;   To retrieve cloud properties from SSFR3 measurements using Patrick Mcbride's retrieval scheme based on transmitted radiance
;
; CATEGORY:
;   SSFR3 online processing, cloud optical properties retrieval
;
; CALLING SEQUENCE:
;   ssfr3_retrieval,cfg_file,sza,tx,rt_wl,cod_ice,ref_ice,cod_liquid,ref_liquid,residual_ice,residual_liquid
;   - cfg_file:   config file, to get the paths of the look up table files
;   - sza:        solar zenith angle (determines which look up table to use)
;   - tx:         measurements of transmittance
;   - rt_wl:      retrieval wavelengths
;   - cod_ice:    returns ice cloud optical depth
;   - ref_ice:    returns ice cloud effective radius 
;   - cod_liquid: returns liquid cloud optical depth
;   - ref_liquid: returns liquid cloud effective radius
;   - residual_ice: returns the residual from the ice cloud retrieval
;   - residual_liquid: returns the residual from the liquid cloud retrieval
;
;
; OUTPUT:
;   liquid water cloud drop size effective radius 
;   liquid water cloud optical depth
;   ice cloud drop size effective radius
;   ice cloud optical depth
;
; KEYWORDS:
;   - none
;
; DEPENDENCIES:
;   - cfg.pro
;
; NEEDED FILES:
;   - cloud properties look up table
;
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
; Written:  Patrick Mcbride, LASP CU Boulder, Date unknown, Boulder
; Modified: by Samuel LeBlanc, September, 2011, Boulder Colorado
;           - modified the retrieval code to run for ATTREX online.
; Modified: by Samuel LeBlanc, May 19th, 2012, Boulder, Colorado
;           - modified the retrieval code to run using SSFR3 data instead of other SSFRs          
;          
;---------------------------------------------------------------------------


; program that retrieves optical depth and effective radius (diameter) from a set of albedos
; made for online processing of ssfr3 data
@cfg.pro
pro ssfr3_retrieval,cfg_file,sza,tx,rt_wl,cod_ice,ref_ice,cod_liquid,ref_liquid,residual_ice,residual_liquid

icelib    = cfg(cfg_file,'icelib')
liquidlib = cfg(cfg_file,'liquidlib')

mu              = cos(sza*!dtor)
cod_ice         = fltarr(n_elements(sza))
ref_ice         = fltarr(n_elements(sza))
cod_liquid      = fltarr(n_elements(sza))
ref_liquid      = fltarr(n_elements(sza))
residual_ice    = fltarr(n_elements(sza))
residual_liquid = fltarr(n_elements(sza))
print, 'Restoring ice library at: '+icelib
restore, icelib
print, 'Restoring liquid water library at: '+liquidlib
restore, liquidlib

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

