pro make_p3_retrieval_input,utc,nadlambda,nspectra,zenlambda,zspectra,sza,alt,lat,lon,path,date
  cloud_top   = float(cfg(path+'.cfg','rt_cloud_top_height'))
  cloud_thick = float(cfg(path+'.cfg','rt_cloud_thickness'))
  tau         = 20 ; initialize with this optical thickness
  ref         =  5  ; initialize with this effective radius
  cld         = [tau,ref,cloud_top,cloud_thick]
  rt_wl       = strsplit(cfg(path+'.cfg','rt_wavelengths'),' ,',escape='#',/extract)
  rt_wl       = float(rt_wl)
  surf_albedo = strsplit(cfg(path+'.cfg','rt_surf_albedo'),' ,',escape='#',/extract)
  surf_albedo = float(surf_albedo)
  if (n_elements(rt_wl) ne n_elements(surf_albedo)) then message, 'Number of wavelength must be equal to Number of surface albedo'
  doy=julian_day(strmid(date,0,4),strmid(date,4,2),strmid(date,6,2))

  mm=min(abs(zenlambda-500),i500)

  sm=10

  insitudata=reform(zspectra[i500,*])
  boxcar=-999.+0.*insitudata
  index=where(insitudata gt 0.)
  if index(0) ne -1 then boxcar(index)=smooth(insitudata(index),sm)
  variance=-999.+0.*insitudata
  variance(index)=sqrt((insitudata(index)-boxcar(index))^2/boxcar(index))
  insitudata=variance

  ;
  ; add variance of altitude
  ;

  gradient_alt1=abs(alt-shift(alt,1))
  gradient_alt2=abs(alt-shift(alt,2))

  flt =where(insitudata lt 0.005 and gradient_alt1 lt 5. and gradient_alt2 lt 5.)
  fltp=where(shift(flt,1)+1 eq flt and shift(flt,-1)-1 eq flt and shift(flt,2)+2 eq flt and shift(flt,-2)-2 eq flt)
  fltn= flt[fltp]
  fltp=where(shift(fltn,1)+1 eq fltn and shift(fltn,-1)-1 eq fltn)
  fltnn=fltn[fltp]
  fltn=fltnn  ;above straight leg

; cloud filter
  mm=min(abs(zenlambda-rt_wl[0]),iz1)
  mm=min(abs(nadlambda-rt_wl[0]),in1)

  flt_cl=where(nspectra[in1,fltn]/zspectra[iz1,fltn] gt 0.1,count)  ;all albedo greater than 0.1 (clouds)
  if count gt 0 then begin
    fltn=fltn[flt_cl]

    utcf=utc[fltn]
    szaf=sza[fltn]
    altf=alt[fltn]
    latf=lat[fltn]
    lonf=lon[fltn]

    print, 'They were '+string(n_elements(fltn))+' points above cloud'

    ; get the walevengths used for the retrieval
    nf =n_elements(fltn)
    nad=fltarr(nf,2)
    zen=fltarr(nf,2)
    for n=0,nf-1 do begin
      nad[n,*]=interpol(reform(nspectra[*,fltn[n]]),nadlambda,rt_wl)
      zen[n,*]=interpol(reform(zspectra[*,fltn[n]]),zenlambda,rt_wl)
    endfor 

    ; zspc=zspectra[*,fltn] & nspc=nspectra[*,fltn]
    save,file=path+'_RI.out',utcf,szaf,altf,latf,lonf,rt_wl,nad,zen,cld, doy, surf_albedo, tau, ref,fltn
    ; save,file=path+'_SP_flt.out', utc,sza,alt,lat,lon,nspc,zspc,nadlambda,zenlambda
  endif else begin
    print, 'No Above Cloud legs in this flight'
  endelse
; stop
end
