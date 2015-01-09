@rayleigh.pro
;@cfg.pro
pro get_cos,dir,date,cfg,utc,alt,sza,dc,facz,dz,facn,dn,wl,ray,BRDF=brdf

print,'** Entering polar_get_cos.pro'

; (0) load necessary input
extra=cfg(cfg,'extra') ; location of extraterrestial radiation file

thousand = dindgen(1001)  ; a thousand cosines from 0 to 1, multiplied by 1000 to make them usable as index
nwl = n_elements(wl)


; (1) Get downward Rayleigh radiation at wl[0] and wl[1] - at "TOA"
;r0=rayleigh(sza,20000.,wl[0],extra)
;r1=rayleigh(sza,20000.,wl[1],extra)
;ray=[transpose(r0),transpose(r1)]
nel=long(n_elements(sza))*2L
ray=fltarr(nel,nwl)  ; the extra element carries d_g, the diffuse fraction
for i=0,nwl-1 do ray[*,i] = rayleigh(sza,20000.0,wl[i],extra,/DIRDIFF)


; (2) Get cosine response zenith
cf=cfg(cfg,'coszen')
cosfile=file_search(cf,count=nc)
if nc lt 1 then message,'No cosine response file found.'
if nc gt 1 then message,'More than one cosine response file found.'

; if ".dat" file, then the expected format is ASCII textfile for one wavelength only
; if ".out" file, then the expected format is binary for all wavelengths

ending=strmid(cosfile[0],3,/REV)
if ending eq '.dat' then begin
  openr,ul,cosfile[0],/get_lun
  cc=indgen(1001)
  print,'Open (1 wl, zenith): ',cosfile[0]
  dcl=fltarr(1001) & ccl=fltarr(1001)
  for i=0,1000 do begin
    readf,ul,a,b
    dcl[i]=a & ccl[i]=b
  endfor
  fcz=interpol(ccl,dcl,cc)
  free_lun,ul
  dz=500./int_tabulated(dcl,ccl) ; diffuse correction factor for zenith
endif
if ending eq '.out' then begin
  print,'Open all-wl file, zenith: ',cosfile[0]
  restore,cosfile[0]   ; should contain variables cosWL (lambda grid) and cosFAC (cos resp f for ALL wl)
                       ;                                                  cosFAC[wl,angle]
                       ;                                                   wl: Si+NIR merged at 950 nm
                       ;                                                   angle: 1001 values for cos(angle)=0--1
  ncoswl = n_elements(cosWL)
  dz0=fltarr(ncoswl)


  for i=0,ncoswl-1 do dz0[i] = 500.0 / int_tabulated(thousand,cosFAC[i,*])
                                                ; diffuse correction factor on cosWL grid from the file

  dz = interpol(dz0,cosWL,wl)   ; interpolate to wl grid used in this program
  fcz = fltarr(nwl,1001)
  for angle=0,1000 do fcz[*,angle] = interpol(cosFAC[*,angle],cosWL,wl)
endif






; (3) Get cosine response nadir
cf=cfg(cfg,'cosnad')
cosfile=file_search(cf,count=nc)
if nc lt 1 then message,'No cosine response file found.'
if nc gt 1 then message,'More than one cosine response file found.'

; if ".dat" file, then the expected format is ASCII textfile for one wavelength only
; if ".out" file, then the expected format is binary for all wavelengths

ending=strmid(cosfile[0],3,/REV)
if ending eq '.dat' then begin
  openr,ul,cosfile[0],/get_lun
  cc=indgen(1001)
  print,'Open (1 wl, nadir): ',cosfile[0]
  dcl=fltarr(1001) & ccl=fltarr(1001)
  for i=0,1000 do begin
    readf,ul,a,b
    dcl[i]=a & ccl[i]=b
  endfor
  fcn=interpol(ccl,dcl,cc)
  free_lun,ul
  dn=500./int_tabulated(dcl,ccl) ; diffuse correction factor for nadir
endif
if ending eq '.out' then begin
  print,'Open all-wl file, nadir: ',cosfile[0]
  restore,cosfile[0]   ; should contain variables cosWL (lambda grid) and cosFAC (cos resp f for ALL wl)
                       ;                                                  cosFAC[wl,angle]
                       ;                                                   wl: Si+NIR merged at 950 nm
                       ;                                                   angle: 1001 values for cos(angle)=0--1
  ncoswl = n_elements(cosWL)
  dn0=fltarr(ncoswl)



  if keyword_set(brdf) then begin
    print,'Calculating BRDF scale factor'
    ; read brdf
    if path_sep() eq '\' then Bpath = 'D:\ARCTAS\otherdata\BRDF-CAR\' else Bpath='/home/seven/schmidt/polar/otherdata/BRDF-CAR/'


   ; Read BRDF data, part 1 ( wl[8]=1.656 micron)
    brdf_file = Bpath + 'car2005b_brdf_v3.hdf'
    restore,Bpath+'CAR_BRDF_TEMPLATE.SAV'   ; template variable 'bt'
    b = hdf_read(brdf_file,template=bt)
    ; b.wl [8 or 9], b.brdf[361 azimuths, 181 zeniths (0=zenith, 180=nadir), 8 wavelengths in version til 1.273 micron]
    ;                                                                        9 wavelengths in version til 1.656/2.205 micron]
    bnwl=n_elements(b.wl)
   ; Prepare common array
    bwl =fltarr(bnwl+1)
    bwl[0:bnwl-1]=b.wl
    b_brdf = fltarr(361,181,bnwl+1)
    for izen=0,360 do for iaz = 0,180 do b_brdf[izen,iaz,0:bnwl-1] = b.brdf[izen,iaz,*]



   ; Read BRDF data, part 2 ( wl[8]=2.205 micron, other wl's identical in both files)
    brdf_file = Bpath + 'car2005a_brdf_v3.hdf'
    b = hdf_read(brdf_file,template=bt)
    ; b.wl [8 or 9], b.brdf[361 azimuths, 181 zeniths (0=zenith, 180=nadir), 8 wavelengths in version til 1.273 micron]
    ;                                                                        9 wavelengths in version til 1.656/2.205 micron]
   ; Fill last column of common array with the additional 2.2u channel
    bwl[bnwl] = b.wl[bnwl-1]
    for izen=0,360 do for iaz = 0,180 do b_brdf[izen,iaz,0:bnwl] = b.brdf[izen,iaz,bnwl-1]

    bnwl = n_elements(bwl)

    az = findgen(361)*!PI/180.0 ; element #360 == #0
    zen= findgen(91)*!PI/180.0
    zenunity = zen & zenunity[*]=1.0

    ; integrate over azimuth angles
    brdf_integrazi = fltarr(91,bnwl) ; we need: 0=nadir, 90=horizon; while CAR: 90=horizon, 180=nadir
    for thiswl=0,bnwl-1 do $
     for thiszen = 90,180 do brdf_integrazi[180-thiszen,thiswl] = int_tabulated(az[0:359],b_brdf[0:359,thiszen,thiswl])/(2.0*!PI)

    ; Normalize
    ; int(brdf_integrazi dtheta)/int(dtheta) must equal 1.0
;    normfac = fltarr(bnwl)
;    for thiswl = 0,bnwl-1 do normfac[thiswl] = int_tabulated(zen,brdf_integrazi[*,thiswl])/int_tabulated(zen,zenunity)
;    for thiswl = 0,bnwl-1 do brdf_integrazi[*,thiswl]/=normfac[thiswl]

    ; interpolate to our 1,001 zenith angles
    brdf_integrazi_interpol = fltarr(1001,bnwl)
    for thiswl=0,bnwl-1 do brdf_integrazi_interpol[*,thiswl] = interpol(brdf_integrazi[*,thiswl],cos(zen),thousand/1000.0)

    ; interpolate to cosFAC wavelength grid
    scale_brdf = fltarr(ncoswl,1001)
    for thiszen=0,1000 do scale_brdf[*,thiszen] = interpol(brdf_integrazi_interpol[thiszen,*],bwl*1000.0,cosWL)
    notcovered = where(coswl gt max(bwl)*1000.0,nnot)
    if nnot gt 0 then $
     for thiszen=0,1000 do scale_brdf[notcovered,thiszen] = scale_brdf[notcovered[0],thiszen]   ; spread data to wls not covered by CAR
  endif else begin
    print,'BRDF scale factor == 1'
    scale_brdf = cosFAC & scale_brdf[*,*]=1.0
  endelse





  for i=0,ncoswl-1 do dn0[i] = $
   500.0 / ( int_tabulated(thousand,cosFAC[i,*]*scale_brdf[i,*]) / (0.001 * int_tabulated(thousand,scale_brdf[i,*]) ) )
             ; diffuse correction factor on cosWL grid from the file
  dn00=dn0 & for i=0,ncoswl-1 do dn00[i] = 500.0 / int_tabulated(thousand,cosFAC[i,*])  ; for comparison
  dn = interpol(dn0,cosWL,wl)   ; interpolate to wl grid used in this program
  fcn = fltarr(nwl,1001)
  for angle=0,1000 do fcn[*,angle] = interpol(cosFAC[*,angle]*scale_brdf[*,angle],cosWL,wl)
endif




; (4) Get correction factors

cf=fix(dc*1000+0.5)   ; dc = directional cosine (of effective angle between sun and inlet)
i0=where(cf gt 1000,in) & if in ge 1 then cf[i0]=1000
i0=where(cf lt 0,in)   & if in ge 1 then cf[i0]=0

if ending eq '.out' then begin
 facz=fcz[*,cf]
 facn=fcn[*,cf]
endif else begin
 facz=fcz[cf]
 facn=fcn[cf]
endelse

ind=where(facz lt 0.01,n0)
if n0 gt 0 then begin
  facz[ind]=1.
endif
ind=where(facn lt 0.01,n0)
if n0 gt 0 then begin
  facn[ind]=1.
endif




; Make the correction factors complete, so that we obtain the ratio
;   I(theta=0°) * cos(theta)
;  -------------------------- = facz
;           I(theta)



mu=cos(sza*!pi/180.)
;facz=mu/facz ; if cosine response 1:1, then fac=1 everywhere
;facn=mu/facn

if ending eq '.out' then begin
 for i=0,nwl-1 do begin
   facz[i,*] = mu / facz[i,*]
   facn[i,*] = mu / facn[i,*]
 endfor
endif else begin
 facz=mu/facz
 facn=mu/facn
endelse

print,'** Leaving polar_get_cos.pro'

return
end
