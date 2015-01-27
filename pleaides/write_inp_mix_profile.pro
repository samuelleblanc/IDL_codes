;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write input file program                                                    ;;
;; used to make the input file for uvspec                                      ;;
;; program to build the mixed phase clouds
;; is not suitable for any other use

@make_ic_files.pro
@make_wc_files.pro
pro write_inp_mix, doy, sza, atm_file, input_file, azimuth, wvl, zout, $
  quiet=quiet, slit=slit, cloudl=cloudl,h2o=h2o,alb_file=alb_file,ice=ice,ic_files=ic_files,$
  cloudi=cloudi, wp=wp, kurudz=kurudz,wc_mie=wc_mie,hi_slit=hi_slit,pw=pw, nofile=nofile,$
  ic_dat=ic_dat, wc_included=wc_included,no_file=no_file,noff=noff,prof=prof

  if n_elements(kurudz) lt 1 then kurudz=0 else kurudz=1
  if n_elements(nofile) lt 1 then nofile=0 else nofile=1
  if nofile eq 0 and n_elements(no_file) lt 1 then nofile=0 else nofile=1
  ;if nofile then print, 'nofile'
  if n_elements(wc_included) lt 1 then wci=0 else wci=1
  if n_elements(prof) lt 1 then prof=0 else prof=1


;set up the vertical profiles
; use the cloudl values to determine the dz
dzl=cloudl[3]/10. ; split into 10 different positions
zsl=findgen(11)*dzl+cloudl[2]
dzi=cloudi[3]/10. ; split into 10 different positions
zsi=findgen(11)*dzi+cloudi[2]

drefl=cloudl[1]/10. & drefi=cloudi[1]/10.
refsl=findgen(11)*drefl+cloudl[1]/2.
refsi=findgen(11)*drefi+cloudi[1]/2.
dtau=cloudl[0]/10.
iwcs=2./3.*refsi*dtau/dzi*0.001
lwcs=2./3.*refsl*dtau/dzl*0.001

cloud=cloudl
  ;cld =[tau,ref,alt,thick]   ; tau,Reff,cloud_height_km; cloud_thickness_km
 
; split the contributions liquid water and ice water to taus
  taul=cloud[0]*(1.-wp)
  taui=cloudi[0]*wp

  lwp=2./3.*cloud[1]*taul
  lwc=lwp/cloud[3]*0.001
  ref=cloud[1]
  refi=cloudi[1]
  iwp=2./3.*refi*taui
  iwc=iwp/cloudi[3]*0.001

if nofile then begin
  k=strsplit(input_file,/extract,'/')
  if n_elements(noff) lt 1 then fi='/lustre/janus_scratch/leblanse/cloud/input/sp_hires5_20120524/'+k[n_elements(k)-1] else $
   fi=noff
endif else fi=input_file
fii=fi+'cld'+string(findgen(11)+1,format='(I02)')+'.dat
fil=fi+'lcld'+string(findgen(11)+1,format='(I02)')+'.dat


  uc=98
  if wp ne 1.0 then begin
   if wci then begin 
      openw ,uc,input_file+'lcld.dat'
      printf,uc,cloud[2]+cloud[3],' 0 0'
      if prof then $
        for j=n_elements(refsl)-2,0 ,-1 do printf,uc,zsl[j]  ,' '+string(lwcs[j])+' '+string(refsl[j]) $
          else printf,uc,cloud[2]  ,' '+string(lwc)+' '+string(ref)
      printf,uc,cloud[2]-0.5*cloud[3],' 0 0'
      free_lun,uc
    endif else begin
      openw ,uc,input_file+'lcld.dat'
      printf,uc,cloud[2]+cloud[3],' '+fi+'lcld0.dat',format='(F12.6," ",A)'
      if prof then $
        for j=n_elements(refsl)-2,0,-1 do printf,uc,zsl[j]  ,' '+fil[j],format='(F12.6," ",A)' $
         else printf,uc,cloud[2]  ,' '+fi+'lcld1.dat',format='(F12.6," ",A)'
      printf,uc,cloud[2]-0.5*cloud[3],' '+fi+'lcld0.dat',format='(F12.6," ",A)'
      free_lun,uc
      if not nofile then make_wc_files, input_file+'lcld0.dat',[0,ref,cloud[2],cloud[3]],wvls=wvl,wc_mie=wc_mie
      if prof then if not nofile then $
        for j=n_elements(refsl)-2,0,-1 do make_wc_files, fil[j],[dtau,refsl[j],zsl[j],dzl],wvls=wvl,wc_mie=wc_mie else $
        if not nofile then make_wc_files, input_file+'lcld1.dat',[taul,ref,cloud[2],cloud[3]],wvls=wvl,wc_mie=wc_mie
    endelse
  endif
  if wp ne 0.0 then begin
    openw ,uc,input_file+'icld.dat'
    printf,uc,cloudi[2]+cloudi[3],' '+fi+'cld0.dat',format='(F12.6," ",A)'
    if prof then for j=n_elements(refsi)-2,0,-1 do printf,uc,zsi[j]  ,' '+fii[j],format='(F12.6," ",A)' else $
      printf,uc,cloudi[2]  ,' '+fi+'cld1.dat',format='(F12.6," ",A)'
    printf,uc,cloudi[2]-0.5*cloudi[3],' '+fi+'cld0.dat',format='(F12.6," ",A)'
    if not nofile then make_ic_files, input_file+'cld0.dat', [0,cloudi[1],cloudi[2],cloudi[3]],/new,ic_dat=ic_dat
    if prof then if not nofile then $
      for j=n_elements(refsi)-2,0,-1 do make_ic_files, fii[j],[dtau,refsi[j],zsi[j],dzi],/new,ic_dat=ic_dat else $
    if not nofile then make_ic_files, input_file+'cld1.dat', [taui,refi,cloudi[2],cloudi[3]],/new,ic_dat=ic_dat
    free_lun,uc
  endif
;stop
ui=99
openw,ui,input_file;,/get_lun
printf,ui,'data_files_path   /projects/leblanse/libradtran/libRadtran-1.6-beta/data'
if kurudz then printf,ui, 'solar_file /projects/leblanse/libradtran/libRadtran-1.6-beta/data/solar_flux/kurudz_1.0nm.dat' else $
printf,ui,'solar_file        /projects/leblanse/libradtran/solar_SSFR.dat' ;/projects/leblanse/libradtran/libRadtran-1.6-beta/data/solar_flux/kurudz_0.1nm.dat';/projects/leblanse/libradtran/solar_SSFR.dat'
printf,ui,'atmosphere_file   '+atm_file
printf,ui,'albedo_file       '+alb_file  ; 

printf,ui,'rte_solver        disort2'
printf,ui,'nstr              28'
printf,ui,'correlated_k      SBDART'        ; pseudo-spectral definition of atmospheric absorption

printf,ui,'sza               '+string(sza)    ; solar zenith angle (deg)
printf,ui,'day_of_year       '+string(doy)    ; day of year for Sun-Earth distance

printf,ui,'phi0              '+string(azimuth+180.0); solar azimuth angle (deg)
printf,ui,'umu               -1  # -1:downward radiance; 1:upward radiance; 0:sidward radiance' ;for radiances (ship)
printf,ui,'phi               270.0   # output azimuth angle (flight-direction: NNW = 350°)' ;for radiances(ship direction)

if wp ne 0.0 then begin
  printf,ui,'ic_files  '+input_file+'icld.dat'
  printf,ui,'ic_layer'
endif 
if wp ne 1.0 then begin
  if wci then begin
    printf,ui,'wc_file  '+input_file+'lcld.dat'
    printf,ui,'wc_properties mie'
    printf,ui,'wc_properties_interpolate'
  endif else begin
    printf,ui,'wc_files '+input_file+'lcld.dat'
    printf,ui,'wc_layer'
  endelse
endif

if n_elements(h2o) gt 0 then printf,ui,'h2o_precip        '+string(h2o,format='(F6.3)')
printf,ui,'wavelength        '+string(wvl[0],format='(I4)')+'  '+string(wvl[1],format='(I4)') ; wavelengths

printf,ui,'zout              '+string(zout+1.66, format='('+strtrim(string(n_elements(zout)),2)+'(" ",F8.2))')
if not wci then printf,ui,'disort_icm moments'

if keyword_set(hi_slit) then begin ; if set, use slit function, must change for IngAas
  if wvl[0] le 940 then printf,ui,'slit_function_file /projects/leblanse/libradtran/vis_0.1nm.dat' else $
   printf,ui,'slit_function_file /projects/leblanse/libradtran/nir_0.1nm.dat'
endif

if keyword_set(slit) then begin ; if set, use slit function, must change for IngAas
;  if wvl[0] le 940 then printf,ui,'slit_function_file /projects/leblanse/libradtran/vis_1nm.dat' else $
;   printf,ui,'slit_function_file /projects/leblanse/libradtran/nir_1nm.dat'
if wvl[0] le 940 then printf,ui,'slit_function_file /lustre/janus_scratch/leblanse/data/vis_1nm.dat' else $
   printf,ui,'slit_function_file /lustre/janus_scratch/leblanse/data/nir_1nm.dat'

endif

if n_elements(pw) gt 0 then printf,ui, 'h2o_precip  '+string(pw)

printf,ui,'deltam on'
printf,ui,'quiet'
free_lun,ui

if not keyword_set(quiet) then print, 'input file saved to: '+input_file

end

