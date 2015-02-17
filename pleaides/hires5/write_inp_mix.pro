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
  ic_dat=ic_dat, wc_included=wc_included

  if n_elements(kurudz) lt 1 then kurudz=0 else kurudz=1
  if n_elements(nofile) lt 1 then nofile=0 else nofile=1
  ;if nofile then print, 'nofile'
  if n_elements(wc_included) lt 1 then wci=0 else wci=1

cloud=cloudl
  ;cld =[tau,ref,alt,thick]   ; tau,Reff,cloud_height_km; cloud_thickness_km
 
; split the contributions liquid water and ice water to taus
  taul=cloud[0]*(1.-wp)
  taui=cloudi[0]*wp

  lwp=2./3.*cloud[1]*taul
  lwc=lwp/cloud[3]*0.001
  ref=cloud[1]
  refi=cloudi[1]
;  lwc=twc*(1.-wp)
;  iwc=twc*wp
  iwp=2./3.*refi*taui
  iwc=iwp/cloudi[3]*0.001
;  dzi=cloudi[3]*1000.
;  taui=iwc*dzi*3./2./refi
;  dzl=cloud[3]*1000.
;  taul=lwc*dzl*3./2./ref

if nofile then begin
  k=strsplit(input_file,/extract,'/')
  fi='/lustre/janus_scratch/leblanse/cloud/input/sp_mix3_lvls_20120524/'+k[n_elements(k)-1]
endif else fi=input_file

  uc=98
  if wp ne 1.0 then begin
    if wci then begin 
      openw ,uc,input_file+'lcld.dat'
      printf,uc,cloud[2]+cloud[3],' 0 0'
      printf,uc,cloud[2]  ,' '+string(lwc)+' '+string(ref)
      printf,uc,cloud[2]-0.5*cloud[3],' 0 0'
      free_lun,uc
    endif else begin
      openw ,uc,input_file+'lcld.dat'
      printf,uc,cloud[2]+cloud[3],' '+fi+'lcld0.dat',format='(F12.6," ",A)'
      printf,uc,cloud[2]  ,' '+fi+'lcld1.dat',format='(F12.6," ",A)'
      printf,uc,cloud[2]-0.5*cloud[3],' '+fi+'lcld0.dat',format='(F12.6," ",A)'
      free_lun,uc
      if not nofile then make_wc_files, input_file+'lcld0.dat',[0,ref,cloud[2],cloud[3]],wvls=wvl,wc_mie=wc_mie
      if not nofile then make_wc_files, input_file+'lcld1.dat',[taul,ref,cloud[2],cloud[3]],wvls=wvl,wc_mie=wc_mie
    endelse
  endif
  if wp ne 0.0 then begin
    openw ,uc,input_file+'icld.dat'
    printf,uc,cloudi[2]+cloudi[3],' '+fi+'cld0.dat',format='(F12.6," ",A)'
    printf,uc,cloudi[2]  ,' '+fi+'cld1.dat',format='(F12.6," ",A)'
    printf,uc,cloudi[2]-0.5*cloudi[3],' '+fi+'cld0.dat',format='(F12.6," ",A)'
    if not nofile then make_ic_files, input_file+'cld0.dat', [0,cloudi[1],cloudi[2],cloudi[3]],/new,ic_dat=ic_dat
    if not nofile then make_ic_files, input_file+'cld1.dat', [taui,refi,cloudi[2],cloudi[3]],/new,ic_dat=ic_dat
    free_lun,uc
  endif

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
  if wvl[0] le 940 then printf,ui,'slit_function_file /projects/leblanse/libradtran/vis_0.1nm.dat' else printf,ui,'slit_function_file /projects/leblanse/libradtran/nir_0.1nm.dat'
endif

if keyword_set(slit) then begin ; if set, use slit function, must change for IngAas
  if wvl[0] le 940 then printf,ui,'slit_function_file /projects/leblanse/libradtran/vis_1nm.dat' else printf,ui,'slit_function_file /projects/leblanse/libradtran/nir_1nm.dat'
endif

if n_elements(pw) gt 0 then printf,ui, 'h2o_precip  '+string(pw)

printf,ui,'deltam on'
printf,ui,'quiet'
free_lun,ui

if not keyword_set(quiet) then print, 'input file saved to: '+input_file

end
