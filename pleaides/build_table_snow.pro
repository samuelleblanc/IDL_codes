; program to build files for modeling cloud properties
; build a whole reference table with snow albedo

@zensun.pro
@make_ic_files.pro
pro build_table_snow

;set the proper directories
date='20120824'

indir ='/lustre/janus_scratch/leblanse/cloud/input/tablesp_snow/';'/scratch/stmp00/leblanse/cloud/input/'
outdir='/lustre/janus_scratch/leblanse/cloud/output/tablesp_snow/';'/scratch/stmp00/leblanse/cloud/output/'
dir   ='/projects/leblanse/cloud/'

spawn,'mkdir '+indir
spawn,'mkdir '+outdir
;spawn,'mkdir '+dir
list_file='/projects/leblanse/cloud/run_cloud_tablesp_snow.sh'

alb=0.2

;make the table spacing
tau=(findgen(5))*50.
tau[0]=25.
tau=[10.,tau]
ref=(findgen(5)+1.)*5.

lat=40.007916667
lon=-105.26825

doy=julian_day(strmid(date,0,4),strmid(date,4,2),strmid(6,2))
zensun, doy,20.0,lat, lon, sza, azi, solfac
print, 'opening list_file:'+list_file

uu=97
openw,uu,list_file

atm_file='/projects/leblanse/libradtran/libRadtran-1.6-beta/data/atmmod/afglms.dat'
alb_file='/lustre/janus_scratch/leblanse/albedo.dat'
zout=[0.02]
  for t=0, n_elements(tau)-1 do begin
    for r=0, n_elements(ref)-1 do begin
          fn='cloud_sn_ta'+string(t,format='(I02)')+'_re'+string(r,format='(I02)')          
          outf=outdir+fn+'.out'
          inf =indir +fn+'.in'
          cloud=[tau[t],ref[r],1.5+1.68,1.5]
          wvl=[400.,2200.]
          albedo=0.2
          write_input_file, doy, sza, atm_file,atm_file,inf,0.0,0.9,0.9,wvl,zout,albedo,$
           /radiance,/quiet,cloud=cloud,alb_file=alb_file,/snow
          printf, uu, '/projects/leblanse/libradtran/libRadtran-1.6-beta/bin/uvspec < '+inf+' > '+outf
          print, tau[t],ref[r]
          fn='cloud_sn_ta'+string(t,format='(I02)')+'_re'+string(r,format='(I02)')+'_ice'
          outf=outdir+fn+'.out'
          inf =indir +fn+'.in'
          write_input_file, doy, sza, atm_file,atm_file,inf,0.0,0.9,0.9,wvl,zout,albedo,$
           /radiance,/quiet,cloud=cloud,alb_file=alb_file,/ice,/ic_files,/snow
          printf, uu, '/projects/leblanse/libradtran/libRadtran-1.6-beta/bin/uvspec < '+inf+' > '+outf
          print, inf
    endfor   ;ref loop
  endfor    ;tau loop

free_lun,uu

end



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write input file program                                                    ;;
;; used to make the input file for uvspec                                      ;;
;; use radiance keyword to set radiance (zenith pointing)                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro write_input_file, doy, sza, tau_file, atm_file, input_file, azimuth, asy, ssa, wvl, zout, albedo, radiance=radiance,$
  quiet=quiet, tau_scale=tau_scale, slit=slit, nofile=nofile, aod=aod,cloud=cloud,h2o=h2o,alb_file=alb_file,ice=ice,$
  ic_files=ic_files,snow=snow

if n_elements(cloud) gt 0 then begin
  ;cld =[tau,ref,alt,thick]   ; tau,Reff,cloud_height_km; cloud_thickness_km
  lwp=2./3.*cloud[1]*cloud[0]
  lwc=lwp/cloud[3]*0.001
  ref=cloud[1]
  uc=98
  openw ,uc,input_file+'cld.dat'
  if n_elements(ic_files) eq 0 then begin
    printf,uc,cloud[2]+0.5*cloud[3],0  ,0
    printf,uc,cloud[2]  ,         lwc,ref
    printf,uc,cloud[2]-0.5*cloud[3],0  ,0
  endif else begin
    printf,uc,cloud[2]+0.5*cloud[3],' '+input_file+'cld0.dat',format='(F12.6," ",A)'
    printf,uc,cloud[2]  ,' '+input_file+'cld1.dat',format='(F12.6," ",A)'
    printf,uc,cloud[2]-0.5*cloud[3],' '+input_file+'cld0.dat',format='(F12.6," ",A)'
    make_ic_files, input_file+'cld0.dat', [0,cloud[1],cloud[2],cloud[3]]
    make_ic_files, input_file+'cld1.dat', cloud
  endelse
  free_lun,uc
endif

ui=99
openw,ui,input_file;,/get_lun
printf,ui,'data_files_path   /projects/leblanse/libradtran/libRadtran-1.6-beta/data'
printf,ui,'solar_file        /projects/leblanse/libradtran/solar_SSFR.dat' ;/projects/leblanse/libradtran/libRadtran-1.6-beta/data/solar_flux/kurudz_0.1nm.dat';/projects/leblanse/libradtran/solar_SSFR.dat'
printf,ui,'atmosphere_file   '+atm_file

if n_elements(snow) gt 0 then begin
  printf,ui,'albedo_library    IGBP    # Spectral albedo library '
  printf,ui,'surface_type      19      # snow albedo from library'
endif else begin
  if albedo lt 0 then albedo=albedo*(-1.)
  ;printf,ui,'albedo            '+string(albedo)  ; 
  printf,ui,'albedo_file     '+alb_file  ; 
endelse

printf,ui,'rte_solver        disort2'
if n_elements(cloud) lt 1 then printf,ui,'nstr              6' else printf,ui,'nstr             16'
printf,ui,'correlated_k      SBDART'        ; pseudo-spectral definition of atmospheric absorption

printf,ui,'sza               '+string(sza)    ; solar zenith angle (deg)
printf,ui,'day_of_year       '+string(doy)    ; day of year for Sun-Earth distance

if keyword_set(radiance) then begin
  printf,ui,'phi0              '+string(azimuth+180.0); solar azimuth angle (deg)
  printf,ui,'umu               -1  # -1:downward radiance; 1:upward radiance; 0:sidward radiance' ;for radiances (ship)
  printf,ui,'phi               270.0   # output azimuth angle (flight-direction: NNW = 350°)' ;for radiances(ship direction)
endif

if n_elements(cloud) lt 1 then begin
  printf,ui,'aerosol_default'                   ; initialize aerosol
  if not keyword_set(nofile) then printf,ui,'aerosol_tau_file  '+tau_file else printf, ui, 'aerosol_set_tau  '+string(aod)
  printf,ui,'aerosol_set_ssa   '+string(ssa)
  printf,ui,'aerosol_set_gg    '+string(asy)
  if keyword_set(tau_scale) then printf, ui, 'aerosol_scale_tau   '+string(tau_scale)
endif else begin
  if n_elements(ice) gt 0 then begin
    if n_elements(ic_files) gt 0 then printf,ui,'ic_files  '+input_file+'cld.dat' else printf,ui,'ic_file  '+input_file+'cld.dat'
    printf,ui,'ic_layer'
    if n_elements(ic_files) eq 0 then begin
      printf,ui,'ic_properties baum'
      printf,ui,'ic_properties_interpolate'
    endif
  endif else begin  
    printf,ui,'wc_file '+input_file+'cld.dat'
    printf,ui,'wc_layer'
    printf,ui,'wc_properties mie'
    printf,ui,'wc_properties_interpolate'
  endelse
endelse

if n_elements(h2o) gt 0 then printf,ui,'h2o_precip        '+string(h2o,format='(F6.3)')
printf,ui,'wavelength        '+string(wvl[0],format='(I4)')+'  '+string(wvl[1],format='(I4)') ; wavelengths

if n_elements(ic_files) eq 0 then begin
  printf,ui,'altitude          1.66' ;0.263 # elevation (ASL) of CalTech in km'      
  printf,ui,'zout              '+string(zout, format='('+strtrim(string(n_elements(zout)),2)+'(" ",F8.2))')
endif else begin
  printf,ui,'zout              '+string(zout+1.66, format='('+strtrim(string(n_elements(zout)),2)+'(" ",F8.2))')
  printf,ui,'disort_icm moments'
endelse

if keyword_set(slit) then begin ; if set, use slit function, must change for IngAas
  if wvl[0] le 940 then printf,ui,'slit_function_file /projects/leblanse/libradtran/vis_0.1nm.dat' else printf,ui,'slit_function_file /projects/leblanse/libradtran/nir_0.1nm.dat'
endif

printf,ui,'quiet'
free_lun,ui

if not keyword_set(quiet) then print, 'input file saved to: '+input_file

end
