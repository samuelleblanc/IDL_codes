;program to build files for modeling cloud properties

pro build_io_cloud

;set the proper directories

indir ='/scratch/stmp001/leblanse/cloud_rad/input/'
outdir='/scratch/stmp001/leblanse/cloud_rad/output/'
dir   ='/projects/leblanse/cloud_rad/'

spawn,'mkdir '+indir
spawn,'mkdir '+outdir
spawn,'mkdir '+dir
list_file='/projects/leblanse/cloud_rad/run_cloud.sh'

;set the albedo
wvls=[515,1565,1571,1577,1582,1588,1594,1600,1605,1611,1617,1623,1628,1634]
alb =[0.0511493,0.171991,0.176302,0.180426,0.18360,0.188400,0.192140,0.196528,$
      0.201539,0.205835,0.208625,0.212767,0.216909,0.219916]

;make arrays of tau and reff
;constant cloud height and thickness of 1km ; 1km

taus = [0.5,1.,2.,5.,7.5,10.,20.,40.,80.,100.]
refs = [3.,5.,10.,15.,20.,25.,30.]
szas = findgen(19)*5.
doy=270 ;20120926

ntau=n_elements(taus)
nref=n_elements(refs)
nsza=n_elements(szas)
nwvl=n_elements(wvls)

uu=97
openw,uu,list_file

atm_file='/projects/leblanse/libradtran/libRadtran-1.6-beta/data/atmmod/afglms.dat'
zout=[0.02]
for i=0, nsza-1 do begin
  for j=0, ntau-1 do begin
    for k=0, nref-1 do begin
      for l=0, nwvl-1 do begin
        fn='cloud_sza'+string(szas[i],format='(I02)')+'_tau'+string(taus[j],format='(F05.1)')+$
           '_ref'+string(refs[k],format='(I02)')+'_wvl'+string(wvls[l],format='(I04)')
        outf=outdir+fn+'.out'
        inf =indir +fn+'.in'
        cloud=[taus[j],refs[k],1.,1.]
        sza=szas[i]
        wvl=[wvls[l],wvls[l]]
        albedo=alb[l]
    
        write_input_file, doy, sza, atm_file,atm_file,inf,0.0,0.9,0.9,wvl,zout,albedo,/radiance,/quiet,/slit,cloud=cloud
        printf, uu, '/projects/leblanse/libradtran/libRadtran-1.6-beta/bin/uvspec < '+inf+' > '+outf
        print, taus[j],refs[k],sza,wvl[0]

      endfor ;wvls loop
    endfor  ;ref loop
  endfor  ;tau loop
endfor  ;sza loop
free_lun,uu

end



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write input file program                                                    ;;
;; used to make the input file for uvspec                                      ;;
;; use radiance keyword to set radiance (zenith pointing)                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro write_input_file, doy, sza, tau_file, atm_file, input_file, azimuth, asy, ssa, wvl, zout, albedo, radiance=radiance,$
  quiet=quiet, tau_scale=tau_scale, slit=slit, nofile=nofile, aod=aod,cloud=cloud

if n_elements(cloud) gt 0 then begin
  ;cld =[tau,ref,alt,thick]   ; tau,Reff,cloud_height_km; cloud_thickness_km
  lwp=2./3.*cloud[1]*cloud[0]
  lwc=lwp/cloud[3]*0.001
  ref=cloud[1]
  uc=98
  openw ,uc,input_file+'cld.dat'
  printf,uc,cloud[2]+0.5*cloud[3],0  ,0
  printf,uc,cloud[2]  ,         lwc,ref
  printf,uc,cloud[2]-0.5*cloud[3],0  ,0
  free_lun,uc
endif

ui=99
openw,ui,input_file;,/get_lun
printf,ui,'data_files_path   /projects/leblanse/libradtran/libRadtran-1.6-beta/data'
printf,ui,'solar_file        /projects/leblanse/libradtran/libRadtran-1.6-beta/data/solar_flux/kurudz_1.0nm.dat'
printf,ui,'atmosphere_file   '+atm_file

;printf,ui,'albedo_library    IGBP    # Spectral albedo library '
;printf,ui,'surface_type      13      # urban albedo from library'
if albedo lt 0 then albedo=albedo*(-1.)
printf,ui,'albedo            '+string(albedo)  ; 

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
  printf,ui,'wc_file '+input_file+'cld.dat'
  printf,ui,'wc_layer'
endelse

printf,ui,'wavelength        '+string(wvl[0],format='(I4)')+'  '+string(wvl[1],format='(I4)') ; wavelengths
printf,ui,'altitude          1.66' ;0.263 # elevation (ASL) of CalTech in km'      
printf,ui,'zout              '+string(zout, format='('+strtrim(string(n_elements(zout)),2)+'(" ",F8.2))')

if keyword_set(slit) then begin ; if set, use slit function, must change for IngAas
  if wvl[0] le 940 then printf,ui,'slit_function_file /projects/leblanse/libradtran/vis_1nm.dat' else printf,ui,'slit_function_file /projects/leblanse/libradtran/nir_1nm.dat'
endif

printf,ui,'quiet'
free_lun,ui

if not keyword_set(quiet) then print, 'input file saved to: '+input_file

end

