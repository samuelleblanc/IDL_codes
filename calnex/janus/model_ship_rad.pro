@/home/leblanc/libradtran/pro/libradtran_reader.pro
pro model_ship_rad

restore, '/home/leblanc/CALNEX/ship/20100522/szas.out'

ll='/'
dir='/home/leblanc/libradtran/input/aero/'
dirout='/home/leblanc/libradtran/output/aero/'
date='20100522'
atm_in = 'atmos_aero_'+date+'_at.dat'   ;input atmosphere file
input_file=dir+'aero_tests.inp'
output_file=dirout+'aero_tests.out'
doy=julday(fix(strmid(date,4,2)),fix(strmid(date,6,2)),fix(strmid(date,0,4)))-julday(1,0,fix(strmid(date,0,4)))
;zensun,doy, 18.0, 40.0,-105.25 ,sza, azimuth, solfac
zout=[0.,1.]
albedo=0.3
azimuth=0.;just to set something

wvl=[350.,950.]
asy=0.9
ssa=0.95
tau550=0
rad=fltarr(n_elements(szas), 601)
irrad=fltarr(n_elements(szas), 601)
for i=0, n_elements(szas)-1 do begin
    print, 'sza:',szas[i]
      write_input_file, doy, szas[i], dir+atm_in, input_file, azimuth, asy, ssa, tau550, wvl, zout,albedo, /radiance,/quiet,/slit,/cst_tau
      spawn, '/home/leblanc/libradtran/libRadtran-1.5-beta/bin/uvspec < '+input_file+' > '+output_file,message
      if message ne '' gt 0 then message, message
    output=libradtran_reader(file = output_file,/radiance,zout = zout,/quiet)
      rad[i,*]=output.rad[0,*]
    irrad[i,*]=output.dir_dn[0,*]+output.dif_dn[0,*]
    wvls=output.wvl[0,*]
endfor


;data={wvl:wvl,dir_dn:dir_dn,dif_dn:dif_dn,dif_up:dif_up,rad:rad,zout:zout}

save, szas, rad, irrad, wvls, filename='/home/leblanc/CALNEX/ship/20100522/model_sza.out'
stop
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write input file program                                                    ;;
;; used to make the input file for uvspec                                      ;;
;; use radiance keyword to set radiance (zenith pointing)                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro write_input_file, doy, sza, atm_file, input_file, azimuth, asy, ssa, tau550, wvl, zout, albedo, radiance=radiance, quiet=quiet,cst_tau=cst_tau,slit=slit

ui=99
openw,ui,input_file;,/get_lun
printf,ui,'data_files_path   /home/leblanc/libradtran/libRadtran-1.5-beta/data'
printf,ui,'solar_file        /home/leblanc/libradtran/libRadtran-1.5-beta/data/solar_flux/kurudz_1.0nm.dat' 
printf,ui,'atmosphere_file   '+atm_file

if albedo lt 0 then albedo=albedo*(-1.)
printf,ui,'albedo            '+string(albedo)  ; 

printf,ui,'rte_solver        disort2'       
printf,ui,'nstr              20'            
printf,ui,'correlated_k      SBDART'        ; pseudo-spectral definition of atmospheric absorption

printf,ui,'sza               '+string(sza)    ; solar zenith angle (deg)
printf,ui,'day_of_year       '+string(doy)    ; day of year for Sun-Earth distance

if keyword_set(radiance) then begin
  printf,ui,'phi0              '+string(azimuth+180.0); solar azimuth angle (deg)
  printf,ui,'umu               -1  # -1:downward radiance; 1:upward radiance; 0:sidward radiance' ;for radiances (ship)
  printf,ui,'phi               270.0   # output azimuth angle (flight-direction: NNW = 350�)' ;for radiances(ship direction)
endif

printf,ui,'aerosol_default'                   ; initialize aerosol
;printf,ui,'aerosol_tau_file  '+tau_file
printf,ui,'aerosol_haze   5' ; set an aerosol type of urban haze below 2km
printf,ui,'aerosol_set_ssa   '+string(ssa)  
printf,ui,'aerosol_set_gg    '+string(asy)      
if keyword_set(cst_tau) then  printf,ui,'aerosol_set_tau  '+string(tau550) else printf,ui,'aerosol_set_tau550  '+string(tau550) ;use constant tau or scale tau at 550 nm
;if keyword_set(tau_scale) then printf, ui, 'aerosol_scale_tau   '+string(tau_scale)

printf,ui,'wavelength        '+string(wvl[0])+'  '+string(wvl[1]) ; wavelengths
printf,ui,'altitude          0.0' ;0.263 # elevation (ASL) of CalTech in km'      
printf,ui,'zout              '+string(zout, format='('+strtrim(string(n_elements(zout)),2)+'(" ",F5.1))')

if keyword_set(slit) then begin ; if set, use slit function, must change for IngAas
  printf,ui,'slit_function_file /home/leblanc/libradtran/vis_1nm.dat'
endif

printf,ui,'quiet'
free_lun,ui

if not keyword_set(quiet) then print, 'input file saved to: '+input_file
end
