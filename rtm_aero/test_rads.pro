; program to run libradtran calculations
; to figure out if there are difference in radiance, irradiance spectra for aerosol properties
; aerosol properties: tau, asymmetry parameter, single scattering albedo
@/home/leblanc/libradtran/pro/libradtran_reader.pro
@zensun.pro

pro test_rads

ll='/'
dir='/home/leblanc/libradtran/input/aero/'
dirout='/home/leblanc/libradtran/output/aero/'
date='20080709'
atm_in = 'atmos_aero_'+date+'_at.dat'   ;input atmosphere file
input_file=dir+'aero_tests.inp'
output_file=dirout+'aero_tests.out'
doy=julday(fix(strmid(date,4,2)),fix(strmid(date,6,2)),fix(strmid(date,0,4)))-julday(1,0,fix(strmid(date,0,4)))
zensun,doy, 18.0, 40.0,-105.25 ,sza, azimuth, solfac
zout=[0.,1.]
albedo=0.3

if (0) then begin
wvl=[350.,950.]
asy=[0.6,0.65,0.7,0.75,0.8]
ssa=[0.8,0.85,0.9,0.95,0.99]
tau550=[0.05,0.1,0.15,0.2,0.25]
rad=fltarr(5,5,5,601)
irrad=fltarr(5,5,5,601)
for i=0,4 do begin
  for j=0,4 do begin
    for k=0,4 do begin
	  print, 'asy:',asy[i],' ssa: ',ssa[j],' tau550: ',tau550[k]
      write_input_file, doy, sza, dir+atm_in, input_file, azimuth, asy[i], ssa[j], tau550[k], wvl, zout,albedo, /radiance,/quiet
      spawn, '/home/leblanc/libradtran/libRadtran-1.5-beta/bin/uvspec < '+input_file+' > '+output_file,message
      if message ne '' gt 0 then message, message
	  output=libradtran_reader(file = output_file,/radiance,zout = zout,/quiet)
      rad[i,j,k,*]=output.rad[0,*]
	  irrad[i,j,k,*]=output.dir_dn[0,*]+output.dif_dn[0,*]
	  wvls=wvl[0,*]
	endfor
  endfor
endfor

;data={wvl:wvl,dir_dn:dir_dn,dif_dn:dif_dn,dif_up:dif_up,rad:rad,zout:zout}

save, asy, ssa, tau550, rad, irrad, filename='/home/leblanc/rtm_aero/data/tests_rad/tests_rad.out'
endif else begin

wvl=[500.,500.]
asy=findgen(500)/500.
ssa=findgen(500)/500.
tau=findgen(500,500)
tau_star=0.1
rad=fltarr(500,500)
irrad=fltarr(500,500)
for i=0,489 do begin
  for j=0,499 do begin
    tau[i,j]=tau_star/(1.-asy[i]*ssa[j])
    print, 'asy:',asy[i],' ssa: ',ssa[j],' tau: ',tau[i,j]
      write_input_file, doy, sza, dir+atm_in, input_file, azimuth, asy[i], ssa[j], tau[i,j], wvl, zout,albedo, /radiance,/quiet,/cst_tau,/slit
      spawn, '/home/leblanc/libradtran/libRadtran-1.5-beta/bin/uvspec < '+input_file+' > '+output_file,message
      if message ne '' gt 0 then message, message
    output=libradtran_reader(file = output_file,/radiance,zout = zout,/quiet)
      rad[i,j]=output.rad[0,0]
    irrad[i,j]=output.dir_dn[0,0]+output.dif_dn[0,0]
    wvls=wvl[0]
  endfor
endfor

save, asy, ssa, tau, rad, irrad,wvls, filename='/home/leblanc/rtm_aero/data/tests_rad/tests_rad_cst_tau_star.out'
endelse
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
