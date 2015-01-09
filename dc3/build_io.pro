; program to build the input files for building the look up table on janus cluster
; this simply makes multiple files for every ssa, asy, sza, and tau possible
; the name of the file shows which ssa, asy, sza, and tau is used
; the command for running the uvspec is then added to a file

pro build_io

;set the constants
date='20120621'
dir='/home/leblanc/DC3_SEAC4RS/SSFR3/'
indir  = '/home/leblanc/libradtran/input/'
outdir = '/home/leblanc/libradtran/output/'
list_file='/home/leblanc/libradtran/run_wvl500_aod0.05.sh'

;coordinates for skywatch observatory (top of Duane, Boulder)
lat=40.007916667
lon=-105.26825
time=18.0 ; UTC time of interest

lambda=findgen(203)*3.2+350. ;build a random wavelength array

;use a grass albedo
alb_wvl=[396.,475.,536.,727.,757.,792.,893.,941.,950.,1038.]
alb    =[0.276,0.309,0.377,0.521,0.645,0.695,0.726,0.521,0.464,0.723]
albedo_wvl=spline(alb_wvl,alb,lambda)

;set the values for a wavelength of 500 nm
nul=min(abs(lambda-500.),ii)
wvl=[lambda[ii],lambda[ii]]
albedo= albedo_wvl[ii]

;set up the model run
doy=julian_day(fix(strmid(date,0,4)),fix(strmid(date,4,2)),fix(strmid(date,6,2)))
zensun, doy, time,lat, lon, sza, azimuth, solfac
zout=[1.643]
atm_file='/home/leblanc/libradtran/libRadtran-1.5-beta/data/atmmod/afglms.dat'
aod=0.05 ;just to be set for now

;build the arrays of ssa and asy
nssa=40
nasy=60

ssa=findgen(nssa)/100.+0.6
asy=findgen(nasy)/100.+0.4


for i=0, nssa-1 do begin
  for j=0, nasy-1 do begin
    fi='aero_wvl'+string(wvl[0],format='(I04)')+'_tau'+string(aod,format='(F04.2)')+$
     '_ssa'+string(ssa[i],format='(F04.2)')+'_asy'+string(asy[j],format='(F04.2)')
    input_file=indir+fi+'.in'  
    output_file=outdir+fi+'.out'

    write_input_file, doy, sza, tau_file, atm_file, input_file, 0., asy[j], ssa[i], wvl, zout, albedo, $
     /radiance, tau_scale=1.,/slit,/quiet,/nofile, aod=aod
    spawn, 'echo "/home/leblanc/libradtran/libRadtran-1.6-beta/bin/uvspec < '+input_file+' > '+output_file+'" >> '+list_file

  endfor
endfor
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write input file program                                                    ;;
;; used to make the input file for uvspec                                      ;;
;; use radiance keyword to set radiance (zenith pointing)                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro write_input_file, doy, sza, tau_file, atm_file, input_file, azimuth, asy, ssa, wvl, zout, albedo, radiance=radiance,$
  quiet=quiet, tau_scale=tau_scale, slit=slit, nofile=nofile, aod=aod

ui=99
openw,ui,input_file;,/get_lun
printf,ui,'data_files_path   /home/leblanc/libradtran/libRadtran-1.5-beta/data'
printf,ui,'solar_file        /home/leblanc/libradtran/libRadtran-1.5-beta/data/solar_flux/kurudz_1.0nm.dat'
printf,ui,'atmosphere_file   '+atm_file

;printf,ui,'albedo_library    IGBP    # Spectral albedo library '
;printf,ui,'surface_type      13      # urban albedo from library'
if albedo lt 0 then albedo=albedo*(-1.)
printf,ui,'albedo            '+string(albedo)  ; 

printf,ui,'rte_solver        sdisort'
printf,ui,'nstr              20'
printf,ui,'correlated_k      SBDART'        ; pseudo-spectral definition of atmospheric absorption

printf,ui,'sza               '+string(sza)    ; solar zenith angle (deg)
printf,ui,'day_of_year       '+string(doy)    ; day of year for Sun-Earth distance

if keyword_set(radiance) then begin
  printf,ui,'phi0              '+string(azimuth+180.0); solar azimuth angle (deg)
  printf,ui,'umu               -1  # -1:downward radiance; 1:upward radiance; 0:sidward radiance' ;for radiances (ship)
  printf,ui,'phi               270.0   # output azimuth angle (flight-direction: NNW = 350°)' ;for radiances(ship direction)
endif

printf,ui,'aerosol_default'                   ; initialize aerosol
if not keyword_set(nofile) then printf,ui,'aerosol_tau_file  '+tau_file else printf, ui, 'aerosol_set_tau  '+string(aod)
printf,ui,'aerosol_set_ssa   '+string(ssa)
printf,ui,'aerosol_set_gg    '+string(asy)
if keyword_set(tau_scale) then printf, ui, 'aerosol_scale_tau   '+string(tau_scale)

printf,ui,'wavelength        '+string(wvl[0],format='(I4)')+'  '+string(wvl[1],format='(I4)') ; wavelengths
printf,ui,'altitude          0.0' ;0.263 # elevation (ASL) of CalTech in km'      
printf,ui,'zout              '+string(zout, format='('+strtrim(string(n_elements(zout)),2)+'(" ",F8.2))')

if keyword_set(slit) then begin ; if set, use slit function, must change for IngAas
  if wvl[0] le 940 then printf,ui,'slit_function_file /home/leblanc/libradtran/vis_1nm.dat' else printf,ui,'slit_function_file /home/leblanc/libradtran/nir_1nm.dat'
endif

printf,ui,'quiet'
free_lun,ui

if not keyword_set(quiet) then print, 'input file saved to: '+input_file

end

