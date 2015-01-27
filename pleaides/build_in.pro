;program to build files for modeling cloud properties

@/projects/leblanse/cloud/pro/zensun.pro
pro build_in,dates

;set the proper directories
;dates='20120912'
date=dates
indir ='/lustre/janus_scratch/leblanse/cloud/input/'+date+'/';'/scratch/stmp00/leblanse/cloud/input/'
outdir='/lustre/janus_scratch/leblanse/cloud/output/'+date+'/';'/scratch/stmp00/leblanse/cloud/output/'
dir   ='/projects/leblanse/cloud/'

spawn,'mkdir '+indir
spawn,'mkdir '+outdir
;spawn,'mkdir '+dir
list_file='/projects/leblanse/cloud/run_cloud'+date+'.sh'

; get the proper albedos
;restore, '/projects/leblanse/cloud/boud_alb_new.out'
; gives wvl and boud_alb
;wvl_alb=wvl

;;;;; restore the total file
print, 'restore, '+dir+'goes_cld_top.out'
restore, dir+'goes_cld_top.out'
datei=date
date=dates
print, 'restoring '+dir+'cloudy.out'
restore, dir+'cloudy.out'
;date='20120820'

n=where(totday eq double(date),ns)
if n_elements(n)-1 eq 0 then stop
base=totbase[n]/1000.
tau=tottau[n]
ref=totref[n]
water=totwater[n]

;get the correct cloud top height
u=where(datei eq long(date))
tops=interpol(top[u],tmhrs[u],tottmhrs[n])
;stop

print, 'found '+strtrim(n_elements(n),2)+' elements'
r=where(ref lt 1.0,nr)
if nr gt 0 then ref[r]=1.0

;make wvls arrays of each the bands at 940, 1150, 810 and ox-a respectively
wvs=[[842.,1074.],[1079.,1295.],[780.,859.],[738.,852.2]]
labl=['wv2','wv3','wv1','oxa']

lat=40.007916667
lon=-105.26825

doy=julian_day(strmid(date,0,4),strmid(date,4,2),strmid(6,2))
zensun, doy,tottmhrs[n],lat, lon, szas, azi, solfac

print, 'opening list_file:'+list_file

uu=97
openw,uu,list_file

atm_file='/projects/leblanse/cloud/atmos_aero_'+date+'_at.dat' 
alb_file='/lustre/janus_scratch/leblanse/albedo.dat'
;'/projects/leblanse/libradtran/libRadtran-1.6-beta/data/atmmod/afglms.dat'
zout=[0.02]
for i=0, ns-1 do begin
  for j=0,3 do begin
        fn='cloud_'+string(i,format='(I05)')+'_'+labl[j]
        outf=outdir+fn+'.out'
        inf =indir +fn+'.in'
        cloud=[tau[i],ref[i],base[i]+1.68,abs(tops[i]-(base[i]+1.68))]
        sza=szas[i]
        wvl=wvs[*,j]
 ;       u=where(wvl_alb ge wvl[0] and wvl_alb le wvl[1])
        albedo=0.1 ;mean(boud_alb[u])
    
        write_input_file, doy, sza, atm_file,atm_file,inf,0.0,0.9,0.9,wvl,zout,albedo,/radiance,/quiet,cloud=cloud,h2o=water[i],alb_file=alb_file
        printf, uu, '/projects/leblanse/libradtran/libRadtran-1.6-beta/bin/uvspec < '+inf+' > '+outf
        print, tottmhrs[n[i]],tau[i],ref[i],labl[j],i,ns

  endfor  ;band loop
endfor  ;time loop
free_lun,uu
stop
end



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write input file program                                                    ;;
;; used to make the input file for uvspec                                      ;;
;; use radiance keyword to set radiance (zenith pointing)                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro write_input_file, doy, sza, tau_file, atm_file, input_file, azimuth, asy, ssa, wvl, zout, albedo, radiance=radiance,$
  quiet=quiet, tau_scale=tau_scale, slit=slit, nofile=nofile, aod=aod,cloud=cloud,h2o=h2o,alb_file=alb_file

if n_elements(cloud) gt 0 then begin
  ;cld =[tau,ref,alt,thick]   ; tau,Reff,cloud_height_km; cloud_thickness_km
  lwp=2./3.*cloud[1]*cloud[0]
  lwc=lwp/cloud[3]*0.001
  ref=cloud[1]
  uc=98
  openw ,uc,input_file+'cld.dat'
  if cloud[2]-0.5*cloud[3] gt 0 then begin
    printf,uc,cloud[2]+0.5*cloud[3],0  ,0
    printf,uc,cloud[2]  ,         lwc,ref
    printf,uc,cloud[2]-0.5*cloud[3],0  ,0
  endif else begin
    printf,uc,cloud[2]+cloud[3]-1.0,0,0
    printf,uc,cloud[2],lwc,ref
    printf,uc,cloud[2]-1.0,0,0
  endelse
  free_lun,uc
endif

ui=99
openw,ui,input_file;,/get_lun
printf,ui,'data_files_path   /projects/leblanse/libradtran/libRadtran-1.6-beta/data'
printf,ui,'solar_file        /projects/leblanse/libradtran/solar_SSFR.dat'
printf,ui,'atmosphere_file   '+atm_file

;printf,ui,'albedo_library    IGBP    # Spectral albedo library '
;printf,ui,'surface_type      13      # urban albedo from library'
if albedo lt 0 then albedo=albedo*(-1.)
;if 0 then printf,ui,'albedo            '+string(albedo) else 
printf,ui,'albedo_file     '+alb_file  ; 

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
  printf,ui,'wc_properties mie'
  printf,ui,'wc_properties_interpolate'
endelse

printf,ui,'h2o_precip        '+string(h2o,format='(F6.3)')
printf,ui,'wavelength        '+string(wvl[0],format='(I4)')+'  '+string(wvl[1],format='(I4)') ; wavelengths
printf,ui,'altitude          1.66' ;0.263 # elevation (ASL) of CalTech in km'      
printf,ui,'zout              '+string(zout, format='('+strtrim(string(n_elements(zout)),2)+'(" ",F8.2))')

if keyword_set(slit) then begin ; if set, use slit function, must change for IngAas
  if wvl[0] le 940 then printf,ui,'slit_function_file /projects/leblanse/libradtran/vis_1nm.dat' else printf,ui,'slit_function_file /projects/leblanse/libradtran/nir_1nm.dat'
endif

printf,ui,'quiet'
close,ui
free_lun,ui

if not keyword_set(quiet) then print, 'input file saved to: '+input_file

end

