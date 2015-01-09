;+
; NAME:
;   aero_rad
;
; PURPOSE:
;   run and aerosol properties retrieval with spectral radiance, irradiance, and optical depth
;
; CATEGORY:
;   Aerosol retrieval, Radiance
;
; CALLING SEQUENCE:
;   aero_rad
;   
; OUTPUT:
;   - Plots of possible aerosol optical properties spectra
;
; KEYWORDS:
;   - none
;
; DEPENDENCIES:
;   legend.pro    ;to make legend on graph
;   read_skywatch.pro ; to read in the values from the spectral optical depth value
;   zensun.pro
;   libradtran_reader.pro ; to read the output file from libradtran
;   
; NEEDED FILES:
;   - spectral optical depth txt file
;   - TS*.out file for the day
;  
; EXAMPLE:
;   aero_rad
;
; MODIFICATION HISTORY:
; Written:  Samuel LeBlanc, LASP CU Boulder, Thursday, June 14th, 2012
; Modified: 
;
;---------------------------------------------------------------------------
@legend.pro
@read_skywatch.pro
@zensun.pro
@/home/leblanc/libradtran/pro/libradtran_reader.pro

pro aero_rad
; currently hardcoded for a single day
;  
date='20120621'

;coordinates for skywatch observatory (top of Duane, Boulder)
lat=40.007916667
lon=-105.26825


idl_sav='/media/usbdisk3/DC3/SSFR3/20120621/out/20120621_TS_007.out'
tau_file='/home/leblanc/DC3_SEAC4RS/SSFR3/sunphotometer/sun_tau_12_06_21.dat'
lutdir='/data/seven/DC3/library/aero/'
dir='/data/seven/DC3/SSFR3/20120621/'

time=22.0 ; UTC time of interest

;; process the tau file
tau=read_skywatch(instrument='tau',name=tau_file)
;tau is a structure with:
; tau, wavelength, hour, minutes,seconds,sec_of_day,day,month,year
; time is in local time (+6)
utc_tau=tau.sec_of_day/3600.+6. ;convert to UTC hours
num=n_elements(utc_tau)

alpha_338_437=fltarr(num)
alpha_437_669=fltarr(num)
b450=fltarr(num)
b550=fltarr(num)
b700=fltarr(num)
asy1=fltarr(3,num)
asy =fltarr(n_elements(tau.wavelength),num)

lnt=alog(tau.tau)
lnw=alog(tau.wavelength)

nul=min(abs(tau.wavelength-338.),n338)
nul=min(abs(tau.wavelength-437.),n437)
nul=min(abs(tau.wavelength-669.),n669)
n338=n338+18
n669=n669-13

for i=0,n_elements(utc_tau)-1 do begin
  a1=linfit(lnw[n338:n437],lnt[n338:n437,i])
  a2=linfit(lnw[n437:n669],lnt[n437:n669,i])

  alpha_338_437[i]=a1[1]*(-1.)
  alpha_437_669[i]=a2[1]*(-1.)

  b450[i]=0.096-0.007*alpha_338_437[i]
  b550[i]=0.093-0.001*alpha_437_669[i]
  b700[i]=0.093-0.003*alpha_437_669[i]

  asy1[0,i]=-7.143889*b450[i]^3.+7.464439*b450[i]^2.-3.96356*b450[i]+0.9893
  asy1[1,i]=-7.143889*b550[i]^3.+7.464439*b550[i]^2.-3.96356*b550[i]+0.9893
  asy1[2,i]=-7.143889*b700[i]^3.+7.464439*b700[i]^2.-3.96356*b700[i]+0.9893 

  asy[*,i]=spline([450.,550.,700.,1050.],[asy1[*,i],0.6],tau.wavelength)
endfor
fl=where(finite(asy1[0,*]) eq 1 and finite(asy1[1,*]) eq 1 and finite(asy1[2,*]) eq 1) ; build filter of good and valid points
;figure out which point to check

;plot a few points to start
set_plot, 'x'
device, decomposed=0
loadct,39
!p.multi=0
if 1 then begin
window, 4, retain=2, title='Time trace of Aerosol optical depth'
nul=min(abs(tau.wavelength-350.),il)

  plot, utc_tau, tau.tau[il,*], title='Time evolution of aerosol optical depth', ytitle='Optical depth',xtitle='UTC (Hours)',yrange=[0,0.3]
  nul=min(abs(tau.wavelength-450.),il)
  oplot,utc_tau, tau.tau[il,*], color=70
  nul=min(abs(tau.wavelength-500.),il)
  oplot,utc_tau, tau.tau[il,*], color=120
  nul=min(abs(tau.wavelength-650.),il)
  oplot,utc_tau, tau.tau[il,*], color=180
  nul=min(abs(tau.wavelength-750.),il)
  oplot,utc_tau, tau.tau[il,*], color=250
  legend, ['350 nm','450 nm','500 nm','650 nm','750 nm'],textcolors=[255,70,120,180,250],box=0,/right

endif

if 0 then begin
for i=0,num-1 do begin
window, 0
  plot, tau.wavelength, tau.tau[*,i],title='Optical depth at UTC:'+string(utc_tau[i],format='(F5.2)')+' asy:'+$
   string(asy1[*,i],format='(F4.2," ",F4.2," ",F4.2)'),ystyle=9,yrange=[0,0.5],$
   xrange=[350,1050],xstyle=1,xtitle='Wavelength (nm)',ytitle='Aerosol Optical Depth'
  axis,yaxis=1,yrange=[0.,1.0],ytitle='Asymmetry parameter',color=250,/save
  ;asy[*,i]=spline([450.,550.,700.],asy1[*,i],tau.wavelength)
  oplot, tau.wavelength,asy[*,i],color=250,linestyle=2
  wait, 0.01
endfor
endif

nul=min(abs(utc_tau-time),ntime) ; find out where the desired time point is
aot=tau.tau[*,ntime]
g=asy[*,ntime]

;now get the proper idl save file to restore
print, 'restoring idl sav file: '+idl_sav
restore, idl_sav
zen_spect=zen_spect/10.

nul=min(abs(tmhrs_big-time),nssfr) ; find out where the proper points are
irr=nad_spect[nssfr,*]
rad=zen_spect[nssfr,*]
lambda=nadlambda

if 0 then begin
window, 2, xsize=1000,ysize=900, retain=2
!p.multi=[0,2,2]
!y.style=1 & !x.style=1
plot, tau.wavelength, aot, title='Aerosol optical depth', xrange=[350.,1050.],xtitle='Wavelength (nm)',ytitle='Optical depth',yrange=[0.,0.3]
plot, tau.wavelength, g, title='Splined asymmetry parameter',xrange=[350.,1050.],xtitle='Wavelength (nm)',ytitle='Asymmetry parameter'
plot, lambda, irr, title='Irradiance',xrange=[350.,1050.],xtitle='Wavelength (nm)',ytitle='Irradiance (W/m!U2!N nm)'
plot, lambda, rad, title='Zenith Radiance',xrange=[350.,1050.],xtitle='Wavelength (nm)',ytitle='Radiance (W/m!U2!N nm sr)'
endif

if 1 then begin
  window, 5, title='Irradiance and radiance time trace',xsize=500,ysize=1000,retain=2
  !p.multi=[0,1,2]
  
  nul=min(abs(lambda-350.),il)
  plot, tmhrs_big,nad_spect[*,il],title='Irradiance over time',xtitle='UTC (Hours)',ytitle='Irradiance (W/m!U2!N nm)',yrange=[0,1.8]
  nul=min(abs(lambda-450.),il)
  oplot, tmhrs_big,nad_spect[*,il],color=70
  nul=min(abs(lambda-500.),il)
  oplot, tmhrs_big,nad_spect[*,il],color=120
  nul=min(abs(lambda-650.),il)
  oplot, tmhrs_big,nad_spect[*,il],color=180
  nul=min(abs(lambda-750.),il)
  oplot, tmhrs_big,nad_spect[*,il],color=250
  legend, ['350 nm','450 nm','500 nm','650 nm','750 nm'],textcolors=[255,70,120,180,250],box=0,/right
  
  nul=min(abs(lambda-350.),il)
  plot, tmhrs_big,zen_spect[*,il],title='Radiance over time',xtitle='UTC (Hours)',ytitle='Radiance (W/m!U2!N nm sr)',yrange=[0,0.3]
  nul=min(abs(lambda-450.),il)
  oplot, tmhrs_big,zen_spect[*,il],color=70
  nul=min(abs(lambda-500.),il)
  oplot, tmhrs_big,zen_spect[*,il],color=120
  nul=min(abs(lambda-650.),il)
  oplot, tmhrs_big,zen_spect[*,il],color=180
  nul=min(abs(lambda-750.),il)
  oplot, tmhrs_big,zen_spect[*,il],color=250
  legend, ['350 nm','450 nm','500 nm','650 nm','750 nm'],textcolors=[255,70,120,180,250],box=0,/right
  
endif

;set up the model run
doy=julian_day(fix(strmid(date,0,4)),fix(strmid(date,4,2)),fix(strmid(date,6,2)))
zensun, doy, utc_tau[ntime],lat, lon, sza, azimuth, solfac
zout=[0.02]
atm_file='/home/leblanc/libradtran/libRadtran-1.5-beta/data/atmmod/afglms.dat' 
 ;'/home/leblanc/libradtran/input/aero/atmos_aero_'+date+'_at.dat'
input_file='/home/leblanc/libradtran/input/aero/aero_'+date+'.inp'
output_file='/home/leblanc/libradtran/output/aero/aero_'+date+'.out'

;use a grass albedo
;alb_wvl=[396.,475.,536.,727.,757.,792.,893.,941.,950.,1038.]
;alb    =[0.276,0.309,0.377,0.521,0.645,0.695,0.726,0.521,0.464,0.723]
;use Odele's MODIS surface albedo for near Boulder
alb_wvl=[350., 469.,555.,645.,858.,1240.,1640.,2130.]
alb    =[0.02,0.038955,0.0758601,0.07409,0.26779,0.29993,0.224589,0.124088]+0.5

albedo_wvl=spline(alb_wvl,alb,lambda)

;plot the albedo
if 0 then begin

  window,3,title='Surface Albedo',retain=2
  !p.multi=0
  plot, lambda,albedo_wvl,xrange=[350.,1050.],xtitle='Wavelength (nm)',ytitle='Surface Albedo', title='Albedo'

endif

;convolve the sunphtotometer to the SSFR values 
kernel=[0.0023,0.0028,0.00862,0.2603,0.12887,0.4679,0.83435,1.0,0.86015,0.49361,0.21055,0.08068,0.02944,0.01037,0.00407]
aot=convol(aot,kernel,/normalize)
g  =convol(g,kernel,/normalize)
;interpolate the sunphotometer wavelengths to SSFR wavelengths
aot=smooth(aot,5,/nan)
g=smooth(g,5,/nan)

aot=interpol(aot,tau.wavelength,lambda)
g  =interpol(g,tau.wavelength,lambda)
nul=min(abs(lambda-1050.),iend)
nwvl=n_elements(lambda[0:iend])

asy_wvl=fltarr(nwvl)*!values.f_nan
ssa_wvl=fltarr(nwvl)*!values.f_nan
aod_wvl=fltarr(nwvl)*!values.f_nan
irr_wvl=fltarr(nwvl)*!values.f_nan
rad_wvl=fltarr(nwvl)*!values.f_nan

;make loop over wavelengths

;for l=0, nwvl-1, 35 do begin
  l=10
  ;l=45 ;500 nm
  ; get index for one wavelength to start
  ii=l & it=l
  
  ;write_input_cpl,tau[i],top/1000.,bot/1000.,0.0,tau_out=tau_file,tau_approx=0,/quiet
  ;start a loop that runs the radiative transfer model
  rr=1
  ssa=0.85
  wvl=[lambda[ii],lambda[ii]]
  albedo= albedo_wvl[ii];0.91 ;0.73 , 0.65
  asyi=g[it]  
  aod=aot[it]
  ;if not finite(aod)  then continue
  ;if not finite(asyi) then continue
  print, 'starting model loop'
  
  if 1 then begin
  while rr do begin 
    print, 'wavelength:',wvl[0]
  
    print, 'tau, ssa, asy'
    print, aod,ssa,asyi
    print, 'input new values'
    read, aod, ssa, asyi
  
    write_input_file, doy, sza, tau_file, atm_file, input_file, 0., asyi, ssa, wvl, zout, albedo, $
     /radiance, tau_scale=1.,/slit,/quiet,/nofile, aod=aod
    spawn, '/home/leblanc/libradtran/libRadtran-1.6-beta/bin/uvspec < '+input_file+' > '+output_file,message
    print, message
    ;if message ne '' then message, message
    output=libradtran_reader(file = output_file,/radiance,zout = zout,/quiet)
  
    print, 'measured, modeled radiance'
    print, rad[ii],output.rad[0];/solfac
    print, 'measured, modeled, irradiance'
    print, irr[ii],(output.dir_dn[0]+output.dif_dn[0]);/solfac
  
  endwhile
  endif else begin
  
  ;set up the look up table values
  asy_arr=(findgen(30)-15.)/100.+asyi
  ssa_arr=findgen(40)/100.+0.6
  rad_mod=fltarr(30,40)
  irr_mod=fltarr(30,40)

  ;loop through all the potential values
  for i=0,29 do begin
    for j=0,39 do begin
      write_input_file, doy, sza, tau_file, atm_file, input_file, 0., asy_arr[i], ssa_arr[j], wvl, zout, albedo, $
       /radiance, tau_scale=1.,/slit,/quiet,/nofile, aod=aod
      spawn, '/home/leblanc/libradtran/libRadtran-1.6-beta/bin/uvspec < '+input_file+' > '+output_file,message
      print, message
      ;if message ne '' then message, message
      output=libradtran_reader(file = output_file,/radiance,zout = zout,/quiet)
      rad_mod[i,j]=output.rad[0]
      irr_mod[i,j]=output.dir_dn[0]+output.dif_dn[0]
      print, i,j
    endfor
  endfor
  nul=min(abs(rad_mod-rad[ii]),irad)
  nul=min(abs(irr_mod-irr[ii]),iirr)
  nul=min(((rad[ii]-rad_mod)/rad_mod)^2.+((irr[ii]-irr_mod)/irr_mod)^2.,m)
  
  irad_ssa=irad/30
  irad_asy=(float(irad)/30.-float(irad_ssa))*30.
  iirr_ssa=iirr/30
  iirr_asy=(float(iirr)/30.-float(iirr_ssa))*30.
  m_ssa=m/30
  m_asy=(float(m)/30.-float(m_ssa))*30.
  
  print, 'closest radiance values  for asy and ssa:',asy_arr[irad_asy],ssa_arr[irad_ssa]
  print, 'closest irradiance values  for asy and ssa:',asy_arr[iirr_asy],ssa_arr[iirr_ssa]
  print, 'closest least squares fit for asy and ssa:',asy_arr[m_asy],ssa_arr[m_ssa]
  print, 'model irradiance and radiance, measured irradiance and radiance'
  print, irr_mod[m],rad_mod[m],irr[ii],rad[ii]
  fi='lut_wvl'+string(wvl[0],format='(I04)')+'_sza'+string(sza,format='(F5.2)')+'_aod'+string(aod,format='(F5.3)')+'.dat'
  save, asy_arr, ssa_arr, wvl, aod, sza, filename=lutdir+fi
  
  asy_wvl[ii]=asy_arr[m_asy] & ssa_wvl[ii]=ssa_arr[m_ssa] & aod_wvl[ii]=aod & rad_wvl[ii]=rad_mod[m] & irr_wvl[ii]=irr_mod[m]
  endelse
;endfor

save, asy_wvl, ssa_wvl, aod_wvl, rad_wvl, irr_wvl, rad, irr,albedo, filename=dir+'results2_utc_'+string(time, format='(F05.2)')+'.dat'

set_plot, 'x'
window, 4, xsize=1000, ysize=900, retain =2
!p.multi=[0,2,2]
!y.style=1 & !x.style=1
plot, lambda, aod_wvl, title='Aerosol optical depth', xrange=[350.,1050.],xtitle='Wavelength (nm)',ytitle='Optical depth',yrange=[0.,0.3]
plot, lambda, asy_wvl, title='Asymmetry parameter and single scattering albedo',xrange=[350.,1050.],xtitle='Wavelength (nm)', yrange=[0,1]
oplot, lambda, ssa_wvl, color=250
legend, ['Asymmetry parameter','Single scattering albedo'],textcolors=[255,250],box=0
plot, lambda, irr, title='Irradiance',xrange=[350.,1050.],xtitle='Wavelength (nm)',ytitle='Irradiance (W/m!U2!N nm)'
oplot, lambda, irr_wvl, color=170
legend, ['Measured','Modeled'],textcolors=[255,170],box=0
plot, lambda, rad, title='Zenith Radiance',xrange=[350.,1050.],xtitle='Wavelength (nm)',ytitle='Radiance (W/m!U2!N nm sr)'
oplot, lambda, rad_wvl, color=170
legend, ['Measured','Modeled'],textcolors=[255,170],box=0
stop
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
printf,ui,'altitude          1.67' ;0.263 # elevation (ASL) of CalTech in km'      ;1.67 km height of the Duane building
printf,ui,'zout              '+string(zout, format='('+strtrim(string(n_elements(zout)),2)+'(" ",F8.2))')

if keyword_set(slit) then begin ; if set, use slit function, must change for IngAas
  if wvl[0] le 940 then printf,ui,'slit_function_file /home/leblanc/libradtran/vis_1nm.dat' else printf,ui,'slit_function_file /home/leblanc/libradtran/nir_1nm.dat'
endif

printf,ui,'quiet'
free_lun,ui

if not keyword_set(quiet) then print, 'input file saved to: '+input_file

end

