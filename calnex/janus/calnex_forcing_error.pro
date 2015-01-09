;+
; NAME:
;   calnex_forcing_error
;
; PURPOSE:
;   to do the aerosol forcing retrieval error derivation, run model, with and without aerosols, get irradiance from model
;
; CATEGORY:
;   CALNEX, Aerosol, error
;
; CALLING SEQUENCE:
;   calnex_forcing_error
;
; OUTPUT:
;   retrieved values in a text file
;
; KEYWORDS:
;
; DEPENDENCIES:
;   
;   
; NEEDED FILES:
;   - hsrl_p3 out file
;
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
; Written:  Samuel LeBlanc, LASP CU Boulder, November 13th, 2010
; Modified: November 30th, by Samuel LeBlanc
;---------------------------------------------------------------------------
@errploty.pro
@write_input.pro
@/home/leblanc/libradtran/pro/libradtran_reader.pro
pro calnex_forcing_error
date='20100519'
dir='/home/leblanc/libradtran/output/aero/'

linux=1

if (1) then begin
; get tau modified
f=file_search(dir+'rtm_taumod_'+date+'_wvl*.txt')
f=f[sort(f)]
print, f
wvl0340=read_ascii(f[0], data_start=1)
wvl0380=read_ascii(f[1], data_start=1)
wvl0440=read_ascii(f[2], data_start=1)
wvl0500=read_ascii(f[3], data_start=1)
wvl0675=read_ascii(f[4], data_start=1)
wvl0870=read_ascii(f[5], data_start=1)
wvl1020=read_ascii(f[6], data_start=1)
;stop
wvls=[340,380,440,500,675,870,1020] 
endif
;#lat  lon     ssa   asy       asy2    albedo          correction    tau modification        flux divergence  model down  model up  tau'

;get errors in retrievals
error=fltarr(6,13)
openr, 95, '/home/leblanc/arctas/nasa/rtm_error.out'
line=' '
readf, 95, line
readf, 95, error
close,/all
err=fltarr(5,n_elements(wvls))
for n=1,5 do begin
  error[n,*]=error[n,*]*0.01
  err[n-1,*]=interpol(error[n,*],error[0,*],wvls)
endfor

  calnex_dir='/home/leblanc/CALNEX/'
  ll='/'
  dir='/home/leblanc/libradtran/input/aero/'
  dirout='/home/leblanc/libradtran/output/aero/'

  ;restore, '/home/leblanc/CALNEX/p3/'+date+'/'+date+'_spectra_save.out'

  restore, calnex_dir+'p3'+ll+date+ll+date+'_p3_hsrl.out'
  zabove=near.zabove_attcorr & zbelow=near.zbelow_attcorr & nabove=near.nabove & nbelow=near.nbelow & leg1=legs.leg1 & leg2=legs.leg2
  
; filtering routine to make sure that both IR and vis are coherent with each other
; filters out if IR larger and vis good (not coherent) and vice versa
tau=fltarr(n_elements(legs.leg1))  
dtau=tau
wl=500.
mm=min(abs(nadlambda-wl),wl_i)
wll=1050.
mm=min(abs(nadlambda-wll),wl_ii)
fl=0
limit=mean(zabove[wl_i,*],/nan)
limit_b=mean(zbelow[wl_i,*],/nan)
limitt=mean(zabove[wl_ii,*],/nan)
limitt_b=mean(zbelow[wl_ii,*],/nan)
limitn_b=mean(nbelow[wl_i,*],/nan)
for i=0,n_elements(leg1)-1 do begin
  fl_t=0 & fl_tt=0 & fl_ttt=0 & fl_tttt=0 & fl_ttttt=0 & fl_tn=0 & fl_tau=0 & fl_tau2=0 & fl_div=0
  if zabove[wl_i,i] lt limit-0.2 then fl_bad=1 else fl_t=1  ;0.1
  if zbelow[wl_i,i] lt limit_b-0.2 then fl_bad=1 else fl_tt=1  ;0.1
  if zabove[wl_ii,i] lt limitt-0.1 then fl_bad=1 else fl_ttt=1 ; 0.05
  if (zbelow[wl_ii,i] lt limitt_b-0.1) xor (zbelow[wl_i,i] lt limit_b-1.0) then fl_bad=1 else fl_tttt=1  ;0.05, 0.5
  fl_ttttt=1; if (r_d[i] gt 2000.0) then fl_bad=1 else fl_ttttt=1 ;500.0
  if nbelow[wl_i,i] lt limitn_b-0.2 then fl_bad=1 else fl_tn=1  ;0.1
  if (zabove[wl_i,i]-nabove[wl_i,i])-(zbelow[wl_i,i]-nbelow[wl_i,i]) lt 0 then fl_bad=1 else fl_div=1
  fl_tau=1 ; if total(finite(tau_aats[leg1[i],*])-1) ge -12 then fl_tau=1
  fl_tau2=1 ; if total(finite(tau_aats[near_leg2[i],*])-1) ge -12 then fl_tau2=1
  if (fl_t and fl_tt and fl_ttt and fl_tttt and fl_ttttt and fl_tn and fl_tau and fl_tau2 and fl_div) then fl=[fl,i]
  tau[i]=total((near.extinction[27:44,i]*(z[28:45]-z[27:44])*0.001),/nan)
endfor
if n_elements(fl) le 1 then print, '*** all points filtered out ***'
fl=fl[1:*]
print, 'number of valid spectras:',n_elements(fl) 

near_leg2=legs.leg1*0
r_d=legs.leg1*0.0
  for i=0, n_elements(legs.leg1)-1 do begin
      ; for second leg onto first
      r=100000.0
      for j=0, n_elements(legs.leg2)-1 do begin
        distance = map_2points(lon[legs.leg1[i]],lat[legs.leg1[i]],lon[legs.leg2[j]],lat[legs.leg2[j]],/meters)
        if distance[0] le r then begin
          near_leg2[i]=legs.leg2[j]
          r=distance[0]
          r_d[i]=distance[0]
        endif
      endfor
  endfor
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Writing of the input file for uvspec                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

atm_in = 'atmos_aero_20100519_at.dat'   ;input atmosphere file
;set random starting values for input file
asy=0.6
asy2=asy
ssa=0.8

ssa_arr=fl*0.0 & asy_arr=fl*0.0 & asy2_arr=fl*0.0
albedo_arr=fl*0.0 & correc_fac_arr=fl*0.0 & tau_mod_arr=fl*0.0
dif_flux=fl*0.0 & tau_arr=fl*0.0 & tau_s=1.0
  
; aeronet
; get aeronet reading
  read_aeronet, calnex_dir+'aeronet'+ll+'*CalTech',  jul_day, jul_day_almu, wvl_aod,wvl_almu, aod_aeronet,ssa_aeronet, sza_aeronet, asy_TT_aeronet, aot2_T_aeronet, up_flux_T_aeronet, down_flux_T_aeronet, diff_flux_T_aeronet, forcing_aeronet, albedo_aeronet

  ; find correct times and wavelengths
  ; make a list of good wavelengths from aeronet
  wvl_arr=0.0 & ia_arr=0
  for i_a=0, n_elements(wvl_aod)-1 do begin
    if finite(aod_aeronet[i_a,0]) then begin
      wvl_arr=[wvl_arr,wvl_aod[i_a]] 
      ia_arr=[ia_arr,i_a]
    endif
  endfor
  wvl_arr=wvl_arr[1:*] & ia_arr=ia_arr[1:*]
  wvl_arr=reverse(wvl_arr) & ia_arr=reverse(ia_arr)
  
  iw_f=n_elements(wvl_arr)-1  ;final iw wavelenght index
  
  sza_arr=sza
print, 'number of wavelength to iterate:',iw_f
print, 'number of points to iterate:',n_elements(fl)
fl_f=12 ;n_elements(fl)-1 ;number of points to go through
fl_s=0 ; start of points

print, 'number of points in file:', n_elements(wvl0340.field01[0,*])

rad_dn=fltarr(iw_f+1,fl_f+1,2)
no_rad_dn=fltarr(iw_f+1,fl_f+1,2)
rad_up=fltarr(iw_f+1,fl_f+1,2)
no_rad_up=fltarr(iw_f+1,fl_f+1,2)
aot=fltarr(iw_f+1,fl_f+1)
ai=fltarr(fl_f+1)
;*******************************************************************
for iw=0, iw_f do begin ;wavelength loop
  aero_wl=ia_arr[iw]
  case iw of
  0:rtm=wvl0340
  1:rtm=wvl0380
  2:rtm=wvl0440
  3:rtm=wvl0500
  4:rtm=wvl0675
  5:rtm=wvl0870
  6:rtm=wvl1020
  endcase
  for jj=0, 12 do begin ;error loop
  
  i=51 ;track loop
	index=fl[i]  ; index for the point along track
	
    write_input_tau_file, z, near, atm_in=atm_in, index=index, tau_out=tau_out, /quiet, alt=alt_tau_hsrl, tau_good=tau_hsrl
    input_file=dir+'aero_'+date+'.inp'
    output_file=dirout+'aero_'+date+'.out'
	
    if date eq '20100519' then doy= 139
    zensun, doy, utc[legs.leg1[index]],lat[legs.leg1[index]], lon[legs.leg1[index]], sza, azimuth, solfac
    zout=[0.,0.5,1.0] ; need to set to right altitudes of flight path currently good for may 19th 
    
    mm=min(abs(jul_day-doy+utc[legs.leg1[index]]/24.0),aeronet_index)  ;get the closest in time aeronet index
	;ai[i]=aeronet_index
    wvl=[fix(wvl_arr[iw]),fix(wvl_arr[iw])]
  ;  print, iw, wvl
  ;  stop
    mm=max(zout, top)
    bottom=1
    mm=min(abs(nadlambda-wvl[0]),wvl_index)

      ;to find the tau aeronet at 532 (HSRL)
      tau_532=interpol(alog(aod_aeronet[ia_arr,aeronet_index]),alog(wvl_arr),alog(532.))
      tau_532=exp(tau_532)
      tauscale1=tau_s*aod_aeronet[aero_wl,aeronet_index]/tau_532
      delta_tau=0.01/tauscale1
      ;delta_tau=err[4,iw]
    ssa=rtm.field01[2,i]
    asy=rtm.field01[3,i]
    albedo=rtm.field01[5,i]
    tau_scale=rtm.field01[7,i]
    tauscale=tau_scale*tauscale1
    
    case jj of
    0:ssa=ssa
    1:ssa=ssa-err[0,iw]*ssa
    2:ssa=ssa+err[0,iw]*ssa
    3:asy=asy-err[1,iw]*asy
    4:asy=asy+err[1,iw]*asy
    5:albedo=albedo*(1.-err[3,iw])
    6:albedo=albedo*(1.+err[3,iw])
    7:tau_scale=tau_scale*(1.-delta_tau) ;err[4,iw])
    8:tau_scale=tau_scale*(1.+delta_tau) ;err[4,iw])
    9:ssa=ssa
    10:ssa=ssa
    11:ssa=ssa
    12:ssa=ssa
    endcase
  print, ssa, asy, albedo, tau_scale, delta_tau

	
	if (finite(ssa) and finite(asy) and finite(albedo) and finite(tau_scale)) then begin
	
	; run with retrieved aerosol values
	write_input_file, doy, sza, dir+tau_out, dir+atm_in, input_file, azimuth, asy, ssa, wvl, zout,albedo, /radiance,/quiet, tau_scale=tauscale
    if linux then spawn, '/home/leblanc/libradtran/libRadtran-1.5-beta/bin/uvspec < '+input_file+' > '+output_file , message else message, 'Must be under linux'
    if message ne '' gt 0 then message, message
    output=libradtran_reader(file = output_file,/radiance,zout = zout,/quiet)

	; run with no aerosols
	write_input_file, doy, sza, dir+tau_out, dir+atm_in, input_file, azimuth, asy, ssa, wvl, zout,albedo, /radiance,/quiet, tau_scale=tauscale, /no_aero
    if linux then spawn, '/home/leblanc/libradtran/libRadtran-1.5-beta/bin/uvspec < '+input_file+' > '+output_file , message else message, 'Must be under linux'
    if message ne '' gt 0 then message, message
    output_no=libradtran_reader(file = output_file,/radiance,zout = zout,/quiet)

	rad_up[iw,jj,*]=output.dif_up[1:*]
	no_rad_up[iw,jj,*]=output_no.dif_up[1:*]
	rad_dn[iw,jj,*]=output.dif_dn[1:*]+output.dir_dn[1:*]
	no_rad_dn[iw,jj,*]=output_no.dif_dn[1:*]+output_no.dir_dn[1:*]
	
	aot[iw,jj]=rtm.field01[11,i]
	endif else begin
	rad_up[iw,jj,*]=[!values.f_nan,!values.f_nan]
	no_rad_up[iw,jj,*]=[!values.f_nan,!values.f_nan]
	rad_dn[iw,jj,*]=[!values.f_nan,!values.f_nan]
	no_rad_dn[iw,jj,*]=[!values.f_nan,!values.f_nan]
	aot[iw,jj]=!values.f_nan
	endelse
  endfor ;end of error loop
  print, wvl
 ; stop
endfor	
	
forcing=rad_up*0.0
eff=rad_up*0.0
effp=rad_up*0.0
for_err=fltarr(iw_f+1,2)
eff_err=fltarr(iw_f+1,2)
for iw=0, iw_f do begin
  for i=0, 8 do begin ;track loop
	forcing[iw,i,0]=(rad_dn[iw,i,0]-rad_up[iw,i,0])-(no_rad_dn[iw,i,0]-no_rad_up[iw,i,0])
    forcing[iw,i,1]=(rad_dn[iw,i,1]-rad_up[iw,i,1])-(no_rad_dn[iw,i,1]-no_rad_up[iw,i,1])
	eff[iw,i,0]=forcing[iw,i,0]/aot[3,i]
	eff[iw,i,1]=forcing[iw,i,1]/aot[3,i]
	effp[iw,i,0]=eff[iw,i,0]/rad_dn[iw,i,1]*100.
	effp[iw,i,1]=eff[iw,i,1]/rad_dn[iw,i,1]*100.
  endfor
  ;dtau=err[4,3]*aot[3,0]
  dtau=0.01
  for_err[iw,0]=sqrt(((forcing[iw,2,0]-forcing[iw,1,0])/2.)^2+((forcing[iw,4,0]-forcing[iw,3,0])/2.)^2+((forcing[iw,6,0]-forcing[iw,5,0])/2.)^2+((forcing[iw,8,0]-forcing[iw,7,0])/2.)^2)
  for_err[iw,1]=sqrt(((forcing[iw,2,1]-forcing[iw,1,1])/2.)^2+((forcing[iw,4,1]-forcing[iw,3,1])/2.)^2+((forcing[iw,6,1]-forcing[iw,5,1])/2.)^2+((forcing[iw,8,1]-forcing[iw,7,1])/2.)^2)
  eff_err[iw,0]=sqrt((forcing[iw,0,0]*dtau/(aot[3,0]^2*rad_dn[iw,0,1]))^2+(forcing[iw,0,0]*rad_dn[iw,0,1]*0.05/(aot[3,0]*rad_dn[iw,0,1]^2))^2+(for_err[iw,0]/(aot[3,0]*rad_dn[iw,0,1]))^2)
  eff_err[iw,1]=sqrt((forcing[iw,0,1]*dtau/(aot[3,0]^2*rad_dn[iw,0,1]))^2+(forcing[iw,0,1]*rad_dn[iw,0,1]*0.05/(aot[3,0]*rad_dn[iw,0,1]^2))^2+(for_err[iw,1]/(aot[3,0]*rad_dn[iw,0,1]))^2)
endfor

; ssa=[wvl0340.field01[2,*],wvl0380.field01[2,*],wvl0440.field01[2,*],wvl0500.field01[2,*],wvl0675.field01[2,*],wvl0870.field01[2,*],wvl1020.field01[2,*]]
; asy=[wvl0340.field01[3,*],wvl0380.field01[3,*],wvl0440.field01[3,*],wvl0500.field01[3,*],wvl0675.field01[3,*],wvl0870.field01[3,*],wvl1020.field01[3,*]]
; asy2=[wvl0340.field01[4,*],wvl0380.field01[4,*],wvl0440.field01[4,*],wvl0500.field01[4,*],wvl0675.field01[4,*],wvl0870.field01[4,*],wvl1020.field01[4,*]]
; albedo=[wvl0340.field01[5,*],wvl0380.field01[5,*],wvl0440.field01[5,*],wvl0500.field01[5,*],wvl0675.field01[5,*],wvl0870.field01[5,*],wvl1020.field01[5,*]]
; #lat  lon     ssa   asy       asy2    albedo          correction    tau modification        flux divergence  model down  model up  tau'

; save, rad_up,no_rad_up,rad_dn, no_rad_dn, aot, wvl,forcing, eff,effp,ssa, asy,asy2,albedo, filename='/home/leblanc/CALNEX/p3/20100519/20100519_forcing.out'
save, for_err, eff_err, filename='/home/leblanc/CALNEX/forcing_err_arctas.out'
stop

end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write input file program                                                    ;;
;; used to make the input file for uvspec                                      ;;
;; use radiance keyword to set radiance (zenith pointing)                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro write_input_file, doy, sza, tau_file, atm_file, input_file, azimuth, asy, ssa, wvl, zout, albedo, radiance=radiance, quiet=quiet, tau_scale=tau_scale, no_aero=no_aero

ui=99
openw,ui,input_file;,/get_lun
printf,ui,'data_files_path   /home/leblanc/libradtran/libRadtran-1.5-beta/data'
printf,ui,'solar_file        /home/leblanc/libradtran/libRadtran-1.5-beta/data/solar_flux/kurudz_1.0nm.dat'
printf,ui,'atmosphere_file   '+atm_file

;printf,ui,'albedo_library    IGBP    # Spectral albedo library '
;printf,ui,'surface_type      13      # urban albedo from library'
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
  printf,ui,'phi               270.0   # output azimuth angle (flight-direction: NNW = 350°)' ;for radiances(ship direction)
endif

if not keyword_set(no_aero) then begin
  printf,ui,'aerosol_default'                   ; initialize aerosol
  printf,ui,'aerosol_tau_file  '+tau_file
  printf,ui,'aerosol_set_ssa   '+string(ssa)
  printf,ui,'aerosol_set_gg    '+string(asy)
  if keyword_set(tau_scale) then printf, ui, 'aerosol_scale_tau   '+string(tau_scale)
endif

printf,ui,'wavelength        '+string(wvl[0])+'  '+string(wvl[1]) ; wavelength used for hsrl
printf,ui,'altitude          0.0' ;0.263 # elevation (ASL) of CalTech in km'
printf,ui,'zout              '+string(zout, format='('+strtrim(string(n_elements(zout)),2)+'(" ",F5.1))')

printf,ui,'quiet'
free_lun,ui

if not keyword_set(quiet) then print, 'input file saved to: '+input_file

end
