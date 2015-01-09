;+
; NAME:
;   arctas_forcing
;
; PURPOSE:
;   to do the aerosol forcing retrieval, run model, with and without aerosols, get irradiance from model
;
; CATEGORY:
;   ARCTAS, Aerosol
;
; CALLING SEQUENCE:
;   arctas_forcing
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
;   - aats ict file
;   - ssfr out file
;
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
; Written:  Samuel LeBlanc, LASP CU Boulder, November 18th, 2010
; Modified: 
;---------------------------------------------------------------------------
@zensun.pro
@/home/leblanc/libradtran/pro/libradtran_reader.pro
@forcing_plot.pro
pro arctas_forcing
date='20080408'

dir='/home/leblanc/libradtran/output/aero/'
aats_dir='/data/seven/schmidt/polar/nasa/'
; get rtm results
  ; get tau modified
  f=file_search(dir+'rtm_taumod_'+date+'_wvl*.txt')
  f=f[sort(f)]
  print, f
    ;lat, lon, ssa, asy, asy2, albedo, correction, tau modification, flux divergence, model down, model up, tau, Ft_up, Ft_dn, Fb_up, Fb_dn
  linux=1
    
  dir='/home/leblanc/arctas/nasa/'+date+'/'
  wvl0353=read_ascii(f[0], data_start=1)
  wvl0380=read_ascii(f[1], data_start=1)
  wvl0452=read_ascii(f[2], data_start=1)
  wvl0499=read_ascii(f[3], data_start=1)
  wvl0519=read_ascii(f[4], data_start=1)
  wvl0605=read_ascii(f[5], data_start=1)
  wvl0675=read_ascii(f[6], data_start=1)
  wvl0779=read_ascii(f[7], data_start=1)
  wvl0864=read_ascii(f[8], data_start=1)
  wvl1019=read_ascii(f[9], data_start=1)
  wvl1241=read_ascii(f[10], data_start=1)
  wvl1558=read_ascii(f[11], data_start=1)
  wvl2139=read_ascii(f[12], data_start=1)

;get errors in retrievals
error=fltarr(6,13)
openr, 95, '/home/leblanc/arctas/nasa/rtm_error.out'
line=' '
readf, 95, line
readf, 95, error
close,/all
error=error*0.01


  arctas_dir='/home/leblanc/arctas/'
  arctas_dir_in='/data/seven/schmidt/polar/'
  ll='/'
  dir='/home/leblanc/libradtran/input/aero/'
  dirout='/home/leblanc/libradtran/output/aero/'

restore, arctas_dir_in+'nasa'+ll+date+ll+date+'_calibspcs_attcorr.out'
utc=tmhrs
; need to get utc to compare
nav_nasa,aats_dir,date,aats_dir+'/'+date+'/'+'polar.cfg',utc,alt,lat,lon,dc,sza,iza,lats,lons,label,roll=rol,pitch=pit
ind=0
for i=0, n_elements(wvl0353.field01[0,*])-1 do begin
  m=min(abs(wvl0380.field01[0,i]-lat)+abs(wvl0380.field01[1,i]-lon),mm)
  ind=[ind,mm]
endfor
ind=ind[1:*]

case date of
'20080408': begin
leg1=where(utc gt 16.727778 and utc lt 16.876389)
leg2=where(utc gt 15.876944 and utc lt 16.298333)
end

'20080709':begin
leg1=where(utc gt 19.613 and utc lt 19.686)
leg2=where(utc gt 19.245 and utc lt 19.343)
end
endcase

gradient_alt1 = abs(alt[leg1]-shift(alt[leg1],1))
gradient_alt2 = abs(alt[leg1]-shift(alt[leg1],2))

flt =where(gradient_alt1 lt 5. and gradient_alt2 lt 5.)

gradient2_alt1 = abs(alt[leg2]-shift(alt[leg2],1))
gradient2_alt2 = abs(alt[leg2]-shift(alt[leg2],2))

flt2=where(gradient2_alt1 lt 5. and gradient2_alt2 lt 5.)

leg1=leg1[flt]
leg2=leg2[flt2]
  
; filter of high roll and pitch angles
ang_high=5.0
  
flt_ang  = where(abs(rol[leg1]) lt ang_high and abs(pit[leg1]) lt ang_high)
flt_ang2 = where(abs(rol[leg2]) lt ang_high and abs(pit[leg2]) lt ang_high)
leg1=leg1[flt_ang]
leg2=leg2[flt_ang2]

; determination of the nearest distance from leg2 to leg1
; writing out the distance
near_leg2=leg1*0
r_d=leg1*0.0
zabove=zspectra[*,leg1]*0.0
zbelow=zspectra[*,leg1]*0.0
nabove=nspectra[*,leg1]*0.0
nbelow=nspectra[*,leg1]*0.0

for i=0, n_elements(leg1)-1 do begin
    ; for second leg onto first
    r=100000.0
    for j=0, n_elements(leg2)-1 do begin
      distance = map_2points(lon[leg1[i]],lat[leg1[i]],lon[leg2[j]],lat[leg2[j]],/meters)
      if distance[0] le r then begin
        near_leg2[i]=leg2[j]
        r=distance[0]
        r_d[i]=distance[0]
      endif
    endfor
   sza_ref=min(sza[leg2]) 
   zabove[*,i]=zspectra[*,near_leg2[i]]*cos(sza_ref*!dtor)/cos(sza[near_leg2[i]]*!dtor)
   nabove[*,i]=nspectra[*,near_leg2[i]]
   ;print, 'correction factor above leg:' , cos(sza[near_leg2[0]]*!dtor)/cos(sza[near_leg2[i]]*!dtor)
   ;print, 'sza ref:', sza[near_leg2[0]]
   zbelow[*,i]=zspectra[*,leg1[i]]*cos(sza_ref*!dtor)/cos(sza[leg1[i]]*!dtor)
   nbelow[*,i]=nspectra[*,leg1[i]] 
   ;print, 'correction factor below leg:' , cos(40.*!dtor)/cos(sza[leg1[i]]*!dtor)      
endfor

; get aats 
get_aats_all,aats_dir,date,uth,lambda,tauh,cld,dtauh,coef
  if n_elements(uth) lt 1 then print, '*** No AATS file ***'
tau_aats=fltarr(n_elements(utc),n_elements(lambda))
dtau_aats=fltarr(n_elements(utc),n_elements(lambda))
cf=fltarr(n_elements(utc),3)

for ju=0, n_elements(utc)-1 do begin
  smm=min(abs(uth/3600.-utc[ju]),jk)
  cf[ju,*]=coef[jk,*]
  for ji=0,n_elements(lambda)-1 do begin
    tau_aats[ju,ji]=tauh[jk,ji]
    dtau_aats[ju,ji]=dtauh[jk,ji]
  endfor
endfor

; filtering routine to make sure that both IR and vis are coherent with each other
; filters out if IR larger and vis good (not coherent) and vice versa
tau=fltarr(n_elements(leg1),n_elements(lambda))  
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
  if (r_d[i] gt 800.0) then fl_bad=1 else fl_ttttt=1 ;500.0
  if nbelow[wl_i,i] lt limitn_b-0.2 then fl_bad=1 else fl_tn=1  ;0.1
  if (zabove[wl_i,i]-nabove[wl_i,i])-(zbelow[wl_i,i]-nbelow[wl_i,i]) lt 0 then fl_bad=1 else fl_div=1
  if total(finite(tau_aats[leg1[i],*])-1) ge -12 then fl_tau=1
  if total(finite(tau_aats[near_leg2[i],*])-1) ge -12 then fl_tau2=1
  if (fl_t and fl_tt and fl_ttt and fl_tttt and fl_ttttt and fl_tn and fl_tau and fl_tau2 and fl_div) then fl=[fl,i]
  tau[i,*]=tau_aats[leg1[i],*]-tau_aats[near_leg2[i],*]
  dtau[i,*]=sqrt((dtau_aats[leg1[i],*])^2.+(tau_aats[near_leg2[i],*])^2.)
endfor
if n_elements(fl) le 1 then print, '*** all points filtered out ***'
fl=fl[1:*]
print, 'number of valid spectras:',n_elements(fl) 

  legs={leg1:leg1,leg2:leg2}
  topt=mean(alt[leg2],/nan)/1000.0
  bottomt=mean(alt[leg1],/nan)/1000.0
  near={nabove:nabove,nbelow:nbelow,zabove_attcorr:zabove,zbelow_attcorr:zbelow}

atm_in = 'atmos_aero_'+date+'_at.dat'   ;input atmosphere file

; aats
  wvl_arr=lambda
  print, wvl_arr
  iw_f=n_elements(lambda)-1
  ia_arr=findgen(n_elements(lambda))
   
print, 'number of wavelength to iterate:',iw_f
print, 'number of points to iterate:',n_elements(fl)
fl_f=n_elements(fl)-1 ;number of points to go through
fl_s=0; start of points
tau_approx=0

print, 'number of points in file:', n_elements(wvl0353.field01[0,*])

rad_dn=fltarr(iw_f+1,fl_f+1,2)
no_rad_dn=fltarr(iw_f+1,fl_f+1,2)
rad_up=fltarr(iw_f+1,fl_f+1,2)
no_rad_up=fltarr(iw_f+1,fl_f+1,2)
aot=fltarr(iw_f+1,fl_f+1)
stop
;*******************************************************************
for iw=0, iw_f do begin ;wavelength loop
  aero_wl=ia_arr[iw]
  case iw of
  0:rtm=wvl0353
  1:rtm=wvl0380
  2:rtm=wvl0452
  3:rtm=wvl0499
  4:rtm=wvl0519
  5:rtm=wvl0605
  6:rtm=wvl0675
  7:rtm=wvl0779
  8:rtm=wvl0864
  9:rtm=wvl1019
  10:rtm=wvl1241
  11:rtm=wvl1558
  12:rtm=wvl2139
  endcase
  for i=fl_s, fl_f do begin ;track loop
    index=fl[i]  ; index for the point along track
    ;print, 'aats tau:',reform(tau[index,*])
    ;print, 'aats wvl:',lambda
    write_input_aats, tau[index,aero_wl], topt, bottomt, tau_aats[near_leg2[index],aero_wl] ,$
     atm_in=atm_in, index=index, tau_out=tau_out, /quiet, tau_approx=tau_approx
  
    input_file=dir+'aero_'+date+'.inp'
    output_file=dirout+'aero_'+date+'.out'
  
    doy=julian_day(fix(strmid(date,0,4)),fix(strmid(date,4,2)),fix(strmid(date,6,2)))
    zensun, doy, utc[legs.leg1[index]],lat[legs.leg1[index]], lon[legs.leg1[index]], sza, azimuth, solfac
    if bottomt le 0. then bottomt=0.1
    zout=[0.,bottomt,topt] 
    
    wvl=[fix(wvl_arr[iw]),fix(wvl_arr[iw])]
  ;  print, iw, wvl
  ;  stop
    mm=max(zout, top)
    bottom=1
    mm=min(abs(nadlambda-wvl[0]),wvl_index)

;#lat  lon     ssa   asy       asy2    albedo          correction    tau modification  
    ssa=rtm.field01[2,i]
    asy=rtm.field01[3,i]
    albedo=rtm.field01[5,i]
    tau_scale=rtm.field01[7,i]
    tauscale=tau_scale
print, ssa, asy, albedo, tau_scale
;stop
  
  if (finite(ssa) and finite(asy) and finite(albedo) and finite(tau_scale)) then begin
  
  ; run with retrieved aerosol values
  write_input_file, doy, sza, dir+tau_out, dir+atm_in, input_file, azimuth, asy, ssa, wvl, zout,albedo, /radiance,/quiet, tau_scale=tauscale,/slit
    if linux then spawn, '/home/leblanc/libradtran/libRadtran-1.5-beta/bin/uvspec < '+input_file+' > '+output_file , message else message, 'Must be under linux'
    if message ne '' gt 0 then message, message
    output=libradtran_reader(file = output_file,/radiance,zout = zout,/quiet)

  ; run with no aerosols
  write_input_file, doy, sza, dir+tau_out, dir+atm_in, input_file, azimuth, asy, ssa, wvl, zout,albedo, /radiance,/quiet, tau_scale=tauscale, /no_aero,/slit
    if linux then spawn, '/home/leblanc/libradtran/libRadtran-1.5-beta/bin/uvspec < '+input_file+' > '+output_file , message else message, 'Must be under linux'
    if message ne '' gt 0 then message, message
    output_no=libradtran_reader(file = output_file,/radiance,zout = zout,/quiet)

  rad_up[iw,i,*]=output.dif_up[1:*]
  no_rad_up[iw,i,*]=output_no.dif_up[1:*]
  rad_dn[iw,i,*]=output.dif_dn[1:*]+output.dir_dn[1:*]
  no_rad_dn[iw,i,*]=output_no.dif_dn[1:*]+output_no.dir_dn[1:*]
  
  aot[iw,i]=tau[index,aero_wl] ;rtm.field01[11,i]
  endif else begin
  rad_up[iw,i,*]=[!values.f_nan,!values.f_nan]
  no_rad_up[iw,i,*]=[!values.f_nan,!values.f_nan]
  rad_dn[iw,i,*]=[!values.f_nan,!values.f_nan]
  no_rad_dn[iw,i,*]=[!values.f_nan,!values.f_nan]
  aot[iw,i]=!values.f_nan
  endelse
  endfor
  print, wvl
endfor  
  
forcing=rad_up*0.0
eff=rad_up*0.0
effp=rad_up*0.0
for iw=0, iw_f do begin
  for i=fl_s, fl_f do begin ;track loop
  forcing[iw,i,0]=(rad_dn[iw,i,0]-rad_up[iw,i,0])-(no_rad_dn[iw,i,0]-no_rad_up[iw,i,0])
  forcing[iw,i,1]=(rad_dn[iw,i,1]-rad_up[iw,i,1])-(no_rad_dn[iw,i,1]-no_rad_up[iw,i,1])
  eff[iw,i,0]=forcing[iw,i,0]/aot[3,i]
  eff[iw,i,1]=forcing[iw,i,1]/aot[3,i]
  effp[iw,i,0]=eff[iw,i,0]/rad_dn[iw,i,1]*100.
  effp[iw,i,1]=eff[iw,i,1]/rad_dn[iw,i,1]*100.
  endfor
endfor

t=2
ssa=[(wvl0353.field01[t,*]),(wvl0380.field01[t,*]),(wvl0452.field01[t,*]),(wvl0499.field01[t,*]),(wvl0519.field01[t,*]),(wvl0605.field01[t,*]),(wvl0675.field01[t,*]),(wvl0779.field01[t,*]),(wvl0864.field01[t,*]),(wvl1019.field01[t,*]),(wvl1241.field01[t,*]),(wvl1558.field01[t,*]),(wvl2139.field01[t,*])]
t=3
asy=[(wvl0353.field01[t,*]),(wvl0380.field01[t,*]),(wvl0452.field01[t,*]),(wvl0499.field01[t,*]),(wvl0519.field01[t,*]),(wvl0605.field01[t,*]),(wvl0675.field01[t,*]),(wvl0779.field01[t,*]),(wvl0864.field01[t,*]),(wvl1019.field01[t,*]),(wvl1241.field01[t,*]),(wvl1558.field01[t,*]),(wvl2139.field01[t,*])]
t=4
asy2=[(wvl0353.field01[t,*]),(wvl0380.field01[t,*]),(wvl0452.field01[t,*]),(wvl0499.field01[t,*]),(wvl0519.field01[t,*]),(wvl0605.field01[t,*]),(wvl0675.field01[t,*]),(wvl0779.field01[t,*]),(wvl0864.field01[t,*]),(wvl1019.field01[t,*]),(wvl1241.field01[t,*]),(wvl1558.field01[t,*]),(wvl2139.field01[t,*])]
t=5
albedo=[(wvl0353.field01[t,*]),(wvl0380.field01[t,*]),(wvl0452.field01[t,*]),(wvl0499.field01[t,*]),(wvl0519.field01[t,*]),(wvl0605.field01[t,*]),(wvl0675.field01[t,*]),(wvl0779.field01[t,*]),(wvl0864.field01[t,*]),(wvl1019.field01[t,*]),(wvl1241.field01[t,*]),(wvl1558.field01[t,*]),(wvl2139.field01[t,*])]
;#lat  lon     ssa   asy       asy2    albedo          correction    tau modification        flux divergence  model down  model up  tau'

save, rad_up,no_rad_up,rad_dn, no_rad_dn, aot, wvl_arr,forcing, eff,effp,ssa, asy,asy2,albedo, filename='/home/leblanc/arctas/nasa/'+date+'/'+date+'_forcing.out'

forcing_plot
stop

end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write input file program                                                    ;;
;; used to make the input file for uvspec                                      ;;
;; use radiance keyword to set radiance (zenith pointing)                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro write_input_file, doy, sza, tau_file, atm_file, input_file, azimuth, asy, ssa, wvl, zout, albedo, radiance=radiance, quiet=quiet, tau_scale=tau_scale, no_aero=no_aero, slit=slit

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

if keyword_set(slit) then begin ; if set, use slit function, must change for IngAas
  if wvl[0] le 940 then printf,ui,'slit_function_file /home/leblanc/libradtran/vis_1nm.dat' else printf,ui,'slit_function_file /home/leblanc/libradtran/nir_1nm.dat'
endif

printf,ui,'quiet'
free_lun,ui

if not keyword_set(quiet) then print, 'input file saved to: '+input_file

end
