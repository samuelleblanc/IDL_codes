; program to run the retrieval for a very specific case


@write_input_tau.pro
@/home/leblanc/libradtran/pro/libradtran_reader.pro
pro arctas_rtm_case
dir='/home/leblanc/arctas/'

restore,dir+'sam.out'


ssa=fltarr(13)
asy=fltarr(13)
asy2=fltarr(13)
alb=fltarr(13)
fe=fltarr(13)


sza=mean(szas)
atm_in='/home/leblanc/libradtran/libRadtran-1.6-beta/data/atmmod/afglss.dat'
input_file='/home/leblanc/libradtran/input/arctas.in'
output_file='/home/leblanc/libradtran/output/arctas.out'
doy=100
zout=[500.,1500.]/1000.

azimuth=90.
for v=0, n_elements(lamda_aats)-1 do begin
  ;interpolate the measured spectra from tau0 to tau max
  zen=interpol(zens[v,*]/cos(szas*!dtor)*cos(sza*!dtor),taus[v,*],[0.,max(taus[v,*])])
  nad=interpol(nads[v,*],taus[v,*],[0.,max(taus[v,*])])
  ln=linfit(taus[v,*],nads[v,*])
  nad[0]=ln[0]
  lz=linfit(taus[v,*],zens[v,*])
  zen[0]=lz[0]

  ; set random starting values for input file
  asy[v]=0.6 & asy2[v]=0.6 & ssa[v]=0.8
  wvl=[lamda_aats[v],lamda_aats[v]]

  big=1
  ct_big=0
  alb[v]=nad[0]/zen[0]
  albedo=alb[v]

  tau_out='tau.out'
  correc_fac=1.
  top=1 & bottom=0
  corr_eps=0.6

  write_input_tau_file,[0.55],max(taus[v,*]),atm_in ,dir+tau_out
  while big do begin ;big loop
    ct_big=ct_big+1
    ssa_loop=1
    ct_ssa=0
    while ssa_loop do begin
      ct_ssa=ct_ssa+1
      write_input_file, doy, sza, dir+tau_out, atm_in, input_file, 90., asy[v], ssa[v], wvl, zout,alb[v],/radiance,/quiet,/slit 
      spawn, '/home/leblanc/libradtran/libRadtran-1.6-beta/bin/uvspec < '+input_file+' > '+output_file

      output=libradtran_reader(file = output_file,/radiance,zout = zout,/quiet)

          ; get correction factor from the modeled downward irradiance (top) to the measured downward irradiance (top)
          correction=(output.dir_dn[top] + output.dif_dn[top])/zen[top]
          z_top    = zen[1] * correction
          z_bottom = zen[0]  * correction
          n_top    = nad[1] * correction
          n_bottom = nad[0] * correction
          correc_fac=correc_fac*correction

          ;to correct the albedo
          model_albedo=output.dif_up[bottom]/(output.dir_dn[bottom]+output.dif_dn[bottom])
          albedo_correction=(n_bottom/z_bottom)/model_albedo
          albedo=albedo*albedo_correction
          if not finite(albedo) then albedo=old_albedo
          if albedo gt 1.0 then albedo =1.0

          ; change ssa for next iteration
          model=-(output.dif_up[top] - (output.dir_dn[top] + output.dif_dn[top]))+(output.dif_up[bottom] - (output.dir_dn[bottom] + output.dif_dn[bottom]))
          measured=-(n_top-z_top)+(n_bottom-z_bottom)

          dif_f=model

          if abs(model-measured) lt 0.001 or abs(model/measured) lt 0.000001 and correction gt corr_eps then begin ;if within epsiolon of model and measured then proceed
            ssa_loop=0  ;converged
          endif else begin
            if model/measured lt 0 || correction le corr_eps || correction gt 1.02 then begin
              ssa_loop=0
              ssa[v]=!values.f_nan & asy[v]=!values.f_nan
              dn=ssa & up=ssa & tau_mod=ssa & tau_now=tau_mod
              goto, failed
            endif else ssa[v]=ssa[v]*(model/measured)^0.08  ;modify ssa for next value
            if not finite(ssa[v]) then begin ;check to make sure the modification did not create a NaN
              if model gt 0 then print, 'model is off' , model
              if measured gt 0 then print, 'measured is off', measured
              stop
              ssa_loop=0 & big_loop=0
              asy=!values.f_nan & asy2=!values.f_nan
              goto, failed
            endif
            if ssa gt 1. then ssa=1.0
            if ssa le 0. then ssa=0.000001
          endelse

          if count_ssa gt 30 then begin
            ssa_loop=0  ;did not converge
            ;print, 'did not converge'
            ;print, count_ssa
          endif
    endwhile ;end ssa loop
if count_ssa gt 30 then print, 'ssa did not converge for index: '+strtrim(string(i),2)

      ; asy loop
      asy_loop=1 & count_asy=0

      while asy_loop do begin
        count_asy=count_asy +1
        if not finite(asy) then read, asy, prompt='ASY out of range! please enter new ASY (0-1):'
        write_input_file, doy, sza, dir+tau_out, dir+atm_in, input_file, azimuth, asy, ssa, wvl, zout,albedo, /radiance,/quiet, tau_scale=tauscale, /slit
        if linux then spawn, '/home/leblanc/libradtran/libRadtran-1.6-beta/bin/uvspec < '+input_file+' > '+output_file,message else message, 'Must be under linux'
        if message ne '' gt 0 then begin
          if message eq 'Segmentation fault' then spawn, '/home/leblanc/libradtran/libRadtran-1.6-beta/bin/uvspec < '+input_file+' > '+output_file,message
          if message ne '' gt 0 then message, message
        endif
        output=libradtran_reader(file = output_file,/radiance,zout = zout,/quiet)

        ; get correction factor from the modeled downward irradiance (top) to the measured downward irradiance (top)
        correction=(output.dir_dn[top] + output.dif_dn[top])/zen[top]

        ;correct measurements to fit into model
        z_top    = zen[top] * correction
        z_bottom = zen[bottom]  * correction
        n_top    = nad[top] * correction
        n_bottom = nad[bottom] * correction
        correc_fac=correc_fac*correction

        ;to correct the albedo
        model_albedo=output.dif_up[bottom]/(output.dir_dn[bottom]+output.dif_dn[bottom])
        albedo_correction=(n_bottom/z_bottom)/model_albedo
        albedo=albedo*albedo_correction
        if not finite(albedo) then albedo=albedo_t1  ; set albedo back to initial value for non finite values (for next guess)
        if albedo gt 1.0 then albedo=1.0 ;make sure albedo doesn't go over 1.0

        ; change asy for next iteration
        model=(output.dir_dn[bottom] + output.dif_dn[bottom])
        measured=z_bottom
        dn=model

        if abs(model/measured) lt 0.000001 or abs(model-measured) lt 0.008*measured and correction gt corr_eps then begin
          asy_loop=0  ;converged
        endif else begin
          if correction le corr_eps || correction gt 1.02 then begin
              asy_loop=0
              ssa=!values.f_nan & asy=!values.f_nan
              dn=ssa & up=ssa & tau_mod=ssa & tau_now=tau_mod
              goto, failed
          endif
          asy_t=0
          if not asy_t then asy=asy*(measured/model)^1. else read, asy, prompt='asy:'
          if asy ge 0.98 then asy=0.9799
          if asy le 0. then asy=0.000001
        endelse
        if count_asy gt 30 then asy_loop=0  ;did not converge
  endwhile  ;end of asy loop
      if count_asy gt 30 then print, 'asy did not converge for index: '+strtrim(string(i),2)

      ; asy second loop to help retrieve tau
      asy2_loop=1 & count_asy2=0

      while asy2_loop do begin
        count_asy2=count_asy2 +1
        if not finite(asy) then read, asy2, prompt='ASY2 out of range! please enter new ASY2 (0-1):'
        write_input_file, doy, sza, dir+tau_out, atm_in, input_file, azimuth, asy2, ssa, wvl, zout,albedo, /radiance,/quiet, tau_scale=tauscale, /slit
        if linux then spawn, '/home/leblanc/libradtran/libRadtran-1.6-beta/bin/uvspec < '+input_file+' > '+output_file,message else message, 'Must be under linux'
        if message ne '' gt 0 then begin
          if message eq 'Segmentation fault' then spawn, '/home/leblanc/libradtran/libRadtran-1.6-beta/bin/uvspec < '+input_file+' > '+output_file,message
          if message ne '' gt 0 then message, message
        endif
        output=libradtran_reader(file = output_file,/radiance,zout = zout,/quiet)

        ; get correction factor from the modeled downward irradiance (top) to the measured downward irradiance (top)
        correction=(output.dir_dn[top] + output.dif_dn[top])/zen[top]
        z_top    = zen[top] * correction
        z_bottom = zen[bottom]  * correction
        n_top    = nad[top] * correction
        n_bottom = nad[bottom] * correction
        correc_fac=correc_fac*correction

        ;to correct the albedo
        model_albedo=output.dif_up[bottom]/(output.dir_dn[bottom]+output.dif_dn[bottom])
        albedo_correction=(n_bottom/z_bottom)/model_albedo
        albedo=albedo*albedo_correction
        if not finite(albedo) then albedo=albedo_t1
        if albedo gt 1.0 then albedo=1.0

        ; change asy2 for next iteration
        model=output.dif_up[top]
        measured=n_top

        up=model

        dif_f=(-(output.dif_up[top] - (output.dir_dn[top] + output.dif_dn[top]))+(output.dif_up[bottom] - (output.dir_dn[bottom] + output.dif_dn[bottom])))/correction
        if abs(model/measured) lt 0.000001 or abs(model-measured) lt 0.002 and correction gt corr_eps then begin
          asy2_loop=0  ;converged
        endif else begin
          if correction le corr_eps || correction gt 1.02 then begin
            asy2_loop=0
            ssa=!values.f_nan & asy=!values.f_nan
            dn=ssa & up=ssa & tau_mod=ssa & tau_now=tau_mod
            goto, failed
          endif
          asy2=asy2*(model/measured)^1.0
          if asy2 ge 0.98 then asy2=0.9799

if asy2 le 0. then asy2=0.000001
        endelse
        if count_asy2 gt 30 then begin
          asy2_loop=0  ;did not converge
        endif

      endwhile  ;end of asy2 loop

if abs(ssa-old_ssa) lt 0.01 and abs(asy-old_asy) lt 0.01 and count_ssa lt 31 then big_loop=0
if ct_big gt 5 then big= 0
failed:
  endwhile

print, lamda_aats[v],ssa[v],asy[v],asy2[v]
fe[v]=(zen[1]-nad[1])-(zen[0]-nad[0])
print, fe[v]
endfor


stop
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write input file program                                                    ;;
;; used to make the input file for uvspec                                      ;;
;; use radiance keyword to set radiance (zenith pointing)                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro write_input_file, doy, sza, tau_file, atm_file, input_file, azimuth, asy, ssa, wvl, zout, albedo, radiance=radiance, quiet=quiet, tau_scale=tau_scale, slit=slit

ui=99
openw,ui,input_file;,/get_lun
printf,ui,'data_files_path   /home/leblanc/libradtran/libRadtran-1.6-beta/data'
printf,ui,'solar_file        /home/leblanc/libradtran/libRadtran-1.6-beta/data/solar_flux/kurudz_1.0nm.dat'
printf,ui,'atmosphere_file   '+atm_file

;printf,ui,'albedo_library    IGBP    # Spectral albedo library '
;printf,ui,'surface_type      13      # urban albedo from library'
if albedo lt 0 then albedo=albedo*(-1.)
if albedo gt 1.0 then albedo=1.0
if albedo lt 0.00001 then albedo=0.00001
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

printf,ui,'aerosol_default'                   ; initialize aerosol
printf,ui,'aerosol_tau_file  '+tau_file
printf,ui,'aerosol_set_ssa   '+string(ssa)
printf,ui,'aerosol_set_gg    '+string(asy)
if keyword_set(tau_scale) then printf, ui, 'aerosol_scale_tau   '+string(tau_scale)

printf,ui,'wavelength        '+string(floor(wvl[0]))+'  '+string(floor(wvl[1])) ; wavelength used for hsrl
printf,ui,'altitude          0.02' ;0.263 # elevation (ASL) of CalTech in km'      
printf,ui,'zout              '+string(zout, format='('+strtrim(string(n_elements(zout)),2)+'(" ",F5.1))')

;if keyword_set(slit) then begin ; if set, use slit function, must change for IngAas
;  if wvl[0] le 940 then printf,ui,'slit_function_file /home/leblanc/libradtran/vis_1nm.dat' else printf,ui,'slit_function_file /home/leblanc/libradtran/nir_1nm.dat'
;endif

printf,ui,'quiet'
free_lun,ui

if not keyword_set(quiet) then print, 'input file saved to: '+input_file

end

