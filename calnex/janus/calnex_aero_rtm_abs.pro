;+
; NAME:
;   calnex_aero_rtm_abs
;
; PURPOSE:
;   to do the aerosol retrieval on select days using the absolute minimum method
;
; CATEGORY:
;   CALNEX, Aerosol
;
; CALLING SEQUENCE:
;   calnex_aero_aero_abs,date
; - date - date of day to check
;
; OUTPUT:
;   plots
;   retrieved values in a text file
;
; KEYWORDS:
;   - use_wvl:    set this to the desired wvl index
;   - cluster:    sets the paths to the cluster paths
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
; Written:  Samuel LeBlanc, Ottawa, Ontario, Januray 9th, 2012
;           Based on calnex_aerosol_retrieval
;           Was written to simplify code
; Modified: 
;---------------------------------------------------------------------------
@write_input.pro
@zensun.pro
@/home/leblanc/libradtran/pro/libradtran_reader.pro
@/projects/leblanse/libradtran/pro/libradtran_reader.pro
@read_aeronet.pro

pro calnex_aero_rtm_abs, dateinp, use_wvl=use_wvl,cluster=cluster ;,$

close, /all
;set windows or linux
if not keyword_set(cluster) then cluster=0
if n_elements(dateinp) lt 1 then begin
  date='20100519'
endif else begin
  date=strcompress(string(dateinp),/REMOVE_ALL)
endelse

  calnex_dir='/home/leblanc/CALNEX/'
  ll='/'
  dir='/home/leblanc/libradtran/input/aero/'
  dirout='/home/leblanc/libradtran/output/aero/'

if cluster then begin
  calnex_dir='/projects/leblanse/CALNEX/'
  ll='/'
  dir='/scratch/stmp00/leblanse/libradtran/input/aero/'
  dirout='/scratch/stmp00/leblanse/libradtran/output/aero/'
endif

restore, calnex_dir+'p3'+ll+date+ll+date+'_p3_hsrl.out'

; determination of the nearest distance from leg2 to leg1
; writing out the distance
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;stopped here

sza_arr=sza
print, 'number of wavelength to iterate:',iw_f
print, 'number of points to iterate:',n_elements(fl)
fl_f=n_elements(fl)-1 ;number of points to go through
fl_s=0; start of points
;ia_arr=[0,1,2,3,4,5,6]
;iw_f=0
;stop
if keyword_set(use_wvl) then begin
  iw_s=use_wvl
  iw_f=use_wvl
endif else iw_s=0
for iw=iw_s, iw_f do begin ;wavelength loop
  aero_wl=ia_arr[iw]
  wl_string=strtrim(string(fix(wvl_aod[aero_wl]),format='(I4.4)'),2)
  if not keyword_set(no_tau_loop) then file_n='rtm_taumod_' else file_n='rtm_'  ; set up file writing routine for retrieval (at every point)
  if keyword_set(angstrom) then file_n='rtm_angstrom_'
  if crd then file_n='rtm_crd_'
  if abs_min then file_n=file_n+'abs_'
  lunn=98  
  openw, lunn, dirout+file_n+date+'_wvl'+wl_string+secon+'.txt';,/get_lun
  printf, lunn, '#lat  lon     ssa   asy       asy2    albedo          correction    tau modification        flux divergence  model down  model up  tau'
  
  for i=fl_s, fl_f do begin ;track loop
    
    index=fl[i]  ; index for the point along track
    tau_out=dir+'aero_'+wl_string+'.level'
    write_input_tau_file, z, near, atm_in=atm_in, index=index, tau_out=tau_out, /quiet, alt=alt, tau_good=tau_alt
    
    input_file=dir+'aero_'+date+'_'+wl_string+'.inp'
    output_file=dirout+'aero_'+date+'_'+wl_string+'.out'
    if date eq '20100519' then doy= 139
    doy=julian_day(strmid(date,0,4),strmid(4,2),strmid(6,2))
    zensun, doy, utc[legs.leg1[index]],lat[legs.leg1[index]], lon[legs.leg1[index]], sza, azimuth, solfac
    zout=[0.,0.5,1.0] ; need to set to right altitudes of flight path currently good for may 19th
    
   if aeronet then  mm=min(abs(jul_day-doy+utc[legs.leg1[index]]/24.0),aeronet_index)  ;get the closest in time aeronet index
    
    wvl=[fix(wvl_arr[iw]),fix(wvl_arr[iw])]
    if fix(wvl_arr[iw]) ne fix(wvl_aod[aero_wl]) then message, 'wvl arrays not equal!'
    mm=max(zout, top)
    bottom=1
    mm=min(abs(nadlambda-wvl[0]),wvl_index)
    
    if aeronet then begin
      ;to find the tau aeronet at 532 (HSRL or CRD)
      tau_532=interpol(alog(aod_aeronet[ia_arr,aeronet_index]),alog(wvl_arr),alog(532.))
      tau_532=exp(tau_532)
      tauscale1=tau_s*aod_aeronet[aero_wl,aeronet_index]/tau_532
    endif else begin
      tauscale1=tau_s ;set it to the scaling factor only
    endelse
    
    ;starting values
    albedo   = near.nbelow[wvl_index, index] / near.zbelow_attcorr[wvl_index,index]
    z_top    = near.zabove_attcorr[wvl_index,index] 
    z_bottom = near.zbelow_attcorr[wvl_index,index]
    n_top    = near.nabove[wvl_index,index]
    n_bottom = near.nbelow[wvl_index,index] 
    correc_fac=1.

    ;make loops that writes input file, then runs uvspec, then reads the output, then modifies the different values
    
    ;tau_loop
    tau_loop=1 & count_tau=0 & tau_mod=1.0 & tau_init=tauscale1 & tau_old=0. & redo_tau=0
      
    tau_mods=findgen(22)*0.04+0.6  ;iterate through +/- 40%
    tau_mods=tau_mods[sort(abs(tau_mods-1.))]
    tau_mods=tau_mods[1:*]
    tau_mods[1]=1.02 & tau_mods[2]=0.98
    
    ;make array to keep g differences for each tau_mods for use with abs_min
    g_diffs=tau_mods*!values.f_nan
    t_mins =tau_mods*!values.f_nan
    ;initialise the different arrays that contains each of the different values associated with a change in tau
    ssa_min=g_diffs & asy_min=g_diffs & asy2_min=g_diffs & alb_min=g_diffs & corr_min=g_diffs & tnow_min=g_diffs & dif_min=g_diffs 

    ;new tau mods system
    ;tau_mods=[1.01,0.99,1.02,0.98,1.03,0.97,1.04,0.96,1.05,0.95,1.07,0.93,1.09,0.91,1.12,0.88,1.16,0.82,
    
    sep_asys=fltarr(21)
    old_tau_mod=fltarr(21)
    while tau_loop do begin
      if count_tau eq 0 then begin
        tauscale=tau_init
        ssa_ct=1
      endif
      count_tau=count_tau+1
    
      big_loop=1 & count_big=0
      old_count_ssa=0 & old_count_asy=0
   
      print, 'starting big loop' 
      ;start of big loop 
      while big_loop do begin
        count_big=count_big +1
        old_albedo=albedo & old_asy=asy & old_asy2=asy2 & old_ssa=ssa
    
        ct_big_asy2=0
      big_asy2:
    
        ; ssa loop
        if ssa_ct then ssa_loop=1 else ssa_loop=0
        count_ssa=0
      
        while ssa_loop do begin   ;in the ssa loop
          count_ssa=count_ssa +1
          if not finite(ssa) then message,'SSA out of range';read, ssa, prompt='SSA out of range! please enter new SSA (0-1): ('+strtrim(string(ssa),2)+') '
          write_input_file, doy, sza, tau_out, dir+atm_in, input_file, azimuth, asy, ssa, wvl, zout,albedo, /radiance,/quiet, tau_scale=tauscale, /slit
          if not cluster then spawn, '/home/leblanc/libradtran/libRadtran-1.5-beta/bin/uvspec < '+input_file+' > '+output_file , message
          if cluster then spawn, '/home/leblanse/libradtran/libRadtran-1.6-beta/bin/uvspec < '+input_file+' > '+output_file, message
          if n_elements(message) gt 10 then begin
            if message[10] eq 'Segmentation fault' then spawn, '/projects/leblanse/libradtran/libRadtran-1.6-beta/bin/uvspec < '+input_file+' > '+output_file,message
            if n_elements(message) gt 10 then message, message[10]
          endif
          output=libradtran_reader(file = output_file,/radiance,zout = zout,/quiet)
      
          ; get correction factor from the modeled downward irradiance (top) to the measured downward irradiance (top)
          correction=(output.dir_dn[top] + output.dif_dn[top])/near.zabove_attcorr[wvl_index,index]
          z_top    = near.zabove_attcorr[wvl_index,index] 
          z_bottom = near.zbelow_attcorr[wvl_index,index]
          n_top    = near.nabove[wvl_index,index]
          n_bottom = near.nbelow[wvl_index,index] 
          
          ;correct measurements to fit into model
          z_top    = z_top * correction
          z_bottom = z_bottom  * correction
          n_top    = n_top * correction
          n_bottom = n_bottom * correction
          correc_fac=correc_fac*correction
          ;to correct the albedo
          model_albedo=output.dif_up[bottom]/(output.dir_dn[bottom]+output.dif_dn[bottom])
          albedo_correction=(n_bottom/z_bottom)/model_albedo
          albedo=albedo*albedo_correction
          if not finite(albedo) then albedo=old_albedo
      
          ; change ssa for next iteration
          model=-(output.dif_up[top] - (output.dir_dn[top] + output.dif_dn[top]))+(output.dif_up[bottom] - (output.dir_dn[bottom] + output.dif_dn[bottom]))
          measured=-(n_top-z_top)+(n_bottom-z_bottom)
          dif_f=model
      
          if abs(model-measured) lt 0.001 or abs(model/measured) lt 0.000001 and correction gt corr_eps then begin ;if within epsilon of model and measured then proceed
            ssa_loop=0  ;converged
          endif else begin
            if model/measured lt 0 || correction le corr_eps then begin 
              if not abs_min then begin
                ssa_loop=0
                ssa=!values.f_nan
                asy=!values.f_nan
                dn=ssa & up=ssa
                tau_mod=ssa
                tau_now=tau_mod
	        print, 'correction factor outside range'
                goto, failed
              endif else begin
                ssa_loop=0
                asy=0.01 & asy2=0.9
                goto, outside
              endelse
            endif else begin
	            if keyword_set(interactive_ssa) then begin
	          	 print, 'Curent SSA value at:', ssa, ' next value at:', ssa*(model/measured)^0.08
		           print, 'model value:', model, ' measured:',measured
		           read, ssa, prompt='enter new SSA value:'
              endif else ssa=ssa*(model/measured)^0.08 ;modify ssa for next value
            endelse
	         if not finite(ssa) then begin ;check to make sure the modification did not create a NaN
              if model gt 0 then print, 'model is off' , model
              if measured gt 0 then print, 'measured is off', measured
              ;stop
              if not abs_min then begin
                ssa_loop=0 & big_loop=0
                asy=!values.f_nan & asy2=!values.f_nan
                goto, failed
              endif else begin
                ssa_loop=0 & big_loop=0
                asy=0.1 & asy2=0.9
                goto, outside
              endelse
            endif
            if ssa gt 1. then ssa=1.0
            if ssa le 0. then ssa=0.000001
          endelse
          
          if tau[index] le 0. then begin
                        ssa_loop=0
              ssa=!values.f_nan
              asy=!values.f_nan
              dn=ssa & up=ssa
              tau_mod=ssa
              tau_now=tau_mod
	      print, 'tau less than 0'
              ;stop
              goto, failed
          endif
      
          if count_ssa gt 50 then begin
            ssa_loop=0  ;did not converge
            print, 'did not converge'
            print, count_ssa
          endif
          if correction gt 2. then message, 'Correction great than 2 at ssa loop'
        endwhile  ;end of ssa loop 
      if count_ssa gt 50 then print, 'ssa did not converge for index: '+strtrim(string(index),2)

        ; asy loop
        asy_loop=1
        count_asy=0
      
        while asy_loop do begin
        count_asy=count_asy +1
        if not finite(asy) then read, asy, prompt='ASY out of range! please enter new ASY (0-1):'
        write_input_file, doy, sza, tau_out, dir+atm_in, input_file, azimuth, asy, ssa, wvl, zout,albedo, /radiance,/quiet, tau_scale=tauscale, /slit
        if linux and not cluster then spawn, '/home/leblanc/libradtran/libRadtran-1.5-beta/bin/uvspec < '+input_file+' > '+output_file,message
        if cluster then spawn, '/home/leblanse/libradtran/libRadtran-1.6-beta/bin/uvspec < '+input_file+' > '+output_file, message 
        if n_elements(message) gt 10 then begin
            if message[10] eq 'Segmentation fault' then spawn, '/projects/leblanse/libradtran/libRadtran-1.6-beta/bin/uvspec < '+input_file+' > '+output_file,message
            if n_elements(message) gt 10 then message, message[10]
          endif
output=libradtran_reader(file = output_file,/radiance,zout = zout,/quiet)
      
        ; get correction factor from the modeled downward irradiance (top) to the measured downward irradiance (top)
        correction=(output.dir_dn[top] + output.dif_dn[top])/near.zabove_attcorr[wvl_index,index]
        z_top    = near.zabove_attcorr[wvl_index,index] 
        z_bottom = near.zbelow_attcorr[wvl_index,index]
        n_top    = near.nabove[wvl_index,index]
        n_bottom = near.nbelow[wvl_index,index] 
        
        ;correct measurements to fit into model
        z_top    = z_top * correction
        z_bottom = z_bottom  * correction
        n_top    = n_top * correction
        n_bottom = n_bottom * correction
        correc_fac=correc_fac*correction
        
        ;to correct the albedo
        model_albedo=output.dif_up[bottom]/(output.dir_dn[bottom]+output.dif_dn[bottom])
        albedo_correction=(n_bottom/z_bottom)/model_albedo
        albedo=albedo*albedo_correction
        if not finite(albedo) then albedo=albedo_t1  ; set albedo back to initial value for non finite values (for next guess)
 
        ; change asy for next iteration
        model=(output.dir_dn[bottom] + output.dif_dn[bottom])
        measured=z_bottom
        dn=model
         
        if abs(model/measured) lt 0.000001 or abs(model-measured) lt 0.008*measured and correction gt corr_eps then begin
          asy_loop=0  ;converged
        endif else begin
          if correction le corr_eps then begin 
              if not abs_min then begin
                asy_loop=0
                ssa=!values.f_nan & asy=!values.f_nan
                dn=ssa & up=ssa & tau_mod=ssa & tau_now=tau_mod
	        print, 'correction factor is outside range'
                goto, failed
              endif else begin
                asy_loop=0
                asy=0.1 & asy=0.9
                goto, outside
              endelse
          endif
          asy_t=0
          if not asy_t then asy=asy*(measured/model)^1. else read, asy, prompt='asy:'
          if asy ge 0.98 then asy=0.9799
          if asy le 0. then asy=0.000001
        endelse
        if count_asy gt 40 then asy_loop=0  ;did not converge
      endwhile  ;end of asy loop
      if count_asy gt 40 then print, 'asy did not converge for index: '+strtrim(string(index),2)
            
      ; asy second loop to help retrieve tau
      asy2_loop=1 & count_asy2=0
    
      while asy2_loop do begin
        count_asy2=count_asy2 +1
        if not finite(asy) then read, asy2, prompt='ASY2 out of range! please enter new ASY2 (0-1):'
        write_input_file, doy, sza, tau_out, dir+atm_in, input_file, azimuth, asy2, ssa, wvl, zout,albedo, /radiance,/quiet, tau_scale=tauscale, /slit
        if linux and not cluster then spawn, '/home/leblanc/libradtran/libRadtran-1.5-beta/bin/uvspec < '+input_file+' > '+output_file,message
        if cluster then spawn, '/home/leblanse/libradtran/libRadtran-1.6-beta/bin/uvspec < '+input_file+' > '+output_file, message
        if n_elements(message) gt 10 then begin
            if message[10] eq 'Segmentation fault' then spawn, '/projects/leblanse/libradtran/libRadtran-1.6-beta/bin/uvspec < '+input_file+' > '+output_file,message
            if n_elements(message) gt 10 then message, message[10]
          endif
 output=libradtran_reader(file = output_file,/radiance,zout = zout,/quiet)
    
        ; get correction factor from the modeled downward irradiance (top) to the measured downward irradiance (top)
        correction=(output.dir_dn[top] + output.dif_dn[top])/near.zabove_attcorr[wvl_index,index]
        z_top    = near.zabove_attcorr[wvl_index,index] 
        z_bottom = near.zbelow_attcorr[wvl_index,index]
        n_top    = near.nabove[wvl_index,index]
        n_bottom = near.nbelow[wvl_index,index] 
        
        ;correct measurements to fit into model
        z_top    = z_top * correction
        z_bottom = z_bottom  * correction
        n_top    = n_top * correction
        n_bottom = n_bottom * correction
        correc_fac=correc_fac*correction
        
        ;to correct the albedo
        model_albedo=output.dif_up[bottom]/(output.dir_dn[bottom]+output.dif_dn[bottom])
        albedo_correction=(n_bottom/z_bottom)/model_albedo
        albedo=albedo*albedo_correction
        if not finite(albedo) then albedo=albedo_t1
 
        ; change asy2 for next iteration
        model=output.dif_up[top]
        measured=n_top
    
        up=model

        if abs(model/measured) lt 0.000001 or abs(model-measured) lt 0.002 and correction gt corr_eps then begin
          asy2_loop=0  ;converged
        endif else begin
          if correction le corr_eps then begin 
            if not abs_min then begin
              asy2_loop=0
              ssa=!values.f_nan & asy=!values.f_nan
              dn=ssa & up=ssa & tau_mod=ssa & tau_now=tau_mod
              print, 'correction factor is outside range'
	      goto, failed
            endif else begin
              asy2_loop=0
              asy=0.1 & asy2=0.9
              goto, outside
            endelse
          endif
          asy2=asy2*(model/measured)^1.0
          if asy2 ge 0.98 then asy2=0.9799
          if asy2 le 0. then asy2=0.000001
        endelse
        if count_asy2 gt 40 then begin
          asy2_loop=0  ;did not converge
        endif

      endwhile  ;end of asy2 loop
      if count_asy2 gt 40 then begin
        print, 'asy2 did not converge for index: '+strtrim(string(i),2)
          if ct_big_asy2 gt 1 then begin
            print, 'Convergence did not happen because asy2 did not converge on second try'
            goto, outside     
          endif else begin 
            if count_asy lt 40 then begin
              ct_big_asy2= ct_big_asy2 +1
              ;goto, big_asy2
            endif else ct_big_asy2=0
          endelse
      endif else ct_big_asy2=0
      
      print, 'Iteration of big loop: '+strtrim(string(count_big),2)+' on index: '+strtrim(string(i),2)+'/'+strtrim(string(fl_f),2)+' wavelength: '+strtrim(string(wvl[0]),2)
      print, 'ssa count, ssa'
      print, count_ssa, ssa
      print, 'asy count, asy'
      print, count_asy, asy
      print, 'asy2 count, asy2'
      print, count_asy2, asy2
      print, 'correction factor: '+strtrim(string(correction),2)
      if aats then tau_now=tau[index,aero_wl]*tau_mod*tau_init else tau_now=tau[index]*tau_mod*tau_init
      print, 'tau: ',tau_mod*tau_init, '  actual tau:', tau_now, '  - end big loop'
      

      if abs(ssa-old_ssa) lt 0.01 and abs(asy-old_asy) lt 0.01 and count_ssa lt 51 then big_loop=0   ;check for convergence of big loop
      if count_big gt 6 then big_loop=0   ;no convergence
      if old_count_ssa eq 51 and count_ssa eq 51 and old_count_asy eq 41 and count_asy eq 41 then big_loop=0 
      old_count_ssa=count_ssa & old_count_asy=count_asy
    endwhile  ;end if big_loop    
    ssa_ct=1
    if count_big gt 6 then begin
      print, 'convergence did not happen during big loop for index: '+strtrim(string(index),2)
      if not no_tau_loop then begin
        print,'Modifying tau'      
      endif else begin
        if not abs_min then begin
          tau_mod=!values.f_nan & asy=!values.F_NAN & asy2=!values.F_NAN
          ssa=!values.f_nan & tau_now=tau_mod & tau_loop=0
          goto, failed
        endif else begin
          asy=0.1 & asy2=0.9 
          goto,outside
        endelse
      endelse
    endif

    if abs(asy-asy2) gt 0.5 and not abs_min then begin
      print, 'divergence of asy and asy2 too large, retrieval failed: ',abs(asy-asy2) 
      tau_mod=!values.f_nan & asy=!values.F_NAN &  asy2=!values.F_NAN
      ssa=!values.f_nan & tau_now=tau_mod &  tau_loop=0
      goto, failed
    endif
      
    outside:
    if tau_mod eq 1.0 then begin    ;save first values, for when the tau retrieval fails
      asy_t1=asy & asy2_t1=asy2 & ssa_t1=ssa & albedo_t1=albedo
    endif
    if no_tau_loop then tau_loop=0
    if ct_big_asy2 gt 1 and count_asy lt 40 then begin
      if not abs_min then begin
        tau_mod=!values.f_nan & asy=!values.F_NAN &  asy2=!values.F_NAN
        ssa=!values.f_nan & tau_now=tau_mod &  tau_loop=0
        goto, failed
      endif else begin
        asy=0.0 & asy2=1.0
        tau_mod=!values.f_nan & ssa=!values.f_nan & tau_now=tau_mod
      endelse
    endif
    
    if keyword_set(sensitivity) then begin  ;to write values of asy, asy2 compared to change of tau
      openw, 94, dirout+'tau_'+date+'_ssact_wvl'+strtrim(string(fix(lambda[aero_wl]),format='(I4.4)'),2)+'.dat',/append
      printf, 94, tau_mod, tau_now, ssa, asy, asy2, correction
      close, 94   
    endif
    
    ; part of code to get convergence of asy2 and asy2 by modifying tau slightly
    if (abs(asy-asy2) lt 0.025) and not keyword_set(sensitivity) and not abs_min and count_ssa lt 51 and count_asy lt 41 and count_asy2 lt 41 then begin
      print, 'tau converged at:', tau_mod*tau_init, ' with tau equals:',tau_now
      if keyword_set(constant_ssa) then begin
        ssa_ct=1      ; redo ssa
        tau_loop=1    ; go back to redo tau
        redo_tau=redo_tau+1
        print, 'in constant ssa tau loop, with tau-now, tau_old, diff:',tau_now,tau_old, abs(tau_now-tau_old)

        if abs(tau_now-tau_old) lt 0.05*tau_now then begin 
          tau_loop=0  ;exit new tau loop
        endif
        tau_old=tau_now
      endif else begin
        tau_loop=0 ; exit old tau loop
      endelse
    endif else begin
      if not no_tau_loop then begin 
        ssa=0.9 & asy = 0.6 & asy2 = 0.6
        if keyword_set(constant_ssa) then ssa=ssa_t1
        if keyword_set(interactive) then begin
          print, 'current tau mod set at:', tau_mod, ' which gives a total tau of:', tau_now
          read, tau_mod, prompt='enter new tau mod: '
        endif else begin
        if 0 then begin ;testing of the sep_asys method, disabled for now
          sep_asys[count_tau-redo_tau-1]=abs(asy-asy2)
          old_tau_mod[count_tau-redo_tau-1]=tau_mod
          if count_tau-redo_tau-1 ge 2 then begin
            if count_tau-redo_tau-1 eq 2 then begin
              nul= min(sep_asys[[0,1,2]],iy)
              ist=0 & isp=0
              case iy of
                0:begin
                  nul= min(sep_asys[[1,2]],iy2)
                  if iy2 eq 0 then tau_mod=mean(tau_mods[[0,1]]) else tau_mod=mean(tau_mods[[0,2]])
                  directions=1.0
                end
                1:begin 
                  tau_mod=tau_mod+0.02
                  directions=1.0
                end
                2:begin
                  tau_mod=tau_mod-0.02
                  directions=-1.0
                end
              endcase 
            endif else begin
              if sep_asys[count_tau-redo_tau-1] le sep_asys[count_tau-redo_tau-2] then begin
                if isp gt 10 then begin
		  directions=directions*(-1.0)
                  isp=0
		  tau_mod=1.0+0.04*directions
		endif else begin
		  isp=isp+1
		  tau_mod=old_tau_mod[count_tau-redo_tau-1]+0.04*directions
                endelse
	      endif else begin
                if sep_asys[count_tau-redo_tau-1] le sep_asys[count_tau-redo_tau-3] then begin
		  if ist gt 5 then begin
                    directions=directions*(-1.0)
                    tau_mod=2.0-old_tau_mod[count_tau-redo_tau-1] ;switch directions and check out another point
                  endif else begin   
                    tau_mod=(old_tau_mod[count_tau-redo_tau-1]+old_tau_mod[count_tau-redo_tau-2])/2.             
                    ist=ist+1
                  endelse
		endif else begin
                  tau_mod=(old_tau_mod[count_tau-redu_tau-2]+old_tau_mod[count_tau-redp_tau-3])/2.
		endelse
              endelse
            endelse
          endif else begin         
            tau_mod=tau_mods[count_tau-redo_tau-1]
          endelse
        endif else begin ;endif of the testing of the sep_asys start of the normal routine
          g_diffs[count_tau-redo_tau-1]=abs(asy-asy2) ;sets the g_diffs for use with abs_min keyword
          t_mins[count_tau-redo_tau-1]=tau_mod
          asy_min[count_tau-redo_tau-1]=asy
          asy2_min[count_tau-redo_tau-1]=asy2
          ssa_min[count_tau-redo_tau-1]=ssa
          corr_min[count_tau-redo_tau-1]=correction
          alb_min[count_tau-redo_tau-1]=albedo
          dif_min[count_tau-redo_tau-1]=dif_f
          tnow_min[count_tau-redo_tau-1]=tau_now
          tau_mod=tau_mods[count_tau-redo_tau-1]
          print, 'On the '+strtrim(count_tau-redo_tau-1)+'/'+strtrim(n_elements(g_diffs))+' tau iteration of the current point'
        endelse ;endelse of the abs_min section
        endelse ;endelse of the interactive if
      endif else tau_mod=1.0
      tauscale=tau_init*tau_mod
    endelse
    if count_tau gt 20 then begin
      tau_loop=0
      if not abs_min then begin
        print, 'tau did not converge for index: '+strtrim(string(index),2)
        tau_mod=!values.f_nan & tau_now=tau_mod & asy=asy_t1
        asy2=asy2_t1 & ssa=ssa_t1 & albedo=albedo_t1 & tau_loop=0
        goto, failed
      endif else goto, failed
    endif        
  endwhile   ; end of tau loop

  failed:
  nul=!values.f_nan
    ; if statement to find the smallest g difference and its equivalent values. must save the values as well...
  if abs_min then begin
    min_g=min(abs(g_diffs),i_min,/nan)
    if min_g gt 0.025 then begin
      ssa    = ssa_min[i_min]  & asy       = asy_min[i_min]  & asy2    = asy2_min[i_min]
      albedo = alb_min[i_min]  & correction= corr_min[i_min] & tau_mod = t_mins[i_min]
      tau_now= tnow_min[i_min] & dif_f     = dif_min[i_min]
    endif else begin
      ssa    = nul & asy       = nul & asy2    = nul
      albedo = nul & correction= nul & tau_mod = nul
      tau_now= nul & dif_f     = nul
    endelse
  endif
  ssa_arr[i]=ssa & asy_arr[i]=asy & asy2_arr[i]=asy2
  albedo_arr[i]=albedo & correc_fac_arr[i]=correction
  tau_mod_arr[i]=tau_mod & tau_arr[i]=tau_now & dif_flux[i] = dif_f
  if count_big gt 6 then begin
    print, 'convergence did not happen during big loop for index: '+strtrim(string(index),2)
    ssa=.9 & asy=0.6 & asy2=asy
  endif
  ssa=.9 & asy=0.6 & asy2=asy
  if keyword_set(constant_ssa) then ssa=ssa_t1
  ; file writing routine for retrieval results
  printf, lunn, lat[legs.leg1[fl[i]]],lon[legs.leg1[fl[i]]], ssa_arr[i], asy_arr[i], asy2_arr[i], albedo_arr[i], correc_fac_arr[i],tau_mod_arr[i], dif_flux[i],dn,up,tau_arr[i], format='(12F)'

endfor  ;longitude loop
 free_lun, lunn ;retrieval output file
endfor ; wavelength loop  
  
;stop
end
  
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write input file program                                                    ;;
;; used to make the input file for uvspec                                      ;;
;; use radiance keyword to set radiance (zenith pointing)                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro write_input_file, doy, sza, tau_file, atm_file, input_file, azimuth, asy, ssa, wvl, zout, albedo, radiance=radiance, quiet=quiet, tau_scale=tau_scale, slit=slit

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

printf,ui,'wavelength        '+string(wvl[0])+'  '+string(wvl[1]) ; wavelengths
printf,ui,'altitude          0.0' ;0.263 # elevation (ASL) of CalTech in km'      
printf,ui,'zout              '+string(zout, format='('+strtrim(string(n_elements(zout)),2)+'(" ",F5.1))')

if keyword_set(slit) then begin ; if set, use slit function, must change for IngAas
  if wvl[0] le 940 then printf,ui,'slit_function_file /projects/leblanse/libradtran/vis_1nm.dat' else printf,ui,'slit_function_file /projects/leblanse/libradtran/nir_1nm.dat'
endif

printf,ui,'quiet'
free_lun,ui

if not keyword_set(quiet) then print, 'input file saved to: '+input_file

end

