;
; PURPOSE:
;   Near real-time data processing for ATTREX, processes OSA2, CG4, and NAV data and then saves
;   it to an idl out file, for further plotting and cloud properties retrieval.
;   Supports incoming files of OSA2, CG4, cnd NAV at different times.
;   Will save to a file only the data at the times from the different inputs coincide   
;   Will only keep in memory the data that has not been saved to a file.
;
; CATEGORY:
;   ATTREX real-time processing
;
; CALLING SEQUENCE:
;   attrex_rt,dir
;   - where dir is the directory within the specified path to watch for incoming files
;
; OUTPUT:
;   IDL sav file, and present plots on screen
;
; KEYWORDS:
;   - none
;
; DEPENDENCIES:
;   - ingest_ssfr.pro
;   - ingest_temp.pro
;   - ingest_nav.pro
;   - ingest_cg4.pro
;   - cfg.pro
;   - get_response.pro
;   - get_wvl.pro
;   - get_cos.pro
;   - attrex_rt_plots.pro
;   
; NEEDED FILES:
;   - config file
;   - response functions
;   - temperature coefficients
;   - extraterrestial irradiance values
;   - spectral cosine response functions
;   - incoming OSA2 files
;   - incoming CG4 files
;   - incoming NAV files
;  
; EXAMPLE:
;  attrex_rt, '/data/seven/schmidt/attrex/gh/'
;
;
; MODIFICATION HISTORY:
; Written:  Samuel LeBlanc and Sebastian Schmidt, LASP CU Boulder, September 8th, 2011, Boulder
; Modified: 
;          
;---------------------------------------------------------------------------

@ingest_ssfr.pro
@ingest_temp.pro
@ingest_nav.pro
@ingest_cg4.pro
@cfg.pro
@get_response.pro
@get_wvl.pro
@get_cos.pro
@attrex_rt_plots.pro
@interpol.pro

pro attrex_rt,dir
seven=0 ;to toggle between using seven or not
if n_elements(dir) lt 1 then dir='/home/radusers/ATTREX/GHflt/'    ; change this at dryden
l='/'
if seven then path='' else path='' ;path+dir is where the incoming OSA2 files reside and also the .cfg file   ; change this at dryden
indir='sciflt' ;path to the OSA2 files on file server   ; change this at dryden
outdir='/home/radusers/ATTREX/IDL/IDL_OUT/' ;path to save the idl out files      ; change this at dryden
wait=5 ; seconds to wait

cfg_file='/home/radusers/ATTREX/GHflt/sciflt/gh.cfg'        ; build cfg file   ; change this at dryden
if file_test(cfg_file) ne 1 then message,'Did not find configuration file.'

; Read from cfg
date     = cfg(cfg_file,'date') ;read the date
doplot   = strcmp(strmid(cfg(cfg_file,'plot'),0,1),'y',/FOLD_CASE) ; make plots
rnav     = strcmp(strmid(cfg(cfg_file,'read_nav'),0,1),'y',/FOLD_CASE) ;  read nav data
rcg4     = strcmp(strmid(cfg(cfg_file,'read_cg4'),0,1),'y',/FOLD_CASE) ; read cg4 data
np       = fix(cfg(cfg_file,'np'  ))    ; number of channels for each spectrum
darkmin  = fix(cfg(cfg_file,'darkmin')) ; required minimum length of dark cycle
platform = cfg(cfg_file,'platform')
extra    = cfg(cfg_file,'extra') ; extra-terrestial flux
if rnav then print, 'NAV reading online' else print, 'NAV reading offline'
if rcg4 then print, 'CG4 reading online' else print, 'CG4 reading offline'
if doplot then print, 'Plotting online' else print, 'Plotting offline'

; Get interval
i0=cfg(cfg_file,'interval') ; look if we should only use data within a certain time window
uu0=0
if n_elements(i0) gt 1 then begin
    uu=float(strsplit(i0,' ,',escape='#',/EXTRACT))
    uu0=1 & u0=uu[0] & u1=uu[1]
endif else begin
  u0=0 & u1=48. ; two days
endelse

; get cosine correction
wl       = float(strsplit(cfg(cfg_file,'wlql'),' ,',escape='#',/EXTRACT)) ; cosine response
nl       = n_elements(wl)
izamax   = cfg(cfg_file,'izamax')
if strmid(izamax,0,1) eq '#' then izamax=-1 else izamax=float(izamax)
pitchoff = float(cfg(cfg_file,'pitchoff')) ; pitch offset
rolloff  = float(cfg(cfg_file,'rolloff'))  ; roll offset

print, 'getting the response and wavelengths'
get_response, cfg_file, response ; get the response function for 0: zsi  1:zir  2:nsi  3:nir
get_wvl, cfg_file, wvl,zenlambda,nadlambda,indices ; get the wavelengths for 0: zsi  1:zir  2:nsi  3:nir and build the connected wavelength arrays as well as the indices

; set variables
action=''
n_osa2_old=0 & n_cg4_old=0 & n_nav_old=0 & inc_drk=0 & ctr_sav=0
i_last_ssfr=0 & i_last_cg4=0 & i_last_nav=0
new_ssfr=0 & new_cg4=0 & new_nav=0

num=40000 ;big num - must be set smaller for ATTREX, for testing the calnex nav files are all together therefore much bigger
spect_all    =fltarr(num,np,4) ;uncalibrated spectra
spect_cal_all=fltarr(num,np,4) ;calibrated spectra
ssfr_utc     =fltarr(num)*!values.f_nan       ;ssfr utc
ssfr_temp_all=fltarr(num,8)    ;ssfr temperatures
cg4_utc      =fltarr(num)*!values.f_nan       ;cg4 utc time
cg4_I_all    =fltarr(num,2)    ;cg4 irradiance 0: zenith 1:nadir
cg4_temp_all =fltarr(num,2)    ;cg4 temperatures 0: zenith 1: nadir
nav_utc      =fltarr(num)*!values.f_nan       ;nav utc time in hours (24 plus)
lat_all      =fltarr(num)      ;nav latitude of GH
lon_all      =fltarr(num)      ;nav longitude of GH
alt_all      =fltarr(num)      ;nav altitude of GH
sza_all      =fltarr(num)      ;nav Solar Zenith Angle
iza_all      =fltarr(num)      ;nav Instrument Zenith Angle (for zenith light collector)
dc_all       =fltarr(num)      ;nav cosine of the direct sun beam to the plane of the light collector
roll_all     =fltarr(num)      ;nav roll of the Global Hawk (GH)
pitch_all    =fltarr(num)      ;nav pitch of the Global Hawk (GH)
heading_all  =fltarr(num)      ;nav Global Hawk (GH) heading 
pres_all     =fltarr(num)      ;nav ambient pressure at Global Hawk (GH) in mbar
facz_all     =fltarr(num,n_elements(zenlambda)) ;direct cosine correction factor (zenith)
dz_all       =fltarr(num,n_elements(zenlambda)) ;diffuse cosine correction factor (zenith
facn_all     =fltarr(num,n_elements(nadlambda)) ;direct cosine correction factor (nadir)
dn_all       =fltarr(num,n_elements(nadlambda)) ;diffuse cosine correction factor (nadir)
nularr       =fltarr(num)*!values.f_nan      ;nul array (with only NaN)

print, 'Starting waiting loop'
print, '**********************************************************'
print, '** To escape real time processing press "s" at any time **'
print, '** To toggle nav reading press "n" at any time          **'
print, '** To toggle cg4 reading press "c" at any time          **' 
print, '** To toggle plotting press "p" at any time             **' 
print, '**********************************************************'
newline = string(10B)
 
while not strcmp(action,'s') do begin
  ;if not seven then spawn, 'rsync -auzb leblanc@seven.lasp.colorado.edu:'+indir+' '+path+dir   ; change this at dryden
  ; Look for new files (OSA2, CG4, NAV)
  nav_path='/home/radusers/ATTREX/GHflt/nav'
  ssfr=file_search(path+dir,'*.OSA2',count=n_osa2)
  cg4 =file_search(path+dir,'*.CG4' ,count=n_cg4)
  nav =file_search(nav_path,'*.NAV' ,count=n_nav)   ; change this at dryden
  ;print,n_osa2,n_cg4,nav
  
  ssfr=ssfr[sort(ssfr)] & cg4=cg4[sort(cg4)] & nav=nav[sort(nav)]

  ; Ingesting any new files
  if n_osa2 gt n_osa2_old then begin
    for i=n_osa2_old,n_osa2-1 do begin
      info=file_info(ssfr[i])
      if info.size ne 212400 then begin
        inc_drk=0
        print, 'file:'+ssfr[i]+' is corrupted, ignoring'
        continue
      endif
      ingest_ssfr,ssfr[i],date,utc_temp,raw,drk,ch,int,n_drk,err, n_sat, n_ctr
      if n_sat gt 0 then print, newline+'*** warning current file: '+ssfr[i]+' has '+string(n_sat)+' saturated points in the spectra ***'
      ingest_temp,cfg_file,ch,temp  ; procedure to change the voltages to temperatures
      ssfr_utc[i_last_ssfr:i_last_ssfr+n_elements(utc_temp)-1]=utc_temp
      ssfr_temp_all[i_last_ssfr:i_last_ssfr+n_elements(utc_temp)-1,*]=temp
	    ; check if the current dark number is correct
      if n_drk ne 0 then begin ; if there is dark
        if n_drk lt darkmin then begin  ; if it is only a partial dark 
          if inc_drk then begin ;check if it is the second partial dark
            for n=0, np-1 do for j=0,3 do drk[n,j]=mean([replicate(drk[n,j],n_drk),replicate(drk_old[n,j],darkmin-n_drk)],/nan) ; recombine the partial darks by weighted mean
            print, newline+'in second part of incomplete dark'
          endif else print, newline+'incomplete dark'
            inc_drk = not inc_drk ; this sets this flag to true at first occurence of an incomplete dark, then false at the next 
		      
		      ; now make take out the old dark count from the raw spectra and calibrate
		      for ii=0, n_ctr-1 do begin
		        for n=0, np-1 do begin
			        for j=0, 3 do begin
			          spect_all[ii+i_last_ssfr,n,j]=raw[ii,n,j]-drk[n,j]
			      	  spect_cal_all[ii+i_last_ssfr,n,j]=((raw[ii,n,j]-drk[n,j])/int[j])/response[n,j]
			        endfor
			      endfor
		      endfor
	    	endif else begin
		      ; now make take out the new dark count from the raw spectra and calibrate
		      for ii=0, n_ctr-1 do begin
		        for n=0, np-1 do begin
			        for j=0, 3 do begin
			          spect_all[ii+i_last_ssfr,n,j]=raw[ii,n,j]-drk[n,j]
				        spect_cal_all[ii+i_last_ssfr,n,j]=((raw[ii,n,j]-drk[n,j])/int[j])/response[n,j]
			        endfor
			      endfor
		      endfor
		    endelse
	      drk_old=drk
	    endif else begin ; if there is no new darks 
	      for ii=0, n_ctr-1 do begin
		      for n=0, np-1 do begin
		        for j=0, 3 do begin
		          spect_all[ii+i_last_ssfr,n,j]=raw[ii,n,j]-drk_old[n,j]
		          spect_cal_all[ii+i_last_ssfr,n,j]=((raw[ii,n,j]-drk_old[n,j])/int[j])/response[n,j]
		        endfor
		      endfor
		    endfor
	    endelse
	    n_osa2_old=n_osa2
	    i_last_ssfr=i_last_ssfr+n_elements(utc_temp)
    endfor
	  new_ssfr=1
	  ;stop
  endif
  if n_cg4 gt n_cg4_old and rcg4 then begin
    for i=n_cg4_old, n_cg4-1 do begin
      infocg4=file_info(cg4[i])
      if infocg4.size ne 7600 then begin
        print, 'CG4 file:'+cg4[i]+' is corrupted, ignoring'
        continue
      endif
      ingest_cg4, cg4[i], cfg_file, utc_temp, cg4_zen, cg4_nad, cg4_zent, cg4_nadt
      cg4_utc[i_last_cg4:i_last_cg4+n_elements(utc_temp)-1]=utc_temp
      cg4_I_all[i_last_cg4:i_last_cg4+n_elements(utc_temp)-1,0]=cg4_zen
      cg4_I_all[i_last_cg4:i_last_cg4+n_elements(utc_temp)-1,1]=cg4_nad
      cg4_temp_all[i_last_cg4:i_last_cg4+n_elements(utc_temp)-1,0]=cg4_zent
      cg4_temp_all[i_last_cg4:i_last_cg4+n_elements(utc_temp)-1,1]=cg4_nadt
      n_cg4_old=n_cg4
      i_last_cg4=i_last_cg4+n_elements(utc_temp)
    endfor
    new_cg4=1
  endif
  if n_nav gt 0 and rnav then begin
    for i=0, n_nav-1 do begin
      ingest_nav,nav[i],date,pitchoff,rolloff,utc_temp,lat_temp,lon_temp,alt_temp,sza_temp,iza_temp,dc_temp,rol_temp,pit_temp,hed_temp,pre_temp,tem_temp,nav_utc[i_last_nav] 
      
      if finite(utc_temp[0]) then begin 
      get_cos,cfg_file,sza_temp,dc_temp,zenlambda,facz_temp,dz_temp,nadlambda,facn_temp,dn_temp
      nav_utc[i_last_nav:i_last_nav+n_elements(utc_temp)-1]    =utc_temp
      lat_all[i_last_nav:i_last_nav+n_elements(utc_temp)-1]    =lat_temp
      lon_all[i_last_nav:i_last_nav+n_elements(utc_temp)-1]    =lon_temp
      alt_all[i_last_nav:i_last_nav+n_elements(utc_temp)-1]    =alt_temp
      sza_all[i_last_nav:i_last_nav+n_elements(utc_temp)-1]    =sza_temp
      iza_all[i_last_nav:i_last_nav+n_elements(utc_temp)-1]    =iza_temp
      dc_all[i_last_nav:i_last_nav+n_elements(utc_temp)-1]     =dc_temp
      roll_all[i_last_nav:i_last_nav+n_elements(utc_temp)-1]   =rol_temp
      pitch_all[i_last_nav:i_last_nav+n_elements(utc_temp)-1]  =pit_temp
      heading_all[i_last_nav:i_last_nav+n_elements(utc_temp)-1]=hed_temp
      pres_all[i_last_nav:i_last_nav+n_elements(utc_temp)-1]   =pre_temp
      facz_all[i_last_nav:i_last_nav+n_elements(utc_temp)-1,*] =facz_temp
      for n=0,n_elements(zenlambda)-1 do dz_all[i_last_nav:i_last_nav+n_elements(utc_temp)-1,n]   =facz_temp[*,n]*0.0+dz_temp[n]
      facn_all[i_last_nav:i_last_nav+n_elements(utc_temp)-1,*] =facn_temp
      for n=0,n_elements(nadlambda)-1 do dn_all[i_last_nav:i_last_nav+n_elements(utc_temp)-1,n]   =facn_temp[*,n]*0.0+dn_temp[n]
      n_nav_old=n_nav
      i_last_nav=i_last_nav+n_elements(utc_temp)
      endif
    endfor
  endif
  
  if new_ssfr then begin ; enter the plotting and saving routine
    ssfr_last=max(ssfr_utc,/nan,i_ssfr_end)
    cg4_last=max(cg4_utc,/nan,i_cg4_end)
    nav_last=max(nav_utc,/nan,i_nav_end)
    if rnav and rcg4 then utc_end=min([ssfr_last,cg4_last,nav_last],i_end,/nan) ;figure out what is the start time
    if rnav and not rcg4 then utc_end=min([ssfr_last,nav_last],i_end,/nan) ;figure out what is the start time
    if not rnav and rcg4 then utc_end=min([ssfr_last,cg4_last],i_end,/nan) ;figure out what is the start time
    
    ssfr_first=min(ssfr_utc,/nan,i_ssfr_start)
    cg4_first=min(cg4_utc,/nan,i_cg4_start)
    nav_first=min(nav_utc,/nan,i_nav_start)
    if rnav and rcg4 then utc_start=max([ssfr_first,cg4_first,nav_first],i_start,/nan) ;figure out what is the end time
    if rnav and not rcg4 then utc_start=max([ssfr_first,nav_first],i_start,/nan) ;figure out what is the end time
    if not rnav and rcg4 then utc_start=max([ssfr_first,cg4_first],i_start,/nan) ;figure out what is the end time
    
    n0=min(abs(nav_utc-utc_end),/nan,in_e) ;to find the indices of nav, cg4 and ssfr utc times that match up to the start and end times 
    n0=min(abs(cg4_utc-utc_end),/nan,ic_e)
    n0=min(abs(ssfr_utc-utc_end),/nan,is_e)
    
    n0=min(abs(nav_utc-utc_start),/nan,in_s)     
    n0=min(abs(cg4_utc-utc_start),/nan,ic_s)
    n0=min(abs(ssfr_utc-utc_start),/nan,is_s)
    
    if utc_start gt utc_end then begin
      is_s = i_ssfr_start & is_e = i_ssfr_start
      ; no data matches from all systems
    endif
    i_last_ssfr = i_ssfr_end-is_e     ;length of the data to be carried over
    i_all_ssfr  = [is_e,i_ssfr_end]   ;start and stop indices to be carried over to next iteration
    if finite(cg4_utc[i_cg4_end]) then begin ;check for empty arrays
      i_last_cg4 = i_cg4_end-ic_e
      i_all_cg4  = [ic_e,i_cg4_end]
    endif else begin
      i_last_cg4 = 0
      i_all_cg4  = [0,0]
    endelse
    if finite(nav_utc[i_nav_end]) then begin
      i_all_nav  = [in_e,i_nav_end]
      i_last_nav = i_nav_end-in_e
    endif else begin
      i_all_nav  = [0,0]
      i_last_nav = 0
    endelse
  
  ;;; now set up all the variables that will be saved and sent to the plotting/retrieval code
    utc = ssfr_utc[is_s:is_e]  ; this is the ssfr time that will be used to interpol all other values
    if n_elements(utc) eq 1 then utc=[!values.f_nan,!VALUES.f_nan,!VALUES.f_nan]
    
    ;interpol all nav_utc based products to utc
    not_nan=where(finite(nav_utc) eq 1,nnn)
    if nnn eq 0 then goto, contd ;theres no valid nav data
    lat  = interpol(lat_all[not_nan],nav_utc[not_nan],utc)  & lon   = interpol(lon_all[not_nan],nav_utc[not_nan],utc)   & alt      = interpol(alt_all[not_nan],nav_utc[not_nan],utc)
    sza  = interpol(sza_all[not_nan],nav_utc[not_nan],utc)  & iza   = interpol(iza_all[not_nan],nav_utc[not_nan],utc)   & dc       = interpol(dc_all[not_nan],nav_utc[not_nan],utc)
    roll = interpol(roll_all[not_nan],nav_utc[not_nan],utc) & pitch = interpol(pitch_all[not_nan],nav_utc[not_nan],utc) & heading  = interpol(heading_all[not_nan],nav_utc[not_nan],utc)
    pres = interpol(pres_all[not_nan],nav_utc[not_nan],utc)    

    ;interpol all cg4_utc based products to utc     
    nc_nan = where(finite(cg4_utc) eq 1)
    zcg4     =interpol(cg4_I_all[nc_nan,0],cg4_utc[nc_nan],utc)
    ncg4     =interpol(cg4_I_all[nc_nan,1],cg4_utc[nc_nan],utc)
    zcg4_temp=interpol(cg4_temp_all[nc_nan,0],cg4_utc[nc_nan],utc)
    ncg4_temp=interpol(cg4_temp_all[nc_nan,1],cg4_utc[nc_nan],utc)
    
    ;interpol the rest of ssfr_utc based products to utc
    ns_nan = where(finite(ssfr_utc) eq 1)
    spect         =fltarr((n_elements(utc)),np,4)
    spect_cal     =fltarr((n_elements(utc)),np,4)
    zspectra_tm   =[[spect_cal_all[ns_nan,indices[0,0]:indices[1,0],0]],[spect_cal_all[ns_nan,indices[0,1]:indices[1,1],1]]]
    nspectra_tm   =[[spect_cal_all[ns_nan,indices[0,2]:indices[1,2],2]],[spect_cal_all[ns_nan,indices[0,3]:indices[1,3],3]]]
    zspectra      =fltarr(n_elements(utc),n_elements(zenlambda))
    nspectra      =fltarr(n_elements(utc),n_elements(nadlambda))
    ssfr_temp     =fltarr(n_elements(utc),8)
    ;also the nav values
    facz          =fltarr((n_elements(utc)),n_elements(zenlambda)) & dz         =fltarr((n_elements(utc)),n_elements(zenlambda))
    facn          =fltarr((n_elements(utc)),n_elements(nadlambda)) & dn         =fltarr((n_elements(utc)),n_elements(nadlambda))
    for n=0, n_elements(zenlambda)-1 do begin
      facz[*,n] = interpol(facz_all[not_nan,n],nav_utc[not_nan],utc) & dz[*,n] = interpol(dz_all[not_nan,n],nav_utc[not_nan],utc)
    endfor
    for n=0, n_elements(nadlambda)-1 do begin
      facn[*,n] = interpol(facn_all[not_nan,n],nav_utc[not_nan],utc) & dn[*,n] = interpol(dn_all[not_nan,n],nav_utc[not_nan],utc)
    endfor
    for n=0, np-1 do begin
      for j=0,3 do begin
        spect[*,n,j]=interpol(spect_all[ns_nan,n,j],ssfr_utc[ns_nan],utc)  
        spect_cal[*,n,j]=interpol(spect_cal_all[ns_nan,n,j],ssfr_utc[ns_nan],utc)     
      endfor
    endfor       
    
    for n=0, 7 do ssfr_temp[*,n]=interpol(ssfr_temp_all[ns_nan,n],ssfr_utc[ns_nan],utc)
    for n=0, n_elements(zenlambda)-1 do zspectra[*,n]=interpol(zspectra_tm[*,n],ssfr_utc[ns_nan],utc)
    for n=0, n_elements(nadlambda)-1 do nspectra[*,n]=interpol(nspectra_tm[*,n],ssfr_utc[ns_nan],utc)
  
    if is_s eq is_e then begin
      sav=0 
      print, 'No data that matches time with all files'
      print, 'Start times: SSFR:'+string(ssfr_first)+' CG4:'+string(cg4_first)+' NAV:'+string(nav_first)
      print, 'End times:   SSFR:'+string(ssfr_last) +' CG4:'+string(cg4_last) +' NAV:'+string(nav_last)
    endif else sav=1 ;check if there is any data on the new SSFR file that matches the other files if so then save, else don't
       ;stop
    if sav then begin
      if rnav then begin ;if there is nav, then use the cosine correction
        print, 'Cosine correction'
        for n=0, n_elements(zenlambda)-1 do begin
            tau=bodhaine(zenlambda[n],pres,0.)
            fac=facz[*,n]*(exp(tau*cos(sza*!pi/180.)))+dz[*,n]*(1-exp(tau*cos(sza*!pi/180.))) ; use the bodhaine formula here
            zspectra[*,n]=zspectra[*,n]*fac
	      endfor 
	      for n=0, n_elements(nadlambda)-1 do begin
            tau=bodhaine(nadlambda[n],pres,0.)
            fac=facn[*,n]*(exp(tau*cos(sza*!pi/180.)))+dn[*,n]*(1-exp(tau*cos(sza*!pi/180.))) ; use the bodhaine formula here
            nspectra[*,n]=nspectra[*,n]*fac
        endfor
        attcorr=1
      endif else attcorr=0
      if doplot then attrex_rt_plots, wvl, utc, spect, spect_cal, sza, extra, rnav, ssfr_temp, zcg4_temp, ncg4_temp, zcg4, ncg4
      if ctr_sav eq 0 then f=file_search(outdir+date+'_RT_*.out',count=nout) else nout=0
      ov=''
      if nout gt 0 then begin
	f=f[sort(f)]
        print, 'There are some files in the out directory' 
	read, 'Override? (y/n)',ov
	if ov ne 'y' then ctr_sav=fix(strmid(f[nout-1],6,3,/reverse_offset))
      endif
      print, 'Saving to new file: '+outdir+date+'_RT_'+string(ctr_sav,format='(I03)')+'.out'
stop
      save, utc,lat,lon,alt,sza,iza,dc,roll,pitch,heading,facz,dz,facn,dn,zcg4,ncg4,zcg4_temp,n_sat,$
       ncg4_temp,spect,spect_cal,zspectra,nspectra,rnav,rcg4,ssfr_temp,attcorr,filename=outdir+date+'_RT_'+string(ctr_sav,format='(I03)')+'.out'
      spawn, 'chmod a+rw '+outdir+date+'_RT_'+string(ctr_sav,format='(I03)')+'.out'
      if not seven then spawn, 'scp '+path+dir+date+'_RT_'+string(ctr_sav,format='(I03)')+'.out leblanc@seven.lasp.colorado.edu:'+outdir   ; change this at dryden
      ctr_sav=ctr_sav+1
    endif

    contd:
    ; build the arrays with the last data that has not been saved to file and append NaN to the leftover array
    ; Only works if going forward in time, not backwards
    if i_all_cg4[1] eq i_all_cg4[0] then begin ;if theres no cg4 data to carry over 
      cg4_utc = nularr & cg4_I_all[*,0]=nularr & cg4_I_all[*,1]=nularr & cg4_temp_all[*,0]=nularr& cg4_temp_all[*,1]=nularr
    endif else begin
      cg4_utc       = [cg4_utc[i_all_cg4[0]:i_all_cg4[1]],nularr[i_last_cg4+1:*]]
      cg4_I_all[*,0]= [cg4_I_all[i_all_cg4[0]:i_all_cg4[1],0],nularr[i_last_cg4+1:*]]
      cg4_I_all[*,1]= [cg4_I_all[i_all_cg4[0]:i_all_cg4[1],1],nularr[i_last_cg4+1:*]]
      cg4_temp_all[*,0]= [cg4_temp_all[i_all_cg4[0]:i_all_cg4[1],0],nularr[i_last_cg4+1:*]]
      cg4_temp_all[*,1]= [cg4_temp_all[i_all_cg4[0]:i_all_cg4[1],1],nularr[i_last_cg4+1:*]]
    endelse
    if i_all_ssfr[1] eq i_all_ssfr[0] then begin ;if theres no SSFR data to carry over
      ssfr_utc=nularr & for j=0,7 do ssfr_temp_all[*,j]=nularr
      for n=0,np-1 do begin
        for j=0,3 do begin
          spect_all[*,n,j]=nularr & spect_cal_all[*,n,j]=nularr
        endfor
      endfor
    endif else begin
      ssfr_utc      = [ssfr_utc[i_all_ssfr[0]:i_all_ssfr[1]],nularr[i_last_ssfr+1:*]]
      for j=0,7 do ssfr_temp_all[*,j] = [ssfr_temp_all[i_all_ssfr[0]:i_all_ssfr[1],j],nularr[i_last_ssfr+1:*]]
      for n=0,np-1 do begin
        for j=0,3 do begin
          spect_all[*,n,j]=[spect_all[i_all_ssfr[0]:i_all_ssfr[1],n,j],nularr[i_last_ssfr+1:*]]
          spect_cal_all[*,n,j]=[spect_cal_all[i_all_ssfr[0]:i_all_ssfr[1],n,j],nularr[i_last_ssfr+1:*]]
        endfor
      endfor
    endelse
    if i_all_nav[1] eq i_all_nav[0] then begin 
      nav_utc=nularr & lat_all=nularr & lon_all=nularr & alt_all=nularr & sza_all=nularr
      iza_all=nularr & dc_all=nularr & roll_all=nularr & pitch_all=nularr & heading_all=nularr
      for n=0,n_elements(zenlamdba)-1 do begin
        facz_all[*,n] = nularr & dz_all[*,n]   = nularr
      endfor
      for n=0,n_elements(nadlambda)-1 do begin
        facn_all[*,n] = nularr & dn_all[*,n]   = nularr
      endfor
    endif else begin
      nav_utc       = [nav_utc[i_all_nav[0]:i_all_nav[1]],nularr[i_last_nav+1:*]]
      lat_all       = [lat_all[i_all_nav[0]:i_all_nav[1]],nularr[i_last_nav+1:*]]
      lon_all       = [lon_all[i_all_nav[0]:i_all_nav[1]],nularr[i_last_nav+1:*]]
      alt_all       = [alt_all[i_all_nav[0]:i_all_nav[1]],nularr[i_last_nav+1:*]]
      sza_all       = [sza_all[i_all_nav[0]:i_all_nav[1]],nularr[i_last_nav+1:*]]
      iza_all       = [iza_all[i_all_nav[0]:i_all_nav[1]],nularr[i_last_nav+1:*]]
      dc_all        = [dc_all[i_all_nav[0]:i_all_nav[1]],nularr[i_last_nav+1:*]]
      roll_all      = [roll_all[i_all_nav[0]:i_all_nav[1]],nularr[i_last_nav+1:*]]
      pitch_all     = [pitch_all[i_all_nav[0]:i_all_nav[1]],nularr[i_last_nav+1:*]]
      heading_all   = [heading_all[i_all_nav[0]:i_all_nav[1]],nularr[i_last_nav+1:*]]
      for n=0,n_elements(zenlamdba)-1 do begin
        facz_all[*,n] = [facz_all[i_all_nav[0]:i_all_nav[1],n],nularr[i_last_nav+1:*]]
        dz_all[*,n]   = [dz_all[i_all_nav[0]:i_all_nav[1],n],nularr[i_last_nav+1:*]]
      endfor
      for n=0,n_elements(nadlambda)-1 do begin
        facn_all[*,n] = [facn_all[i_all_nav[0]:i_all_nav[1],n],nularr[i_last_nav+1:*]]
        dn_all[*,n]   = [dn_all[i_all_nav[0]:i_all_nav[1],n],nularr[i_last_nav+1:*]]
      endfor
    endelse
  endif
    
  wait,wait
  print, '.', format='(A1,$)' ; progress bar to show that idl is waiting
  new_ssfr=0 & new_cg4=0 & new_nav=0
  action=get_kbrd(0) ; s=stop
                     ; p=toggle plot on/off
                     ; n=toggle nav on/off
                     ; c=toggle cg4 on/off
  if strcmp(action,'p') then begin
    if doplot eq 1 then doplot=0 else doplot=1
    if doplot eq 1 then print,newline+' Real time Plotting on'
    if doplot eq 0 then print,newline+' Real time Plotting off'
  endif
  if strcmp(action,'n') then begin
    if rnav eq 0 then rnav=1 else rnav=0
    if rnav eq 1 then print,newline+' Toggle nav file reader on'
    if rnav eq 0 then print,newline+' Toggle nav file reader off'
  endif
  if strcmp(action,'c') then begin
    if rcg4 eq 0 then rcg4=1 else rcg4=0
    if rcg4 eq 1 then print,newline+' Toggle cg4 file reader on'
    if rcg4 eq 0 then print,newline+' Toggle cg4 file reader off'
  endif
endwhile

print,newline+' End of real-time processing.'

stop
end
