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

pro attrex_rt,date,skip=skip,verbose=verbose

spawn,'hostname',host & pos=strpos(host,'.') & if pos gt 1 then host=strmid(host,0,pos)
if strcmp(host,'Mac',3) then mac=1
if strcmp(host,'van',3) then van=1
if n_elements(date) lt 1 then $
  date   = 20111028
date=strcompress(string(date),/remove_all)
if n_elements(van) eq 1 then begin
  dir      ='/home/radusers/ATTREX/dat/' ; location of osa2, cg4, nav
  outdir   ='/home/radusers/ATTREX/rt/'
endif
if n_elements(mac) eq 1 then begin
  dir      ='/Users/schmidt/data/attrex/dat/'
  outdir   ='/Users/schmidt/data/attrex/test/'
endif
if n_elements(mac) gt 0 or n_elements(van) gt 0 then l='/'
dir=dir+date+l
if n_elements(skip) lt 1 then skip=0

;; settings **************************
wait=1        ; seconds to wait between data ingests
num=60000     ; big number (arrays; could be much smaller, but is currently needed for nav! - This should change!!!!!!!
mn_ingest=skip; ignore first OSA2 files up to mn_ingest-1
mx_ingest=10  ; maximum number of new OSA2 files to ingest at one time - keep this low to not overload the memory 
utclast=99    ; processing will stop at this UTC time
plot_s =2     ; plot spectra during ingest      (0=none, 2=show all
plot_t =2     ; plot time series during ingest   1=show one per mx_ingest OSA2 files, 2=show all)
save_RT=1     ; produce real-time out files (1) or not (0)
retrieval=1   ; do retrieval (1/0)
;online='ssfr@130.134.190.11:/home/ssfr/data/' ; uncomment this line if you don't want to transfer
cfg_file='gh.cfg'
if n_elements(verbose) lt 1 then $
verbose=0     ; 0: quiet, 1: normal messages, 2: all messages
;; end settings **********************
if n_elements(dir) lt 1 then message,'Could not detect computer name.'

;;TBD
; What if
; - we can't listen to nav data on vanzetti (either curl/wget or do that processing on seven/Mac/group-laptop side)
; - we can't transfer from vanzetti to outside world (transfer files via stick 'on the fly')
; - CG-4 calibration / brightness temperatures?
; - vertical profiles, turn on/off?
; - restart attrex_rt multiple times in simple way without messing with _RT_ files
; - all  nav: curl 'http://ghoc.dfrc.nasa.gov/API/parameter_data/N872NA/IWG1?Start=0' > tmp
; - last nav: curl 'http://ghoc.dfrc.nasa.gov/API/parameter_data/N872NA/IWG1' > tmp
; - curl may not exist everywhere; we can also use wget instead
; - automatic recognizing which machine we are on for most of our software
; - print out "12:25 SSFR running. Last OSA2: 12:22; last NAV: 12:24"
; - Do the setup automatically (either "listen" or wget nav data)
; - Retrievals, either in control room or later
; - turn on/off retrievals; turn on/off transfer of spectra (0: none, 1: mean, 2: all)
; - Display temperatures including SSFR, CG4, BT (CG4)
; - Derive Cloud Top Height from Rayleigh Signal and Water Vapor

d24=0.
utcplotmin=0.0
;outdir=dir ;'/home/radusers/ATTREX/IDL/IDL_OUT/' ;path to save the idl out files      ; change this at dryden
l='/'
cfg_file=dir+l+cfg_file

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
if verbose gt 0 then begin
  if rnav then print, 'NAV reading online' else print, 'NAV reading offline'
  if rcg4 then print, 'CG4 reading online' else print, 'CG4 reading offline'
  if doplot then print, 'Plotting online' else print, 'Plotting offline'
endif

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

if verbose gt 0 then print, 'getting the response and wavelengths'
get_response, cfg_file, response ; get the response function for 0: zsi  1:zir  2:nsi  3:nir
get_wvl, cfg_file, wvl,zenlambda,nadlambda,indices ; get the wavelengths for 0: zsi  1:zir  2:nsi  3:nir and build the connected wavelength arrays as well as the indices

; set variables
action=''
drk_old=fltarr(np,4)
n_osa2_old=0 & n_cg4_old=0 & n_nav_old=0 & inc_drk=0 & ctr_sav=0
i_last_ssfr=0 & i_last_cg4=0 & i_last_nav=0
new_ssfr=0 & new_cg4=0 & new_nav=0

;num=40000 ;big num - must be set smaller for ATTREX, for testing the calnex nav files are all together therefore much bigger
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

if verbose gt 0 then begin
print, 'Starting waiting loop'
print, '**********************************************************'
print, '** To escape real time processing press "s" at any time **'
print, '** To toggle nav reading press "n" at any time          **'
print, '** To toggle cg4 reading press "c" at any time          **' 
print, '** To toggle plotting press "p" at any time             **' 
print, '**********************************************************'
endif
newline = string(10B)

while not strcmp(action,'s') do begin
  ;if van then spawn, 'rsync -auzb leblanc@seven.lasp.colorado.edu:'+indir+' '+path+dir   ; change this at dryden
  ; Look for new files (OSA2, CG4, NAV)
  nav_path=dir
  ssfr=file_search(dir,'*.OSA2',count=n_osa2)
  cg4 =file_search(dir,'*.CG4' ,count=n_cg4)
  nav =file_search(nav_path,'*.nav' ,count=n_nav)   ; change this at dryden
  if rnav gt 0 and n_nav lt 1 then begin
    print,'Could not fine nav file.'
    stop
  endif
  ;print,n_osa2,n_cg4,nav

  ssfrsort=sort(ssfr)
  ssfrsort=ssfrsort[mn_ingest:*]  
  ssfr=ssfr[ssfrsort] & cg4=cg4[sort(cg4)] & nav=nav[sort(nav)]
  ; Ingesting any new files
  if n_osa2 gt n_osa2_old then begin
    goal=n_osa2_old+mx_ingest ; Only ingest so many files at a time.
    if goal gt n_osa2-mn_ingest then goal=n_osa2-mn_ingest 
    for i=n_osa2_old,goal-1 do begin ; originally n_osa2-1 <- goal-1
      info=file_info(ssfr[i])
      if info.size ne 212400 then begin
        inc_drk=0
        if verbose gt 1 then print, 'file:'+ssfr[i]+' is corrupted, ignoring'
        continue
      endif
      ingest_ssfr,ssfr[i],date,utc_temp,raw,drk,ch,int,n_drk,err, n_sat, n_ctr, verbose=verbose
      if max(utc_temp) gt utclast then br=1
      
      if n_sat gt 0 then print, newline+'*** warning current file: '+ssfr[i]+' has '+string(n_sat)+' saturated points in the spectra ***'
      ingest_temp,cfg_file,ch,temp  ; procedure to change the voltages to temperatures
      mu=max(utc_temp)
      ssfr_utc[i_last_ssfr:i_last_ssfr+n_elements(utc_temp)-1]=utc_temp;+d24
      ssfr_temp_all[i_last_ssfr:i_last_ssfr+n_elements(utc_temp)-1,*]=temp
	    ; check if the current dark number is correct
      if n_drk ne 0 then begin ; if there is dark
        if n_drk lt darkmin then begin  ; if it is only a partial dark 
          if inc_drk then begin ;check if it is the second partial dark
            for n=0, np-1 do for j=0,3 do drk[n,j]=mean([replicate(drk[n,j],n_drk),replicate(drk_old[n,j],darkmin-n_drk)],/nan) ; recombine the partial darks by weighted mean
            if verbose gt 1 then print, newline+'in second part of incomplete dark'
          endif else if verbose gt 1 then print, newline+'incomplete dark'
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
	    n_osa2_old=goal ;n_osa2
	    i_last_ssfr=i_last_ssfr+n_elements(utc_temp)
    endfor
	  new_ssfr=1
  endif ; end ingesting any new SSFR files.

  if n_cg4 gt n_cg4_old and rcg4 then begin
    utc_temp=0.
    cont=1
    for i=n_cg4_old, n_cg4-1 do begin
      infocg4=file_info(cg4[i])
      if infocg4.size ne 7600 then begin
        if verbose gt 1 then print, 'CG4 file:'+cg4[i]+' is corrupted, ignoring'
        continue
      endif 
      if max(ssfr_utc) gt max(utc_temp) then begin
      if verbose gt 1 then print,'Read '+cg4[i]
      ingest_cg4, cg4[i], cfg_file, utc_temp, cg4_zen, cg4_nad, cg4_zent, cg4_nadt, verbose=verbose ; rewrite
      cg4_utc[i_last_cg4:i_last_cg4+n_elements(utc_temp)-1]=utc_temp
      cg4_I_all[i_last_cg4:i_last_cg4+n_elements(utc_temp)-1,0]=cg4_zen
      cg4_I_all[i_last_cg4:i_last_cg4+n_elements(utc_temp)-1,1]=cg4_nad
      cg4_temp_all[i_last_cg4:i_last_cg4+n_elements(utc_temp)-1,0]=cg4_zent
      cg4_temp_all[i_last_cg4:i_last_cg4+n_elements(utc_temp)-1,1]=cg4_nadt
      n_cg4_old=i+1;n_cg4
      i_last_cg4=i_last_cg4+n_elements(utc_temp)
      endif
    endfor
    new_cg4=1
  endif
  ;stop

  if n_nav gt 0 and rnav then begin
    for i=0, n_nav-1 do begin
      ;print,nav_utc[i_last_nav]
      first_utc_time=nav_utc[i_last_nav] 
      ingest_nav,nav[i],date,pitchoff,rolloff,utc_temp,lat_temp,lon_temp,alt_temp,sza_temp,iza_temp,dc_temp,rol_temp,pit_temp,hed_temp,pre_temp,tem_temp,first_utc_time,verbose=verbose
      if finite(utc_temp[0]) then begin 
      get_cos,cfg_file,sza_temp,dc_temp,zenlambda,facz_temp,dz_temp,nadlambda,facn_temp,dn_temp,verbose=verbose
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
    if not (rnav or rcg4) then utc_end=min(ssfr_last,i_end,/nan) ;figure out what is the start time
    ssfr_first=min(ssfr_utc,/nan,i_ssfr_start)
    cg4_first=min(cg4_utc,/nan,i_cg4_start)
    nav_first=min(nav_utc,/nan,i_nav_start)
    if rnav and rcg4 then utc_start=max([ssfr_first,cg4_first,nav_first],i_start,/nan) ;figure out what is the end time
    if rnav and not rcg4 then utc_start=max([ssfr_first,nav_first],i_start,/nan) ;figure out what is the end time
    if not rnav and rcg4 then utc_start=max([ssfr_first,cg4_first],i_start,/nan) ;figure out what is the end time
    if not (rnav or rcg4) then utc_start=max(ssfr_first,istart,/nan) ;figure out what is the end time
    
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
    if n_elements(utc) gt 0 then utc_old=utc
    utc = ssfr_utc[is_s:is_e]  ; this is the ssfr time that will be used to interpol all other values
    if n_elements(utc) eq 1 then utc=[!values.f_nan,!VALUES.f_nan,!VALUES.f_nan]    ;plot,utc

    
    ;interpol all nav_utc based products to utc
    not_nan=where(finite(nav_utc) eq 1,nnn)
    if nnn eq 0 then goto, contd ;theres no valid nav data
    lat  = interpol(lat_all[not_nan],nav_utc[not_nan],utc)  & lon   = interpol(lon_all[not_nan],nav_utc[not_nan],utc)   & alt      = interpol(alt_all[not_nan],nav_utc[not_nan],utc)
    sza  = interpol(sza_all[not_nan],nav_utc[not_nan],utc)  & iza   = interpol(iza_all[not_nan],nav_utc[not_nan],utc)   & dc       = interpol(dc_all[not_nan],nav_utc[not_nan],utc)
    roll = interpol(roll_all[not_nan],nav_utc[not_nan],utc) & pitch = interpol(pitch_all[not_nan],nav_utc[not_nan],utc) & heading  = interpol(heading_all[not_nan],nav_utc[not_nan],utc)
    pres = interpol(pres_all[not_nan],nav_utc[not_nan],utc)    

    ;interpol all cg4_utc based products to utc     
    nc_nan = where(cg4_utc gt 0.,nnc)
    ;stop
    if nnc gt 0 and rcg4 gt 0 then begin
      zcg4     =interpol(cg4_I_all[nc_nan,0],cg4_utc[nc_nan],utc)
      ncg4     =interpol(cg4_I_all[nc_nan,1],cg4_utc[nc_nan],utc)
      zcg4_temp=interpol(cg4_temp_all[nc_nan,0],cg4_utc[nc_nan],utc)
      ncg4_temp=interpol(cg4_temp_all[nc_nan,1],cg4_utc[nc_nan],utc)
    endif
    
    ;interpol the rest of ssfr_utc based products to utc
    ns_nan = where(finite(ssfr_utc) eq 1,ns_nanx)
    if ns_nanx gt 0 then begin
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
    endif

    if ns_nanx gt 0 then begin    
    for n=0, 7 do ssfr_temp[*,n]=interpol(ssfr_temp_all[ns_nan,n],ssfr_utc[ns_nan],utc)
    for n=0, n_elements(zenlambda)-1 do zspectra[*,n]=interpol(zspectra_tm[*,n],ssfr_utc[ns_nan],utc)
    for n=0, n_elements(nadlambda)-1 do nspectra[*,n]=interpol(nspectra_tm[*,n],ssfr_utc[ns_nan],utc)
    endif  

    if is_s eq is_e then begin
      sav=0 
      if verbose gt 0 then begin
        print, 'No data that matches time with all files'
        print, 'Start times: SSFR:'+string(ssfr_first)+' CG4:'+string(cg4_first)+' NAV:'+string(nav_first)
        print, 'End times:   SSFR:'+string(ssfr_last) +' CG4:'+string(cg4_last) +' NAV:'+string(nav_last)
      endif
    endif else sav=1 ;check if there is any data on the new SSFR file that matches the other files if so then save, else don't
       ;stop
    
    if sav then begin
      if rnav then begin ;if there is nav, then use the cosine correction
        if verbose gt 1 then print, 'Cosine correction'
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

      if doplot then attrex_rt_plots, wvl, utc, spect, spect_cal, sza, extra, rnav, ssfr_temp, zcg4_temp, ncg4_temp, zcg4, ncg4, alt, utcplotmin=utcplotmin,history=history,buffer=buffer,plot_s=plot_s,plot_t=plot_t,retrieval=retrieval,verbose=verbose
      if save_RT then begin
      if ctr_sav eq 0 then f=file_search(outdir+'_RT_*.out',count=nout) else nout=0
      ov=''
      if nout gt 0 then begin
	f=f[sort(f)]
        print, 'There are some files in the out directory' 
	read, 'Override? (y/n)',ov
	if ov ne 'y' then ctr_sav=fix(strmid(f[nout-1],6,3,/reverse_offset))
      endif
      if verbose gt 0 then print, 'Saving to new file: '+outdir+date+'_RT_'+string(ctr_sav,format='(I03)')+'.out'
      ;save, utc,lat,lon,alt,sza,iza,dc,roll,pitch,heading,facz,dz,facn,dn,zcg4,ncg4,zcg4_temp,n_sat,$
      ; ncg4_temp,spect,spect_cal,zspectra,nspectra,rnav,rcg4,ssfr_temp,attcorr,filename=outdir+date+'_RT_'+string(ctr_sav,format='(I03)')+'.out'
      ; The following is only temporary; we don't want to write huge amounts of data, so just choose some channels!
      mm=min(abs(wvl[*,0]-500),m0)
      z500=reform(spect_cal[*,m0,0])
      mm=min(abs(wvl[*,2]-500),m0)
      n500=reform(spect_cal[*,m0,2])
      save,utc,lat,lon,alt,sza,zcg4,ncg4,zcg4_temp,ncg4_temp,ssfr_temp,z500,n500,$
        filename=outdir+date+'_RT_'+string(ctr_sav,format='(I03)')+'.out'
      spawn, 'chmod a+rw '+outdir+date+'_RT_'+string(ctr_sav,format='(I03)')+'.out'
      if n_elements(online) gt 0 then begin
        spawn, 'scp '+outdir+date+'_RT_'+string(ctr_sav,format='(I03)')+'.out '+online+'&',res,err
        if verbose gt 0 then print,'Initiated transfer to NASA server.'
      endif
      ctr_sav=ctr_sav+1
      endif
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
    ;print,'I_ALL_NAV',i_all_nav[0],i_all_nav[1]
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
  endif else begin ; end "new_ssfr" (plotting)
      nav_utc=nularr & lat_all=nularr & lon_all=nularr & alt_all=nularr & sza_all=nularr
      iza_all=nularr & dc_all=nularr & roll_all=nularr & pitch_all=nularr & heading_all=nularr
      for n=0,n_elements(zenlamdba)-1 do begin
        facz_all[*,n] = nularr & dz_all[*,n]   = nularr
      endfor
      for n=0,n_elements(nadlambda)-1 do begin
        facn_all[*,n] = nularr & dn_all[*,n]   = nularr
      endfor
      i_last_nav=0
      ;print,'RESET NAV'
  endelse
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
  if n_elements(br) gt 0 then action='s'
endwhile

print,newline+' End of real-time processing.'

stop
end
