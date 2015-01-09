;+
; NAME:
;   ssfr3_rt
;
; PURPOSE:
;   Near real-time data processing for SSFR3, processes OSA data and then saves
;   it to an idl out file, for further plotting and cloud properties retrieval.
;   Supports incoming files of OSA files
;   Will save to a file only the data at the times from the different inputs coincide   
;   Will only keep in memory the data that has not been saved to a file.
;
; CATEGORY:
;   SSFR3 roof top real-time processing
;
; CALLING SEQUENCE:
;   ssfr3_rt,dir
;   - where dir is the directory within the specified path to watch for incoming files
;   - automatically determines the current day
;
; OUTPUT:
;   IDL sav file, and plots in ps
;
; KEYWORDS:
;   - none
;
; DEPENDENCIES:
;   - ingest_ssfr.pro
;   - ingest_temp.pro
;   - cfg.pro
;   - get_response.pro
;   - get_wvl.pro
;   - get_cos.pro
;   - ssfr3_rt_plots.pro
;   - zensun.pro
;   
; NEEDED FILES:
;   - config file
;   - response functions
;   - temperature coefficients
;   - extraterrestial irradiance values
;   - spectral cosine response functions
;   - incoming OSA files
;  
; EXAMPLE:
;  ssfr3_rt, '/home/leblanc/DC3_SEAC4RS/SSFR3/'
;
;
; MODIFICATION HISTORY:
; Written:  Samuel LeBlanc and Sebastian Schmidt, LASP CU Boulder, September 8th, 2011, Boulder
; Modified: May 10th, 2012, first day of funding for DC3 
;           - by Samuel LeBlanc, modified from the ATTREX_rt to be able to use SSFR3 data
;           - took out the nav and cg4 data reading
;          
;---------------------------------------------------------------------------

@ingest_ssfr.pro
@ingest_temp.pro
@cfg.pro
@get_response.pro
@get_wvl.pro
@get_cos.pro
@ssfr3_rt_plots.pro
@interpol.pro
@zensun.pro
pro ssfr3_rt,dir
seven=1 ;to toggle between using seven or not
if n_elements(dir) lt 1 then dir='/data/seven/DC3/SSFR3/'    ; default path
l='/'
spawn, "date '+%Y%m%d'",date
date=date[0]
print, 'Today date is:'+date
path=dir+date+l
indir='' ;path to the OSA2 files on file server   ; change this at dryden
outdir=path+'out'+l ;path to save the idl out files      ; change this at dryden
wait=5 ; seconds to wait

cfg_file=dir+date+l+date+'.cfg'        ; build cfg file   ; change this at dryden

if file_test(cfg_file) ne 1 then message,'Did not find configuration file.'

; Read from cfg
doplot   = strcmp(strmid(cfg(cfg_file,'plot'),0,1),'y',/FOLD_CASE) ; make plots
np       = fix(cfg(cfg_file,'np'  ))    ; number of channels for each spectrum
darkmin  = fix(cfg(cfg_file,'darkmin')) ; required minimum length of dark cycle
platform = cfg(cfg_file,'platform')
extra    = cfg(cfg_file,'extra') ; extra-terrestial flux
lat      = float(cfg(cfg_file,'lat')) ; get latitude
lon      = float(cfg(cfg_file,'lon')) ; get longitude

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
get_wvl, cfg_file, wvl,zenlambda,nadlambda,indices,/reverse ; get the wavelengths for 0: zsi  1:zir  2:nsi  3:nir and build the connected wavelength arrays as well as the indices
response=response*100.
;response[*,0]=response[*,0]/10. & response[*,1]=response[*,1]/10.

; set variables
action=''
n_osa2_old=0 & n_cg4_old=0 & n_nav_old=0 & inc_drk=0 & ctr_sav=0
i_last_ssfr=0 ;& i_last_cg4=0 & i_last_nav=0
new_ssfr=0 ;& new_cg4=0 & new_nav=0

num=1000 ;big num - max of 1000 points
spect_all    =fltarr(num,np,4) ;uncalibrated spectra
spect_cal_all=fltarr(num,np,4) ;calibrated spectra
ssfr_utc     =fltarr(num)*!values.f_nan       ;ssfr utc
sza_all      =fltarr(num)
ssfr_temp_all=fltarr(num,3)    ;ssfr temperatures
nularr       =fltarr(num)*!values.f_nan      ;nul array (with only NaN)
drk_old      =fltarr(np,4)

print, 'Starting waiting loop'
print, '**********************************************************'
print, '** To escape real time processing press "s" at any time **'
;print, '** To toggle nav reading press "n" at any time          **'
;print, '** To toggle cg4 reading press "c" at any time          **' 
print, '** To toggle plotting press "p" at any time             **' 
print, '**********************************************************'
newline = string(10B)
 
while not strcmp(action,'s') do begin
  ;if not seven then spawn, 'rsync -auzb leblanc@seven.lasp.colorado.edu:'+indir+' '+path+dir   ; change this at dryden
  ; Look for new files (OSA2, CG4, NAV)
  nav_path=path ;'/home/leblanc/DC3_SEAC4RS/SSFR3/';'/home/radusers/ATTREX/GHflt/nav'
  ssfr=file_search(path,'*.OSA',count=n_osa2)
  info=file_info(ssfr)  
  flt=where(info.size eq 210000,n_osa2)
  
  if n_osa2 gt 0 then ssfr=ssfr[sort(info[flt].mtime)] ;& cg4=cg4[sort(cg4)] & nav=nav[sort(nav)]
  if n_osa2 gt n_osa2_old + 5 then n_osa2=n_osa2_old+5 ;limit the amount of files to process at once to only 5
  ; Ingesting any new files
  if n_osa2 gt n_osa2_old then begin
    for i=n_osa2_old,n_osa2-1 do begin
      ingest_ssfr,ssfr[i],date,utc_temp,raw,drk,ch,int,n_drk,err, n_sat, n_ctr,/ssfr3,/drktime
      if n_sat gt 0 then print, newline+'*** warning current file: '+ssfr[i]+' has '+string(n_sat)+' saturated points in the spectra ***'
      ingest_temp,cfg_file,ch,temp,/ssfr3  ; procedure to change the voltages to temperatures
      ssfr_utc[i_last_ssfr:i_last_ssfr+n_elements(utc_temp)-1]=utc_temp
      if n_elements(utc_temp) ne n_elements(temp) then temp=temp[0:n_elements(utc_temp)-1,*]
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
			      	  spect_cal_all[ii+i_last_ssfr,n,j]=((raw[ii,n,j]-drk[n,j]))/response[n,j]
			        endfor
			      endfor
		      endfor
	    	endif else begin
		      ; now make take out the new dark count from the raw spectra and calibrate
		      for ii=0, n_ctr-1 do begin
		        for n=0, np-1 do begin
			        for j=0, 3 do begin
			          spect_all[ii+i_last_ssfr,n,j]=raw[ii,n,j]-drk[n,j]
				        spect_cal_all[ii+i_last_ssfr,n,j]=((raw[ii,n,j]-drk[n,j]))/response[n,j]
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
		          spect_cal_all[ii+i_last_ssfr,n,j]=((raw[ii,n,j]-drk_old[n,j]))/response[n,j]
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

  if new_ssfr then begin ; enter the plotting and saving routine
  
    ;;; now set up all the variables that will be saved and sent to the plotting/retrieval code
    utc = ssfr_utc[0:i_last_ssfr-1]  ; this is the ssfr time that will be used to plot
    spect = spect_all[0:i_last_ssfr-1,*,*]
    spect_cal = spect_cal_all[0:i_last_ssfr-1,*,*]
    ssfr_temp = ssfr_temp_all[0:i_last_ssfr-1,*]
    sza = sza_all[0:i_last_ssfr-1]
    zspectra   =[[spect_cal[*,indices[0,0]:indices[1,0],0]],[spect_cal[*,indices[0,1]:indices[1,1],1]]]
    nspectra   =[[spect_cal[*,indices[0,2]:indices[1,2],2]],[spect_cal[*,indices[0,3]:indices[1,3],3]]]

    ;;;; Portion of the code that returns the sza and cosine factors of the light collectors ;;;;
    juld=julian_day(fix(strmid(date,0,4)),fix(strmid(date,4,2)),fix(strmid(date,6,2)))
    if juld eq 0 or juld gt 366 then message,'Wrong definition of Julian day!'
    zensun,juld,utc,lat,lon,sza,azimuth,solfac
    pit = 0.0 & rol = 0.0 ; set the pitch and roll to zero since the light collectors are not moving
    hed = 0.0
    rph2za,pit+pitchoff,rol+rolloff,hed,ssfrzenith,ssfrazimuth
    dc = muslope(sza,azimuth,ssfrzenith,ssfrazimuth) ; directional cosine^M
    iza=sza*0.0+ssfrzenith

    ; get the cosine corrections
    get_cos,cfg_file,sza,dc,zenlambda,facz,dz,nadlambda,facn,dn
    pres=replicate(830.0,n_elements(utc)) ;must change this to read in the actual pressure, right now using some average boulder pressure

    ;for n=0, n_elements(zenlambda)-1 do begin
    ;  tau=bodhaine(zenlambda[n],pres,0.)
    ;  fac=facz[*,n]*(exp(tau*cos(sza*!pi/180.)))+dz[n]*(1-exp(tau*cos(sza*!pi/180.))) ; use the bodhaine formula here
    ;  zspectra[*,n]=zspectra[*,n]*fac
    ;endfor 
      for n=0, n_elements(nadlambda)-1 do begin
          tau=bodhaine(nadlambda[n],pres,0.)
          fac=facn[*,n]*(exp(tau*cos(sza*!pi/180.)))+dn[n]*(1-exp(tau*cos(sza*!pi/180.))) ; use the bodhaine formula here
          nspectra[*,n]=nspectra[*,n]*fac
      endfor
    attcorr=1
   ; if doplot then ssfr3_rt_plots, wvl, utc, spect, spect_cal, sza, extra, rnav, ssfr_temp
    if ctr_sav eq 0 then f=file_search(outdir+date+'_RT_*.out',count=nout) else nout=0
    ov=''
    if nout gt 0 then begin
      f=f[sort(f)]
      print, 'There are some files in the out directory' 
      ;read, 'Override? (y/n)',ov
      print, 'Overwriting'
      ov='y'
      if ov ne 'y' then ctr_sav=fix(strmid(f[nout-1],6,3,/reverse_offset))
    endif
    print, 'Saving to new file: '+outdir+date+'_RT_'+string(ctr_sav,format='(I03)')+'.out'
       
    ; save the new data to an out file to be processed by ssfr3_online.pro
    save, utc,lat,lon,sza,dc,facz,dz,facn,dn,n_sat,$
     spect,spect_cal,zspectra,nspectra,ssfr_temp,attcorr,iza,filename=outdir+date+'_RT_'+string(ctr_sav,format='(I03)')+'.out'
    spawn, 'chmod a+rw '+outdir+date+'_RT_'+string(ctr_sav,format='(I03)')+'.out'
    ctr_sav=ctr_sav+1
    i_last_ssfr=0
  endif
  ; reset the ssfr files to nans now
  ssfr_utc      = nularr
  for j=0,2 do ssfr_temp_all[*,j] = nularr
  for n=0,np-1 do begin
    for j=0,3 do begin
      spect_all[*,n,j]=nularr
      spect_cal_all[*,n,j]=nularr
    endfor
  endfor
;endif
    
  wait,wait
  print, '.', format='(A1,$)' ; progress bar to show that idl is waiting
  spawn, "date '+%H'", time
  time=float(time[0])
  new_ssfr=0
  ;action=get_kbrd(0) ; s=stop
                     ; p=toggle plot on/off
                     ; n=toggle nav on/off
                     ; c=toggle cg4 on/off
  action=''
  if time gt 22.0 then action='s'
  if strcmp(action,'p') then begin
    if doplot eq 1 then doplot=0 else doplot=1
    if doplot eq 1 then print,newline+' Real time Plotting on'
    if doplot eq 0 then print,newline+' Real time Plotting off'
  endif
endwhile

print,newline+' End of real-time processing.'

stop
end
