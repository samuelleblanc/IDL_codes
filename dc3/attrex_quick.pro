;
; PURPOSE:
;   Quick and dirty way to get real time SSFR and CG4 data, without Nav data
;   Directories must be hard wired
;
; CATEGORY:
;   ATTREX real-time processing
;
; CALLING SEQUENCE:
;   attrex_quick
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
;   - ingest_cg4.pro
;   - cfg.pro
;   - get_response.pro
;   - get_wvl.pro
;   - attrex_rt_plots.pro
;   
; NEEDED FILES:
;   - config file
;   - response functions
;   - temperature coefficients
;   - extraterrestial irradiance values
;   - incoming OSA2 files
;   - incoming CG4 files
;  
; EXAMPLE:
;  attrex_quick
;
;
; MODIFICATION HISTORY:
; Written:  Samuel LeBlanc and Sebastian Schmidt, LASP CU Boulder, October 18th, 2011, Boulder
; Modified: 
;          
;---------------------------------------------------------------------------

@ingest_ssfr.pro
@ingest_temp.pro
@ingest_cg4.pro
@cfg.pro
@get_response.pro
@get_wvl.pro
@attrex_rt_plots.pro

pro attrex_quick

cfg_dir='/data/seven/schmidt/attrex/gh/test/'
osa_dir='/data/seven/schmidt/attrex/gh/test/'
cg4_dir='/data/seven/schmidt/attrex/gh/test/'
out_dir='/data/seven/schmidt/attrex/gh/test/'

cfg_dir='/home/leblanc/ATTREX/gh/TEMP/'
osa_dir='/home/leblanc/ATTREX/gh/TEMP/'
cg4_dir='/home/leblanc/ATTREX/gh/TEMP/'
out_dir='/home/leblanc/ATTREX/gh/TEMP/'

l='/'

cfg_file=cfg_dir+'gh.cfg'        ; build cfg file   ; change this at dryden
if file_test(cfg_file) ne 1 then message,'Did not find configuration file.'

; Read from cfg
date     = cfg(cfg_file,'date') ;read the date
doplot   = strcmp(strmid(cfg(cfg_file,'plot'),0,1),'y',/FOLD_CASE) ; make plots
rcg4     = strcmp(strmid(cfg(cfg_file,'read_cg4'),0,1),'y',/FOLD_CASE) ; read cg4 data
np       = fix(cfg(cfg_file,'np'  ))    ; number of channels for each spectrum
darkmin  = fix(cfg(cfg_file,'darkmin')) ; required minimum length of dark cycle
platform = cfg(cfg_file,'platform')
extra    = cfg(cfg_file,'extra') ; extra-terrestial flux
rnav=0

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

print, 'getting the response and wavelengths'
get_response, cfg_file, response ; get the response function for 0: zsi  1:zir  2:nsi  3:nir
get_wvl, cfg_file, wvl,zenlambda,nadlambda,indices ; get the wavelengths for 0: zsi  1:zir  2:nsi  3:nir and build the connected wavelength arrays as well as the indices


num=60000 ;big num - must be set smaller for ATTREX, for testing the calnex nav files are all together therefore much bigger
spect_all    =fltarr(num,np,4) ;uncalibrated spectra
spect_cal_all=fltarr(num,np,4) ;calibrated spectra
ssfr_utc     =fltarr(num)*!values.f_nan       ;ssfr utc
ssfr_temp_all=fltarr(num,8)    ;ssfr temperatures
cg4_utc      =fltarr(num)*!values.f_nan       ;cg4 utc time
cg4_I_all    =fltarr(num,2)    ;cg4 irradiance 0: zenith 1:nadir
cg4_temp_all =fltarr(num,2)    ;cg4 temperatures 0: zenith 1: nadir
i_last_ssfr=0 & i_last_cg4=0 & inc_drk=0
newline=string(10b)

ssfr=file_search(osa_dir,'*.OSA2',count=n_osa2)
cg4 =file_search(cg4_dir,'*.CG4' ,count=n_cg4)
ssfr=ssfr[sort(ssfr)] & cg4=cg4[sort(cg4)]

; now run through the osa2 files
for i=0,n_osa2-1 do begin
  info=file_info(ssfr[i])
  if i ne n_osa2-1 and info.size ne 212400 then begin
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
  i_last_ssfr=i_last_ssfr+n_elements(utc_temp)
endfor

for i=0, n_cg4-1 do begin
  ingest_cg4, cg4[i], cfg_file, utc_temp, cg4_zen, cg4_nad, cg4_zent, cg4_nadt
  cg4_utc[i_last_cg4:i_last_cg4+n_elements(utc_temp)-1]=utc_temp
  cg4_I_all[i_last_cg4:i_last_cg4+n_elements(utc_temp)-1,0]=cg4_zen
  cg4_I_all[i_last_cg4:i_last_cg4+n_elements(utc_temp)-1,1]=cg4_nad
  cg4_temp_all[i_last_cg4:i_last_cg4+n_elements(utc_temp)-1,0]=cg4_zent
  cg4_temp_all[i_last_cg4:i_last_cg4+n_elements(utc_temp)-1,1]=cg4_nadt
  i_last_cg4=i_last_cg4+n_elements(utc_temp)
endfor

utc = ssfr_utc
zspectra      =[[spect_cal_all[*,indices[0,0]:indices[1,0],0]],[spect_cal_all[*,indices[0,1]:indices[1,1],1]]]
nspectra      =[[spect_cal_all[*,indices[0,2]:indices[1,2],2]],[spect_cal_all[*,indices[0,3]:indices[1,3],3]]]

zcg4_temp = cg4_temp_all[*,0]
ncg4_temp = cg4_temp_all[*,1]
zcg4      = cg4_I_all[*,0]
ncg4      = cg4_I_all[*,1]

;now plot
attrex_rt_plots, wvl, utc, spect_all, spect_cal_all, utc, extra, rnav, ssfr_temp_all, zcg4_temp, ncg4_temp, zcg4, ncg4

; now save
print, 'Saving to new file: '+outdir+date+'_quick.out'
save, utc,zcg4,ncg4,zcg4_temp,n_sat,$
 ncg4_temp,spect_all,spect_cal_all,zspectra,nspectra,ssfr_temp_all,filename=outdir+date+'_quick.out'

stop
end
