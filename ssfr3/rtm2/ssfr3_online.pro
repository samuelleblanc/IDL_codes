;+
; NAME:
;   ssfr3_online
;
; PURPOSE:
;   to present get the new data file that is incoming at every 3 minutes or so. Then to send it to the retrieval and produce all the required plots
;
; CATEGORY:
;   SSFR3 online processing
;
; CALLING SEQUENCE:
;   ssfr3_online,in
;   - where in is the last file index
;
; OUTPUT:
;   plots
;   values in a save file
;
; KEYWORDS:
;   - none
;
; DEPENDENCIES:
;   - retrieval code: ssfr3_retrieval.pro
;   - plotting code: ssfr3_plots.pro
;   - cfg.pro
;   - get_response.pro
;   - get_wvl.pro
;   
; NEEDED FILES:
;   - ssfr3_plots.pro
;   - ssfr3_retrieval.pro
;   - ssfr3 save file
;
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
; Written:  Samuel LeBlanc, LASP CU Boulder, September 8th, 2011, Boulder
; Modified: by Samuel LeBlanc, May 18th, 2012, Boulder Colorado
;           - changed this version to plot SSFR3 data instead
;          
;---------------------------------------------------------------------------
@ssfr3_retrieval.pro
@ssfr3_plots.pro
@cfg.pro
@get_response.pro
@get_wvl.pro

pro ssfr3_online, in
seven=1
spawn, "date '+%Y%m%d'",date
date=date[0]
if seven then l='/' else l='\'
indir     = '/data/seven/DC3/SSFR3/'+date+'/out/'  ;input directory seven 
cfg_dir   = '/data/seven/DC3/SSFR3/'+date+l       ;seven                 
dirout    = '/data/seven/DC3/SSFR3/'+date+'/out/'  ;output directory seven    

cfg_file=cfg_dir+date+'.cfg'        ; build cfg file
if file_test(cfg_file) ne 1 then message,'Did not find configuration file.'

print, 'getting the response and wavelengths'
get_response, cfg_file, response ; get the response function for 0: zsi  1:zir  2:nsi  3:nir
get_wvl, cfg_file, wvl,zenlambda,nadlambda_tm,indices,/reverse ; get the wavelengths for 0: zsi  1:zir  2:nsi  3:nir and build the connected wavelength arrays as well as the indices

; Read from cfg
date     = cfg(cfg_file,'date') ;read the date
np       = fix(cfg(cfg_file,'np'  ))    ; number of channels for each spectrum
retrieval= strcmp(strmid(cfg(cfg_file,'retrieval'),0,1),'y',/FOLD_CASE)  ;check if the retrieval must be run
rt_wl    = [870.,1600.]
plot_wl  = [350.,500.,800.,1300.,1600.]
ts=1 & sp=1 & rt=1
if ts then print, 'will plot the time series'
if sp then print, 'will plot the spectra'
if rt then print, 'will plot the results from the retrieval'


print, 'initialising arrays'
big_num=15000L ; large number  ;about 4 hours+ of data
i=0L ;index at where we are at
i_last=0L ;last index
i_save_ct=0 ;last file save count
spect_big     = replicate(!values.f_nan,big_num,np,4)
spect_cal_big = replicate(!values.f_nan,big_num,np,4)
ssfr_temp_big = replicate(!values.f_nan,big_num,3)
nad_spect     = replicate(!values.f_nan,big_num,n_elements(zenlambda)) ;for storing interpolated values
zen_spect     = replicate(!values.f_nan,big_num,n_elements(zenlambda))
tmhrs_big     = replicate(!values.f_nan,big_num)
sat_big       = replicate(!values.f_nan,big_num)
sza_big       = replicate(!values.f_nan,big_num)
iza_big       = replicate(!values.f_nan,big_num)
dc_big        = replicate(!values.f_nan,big_num)
facz_big      = replicate(!values.f_nan,big_num,n_elements(zenlambda))
facn_big      = replicate(!values.f_nan,big_num,n_elements(zenlambda))
dn            = replicate(!values.f_nan,n_elements(zenlambda))
dz            = replicate(!values.f_nan,n_elements(zenlambda))
cod_ice_big   = replicate(!values.f_nan,big_num)
ref_ice_big   = replicate(!values.f_nan,big_num)
cod_liquid_big= replicate(!values.f_nan,big_num)
ref_liquid_big= replicate(!values.f_nan,big_num)
residual_ice_big    = replicate(!values.f_nan,big_num)
residual_liquid_big = replicate(!values.f_nan,big_num)
nuc = 3600
nul = replicate(!values.f_nan,nuc)

if n_elements(in) lt 1 then last_file_index=0 else last_file_index=in
;start loop that waits for a file
print, 'starting loop - press "s" at any time to stop loop'
print, '              - press "p" at any time to access the plotting options'
loop:
not_found=1
action=''
ans=''
while not_found do begin
  f=file_search(indir+'*_RT_'+string(last_file_index,format='(I03)')+'.out',count=n)
  if n gt 0 then not_found=0 else begin
    back:
    wait,10
    not_found=1
    print, '.', format='(A1,$)' ; progress bar to show that idl is waiting
    spawn, "date '+%H'", time
    time=float(time[0])
  endelse
  ;action=get_kbrd(0) ; s=stop
  action=''
  spawn, "date '+%H'", time
  time=float(time[0])
  if time gt 22.0 then action='s'
  if strcmp(action,'s') then goto, ends
  if strcmp(action,'p') then begin
    read, 'Do you want to plot the time series? (y/n)',ans
    if ans eq 'y' then ts=1 else ts=0
    if ts then print, 'plotting time series' else print, 'not plotting time series'
    read, 'Do you want to plot the spectra? (y/n)',ans
    if ans eq 'y' then sp=1 else sp=0
    if sp then print, 'plotting spectra' else print, 'not plotting spectra'
    read, 'Do you want to plot the retrieved properties? (y/n)',ans
    if ans eq 'y' then rt=1 else rt=0
    if rt then print, 'plotting retrieved properties' else print, 'not plotting retrieved properties'
  endif
endwhile
last_file_index=last_file_index+1
print, ' '
print, 'restoring file:' +f
on_ioerror, back
restore, f

;;;;;;;;;;;;;;;;;;;;;; now start processing file ;;;;;;;;;;;;;;;;;;;;;;;;;;;
i_last=n_elements(utc)-1L + i
tmhrs_big[i:i_last] = utc    
sat_big[i:i_last]   = n_sat  & sza_big[i:i_last]   = sza      & dc_big[i:i_last]       = dc   
iza_big[i:i_last]   = iza    

for n=0,2 do ssfr_temp_big[i:i_last,n]=ssfr_temp[*,n]
for n=0,np-1 do begin
  for l=0,3 do begin
    spect_big[i:i_last,n,l]     = spect[*,n,l]
    spect_cal_big[i:i_last,n,l] = spect_cal[*,n,l]
  endfor
endfor  
;interpolate all new spectra to zenlambda
nspectra_tm=zspectra
facn_tm=facz
dn_tm=dz
for j=0,n_elements(utc)-1 do begin
  nspectra_tm[j,*]=interpol(nspectra[j,*],nadlambda_tm,zenlambda)
  facn_tm[j,*]=interpol(facn[j,*],nadlambda_tm,zenlambda)
endfor
dn_tm=interpol(dn,nadlambda_tm,zenlambda)
dn=dn_tm
dz=dz
nadlambda=zenlambda

for n=0, n_elements(zenlambda)-1 do begin
  nad_spect[i:i_last,n] = nspectra_tm[*,n] & zen_spect[i:i_last,n] = zspectra[*,n]
  facz_big[i:i_last,n]  = facz[*,n]        & facn_big[i:i_last,n]  = facn_tm[*,n] 
endfor

;call retrieval module requires albedo for only 3 wavelengths,and mu - outputs aod and ref
if retrieval then begin
  albedo=fltarr(n_elements(rt_wl),n_elements(utc))
  for ii=0,n_elements(rt_wl)-1 do begin
    nuls=min(abs(zenlambda-rt_wl[ii]),i_z)
    nuls=min(abs(nadlambda-rt_wl[ii]),i_n)
    albedo[ii,*]=nspectra[*,i_n]/zspectra[*,i_z]  ; interpolate to common wavelength
  endfor
  
  ssfr3_retrieval,cfg_file,sza,albedo,rt_wl,cod_ice,ref_ice,cod_liquid,ref_liquid,residual_ice,residual_liquid
  cod_ice_big[i:i_last]=cod_ice
  ref_ice_big[i:i_last]=ref_ice
  cod_liquid_big[i:i_last]=cod_liquid
  ref_liquid_big[i:i_last]=ref_liquid
  residual_ice_big[i:i_last]=residual_ice
  residual_liquid_big[i:i_last]=residual_liquid
  mu=mean(cos(sza*!dtor))
endif else begin
  mu=mean(cos(sza*!dtor))
  rt =0
endelse

;call plotting module
ssfr3_plots, tmhrs_big,nad_spect,zen_spect,nadlambda,zenlambda,i,i_last,cod_ice_big,ref_ice_big,$
  cod_liquid_big,ref_liquid_big,residual_ice_big,residual_liquid_big,dirout,date,plot_wl,attcorr,mu,ts,sp,rt

;save current data to a file
if i_last gt 10800 then begin ;save file every two hour of data 
  print, 'Saving file to: '+dirout+date+'_TS_'+string(i_save_ct,format='(I03)')+'.out'
  save, tmhrs_big,nad_spect,zen_spect,nadlambda,zenlambda,cod_ice_big,ref_ice_big,$
    cod_liquid_big,ref_liquid_big,residual_ice_big,residual_liquid_big,spect_big,spect_cal_big,$
    ssfr_temp_big,sat_big,sza_big,iza_big,dc_big,$
    facz_big,facn_big,dn,dz,    filename=dirout+date+'_TS_'+string(i_save_ct,format='(I03)')+'.out'
  i_save_ct=i_save_ct+1 ; add to one count the save file

  ; reset the arrays to ignore the first hours of the array
  tmhrs_big=[tmhrs_big[nuc:*],nul]
  cod_ice_big=[cod_ice_big[nuc:*],nul] & ref_ice_big=[ref_ice_big[nuc:*],nul]
  cod_liquid_big=[cod_liquid_big[nuc:*],nul] & ref_liquid_big=[ref_liquid_big[nuc:*],nul] & residual_ice_big=[residual_ice_big[nuc:*],nul]
  residual_liquid_big=[residual_liquid_big[nuc:*],nul]
  sat_big=[sat_big[nuc:*],nul] & sza_big=[sza_big[nuc:*],nul] & iza_big=[iza_big[nuc:*],nul] & dc_big=[dc_big[nuc:*],nul]
  for n=0,2 do ssfr_temp_big[*,n]=[ssfr_temp_big[nuc:*,n],nul]
  for n=0,np-1 do begin
    for l=0,3 do begin
      spect_big[*,n,l]     = [spect_big[nuc:*,n,l],nul]
      spect_cal_big[*,n,l] = [spect_cal_big[nuc:*,n,l],nul]
    endfor
  endfor  
  for n=0, n_elements(zenlambda)-1 do begin
    nad_spect[*,n] = [nad_spect[nuc:*,n],nul] & zen_spect[*,n] = [zen_spect[nuc:*,n],nul]
    facz_big[*,n]  = [facz_big[nuc:*,n],nul]  & facn_big[*,n]  = [facn_big[nuc:*,n],nul]
  endfor
  i_last=i_last-3600
endif
i=i_last
goto,loop
ends:
print, 'the last file index is:',last_file_index
stop
end
