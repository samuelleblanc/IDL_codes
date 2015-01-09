;+
; NAME:
;   attrex_online
;
; PURPOSE:
;   to present get the new data file that is incoming at every 3 minutes or so during the GH flight. Then to send it to the retrieval and produce all the required plots
;
; CATEGORY:
;   ATTREX online processing
;
; CALLING SEQUENCE:
;   attrex_online,in
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
;   - retrieval code: attrex_retrieval.pro
;   - plotting code: attrex_plots.pro
;   - cfg.pro
;   - get_response.pro
;   - get_wvl.pro
;   
; NEEDED FILES:
;   - attrex_plots.pro
;   - attrex_retrieval.pro
;   - attrex save file
;
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
; Written:  Samuel LeBlanc, LASP CU Boulder, September 8th, 2011, Boulder
; Modified: 
;          
;---------------------------------------------------------------------------
@attrex_retrieval.pro
@attrex_plots.pro
@cfg.pro
@get_response.pro
@get_wvl.pro

pro attrex_online, in
seven=0
flight = '00'
if seven then begin
remotedir = '/data/seven/schmidt/attrex/gh/test/out/' ;directory on the remote server
indir     = '/data/seven/schmidt/attrex/gh/test/out/'  ;input directory seven 
cfg_dir   = '/data/seven/schmidt/attrex/gh/test/'       ;seven                 
dirout    = '/data/seven/schmidt/attrex/gh/test/'  ;output directory seven    

endif else begin ;this is when its running on group laptop
remotedir = '/data/seven/schmidt/attrex/gh/test/out/'  ;directory on the remote server  ; change this at dryden
rmdir_cfg = '/data/seven/schmidt/attrex/gh/test/'
indir     = '/localhome/kindel/ATTREX/ONLINE/'  ; change this at dryden
cfg_dir   = '/localhome/kindel/ATTREX/ONLINE/'  ; change this at dryden
dirout    = '/localhome/kindel/ATTREX/ONLINE/'  ; change this at dryden

endelse
;spawn, 'rsync -auzb kindel@seven.lasp.colorado.edu:'+rmdir_cfg+' '+cfg_dir  ; change this at dryden
cfg_file=cfg_dir+'gh.cfg'        ; build cfg file
if file_test(cfg_file) ne 1 then message,'Did not find configuration file.'

print, 'getting the response and wavelengths'
get_response, cfg_file, response ; get the response function for 0: zsi  1:zir  2:nsi  3:nir
get_wvl, cfg_file, wvl,zenlambda,nadlambda_tm,indices ; get the wavelengths for 0: zsi  1:zir  2:nsi  3:nir and build the connected wavelength arrays as well as the indices

; Read from cfg
date    = cfg(cfg_file,'date') ;read the date
np      = fix(cfg(cfg_file,'np'  ))    ; number of channels for each spectrum
rt_wl   = [870.,1600.]
plot_wl = [350.,500.,870.,1600.]
ts=1 & sp=1 & pr=1 & rt=1
if ts then print, 'will plot the time series'
if sp then print, 'will plot the spectra'
if pr then print, 'will plot the profiles of net irradiance;'
if rt then print, 'will plot the results from the retrieval'


print, 'initialising arrays'
big_num=12000L ; large number  ;about 4 hours+ of data
i=0L ;index at where we are at
i_last=0L ;last index
i_save_ct=0 ;last file save count
spect_big     = replicate(!values.f_nan,big_num,np,4)
spect_cal_big = replicate(!values.f_nan,big_num,np,4)
ssfr_temp_big = replicate(!values.f_nan,big_num,8)
nad_spect     = replicate(!values.f_nan,big_num,n_elements(zenlambda)) ;for storing interpolated values
zen_spect     = replicate(!values.f_nan,big_num,n_elements(zenlambda))
lat_big       = replicate(!values.f_nan,big_num)
lon_big       = replicate(!values.f_nan,big_num)
tmhrs_big     = replicate(!values.f_nan,big_num)
alt_big       = replicate(!values.f_nan,big_num)
ncg4_big      = replicate(!values.f_nan,big_num)
zcg4_big      = replicate(!values.f_nan,big_num)
ncg4_temp_big = replicate(!values.f_nan,big_num)
zcg4_temp_big = replicate(!values.f_nan,big_num)
sat_big       = replicate(!values.f_nan,big_num)
sza_big       = replicate(!values.f_nan,big_num)
iza_big       = replicate(!values.f_nan,big_num)
dc_big        = replicate(!values.f_nan,big_num)
roll_big      = replicate(!values.f_nan,big_num)
pitch_big     = replicate(!values.f_nan,big_num)
heading_big   = replicate(!values.f_nan,big_num)
facz_big      = replicate(!values.f_nan,big_num,np)
facn_big      = replicate(!values.f_nan,big_num,np)
dn_big        = replicate(!values.f_nan,big_num,np)
dz_big        = replicate(!values.f_nan,big_num,np)
cod_ice_big   = replicate(!values.f_nan,big_num)
ref_ice_big   = replicate(!values.f_nan,big_num)
cod_liquid_big= replicate(!values.f_nan,big_num)
ref_liquid_big= replicate(!values.f_nan,big_num)
residual_ice_big    = replicate(!values.f_nan,big_num)
residual_liquid_big = replicate(!values.f_nan,big_num)
nul = replicate(!values.f_nan,3600)

if n_elements(in) lt 1 then last_file_index=0 else last_file_index=in
;start loop that waits for a file
print, 'starting loop - press "s" at any time to stop loop'
print, '              - press "p" at any time to access the plotting options'
loop:
not_found=1
action=''
ans=''
while not_found do begin
  ;spawn, 'rsync -auzb kindel@seven.lasp.colorado.edu:'+remotedir+' '+cfg_dir	  ; change this at dryden
  f=file_search(indir+'*_RT_'+string(last_file_index,format='(I03)')+'.out',count=n)
  if n gt 0 then not_found=0 else begin
    wait,10
    not_found=1
    print, '.', format='(A1,$)' ; progress bar to show that idl is waiting
  endelse
  action=get_kbrd(0) ; s=stop
  if strcmp(action,'s') then goto, ends
  if strcmp(action,'p') then begin
    read, 'Do you want to plot the time series? (y/n)',ans
    if ans eq 'y' then ts=1 else ts=0
    if ts then print, 'plotting time series' else print, 'not plotting time series'
    read, 'Do you want to plot the spectra? (y/n)',ans
    if ans eq 'y' then sp=1 else sp=0
    if sp then print, 'plotting spectra' else print, 'not plotting spectra'
    read, 'Do you want to plot the profile? (y/n)',ans
    if ans eq 'y' then pr=1 else pr=0
    if pr then print, 'plotting profiles' else print, ' not plotting profiles
    read, 'Do you want to plot the retrieved properties? (y/n)',ans
    if ans eq 'y' then rt=1 else rt=0
    if rt then print, 'plotting retrieved properties' else print, 'not plotting retrieved properties'
  endif
endwhile
last_file_index=last_file_index+1
print, ' '
print, 'restoring file:' +f
restore, f

;;;;;;;;;;;;;;;;;;;;;; now start processing file ;;;;;;;;;;;;;;;;;;;;;;;;;;;
i_last=n_elements(utc)-1L + i
lat_big[i:i_last]   = lat    & lon_big[i:i_last]   = lon
tmhrs_big[i:i_last] = utc    & alt_big[i:i_last]   = alt      & ncg4_big[i:i_last]     = ncg4      & zcg4_big[i:i_last]= zcg4
sat_big[i:i_last]   = n_sat  & sza_big[i:i_last]   = sza      & dc_big[i:i_last]       = dc        & roll_big[i:i_last]= roll
iza_big[i:i_last]   = iza    & pitch_big[i:i_last] = pitch    & heading_big[i:i_last]  = heading
zcg4_temp_big[i:i_last]= zcg4_temp & ncg4_temp_big[i:i_last] = ncg4_temp  
for n=0,7 do ssfr_temp_big[i:i_last,n]=ssfr_temp[*,n]
for n=0,np-1 do begin
  facz_big[i:i_last,n] = facz[*,n] & facn_big[i:i_last,n] = facn[*,n] & dn_big[i:i_last,n] = dn[*,n] & dz_big[i:i_last,n] = dz[*,n]
  for l=0,3 do begin
    spect_big[i:i_last,n,l]     = spect[*,n,l]
    spect_cal_big[i:i_last,n,l] = spect_cal[*,n,l]
  endfor
endfor  
;interpolate all new spectra to zenlambda
nspectra_tm=zspectra
for j=0,n_elements(utc)-1 do nspectra_tm[j,*]=interpol(nspectra[j,*],nadlambda_tm,zenlambda)
nadlambda=zenlambda

for n=0, n_elements(nadlambda)-1 do nad_spect[i:i_last,n] = nspectra_tm[*,n]
for n=0, n_elements(zenlambda)-1 do zen_spect[i:i_last,n] = zspectra[*,n]

;call retrieval module requires albedo for only 3 wavelengths,and mu - outputs aod and ref
albedo=fltarr(n_elements(rt_wl),n_elements(utc))
for ii=0,n_elements(rt_wl)-1 do begin
  nul=min(abs(zenlambda-rt_wl[ii]),i_z)
  nul=min(abs(nadlambda-rt_wl[ii]),i_n)
  albedo[ii,*]=nspectra[*,i_n]/zspectra[*,i_z]  ; interpolate to common wavelength
endfor

;rnav=0
if rnav then begin
  attrex_retrieval,sza,albedo,rt_wl,cod_ice,ref_ice,cod_liquid,ref_liquid,residual_ice,residual_liquid
  cod_ice_big[i:i_last]=cod_ice
  ref_ice_big[i:i_last]=ref_ice
  cod_liquid_big[i:i_last]=cod_liquid
  ref_liquid_big[i:i_last]=ref_liquid
  residual_ice_big[i:i_last]=residual_ice
  residual_liquid_big[i:i_last]=residual_liquid
  mu=mean(cos(sza*!dtor))
endif else mu=!values.f_nan

;call plotting module
attrex_plots, tmhrs_big,lat_big,lon_big,nad_spect,zen_spect,nadlambda,zenlambda,alt_big,ncg4_big,zcg4_big,i,i_last,cod_ice_big,ref_ice_big,$
  cod_liquid_big,ref_liquid_big,residual_ice_big,residual_liquid_big,dirout,flight,plot_wl,attcorr,mu,ts,sp,pr,rt

;save current data to a file
if i_last gt 10800 then begin ;save file every hour of data 
  print, 'Saving file to: '+dirout+flight+'_TS_'+string(i_save_ct,format='(I03)')+'.out'
  save, tmhrs_big,lat_big,lon_big,nad_spect,zen_spect,nadlambda,zenlambda,alt_big,ncg4_big,zcg4_big,cod_ice_big,ref_ice_big,$
    cod_liquid_big,ref_liquid_big,residual_ice_big,residual_liquid_big,spect_big,spect_cal_big,$
    ssfr_temp_big,ncg4_temp_big,zcg4_temp_big,sat_big,sza_big,iza_big,dc_big,roll_big,pitch_big,heading_big,$
    facz_big,facn_big,dn_big,dz_big,    filename=dirout+flight+'_TS_'+string(i_save_ct,format='(I03)')+'.out'
  i_save_ct=i_save_ct+1 ; add to one count the save file

  ; reset the arrays to ignore the first hours of the array
  tmhrs_big=[tmhrs_big[3600:*],nul] & lat_big=[lat_big[3600:*],nul] & lon_big=[lon_big[3600:*],nul] & alt_big=[alt_big[3600:*],nul]
  ncg4_big=[ncg4_big[3600:*],nul] & zcg4_big=[zcg4_big[3600:*],nul] & cod_ice_big=[cod_ice_big[3600:*],nul] & ref_ice_big=[ref_ice_big[3600:*],nul]
  cod_liquid_big=[cod_liquid_big[3600:*],nul] & ref_liquid_big=[ref_liquid_big[3600:*],nul] & residual_ice_big=[residual_ice_big[3600:*],nul]
  residual_liquid_big=[residual_liquid_big[3600:*],nul] & ncg4_temp_big=[ncg4_temp_big[3600:*],nul] & zcg4_temp_big=[zcg4_temp_big[3600:*],nul]
  sat_big=[sat_big[3600:*],nul] & sza_big=[sza_big[3600:*],nul] & iza_big=[iza_big[3600:*],nul] & dc_big=[dc_big[3600:*],nul]
  roll_big=[roll_big[3600:*],nul] & pitch_big=[pitch_big[3600:*],nul] & heading_big=[heading_big[3600:*],nul]
  for n=0,7 do ssfr_temp_big[*,n]=[ssfr_temp[3600:*,n],nul]
  for n=0,np-1 do begin
    facz_big[*,n] = [facz[3600:*,n],nul] & facn_big[*,n] = [facn[3600:*,n],nul]
    dn_big[*,n] = [dn[3600:*,n],nul] & dz_big[*,n] = [dz[3600:*,n],nul]
    for l=0,3 do begin
      spect_big[*,n,l]     = [spect[3600:*,n,l],nul]
      spect_cal_big[*,n,l] = [spect_cal[3600:*,n,l],nul]
    endfor
  endfor  
  for n=0, n_elements(nadlambda)-1 do nad_spect[*,n] = [nspectra[3600:*,n],nul]
  for n=0, n_elements(zenlambda)-1 do zen_spect[*,n] = [zspectra[3600:*,n],nul]
  i_last=i_last-3600
endif
i=i_last
goto,loop
ends:
print, 'the last file index is:',last_file_index
stop
end
