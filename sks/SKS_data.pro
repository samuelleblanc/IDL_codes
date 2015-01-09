;+
; NAME:
;   SKS_data
;
; PURPOSE:
;   Go through an entire directory and open each *.sks file. Once all files have been opened and read,
;   substract the appropriate darks, and then run the calibration determined in the .cfg file
;   Save the results in a IDL out file, similar to SSFR
;   Potential to plot each spectra as it gets analysed. 
;
; CATEGORY:
;   SKS data analysis. First data read
;
; CALLING SEQUENCE:
;   SKS_data, date
;    - where date is in format yyyymmdd
;   
; OUTPUT:
;   - yyyymmdd_SP.out - IDL out file for Spectra
;   - plots
;
; KEYWORDS:
;   - doplot - real time plotting
;
; DEPENDENCIES:
;   - read_SKS.pro - program to ingest the SKS files
;   - SKS_rt_plots.pro - program to plot real time data plots of calibrated and uncalibrated spectra
;   - get_wvl.pro - program to build the wavelength arrays
;   - get_response.pro - program to get the response functions for calibrating the spectra
;   - get_cos.pro - program to get the cosine correction for the irradiance measurement 
;   - zensun.pro
;   - cfg.pro - program to read the .cfg file
;   - rph2za.pro - program for cosine correction
;   - muslope.pro - program for cosine correction
;
; NEEDED FILES:
;   - .SKS files
;   - .cfg file
;   - response functions
;   - spectral cosine response functions
;   
; EXAMPLE:
;   
;
; MODIFICATION HISTORY:
; Written:  Samuel LeBlanc, October 12th, 2012, Ottawa, Canada
; Modified: 
;           
;---------------------------------------------------------------------------

@read_SKS.pro
@SKS_RT_plots.pro
@get_wvl.pro
@get_response.pro
@get_cos.pro
@zensun.pro
@rph2za.pro
@muslope.pro

pro SKS_data, date, doplot=doplot

close, /all

if n_elements(date) lt 1 then $
	date='20120507' $
	else date=date

dir='C:\Users\Samuel\Research\SKS\data\'
l='\'

cfg_file=dir+date+l+date+'.cfg'        ; build cfg file   ; change this at dryden

if file_test(cfg_file) ne 1 then message,'Did not find configuration file.'

; Read from cfg
if n_elements(doplot) lt 1 then doplot   = strcmp(strmid(cfg(cfg_file,'plot'),0,1),'y',/FOLD_CASE) ; make plots
np       = fix(cfg(cfg_file,'np'  ))    ; number of channels for each spectrum
darkmin  = fix(cfg(cfg_file,'darkmin')) ; required minimum length of dark cycle
platform = cfg(cfg_file,'platform')
extra    = cfg(cfg_file,'extra') ; extra-terrestial flux
lat      = float(cfg(cfg_file,'lat')) ; get latitude
lon      = float(cfg(cfg_file,'lon')) ; get longitude

data     = cfg(cfg_file,'data')    ; data file names
data     = dir+l+date+l+data              ; append file name to path

attcorr  = cfg(cfg_file,'attcorr')
if attcorr eq 'yes' then attcorr=1 else attcorr=0
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
response=response;*100.

;set up the variables
num=60000 ; big num to set variables
spect    =fltarr(num,np,4) ;uncalibrated spectra
spectcal =fltarr(num,np,4) ;calibrated spectra
utc      =fltarr(num)*!values.f_nan       ;ssfr utc
temps    =fltarr(num,9)    ;ssfr temperatures
darks    =fltarr(num,np,4) ; all the darks
dark_utc =fltarr(num)      ; times for the darks (for interpolating)
shut     =fltarr(num)      ; shuttered times

; now check for files
spc_files = file_search(data, count = numfiles, /FOLD_CASE)
if (numfiles eq 0) then message,'No data files found.'

print, 'Process ',data
print, 'Number of files: ', numfiles

count=0
dcount=0
for i=0, numfiles-1 do begin ; primary read loop
  print, "Opening ",spc_files[i]
  print,format='(a1,$)',"."
  
  read_SKS, spc_files[i],sp, eos, shutter, inttime, temp, time, ct, ndarks, dark
  
  ; verify the correct integration time
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; to be written
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ; verify that the big num is big enough
  if count+ct ge num then message, 'num not big enough, please make bigger'

  ; now put the data from the one file to the arrays
  spect[count:count+ct,*,*]=sp
  ; the spect is only the raw spectra, which includes the darks
  n=where(shutter[*,0] eq 0,ns)
  if ns gt 0 then begin ;if there is darks in this file, save to array for ater interpolation
    dark_utc[dcount:dcount+ndarks]=time[n]
    darks[dcount:dcount+ndarks,*,*]=dark
    dcount=dcount+ndarks
  endif
  temps[count:count+ct,*]= temp
  utc[count:count+ct]    = time
  shut[count:count+ct]   = shutter[*,0]
  count=count+ct
endfor

; now cut the arrays to proper length
spect    = spect[0:count,*,*]
dark_utc = dark_utc[0:dcount]
darks    = darks[0:dcount,*,*]
temps    = temps[0:count,*]
utc      = utc[0:count]
shut     = shut[0:count] ; 1 when open, 0 when closed
spectcal = spectcal[0:count,*,*]
dark     = spect

; now process the darks by interpolating between the dark cycles and take out the darks from the spectra
if dark_utc[0] gt utc[0] then dark_utc[0]=utc[0]
if dark_utc[n_elements(dark_utc)-1] lt utc[n_elements(utc)-1] then dark_utc[n_elements(dark_utc)-1]=utc[n_elements(utc)-1]
for i=0, np-1 do begin ; go through each wavelength
  for j=0,3 do begin ; go through each spectrometer
    darktemp       = interpol(smooth(darks[*,i,j],4),dark_utc,utc)
    spect[*,i,j]   = spect[*,i,j]-darktemp
    spectcal[*,i,j]= spect[*,i,j]/response[i,j]
    dark[*,i,j]    = darktemp
  endfor
endfor

shutop=where(shut eq 1 and shift(shut,1) eq 1 and shift(shut,-1) eq 1,cs)
if cs lt 1 then message, 'No measured spectra that is not shuttered'
; now take out the darks in the 'final' joined spectra
zspectra   =[[spectcal[shutop,indices[0,0]:indices[1,0],0]],[spectcal[shutop,indices[0,1]:indices[1,1],1]]]
nspectra   =[[spectcal[shutop,indices[0,2]:indices[1,2],2]],[spectcal[shutop,indices[0,3]:indices[1,3],3]]]
tmhrs      =utc[shutop]
;stop

;;;; Portion of the code that returns the sza and cosine factors of the light collectors ;;;;
juld=julian_day(fix(strmid(date,0,4)),fix(strmid(date,4,2)),fix(strmid(date,6,2)))
if juld eq 0 or juld gt 366 then message,'Wrong definition of Julian day!'
zensun,juld,utc,lat,lon,sza,azimuth,solfac
pit = 0.0 & rol = 0.0 ; set the pitch and roll to zero since the light collectors are not moving
hed = 0.0

if attcorr then begin
  rph2za,pit+pitchoff,rol+rolloff,hed,ssfrzenith,ssfrazimuth
  dc = muslope(sza,azimuth,ssfrzenith,ssfrazimuth) ; directional cosine^M
  iza=sza*0.0+ssfrzenith

  ; get the cosine corrections
  get_cos,cfg_file,sza,dc,zenlambda,facz,dz,nadlambda,facn,dn
  pres=replicate(830.0,n_elements(utc)) ;must change this to read in the actual pressure, right now using some average boulder pressure

  for n=0, n_elements(nadlambda)-1 do begin
    tau=bodhaine(nadlambda[n],pres,0.)
    fac=facn[*,n]*(exp(tau*cos(sza*!pi/180.)))+dn[n]*(1-exp(tau*cos(sza*!pi/180.))) ; use the bodhaine formula here
    nspectra[*,n]=nspectra[*,n]*fac
  endfor
endif ; end of attcorr segment

;now plot the data
if doplot then SKS_rt_plots, wvl, utc, spect, spectcal, sza, extra


;save the data
save, utc, sza, spect, spectcal, nspectra, zspectra, temps, darks, dark, dark_utc, shut, attcorr, nadlambda, zenlambda, $
  wvl,tmhrs,filename=dir+date+l+date+'_SP.out'

;need to determine an easy way of finding saturation
end
