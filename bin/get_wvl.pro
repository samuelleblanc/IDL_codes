;+
; NAME:
;   get_wvl
;
; PURPOSE:
;   Read in the cfg file and build the wavelength arrays
;
; CATEGORY:
;   ATTREX real time data module   
;
; CALLING SEQUENCE:
;   get_wvl, cfg_file, wvl, zenlambda, nadlambda, indices
;   - where cfg_file is the path to the configuration file
;   - where wvl is an array of wavelengths for 0: zsi  1:zir  2:nsi  3:nir
;   - where zenlambda is the joined zenith wavelength array
;   - where nadlambda is the joined nadir wavelength array
;   - where indices has an array of the indices to make the joined spectra
;     in the form of [start,stop] for 0: zsi  1:zir  2:nsi  3:nir
;   
; OUTPUT:
;   wavelength arrays
;
; KEYWORDS:
;   - reverse: sets the InGaAs wavelengths reversed (for SSFR3)
;
; DEPENDENCIES:
;   - cfg.pro  
; 
; NEEDED FILES:
;   - .cfg file 
;   
; EXAMPLE:
;   
;
; MODIFICATION HISTORY:
; Written:  Samuel LeBlanc, LASP CU Boulder, September 19th, 2011
; Modified: Tuesday, April 24th, 2012
;           by Samuel LeBlanc
;           - added a reverse keyword, for reversing the InGaAs wavelengths (useful for ssfr3) 
;           
;---------------------------------------------------------------------------
@cfg.pro

pro get_wvl, cfg_file, wvl, zenlambda, nadlambda, indices, reverse=reverse
if not keyword_set(reverse) then reverse=0

; Read parameters from configuration file
np     =cfg(cfg_file,'np'  )    ; number of channels for each spectrum
np     =fix(np)

zlambdasi=cfg(cfg_file,'zlambdaSI') ; wavelength coefficients SI
zlambdasi=strsplit(zlambdasi,' ,',escape='#',/EXTRACT) ; make string into substrings
znlsi=n_elements(zlambdasi)
zlambdasi=float(zlambdasi)
zlambdair=cfg(cfg_file,'zlambdaIR') ; wavelength coefficients IR
zlambdair=strsplit(zlambdair,' ,',escape='#',/EXTRACT) ; make string into substrings
znlir=n_elements(zlambdair)
zlambdair=float(zlambdair)

nlambdasi=cfg(cfg_file,'nlambdaSI') ; wavelength coefficients SI
nlambdasi=strsplit(nlambdasi,' ,',escape='#',/EXTRACT) ; make string into substrings
nnlsi=n_elements(nlambdasi)
nlambdasi=float(nlambdasi)
nlambdair=cfg(cfg_file,'nlambdaIR') ; wavelength coefficients IR
nlambdair=strsplit(nlambdair,' ,',escape='#',/EXTRACT) ; make string into substrings
nnlir=n_elements(nlambdair)
nlambdair=float(nlambdair)

; Initialize: get wavelengths
wvlisz=fltarr(np) & wlnirz=fltarr(np)
for i=0,np-1 do begin
   for j=0,znlsi-1 do wvlisz[i]     =wvlisz[i]     +zlambdasi[j]*float(i)^j
   for j=0,znlir-1 do wlnirz[np-1-i]=wlnirz[np-1-i]+zlambdair[j]*float(i)^j
endfor
wvlisn=fltarr(np) & wlnirn=fltarr(np)
for i=0,np-1 do begin
   for j=0,nnlsi-1 do wvlisn[i]     =wvlisn[i]     +nlambdasi[j]*float(i)^j
   for j=0,nnlir-1 do wlnirn[np-1-i]=wlnirn[np-1-i]+nlambdair[j]*float(i)^j
endfor

if reverse then begin
  wlnirz=reverse(wlnirz)
  wlnirn=reverse(wlnirn)
endif

wvl=[[wvlisz],[wlnirz],[wvlisn],[wlnirn]]

zsi0    =cfg(cfg_file,'zSIfirst') ; first wavelength Si
zjoin   =cfg(cfg_file,'zJOIN')    ; joinder wavelength Si - IR
zir1    =cfg(cfg_file,'zIRlast')  ; last wavelength IR
zsi0=float(zsi0) & zjoin=float(zjoin) & zir1=float(zir1)
zsmooth =cfg(cfg_file,'zsmooth')  ; over how many channels to smooth
zsmooth =fix(zsmooth)

nsi0    =cfg(cfg_file,'nSIfirst') ; first wavelength Si
njoin   =cfg(cfg_file,'nJOIN')    ; joinder wavelength Si - IR
nir1    =cfg(cfg_file,'nIRlast')  ; last wavelength IR
nsi0=float(nsi0) & njoin=float(njoin) & nir1=float(nir1)
nsmooth =cfg(cfg_file,'nsmooth')  ; over how many channels to smooth
nsmooth =fix(nsmooth)

; Initialize: get wavelength indices
zind=where(wvl[*,0] ge zsi0)         ; get index of first Si wavelength
zindsi1=zind[0]
zind=where(wvl[*,0] lt zjoin,ct)     ; get index of last Si wavelength
zindsi2=zind[ct-1]
zind=where(wvl[*,1] gt zjoin)        ; get index of first IR wavelength
zindin1=zind[0]
zind=where(wvl[*,1] le zir1,ct)      ; get index of last IR wavelength
zindin2=zind[ct-1]
znumsi   =zindsi2-zindsi1+1
znumir   =zindin2-zindin1+1
znlambda=znumsi+znumir

nind=where(wvl[*,2] ge nsi0)         ; get index of first Si wavelength
nindsi1=nind[0]
nind=where(wvl[*,2] lt njoin,ct)     ; get index of last Si wavelength
nindsi2=nind[ct-1]
nind=where(wvl[*,3] gt njoin)        ; get index of first IR wavelength
nindin1=nind[0]
nind=where(wvl[*,3] le nir1,ct)      ; get index of last IR wavelength
nindin2=nind[ct-1]
numsi   =nindsi2-nindsi1+1
numir   =nindin2-nindin1+1
nnlambda=numsi+numir

; Initialize: get wavelengths of joined spectra
zenlambda=fltarr(znlambda)
zenlambda[0:zindsi2-zindsi1]=wvl[zindsi1:zindsi2,0]
zenlambda[zindsi2-zindsi1+1:znlambda-1]=wvl[zindin1:zindin2,1]

nadlambda=fltarr(nnlambda)
nadlambda[0:nindsi2-nindsi1]=wvl[nindsi1:nindsi2,2]
nadlambda[nindsi2-nindsi1+1:nnlambda-1]=wvl[nindin1:nindin2,3]

indices=[[zindsi1,zindsi2],[zindin1,zindin2],[nindsi1,nindsi2],[nindin1,nindin2]]
end
