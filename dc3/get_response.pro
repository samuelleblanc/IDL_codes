;+
; NAME:
;   get_response
;
; PURPOSE:
;   Read in the data from the response file defined in cfg_file and return the response functions
;
; CATEGORY:
;   ATTREX real time data module   
;
; CALLING SEQUENCE:
;   get_response, cfg_file, response
;   - cfg_file is the file path for the config file
;   - response is an array with the response function for 0: zsi  1:zir  2:nsi  3:nir
;
; OUTPUT:
;   response functions
;
; KEYWORDS:
;   none
;
; DEPENDENCIES:
;   - cfg.pro
; 
; NEEDED FILES:
;   - .cfg file
;   - response functions files 
;   
; EXAMPLE:
;   
;
; MODIFICATION HISTORY:
; Written:  Samuel LeBlanc, LASP CU Boulder, September 19th, 2011
; Modified: 
;           
;---------------------------------------------------------------------------
@cfg.pro

pro get_response, cfg_file, response

; Read parameters from configuration file
date   =cfg(cfg_file,'date')
np     =cfg(cfg_file,'np'  )    ; number of channels for each spectrum
np     =fix(np)
zrespsi =cfg(cfg_file,'zresponseSI') ; response function Si
zrespir =cfg(cfg_file,'zresponseIR') ; response function IR
nrespsi =cfg(cfg_file,'nresponseSI') ; response function Si
nrespir =cfg(cfg_file,'nresponseIR') ; response function IR
respdir =cfg(cfg_file,'respdir') ; directory where response functions are located

; Initialize: get response functions
cd,respdir,current=prevdir

zrespsi1 = file_search(zrespsi,count=zrsi,/FOLD_CASE)
zrespir1 = file_search(zrespir,count=zrir,/FOLD_CASE)

nrespsi1 = file_search(nrespsi,count=nrsi,/FOLD_CASE)
nrespir1 = file_search(nrespir,count=nrir,/FOLD_CASE)

if zrsi lt 1 then message,'Did not find zenith si response functions.' ; Exit program
if zrir lt 1 then message,'Did not find zenith ir response functions.' ; Exit program
if nrsi lt 1 then message,'Did not find nadir si response functions.'  ; Exit program
if nrir lt 1 then message,'Did not find nadir ir response functions.'  ; Exit program

if zrsi gt 1 then begin      ; found more than one SI response function
  resp_date = lonarr(zrsi)
  for ii=0,zrsi-1 do begin
    tmp = strsplit(zrespsi1[ii],'_',/extract)
    resp_date[ii] = long(tmp[0])
  endfor
  resp_index = max(where(resp_date le date)) ; use the latest response function, in the standard setting
  print, "Using zenith Si response function ", zrespsi1[resp_index]
  zrespsi1 = respdir+zrespsi1[resp_index]
endif
if nrsi gt 1 then begin      ; found more than one SI response function
  resp_date = lonarr(nrsi)
  for ii=0,nrsi-1 do begin
    tmp = strsplit(nrespsi1[ii],'_',/extract)
    resp_date[ii] = long(tmp[0])
  endfor
  resp_index = max(where(resp_date le date)) ; use the latest response function, in the standard setting
  print, "Using nadir Si response function ", nrespsi1[resp_index]
  nrespsi1 = respdir+nrespsi1[resp_index]
endif

if zrir gt 1 then begin      ; found more than one IR response function
  resp_date = lonarr(zrir)
  for ii=0,zrir-1 do begin
    tmp = strsplit(zrespir1[ii],'_',/extract)
    resp_date[ii] = long(tmp[0])
  endfor
  resp_index = max(where(resp_date le date)) ; use the latest response function, in the standard setting
  print, "Using zenith InGaAs response function ", zrespir1[resp_index]
  zrespir1 = respdir+zrespir1[resp_index]
endif
if nrir gt 1 then begin      ; found more than one IR response function
  resp_date = lonarr(nrir)
  for ii=0,nrir-1 do begin
    tmp = strsplit(nrespir1[ii],'_',/extract)
    resp_date[ii] = long(tmp[0])
  endfor
  resp_index = max(where(resp_date le date)) ; use the latest response function, in the standard setting
  print, "Using nadir InGaAs response function ", nrespir1[resp_index]
  nrespir1 = respdir+nrespir1[resp_index]
endif

; Read zenith response functions into "si" and "ir", respectively
zsi = fltarr(2, 256)            & zir = fltarr(2, 256)
openr,zsilun,zrespsi1,/get_lun  & openr,zirlun,zrespir1,/get_lun
readf,zsilun,zsi                & readf,zirlun,zir
free_lun,zsilun                 & free_lun,zirlun

; Read nadir response functions into "si" and "ir", respectively
nsi = fltarr(2, 256)             & nir = fltarr(2, 256)
openr,nsilun,nrespsi1,/get_lun   & openr,nirlun,nrespir1,/get_lun
readf,nsilun,nsi                 & readf,nirlun,nir
free_lun,nsilun                  & free_lun,nirlun

cd,prevdir

response=[[reform(zsi[1,*])],[reform(zir[1,*])],[reform(nsi[1,*])],[reform(nir[1,*])]]
end
