;+
; NAME:
;   archive_p3
;
; PURPOSE:
;   Archiving utility for the NOAA archiving format, prints SSFR on the P3 data in the desired format to be
;   archived using NOAA's archiving system for research missions 
;
; CATEGORY:
;   CALNEX / NOAA 
;
; CALLING SEQUENCE:
;   archive_p3,dir,ll,date,utf,nf,zenlambda,zen,bz1,bz2,nadlambda,nad,bn1,bn2,bb1,bb2,wl,nl,zind,nind,cg4up,cg4dn,comment,status,izamax,platform,satnb
;
; OUTPUT:
;   archiving files
;
; KEYWORDS:
;   Archive, SSFR, P3, NOAA, CALNEX
;
; DEPENDENCIES:
;   cfg.pro       ;config file cheker
;   nav_p3.pro    ;p3 nav program
;       
;  
; EXAMPLE:
; 
;
; MODIFICATION HISTORY:
; Written:  Sebastian Schmidtc, LASP CU Boulder, date unknown
; Modified: wednesday April 21st, 2010
;          -by Samuel LeBlanc
;           changed writing formats to NOAA 2010 Standards
;           added cfg file checking for version of revision archive format
;           added revision "RA"
;           changed nav program dependency
;          -by Samuel LeBlanc, May 4th, 2010
;	    added comma in first line seperator
;	         -by Samuel LeBlanc, Jan 14th, 2011
;	         changed revision "R0" and comments
;---------------------------------------------------------------------------
@cfg.pro
pro archive_p3,dir,ll,date,utf,nf,zenlambda,zen,bz1,bz2,nadlambda,nad,bn1,bn2,bb1,bb2,wl,nl,zind,nind,cg4up,cg4dn,comment,status,izamax,platform,satnb
;message,'Needs to be modified to new format.'


if not strcmp(strmid(platform,0,4),'noaa',/FOLD_CASE) then message,'Wrong platform.'

cfg_file=dir+ll+date+ll+date+'.cfg'        ; build cfg file
if file_test(cfg_file) ne 1 then message,'Did not find configuration file.'



revision=cfg(cfg_file, 'revision')
revision=strsplit(revision,' ,',escape='#',/EXTRACT)
comments_revision=['R0: Final upwelling shortwave and longwave (both upwelling and downwelling) irradiance , but pre-final downwelling shortwave irradiance']


; read nav data again, just to get the AOCTimeWave
nav_p3,dir,ll,date,d0,aoc,alt,lat,lon,dc,sza,iza ; d0,d1 are dummies, not set

flight_date   =strmid(date,0,4)+', '+strmid(date,4,2)+', '+strmid(date,6,2)
systime       =systime(/julian)
caldat,systime,mm,dd,yyyy
archival_date =strcompress(string(yyyy,format='(I4.4)'),/remove_all)+', '+strcompress(string(mm,format='(I2.2)'),/remove_all)+', '+strcompress(string(dd,format='(I2.2)'),/remove_all)

warned=0

ns1='DN'+strcompress(string(fix(wl)),/remove_all)+', '+'W m^-2 nm^-1, Downward Shortwave Irradiance at '+strcompress(string(zenlambda[zind]),/remove_all)+' nm '
ns1=[ns1,'DN_SW'+   strcompress(string(fix(bb1[0])),/remove_all)+'_'+ strcompress(string(fix(bb1[1])),/remove_all)+', '+$
'W m^-2 , Downward Shortwave Broadband Irradiance '+strcompress(string(bb1[0]),/remove_all)+'-'+strcompress(string(bb1[1]),/remove_all)+' nm']
ns1=[ns1,'DN_SW'+   strcompress(string(fix(bb2[0])),/remove_all)+'_'+ strcompress(string(fix(bb2[1])),/remove_all)+', '+$
'W m^-2 , Downward Shortwave Broadband Irradiance '+strcompress(string(bb2[0]),/remove_all)+'-'+strcompress(string(bb2[1]),/remove_all)+' nm']
ns1=[ns1,'DN_LW'+', '+'W m^-2 , Downward Longwave Broadband Irradiance 4.5-40 um ']
ns2='UP'+strcompress(string(fix(wl)),/remove_all)+', '+'W m^-2 nm^-1 , Upward Shortwave Irradiance at '  +strcompress(string(nadlambda[nind]),/remove_all)+' nm'
ns2=[ns2,'UP_SW'+   strcompress(string(fix(bb1[0])),/remove_all)+'_'+ strcompress(string(fix(bb1[1])),/remove_all)+', '+$
'W m^-2 , Upward Shortwave Broadband Irradiance '+strcompress(string(bb1[0]),/remove_all)+'-'+strcompress(string(bb1[1]),/remove_all)+' nm']
ns2=[ns2,'UP_SW'+   strcompress(string(fix(bb2[0])),/remove_all)+'_'+ strcompress(string(fix(bb2[1])),/remove_all)+', '+$
'W m^-2 , Upward Shortwave Broadband Irradiance '+strcompress(string(bb2[0]),/remove_all)+'-'+strcompress(string(bb2[1]),/remove_all)+' nm']
ns2=[ns2,'UP_LW'+', '+'W m^-2 , Upward Longwave Broadband Irradiance 4.5-40 um']

nc=n_elements(comment) ; special comment
if nc eq 0 and satnb gt 0 then begin
  comment='Number of saturated spectra = '+string(satnb)
  nc=nc+1
endif else begin
  if nc gt 0 and satnb gt 0 then begin
    comment=[comment,'Number of saturated spectra = '+string(satnb)]
    nc=nc+1
  endif
endelse

if nc eq 0 then ncs= string(nc)+' ' else $
                ncs=[string(nc)+' ',$
                     comment]

if strcmp(strmid(platform,0,4),'noaa',/FOLD_CASE) then begin
  pi='Peter Pilewskie'
  mi='CALNEX'
  mailphone='email: Peter.Pilewskie@lasp.colorado.edu phone: 303 492 5724'
  instp='; CG-4 broadband longwave irradiance 4.5-4000 um'
  datinfo='DATA_INFO: Reported are only selected wavelengths, as well as integrated solar irradiance in two ranges (SSFR). In addition, up- and downward longwave irradiance is archived (CG-4).'
endif 

pos=strpos(status[4],'/',/REVERSE_SEARCH)+1
if izamax lt 0 then h0='Preliminary data '+revision+' not corrected for aircraft attitude' else h0=''
rc=[h0,$
    'PI_CONTACT_INFO: Address: 392 LASP Campus Box, University of Colorado Boulder, CO 80309; '+mailphone,$
    'PLATFORM: '+platform,$
    'LOCATION: Get from NAV files which are recorded separately.',$
    'ASSOCIATED_DATA: N/A',$
    'INSTRUMENT_INFO: SSFR=Solar Spectral Flux Radiometer 350-2150 nm'+' '+instp,$
    datinfo,$
    'UNCERTAINTY: Nominal SSFR uncertainty: 5%',$
    'ULOD_FLAG: -7777',$
    'ULOD_VALUE: N/A',$
    'LLOD_FLAG: -8888',$
    'LLOD_VALUE: N/A',$
    'DM_CONTACT_INFO: N/A',$
    'PROJECT_INFO: CALNEX experiment operated out of Ontario, CA during May - June 2010',$
    'STIPULATIONS_ON_USE: Use of these data requires PRIOR OK from the PI',$
    'OTHER_COMMENTS: For full spectra contact PI.',$
    'REVISION: '+strmid(revision,0),$
    comments_revision,$
    '',$
    'To get the full SSFR spectra from 350-2150 nm, please contact Peter.Pilewskie@lasp.colorado.edu',$
    '',$
    'Calibration Zenith Silicon : '+strmid(status[4],pos),$
    'Calibration Zenith InGaAs  : '+strmid(status[5],pos),$
    'Calibration Nadir Silicon  : '+strmid(status[6],pos),$
    'Calibration Nadir InGaAs   : '+strmid(status[7],pos),$
    'Maximum allowed SSFR Zenith Angle :'+string(izamax),$
    '']
hd='AOCTimewave, '+strjoin('DN'+strcompress(string(fix(wl)),/remove_all)+', ') $
         +'DN_SW'+   strcompress(string(fix(bb1[0])),/remove_all)+'_'+ strcompress(string(fix(bb1[1])),/remove_all)+', '    $
         +'DN_SW'+   strcompress(string(fix(bb2[0])),/remove_all)+'_'+ strcompress(string(fix(bb2[1])),/remove_all)+', '    $
         +'DN_LW'+', '$
         +strjoin('UP'+strcompress(string(fix(wl)),/remove_all)+', ') $
         +'UP_SW'+   strcompress(string(fix(bb1[0])),/remove_all)+'_'+ strcompress(string(fix(bb1[1])),/remove_all)+', '    $
         +'UP_SW'+   strcompress(string(fix(bb2[0])),/remove_all)+'_'+ strcompress(string(fix(bb2[1])),/remove_all)+', '    $
         +'UP_LW'
nr=n_elements(rc)+1

header=['1001 ; header lines, data format',$
        pi+' ; name of instrument Principal Investigator',$
        'University of Colorado at Boulder ; affiliation of instrument Principal Investigator',$
        'SW Spectral / LW Broadband Irradiance from '+platform+' Solar Spectral Flux Radiometer + CG-4 ',$
        mi+' ; mission name',$
        '1, 1 ; ivol nvol',$
        flight_date+',     '+archival_date+' ; flight date and reduction/archival date',$
        string(1.0)+' ',$
        'AOCTimewave, seconds, time Data ; times greater or equal 86400 refer to the next day',$
        string((nl+3)*2)+' ',$
        strmid(strjoin(string(replicate('1. ,',(nl+3)*2))),0,strlen(strjoin(string(replicate('1. ,',(nl+3)*2))))-1)+' ; primary variable scale factors',$
        strjoin(string(replicate('-999, ',nl)))+' -9999, -9999, -9999, '+strjoin(string(replicate('-999, ',nl)))+' -9999, -9999, -9999 ; primary variable missing values',$
        ns1,ns2,ncs,$
        string(nr)+' ',$
        rc,hd]
nhd=n_elements(header)
header[0]=strcompress(string(nhd),/remove_all)+', '+header[0]

name=dir+date+ll+'SSFR_NP3_'+date+'_'+revision+'.ict'
print,'Archive the data in '+name
openw,ua,name,/get_lun
for i=0,nhd-1 do begin
  printf,ua,header[i]
endfor


utf0=0.
ulast=utf[0]*3600.

na=n_elements(aoc)
i=0
while aoc[i] lt utf[0] do begin ; fill in gaps *before* SSFR measurement
  printf,ua,strcompress(string(aoc[i]*3600.),/REMOVE_ALL)+', '+strjoin(string(replicate('-999, ',nl)))+' -9999, -9999, -9999, '+strjoin(string(replicate('-999, ',nl)))+' -9999, -9999, -9999' ; just copy from top
  i=i+1
endwhile
ii=i
for i=ii,na-1 do begin
  t=aoc[i]
  ind=where(utf le t, num)
  if num ge 1 then begin
    i0=max(ind)
    i1=i0+1
    if i1 lt n_elements(utf) then begin
    t0=(t-utf[i0])*3600. & t1=(utf[i1]-t)*3600.
    ;print,t
    if t0 gt 1.5 or t1 gt 1.5 then begin ; if 1
      ;print,'*',utf[i0],utf[i1]
      printf,ua,strcompress(string(t*3600.),/REMOVE_ALL)+', '+strjoin(string(replicate('-999, ',nl)))+' -9999, -9999, -9999, '+strjoin(string(replicate('-999, ',nl)))+' -9999, -9999, -9999' ; just copy from top
    endif else begin                     ; if 1
        printf,format='(f8.1,",",x,$)',ua,t*3600. ; time
        for l=0,nl-1 do begin
          dat=zen[l,i0] + (zen[l,i1]-zen[l,i0])/(utf[i1]-utf[i0])*(aoc[i]-utf[i0])
          if not(dat ge 0. and dat lt 3.0) then dat=-999.
          printf,format='(f13.7,",",x,$)',ua,dat
        endfor
        dat1=bz1[i0]   + (bz1  [i1]-  bz1[i0])/(utf[i1]-utf[i0])*(aoc[i]-utf[i0])
        dat2=bz2[i0]   + (bz2  [i1]-  bz2[i0])/(utf[i1]-utf[i0])*(aoc[i]-utf[i0])
        dat3=cg4dn[i0] + (cg4dn[i1]-cg4dn[i0])/(utf[i1]-utf[i0])*(aoc[i]-utf[i0])
        if not(dat1 ge 0 and dat1 lt 2000.) or not(dat2 ge 0 and dat2 lt 2000) then begin
          dat1=-9999. & dat2=-9999.
        endif
        if not (dat3 gt 0. and dat3 lt 2000.) then dat3=-9999.
        printf,format='(F11.3,",",x,F11.3,",",x,F11.3,",",x,$)',ua,dat1,dat2,dat3
        for l=0,nl-1 do begin
          dat=nad[l,i0] + (nad[l,i1]-nad[l,i0])/(utf[i1]-utf[i0])*(aoc[i]-utf[i0])
          if not(dat ge 0. and dat lt 3.0) then dat=-999.
          printf,format='(f13.7,",",x,$)',ua,dat
        endfor
        dat1=bn1[i0]   + (bn1  [i1]-  bn1[i0])/(utf[i1]-utf[i0])*(aoc[i]-utf[i0])
        dat2=bn2[i0]   + (bn2  [i1]-  bn2[i0])/(utf[i1]-utf[i0])*(aoc[i]-utf[i0])
        dat3=cg4up[i0] + (cg4up[i1]-cg4up[i0])/(utf[i1]-utf[i0])*(aoc[i]-utf[i0])
        if not(dat1 ge 0 and dat1 lt 2000.) or not(dat2 ge 0 and dat2 lt 2000) then begin
          dat1=-9999. & dat2=-9999.
        endif
        if not (dat3 gt 0. and dat3 lt 2000.) then dat3=-9999.
        printf,format='(F11.3,",",x,F11.3,",",x,F11.3)',ua,dat1,dat2,dat3
    endelse                        ; if 1
    endif else begin
      if not warned then print,'Warning, SSFR files stopped before AOC.'
      printf,ua,strcompress(string(t*3600.),/REMOVE_ALL)+', '+strjoin(string(replicate('-999, ',nl)))+' -9999, -9999, -9999, '+strjoin(string(replicate('-999, ',nl)))+' -9999, -9999, -9999' ; just copy from top
      warned=1
    endelse
  endif else begin
    print, 'something like a star or whatever, for now'
  endelse
endfor
free_lun,ua
return
end
