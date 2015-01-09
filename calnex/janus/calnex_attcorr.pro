;+
; NAME:
;   calnex_attcorr
;
; PURPOSE:
;   Run the attitude correction routine on the calibspcs data from calnex. Can be done to maximize roll and pitch offsets
;
; CATEGORY:
;   CALNEX / Calibration
;
; CALLING SEQUENCE:
;   calnex_attcorr, date, 
;   
; OUTPUT:
;   calibspecs_attcorr.out files
;
; KEYWORDS:
;   - mass         makes large scale calculation and finds the best offset, no display
;   - comps        compare on a single plot
;   - png          make png of plots
;   - da           delta angle for angular offset check - default is set to 0.2
;   - uuin         interval window to select
;   - few          make a few plots
;   - offsetcheck  disable writing file, but enable calculations of offsets
;
; DEPENDENCIES:
;   cfg.pro            ;config file cheker
;   nav_p3.pro         ;calnex nav reader routine for the p3
;   get_cos_simple.pro ;cosine correction routine
;   legend.pro         ;to make legend on graph
;   
; NEEDED FILES:
;   - config file for day
;   - calibspcs file
;   - nav files
;  
; EXAMPLE:
;   calnex_attcorr
;
; MODIFICATION HISTORY:
; Written:  Sebastian Schmidt, LASP CU Boulder, date unknown
; Modified: November, 2010
;          -by Samuel LeBlanc
;           Added large comparisons of offsets and offset determination
;---------------------------------------------------------------------------

@nav_p3.pro
@legend.pro
@get_cos.pro ;@get_cos_simple.pro
@cfg.pro

pro calnex_attcorr, dateinp, mass=mass,comps=comps, png=png, da=da, uuin=uuin, few=few, offsetcheck=offsetcheck

; settings *****************************************************
if n_elements(dateinp) lt 1 then date = '20100516' else date = strtrim(string(dateinp),2)	; date to use

if (!VERSION.os ne 'linux') then begin								; directory path (for windows and linux)
  ; windows:
  dir='C:\CalNex\p3\'
  ll ='\'
  l=ll
endif else begin
  ; linux:
  dir='/data/seven/schmidt/calnex/p3/'
  ll='/' ; directory separator
  l=ll
endelse
close, /all

if keyword_set(mass) then massive=1 else massive=0      ; makes large scale calculation and finds the best offset, no display
if keyword_set(comps) then compares=1 else compares=0   ; compare on a single plot
if keyword_set(png) then png = 1 else png=0 				    ; make png of plots
if keyword_set(few) then few=1 else few=0               ; make a few plots
; initialize configuration file
cfgf=dir+ll+date+ll+date+'.cfg'        							; build cfg file
if file_test(cfgf) ne 1 then message,'Did not find configuration file.'

izamax=float(cfg(cfgf,'izamax'))						                ; maximum SSFR zenith angle - standard: 0.5 deg
wl=cfg(cfgf,'wlql')
wl=strsplit(wl,' ,',escape='#',/EXTRACT)
wl=float(wl)
nl=n_elements(wl) 
wl=wl[1]	             ; wavelength that is plotted
if keyword_set(offsetcheck) then offsetcheck=1 else offsetcheck=0 
                             ; if this is set, offset angles are checked. When
                             ; set to 0, the attcorr is made with the cfg file values
                             ; and output is actually written.
if keyword_set(da) then da =da else da     = 0.2                 ; angular resolution of offset check [deg]
; settings *****************************************************

; graphics *****************************************************
device,decomposed=0
loadct,39
;tvlct, r,g,b, /get
;r=reverse(r) & g=reverse(g) & b=reverse(b)
;tvlct,r,g,b
!p.thick=2.0 & !p.charsize=1.8
!x.style=1 & !y.style=1 
!y.thick=1.2 & !x.thick=1.2
!p.color=0 & !p.background=255
; graphics *****************************************************

; read cfg file ************************************************
i0=cfg(cfgf,'interval') ; look if we should only use data within a certain time window
uu0=0
if not strcmp(i0,'#') then begin
    uu=strsplit(i0,' ,',escape='#',/EXTRACT)
    uu=float(uu)
    uu0=1
endif
if keyword_set(uuin) then uu=uuin else uu=uu
platform=cfg(cfgf,'platform')
if strcmp(platform,'NOAAP3',3,/FOLD_CASE) then p3=1 else p3=0
nav=cfg(cfgf,'read_nav')
if strcmp(strmid(nav,0,1),'n',/FOLD_CASE) then nav=0 else nav=1
; read cfg file ************************************************

; get spectra  ************************************************
print,'Restore SSFR data...'
restore, dir+date+l+date+'_calibspcs.out'
utc   = tmhrs
nn    = n_elements(utc)
lamda = zenlambda
nl    = n_elements(lamda)
zs    = zspectra
ns    = fltarr(nl,nn)
mm=min(abs(wl-lamda),wlind)

print,'Interpolating spectra...'
for i=0,nn-1 do begin
  ns[*,i]=interpol(nspectra[*,i],nadlambda,lamda)
endfor
; get spectra  *************************************************

if offsetcheck then begin ; you can switch off this step, it is only thought for
                          ; comparing several pitch and roll offset values

  ; intermediate step: get nav data with a couple of offset angles ****
  siz=get_screen_size()
  window,0,tit='Offset Angles',xs=siz[0],ys=siz[1]*0.9
  rol0   =cfg(cfgf,'rolloff') ; get roll offset
  pit0   =cfg(cfgf,'pitchoff') ; get pitch offset
  rolz   =float(rol0)
  pitz   =float(pit0)
  if da le 1. then begin
;    pit0=[-1,0,1]*da+pitz
;    rol0=[-1,0,1]*da+rolz
    mat=[-1,0,1]
    if massive then mat=(findgen(11)-5.)
    if few then mat=[-1,0,1] else mat=[-2,-1,0,1,2]
  endif else begin
;    pit0=[-2,-1,0,1,2]*da+pitz
;    rol0=[-2,-1,0,1,2]*da+rolz
    if few then mat=[-1,0,1] else mat=[-2,-1,0,1,2]
  endelse
  pit0=mat*da+pitz
  rol0=mat*da+rolz
  np=n_elements(pit0)&nr=n_elements(rol0)
  if compares then !P.multi=0 else !P.multi=[0,np,nr]
  extra=cfg(cfgf,'extra') ; location of extraterrestial radiation file
  ss_o=1.
  off_pit=0.
  off_rol=0.
  cl=0
  for i=0,np-1 do begin
    for j=0,nr-1 do begin
      if p3 then begin
        nav_p3,dir,ll,date,cfgf,utc,alt,lat,lon,dc,sza,iza,offsetp=pit0[i],offsetr=rol0[j],heading=hed
      endif else begin
        print, 'Wrong Platform'
      endelse

      get_cos,dir,date,cfgf,utc,alt,sza,dc,zenlambda,facz,dz,nadlambda,facn,dn
      
      hedp=shift(hed, 2)
      flt=where(utc gt uu[0] and utc le uu[1] and abs(iza) lt izamax and abs(hed-hedp) lt  0.2)
      
      if ~massive then begin
        if compares and ~(i eq 0 and j eq 0) then begin
          oplot, tmhrs, zs[wlind,*]*facz, color=cl*250/10,psym=3 
          txt_cl=[txt_cl,cl*250/10,cl*250/10]
          leg_txt=[leg_txt,'pitch '+strtrim(string(pit0[i]),2),'roll  '+strtrim(string(rol0[j]),2)]   
        endif else begin
          plot,tmhrs,zs[wlind,*]*facz,xr=uu,/xs,psym=3, xmargin=[8,16], ystyle=8,$
          yrange=[min(zs[wlind,flt])*0.96,max(zs[wlind,flt])*1.04];,yr=[0,max(zs[wlind,*])*1.2
          legend,['pitch '+strtrim(string(pit0[i]),2),'roll  '+strtrim(string(rol0[j]),2)],textcolors=[0,0],/bottom
          txt_cl=[255,255]
          leg_txt=['pitch '+strtrim(string(pit0[i]),2),'roll  '+strtrim(string(rol0[j]),2)]
        endelse
        
        cl=cl+1
      endif
  
      k=linfit(utc[flt],zs[wlind,flt]*facz[flt],sigma=ss)
      print, ss_o, ss[1]
      if ss[1] lt ss_o then begin
        off_pit=pit0[i]
        off_rol=rol0[j]
        ss_o=ss[1]
      endif
        
    endfor
  endfor
  
  axis, yaxis=1, ytitle='altitude', ystyle=1, yrange = [000.,max(alt)], /save, color=250
  oplot, utc, alt, color=250
  
  axis, 0.97,yaxis=1, ytitle='heading', ystyle=1, yrange = [0,360.], /save, /normal, color=70
  oplot, utc, hed, color=70
  
  if compares then legend,leg_txt,textcolors=txt_cl,/bottom,/clear
  
  print, 'pitch offset:',off_pit
  print, 'roll offset:',off_rol
  if png then begin
    p=tvrd(true=1)
    write_png,dir+date+l+date+'_offset.png',p
  endif 
  
  ; intermediate step: get nav data with a couple of offset angles ****
  !P.multi=0
  window,1,tit='heading'
  plot,utc[flt],hed[flt],xr=uu, yrange=[0,360],/xs,psym=3,tit='Heading for the '+date
  if png then begin
    p=tvrd(true=1)
    write_png,dir+date+l+date+'_heading.png',p
  endif

endif else begin

  ; get nav data with offset angles as specified in cfg file  ****
  if p3 then begin
    nav_p3,dir,ll,date,cfgf,utc,alt,lat,lon,dc,sza,iza,heading=hed
  endif else begin
    print, 'Wrong Platform'
  endelse

  ; get cosine correction **************************************** only for direct correction
  get_cos,dir,date,cfgf,utc,alt,sza,dc,zenlambda,facz,dz,nadlambda,facn,dn
  flt=where(utc gt uu[0] and utc le uu[1] and abs(iza) lt izamax)
  
  fac=facz
  nadcorr=dn

  ;nspectra = ns*nadcorr
  for i=0,nl-1 do begin
    nspectra[i,*] = ns[i,*]*nadcorr[i]
    zspectra[i,*]=zs[i,*]*facz[*,i]
  endfor

  ; archive for cerberus
  nadlambda=zenlambda
  rol0   =cfg(cfgf,'rolloff') ; get roll offset
  pit0   =cfg(cfgf,'pitchoff') ; get pitch offset
  rol0   =float(rol0)
  pit0   =float(pit0)
  stop
  save, tmhrs, zenlambda, zspectra, nadlambda, nspectra, sat, status, rol0, pit0, nadcorr, flt, filename = dir+date+l+date+'_calibspcs_attcorr.out'
endelse ; offsetcheck

  window,3
  plot,utc[flt],cos(!pi/180.*sza[flt]),xr=uu,psym=3,tit='Cosine Response for '+date
  oplot,[min(utc),max(utc)],[0.82,0.82],linesty=1
  oplot,utc[flt],nspectra[45,flt]/zspectra[45,flt],psym=3,color=40
  
  if png then begin
    p=tvrd(true=1)
    write_png,dir+date+l+date+'_bruce.png',p
  endif
;stop
end
