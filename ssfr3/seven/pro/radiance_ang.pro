;+
; NAME:
;   radiance_ang
;
; PURPOSE:
;   Read in the data for calibrations of the radiance light collector, calculate the field of view
;
; CATEGORY:
;   Radiance / Calibration
;
; CALLING SEQUENCE:
;   radiance_ang,/ps
;   
; OUTPUT:
;   - Plots of the angular calibration
;
; KEYWORDS:
;   ps - to output plots to ps instead of the screen   
;
; DEPENDENCIES:
;   cfg.pro       ;config file cheker
;   legend.pro    ;to make legend on graph
;   ingest_ssfr.pro ;to read the files from SSFR
;   get_wvl.pro   ; to build the wavelength arrays
;   
; NEEDED FILES:
;   - config file for calib
;   - all ".OSA" files for the calibs under the proper directory (/SSFR3/cal/*date*/p3/(*lamp number*)/(cal or dark))
;   - temp.dat for SSFR3
;  
; EXAMPLE:
;   radiance_ang
;
; MODIFICATION HISTORY:
; Written:  Samuel LeBlanc, LASP CU Boulder, Tuesday, April 24th, 2012
; Modified: 
;
;---------------------------------------------------------------------------
@ingest_ssfr.pro
@legend.pro
@cfg.pro
@get_wvl.pro
@ingest_temp.pro


pro radiance_ang,ps=ps
if not keyword_set(ps) then ps=0
path='/home/leblanc/SSFR3/cal/'
l   ='/'                         ; directory separator
cfg_file=path+l+'ssfr3_ang.cfg'

if ps then begin 
 set_plot, 'ps'
endif else begin
  set_plot, 'x'
  device,decomposed=0
endelse

loadct,39, /silent
!p.color=0
!p.background=255
!P.multi=0
if file_test(cfg_file) ne 1 then message,'Did not find configuration file.'

np                    = fix(cfg(cfg_file,'np'  ))    ; number of channels for each spectrum
innp_used             = fix(cfg(cfg_file,'innp_used'))    ; number of channels used in the IR
data                  = cfg(cfg_file,'data')    ; data file extension
port                  = cfg(cfg_file,'port')    ; which connection is used
;if port ne 'zenith' then message, 'only set up for doing the zenith  angular calibrations'
cal_date              = strsplit(cfg(cfg_file,'cal_date') ,', ',escape='#',/extract)  ; array of primary lamp

;plotting commands
line_color            = strsplit(cfg(cfg_file,'line_color'),', ',escape='#',/extract)
line_style            = strsplit(cfg(cfg_file,'line_style'),', ',escape='#',/extract)
line_thick            = strsplit(cfg(cfg_file,'line_thick'),', ',escape='#',/extract)
wvls                  = strsplit(cfg(cfg_file,'wlql'),', ',escape='#',/extract)
line_color = float(line_color)
line_style = float(line_style)
line_thick = float(line_thick)
wvls       = float(wvls)

ang_cals = cfg(cfg_file,'ang_cals')
ang_dark = cfg(cfg_file,'ang_dark')
angs     = float(strsplit(cfg(cfg_file,'angs'),', ',escape='#',/extract))-0.2 ;array of the angles used
num=n_elements(angs)

;build the wavelength arrays
get_wvl, cfg_file, wvl, zenlambda, nadlambda, indices, /reverse

;find the files
f_dark=file_search(ang_dark+l+data,count=numfiles,/FOLD_CASE)
if numfiles ne 1 then message, 'There should only be one dark file'
f_ang=file_search(ang_cals+l+data,count=numfiles,/FOLD_CASE)
if numfiles ne num then message, 'The number of files do not match the number of angles'
f_ang=f_ang[sort(f_ang)] ;sorted by ascending order
ingest_ssfr,f_dark,cal_date[0],ssfr_utc,raw,drk,ch,int,n_drk,err,n_sat,n_ctr,/ssfr3,/avg
dark=raw

cal=fltarr(num,np,4)

for i=0,num-1 do begin
  ingest_ssfr,f_ang[i],cal_date[0],ssfr_utc,raw,drk,ch,int,n_drk,err,n_sat,n_ctr,/ssfr3,/avg
  cal[i,*,*]=raw-dark
endfor
;stop
;build the entire spectrum
if port eq 'zenith' then begin
  spec=fltarr(num,n_elements(zenlambda))
  for i=0,num-1 do begin
    spec[i,*]=[reform(cal[i,indices[0,0]:indices[1,0],0]),reform(cal[i,indices[0,1]:(indices[1,1]),1])]
  ;  spec[i,*]=spec[i,*]/spec[0,*] ;normalize to the 0 degree level
  endfor
  lambda=zenlambda
endif else begin
  spec=fltarr(num,n_elements(nadlambda))
  for i=0,num-1 do begin 
    spec[i,*]=[reform(cal[i,indices[0,2]:indices[1,2],2]),reform(cal[i,indices[0,3]:indices[1,3],3])]
  ;  spec[i,*]=spec[i,*]/spec[0,*] ;normalize to the 0 degree level
  endfor
  lambda=nadlambda
endelse
;stop
;normalize to the one degree level
norm=spec[1,*]
for i=0, num-1 do spec[i,*]=spec[i,*]/norm

; determine the field of view (via the FWHM)
angs_fine = findgen(1000)*max(angs)/1000. + min(angs)
spec_fine = fltarr(1000,n_elements(lambda))
FWHM = fltarr(n_elements(lambda))
for n=0, n_elements(lambda)-1 do begin
  spec_fine[*,n]=spline(angs,spec[*,n],angs_fine)
  nul=min(abs(spec_fine[*,n]-0.5),mm)
  FWHM[n]=angs_fine[mm]*2.
endfor 

print, 'The Average FWHM is:', mean(FWHM,/nan), ' +/-', stddev(FWHM,/nan)

; now plot the results of the angular cal at the selected wavelenghts and linecolor/style/thickness
  if ps then begin
    print, 'plotting in ps'
    
    device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=path+l+'ang.ps'
    device, xsize=20, ysize=40
  endif else begin
    window,1,tit='Ang', xsize=500, ysize=1000
  endelse
    !p.font=1 & !p.thick=5 & !p.charsize=1.8 & !x.style=1
    !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8 
    th=3.5
	!p.multi=[0,1,2]
	
	nul=min(abs(lambda-wvls[0]),nv)
	
  plot, angs,spec[*,nv],color=line_color[0],ytitle='normalized counts',xtitle='angle (degrees)',$
   title='Angular calibration',charsize=1.5,thick=line_thick[0],/xs,/ys,$ 
   xrange=[0,max(angs)],yrange=[0,1.0], linestyle=line_style[0]
  oplot, [FWHM[nv],FWHM[nv]]/2.,[0,1],color=line_color[0], linestyle=1,thick=2
  legend_tit=[strtrim(wvls[0],2)]
  
  for k=1,n_elements(wvls)-1 do begin
    nul=min(abs(lambda-wvls[k]),nv)
    oplot, angs,spec[*,nv],color=line_color[k],thick=line_thick[k],linestyle=line_style[k]
    oplot, [FWHM[nv],FWHM[nv]]/2.,[0,1],color=line_color[k], linestyle=1,thick=2
    legend_tit=[legend_tit,strtrim(wvls[k],2)]
  endfor
  legend,legend_tit,textcolors=line_color,linestyle=line_style,thick=line_thick,color=line_color,box=0,/right,pspacing=1
  
  plot, lambda, FWHM, title='FWHM per wavelenght', ytitle='FWHM', xtitle='Wavelength (nm)', yrange=[0,max(angs)], xrange=[350,1700]
  
  if ps then begin
    device, /close
    spawn, 'convert "'+path+l+'ang.ps" "'+path+l+'ang.png"'
    spawn, 'rm -f "'+path+l+'ang.ps"'
  endif else begin
    p=tvrd(true=1)
    write_png,path+l+'ang.png',p
  endelse
stop
end
