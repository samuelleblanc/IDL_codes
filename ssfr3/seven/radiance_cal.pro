;+
; NAME:
;   radiance_cal
;
; PURPOSE:
;   Read in the data for calibrations of the radiance light collector, make response functions from primary calibration 
;
; CATEGORY:
;   Radiance / Calibration
;
; CALLING SEQUENCE:
;   radiance_cal,/ps
;   
; OUTPUT:
;   - Plots of the calibration
;
; KEYWORDS:
;   ps - to output plots to ps instead of the screen   
;
; DEPENDENCIES:
;   cfg.pro       ;config file cheker
;   legend.pro    ;to make legend on graph
;   ingest_ssfr.pro ;to read the files from SSFR
;   get_wvl.pro   ; to build the wavelength arrays
;   ingest_temp.pro ; to get the temperatures from the SSFR files
;   
; NEEDED FILES:
;   - config file for calib
;   - all ".OSA" files for the calibs under the proper directory (/SSFR3/cal/*date*/p3/(*lamp number*)/(cal or dark))
;   - temp.dat for SSFR3
;  
; EXAMPLE:
;   radiance_cal
;
; MODIFICATION HISTORY:
; Written:  Samuel LeBlanc, LASP CU Boulder, Monday, April 23rd, 2012
; Modified: 
;
;---------------------------------------------------------------------------
@ingest_ssfr.pro
@legend.pro
@cfg.pro
@get_wvl.pro
@ingest_temp.pro


pro radiance_cal, ps=ps
if not keyword_set(ps) then ps=0

path='/home/leblanc/DC3_SEAC4RS/cal/SSFR3/'
l   ='/'                         ; directory separator
cfg_file=path+l+'ssfr3_rad.cfg'


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

months = ["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"]

np                    = fix(cfg(cfg_file,'np'  ))    ; number of channels for each spectrum
innp_used             = fix(cfg(cfg_file,'innp_used'))    ; number of channels used in the IR

platform              = cfg(cfg_file,'platform')
data                  = cfg(cfg_file,'data')    ; data file extension
port                  = cfg(cfg_file,'port')    ; which connection is used
fov                   = float(cfg(cfg_file,'Field_of_View')) ; put in the field of view of the light collector (in steradians)
if port eq 'zenith' then zen=1 else zen=0
if port eq 'nadir' then nad=1 else nad=0
nist_traceable_source = strsplit(cfg(cfg_file,'nist_traceable_source'),', ',escape='#',/EXTRACT)
spectralon            = strsplit(cfg(cfg_file,'spectralon_reflectance'),', ',escape='#',/EXTRACT)
primary_lamp          = strsplit(cfg(cfg_file,'primary_lamp') ,', ',escape='#',/extract)  
cal_date              = strsplit(cfg(cfg_file,'cal_date') ,', ',escape='#',/extract)  ; array of primary lamp

ref                   = cfg(cfg_file,'reference_date')
line_color            = strsplit(cfg(cfg_file,'line_color'),', ',escape='#',/extract)
line_style            = strsplit(cfg(cfg_file,'line_style'),', ',escape='#',/extract)
line_thick            = strsplit(cfg(cfg_file,'line_thick'),', ',escape='#',/extract)

line_color = float(line_color[sort(cal_date)])
line_style = float(line_style[sort(cal_date)])
line_thick = float(line_thick[sort(cal_date)])

num = n_elements(cal_date)
if num gt 1 then print, 'Multiple Calibration dates, Plotting all for comparison, Last cal date will be used for response function'

; read file location for primary, transfer, and integration times
nadir_si      = strsplit(cfg(cfg_file,'nadir_si')   ,', ',escape='#',/EXTRACT)
nadir_ir      = strsplit(cfg(cfg_file,'nadir_ir')   ,', ',escape='#',/EXTRACT)
zenith_si     = strsplit(cfg(cfg_file,'zenith_si')  ,', ',escape='#',/EXTRACT)
zenith_ir     = strsplit(cfg(cfg_file,'zenith_ir')  ,', ',escape='#',/EXTRACT)

zsi_intime_p=lonarr(num)
zir_intime_p=lonarr(num)
nsi_intime_p=lonarr(num)
nir_intime_p=lonarr(num)

for ii=0, num-1 do begin
  zsi_intime_p[ii] = long(zenith_si[2*ii+1])  & zenith_si[ii] = zenith_si[2*ii]
  zir_intime_p[ii] = long(zenith_ir[2*ii+1])  & zenith_ir[ii] = zenith_ir[2*ii]
  nsi_intime_p[ii] = long(nadir_si[2*ii+1])   & nadir_si[ii]  = nadir_si[2*ii]
  nir_intime_p[ii] = long(nadir_ir[2*ii+1])   & nadir_ir[ii]  = nadir_ir[2*ii]
endfor

zenith_si=zenith_si[0:num-1]
zenith_ir=zenith_ir[0:num-1]
nadir_si=nadir_si[0:num-1]
nadir_ir=nadir_ir[0:num-1]

ref=where(ref eq cal_date)  ;find where the cal date is equal to the reference date
ref=ref[0]

resp_func_dir = cfg(cfg_file,'resp_func_dir')+l

dozenith = cfg(cfg_file,'dozenith')
;donadir  = cfg(cfg_file,'donadir')
write_primary  = cfg(cfg_file,'write_primary')

; definitions
; 0 - ZSI, 1 - ZIR, 2 - NSI, 3 - NIR
dark  =fltarr(np,4)
cali  =fltarr(np,4)
resp  =fltarr(np,4) ; primary response function
resp_arr=fltarr(np,4,num) ; array of primary response functions
cali_arr=fltarr(np,4,num) ; array of calibrations for primary
dark_arr=fltarr(np,4,num) ; array of darks for primary
spec_arr=fltarr(np,4,num)
lamp_arr=fltarr(np,4,num)

lamp_spec = fltarr(np,4) ; lamp splined spectra
refl_spec = fltarr(np,4) ; reflectance from the spectralon

;build the wavelength arrays
get_wvl, cfg_file, wvl, zenlambda, nadlambda, indices,/reverse

;start the large loop over all cal_dates
for i=0, num-1 do begin

  ; now get the lamp spectra
  restore, nist_traceable_source[i]
  case primary_lamp[i] of
   '506C': begin
     spec_p=specir506/100.
     wlp=lambda506
   end
   '765C': begin
     spec_p=specir765/100.
     wlp=lambda765
   end
   else: message,'Primary lamp not recognised'
  endcase
  
  ;now interpolate the lamp sepctra to the wvls
  print,'Warning, the lamp splining should be replaced by Planck fitting!'
  lamp_spec[*,0] = spline(wlp,spec_p,wvl[*,0]) ; spline lamp response to SSFR wavelengths
  lamp_spec[*,1] = spline(wlp,spec_p,wvl[*,1])
  lamp_spec[*,2] = spline(wlp,spec_p,wvl[*,2]) ; spline lamp response to SSFR wavelengths
  lamp_spec[*,3] = spline(wlp,spec_p,wvl[*,3])
  
  ;get the reflectance of the spectralon and then interpolate
  sp=read_ascii(spectralon[i], comment_symbol='#')
  refl_spec[*,0] = interpol(sp.field1[1,*],sp.field1[0,*],wvl[*,0])
  refl_spec[*,1] = interpol(sp.field1[1,*],sp.field1[0,*],wvl[*,1])
  refl_spec[*,2] = interpol(sp.field1[1,*],sp.field1[0,*],wvl[*,2])
  refl_spec[*,3] = interpol(sp.field1[1,*],sp.field1[0,*],wvl[*,3])

  ; now read the SSFR files
  
  ;do the Zenith Silicon
  ;get the cal
  file=file_search(zenith_si[i]+l+'cal'+l+data, count = numfiles, /FOLD_CASE)
  if (numfiles ne 1) then message,'There should be exactly 1 file for the zenith silicon cal.'
  ingest_ssfr,file,cal_date[i],ssfr_utc,raw,drk,ch,int,n_drk,err,n_sat,n_ctr,/ssfr3,/avg
  cali[*,0]=raw[*,0]
  if int[0] ne zsi_intime_p[i] and zen then message,'Integration times are wrong in zenith silicon'
    
  ;get the dark
  file=file_search(zenith_si[i]+l+'dark'+l+data, count = numfiles, /FOLD_CASE)
  if (numfiles ne 1) then message,'There should be exactly 1 file for the zenith silicon dark.'
  ingest_ssfr,file,cal_date[i],ssfr_utc,raw,drk,ch,int,n_drk,err,n_sat,n_ctr,/ssfr3,/avg
  dark[*,0]=raw[*,0]
  if int[0] ne zsi_intime_p[i] and zen then message,'Integration times are wrong in zenith silicon'
  
  ;do the Zenith InGaAs
  ;get the cal
  file=file_search(zenith_ir[i]+l+'cal'+l+data, count = numfiles, /FOLD_CASE)
  if (numfiles ne 1) then message,'There should be exactly 1 file for the zenith ingaas cal.'
  ingest_ssfr,file,cal_date[i],ssfr_utc,raw,drk,ch,int,n_drk,err,n_sat,n_ctr,/ssfr3,/avg
  cali[*,1]=raw[*,1]
  if int[1] ne zir_intime_p[i] and zen then message,'Integration times are wrong in zenith ingaas'
    
  ;get the dark
  file=file_search(zenith_ir[i]+l+'dark'+l+data, count = numfiles, /FOLD_CASE)
  if (numfiles ne 1) then message,'There should be exactly 1 file for the zenith ingaas dark.'
  ingest_ssfr,file,cal_date[i],ssfr_utc,raw,drk,ch,int,n_drk,err,n_sat,n_ctr,/ssfr3,/avg
  dark[*,1]=raw[*,1]
  if int[1] ne zir_intime_p[i] and zen then message,'Integration times are wrong in zenith ingaas'
  
  ;do the Nadir Silicon
  ;get the cal
  file=file_search(nadir_si[i]+l+'cal'+l+data, count = numfiles, /FOLD_CASE)
  if (numfiles ne 1) then message,'There should be exactly 1 file for the nadir silicon cal.'
  ingest_ssfr,file,cal_date[i],ssfr_utc,raw,drk,ch,int,n_drk,err,n_sat,n_ctr,/ssfr3,/avg
  cali[*,2]=raw[*,2]
  if int[2] ne nsi_intime_p[i] and nad then message,'Integration times are wrong in nadir silicon'
    
  ;get the dark
  file=file_search(nadir_si[i]+l+'dark'+l+data, count = numfiles, /FOLD_CASE)
  if (numfiles ne 1) then message,'There should be exactly 1 file for the nadir silicon dark.'
  ingest_ssfr,file,cal_date[i],ssfr_utc,raw,drk,ch,int,n_drk,err,n_sat,n_ctr,/ssfr3,/avg
  dark[*,2]=raw[*,2]
  if int[2] ne nsi_intime_p[i] and nad then message,'Integration times are wrong in nadir silicon'
  
  ;do the Nadir InGaAs
  ;get the cal
  file=file_search(nadir_si[i]+l+'cal'+l+data, count = numfiles, /FOLD_CASE)
  if (numfiles ne 1) then message,'There should be exactly 1 file for the nadir ingaas cal.'
  ingest_ssfr,file,cal_date[i],ssfr_utc,raw,drk,ch,int,n_drk,err,n_sat,n_ctr,/ssfr3,/avg
  cali[*,3]=raw[*,3]
  if int[3] ne nir_intime_p[i] and nad then message,'Integration times are wrong in nadir ingaas'
    
  ;get the dark
  file=file_search(nadir_si[i]+l+'dark'+l+data, count = numfiles, /FOLD_CASE)
  if (numfiles ne 1) then message,'There should be exactly 1 file for the nadir ingaas dark.'
  ingest_ssfr,file,cal_date[i],ssfr_utc,raw,drk,ch,int,n_drk,err,n_sat,n_ctr,/ssfr3,/avg
  dark[*,3]=raw[*,3]
  if int[3] ne nir_intime_p[i] and nad then message,'Integration times are wrong in nadir ingaas'
  
  int_times=[zsi_intime_p[i],zir_intime_p[i],nsi_intime_p[i],nir_intime_p[i]]
  ;now determine the response functions
  spec=cali-dark
  for ii=0, 3 do resp[*,ii]=spec[*,ii]*!dpi/lamp_spec[*,ii]/refl_spec[*,ii] ;make the response functions 
   
  lamp_arr[*,*,i]=lamp_spec
  spec_arr[*,*,i]=spec 
  resp_arr[*,*,i]=resp
  cali_arr[*,*,i]=cali
  dark_arr[*,*,i]=dark
endfor

;now write response function to file 

if port eq 'zenith' then begin
  print, 'Radiance head is connected to Zenith'
  resp_si_file = resp_func_dir + cal_date[num-1] + '_'+primary_lamp[num-1]+'_resp_'+strcompress(string(zsi_intime_p[num-1]),/REMOVE_ALL)+'_zensi.dat'
  resp_ir_file = resp_func_dir + cal_date[num-1] + '_'+primary_lamp[num-1]+'_resp_'+strcompress(string(zir_intime_p[num-1]),/REMOVE_ALL)+'_zenir.dat'
  tmp=file_search(resp_si_file,count=count)
  ;;if(count gt 0) then message, resp1_si_file + ' already exists'
  tmp=file_search(resp_ir_file,count=count)
  ;;if(count gt 0) then message, resp1_ir_file + ' already exists'
  
  openw,10,resp_si_file
  openw,11,resp_ir_file
  for i=0,np-1 do begin
    printf,10,wvl[i,0],resp[i,0]
    printf,11,wvl[i,1],resp[i,1]
  endfor
  close,10
  close,11
  
  
  ;plot the response functions
  if ps then begin
    print, 'in the zenith ps'
    device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=path+l+'zenith.ps'
    device, xsize=20, ysize=40
    !p.font=1 & !p.thick=5
    !p.charsize=1.8 & !x.style=1
    !y.style=1 & !z.style=1
    !y.thick=1.8 & !x.thick=1.8 
     th=3.5
  endif else begin
    window,0,tit='zenith', xsize=500, ysize=1000
    th=1.8
  endelse
  !p.multi=[0,1,2]
  ymax = max([max(resp_arr(*,0,*)),max(resp_arr(*,1,*))])*1.04
  plot, wvl[*,0],resp_arr(*,0,0),color=line_color[0],ytitle='counts/ms W!E-1!N m!E2!N nm',xtitle='Wavelength (nm)',$
   title='Zenith'+' lamp: '+primary_lamp[num-1]+' si: '+strtrim(zsi_intime_p[num-1],2)+' in: '+strtrim(zir_intime_p[num-1],2) ,$
   charsize=1.5,thick=line_thick[0],$ 
   /xs,/ys,xrange=[300,2200],yrange=[0,ymax], linestyle=line_style[0]
  oplot,wvl[*,1],resp_arr(*,1,0),thick=line_thick[0],color=line_color[0], linestyle=line_style[0]
  legend_tit=['Primary Cal - '+  cal_date[0]]
  for i=1,num-1 do begin
    oplot,wvl[*,0],resp_arr(*,0,i),thick=line_thick[i],color=line_color[i]
    oplot,wvl[*,1],resp_arr(*,1,i),thick=line_thick[i],color=line_color[i]
    legend_tit=[legend_tit,'Primary Cal - '+cal_date[i]]
  endfor
  legend,legend_tit,textcolors=line_color,linestyle=line_style,color=line_color,thick=line_thick,pspacing=1,/right,box=0
    
  plot, wvl[*,0],resp_arr[*,0,ref],color=0,ytitle='%',xtitle='Wavelength (nm)',title='Percent Difference of response function !C Reference:'+$
   cal_date[ref]+' - Zenith',charsize=1.5,thick=th,/xs,/ys,xrange=[300,2200],yrange=[-20,20], /nodata
  leg_tit= '0'
  for i=0, num-1 do begin
    oplot,wvl[*,0],(resp_arr[*,0,i]-resp_arr[*,0,ref])*100./resp_arr[*,0,ref],thick=line_thick[i],color=line_color[i],linestyle=line_style[i]
    oplot,wvl[*,1],(resp_arr[*,1,i]-resp_arr[*,1,ref])*100./resp_arr[*,1,ref],thick=line_thick[i],color=line_color[i],linestyle=line_style[i]
    leg_tit=[leg_tit, cal_date[i]]
  endfor
  legend,['Primary',leg_tit[1:*]],textcolors=[0,line_color],/right,box=0
  if ps then begin
    device, /close
    spawn, 'convert "'+path+l+'zenith.ps" "'+path+l+'zenith.png"'
    spawn, 'rm -f "'+path+l+'zenith.ps"'
  endif else begin
    p=tvrd(true=1)
    write_png,path+l+'zenith.png',p
  endelse
  
endif else begin ;else of the port if
  print, 'Radiance head is connected to Nadir'
  resp_si_file = resp_func_dir + cal_date[num-1] + '_'+primary_lamp[num-1]+'_resp_'+strcompress(string(nsi_intime_p[num-1]),/REMOVE_ALL)+'_nadsi.dat'
  resp_ir_file = resp_func_dir + cal_date[num-1] + '_'+primary_lamp[num-1]+'_resp_'+strcompress(string(nir_intime_p[num-1]),/REMOVE_ALL)+'_nadir.dat'
  tmp=file_search(resp_si_file,count=count)
  ;;if(count gt 0) then message, resp1_si_file + ' already exists'
  tmp=file_search(resp_ir_file,count=count)
  ;;if(count gt 0) then message, resp1_ir_file + ' already exists'
  
  openw,10,resp_si_file
  openw,11,resp_ir_file
  for i=0,np-1 do begin
    printf,10,wvl[i,2],resp[i,2]
    printf,11,wvl[i,3],resp[i,3]
  endfor
  close,10
  close,11
  
  ;now plot the response functions
  if ps then begin
    print, 'in the nadir ps'
    device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=path+l+'nadir.ps'
    device, xsize=20, ysize=40
  endif else begin
    window,1,tit='nadir', xsize=500, ysize=1000
  endelse
  ymax = max([max(resp_arr(*,2,*)),max(resp_arr(*,3,*))])*1.04
  plot, wvl[*,2],resp_arr(*,2,0),color=line_color[0],ytitle='counts/ms W!E-1!N m!E2!N nm',xtitle='Wavelength (nm)',$
   title='Nadir'+' lamp: '+primary_lamp[num-1]+' si: '+strtrim(nsi_intime_p[num-1],2)+' in: '+strtrim(nir_intime_p[num-1],2),$
   charsize=1.5,thick=line_thick[0],/xs,/ys,$ 
   xrange=[300,2200],yrange=[0,ymax], linestyle=line_style[0]
  oplot,wvl[*,3],resp_arr(*,3,0),thick=line_thick[0],color=line_color[0], linestyle=line_style[0]
  
  legend_tit=['Primary Cal - '+ cal_date[0]]
  
  for i=1,num-1 do begin
    oplot,wvl[*,2],resp_arr(*,2,i),thick=line_thick[i],color=line_color[i],linestyle=line_style[i]
    oplot,wvl[*,3],resp_arr(*,3,i),thick=line_thick[i],color=line_color[i],linestyle=line_style[i]
    legend_tit=[legend_tit,  'Cal - '+cal_date[i]]
  endfor
  legend,legend_tit,textcolors=line_color,linestyle=line_style,thick=line_thick,/right,outline_color=0,pspacing=1
  
  plot, wvl[*,2], resp_arr(*,2,ref),color=0,ytitle='%',xtitle='Wavelength (nm)',$
   title='Percent Difference of response function !C Reference:'+ cal_date[ref]+' - Nadir',charsize=1.5,$
   thick=line_thick[ref],/xs,/ys,xrange=[300,2200],yrange=[-60,20], /nodata
  leg_tit='0'
  for i=0, num-1 do begin
    oplot,wvl[*,2],(resp_arr(*,2,i)-resp_arr(*,2,ref))*100./resp_arr(*,2,ref),thick=line_thick[i],color=line_color[i],linestyle=line_style[i]
    oplot,wvl[*,3],(resp_arr(*,3,i)-resp_arr(*,3,ref))*100./resp_arr(*,3,ref),thick=line_thick[i],color=line_color[i],linestyle=line_style[i]
    leg_tit=[leg_tit, cal_date[i]]
  endfor
  legend,['Primary',leg_tit[1:*]],textcolors=[0,line_color],/right, outline_color=0
  if ps then begin
    device, /close
    spawn, 'convert "'+path+l+'nadir.ps" "'+path+l+'nadir.png"'
    spawn, 'rm -f "'+path+l+'nadir.ps"'
  endif else begin
    p=tvrd(true=1)
    write_png,path+l+'nadir.png',p
  endelse
endelse ; end of the port else

stop
; chekc the ratio of tow lamps
device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=path+l+'lamp.ps'
    device, xsize=20, ysize=40
!p.multi=[0,1,2]
plot, wvl[*,0],spec_arr[*,0,0]/spec_arr[*,0,1], title='Ratio of lamp 506C to 765C',$
 xtitle='Wavelength (nm)',xrange=[350,1700],yrange=[0.7,1.3]
oplot, wvl[*,1],spec_arr[*,1,0]/spec_arr[*,1,1]

oplot, wvl[*,0],lamp_arr[*,0,0]/lamp_arr[*,0,1], color=250
oplot, wvl[*,1],lamp_arr[*,1,0]/lamp_arr[*,1,1], color=250

oplot, [350.,1700.], [1,1],linestyle=1,thick=1.5
legend,['measurement','calibration'],textcolors=[0,250],box=0

plot, wvl[*,0],(spec_arr[*,0,0]/spec_arr[*,0,1])/(lamp_arr[*,0,0]/lamp_arr[*,0,1]), title='Ratio of the ratios',xtitle='Wavelength (nm)',xrange=[350.,1700.],yrange=[0.9,1.1]
oplot, wvl[*,1],(spec_arr[*,1,0]/spec_arr[*,1,1])/(lamp_arr[*,1,0]/lamp_arr[*,1,1])

    device, /close
    spawn, 'convert "'+path+l+'lamp.ps" "'+path+l+'lamp_ratio.png"'
    spawn, 'rm -f "'+path+l+'lamp.ps"'



;double check the blackbody radiation
device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=path+l+'blackbody.ps'
    device, xsize=20, ysize=20
!p.multi=0
plot, wvl[*,0],(spec[*,0])*!dpi/resp[*,0]/refl_spec[*,0], title='blackbody spectrum',$
 xtitle='Wavelength (nm)',xrange=[350,1700],yrange=[0,0.25]
oplot, wvl[*,1],spec[*,1]*!dpi/resp[*,1]/refl_spec[*,0]
    device, /close
    spawn, 'convert "'+path+l+'blackbody.ps" "'+path+l+'blackbody.png"'
    spawn, 'rm -f "'+path+l+'blackbody.ps"'
stop
end
