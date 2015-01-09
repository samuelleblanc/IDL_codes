;+
; NAME:
;   rtm_aero_unique
;
; PURPOSE:
;   to present results from the unique runs in arctas_rtm_aero
;
; CATEGORY:
;   Aerosol retrieval, ARCTAS, unique
;
; CALLING SEQUENCE:
;   rtm_aero_unique
; 
;
; OUTPUT:
;   plots
;   values in a save file
;
; KEYWORDS:
;   - none
;
; DEPENDENCIES:
;   
;   
; NEEDED FILES:
;   - rtm_unique file
;
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
; Written:  Samuel LeBlanc, LASP CU Boulder, March 24th, 2011, Ottawa, Ontario, Canada
; Modified: April 27th, 2011 by Samuel LeBlanc
;           - added plotting of multiple unique runs at once.
;          
;---------------------------------------------------------------------------


pro rtm_aero_unique

one=1


dir='/home/leblanc/libradtran/output/aero/'
dirout='/home/leblanc/rtm_aero/data/'
ver='_v17'
;put in manually the original optical depth measured at index 56
;tau_original=[0.705700,0.675900,0.568800,0.503500,0.481900,0.377400,0.308200,0.238100,0.190500,0.137200,0.0923000,0.0535000,0.0379000]
wvl_original=[353.500,380.000,451.200,499.400,520.400,605.800,675.100,779.100,864.500,1019.10,1241.30,1558.50,2139.10]

if one then begin
  dat=read_ascii(dirout+'rtm_unique_20080709'+ver+'.txt',data_start=1) ;read in the unique run
  
  wvl=dat.field01[2,*]
  num=n_elements(wvl)
  lat=fltarr(13,num/13)
  lon=lat & ssa=lat & asy=lat & asy2=lat & albedo=lat & tau_rtm=lat & tau_input=lat & wvl=lat & tau_original=lat
  
  for ii=0,num/13-1  do begin  ; seperate the unique run output
    for i=0,12 do begin
     lat[i,ii]=dat.field01[0,(ii*13)+i]
     lon[i,ii]=dat.field01[1,(ii*13)+i]
     wvl[i,ii]=dat.field01[2,(ii*13)+i]
     ssa[i,ii]=dat.field01[3,(ii*13)+i]
     asy[i,ii]=dat.field01[4,(ii*13)+i]
     asy2[i,ii]=dat.field01[5,(ii*13)+i]
     albedo[i,ii]=dat.field01[6,(ii*13)+i]
     tau_rtm[i,ii]=dat.field01[8,(ii*13)+i]
     tau_original[i,ii]=dat.field01[10,(ii*13)+i]
     tau_input[i,ii]=dat.field01[9,(ii*13)+i] * tau_original[i,ii]
     
    endfor
  endfor
  nul=where(ssa lt 0.01, ct) 
  if ct gt 0 then begin
   tau_rtm[nul]=!values.f_nan
   tau_input[nul]=!values.f_nan
  endif
  
  save, lat, lon, wvl, ssa, asy, asy2, albedo, tau_rtm, tau_input, tau_original, filename=dirout+'rtm_unique'+ver+'.out'  ;save the unique run
  
  ; plot the unique run output at each wavelengths
  set_plot, 'ps'
  loadct, 39,/silent
  
     device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8.
     device, filename=dirout+'rtm_unique.ps'
     device, xsize=20, ysize=20
     !p.font=1 & !p.thick=5
     !p.charsize=1.8 & !x.style=1
     !y.style=1 & !z.style=1
     !y.thick=1.8 & !x.thick=1.8
     !x.margin=[6,4]
     !p.multi=0
     cl=findgen(13)*255/13
     plot, tau_rtm[0,*],tau_input[0,*], title='Retrieved optical depth from varrying optical depth input',ytitle='Input optical depth', xtitle='Retrieved optical depth', psym=4, color=cl[0], yrange=[0,2.],xrange=[0,1.5]
     oplot, [tau_original[0,0],tau_original[0,0]],[0,2],color=cl[0], thick=2
     for i=1, 12 do begin
      oplot, [tau_original[i,0],tau_original[i,0]],[0,2],color=cl[i], thick=2
  	oplot, tau_rtm[i,*],tau_input[i,*], psym=4, color=cl[i]
     endfor
     
     legend, string(wvl_original,format='(F6.1)'),textcolors=cl, box=0
     device, /close
  spawn, 'convert '+dirout+'rtm_unique.ps '+dirout+'rtm_unique'+ver+'.png'
  spawn, 'rm -f '+dirout+'rtm_unique.ps'
  
endif else begin ; start plotting for mutiple unique runs
;lat, lon, wvl, ssa, asy, asy2, albedo, tau_rtm, tau_input, tau_original,
  f=[file_search(dirout+'rtm_unique_v[5-9].out'),file_search(dirout+'rtm_unique_v??.out')]
  num_f=n_elements(f)
  nans=replicate(!values.f_nan, 13,51)
  num_n=n_elements(nans)
  lat_arr=nans & lon_arr=nans & wvl_arr=nans & ssa_arr=nans & asy_arr=nans & asy2_arr=nans & albedo_arr=nans & tau_rtm_arr=nans & tau_input_arr=nans & tau_org_arr=nans
  for i=0, num_f-1 do begin
    restore, f[i]
    
    if n_elements(lat) lt num_n then begin
      ss=size(lat,/dimensions)
      lats=nans
      for ii=0, ss[0]-1 do for jj=0, ss[1]-1 do lats[ii,jj]=lat[ii,jj]
    endif else lats=lat
    lat_arr=[[[lat_arr]],[[lats]]]
    
    if n_elements(lon) lt num_n then begin
      ss=size(lon,/dimensions)
      lons=nans
      for ii=0, ss[0]-1 do for jj=0, ss[1]-1 do lons[ii,jj]=lon[ii,jj]
    endif else lons=lon
    lon_arr=[[[lon_arr]],[[lons]]]
    
    if n_elements(wvl) lt num_n then begin
      ss=size(wvl,/dimensions)
      wvls=nans
      for ii=0, ss[0]-1 do for jj=0, ss[1]-1 do wvls[ii,jj]=wvl[ii,jj]
    endif else wvls=wvl
    wvl_arr=[[[wvl_arr]],[[wvls]]]

    if n_elements(ssa) lt num_n then begin
      ss=size(ssa,/dimensions)
      ssas=nans
      for ii=0, ss[0]-1 do for jj=0, ss[1]-1 do ssas[ii,jj]=ssa[ii,jj]
    endif else ssas=ssa
    ssa_arr=[[[ssa_arr]],[[ssas]]]
    
     if n_elements(asy) lt num_n then begin
      ss=size(asy,/dimensions)
      asys=nans
      for ii=0, ss[0]-1 do for jj=0, ss[1]-1 do asys[ii,jj]=asy[ii,jj]
    endif else asys=asy
    asy_arr=[[[asy_arr]],[[asys]]]
    
    if n_elements(asy2) lt num_n then begin
      ss=size(asy2,/dimensions)
      asy2s=nans
      for ii=0, ss[0]-1 do for jj=0, ss[1]-1 do asy2s[ii,jj]=asy2[ii,jj]
    endif else asy2s=asy2
    asy2_arr=[[[asy2_arr]],[[asy2s]]]
    
    if n_elements(albedo) lt num_n then begin
      ss=size(albedo,/dimensions)
      albedos=nans
      for ii=0, ss[0]-1 do for jj=0, ss[1]-1 do albedos[ii,jj]=albedo[ii,jj]
    endif else albedos=albedo
    albedo_arr=[[[albedo_arr]],[[albedos]]]
    
    if n_elements(tau_rtm) lt num_n then begin
      ss=size(tau_rtm,/dimensions)
      tau_rtms=nans
      for ii=0, ss[0]-1 do for jj=0, ss[1]-1 do tau_rtms[ii,jj]=tau_rtm[ii,jj]
    endif else tau_rtms=tau_rtm
    tau_rtm_arr=[[[tau_rtm_arr]],[[tau_rtms]]]
    
    if n_elements(tau_input) lt num_n then begin
      ss=size(tau_input,/dimensions)
      tau_inputs=nans
      for ii=0, ss[0]-1 do for jj=0, ss[1]-1 do tau_inputs[ii,jj]=tau_input[ii,jj]
    endif else tau_inputs=tau_input
    tau_input_arr=[[[tau_input_arr]],[[tau_inputs]]]
    
    if n_elements(tau_original) lt num_n then begin
      ss=size(tau_original,/dimensions)
      tau_originals=nans
      for ii=0, ss[0]-1 do for jj=0, ss[1]-1 do tau_originals[ii,jj]=tau_original[ii,jj]
    endif else tau_originals=tau_original
    tau_org_arr=[[[tau_org_arr]],[[tau_originals]]]
  endfor
  lat_arr=lat_arr[*,*,1:*] & lon_arr=lon_arr[*,*,1:*] & wvl_arr=wvl_arr[*,*,1:*] & ssa_arr=ssa_arr[*,*,1:*] & asy_arr=asy_arr[*,*,1:*]
  asy2_arr=asy2_arr[*,*,1:*] & albedo_arr=albedo_arr[*,*,1:*] & tau_rtm_arr=tau_rtm_arr[*,*,1:*] & tau_input_arr=tau_input_arr[*,*,1:*] & tau_org_arr=tau_org_arr[*,*,1:*]
  ; now plot all the different unique runs together
  for ii=0, 12 do begin ; loop through all wavelengths
    
    set_plot, 'ps'
    loadct, 39,/silent
    
       device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8.
       device, filename=dirout+'rtm_unique.ps'
       device, xsize=20, ysize=20
       !p.font=1 & !p.thick=5
       !p.charsize=1.8 & !x.style=1
       !y.style=1 & !z.style=1
       !y.thick=1.8 & !x.thick=1.8
       !x.margin=[6,4]
       !p.multi=0
       
       plot, transpose(tau_rtm_arr[ii,*,*]) , psym=2, title='Optical depth for each test points at '+string(round(wvl_original[ii]),format='(I4)')+' nm',xtitle='Test points',ytitle='Optical depth ratio',yrange=[0,2.5],xrange=[0,num_f],/nodata
       oplot, [0,num_f-1],[1,1],thick=0.2, linestyle=3
       for io=0,num_f-1 do begin
         oplot,replicate(io,51), tau_rtm_arr[ii,*,io]/tau_org_arr[ii,*,io], psym=2, color=70,symsize=2
         oplot,replicate(io,51), tau_org_arr[ii,*,io]/tau_org_arr[ii,*,io],  psym=4, color=250,symsize=2
       endfor
       legend,['Retrieved optical depth', 'Measured optical depth'],box=0,textcolors=[70,250],color=[70,250],psym=[2,4]
       device, /close
    spawn, 'convert '+dirout+'rtm_unique.ps '+dirout+'rtm_unique_all_wvl_'+string(round(wvl_original[ii]),format='(I04)')+'.png'
    spawn, 'rm -f '+dirout+'rtm_unique.ps'
  endfor
endelse
stop
end
