;+
; NAME:
;   arctas_rtm_aero
;
; PURPOSE:
;   to do the aerosol retrieval on select days of arctas
;
; CATEGORY:
;   ARCTAS, Aerosol
;
; CALLING SEQUENCE:
;   arctas_rtm_aero, date
; - date - date of day to check
;
; OUTPUT:
;   plots
;   retrieved values in a text file
;
; KEYWORDS:
;   - plotting:     to plot irradiance and albedo longitudinal (time lapse)
;   - video:        to make consecutive plots of spectras
;   - divergence:   to make a plot of the flux divergence
;   - tau_scale:    to scale the retrieval by this amount (can also be used while the multi wavelenght mode is on)
;   - only_hsrl:    to turn off the multi wavelength retrieval and only used the hsrl value at 532 (good for testing)
;   - no_tau_loop:  to turn off the tau loop
;   - find_legs:    to find the correct leg intervals
;   - second:       to use the second loop for that day
;   - save:         to make a save file to use further
;   - tau_approx:   to change the tau value to an approximate value; for testing
;   - map:          to make and save a map of the flight path, with points showing the desired area
;   - aeronet:      to read in the aeronet values
;   - no_szacorr:   to not use sza corr in the irradiances
;   - tau_above:    returns tau above the flight legs
;   - constant_ssa: to use the tau retrieval by keeping ssa constant after first loop, speeds up convergence drastically
;   - sensitivity:  to make retrieval not convergence, but save to a file for sensitivity analysis
;   - choose:       to choose the particular points in the legs (use in conjunction with find_legs
;   - angstrom:     to use the angstrom approximation (testing)
;   - error_loop:   to be able to derive errors for tau and others
;   - hsrl:         to use the hsrl during arctas (testing)
;   - unique:       to make retrieval converge but by varrying input optical depth, to solve the entire solution space
;   - hist:         to plot the histogram of irradiances at different wavelengths (net and downwelling)
;   - use_wl:       to use run the retrieval for this wavelength index only
;
; DEPENDENCIES:
;   
;   
; NEEDED FILES:
;   - aats ict file
;   - arctas calibspecs_attcorr files
;
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
; Written:  Samuel LeBlanc, LASP CU Boulder, August 31st, 2010, Ottawa, Ontario, Canada
; Modified: December 8th, 2010 by Samuel LeBlanc
;           - added hsrl reading for arctas
; Modified: March 24th, 2011 by Samuel LeBlanc
;           - added unique run
; Modified: August 2nd, 2011 by Samuel LeBlanc
;           - added the hist keyword
; Modified: September 13th, 2011 by Samuel LeBlanc
;           - added the use_wl keyword to be used on the cluster
;---------------------------------------------------------------------------
@\\lasp-smb\leblanc\arctas\pro\write_input_aats.pro
@write_input_aats.pro
@zensun.pro
@\\lasp-smb\leblanc\libradtran\pro\libradtran_reader.pro
@/home/leblanc/libradtran/pro/libradtran_reader.pro
@read_aeronet.pro
@\\lasp-smb\leblanc\CALNEX\pro\read_aeronet.pro
@get_aats_all.pro
@\\lasp-smb\leblanc\arctas\pro\get_aats_all.pro
@nav_nasa.pro
@\\lasp-smb\leblanc\arctas\pro\nav_nasa.pro
@get_hsrl_arctas

pro arctas_rtm_aero, datein, second=second, save=save, plotting=plotting, video=video, divergence=divergence,$
 tau_scale=tau_scale, only_hsrl=only_hsrl,tau_approx=tau_approx,map=map, aeronet=aeronet,no_szacorr=no_szacorr,$
 no_tau_loop=no_tau_loop, find_legs=find_legs, tau_above=tau_above, constant_ssa=constant_ssa, sensitivity=sensitivity,$
 choose=choose, angstrom=angstrom, error_loop=error_loop, hsrl=hsrl, unique=unique, hist=hist, use_wl=use_wl

close, /all

; set all the right keyword
if !VERSION.OS eq 'linux' then linux=1 else linux=0  ;set windows or linux
aats=1
if not keyword_set(plotting) then plotting=0
if not keyword_set(hist) then hist=0
if not keyword_set(no_tau_loop) then no_tau_loop=0
if keyword_set(second) then secon='_2' else secon=''
if not keyword_set(error_loop) then error_loop=0
if not keyword_set(hsrl) then hsrl=0
if not keyword_set(unique) then unique=0
n_unique=50.
if not keyword_set(tau_scale) then tau_s=1.0 else tau_s=tau_scale  ;at normal 
if not keyword_set(aeronet) then aeronet=0
if not plotting then begin
  video=0
  divergence=0
endif
if not keyword_set(tau_approx) then tau_approx=0 else tau_approx=1
if not keyword_set(video) then video=0
if n_elements(datein) lt 1 then date='20080709' else date=strcompress(string(datein),/REMOVE_ALL)

;set up correct paths
if linux then begin
  arctas_dir='/home/leblanc/arctas/'
  arctas_dir_in='/data/seven/schmidt/polar/'
  ll='/'
  dir='/home/leblanc/libradtran/input/aero/'
  dirout='/home/leblanc/libradtran/output/aero/'
endif else begin
  arctas_dir='\\lasp-smb\leblanc\arctas\'
  ll='\'
  dir='\\lasp-smb\leblanc\libradtran\input\aero\'
  dirout='\\lasp-smb\leblanc\libradtran\output\aero\'
endelse
if not keyword_set(find_legs) then aats_dir=arctas_dir_in+'nasa'+ll else aats_dir='/data/seven/schmidt/polar/nasa/'

;ge the SSFR data, attitude corrected
restore, arctas_dir_in+'nasa'+ll+date+ll+date+'_calibspcs_attcorr.out' ;  restore the data, attitude corrected
utc=tmhrs

; get nav data for the nasa
nav_nasa,arctas_dir_in+'nasa'+ll,date,arctas_dir_in+'nasa'+ll+date+ll+'polar.cfg',utc,alt,lat,lon,dc,sza,iza,lats,lons,label,roll=rol,pitch=pit

;get the aats data
if aats then begin
  ; get aats 
  get_aats_all,aats_dir,date,uth,lambda,tauh,cld,dtauh, coef
    if n_elements(uth) lt 1 then print, '*** No AATS file ***'
  tau_aats=fltarr(n_elements(utc),n_elements(lambda))
  dtau_aats=fltarr(n_elements(utc),n_elements(lambda))
  
  for ju=0, n_elements(utc)-1 do begin
    smm=min(abs(uth/3600.-utc[ju]),jk) ; find the closest in time AATS point and relate it to SSFR time
    for ji=0,n_elements(lambda)-1 do begin
      tau_aats[ju,ji]=tauh[jk,ji]
      dtau_aats[ju,ji]=dtauh[jk,ji]
    endfor
  endfor
endif else begin ; if no aats then set all taus to NaNs
  tau_aats=fltarr(n_elements(utc),13)*!values.f_nan
  dtau_aats=tau_aats
endelse


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Choose region to plot with find legs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
if keyword_set(find_legs) then begin
plotting=1 & divergence=1 & map=1

set_plot, 'x'
device, decomposed=0
loadct, 39, /silent
!p.color=0 & !p.background=255 & !p.multi=0
; initial plot of the map to select the specified region

window, 0, xsize=800, ysize=800, title='map for '+date
  maxlon=max(lon) & maxlat=max(lat) & minlon=min(lon) & minlat=min(lat)
  if minlon lt -180 then minlon=-180
  mlon=(maxlon+minlon)*0.5 & mlat=(minlat+maxlat)*0.5

  map_set,mlat,mlon,/grid,limit=[minlat,minlon,maxlat,maxlon],/label,title='ARCTAS on '+date
  map_continents,/usa,/hires,thick=2
  map_continents,/countries,/hires,/coast,thick=2

  oplot, lon, lat, psym=3, color=250
  ; if we have aats measurements, then we overplot color-coordinated optical depth at a midvisible wavelength
  if aats then begin
    A = FINDGEN(17) * (!PI*2/16.)
    USERSYM, COS(A), SIN(A), /FILL
    for i=0, n_elements(lat)-1 do begin
      cl=tau_aats[i,4]*255/max(tau_aats[*,4],/nan)
      oplot, [lon[i],lon[i]], [lat[i],lat[i]], psym=8, symsize=1, color=cl
    endfor
    colorbar, position=[0.05,0.93,0.96,0.94] ,/right, /vertical, range=[0,max(tau_aats[*,4],/nan)], format='(F6.4)', ytitle='Optical depth at:'+string(lambda[4],format='(F7.2)')
  endif
  print, 'now click the desired start, and end of interest'
  
  ; get the cursor locations
  cursor, x,y,/down
  print, x,y
  mm=min(abs(x-lon)+abs(y-lat), st)
  print, 'now select end of leg region'
  cursor, x1,y1,/down
  print, x1,y1
  mm=min(abs(x1-lon)+abs(y1-lat), en)
  
; now plot the desired location that was selected
!p.multi=[0,2,2] & !y.style=1
window, 1, xsize=800, ysize=800, title='lat vs. lon',xpos=0,ypos=0
if st lt en then pa=where(utc gt utc[st] and utc lt utc[en]) else pa=where(utc gt utc[en] and utc lt utc[st])
plot, lon[pa], lat[pa], title='lat lon of flight track',ytitle='lat', xtitle='lon'
plot, utc[pa], lat[pa], title='lat vs. utc', ytitle='lat', xtitle='utc'
plot, lon[pa], utc[pa], title='utc vs. lon', ytitle='utc', xtitle='lon'
plot, utc[pa], alt[pa], title='altitude', ytitle='alt(m)',xtitle='utc'

;select even more precise the correct legs
print, 'please select start and end of bottom leg'
cursor, x, y,/down
mm=min(abs(x-utc),l1_st)
print, 'utc start at:', utc[l1_st]

cursor, x,y,/down
mm=min(abs(x-utc),l1_en)
print, 'utc end at:', utc[l1_en]

print, 'select start and end of top leg'
cursor, x,y,/down
mm=min(abs(x-utc),l2_st)
print, 'utc next start at:', utc[l2_st] 

cursor, x,y,/down
mm=min(abs(x-utc),l2_en)
print, 'utc next end at:', utc[l2_en]

leg1=where(utc gt utc[l1_st] and utc lt utc[l1_en])
leg2=where(utc gt utc[l2_st] and utc lt utc[l2_en])

endif else begin

; use predefined legs
; make above and below zenith and nadir values
case date of
'20080408': begin
leg1=where(utc gt 16.727778 and utc lt 16.876389)
leg2=where(utc gt 15.876944 and utc lt 16.298333)
end

'20080409': begin
leg1=where(utc gt 20.796 and utc lt 21.31)
leg2=where(utc gt 21.84 and utc lt 22.13)
end

'20080415':begin
leg1=where(utc gt 19.89 and utc lt 20.39)
leg2=where(utc gt 20.57 and utc lt 20.95)
if keyword_set(second) then begin
  leg1=where(utc gt 25.936 and utc lt 26.25)
  leg2=where(utc gt 26.339 and utc lt 26.577)
endif
end

'20080709':begin
leg1=where(utc gt 19.613 and utc lt 19.686)
leg2=where(utc gt 19.245 and utc lt 19.343)
end

'20080707':begin
leg1=where(utc gt 23.05 and utc lt 23.14)
leg2=where(utc gt 22.73 and utc lt 22.82)
end

'20080630':begin
leg1=where(utc gt 21.04 and utc lt 21.19)
leg2=where(utc gt 20.81 and utc lt 20.94)
if keyword_set(second) then begin
  leg1=where(utc gt 21.74 and utc lt 21.99)
  leg2=where(utc gt 21.47 and utc lt 21.65)
endif
doy=182
end

else: begin
print, 'date not programmed yet'
end

endcase
endelse

; get hsrl data
if hsrl then begin
  get_hsrl_arctas, arctas_dir+ll+'nasa'+ll, date, utc_hslr, lat_hsrl, lon_hsrl, n_hsrl, z_o_hsrl, dz_hsrl, z_n_hsrl, ext532
  fl_hsrl=where(lat_hsrl le max(lat[leg1],/nan) and lat_hsrl ge min(lat[leg1],/nan) and lon_hsrl le max(lon[leg1],/nan) and lon_hsrl ge min(lon[leg1],/nan),gg)  
  if gg lt 1 then message, 'No hsrl points found'
endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; choosing appropriate points to do retrieval over the entire legs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; level leg filter, for both top and bottom of legs
gradient_alt1 = abs(alt[leg1]-shift(alt[leg1],1))
gradient_alt2 = abs(alt[leg1]-shift(alt[leg1],2))
flt =where(gradient_alt1 lt 5. and gradient_alt2 lt 5.)

gradient2_alt1 = abs(alt[leg2]-shift(alt[leg2],1))
gradient2_alt2 = abs(alt[leg2]-shift(alt[leg2],2))
flt2=where(gradient2_alt1 lt 5. and gradient2_alt2 lt 5.)

leg1=leg1[flt]
leg2=leg2[flt2]
  
; filter of high roll and pitch angles
ang_high=5.0
  
flt_ang  = where(abs(rol[leg1]) lt ang_high and abs(pit[leg1]) lt ang_high)
flt_ang2 = where(abs(rol[leg2]) lt ang_high and abs(pit[leg2]) lt ang_high)
leg1=leg1[flt_ang]
leg2=leg2[flt_ang2]

; determination of the nearest distance from leg2 to leg1
; writing out the distance
near_leg2=leg1*0
near_hsrl=leg1*0.
r_d=leg1*0.0
zabove=zspectra[*,leg1]*0.0
zbelow=zspectra[*,leg1]*0.0
nabove=nspectra[*,leg1]*0.0
nbelow=nspectra[*,leg1]*0.0

for i=0, n_elements(leg1)-1 do begin
    ; for second leg onto first
    r=100000.0
    for j=0, n_elements(leg2)-1 do begin
      distance = map_2points(lon[leg1[i]],lat[leg1[i]],lon[leg2[j]],lat[leg2[j]],/meters)
      if distance[0] le r then begin
        near_leg2[i]=leg2[j]
        r=distance[0]
        r_d[i]=distance[0]
      endif
    endfor
	if hsrl then begin
	  r=100000.0
	  for j=0, n_elements(fl_hsrl)-1 do begin
	    distance = map_2points(lon[leg1[i]],lat[leg1[i]],lon_hsrl[fl_hsrl[j]],lat_hsrl[fl_hsrl[j]],/meters)
        if distance[0] le r then begin
          near_hsrl[i]=fl_hsrl[j]
          r=distance[0]
        endif
	  endfor
	endif
	
	; do the sza correction to normalize for a middle point (first of second leg)
   sza_ref=min(sza[leg2]) 
   if keyword_set(no_szacorr) then zabove[*,i]=zspectra[*,near_leg2[i]] else zabove[*,i]=zspectra[*,near_leg2[i]]*cos(sza_ref*!dtor)/cos(sza[near_leg2[i]]*!dtor)
    nabove[*,i]=nspectra[*,near_leg2[i]]
   ;print, 'correction factor above leg:' , cos(sza[near_leg2[0]]*!dtor)/cos(sza[near_leg2[i]]*!dtor)
   ;print, 'sza ref:', sza[near_leg2[0]]
   if keyword_set(no_szacorr) then zbelow[*,i]=zspectra[*,leg1[i]] else zbelow[*,i]=zspectra[*,leg1[i]]*cos(sza_ref*!dtor)/cos(sza[leg1[i]]*!dtor)
    nbelow[*,i]=nspectra[*,leg1[i]] 
   ;print, 'correction factor below leg:' , cos(40.*!dtor)/cos(sza[leg1[i]]*!dtor)	    
endfor

if not keyword_set(no_szacorr) then print, 'sza factor:',cos(sza_ref*!dtor)/cos(sza[leg1[0]]*!dtor)
;sza_ref=40.0

;plot a histogram of the net irradiance and downwelling irradiance for top and bottom of layer
if hist then begin
  zabove_org=zspectra[*,leg2]
  nabove_org=nspectra[*,leg2]
  nul=min(abs(nadlambda-1600.),wl_i)
  anet_1200=histogram(zabove_org[wl_i,*]-nabove_org[wl_i,*],nbins=50,max=0.5,min=0)
  bnet_1200=histogram(zbelow[wl_i,*]-nbelow[wl_i,*],nbins=50,max=0.5,min=0)
  ad_1200=histogram(zabove_org[wl_i,*],nbins=50,max=1.0,min=0)
  bd_1200=histogram(zbelow[wl_i,*],nbins=50,max=1.0,min=0)
  nul=min(abs(nadlambda-500.),wl_i)
  anet_900=histogram(zabove_org[wl_i,*]-nabove_org[wl_i,*],nbins=50,max=1.0,min=0)
  bnet_900=histogram(zbelow[wl_i,*]-nbelow[wl_i,*],nbins=50,max=1.0,min=0)
  ad_900=histogram(zabove_org[wl_i,*],nbins=50,max=1.0,min=0)
  bd_900=histogram(zbelow[wl_i,*],nbins=50,max=1.0,min=0)
  nul=min(abs(nadlambda-400.),wl_i)
  anet_500=histogram(zabove_org[wl_i,*]-nabove_org[wl_i,*],nbins=50,max=1.0,min=0)
  bnet_500=histogram(zbelow[wl_i,*]-nbelow[wl_i,*],nbins=50,max=1.0,min=0)
  ad_500=histogram(zabove_org[wl_i,*],nbins=50,max=1.0,min=0)
  bd_500=histogram(zbelow[wl_i,*],nbins=50,max=1.0,min=0)

; plotting
  set_plot, 'ps'
  loadct, 39,/silent
   !p.font=1 & !p.thick=5 & !p.charsize=1.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
   !p.multi=[0,3,2]

  device,/color,bits_per_pixel=8., /encapsulated, /tt_font, set_font='Helvetica Bold'
  device, filename=arctas_dir+'nasa'+ll+date+ll+date+'_hist_irr.ps'
  device, xsize=50, ysize=30
  
  plot, findgen(50)*0.01-0.005, anet_1200/total(anet_1200), title='Distribution of net irradiance at 1600 nm',ytitle='Normalized counts', xtitle='Irradiance (W/m!U2!N)',$
    xrange=[0.0,0.6],psym=10, yrange=[0.01,1],/ylog
  oplot, findgen(50)*0.01-0.005, bnet_1200/total(bnet_1200),color=250,psym=10
  legend, ['Top leg','Bottom leg'],textcolors=[0,250],box=0,/right
  
  plot, findgen(50)*0.02-0.01, anet_900/total(anet_900), title='Distribution of net irradiance at 500 nm',ytitle='Normalized counts', xtitle='Irradiance (W/m!U2!N)',$
    xrange=[0.0,0.6],psym=10, yrange=[0.01,1],/ylog
  oplot, findgen(50)*0.02-0.01, bnet_900/total(bnet_900),color=250,psym=10
  legend, ['Top leg','Bottom leg'],textcolors=[0,250],box=0,/right
  
  plot, findgen(50)*0.02-0.01, anet_500/total(anet_500), title='Distribution of net irradiance at 400 nm',ytitle='Normalized counts', xtitle='Irradiance (W/m!U2!N)',$
    xrange=[0.0,0.6],psym=10, yrange=[0.01,1],/ylog
  oplot, findgen(50)*0.02-0.01, bnet_500/total(bnet_500),color=250,psym=10
  legend, ['Top leg','Bottom leg'],textcolors=[0,250],box=0,/right

  plot, findgen(50)*0.01-0.005, ad_1200/total(ad_1200), title='Distribution of downwelling irradiance at 1600 nm',ytitle='Normalized counts', xtitle='Irradiance (W/m!U2!N)',$
    xrange=[0.0,0.6],psym=10, yrange=[0.01,1],/ylog
  oplot, findgen(50)*0.01-0.005, bd_1200/total(bd_1200),color=250,psym=10
  legend, ['Top leg','Bottom leg'],textcolors=[0,250],box=0,/right
  
  plot, findgen(50)*0.02-0.01, ad_900/total(ad_900), title='Distribution of downwelling irradiance at 500 nm',ytitle='Normalized counts', xtitle='Irradiance (W/m!U2!N)',$
    xrange=[0.0,0.6],psym=10, yrange=[0.01,1],/ylog
  oplot, findgen(50)*0.02-0.01, bd_900/total(bd_900),color=250,psym=10
  legend, ['Top leg','Bottom leg'],textcolors=[0,250],box=0,/right
  
  plot, findgen(50)*0.02-0.01, ad_500/total(ad_500), title='Distribution of downwelling irradiance at 400 nm',ytitle='Normalized counts', xtitle='Irradiance (W/m!U2!N)',$
    xrange=[0.0,0.6],psym=10, yrange=[0.01,1],/ylog
  oplot, findgen(50)*0.02-0.01, bd_500/total(bd_500),color=250,psym=10
  legend, ['Top leg','Bottom leg'],textcolors=[0,250],box=0,/right

  device, /close
  spawn, 'convert '+arctas_dir+'nasa'+ll+date+ll+date+'_hist_irr.ps '+arctas_dir+'nasa'+ll+date+ll+date+'_hist_irr'+secon+'.png'
  if linux then spawn, 'rm '+arctas_dir+'nasa'+ll+date+ll+date+'_hist_irr.ps' else spawn, 'del /Q '+arctas_dir+'nasa'+ll+date+ll+date+'_hist_irr.ps'
  
  
 ;matching upper leg to lower legs
  dn=fltarr(n_elements(leg1),n_elements(leg2))
  dd=dn
  d_ini=intarr(n_elements(leg1),n_elements(leg2))
  d_inj=intarr(n_elements(leg1),n_elements(leg2))
  nul=min(abs(nadlambda-1600.),wl_i)
  print, 'starting loop'
  for i=0, n_elements(leg1)-1 do begin
    for j=0, n_elements(leg2)-1 do begin
      dn[i,j]=abs((zabove_org[wl_i,j]-nabove_org[wl_i,j])-(zbelow[wl_i,i]-nbelow[wl_i,i]))
      dd[i,j]=zabove_org[wl_i,j]-zbelow[wl_i,i]
      d_ini[i,j]=i
      d_inj[i,j]=j
    endfor
  endfor 
  eps=0.01
  dds=where(dd ge 0.)
  print,'starting sort'
  dnf=dn[dds]
  dns=sort(dnf)
  ddf=dd[dds]
  print, 'end sort'
  
  fl=where(dn le 0.001 and dd ge 0.)
  
  histi=histogram(d_ini[fl],nbins=n_elements(leg1), max=n_elements(leg1),min=0)
  histj=histogram(d_inj[fl],nbins=n_elements(leg2), max=n_elements(leg2),min=0)
  
  ; plotting
  set_plot, 'ps'
  loadct, 39,/silent
   !p.font=1 & !p.thick=5 & !p.charsize=1.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
   !p.multi=[0,2,2]
    device,/color,bits_per_pixel=8., /encapsulated, /tt_font, set_font='Helvetica Bold'
  device, filename=arctas_dir+'nasa'+ll+date+ll+date+'_match_irr.ps'
  device, xsize=40, ysize=40
  
  plot, dnf[dns], title='Sorted difference in net irradiance'
  plot, ddf[dns], title='Sorted difference in downwelling irradiance'
  
  plot, findgen(n_elements(leg1)),histi, title='Amount of times each value is reused - Bottom leg', xtitle='values',psym=10
  plot, findgen(n_elements(leg2)),histj, title='Amount of times each value is reused - Top leg', xtitle='values',psym=10
  
    device, /close
  spawn, 'convert '+arctas_dir+'nasa'+ll+date+ll+date+'_match_irr.ps '+arctas_dir+'nasa'+ll+date+ll+date+'_match_irr'+secon+'.png'
  if linux then spawn, 'rm '+arctas_dir+'nasa'+ll+date+ll+date+'_match_irr.ps' else spawn, 'del /Q '+arctas_dir+'nasa'+ll+date+ll+date+'_match_irr.ps'
  
  ;setting up indices
  leg1=leg1[d_ini[fl]]
  near_leg2=leg2[d_inj[fl]]
  zabove=zspectra[*,near_leg2]
  nabove=nspectra[*,near_leg2]
  zbelow=zspectra[*,leg1]
  nbelow=nspectra[*,leg1]
  print, 'making new filters'
  ;stop
endif

; filtering routine to make sure that both IR and vis are coherent with each other
; filters out if IR larger and vis good (not coherent) and vice versa
tau=fltarr(n_elements(leg1),n_elements(lambda))  
lambdas=lambda*0
alpha=leg1*0.
dtau=tau
for i=0, n_elements(lambda)-1 do begin
	mm=min(abs(nadlambda-lambda[i]),zz)
	lambdas[i]=zz
endfor
wl=500.
mm=min(abs(nadlambda-wl),wl_i)
wll=1050.
mm=min(abs(nadlambda-wll),wl_ii)
wlll=650.
mm=min(abs(nadlambda-wlll),wl_iii)
fl=0
cts=intarr(n_elements(leg1))
limit=mean(zabove[wl_i,*],/nan)
limit_b=mean(zbelow[wl_i,*],/nan)
limitt=mean(zabove[wl_ii,*],/nan)
limitt_b=mean(zbelow[wl_ii,*],/nan)
limitn_b=mean(nbelow[wl_i,*],/nan)
for i=0,n_elements(leg1)-1 do begin
  fl_t=0 & fl_tt=0 & fl_ttt=0 & fl_tttt=0 & fl_ttttt=0 & fl_tn=0 & fl_tau=0 & fl_tau2=0 & fl_div=0
  if zabove[wl_i,i] lt limit-0.2 then fl_bad=1 else fl_t=1  ;0.1
  if zbelow[wl_i,i] lt limit_b-0.2 then fl_bad=1 else fl_tt=1  ;0.1
  if zabove[wl_ii,i] lt limitt-0.1 then fl_bad=1 else fl_ttt=1 ; 0.05
  if (zbelow[wl_ii,i] lt limitt_b-0.1) xor (zbelow[wl_i,i] lt limit_b-1.0) then fl_bad=1 else fl_tttt=1  ;0.05, 0.5
  ;fl_ttttt=1 ;if (r_d[i] gt 800.0) then fl_bad=1 else fl_ttttt=1 ;500.0
  if nbelow[wl_i,i] lt limitn_b-0.2 then fl_bad=1 else fl_tn=1  ;0.1
  if (zabove[wl_i,i]-nabove[wl_i,i])-(zbelow[wl_i,i]-nbelow[wl_i,i]) lt 0 then fl_bad=1 else fl_div=1
  nul=where((zabove[lambdas,i]-nabove[lambdas,i])-(zbelow[lambdas,i]-nbelow[lambdas,i]) lt 0, ct) ;make sure that the flux divergence at the wavelengths are above zero
  cts[i]=ct
  if ct eq n_elements(lambda) then fl_bad=1 else fl_ttttt=1
  if total(finite(tau_aats[leg1[i],*])-1) ge -12 then fl_tau=1   ; check to make sure that aats has at least one non NaN in all wavelengths measured
  if total(finite(tau_aats[near_leg2[i],*])-1) ge -12 then fl_tau2=1
  if (fl_t and fl_tt and fl_ttt and fl_tttt and fl_ttttt and fl_tn and fl_tau and fl_tau2 and fl_div) then fl=[fl,i]
  tau[i,*]=tau_aats[leg1[i],*]-tau_aats[near_leg2[i],*]  ; determine optical depth between each layer
  dtau[i,*]=sqrt((dtau_aats[leg1[i],*])^2.+(dtau_aats[near_leg2[i],*])^2.)
  if hsrl then begin
    tau_h=leg1*0.0
    z_hsrl=findgen(n_hsrl[near_hsrl[i]])*dz_hsrl[near_hsrl[i]] + z_o_hsrl[near_hsrl[i]]    ; get altitudes of hsrl
    zs=where(z_hsrl ge alt[leg1[i]] and z_hsrl le alt[near_leg2[i]],cts)   ; find nearest altitude
    if cts gt 0 then begin
      for iz=0, cts-1 do if not finite(ext532[near_hsrl[i],iz]) then ext532[near_hsrl[i],iz]=0.0
      tau_h[i]=total(ext532[near_hsrl[i],zs] * (z_hsrl[zs+1]-z_hsrl[zs])/1000.0,/nan)  ; get hsrl optical depth between the two legs
      for iws=0, n_elements(lambda)-1 do begin ; use aats measurements to determine the angstrom exponent, and then extrapolate hsrl optical depth to other wavelengths
        ggg=where(finite(alog(tau_aats[leg1[i],*])))
        garg=linfit(alog(lambda[ggg]),alog(tau_aats[leg1[i],ggg]))
        alpha[i]=garg[1]
        tau[i,iws]=tau_h[i]* ((lambda[iws])/(532.))^(alpha[i])
      endfor
    endif else tau[i,*]=tau[i,*]*0.0
  endif
endfor

if n_elements(fl) le 1 then print, '*** all points filtered out ***'
fl=fl[1:*]

print, 'number of valid spectras:',n_elements(fl) 
if hsrl then print, 'hsrl finished'

legs={leg1:leg1,leg2:leg2}
topt=mean(alt[leg2],/nan)/1000.0
bottomt=mean(alt[leg1],/nan)/1000.0
near={nabove:nabove,nbelow:nbelow,zabove_attcorr:zabove,zbelow_attcorr:zbelow} ; make legs data into structure "near" for use later, similar to CalNex

if hist then begin
;plotting
print, 'plotting absorptions'
  set_plot, 'ps'
  loadct, 39,/silent
   !p.font=1 & !p.thick=5 & !p.charsize=1.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
   !p.multi=[0,2,2]
    device,/color,bits_per_pixel=8., /encapsulated, /tt_font, set_font='Helvetica Bold'
  device, filename=arctas_dir+'nasa'+ll+date+ll+date+'_abs_irr.ps'
  device, xsize=40, ysize=40
  plot, nadlambda, (zabove[*,fl[0]]-nabove[*,fl[0]])-(zbelow[*,fl[0]]-nbelow[*,fl[0]]), title='Absorption',xtitle='Wavelenght (nm)',ytitle='Irradiance (W/m!U2!N nm)',yrange=[-0.1,0.2]
  for i=1, n_elements(fl)-1 do oplot, nadlambda, (zabove[*,fl[i]]-nabove[*,fl[i]])-(zbelow[*,fl[i]]-nbelow[*,fl[i]])
  
  mm=min(abs(lambda-500), w5)
  
  plot, utc[leg1[fl]], tau[fl,w5], title='UTC of tau',xtitle='UTC',ytitle='optical depth at 500 nm', psym=3
    
  plot, tau[fl,w5], (zabove[wl_i,fl]-nabove[wl_i,fl])-(zbelow[wl_i,fl]-nbelow[wl_i,fl]),title='Optical depth at 500 nm', xtitle='Optical depth', ytitle='Absorption (W/m!U2!N nm)',psym=3

  plot, utc[leg1[fl]], (zabove[wl_i,fl]-nabove[wl_i,fl])-(zbelow[wl_i,fl]-nbelow[wl_i,fl]), title='UTC of absorption',xtitle='UTC',ytitle='Absorption (W/m!U2!N nm)', psym=3
 
  device, /close
  spawn, 'convert '+arctas_dir+'nasa'+ll+date+ll+date+'_abs_irr.ps '+arctas_dir+'nasa'+ll+date+ll+date+'_abs_irr'+secon+'.png'
  if linux then spawn, 'rm '+arctas_dir+'nasa'+ll+date+ll+date+'_abs_irr.ps' else spawn, 'del /Q '+arctas_dir+'nasa'+ll+date+ll+date+'_abs_irr.ps'
  stop
endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; plotting routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
if keyword_set(choose) then goto, f_plot

;;; irradiances for top bottom at both 500 and 1050nm
if plotting then begin
  set_plot, 'ps'
  loadct, 39,/silent
   !p.font=1 & !p.thick=5 & !p.charsize=1.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
   !p.multi=[0,3,2]

  device,/color,bits_per_pixel=8., /encapsulated, /tt_font, set_font='Helvetica Bold'
  device, filename=arctas_dir+'nasa'+ll+date+ll+date+'_nasa_irr.ps'
  device, xsize=50, ysize=30

  plot, lon[legs.leg1[fl]],  near.zabove_attcorr[wl_i,fl],color=0,$
   title='Irradiance timelapse for '+date+' at '+strtrim(string(nadlambda[wl_i]),2)+'nm', ytitle='Irradiance', xtitle='Longitude', yrange=[0,1.5]
  oplot, lon[legs.leg1[fl]],  near.nabove[wl_i,fl], color=150
  oplot, lon[legs.leg1[fl]],  near.zabove_attcorr[wl_i,fl]-near.nabove[wl_i,fl], color=240
  
  oplot, lon[legs.leg1[fl]],  near.zbelow_attcorr[wl_i,fl], color=0, linestyle=2
  oplot, lon[legs.leg1[fl]],  near.nbelow[wl_i,fl], color=150, linestyle=2
  oplot, lon[legs.leg1[fl]],  near.zbelow_attcorr[wl_i,fl]-near.nbelow[wl_i,fl], color=240, linestyle=2
  
  legend, ['Downwelling', 'Upwelling','Net','Bottom leg'],textcolors=[0,150,240,0],linestyle=[0,0,0,2],color=[0,150,240,0], box=0, /center
  
  wl=1050.
  mm=min(abs(nadlambda-wl),wl_i)
 
  plot, lon[legs.leg1[fl]],  near.zabove_attcorr[wl_i,fl],color=0,$
   title='Irradiance timelapse at '+strtrim(string(nadlambda[wl_i]),2)+'nm', ytitle='Irradiance', xtitle='Longitude',yrange=[0,0.8], xmargin=[10,6]
  oplot, lon[legs.leg1[fl]],  near.nabove[wl_i,fl], color=150
  oplot, lon[legs.leg1[fl]],  near.zabove_attcorr[wl_i,fl]-near.nabove[wl_i,fl], color=240
  
  oplot, lon[legs.leg1[fl]],  near.zbelow_attcorr[wl_i,fl], color=0, linestyle=2
  oplot, lon[legs.leg1[fl]],  near.nbelow[wl_i,fl], color=150, linestyle=2
  oplot, lon[legs.leg1[fl]],  near.zbelow_attcorr[wl_i,fl]-near.nbelow[wl_i,fl], color=240, linestyle=2
  
  ;plotting tau
  ;axis, yaxis=1, /device, yrange=[0,0.3], /save, color=200, ytitle='tau'
  plot, lon[legs.leg1[fl]], tau[fl,4], psym=2, ytitle='tau', xtitle='longitude', title='Optical thickness between flight legs from AATS'
  
  ;plotting of albedo
  wl=650.
  mm=min(abs(nadlambda-wl),wl_i)
  plot, lon[legs.leg1[fl]],  near.nabove[wl_i,fl]/near.zabove_attcorr[wl_i,fl],color=0,$
  title='Albedo at different wavelengths', ytitle='Albedo', xtitle='Longitude',yrange=[0.1,1.0]
  oplot, lon[legs.leg1[fl]], near.nbelow[wl_i,fl]/near.zbelow_attcorr[wl_i,fl], color=240, linestyle=0
  txt=[strtrim(string(nadlambda[wl_i]),2)+'nm']
  wl=550.
  mm=min(abs(nadlambda-wl),wl_i)
  oplot, lon[legs.leg1[fl]],  near.nabove[wl_i,fl]/near.zabove_attcorr[wl_i,fl], color=0, linestyle=2
  oplot, lon[legs.leg1[fl]],  near.nbelow[wl_i,fl]/near.zbelow_attcorr[wl_i,fl], color=240, linestyle=2
  txt=[txt,strtrim(string(nadlambda[wl_i]),2)+'nm']
  wl=1050.
  mm=min(abs(nadlambda-wl),wl_i)
  oplot, lon[legs.leg1[fl]],  near.nabove[wl_i,fl]/near.zabove_attcorr[wl_i,fl], color=0, linestyle=3
  oplot, lon[legs.leg1[fl]],  near.nbelow[wl_i,fl]/near.zbelow_attcorr[wl_i,fl], color=240, linestyle=3
  txt=[txt,strtrim(string(nadlambda[wl_i]),2)+'nm']
  
  legend, [txt,'above', 'below'], textcolors=[0,0,0,0,240], color=[0,0,0,0,240],linestyle=[0,2,3,0,0], box=0, /bottom

  plot, lon[legs.leg1[fl]], r_d[fl], title='Distance from the lower leg to the top leg', xtitle='Longitude', ytitle='meters', ystyle=8, xmargin=[10,6]
  oplot, lon[legs.leg1[fl]], findgen(n_elements(legs.leg1[fl]))*10, psym=2, color=70
  for i=0, n_elements(legs.leg1[fl])-1,5 do begin
    xyouts, lon[legs.leg1[fl[i]]],i*10,strtrim(string(fl[i]),2), color=70
  endfor
  
  ; checking angles
  axis, yaxis=1, ytitle='roll', yrange=[-10.,+10.0],/save, color=250
 ; SSFR_path=calnex_dir+'p3'+ll+date+ll+date+'_SP.out'
 ; restore, SSFR_path
  oplot, lon[legs.leg1[fl]],rol[legs.leg1[fl]], color=250
  oplot, lon[legs.leg1[fl]],pit[legs.leg1[fl]], color=150
  xyouts, lon[legs.leg1[n_elements(fl)-1]] , pit[legs.leg1[n_elements(fl)-1]],'Pitch', color=150
  
  plot, utc[near_leg2[fl]],tau_aats[near_leg2[fl],0],yrange=[0,0.10],xtitle='UTC',ytitle='Tau',title='aats optical thickness above flight leg'
  for yt=1, 12 do begin
    oplot, utc[near_leg2[fl]],tau_aats[near_leg2[fl],yt], color=yt*20
  endfor
  legend,strtrim(string(lambda),2),textcolors=indgen(13)*20, /box
    
  device, /close
  spawn, 'convert '+arctas_dir+'nasa'+ll+date+ll+date+'_nasa_irr.ps '+arctas_dir+'nasa'+ll+date+ll+date+'_nasa_irr'+secon+'.png'
  if linux then spawn, 'rm '+arctas_dir+'nasa'+ll+date+ll+date+'_nasa_irr.ps' else spawn, 'del /Q '+arctas_dir+'nasa'+ll+date+ll+date+'_nasa_irr.ps'
  
  if keyword_set(find_legs) then begin
	set_plot, 'x'
	window, 2, xsize=1000, ysize=800, title='time series',xpos=0,ypos=0, retain=2
	loadct, 39, /silent
	device, decomposed=0
	!p.color=0 & !p.background=255 & !p.charsize=1.8 & !x.charsize=1.5 & !y.charsize=1.5 & !p.multi=[0,3,2] & !p.font=0 & !p.thick=2.0
    wl=550.
    mm=min(abs(nadlambda-wl),wl_i)
    plot, lon[legs.leg1[fl]],  near.zabove_attcorr[wl_i,fl],color=0,$
   title='Irradiance timelapse for '+date+' at '+strtrim(string(nadlambda[wl_i]),2)+'nm', ytitle='Irradiance', xtitle='Longitude', yrange=[0,1.7]
  oplot, lon[legs.leg1[fl]],  near.nabove[wl_i,fl], color=150
  oplot, lon[legs.leg1[fl]],  near.zabove_attcorr[wl_i,fl]-near.nabove[wl_i,fl], color=240
  
  oplot, lon[legs.leg1[fl]],  near.zbelow_attcorr[wl_i,fl], color=0, linestyle=2
  oplot, lon[legs.leg1[fl]],  near.nbelow[wl_i,fl], color=150, linestyle=2
  oplot, lon[legs.leg1[fl]],  near.zbelow_attcorr[wl_i,fl]-near.nbelow[wl_i,fl], color=240, linestyle=2
  
  legend, ['Downwelling', 'Upwelling','Net','Bottom leg'],textcolors=[0,150,240,0],linestyle=[0,0,0,2],color=[0,150,240,0], box=0, /center
  
  wl=1050.
  mm=min(abs(nadlambda-wl),wl_i)
  plot, lon[legs.leg1[fl]],  near.zabove_attcorr[wl_i,fl],color=0,$
   title='Irradiance timelapse at '+strtrim(string(nadlambda[wl_i]),2)+'nm', ytitle='Irradiance', xtitle='Longitude',yrange=[0,0.8], xmargin=[10,6]
  oplot, lon[legs.leg1[fl]],  near.nabove[wl_i,fl], color=150
  oplot, lon[legs.leg1[fl]],  near.zabove_attcorr[wl_i,fl]-near.nabove[wl_i,fl], color=240
  
  oplot, lon[legs.leg1[fl]],  near.zbelow_attcorr[wl_i,fl], color=0, linestyle=2
  oplot, lon[legs.leg1[fl]],  near.nbelow[wl_i,fl], color=150, linestyle=2
  oplot, lon[legs.leg1[fl]],  near.zbelow_attcorr[wl_i,fl]-near.nbelow[wl_i,fl], color=240, linestyle=2
  
  ;plotting tau
  ;axis, yaxis=1, /device, yrange=[0,0.3], /save, color=200, ytitle='tau'
  plot, lon[legs.leg1[fl]], tau[fl,4], psym=2, ytitle='tau', xtitle='longitude', title='Optical thickness between flight legs from AATS'
  
  ;plotting of albedo
  wl=650.
  mm=min(abs(nadlambda-wl),wl_i)
  plot, lon[legs.leg1[fl]],  near.nabove[wl_i,fl]/near.zabove_attcorr[wl_i,fl],color=0,$
   title='Albedo at different wavelengths', ytitle='Albedo', xtitle='Longitude',yrange=[0.1,1.0]
  oplot, lon[legs.leg1[fl]], near.nbelow[wl_i,fl]/near.zbelow_attcorr[wl_i,fl], color=240, linestyle=0
  txt=[strtrim(string(nadlambda[wl_i]),2)+'nm']
  wl=550.
  mm=min(abs(nadlambda-wl),wl_i)
  oplot, lon[legs.leg1[fl]],  near.nabove[wl_i,fl]/near.zabove_attcorr[wl_i,fl], color=0, linestyle=2
  oplot, lon[legs.leg1[fl]],  near.nbelow[wl_i,fl]/near.zbelow_attcorr[wl_i,fl], color=240, linestyle=2
  txt=[txt,strtrim(string(nadlambda[wl_i]),2)+'nm']
  wl=1050.
  mm=min(abs(nadlambda-wl),wl_i)
  oplot, lon[legs.leg1[fl]],  near.nabove[wl_i,fl]/near.zabove_attcorr[wl_i,fl], color=0, linestyle=3
  oplot, lon[legs.leg1[fl]],  near.nbelow[wl_i,fl]/near.zbelow_attcorr[wl_i,fl], color=240, linestyle=3
  txt=[txt,strtrim(string(nadlambda[wl_i]),2)+'nm']
  
  legend, [txt,'above', 'below'], textcolors=[0,0,0,0,240], color=[0,0,0,0,240],linestyle=[0,2,3,0,0], box=0, /bottom

  plot, lon[legs.leg1[fl]], r_d[fl], title='Distance from the lower leg to the top leg', xtitle='Longitude', ytitle='meters', ystyle=8, xmargin=[10,6]
  oplot, lon[legs.leg1[fl]], findgen(n_elements(legs.leg1[fl]))*10, psym=2, color=70
  for i=0, n_elements(legs.leg1[fl])-1,5 do begin
    xyouts, lon[legs.leg1[fl[i]]],i*10,strtrim(string(fl[i]),2), color=70
  endfor
  
  ; checking angles
  axis, yaxis=1, ytitle='roll', yrange=[-10.,+10.0],/save, color=250
 ; SSFR_path=calnex_dir+'p3'+ll+date+ll+date+'_SP.out'
 ; restore, SSFR_path
  oplot, lon[legs.leg1[fl]],rol[legs.leg1[fl]], color=250
  oplot, lon[legs.leg1[fl]],pit[legs.leg1[fl]], color=150
  xyouts, lon[legs.leg1[n_elements(fl)-1]] , pit[legs.leg1[n_elements(fl)-1]],'Pitch', color=150
  
  plot, lon[legs.leg1[fl]],tau_aats[near_leg2[fl],0],yrange=[0,0.1],xtitle='longitude',ytitle='Tau',title='aats optical thickness above flight leg'
  for yt=1, 12 do begin
    oplot, lon[legs.leg1[fl]],tau_aats[near_leg2[fl],yt], color=yt*20
  endfor
  legend,strtrim(string(lambda),2),textcolors=indgen(13)*20, /box
  
  f_plot: ;interactive plotting
    ans=' '
  read,ans, prompt='select a point?(y=yes n=no)'
  if ans eq 'y' then begin
    print, 'Please click point'
        set_plot, 'x'
        window, 2, xsize=1000, ysize=800, title='time series',xpos=0,ypos=0, retain=2
        loadct, 39, /silent
        device, decomposed=0
        !p.color=0 & !p.background=255 & !p.charsize=1.8 & !x.charsize=1.5 & !y.charsize=1.5 & !p.multi=[0,3,2] & !p.font=0
  wl=550.
  mm=min(abs(nadlambda-wl),wl_i)
  plot, lon[legs.leg1[fl]],  near.zabove_attcorr[wl_i,fl],color=0,$
   title='Irradiance timelapse for '+date+' at '+strtrim(string(nadlambda[wl_i]),2)+'nm', ytitle='Irradiance', xtitle='Longitude', yrange=[0,1.5]
  oplot, lon[legs.leg1[fl]],  near.nabove[wl_i,fl], color=150
  oplot, lon[legs.leg1[fl]],  near.zabove_attcorr[wl_i,fl]-near.nabove[wl_i,fl], color=240

  oplot, lon[legs.leg1[fl]],  near.zbelow_attcorr[wl_i,fl], color=0, linestyle=2
  oplot, lon[legs.leg1[fl]],  near.nbelow[wl_i,fl], color=150, linestyle=2
  oplot, lon[legs.leg1[fl]],  near.zbelow_attcorr[wl_i,fl]-near.nbelow[wl_i,fl], color=240, linestyle=2

  legend, ['Downwelling', 'Upwelling','Net','Bottom leg'],textcolors=[0,150,240,0],linestyle=[0,0,0,2],color=[0,150,240,0], box=0, /center

  wl=1050.
  mm=min(abs(nadlambda-wl),wl_i)
  plot, lon[legs.leg1[fl]],  near.zabove_attcorr[wl_i,fl],color=0,$
   title='Irradiance timelapse at '+strtrim(string(nadlambda[wl_i]),2)+'nm', ytitle='Irradiance', xtitle='Longitude',yrange=[0,0.8], xmargin=[10,6]
  oplot, lon[legs.leg1[fl]],  near.nabove[wl_i,fl], color=150
  oplot, lon[legs.leg1[fl]],  near.zabove_attcorr[wl_i,fl]-near.nabove[wl_i,fl], color=240

  oplot, lon[legs.leg1[fl]],  near.zbelow_attcorr[wl_i,fl], color=0, linestyle=2
  oplot, lon[legs.leg1[fl]],  near.nbelow[wl_i,fl], color=150, linestyle=2
  oplot, lon[legs.leg1[fl]],  near.zbelow_attcorr[wl_i,fl]-near.nbelow[wl_i,fl], color=240, linestyle=2

  ;plotting tau
  ;axis, yaxis=1, /device, yrange=[0,0.3], /save, color=200, ytitle='tau'
  plot, lon[legs.leg1[fl]], tau[fl,4], psym=2, ytitle='tau', xtitle='longitude', title='Optical thickness between flight legs from AATS'

  ;plotting of albedo
  wl=650.
  mm=min(abs(nadlambda-wl),wl_i)
  plot, lon[legs.leg1[fl]],  near.nabove[wl_i,fl]/near.zabove_attcorr[wl_i,fl],color=0,$
   title='Albedo at different wavelengths', ytitle='Albedo', xtitle='Longitude',yrange=[0.1,1.0]
  oplot, lon[legs.leg1[fl]], near.nbelow[wl_i,fl]/near.zbelow_attcorr[wl_i,fl], color=240, linestyle=0
  txt=[strtrim(string(nadlambda[wl_i]),2)+'nm']
  wl=550.
  mm=min(abs(nadlambda-wl),wl_i)
  oplot, lon[legs.leg1[fl]],  near.nabove[wl_i,fl]/near.zabove_attcorr[wl_i,fl], color=0, linestyle=2
  oplot, lon[legs.leg1[fl]],  near.nbelow[wl_i,fl]/near.zbelow_attcorr[wl_i,fl], color=240, linestyle=2
  txt=[txt,strtrim(string(nadlambda[wl_i]),2)+'nm']
  wl=1050.
  mm=min(abs(nadlambda-wl),wl_i)
  oplot, lon[legs.leg1[fl]],  near.nabove[wl_i,fl]/near.zabove_attcorr[wl_i,fl], color=0, linestyle=3
  oplot, lon[legs.leg1[fl]],  near.nbelow[wl_i,fl]/near.zbelow_attcorr[wl_i,fl], color=240, linestyle=3
  txt=[txt,strtrim(string(nadlambda[wl_i]),2)+'nm']

  legend, [txt,'above', 'below'], textcolors=[0,0,0,0,240], color=[0,0,0,0,240],linestyle=[0,2,3,0,0], box=0, /bottom

  plot, lon[legs.leg1[fl]], r_d[fl], title='Distance from the lower leg to the top leg', xtitle='Longitude', ytitle='meters', ystyle=8, xmargin=[10,6]
  oplot, lon[legs.leg1[fl]], findgen(n_elements(legs.leg1[fl]))*10, psym=2, color=70
  for i=0, n_elements(legs.leg1[fl])-1,5 do begin
    xyouts, lon[legs.leg1[fl[i]]],i*10,strtrim(string(fl[i]),2), color=70
  endfor

  ; checking angles
  axis, yaxis=1, ytitle='roll', yrange=[-10.,+10.0],/save, color=250
 ; SSFR_path=calnex_dir+'p3'+ll+date+ll+date+'_SP.out'
 ; restore, SSFR_path
  oplot, lon[legs.leg1[fl]],rol[legs.leg1[fl]], color=250
  oplot, lon[legs.leg1[fl]],pit[legs.leg1[fl]], color=150
  xyouts, lon[legs.leg1[n_elements(fl)-1]] , pit[legs.leg1[n_elements(fl)-1]],'Pitch', color=150

  plot, lon[near_leg2[fl]],tau_aats[near_leg2[fl],0],yrange=[0,0.1],xtitle='longitude',ytitle='Tau',title='aats optical thickness above flight leg'
  for yt=1, 12 do begin
    oplot, lon[near_leg2[fl]],tau_aats[near_leg2[fl],yt], color=yt*20
  endfor
  legend,strtrim(string(lambda),2),textcolors=indgen(13)*20, /box
	
  cursor, x,y,/down, /data
  mm=min(abs(x-lon[legs.leg1[fl]]),inds)
  it=0
  if n_elements(ind) lt 1 then ind=[inds] else begin 
	ind=[ind,inds]
    it=n_elements(ind)-1
  endelse
  window, 3, title='Spectrums',xsize=800, ysize=800,xpos=0,ypos=0
  !p.multi=[0,3,2]
  plot, nadlambda,  ((zabove[*,ind[it]]-nabove[*,ind[it]])-(zbelow[*,ind[it]]-nbelow[*,ind[it]])),$;/zabove[*,ind[it]],$
   title='Flux Divergence spectra for '+date, ytitle=' absorptance', xtitle='wavelength (nm)', yrange=[-0.1,1.2]
  oplot, [300,2000],[0,0],linestyle=1 
  cl_leg=[0]
  txt_leg=['index:'+strtrim(string(ind[it]),2)]
  legend, txt_leg, textcolors=cl_leg, /box
	
  plot, nadlambda, zabove[*,ind[it]], title='Flight leg above', ytitle='Irradiance (w/m^2)', xtitle='wavelength (nm)', yrange=[0,2]
  oplot, nadlambda, nabove[*,ind[it]],color=ind[it]*3, linestyle=2
  legend,[txt_leg,'Zenith','Nadir'], textcolors=[cl_leg,0,0], /box, color=[cl_leg,0,0], linestyle=[0*cl_leg,0,2],/right
  
  plot, nadlambda, zbelow[*,ind[it]], title='Flight leg below', ytitle='Irradiance (w/m^2)', xtitle='wavelength (nm)',yrange=[0,2.0]
  oplot, nadlambda, nbelow[*,ind[it]],color=ind[it]*3, linestyle=2
  legend,[txt_leg,'Zenith','Nadir'], textcolors=[cl_leg,0,0], /box, color=[cl_leg,0,0], linestyle=[0*cl_leg,0,2],/right
  
  plot, lambda, tau[ind[it],*], title='AATS Optical Thickness in between each flight leg', ytitle='Optical Thickness', xtitle='wavelength (nm)', yrange=[0,1]
  legend,txt_leg, textcolors=cl_leg, /box

  plot, nadlambda, zabove[*,ind[it]], title='Zenith irradiance', ytitle='Irradiance (W/m^2)', xtitle='wavelength (nm)',yrange=[0,2]
  oplot, nadlambda, zbelow[*,ind[it]], color=250
  legend, ['above','below'],textcolors=[0,250]
  plot, nadlambda, nabove[*,ind[it]], title='Nadir irradiance', ytitle='Irradiance (W/m^2)', xtitle='wavelength (nm)',yrange=[0,2] 
  oplot, nadlambda, nbelow[*,ind[it]], color=250
  legend, ['above','below'],textcolors=[0,250]

  goto, f_plot
endif
  endif
endif
if keyword_set(find_legs) then stop

if divergence then begin
  if not keyword_set(find_legs) or not keyword_set(choose) then ind=[0,1,20,40,60,80] else print, 'indices to plot over:', ind
  
  ite=n_elements(ind)-1
  for it=0, ite  do begin
    if it eq 0 then begin
      set_plot, 'ps'
      loadct, 39
      !p.font=1 & !p.thick=3 & !p.charsize=1.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8 & !p.multi=[0,2,2]
      device, /encapsulated
       device, /tt_font, set_font='Helvetica Bold'
       device, filename=arctas_dir+'nasa'+ll+date+ll+date+'_nasa_0.ps'
       device,/color,bits_per_pixel=8.
       device, xsize=40, ysize=40
            
      plot, nadlambda,  ((zabove[*,ind[it]]-nabove[*,ind[it]])-(zbelow[*,ind[it]]-nbelow[*,ind[it]])),$;/zabove[*,ind[it]],$
      title='Flux Divergence spectra for '+date, ytitle=' absorptance', xtitle='wavelength (nm)', yrange=[-0.1,1.2]
      cl_leg=[0]
      txt_leg=['index:'+strtrim(string(ind[it]),2)]
    endif else begin
      oplot, nadlambda,  ((zabove[*,ind[it]]-nabove[*,ind[it]])-(zbelow[*,ind[it]]-nbelow[*,ind[it]]))/zabove[*,ind[it]],$
       color=ind[it]*3
      cl_leg=[cl_leg,ind[it]*3]
      txt_leg=[txt_leg,'index:'+strtrim(string(ind[it]),2)]
    endelse
  endfor
  
  legend, txt_leg, textcolors=cl_leg, /box
  
  it=0 
  plot, nadlambda, zabove[*,ind[it]], title='Flight leg above', ytitle='Irradiance (w/m^2)', xtitle='wavelength (nm)', yrange=[0,2]
  oplot, nadlambda, nabove[*,ind[it]],color=ind[it]*3, linestyle=2
  for it=1, ite do begin
    oplot, nadlambda, zabove[*,ind[it]], color=ind[it]*3
    oplot, nadlambda, nabove[*,ind[it]], color=ind[it]*3, linestyle=2
  endfor
  legend,[txt_leg,'Zenith','Nadir'], textcolors=[cl_leg,0,0], /box, color=[cl_leg,0,0], linestyle=[0*cl_leg,0,2],/right
 
  it=0
  plot, nadlambda, zbelow[*,ind[it]], title='Flight leg below', ytitle='Irradiance (w/m^2)', xtitle='wavelength (nm)',yrange=[0,2.0]
  oplot, nadlambda, nbelow[*,ind[it]],color=ind[it]*3, linestyle=2
  for it=1, ite do begin
    oplot, nadlambda, zbelow[*,ind[it]], color=ind[it]*3
    oplot, nadlambda, nbelow[*,ind[it]], color=ind[it]*3, linestyle=2
  endfor
  legend,[txt_leg,'Zenith','Nadir'], textcolors=[cl_leg,0,0], /box, color=[cl_leg,0,0], linestyle=[0*cl_leg,0,2],/right
  
  it=0
  plot, lambda, tau[ind[it],*], title='AATS Optical Thickness in between each flight leg', ytitle='Optical Thickness', xtitle='wavelength (nm)', yrange=[0,1]
  for it=1, ite do begin
    oplot, lambda, tau[ind[it],*], color=ind[it]*3
  endfor
  legend,txt_leg, textcolors=cl_leg, /box

  device, /close
  spawn, 'convert '+arctas_dir+'nasa'+ll+date+ll+date+'_nasa_0.ps '+arctas_dir+'nasa'+ll+date+ll+date+'_nasa_0'+secon+'.png'
  ;spawn, 'display '+calnex_dir+'p3'+ll+date+ll+date+'_p3_hsrl_0.png'
  if linux then spawn, 'rm '+arctas_dir+'nasa'+ll+date+ll+date+'_nasa_0.ps' else spawn, 'del /Q '+arctas_dir+'nasa'+ll+date+ll+date+'_nasa_0.ps'
endif

; plotting for making video
; plot of all the different spectras throughout the leg
if video then begin
  !p.multi=[0,2,1]
  en=n_elements(legs.leg1)-1
  for i=0, en do begin
    set_plot, 'ps'
    loadct, 39, /silent
     device, /encapsulated
      device, /tt_font, set_font='Helvetica Bold'
      device, filename=arctas_dir+'nasa'+ll+date+ll+strtrim(string(i,format='(I02.2)'),2)+'.ps'
      device,/color,bits_per_pixel=8.
      device, xsize=40, ysize=20
    
    plot, nadlambda,  near.zabove_attcorr[*,i],$
    title='Spectra for '+date+' Longitude: '+strtrim(string(lon[legs.leg1[i]]),2), ytitle='Irradiance', xtitle='wavelength (nm)', yrange=[0,2.]
    oplot, nadlambda,  near.nabove[*,i], color=150
    oplot, nadlambda,  near.zabove_attcorr[*,i]-near.nabove[*,i], color=240
    
    oplot, nadlambda,  near.zbelow_attcorr[*,i], color=0, linestyle=2
    oplot, nadlambda,  near.nbelow[*,i], color=150, linestyle=2
    oplot, nadlambda,  near.zbelow_attcorr[*,i]-near.nbelow[*,i], color=240, linestyle=2
    
    legend, ['Downwelling', 'Upwelling','Net','Bottom leg'],textcolors=[0,150,240,0],linestyle=[0,0,0,2],color=[0,150,240,0], box=0, /right
    
    plot, nadlambda,  near.nabove[*,i]/near.zabove_attcorr[*,i],color=0,$
    title='Spectral Albedo', ytitle='Albedo', xtitle='Wavelength (nm)',yrange=[0,0.3]
    oplot, nadlambda, near.nbelow[*,i]/near.zbelow_attcorr[*,i], color=0, linestyle=2
    
    device, /close
    spawn, 'convert '+arctas_dir+'nasa'+ll+date+ll+strtrim(string(i,format='(I02.2)'),2)+'.ps '+arctas_dir+'nasa'+ll+date+ll+strtrim(string(i,format='(I02.2)'),2)+'.jpg'
    if linux then spawn, 'rm '+arctas_dir+'nasa'+ll+date+ll+strtrim(string(i,format='(I02.2)'),2)+'.ps' else spawn, 'del /Q '+arctas_dir+'nasa'+ll+date+ll+strtrim(string(i,format='(I02.2)'),2)+'.ps'
  endfor
endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Writing of the input file for uvspec                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

atm_in = 'atmos_aero_'+date+'_at.dat'   ;input atmosphere file
if keyword_set(second) then atm_in = 'atmos_aero_'+date+'_ats.dat'

; set random starting values for input file
asy=0.6 & asy2=asy & ssa=0.8

ssa_arr=fl*0.0 & asy_arr=fl*0.0 & asy2_arr=fl*0.0
albedo_arr=fl*0.0 & correc_fac_arr=fl*0.0 & tau_mod_arr=fl*0.0
dif_flux=fl*0.0 & tau_arr=fl*0.0
Ft_up_arr=fl*0.0 & Ft_dn_arr=fl*0.0 & Fb_up_arr=fl*0.0 & Fb_dn_arr=fl*0.0

unique_ct=1. ; set unique loop to start
unique_loop=1
if unique then begin
  lunun=92  
  openw, lunun, dirout+'rtm_unique_'+date+secon+'.txt';,/get_lun
  printf, lunun, '#lat	 lon   wvl   ssa   asy   asy2    albedo     correction    tau    tau_scale    original tau      flux divergence	  model down	  model up	 Ft_up   Ft_dn    Fb_up   Fb_dn'
  free_lun,lunun
endif

while unique_loop eq 1 do begin

if unique then begin
  if unique_ct gt n_unique then unique_loop=0 else begin
    tau_scale=float(unique_ct)*0.1
	print, 'tau_scale factor at:',tau_scale, ' with unique loop at:',unique_ct
    unique_ct=unique_ct + 1.
  endelse
endif else unique_loop=0

if keyword_set(only_hsrl) then begin
  aeronet=0 & wvl_arr=[532.] ; set the wavelength to the hsrl wavelength
  iw_f=0 & ia_arr=[0]
endif ; hsrl keyword

if aeronet then begin
  ; get aeronet reading
  read_aeronet, calnex_dir+'aeronet'+ll+'*CalTech',  jul_day, jul_day_almu, wvl_aod,wvl_almu, aod_aeronet,ssa_aeronet, sza_aeronet, asy_TT_aeronet, aot2_T_aeronet, up_flux_T_aeronet, down_flux_T_aeronet, diff_flux_T_aeronet, forcing_aeronet, albedo_aeronet
  ; find correct times and wavelengths
  ; make a list of good wavelengths from aeronet
  wvl_arr=0.0 & ia_arr=0
  for i_a=0, n_elements(wvl_aod)-1 do begin
    if finite(aod_aeronet[i_a,0]) then begin
      wvl_arr=[wvl_arr,wvl_aod[i_a]] 
      ia_arr=[ia_arr,i_a]
    endif
  endfor
  wvl_arr=wvl_arr[1:*] & ia_arr=ia_arr[1:*]
  iw_f=n_elements(wvl_arr)-1  ;final iw wavelenght index
endif 

; making the correct wavelength arrays
if aats then begin
  wvl_arr=lambda
  iw_f=n_elements(lambda)-1
  ia_arr=findgen(n_elements(lambda))
  if keyword_set(angstrom) then begin ; if we use angstrom, set the new tau for the angstrom exponent
    wvl_ref=500.
    m=min(abs(wvl_ref-lambda),ref_ind)
    alpha=-1.5 ; angstrom coefficient
    for ii=0,n_elements(fl)-1 do begin
      tau_ref=tau[fl[ii],ref_ind]
      for jj=0,n_elements(lambda)-1 do begin
      tau[fl[ii],jj]=tau_ref*(lambda[jj]/lambda[ref_ind])^alpha
      endfor      
    endfor
  endif
endif
;[0.580000  ,   0.550000 ,    0.450000 ,    0.400000 ,    0.380000 ,    0.295300 ,    0.237800  ,   0.180800  ,   0.143300   ,  0.103200  ,  0.0668000  ,  0.0315000  ,  0.0247000]
;[  353.500    ,  380.000  ,    452.600  ,    499.400   ,   519.400 ,     605.800   ,   675.100 ,     779.100  ,    864.500  ,    1019.10   ,   1241.30   ,   1558.50  ,    2139.30]

;;plotting of map
if keyword_set(map) then begin
  set_plot, 'ps'
  loadct, 39,/silent
  device, /encapsulated
   device, /tt_font, set_font='Helvetica Bold'
   device, filename=arctas_dir+'nasa'+ll+date+ll+date+'_nasa_map.ps'
   device,/color,bits_per_pixel=8.
   device, xsize=40, ysize=40
  !p.multi=0
  
  maxlon=max(lon) & maxlat=max(lat) & minlon=min(lon) & minlat=min(lat)
  if minlon lt -180 then minlon=-180
  mlon=(maxlon+minlon)*0.5 & mlat=(minlat+maxlat)*0.5

  map_set,mlat,mlon,/grid,limit=[minlat,minlon,maxlat,maxlon],/label,title='ARCTAS on '+date
  map_continents,/usa,/hires,thick=2
  map_continents,/countries,/hires,/coast,thick=2

  oplot, lon, lat, psym=3
  oplot, lon[leg1], lat[leg1], psym=2, color=250
  if hsrl then begin
    oplot, lon_hsrl, lat_hsrl, psym=3, color=70
    oplot, lon_hsrl[fl_hsrl], lat_hsrl[fl_hsrl], psym=2, color=150
    legend,['P-3 flight path','P-3 aerosol leg','B-200 flight path','B-200 over flight leg'], textcolors=[0,250,70,150]
  endif else legend,['flight path', 'aerosol leg'],textcolors=[0,250]
  device,/close
  spawn,'convert '+arctas_dir+'nasa'+ll+date+ll+date+'_nasa_map.ps '+arctas_dir+'nasa'+ll+date+ll+date+'_nasa_map'+secon+'.png'
  spawn,'rm -f '+arctas_dir+'nasa'+ll+date+ll+date+'_nasa_map.ps'
endif

;setting the right filter array, using the total indices is not choose of find_legs
if not (keyword_set(choose) or keyword_set(find_legs)) then ind=findgen(n_elements(fl))
fl=fl[ind]

;;saving important pieces to a file 
if keyword_set(save) then begin
  nab=nabove & nbe=nbelow & zab=zabove & zbe=zbelow & dtaa=dtau_aats
  taa=tau_aats & nabove=nabove[*,fl] & nbelow=nbelow[*,fl]
  zabove=zabove[*,fl] & zbelow=zbelow[*,fl] 
  latabove=lat[near_leg2[fl]] & latbelow=lat[leg1[fl]]
  lonabove=lon[near_leg2[fl]] & lonbelow=lon[leg1[fl]]
  altabove=alt[near_leg2[fl]] & altbelow=alt[leg1[fl]]
  szaabove=sza[near_leg2[fl]] & szabelow=sza[leg1[fl]]
  utcabove=utc[near_leg2[fl]] & utcbelow=utc[leg1[fl]]
  tau_aats=tau[fl,*] & dtau_aats=dtau[fl,*] & lambda_aats=lambda & wvl=nadlambda
  save, nabove,nbelow,zabove,zbelow,latabove,latbelow,lonabove,lonbelow,$
   altabove,altbelow,szaabove,szabelow,utcabove,utcbelow,tau_aats,lambda_aats,wvl,dtau_aats,$
   filename='/home/leblanc/arctas/nasa/'+date+ll+date+'_spectra_save.out'
  nabove=nab & nbelow=nbe & zabove=zab & zbelow=zbe & tau_aats=taa & dtau_aats=dtaa
  if keyword_set(no_szacorr) then begin
    for i=0, n_elements(fl)-1 do begin
      zabove[*,i]=zab[*,fl[i]]*cos(sza_ref*!dtor)/cos(sza[near_leg2[fl[i]]]*!dtor)
      zbelow[*,i]=zbe[*,fl[i]]*cos(sza_ref*!dtor)/cos(sza[leg1[fl[i]]]*!dtor)
    endfor
  endif
  stop
endif

sza_arr=sza
fl_f=n_elements(fl)-1 ;number of points to go through
fl_s=0; start of points
print, 'number of wavelength to iterate:',iw_f
print, 'number of points to iterate:',n_elements(fl), ' points that will be calculated:',fl_f-fl_s

if unique then begin 
 fl_f=31
 fl_s=31
endif
;stop
corr_eps=0.90 ; suitable correction factors

if not keyword_set(use_wl) then iw_start=0 else begin
  iw_start=use_wl
  iw_f=use_wl
endelse

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;loops start;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
for iw=iw_start, iw_f do begin ;wavelength loop
  aero_wl=ia_arr[iw]
print, 'wavelenght at:',lambda[aero_wl]
;stop  
  if not no_tau_loop then file_n='rtm_taumod_' else file_n='rtm_'  ; set up file writing routine for retrieval (at every point)
  if keyword_set(angstrom) then file_n='rtm_angstrom_'
  if error_loop then file_n='rtm_error_notau_'
  if tau_approx then file_n='rtm_tauapprox_'
  if hsrl then file_n='rtm_hsrl_'
  if hist then file_n='rtm_hist_'
  if not unique then begin 
    lunn=98  
    openw, lunn, dirout+file_n+date+'_wvl'+strtrim(string(fix(lambda[aero_wl]),format='(I4.4)'),2)+secon+'.txt';,/get_lun
    printf, lunn, '#lat	 lon   ssa   asy   asy2    albedo     correction    tau modification    flux divergence	  model down	  model up	 tau    Ft_up   Ft_dn    Fb_up   Fb_dn'
  endif
  
  for i=fl_s, fl_f do begin ;lon loop
    
    index=fl[i]  ; index for the point along track
    
    error_count=0
    error_start:
    if error_loop then print, 'this is doing the error loop modification #:',error_count  ; do an error loop 
    
    print, 'aats tau:',reform(tau[index,*])
    print, 'aats wvl:',lambda
    if not finite(tau[index,aero_wl]) then begin
      ssa=!values.f_nan & old_ssa=ssa & asy=!values.f_nan & old_asy=asy
      asy=asy2 & albedo=ssa & correction=ssa & tau_mod=ssa
      tau_now=tau_mod & dif_f=ssa & count_big=8 & dn=ssa & up=ssa
      ft_up=ssa & ft_dn=ssa & fb_up=ssa & fb_dn=ssa
      goto, failed 
    endif else $
      write_input_aats, tau[index,aero_wl], topt, bottomt, tau_aats[near_leg2[index],aero_wl] ,$
       atm_in=atm_in, index=index, tau_out=tau_out, /quiet, tau_approx=tau_approx
      input_file=dir+'aero_'+date+secon+'.inp'
      output_file=dirout+'aero_'+date+secon+'.out'
    if tau_approx then begin
      tau[index,aero_wl]=round(tau[index,aero_wl]*100.)/100.0
      if tau[index,aero_wl] eq 0. then tau[index,aero_wl]=tau[index,aero_wl]+0.002
    endif
   
    ;sza=sza_arr[legs.leg1[index]]
    doy=julian_day(fix(strmid(date,0,4)),fix(strmid(date,4,2)),fix(strmid(date,6,2)))
    zensun, doy, utc[legs.leg1[index]],lat[legs.leg1[index]], lon[legs.leg1[index]], sza, azimuth, solfac
  
    if bottomt le 0. then bottomt=0.1
    zout=[0.,bottomt,topt] 
    
    if aeronet then  mm=min(abs(jul_day-doy+utc[legs.leg1[index]]/24.0),aeronet_index)  ;get the closest in time aeronet index
    
    wvl=[fix(wvl_arr[iw]),fix(wvl_arr[iw])]
    mm=max(zout, top)
    bottom=1
    mm=min(abs(nadlambda-wvl[0]),wvl_index)
    
    if aeronet then begin
      ;to find the tau aeronet at 532 (HSRL)
      tau_532=interpol(alog(aod_aeronet[ia_arr,aeronet_index]),alog(wvl_arr),alog(532.))
      tau_532=exp(tau_532)
      tauscale1=tau_s*aod_aeronet[aero_wl,aeronet_index]/tau_532
    endif else begin
      tauscale1=tau_s ;set it to the scaling factor only
    endelse
    
    ;starting values
    albedo   = near.nbelow[wvl_index, index] / near.zbelow_attcorr[wvl_index,index]
    z_top    = near.zabove_attcorr[wvl_index,index] 
    z_bottom = near.zbelow_attcorr[wvl_index,index]
    n_top    = near.nabove[wvl_index,index]
    n_bottom = near.nbelow[wvl_index,index] 
    correc_fac=1.
    
    ;saving of values
    Ft_up=n_top & Ft_dn=z_top & Fb_up=n_bottom & Fb_dn=z_bottom
    ; make loops that writes input file, then runs uvspec, then reads the output, then modifies the different values
    
    ;tau_loop
    tau_loop=1 & count_tau=0 & tau_mod=1.0 & tau_init=tauscale1 & tau_old=0. & redo_tau=0
      
    tau_mods=findgen(22)*0.04+0.6  ;iterate through +/- 40%
    tau_mods=tau_mods[sort(abs(tau_mods-1.))]
    tau_mods=tau_mods[1:*]
    while tau_loop do begin
      if count_tau eq 0 then begin
        tauscale=tau_init
        ssa_ct=1
      endif
      count_tau=count_tau+1
    
      big_loop=1 & count_big=0
      old_count_ssa=0 & old_count_asy=0

      ;start of big loop 
      while big_loop do begin
        count_big=count_big +1
        old_albedo=albedo & old_asy=asy & old_asy2=asy2 & old_ssa=ssa
    
	      ct_big_asy2=0
	  	big_asy2:
		
        ; ssa loop
        if not keyword_set(constant_ssa) then ssa_ct=1
        if ssa_ct then ssa_loop=1 else ssa_loop=0
        count_ssa=0
      
        while ssa_loop do begin  ;in the ssa loop
          count_ssa=count_ssa +1
          if not finite(ssa) then message,'SSA out of range';read, ssa, prompt='SSA out of range! please enter new SSA (0-1): ('+strtrim(string(ssa),2)+') '
          write_input_file, doy, sza, dir+tau_out, dir+atm_in, input_file, azimuth, asy, ssa, wvl, zout,albedo, /radiance,/quiet, tau_scale=tauscale, /slit ;write input file, then run uvspec
          if linux then spawn, '/home/leblanc/libradtran/libRadtran-1.6-beta/bin/uvspec < '+input_file+' > '+output_file , message else message, 'Must be under linux'
          if message ne '' gt 0 then begin
            if message eq 'Segmentation fault' then spawn, '/home/leblanc/libradtran/libRadtran-1.6-beta/bin/uvspec < '+input_file+' > '+output_file,message
            if message ne '' gt 0 then message, message
          endif
          output=libradtran_reader(file = output_file,/radiance,zout = zout,/quiet)
      
          ; get correction factor from the modeled downward irradiance (top) to the measured downward irradiance (top)
          correction=(output.dir_dn[top] + output.dif_dn[top])/near.zabove_attcorr[wvl_index,index]
          z_top    = near.zabove_attcorr[wvl_index,index] 
          z_bottom = near.zbelow_attcorr[wvl_index,index]
          n_top    = near.nabove[wvl_index,index]
          n_bottom = near.nbelow[wvl_index,index] 
          
          ;correct measurements to fit into model
          z_top    = z_top * correction
          z_bottom = z_bottom  * correction
          n_top    = n_top * correction
          n_bottom = n_bottom * correction
          correc_fac=correc_fac*correction
          
          ;to correct the albedo
          model_albedo=output.dif_up[bottom]/(output.dir_dn[bottom]+output.dif_dn[bottom])
          albedo_correction=(n_bottom/z_bottom)/model_albedo
          albedo=albedo*albedo_correction
          if not finite(albedo) then albedo=old_albedo
          if albedo gt 1.0 then albedo =1.0

          ; change ssa for next iteration
          model=-(output.dif_up[top] - (output.dir_dn[top] + output.dif_dn[top]))+(output.dif_up[bottom] - (output.dir_dn[bottom] + output.dif_dn[bottom]))
          measured=-(n_top-z_top)+(n_bottom-z_bottom)
      
          dif_f=model

          if abs(model-measured) lt 0.001 or abs(model/measured) lt 0.000001 and correction gt corr_eps then begin ;if within epsiolon of model and measured then proceed
            ssa_loop=0  ;converged
          endif else begin
            if model/measured lt 0 || correction le corr_eps || correction gt 1.02 then begin 
              ssa_loop=0
              ssa=!values.f_nan & asy=!values.f_nan
              dn=ssa & up=ssa & tau_mod=ssa & tau_now=tau_mod
              goto, failed
            endif else ssa=ssa*(model/measured)^0.08  ;modify ssa for next value
            if not finite(ssa) then begin ;check to make sure the modification did not create a NaN
              if model gt 0 then print, 'model is off' , model
              if measured gt 0 then print, 'measured is off', measured
              stop
              ssa_loop=0 & big_loop=0
              asy=!values.f_nan & asy2=!values.f_nan
              goto, failed
            endif
            if ssa gt 1. then ssa=1.0
            if ssa le 0. then ssa=0.000001
          endelse
      
          if count_ssa gt 30 then begin
            ssa_loop=0  ;did not converge
            ;print, 'did not converge'
            ;print, count_ssa
          endif
          if correction gt 2. then message, 'Correction great than 2 at ssa loop'
        endwhile  ;end of ssa loop 
      if count_ssa gt 30 then print, 'ssa did not converge for index: '+strtrim(string(i),2)

      ; asy loop
      asy_loop=1 & count_asy=0
      
      while asy_loop do begin
        count_asy=count_asy +1
        if not finite(asy) then read, asy, prompt='ASY out of range! please enter new ASY (0-1):'
        write_input_file, doy, sza, dir+tau_out, dir+atm_in, input_file, azimuth, asy, ssa, wvl, zout,albedo, /radiance,/quiet, tau_scale=tauscale, /slit
        if linux then spawn, '/home/leblanc/libradtran/libRadtran-1.6-beta/bin/uvspec < '+input_file+' > '+output_file,message else message, 'Must be under linux'
        if message ne '' gt 0 then begin
          if message eq 'Segmentation fault' then spawn, '/home/leblanc/libradtran/libRadtran-1.6-beta/bin/uvspec < '+input_file+' > '+output_file,message
          if message ne '' gt 0 then message, message
        endif
        output=libradtran_reader(file = output_file,/radiance,zout = zout,/quiet)
      
        ; get correction factor from the modeled downward irradiance (top) to the measured downward irradiance (top)
        correction=(output.dir_dn[top] + output.dif_dn[top])/near.zabove_attcorr[wvl_index,index]
        z_top    = near.zabove_attcorr[wvl_index,index] 
        z_bottom = near.zbelow_attcorr[wvl_index,index]
        n_top    = near.nabove[wvl_index,index]
        n_bottom = near.nbelow[wvl_index,index] 
        
        ;correct measurements to fit into model
        z_top    = z_top * correction
        z_bottom = z_bottom  * correction
        n_top    = n_top * correction
        n_bottom = n_bottom * correction
        correc_fac=correc_fac*correction
        
        ;to correct the albedo
        model_albedo=output.dif_up[bottom]/(output.dir_dn[bottom]+output.dif_dn[bottom])
        albedo_correction=(n_bottom/z_bottom)/model_albedo
        albedo=albedo*albedo_correction
        if not finite(albedo) then albedo=albedo_t1  ; set albedo back to initial value for non finite values (for next guess)
        if albedo gt 1.0 then albedo=1.0 ;make sure albedo doesn't go over 1.0
		 
        ; change asy for next iteration
        model=(output.dir_dn[bottom] + output.dif_dn[bottom])
        measured=z_bottom
        dn=model
         
        if abs(model/measured) lt 0.000001 or abs(model-measured) lt 0.008*measured and correction gt corr_eps then begin
          asy_loop=0  ;converged
        endif else begin
          if correction le corr_eps || correction gt 1.02 then begin 
              asy_loop=0
              ssa=!values.f_nan & asy=!values.f_nan
              dn=ssa & up=ssa & tau_mod=ssa & tau_now=tau_mod
              goto, failed
          endif
          asy_t=0
          if not asy_t then asy=asy*(measured/model)^1. else read, asy, prompt='asy:'
          if asy ge 0.98 then asy=0.9799
          if asy le 0. then asy=0.000001
        endelse
        if count_asy gt 30 then asy_loop=0  ;did not converge
      endwhile  ;end of asy loop
      if count_asy gt 30 then print, 'asy did not converge for index: '+strtrim(string(i),2)
            
      ; asy second loop to help retrieve tau
      asy2_loop=1 & count_asy2=0
    
      while asy2_loop do begin
        count_asy2=count_asy2 +1
        if not finite(asy) then read, asy2, prompt='ASY2 out of range! please enter new ASY2 (0-1):'
        write_input_file, doy, sza, dir+tau_out, dir+atm_in, input_file, azimuth, asy2, ssa, wvl, zout,albedo, /radiance,/quiet, tau_scale=tauscale, /slit
        if linux then spawn, '/home/leblanc/libradtran/libRadtran-1.6-beta/bin/uvspec < '+input_file+' > '+output_file,message else message, 'Must be under linux'
        if message ne '' gt 0 then begin
          if message eq 'Segmentation fault' then spawn, '/home/leblanc/libradtran/libRadtran-1.6-beta/bin/uvspec < '+input_file+' > '+output_file,message
          if message ne '' gt 0 then message, message
        endif
        output=libradtran_reader(file = output_file,/radiance,zout = zout,/quiet)
    
        ; get correction factor from the modeled downward irradiance (top) to the measured downward irradiance (top)
        correction=(output.dir_dn[top] + output.dif_dn[top])/near.zabove_attcorr[wvl_index,index]
        z_top    = near.zabove_attcorr[wvl_index,index] 
        z_bottom = near.zbelow_attcorr[wvl_index,index]
        n_top    = near.nabove[wvl_index,index]
        n_bottom = near.nbelow[wvl_index,index] 
        
        ;correct measurements to fit into model
        z_top    = z_top * correction
        z_bottom = z_bottom  * correction
        n_top    = n_top * correction
        n_bottom = n_bottom * correction
        correc_fac=correc_fac*correction
        
        ;to correct the albedo
        model_albedo=output.dif_up[bottom]/(output.dir_dn[bottom]+output.dif_dn[bottom])
        albedo_correction=(n_bottom/z_bottom)/model_albedo
        albedo=albedo*albedo_correction
        if not finite(albedo) then albedo=albedo_t1
        if albedo gt 1.0 then albedo=1.0

        ; change asy2 for next iteration
        model=output.dif_up[top]
        measured=n_top
    
        up=model

        dif_f=(-(output.dif_up[top] - (output.dir_dn[top] + output.dif_dn[top]))+(output.dif_up[bottom] - (output.dir_dn[bottom] + output.dif_dn[bottom])))/correction
        if abs(model/measured) lt 0.000001 or abs(model-measured) lt 0.002 and correction gt corr_eps then begin
          asy2_loop=0  ;converged
        endif else begin
          if correction le corr_eps || correction gt 1.02 then begin 
            asy2_loop=0
            ssa=!values.f_nan & asy=!values.f_nan
            dn=ssa & up=ssa & tau_mod=ssa & tau_now=tau_mod
            goto, failed
          endif
          asy2=asy2*(model/measured)^1.0
          if asy2 ge 0.98 then asy2=0.9799
          if asy2 le 0. then asy2=0.000001
        endelse
        if count_asy2 gt 30 then begin
          asy2_loop=0  ;did not converge
        endif

      endwhile  ;end of asy2 loop
      if count_asy2 gt 30 then begin
	      print, 'asy2 did not converge for index: '+strtrim(string(i),2)
          if ct_big_asy2 gt 1 then begin
		        print, 'Convergence did not happen because asy2 did not converge on second try'
	          goto, outside			
		      endif else begin 
		        if count_asy lt 30 then begin
  		        ct_big_asy2= ct_big_asy2 +1
		          ;goto, big_asy2
		        endif else ct_big_asy2=0
	        endelse
      endif else ct_big_asy2=0
	  
      print, 'Iteration of big loop: '+strtrim(string(count_big),2)+' on index: '+strtrim(string(i),2)+'/'+strtrim(string(fl_f),2)+' wavelength: '+strtrim(string(wvl[0]),2)
      print, 'ssa count, ssa'
      print, count_ssa, ssa
      print, 'asy count, asy'
      print, count_asy, asy
      print, 'asy2 count, asy2'
      print, count_asy2, asy2
      print, 'correction factor: '+strtrim(string(correction),2)
      print, 'tau: ',tau_mod*tau_init, '  actual tau:', tau[index,aero_wl]*tau_mod*tau_init, '  - end big loop'

      tau_now=tau[index,aero_wl]*tau_mod*tau_init
      if abs(ssa-old_ssa) lt 0.01 and abs(asy-old_asy) lt 0.01 and count_ssa lt 31 then big_loop=0   ;check for convergence of big loop
      if count_big gt 6 then big_loop=0   ;no convergence
      if old_count_ssa eq 31 and count_ssa eq 31 and old_count_asy eq 31 and count_asy eq 31 then begin
	    ;print, 'Convergence failed twice in a row'
		big_loop=0
		;if tau_mod eq 1.0 then begin    ;save first values, for when the tau retrieval fails
        ; asy_t1=asy & asy2_t1=asy2 & ssa_t1=ssa & albedo_t1=albedo
        ;endif 
		;tau_mod=!values.f_nan & asy=!values.F_NAN & asy2=!values.F_NAN
        ;ssa=!values.f_nan & tau_now=tau_mod & tau_loop=0
        ;goto, failed
      endif
	  
	  old_count_ssa=count_ssa & old_count_asy=count_asy
	  if count_ssa lt 31 then ssa_ct=0
    endwhile  ;end if big_loop
    ssa_ct=1
    if count_big gt 6 then begin
      print, 'convergence did not happen during big loop for index: '+strtrim(string(index),2)
      tau_mod=!values.f_nan & asy=!values.F_NAN & asy2=!values.F_NAN
      ssa=!values.f_nan & tau_now=tau_mod & tau_loop=0
      goto, failed
    endif

    if abs(asy-asy2) gt 0.5 then begin
      print, 'divergence of asy and asy2 too large, retrieval failed: ',abs(asy-asy2) 
      tau_mod=!values.f_nan & asy=!values.F_NAN &  asy2=!values.F_NAN
      ssa=!values.f_nan & tau_now=tau_mod &  tau_loop=0
      goto, failed
    endif
      
	  outside:
    if tau_mod eq 1.0 then begin    ;save first values, for when the tau retrieval fails
      asy_t1=asy & asy2_t1=asy2 & ssa_t1=ssa & albedo_t1=albedo
	  ;asy_t1=!values.f_nan & asy2_t1=!values.f_nan & ssa_t1=!values.f_nan & albedo_t1=!values.f_nan
    endif 
    if no_tau_loop then tau_loop=0
	  
	if ct_big_asy2 gt 1 and count_asy lt 30 then begin
	  tau_mod=!values.f_nan & asy=!values.F_NAN &  asy2=!values.F_NAN
      ssa=!values.f_nan & tau_now=tau_mod &  tau_loop=0
      goto, failed
	endif
	
    if keyword_set(sensitivity) then begin  ;to write values of asy, asy2 compared to change of tau
      openw, 94, dirout+'tau_'+date+'_ssact_wvl'+strtrim(string(fix(lambda[aero_wl]),format='(I4.4)'),2)+'.dat',/append
      printf, 94, tau_mod, tau_now, ssa, asy, asy2, correction
      close, 94	  
    endif
	    
	  ; part of code experimental, to get convergence of asy2 and asy2 by modifying tau slightly
    if (abs(asy-asy2) lt 0.02) and not keyword_set(sensitivity) and count_ssa lt 31 and count_asy lt 31 then begin
      print, 'tau converged at:', tau_mod*tau_init, ' with tau equals:',tau_now
      if keyword_set(constant_ssa) then begin
        ssa_ct=1      ; redo ssa
        tau_loop=1    ; go back to redo tau
        redo_tau=redo_tau+1
        print, 'in constant ssa tau loop, with tau-now, tau_old, diff:',tau_now,tau_old, abs(tau_now-tau_old)
        
        if abs(tau_now-tau_old) lt 0.04*tau_now then begin 
          tau_loop=0  ;exit new tau loop
        endif
        tau_old=tau_now
      endif else begin
        tau_loop=0 ; exit old tau loop
      endelse
    endif else begin
      if not no_tau_loop then begin 
        ssa=0.9 & asy = 0.6 & asy2 = 0.6
        if keyword_set(constant_ssa) then ssa=ssa_t1
        tau_mod=tau_mods[count_tau-redo_tau-1]
      endif else tau_mod=1.0
      tauscale=tau_init*tau_mod
    endelse
    if count_tau gt 20 then begin
      tau_loop=0
      print, 'tau did not converge for index: '+strtrim(string(index),2)
      tau_mod=!values.f_nan & tau_now=tau_mod & asy=asy_t1
      asy2=asy2_t1 & ssa=ssa_t1 & albedo=albedo_t1 & tau_loop=0
      goto, failed
    endif        
  endwhile   ; end of tau loop
  
  failed:
  ssa_arr[i]=ssa & asy_arr[i]=asy & asy2_arr[i]=asy2
  albedo_arr[i]=albedo & correc_fac_arr[i]=correction
  tau_mod_arr[i]=tau_mod & tau_arr[i]=tau_now & dif_flux[i] = dif_f
  Ft_up_arr[i]=ft_up & Ft_dn_arr[i]=ft_dn & Fb_up_arr[i]=fb_up & Fb_dn_arr[i]=fb_dn
  if count_big gt 6 then begin
    print, 'convergence did not happen during big loop for index: '+strtrim(string(index),2)
    ssa=.9 & asy=0.6 & asy2=asy
  endif
  ssa=.9 & asy=0.6 & asy2=asy
  if keyword_set(constant_ssa) then ssa=ssa_t1
  ; file writing routine for retrieval results
  if not unique then printf, lunn, lat[legs.leg1[fl[i]]],lon[legs.leg1[fl[i]]], ssa_arr[i], asy_arr[i], asy2_arr[i], albedo_arr[i], correc_fac_arr[i],tau_mod_arr[i], dif_flux[i],dn,up,tau_arr[i],Ft_up_arr[i],Ft_dn_arr[i],Fb_up_arr[i],Fb_dn_arr[i], format='(16F)' else begin
   openu, lunun, dirout+'rtm_unique_'+date+secon+'.txt',/append
   printf, lunun, lat[legs.leg1[fl[i]]],lon[legs.leg1[fl[i]]], lambda[aero_wl], ssa_arr[i], asy_arr[i], asy2_arr[i], albedo_arr[i], correc_fac_arr[i],tau_arr[i],tau_scale, tau[index,aero_wl],dif_flux[i],dn,up,Ft_up_arr[i],Ft_dn_arr[i],Fb_up_arr[i],Fb_dn_arr[i], format='(18F)' 
   free_lun, lunun
  endelse
 if error_loop then begin   ; error loop details
    case error_count of 
      0:begin
        ft_up0=ft_up & ft_dn0=ft_dn & fb_up0=fb_up & fb_dn0=fb_dn ;ref values
        error_count=error_count+1
        goto, error_start
      end
      
      1:begin
        near.zabove_attcorr[wvl_index,index] = ft_dn0
        near.zbelow_attcorr[wvl_index,index] = fb_dn0
        near.nabove[wvl_index,index]         = ft_up0
        near.nbelow[wvl_index,index]         = fb_up0+fb_up0*0.02
        error_count=error_count+1
        goto, error_start
      end

      2:begin
        near.zabove_attcorr[wvl_index,index] = ft_dn0
        near.zbelow_attcorr[wvl_index,index] = fb_dn0
        near.nabove[wvl_index,index]         = ft_up0
        near.nbelow[wvl_index,index]         = fb_up0-fb_up0*0.02
        error_count=error_count+1
        goto, error_start
      end
            
      3:begin
        near.zabove_attcorr[wvl_index,index] = ft_dn0
        near.zbelow_attcorr[wvl_index,index] = fb_dn0+fb_dn0*0.02
        near.nabove[wvl_index,index]         = ft_up0
        near.nbelow[wvl_index,index]         = fb_up0
        error_count=error_count+1
        goto, error_start
      end
    
      4:begin
        near.zabove_attcorr[wvl_index,index] = ft_dn0
        near.zbelow_attcorr[wvl_index,index] = fb_dn0-fb_dn0*0.02
        near.nabove[wvl_index,index]         = ft_up0
        near.nbelow[wvl_index,index]         = fb_up0
        error_count=error_count+1
        goto, error_start
      end
      
      5:begin
        near.zabove_attcorr[wvl_index,index] = ft_dn0
        near.zbelow_attcorr[wvl_index,index] = fb_dn0
        near.nabove[wvl_index,index]         = ft_up0+ft_up0*0.02
        near.nbelow[wvl_index,index]         = fb_up0
        error_count=error_count+1
        goto, error_start
      end
      
      6:begin
        near.zabove_attcorr[wvl_index,index] = ft_dn0
        near.zbelow_attcorr[wvl_index,index] = fb_dn0
        near.nabove[wvl_index,index]         = ft_up0-ft_up0*0.02
        near.nbelow[wvl_index,index]         = fb_up0
        error_count=error_count+1
        goto, error_start
      end
      
      7:begin
        near.zabove_attcorr[wvl_index,index] = ft_dn0+ft_dn0*0.02
        near.zbelow_attcorr[wvl_index,index] = fb_dn0
        near.nabove[wvl_index,index]         = ft_up0
        near.nbelow[wvl_index,index]         = fb_up0
        error_count=error_count+1
        goto, error_start
      end
      
      8:begin
        near.zabove_attcorr[wvl_index,index] = ft_dn0-ft_dn0*0.02
        near.zbelow_attcorr[wvl_index,index] = fb_dn0
        near.nabove[wvl_index,index]         = ft_up0
        near.nbelow[wvl_index,index]         = fb_up0
        error_count=error_count+1
        goto, error_start
      end
      
      9:begin
        print, '**** error loop finished ****'
      end
    endcase
  endif
endfor  ;longitude loop
 if not unique then free_lun, lunn ;retrieval output file
endfor ; wavelength loop  
endwhile ; unique loop
if unique then free_lun, lunun ; unique retrieval output file
stop
end
  
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write input file program                                                    ;;
;; used to make the input file for uvspec                                      ;;
;; use radiance keyword to set radiance (zenith pointing)                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro write_input_file, doy, sza, tau_file, atm_file, input_file, azimuth, asy, ssa, wvl, zout, albedo, radiance=radiance, quiet=quiet, tau_scale=tau_scale, slit=slit

ui=99
openw,ui,input_file;,/get_lun
printf,ui,'data_files_path   /home/leblanc/libradtran/libRadtran-1.6-beta/data'
printf,ui,'solar_file        /home/leblanc/libradtran/libRadtran-1.6-beta/data/solar_flux/kurudz_1.0nm.dat' 
printf,ui,'atmosphere_file   '+atm_file

;printf,ui,'albedo_library    IGBP    # Spectral albedo library '
;printf,ui,'surface_type      13      # urban albedo from library'
if albedo lt 0 then albedo=albedo*(-1.)
if albedo gt 1.0 then albedo=1.0
if albedo lt 0.00001 then albedo=0.00001
printf,ui,'albedo            '+string(albedo)  ; 

printf,ui,'rte_solver        disort2'       
printf,ui,'nstr              20'            
printf,ui,'correlated_k      SBDART'        ; pseudo-spectral definition of atmospheric absorption

printf,ui,'sza               '+string(sza)    ; solar zenith angle (deg)
printf,ui,'day_of_year       '+string(doy)    ; day of year for Sun-Earth distance

if keyword_set(radiance) then begin
  printf,ui,'phi0              '+string(azimuth+180.0); solar azimuth angle (deg)
  printf,ui,'umu               -1  # -1:downward radiance; 1:upward radiance; 0:sidward radiance' ;for radiances (ship)
  printf,ui,'phi               270.0   # output azimuth angle (flight-direction: NNW = 350°)' ;for radiances(ship direction)
endif

printf,ui,'aerosol_default'                   ; initialize aerosol
printf,ui,'aerosol_tau_file  '+tau_file
printf,ui,'aerosol_set_ssa   '+string(ssa)  
printf,ui,'aerosol_set_gg    '+string(asy)      
if keyword_set(tau_scale) then printf, ui, 'aerosol_scale_tau   '+string(tau_scale)

printf,ui,'wavelength        '+string(wvl[0])+'  '+string(wvl[1]) ; wavelength used for hsrl
printf,ui,'altitude          0.0' ;0.263 # elevation (ASL) of CalTech in km'      
printf,ui,'zout              '+string(zout, format='('+strtrim(string(n_elements(zout)),2)+'(" ",F5.1))')

if keyword_set(slit) then begin ; if set, use slit function, must change for IngAas
  if wvl[0] le 940 then printf,ui,'slit_function_file /home/leblanc/libradtran/vis_1nm.dat' else printf,ui,'slit_function_file /home/leblanc/libradtran/nir_1nm.dat'
endif

printf,ui,'quiet'
free_lun,ui

if not keyword_set(quiet) then print, 'input file saved to: '+input_file

end

