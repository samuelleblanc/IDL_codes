;+
; NAME:
;   calnex_oil
;
; PURPOSE:
;   Gulf of mexico Oil measurements during CALNEX
;
; CATEGORY:
;   Gulf / CALNEX 
;
; CALLING SEQUENCE:
;   calnex_oil
;
; OUTPUT:
;   plot of albedo and zenith irradiance
;   
;
; KEYWORDS:
;   SSFR, P3, CALNEX, Oil
;
; DEPENDENCIES:
;   legend.pro    ;program to make the legend;   config.pro	  ;program to read in the config file
;
; NEEDED FILES:
;   - SP.out files for the day
;   - config file for the day
;
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
; Written:  Samuel LeBlanc, LASP CU Boulder, June 10th, 2010
; Modified: June 14th, 2010
;           -by Samuel LeBlanc
;           added the flight date 20100610
; Modified: June 16th, 2010
;           -by Samuel LeBlanc
;           added slope determination feature with uncertainty (to find a way to get the oil retrieval)
;           added plotting of slope
;   
;---------------------------------------------------------------------------

@legend.pro
@cfg.pro

pro calnex_oil, date_inp, png=png, ps=ps, bp_oil=bp_oil, interactive=interactive

if n_elements(date_inp) lt 1 then begin
  date='20100608'
endif else begin
  date=strcompress(string(date_inp),/REMOVE_ALL)
endelse

  dir='/data/seven/schmidt/calnex/p3/'
  ll='/' ; directory separator

if keyword_set(png) then png=1 else png=0
if keyword_set(ps)  then ps =1 else ps=0
if keyword_set(bp_oil) then bp_oil=1 else bp_oil=0
if keyword_set(interactive) then interactive=1 else interactive=0

if ~ps and ~png then png=1

; initialize configuration file
cfg_file=dir+ll+date+ll+date+'.cfg'        ; build cfg file
if file_test(cfg_file) ne 1 then message,'Did not find configuration file.'
plot=cfg(cfg_file,'plot')
; get platform
if strcmp(strmid(plot,0,1),'y',/FOLD_CASE) then plot=1 else plot=0
;plot=0
platform=cfg(cfg_file,'platform')
platform='NOAAP3'
print,'Process flight from '+date+' onboard '+platform+'.'
c0=cfg(cfg_file,'comment')
if not strcmp(c0,'#') then begin
  print,c0
  comment=c0
endif
; get extraterrestial flux
extra=cfg(cfg_file,'extra') ; extra-terrestial flux
; get time range
i0=cfg(cfg_file,'interval') ; look if we should only use data within a certain time window
uu0=0
if not strcmp(i0,'#') then begin
    uu=strsplit(i0,' ,',escape='#',/EXTRACT)
    uu=float(uu)
    uu0=1
    u0=uu[0] & u1=uu[1]
endif


case date of
  '20100608' : begin
    SSFR_path=dir+'20100608/20100608_SP.out'
    pathout=dir+'20100608/20100608_oil.out'
  end

  '20100610' : begin
    SSFR_path=dir+'20100610/20100610_SP.out'
    pathout=dir+'20100610/20100610_oil.out'
  end

  else: begin
    message, 'not a correct date'
  end
endcase

restore, SSFR_path			;restore ssfr file

;; filter the data

izamax=cfg(cfg_file,'izamax')
if strmid(izamax,0,1) eq '#' then izamax=-1 else izamax=float(izamax)
if izamax gt 0 then flt=where(utc ge uu[0] and utc le uu[1] and alt lt 500.0,nf) else flt=where(utc ge uu[0] and utc le uu[1] and alt lt 500.0,nf)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Plotting of albedos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
if ps then begin
set_plot, 'ps'
device, filename=dir+date+ll+date+'_albedo_'+platform+'.ps', /encapsulated, /tt_font, set_font='Helvetica Bold' 
device, /color, bits_per_pixel=8
device, xsize=36, ysize=36
endif
!p.multi=[0,1,2]
        
loadct, 33
  tvlct, r,g,b, /get
  r[0]=255 & g[0]=255 & b[0]=255
  r[255]=0 & g[255]=0 & b[255]=0
  tvlct,r,g,b
  !p.charsize=1.8
  !p.color=255
  !p.background=0  
  !p.thick=3
  device, decomposed=0

      
if png then window,9,xsize=1000,ysize=1000,retain=2,tit='Albedo'
  ut0=max([u0,min(utc)])
  count = 0
  num_plots = (fix(max(utc)-ut0))*4+1
  data_times = findgen(num_plots)/4. + ut0
  linestyle=0

  ut0=max([u0,min(utc)])
  while ut0 lt max(utc) do begin
    mm=min(abs(ut0-utc),ind)
    ind=ind[0]
    hh=fix(utc[ind]) & mm=fix( (utc[ind]-fix(utc[ind]))*60.+0.5)
    tim=strcompress(string(hh,format='(i2.2)'),/remove_all)+strcompress(string(mm,format='(i2.2)'),/remove_all)

    nsp=interpol(nspectra[*,ind],nadlambda,zenlambda)
    if alt[ind] lt 200.0 then begin
      if(count eq 0) then begin
        plot,zenlambda,(nsp*dn)/(zspectra[*,ind]*fac[ind]),tit=platform+' '+date+' SSFR Albedo',xtit='wavelength [nm]',ytit='Albedo',xr=[350,1300],yr=[0,0.2],color=255,linestyle=0,/xs,/ys
        textcolor=255
        linestyle=0
        leg      =strmid(tim,0,2) + ':' +  strmid(tim,2,2) + ' UTC'
        indices=ind
     endif else begin
        oplot,zenlambda,(nsp*dn)/(zspectra[*,ind]*fac[ind]),color=count*25,linestyle=(count mod 6)
        textcolor=[textcolor,count*25]
        linestyle=[linestyle,(count mod 6)]
        leg      =[leg,strmid(tim,0,2) + ':' +  strmid(tim,2,2) + ' UTC']
        indices=[indices,ind]
      endelse
           
      count += 1
    endif
    ut0=ut0+0.25
  endwhile

        

  ut0=max([u0,min(utc)])
  count=0
  while ut0 lt max(utc) do begin
    mm=min(abs(ut0-utc),ind)
    ind=ind[0]
    hh=fix(utc[ind]) & mm=fix( (utc[ind]-fix(utc[ind]))*60.+0.5)
    tim=strcompress(string(hh,format='(i2.2)'),/remove_all)+strcompress(string(mm,format='(i2.2)'),/remove_all)

    if alt[ind] lt 200.0 then begin
      if(count eq 0) then begin
        plot,zenlambda,zspectra[*,ind]*fac[ind],tit='SSFR Downwelling',xtit='wavelength [nm]',ytit='Irradiance [W m!E-2!N nm!E-1!N]',xr=[350,1300],yr=[0,1.8],color=255,linestyle=0,/xs,/ys, thick=4
        textcolor=255
        linestyle=0
        leg      =strmid(tim,0,2) + ':' +  strmid(tim,2,2) + ' UTC'
      endif else begin
        oplot,zenlambda,zspectra[*,ind]*fac[ind],color=count*25,linestyle=(count mod 6)
        textcolor=[textcolor,count*25]
        linestyle=[linestyle,(count mod 6)]
        leg      =[leg,strmid(tim,0,2) + ':' +  strmid(tim,2,2) + ' UTC']
      endelse
           
      count += 1
    endif
    ut0=ut0+0.25
  endwhile
  legend,leg,textcolors=textcolor,linestyle=linestyle,/right,colors=textcolor,outline_color=255
        
if png then begin
  p=tvrd(true=1)
  write_png,dir+date+ll+date+'_albedo_'+platform+'.png',p
endif

if ps then begin
  device, /close
  spawn, 'convert '+dir+date+ll+date+'_albedo_'+platform+'.ps '+dir+date+ll+date+'_albedo_'+platform+'.png'
  spawn, 'rm '+dir+date+ll+date+'_albedo_'+platform+'.ps'
  ; spawn, 'display '+dir+date+ll+date+'_albedo_'+platform+'.png &'
endif

  ut0=max([u0,min(utc)])
  ux=where(utc ge ut0,nx)

  xaxis=utc[ux]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; plotting of flight track
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  lats  =[27.9730,39.9088, 34.05, 34.051,32.685 , 28.7366, 30.35, 29.25]
  lons  =[-82.535,-105.117,-118.25, -117.6117, -117.134, -88.3871, -87.32, -89.25]
  label =['Tampa','Jeffco','Los Angeles','Ontario Airport','San Diego', 'BP Oil Rig', 'Pensacola', 'Louisiana' ]
  nlabel=n_elements(lats)

if ps then begin
  device, filename=dir+date+ll+date+'_map_oil_'+platform+'.ps', /encapsulated, /tt_font, set_font='Helvetica Bold' 
  device, /color, bits_per_pixel=8
  device, xsize=36, ysize=20
endif


if png then begin
  siz=get_screen_size()
  window,5,retain=2,tit='Flight Chart',xs=siz[0],ys=siz[1]*0.8
endif  
!P.multi=0
!p.font=1
!x.thick=2
!y.thick=2


A = FINDGEN(17) * (!PI*2/16.)
USERSYM, COS(A), SIN(A), /FILL

  maxlon=max(lon[flt]) & maxlat=max(lat[flt])
  minlon=min(lon[flt]) & minlat=min(lat[flt])  if minlon lt -180 then minlon=-180
  
  if bp_oil then begin
    mlon=lons[5]
    mlat=lats[5]
  endif else begin
    mlon=(maxlon+minlon)*0.5
    mlat=(minlat+maxlat)*0.5
  endelse
  rlon=maxlon-mlon & rlat=maxlat-mlat
  rang=max([rlon,rlat])

  
  if bp_oil then $
    map_set, mlat, mlon, limit=[mlat-0.3,mlon-0.6,mlat+0.4,mlon+0.6] else $
    map_set,mlat,mlon,limit=[minlat-0.5,minlon-0.5,maxlat+0.5,maxlon+0.5];,/grid,/label
    
  map_continents,/coast,/hires,thick=3

  ; plotting of flight track
  oplot,lon,lat,psym=3
  xyouts,mlon-rang*0.9,mlat+rang*0.9,date,charsize=2

  mxa=max(alt)
  ngeo=n_elements(alt)
  utc0=0.
    
  mm=min(abs(zenlambda-450.),wl_450)
  mm=min(abs(zenlambda-550.),wl_550)
  mm=min(abs(zenlambda-650.),wl_650)
  mm=min(abs(zenlambda-900.),wl_end)
  mm=min(abs(zenlambda-400.),wl_start)
  cl_hist=[[0],[0],[0]]
  min_r=0.026 & max_r=0.157
  min_g=0.037 & max_g=0.165
  min_b=0.040 & max_b=0.160
  
  
  slope=fltarr(ngeo,2)  ; two element array that contains 
  good=[0]              ; array of indices containing the points lower than 500m altitude
  
  for i=0,ngeo-2 do begin
    nsp=interpol(nspectra[*,i],nadlambda,zenlambda)
    rouge=(nsp[wl_650]*dn)/(zspectra[wl_650,i]*fac[i])
    if rouge lt min_r then rouge=min_r
    if rouge gt max_r then rouge=max_r
    rouge=(rouge-min_r)/(max_r-min_r)   
    rouge=fix(255.*rouge)
    
    vert=(nsp[wl_550]*dn)/(zspectra[wl_550,i]*fac[i])
    if vert lt min_g then vert=min_g
    if vert gt max_g then vert=max_g
    vert=(vert-min_g)/(max_g-min_g)
    vert=fix(255.*vert)
    
    bleu=(nsp[wl_450]*dn)/(zspectra[wl_450,i]*fac[i])
    if bleu lt min_b then bleu=min_b
    if bleu gt max_b then bleu=max_b
    bleu=(bleu-min_b)/(max_b-min_b)
    bleu=fix(255.*bleu)

    cl=[[rouge],[vert],[bleu]]
    tvlct, cl, 100
    cl_hist=[cl_hist,[[rouge],[vert],[bleu]]]
    if alt[i] lt 500.0 then begin 
      oplot,lon[i:i+1],lat[i:i+1],color=100,psym=8, symsize=1
    
      albedo=(nsp[*]*dn)/(zspectra[*,i]*fac[i])
    
      rg = linfit(zenlambda[wl_start:wl_end],albedo[wl_start:wl_end], sigma=ss)
      slope[i,0]=rg[1]
      slope[i,1]=ss
      good=[good,i]
    endif
    
  endfor
  
  good=good[1:*]
  red=cl_hist[1:*,0] 
  green=cl_hist[1:*,1]
  blue=cl_hist[1:*,2]
  
  case date of
  
  '20100608': begin
    ;find the correct red coloured points
    i_r=6383
    tvlct, red[i_r],green[i_r],blue[i_r],100
    ss=5 ;fix(5.*(float(red[i_r])/float(blue[i_r]))-1)
    oplot,lon[i_r:i_r+1],lat[i_r:i_r+1],color=100,psym=8, symsize=ss

    ;find the correct green coloured points
    i_g=10290
    tvlct, red[i_g],green[i_g],blue[i_g],100
    ss=5 ;fix(4.*(float(green[i_g])/float(blue[i_g]))-1)
    oplot,lon[i_g:i_g+1],lat[i_g:i_g+1],color=100,psym=8, symsize=ss

    ;find the correct black coloured points
    i_k=9050
    tvlct, red[i_k],green[i_k],blue[i_k],100
    ss=5
    oplot,lon[i_k:i_k+1],lat[i_k:i_k+1],color=100,psym=8, symsize=ss
    
    xr=[18,23.5]
    yr=[-0.00025,0.0005]
  end
  
  '20100610': begin
    print, 'Using colored points from flight 20100608'
    i_r=6383
    i_g=10290
    i_k=9050
    xr=[16,21]
    yr=[-0.0002,0.0002]
  end
  
  else: message, 'Not the right date'
  
  endcase

  for l=0,nlabel-1 do begin
    xyouts,lons[l],lats[l],label[l],charsize=2.0,color=200, charthick=1.5
    plots,lons[l],lats[l],psym=6,color=255
  endfor
  ;find the correct blue coloured points
;  i_b=8000
;  tvlct, red[i_b],green[i_b],blue[i_b],100
;  ss=5
;  oplot,lon[i_b:i_b+1],lat[i_b:i_b+1],color=100,psym=8, symsize=ss

  ch_r=histogram(cl_hist[*,0])
  ch_g=histogram(cl_hist[*,1])
  ch_b=histogram(cl_hist[*,2])
  tvlct, r,g,b
  for i=0, n_elements(indices)-1 do begin
    plots, lon[indices[i]], lat[indices[i]],psym=2, color=textcolor[i]
    xyouts, lon[indices[i]], lat[indices[i]], leg[i], color=textcolor[i], orientation=45., charthick=1.5
  endfor

if ps then begin
  device, /close
  spawn, 'convert '+dir+date+ll+date+'_map_oil_'+platform+'.ps '+dir+date+ll+date+'_map_oil_'+platform+'.png'
  spawn, 'rm '+dir+date+ll+date+'_map_oil_'+platform+'.ps'
  ;spawn, 'display '+dir+date+ll+date+'_map_oil_'+platform+'.png &'
endif

if png then begin
  p2=tvrd(true=1)
  write_png,dir+date+ll+date+'_map_oil_'+platform+'.png',p2
endif


; plotting of spectras - red 
if ps then begin
  device, filename=dir+date+ll+date+'_spec_oil_red_'+platform+'.ps', /encapsulated, /tt_font, set_font='Helvetica' 
  device, /color, bits_per_pixel=8
  device, xsize=36, ysize=24
endif
if png then begin
  siz=get_screen_size()
  window,6,retain=2,tit='Red Ocean Spectra',xs=siz[0]*0.8,ys=siz[1]*0.8
endif  

ff=file_search(dir+date+ll+'clean-ocean.out')
restore, ff[0]

plot, wl, al, title='Ocean Albedo', xtitle='wavelength (nm)', ytitle='Albedo', thick=3.8, yrange=[0,0.2],xr=[350,900], xstyle=1, /nodata, charsize=4
tvlct, [[0],[0],[255]],90
oplot, wl, al, color=90, thick=10
tvlct, red[i_r],green[i_r],blue[i_r],100
tvlct, [[200],[90],[0]],100
nsp=interpol(nspectra[*,i_r],nadlambda,zenlambda)
oplot, zenlambda, smooth((nsp*dn)/(zspectra[*,i_r]*fac[i_r]),10), color=100, thick=10

tvlct, red[i_g],green[i_g],blue[i_g],110
tvlct, [[0],[255],[0]],110
nsp=interpol(nspectra[*,i_g],nadlambda,zenlambda)
oplot, zenlambda, smooth((nsp*dn)/(zspectra[*,i_g]*fac[i_g]),10), color=110, thick=10

tvlct, red[i_k],green[i_k],blue[i_k],120
nsp=interpol(nspectra[*,i_k],nadlambda,zenlambda)
oplot, zenlambda, smooth((nsp*dn)/(zspectra[*,i_k]*fac[i_k]),10), color=120, thick=10

;tvlct, red[i_b],green[i_b],blue[i_b],130
;nsp=interpol(nspectra[*,i_b],nadlambda,zenlambda)
;oplot, zenlambda, (nsp*dn)/(zspectra[*,i_b]*fac[i_b]), color=130, thick=1.8


;legend,['Clean Ocean','Oil contaminated', 'Green ocean', 'Black Ocean'], textcolor=[255,100,110,120],color=[255,100,110,120], charthick=2.8, linestyle=[5,0,0,0]

if png then begin
  p2=tvrd(true=1)
  write_png,dir+date+ll+date+'_spec_oil_red_'+platform+'.png',p2
endif
if ps then begin
  device, /close
  spawn, 'convert '+dir+date+ll+date+'_spec_oil_red_'+platform+'.ps '+dir+date+ll+date+'_spec_oil_red_'+platform+'.png'
  spawn, 'rm '+dir+date+ll+date+'_spec_oil_red_'+platform+'.ps'
  ; spawn, 'display '+dir+date+ll+date+'_spec_oil_red_'+platform+'.png &'
endif


; plotting of slope to get oil retrieval

if ps then begin
  device, filename=dir+date+ll+date+'_oil_slope_'+platform+'.ps', /encapsulated, /tt_font, set_font='Helvetica' 
  device, /color, bits_per_pixel=8
  device, xsize=36, ysize=24
endif
if png then begin
  siz=get_screen_size()
  window,7,retain=2,tit='Slope of oil',xs=siz[0]*0.8,ys=siz[1]*0.8
endif  
if interactive and png then device, /cursor_crosshair
rg=linfit(wl,al)

plot, utc[good], slope[good,0], title='Slope of ocean albedo at flight points below 500m',$
 xtitle='UTC (H)', ytitle='Albedo slope (from '+strtrim(string(zenlambda[wl_start]),2)+'nm to '+strtrim(string(zenlambda[wl_end]),2)+'nm)',$
 thick=3.8, yrange=yr, xrange=xr,xstyle=1, /nodata, charsize=4
oplot, utc[good], slope[good,0], thick=2
oplot, utc[good], slope[good,0]+(slope[good,1]) , color=15, psym=3
oplot, utc[good], slope[good,0]-(slope[good,1]) , color=15, psym=3

oplot, [utc[0],utc[n_elements(utc)-1]],[0,0], linestyle=1, color=70, thick =1           ; zero line
oplot, [utc[0],utc[n_elements(utc)-1]],[rg[1],rg[1]], linestyle=2, color=150   ; reference line


legend,['Clean Ocean','Measured value','slope uncertainty'], textcolor=[150,255,15],charthick=2.8

if interactive and png then begin
  print, 'entering interactive mode'
  redo=1
  while redo do begin
  wset, 7
  cursor, x, y,/wait

  good_up=where(slope[good,0] gt rg[1], count)
  if count le 0 then begin
    print, 'No values above the clean ocean line'
  endif else begin
    mm=min(abs(utc[good[good_up]]-x),sp_in)
    
    if ps then begin
      device, filename=dir+date+ll+date+'_oil_slope_spectra_'+platform+'.ps', /encapsulated, /tt_font, set_font='Helvetica' 
      device, /color, bits_per_pixel=8
      device, xsize=36, ysize=24
    endif
    if png then begin
      siz=get_screen_size()
      window,8,retain=2,tit='Oil spectra at slope '+strtrim(string(slope[good[good_up[sp_in]],0]),2),xs=siz[0]*0.8,ys=siz[1]*0.8
    endif  
        
    ii=good[good_up[sp_in]]    
    nsp=interpol(nspectra[*,ii],nadlambda,zenlambda)
    plot, zenlambda, (nsp*dn)/(zspectra[*,ii]*fac[ii]),$
     title='Albedo Spectrum at slope of '+strtrim(string(slope[ii,0]),2)+' and time of '+strtrim(string(utc[ii]),2),$
     xtitle='Wavelength (nm)', ytitle='albedo', xrange=[350,900], charsize=4

    read, int, prompt='redo? (0=no, 1=yes)'
    if int eq 0 then redo=0 else redo=1
    
    if png then begin
      p2=tvrd(true=1)
      write_png,dir+date+ll+date+'_oil_slope_spectra_'+platform+'.png',p2
    endif
    if ps then begin
      device, /close
      spawn, 'convert '+dir+date+ll+date+'_oil_slope_spectra_'+platform+'.ps '+dir+date+ll+date+'_oil_slope_spectra_'+platform+'.png'
      spawn, 'rm '+dir+date+ll+date+'_oil_slope_spectra_'+platform+'.ps'
      ; spawn, 'display '+dir+date+ll+date+'_spec_oil_red_'+platform+'.png &'
    endif
  endelse
  endwhile
endif

if png then begin
  wset, 7
  p2=tvrd(true=1)
  write_png,dir+date+ll+date+'_oil_slope_'+platform+'.png',p2
endif
if ps then begin
  device, /close
  spawn, 'convert '+dir+date+ll+date+'_oil_slope_'+platform+'.ps '+dir+date+ll+date+'_oil_slope_'+platform+'.png'
  spawn, 'rm '+dir+date+ll+date+'_oil_slope_'+platform+'.ps'
  ; spawn, 'display '+dir+date+ll+date+'_spec_oil_red_'+platform+'.png &'
endif


stop
end
