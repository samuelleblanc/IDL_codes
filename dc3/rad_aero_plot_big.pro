;+
; NAME:
;   rad_aero_plot
;
; PURPOSE:
;   to present results form the look up tables built for aerosol over Boulder for multiple sza
;
; CATEGORY:
;   Aerosol retrieval, radiance, explarotory
;
; CALLING SEQUENCE:
;   rad_aero_plot
; 
;
; OUTPUT:
;   plots
;
; KEYWORDS:
;   - none
;
; DEPENDENCIES:
;   - legend.pro
;   - colorbar.pro
;   - read_skywatch.pro
;   - errploty.pro
;   
; NEEDED FILES:
;   - lut
;
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
; Written:  Samuel LeBlanc, LASP CU Boulder, August 28th, 2012
; Modified: by Samuel LeBlanc, Spetember, 11th, 2012
;           - added plotting of rad vs. irr for various ssa, and asy
;           - added use of measurements of the plot, including the sunphotometer and ssfr
; Modified: by Samuel LeBlanc, September, 24th, 2012 
;           - ported from rad_aero_plot.pro
;           - changed to use full spectra and sza model runs          
;---------------------------------------------------------------------------

@/home/leblanc/IDL/bin/errploty.pro
@legend.pro
@/home/leblanc/IDL/bin/colorbar.pro
@/home/leblanc/IDL/bin/read_skywatch.pro
@zensun.pro
pro rad_aero_plot

date='20120814'
lat=40.007916667
lon=-105.26825

print, 'restoring'
restore, '/home/leblanc/DC3_SEAC4RS/library/LUT_DUANE_big.out'
;restore, '/home/leblanc/DC3_SEAC4RS/library/LUT_DUANE_sun.out'
dir='/home/leblanc/DC3_SEAC4RS/SSFR3/plots/'
tau_file='/home/leblanc/DC3_SEAC4RS/SSFR3/sunphotometer/sun_tau_'+strmid(date,2,2)+'_'+strmid(date,4,2)+'_'+strmid(date,6,2)+'.dat'
;time=18.0
restore, '/home/leblanc/DC3_SEAC4RS/SSFR3/'+date+'/'+date+'_sample.out'
ntime=n_elements(times)
time=times ;findgen(ntime)/5.+14.;18.0 ; UTC time of interes

;get the proper values of aot for multiple times
tau=read_skywatch(instrument='tau',name=tau_file)
utc_tau=tau.sec_of_day/3600.+6.

aot=findgen(ntime,n_elements(tau.wavelength))
for i=0, ntime-1 do begin
  nul=min(abs(utc_tau-time[i]),/nan,nt)
  aot[i,*]=tau.tau[*,nt]
endfor

doy=julian_day(fix(strmid(date,0,4)),fix(strmid(date,4,2)),fix(strmid(date,6,2)))
zensun, doy, time, lat, lon, sza, azimuth, solfac

;now plot radiance vs. irradiance 
;irr=irr500;-irr_dif500*0.05 ;take out the diffuse contributions from the buildings 
;rad=rad500


;make a plot of irradiance vs. radiance for the model results at all wavelengths giving colored lines various values
;plot over the sza
     fn=dir+'rad_irr_wvls_';+string(time[tm],format='(F4.1)')
     print, 'plotting:'+fn
     set_plot, 'ps'
     loadct, 39, /silent
     device, /encapsulated, /tt_font, set_font='Helvetica Bold'
     device, filename=fn+'.ps'
     device,/color,bits_per_pixel=8., xsize=30, ysize=20
      !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
      !p.multi=0 & !x.margin=[6,9]

     nsza=5 ; at sza 5
     
     ; make array of correct model values that match measured aot
     iaot=lonarr(n_elements(lambda))
     aot_m=fltarr(n_elements(lambda))
     ;make the proper index arrays
     is=indgen(203,20,15,11,10,/l64) 
     i_100_060=iaot & i_090_060=iaot & i_080_060=iaot
     i_100_070=iaot & i_090_070=iaot & i_080_070=iaot
     i_100_080=iaot & i_090_080=iaot & i_080_080=iaot
     ii=lonarr(203,11,10)
     for i=0, n_elements(lambda)-1 do begin
       nul=min(abs(lambda[i]-tau.wavelength),/nan,il)
       aot_m[i]=aot[nsza,il]
       nul=min(abs(aod-aot_m[i]),/nan,it)
       iaot[i]=it
       i_100_060[i]=is[i,nsza,it,10,2] & i_090_060[i]=is[i,nsza,it,8,2] & i_080_060[i]=is[i,nsza,it,6,2]
       i_100_070[i]=is[i,nsza,it,10,4] & i_090_070[i]=is[i,nsza,it,8,4] & i_080_070[i]=is[i,nsza,it,6,4]
       i_100_080[i]=is[i,nsza,it,10,6] & i_090_080[i]=is[i,nsza,it,8,6] & i_080_080[i]=is[i,nsza,it,6,6]
       for j=0, 10 do for k=0, 9 do ii[i,j,k]=is[i,nsza,it,j,k]
     endfor
     

     
     plot, rad[*,nsza,*,*,*],irr[*,nsza,*,*,*],/nodata, xtitle='Radiance (W/m!U2!N nm sr)',ytitle='Irradiance (W/m!U2!N nm)', $
      title='Radiance and Irradiance at various SSA and ASY'
      ns=2
     oplot, smooth(rad[i_100_060],ns,/edge_truncate),smooth(irr[i_100_060],ns,/edge_truncate), color=70, linestyle=0
     oplot, smooth(rad[i_090_060],ns,/edge_truncate),smooth(irr[i_090_060],ns,/edge_truncate), color=130, linestyle=0
     oplot, smooth(rad[i_080_060],ns,/edge_truncate),smooth(irr[i_080_060],ns,/edge_truncate), color=250, linestyle=0
     
     oplot, smooth(rad[i_100_070],ns,/edge_truncate),smooth(irr[i_100_070],ns,/edge_truncate), color=70, linestyle=1
     oplot, smooth(rad[i_090_070],ns,/edge_truncate),smooth(irr[i_090_070],ns,/edge_truncate), color=130, linestyle=1
     oplot, smooth(rad[i_080_070],ns,/edge_truncate),smooth(irr[i_080_070],ns,/edge_truncate), color=250, linestyle=1
     
     oplot, smooth(rad[i_100_080],ns,/edge_truncate),smooth(irr[i_100_080],ns,/edge_truncate), color=70, linestyle=2
     oplot, smooth(rad[i_090_080],ns,/edge_truncate),smooth(irr[i_090_080],ns,/edge_truncate), color=130, linestyle=2
     oplot, smooth(rad[i_080_080],ns,/edge_truncate),smooth(irr[i_080_080],ns,/edge_truncate), color=250, linestyle=2
     
     ;now the measurement in black
     oplot, zensp[nsza,0:193],nadsp[nsza,0:193]
     tvlct, 200, 200, 200, 200 ;load up the grey
     for i=0, n_elements(lambda)-1, 30 do begin
       xyouts,zensp[nsza,i]+0.002,nadsp[nsza,i],string(lambda[i],format='(I3)')+' nm', charsize=1.2
       ra=[rad[i_100_060[i]],rad[i_090_060[i]],rad[i_080_060[i]],rad[i_100_070[i]],rad[i_090_070[i]],rad[i_080_070[i]],rad[i_100_080[i]],rad[i_090_080[i]],rad[i_080_080[i]],zensp[nsza,i]]
       ir=[irr[i_100_060[i]],irr[i_090_060[i]],irr[i_080_060[i]],irr[i_100_070[i]],irr[i_090_070[i]],irr[i_080_070[i]],irr[i_100_080[i]],irr[i_090_080[i]],irr[i_080_080[i]],nadsp[nsza,i]]
      ; oplot, ra, ir, color=200, linestyle=3
     endfor
    legend, ['ASY 0.6','ASY 0.7','ASY 0.8','SSA 1.0','SSA 0.9','SSA 0.8'],linestyle=[0,1,2,0,0,0],color=[0,0,0,70,130,250],box=0,/right,/bottom, pspacing=1.0

    device, /close
    spawn, 'convert '+fn+'.ps '+fn+'.png'
    
    ;plot only changes in ssa
     fn=dir+'rad_irr_ssas_';+string(time[tm],format='(F4.1)')
     print, 'plotting:'+fn
     set_plot, 'ps'
     loadct, 39, /silent
     device, /encapsulated, /tt_font, set_font='Helvetica Bold'
     device, filename=fn+'.ps'
     device,/color,bits_per_pixel=8., xsize=30, ysize=20
      !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
      !p.multi=0 & !x.margin=[6,9]
     plot, rad[*,nsza,*,*,*],irr[*,nsza,*,*,*],/nodata, xtitle='Radiance (W/m!U2!N nm sr)',ytitle='Irradiance (W/m!U2!N nm)', $
      title='Radiance and Irradiance at various SSA'
     for j=0, 10 do oplot, smooth(rad[ii[*,j,2]],ns,/edge_truncate), smooth(irr[ii[*,j,2]],ns,/edge_truncate),color=23*(j+1)
     tvlct, 210, 210, 210, 200 ;load up the grey
     oplot, zensp[nsza,0:193],nadsp[nsza,0:193], color=0
     for i=0, n_elements(lambda)-1, 30 do begin
       xyouts,zensp[nsza,i]+0.002,nadsp[nsza,i],string(lambda[i],format='(I3)')+' nm', charsize=1.2, color=0
       oplot, [reform(rad[ii[i,*,2]]),zensp[nsza,i]], [reform(irr[ii[i,*,2]]),nadsp[nsza,i]], color=200, linestyle=3
     endfor
     
     loadct, 39, /silent
     colorbar, minrange=min(ssa),maxrange=max(ssa), title='Single Scattering Albedo',/vertical,/right,format='(F4.2)', position=[0.86,0.13,0.88,0.93]
     device, /close
     spawn, 'convert '+fn+'.ps '+fn+'.png'
     
         ;plot only changes in asy
     fn=dir+'rad_irr_asys_';+string(time[tm],format='(F4.1)')
     print, 'plotting:'+fn
     set_plot, 'ps'
     loadct, 39, /silent
     device, /encapsulated, /tt_font, set_font='Helvetica Bold'
     device, filename=fn+'.ps'
     device,/color,bits_per_pixel=8., xsize=30, ysize=20
      !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
      !p.multi=0 & !x.margin=[6,9]
     plot, rad[*,nsza,*,*,*],irr[*,nsza,*,*,*],/nodata, xtitle='Radiance (W/m!U2!N nm sr)',ytitle='Irradiance (W/m!U2!N nm)', $
      title='Radiance and Irradiance at various ASY'
     for j=0, 9 do oplot, smooth(rad[ii[*,9,j]],ns,/edge_truncate), smooth(irr[ii[*,9,j]],ns,/edge_truncate),color=25*(j+1)
     tvlct, 210, 210, 210, 200 ;load up the grey
     oplot, zensp[nsza,0:193],nadsp[nsza,0:193], color=0
     for i=0, n_elements(lambda)-1, 30 do begin
       xyouts,zensp[nsza,i]+0.002,nadsp[nsza,i],string(lambda[i],format='(I3)')+' nm', charsize=1.2, color=0
       oplot, [reform(rad[ii[i,9,*]]),zensp[nsza,i]], [reform(irr[ii[i,9,*]]),nadsp[nsza,i]], color=200, linestyle=3
     endfor
     loadct, 39, /silent
     colorbar, minrange=min(asy),maxrange=max(asy), title='Asymetry Parameter',/vertical,/right,format='(F4.2)', position=[0.86,0.13,0.88,0.93]
     device, /close
     spawn, 'convert '+fn+'.ps '+fn+'.png'

;;; normalization schemes
    ;plot only changes in ssa
     fn=dir+'rad_irr_ssas_norm';+string(time[tm],format='(F4.1)')
     print, 'plotting:'+fn
     set_plot, 'ps'
     loadct, 39, /silent
     device, /encapsulated, /tt_font, set_font='Helvetica Bold'
     device, filename=fn+'.ps'
     device,/color,bits_per_pixel=8., xsize=30, ysize=20
      !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
      !p.multi=0 & !x.margin=[6,9]
     plot, rad[*,nsza,*,*,*],irr[*,nsza,*,*,*],/nodata, xtitle='Radiance (normalized)',ytitle='Irradiance (normalized)', $
      title='Radiance and Irradiance at various SSA', yrange=[0.8,9], xrange=[0.8,25]
     for j=0, 10 do oplot, smooth(rad[ii[*,j,2]],ns,/edge_truncate)/min(rad[ii[*,j,2]]), smooth(irr[ii[*,j,2]],ns,/edge_truncate)/min(irr[ii[*,j,2]]),color=23*(j+1)
     tvlct, 210, 210, 210, 200 ;load up the grey
     oplot, zensp[nsza,0:193]/min(zensp[nsza,0:193]),nadsp[nsza,0:193]/min(nadsp[nsza,0:193]), color=0
     for i=0, n_elements(lambda)-1, 30 do begin
       xyouts,zensp[nsza,i]/min(zensp[nsza,0:193])+0.002,nadsp[nsza,i]/min(nadsp[nsza,0:193]),string(lambda[i],format='(I3)')+' nm', charsize=1.2, color=0
     ;  oplot, [reform(rad[ii[i,*,2]]),zensp[nsza,i]], [reform(irr[ii[i,*,2]]),nadsp[nsza,i]], color=200, linestyle=3
     endfor
     
     loadct, 39, /silent
     colorbar, minrange=min(ssa),maxrange=max(ssa), title='Single Scattering Albedo',/vertical,/right,format='(F4.2)', position=[0.86,0.13,0.88,0.93]
     device, /close
     spawn, 'convert '+fn+'.ps '+fn+'.png'
     
         ;plot only changes in asy
     fn=dir+'rad_irr_asys_norm';+string(time[tm],format='(F4.1)')
     print, 'plotting:'+fn
     set_plot, 'ps'
     loadct, 39, /silent
     device, /encapsulated, /tt_font, set_font='Helvetica Bold'
     device, filename=fn+'.ps'
     device,/color,bits_per_pixel=8., xsize=30, ysize=20
      !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
      !p.multi=0 & !x.margin=[6,9]
     plot, rad[*,nsza,*,*,*],irr[*,nsza,*,*,*],/nodata, xtitle='Radiance (normalized)',ytitle='Irradiance (normalized)', $
      title='Radiance and Irradiance at various ASY', yrange=[0.8,9], xrange=[0.8,25]
     for j=0, 9 do oplot, smooth(rad[ii[*,9,j]],ns,/edge_truncate)/min(rad[ii[*,9,j]]), smooth(irr[ii[*,9,j]]/min(irr[ii[*,9,j]]),ns,/edge_truncate),color=25*(j+1)
     tvlct, 210, 210, 210, 200 ;load up the grey
     oplot, zensp[nsza,0:193]/min(zensp[nsza,0:193]),nadsp[nsza,0:193]/min(nadsp[nsza,0:193]), color=0
     for i=0, n_elements(lambda)-1, 30 do begin
       xyouts,zensp[nsza,i]/min(zensp[nsza,0:193])+0.002,nadsp[nsza,i]/min(nadsp[nsza,0:193]),string(lambda[i],format='(I3)')+' nm', charsize=1.2, color=0
       oplot, [reform(rad[ii[i,9,*]]),zensp[nsza,i]], [reform(irr[ii[i,9,*]]),nadsp[nsza,i]], color=200, linestyle=3
     endfor
     loadct, 39, /silent
     colorbar, minrange=min(asy),maxrange=max(asy), title='Asymetry Parameter',/vertical,/right,format='(F4.2)', position=[0.86,0.13,0.88,0.93]
     device, /close
     spawn, 'convert '+fn+'.ps '+fn+'.png'


stop

if 0 then begin
  for tm=0, ntime-1 do begin
    set_plot, 'ps'
     loadct, 39, /silent
     device, /encapsulated, /tt_font, set_font='Helvetica Bold'
     device, filename=dir+'rad_irr_'+date+string(time[tm],format='(F4.1)')+'.ps'
     device,/color,bits_per_pixel=8., xsize=20, ysize=20
      !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
      !p.multi=0 & !x.margin=[6,9]
      vl=500.
    nul=min(abs(vl-lambda),nl)
    nul=min(abs(vl-tau.wavelength),tl)
    nul=min(abs(vl-nadlambda),nl)
    nul=min(abs(vl-zenlambda),zl)
    nul=min(abs(aod-aot[tm,tl]),t)
    
    lvl=asy[0:*:2]
    lbl=string(lvl,format='(F4.2)')+ 'ASY'
    
    ssar=fltarr(n_elements(ssa),n_elements(asy))
    asyr=fltarr(n_elements(ssa),n_elements(asy))
    for i=0,n_elements(ssa)-1 do begin
      for j=0,n_elements(asy)-1 do begin
        ssar[i,j]=ssa[i]
        asyr[i,j]=asy[j]
      endfor
    endfor
    
    print, 'plotting'
    print, time[tm]
    
    ymin=min(irr[tm,t,*,*])
    ymin=min([ymin,nadsp[tm,nl]])*0.98
    ymax=max(irr[tm,t,*,*])
    ymax=max([ymax,nadsp[tm,nl]])*1.02
    xmin=min(rad[tm,t,*,*])
    xmin=min([xmin,zensp[tm,zl]])*0.98
    xmax=max(rad[tm,t,*,*])
    xmax=max([xmax,zensp[tm,zl]])*1.02
    
    contour, ssar,rad[tm,t,*,*],irr[tm,t,*,*],/fill,nlevels=15, xtitle='Radiance (W/m!U2!N nm sr)',ytitle='Irradiance (W/m!U2!N nm)',$
     title='Radiance and irradiance at 500 nm and tau='+string(aod[t],format='(F4.2)'), yrange=[ymin,ymax],xrange=[xmin,xmax]
    contour, asyr,rad[tm,t,*,*],irr[tm,t,*,*], /overplot, color=255, c_annotation=lbl,c_charsize=1.5,level=lvl
    oplot,[zensp[tm,zl],zensp[tm,zl]], [nadsp[tm,nl],nadsp[tm,nl]],psym=2, color=0, thick=5, symsize=2.0
    xyouts,zensp[tm,zl]+0.005,nadsp[tm,nl],string(time[tm],format='(F5.2)')+'UTC',charsize=1.2
    errplot, zensp[tm,zl],nadsp[tm,nl]*(0.95),nadsp[tm,nl]*(1.05)
    errploty,nadsp[tm,nl],zensp[tm,nl]*(0.95),zensp[tm,nl]*(1.05)
    colorbar, minrange=min(ssa),maxrange=max(ssa), title='Single Scattering Albedo',/vertical,/right,format='(F4.2)', position=[0.82,0.13,0.85,0.93]
    device, /close
    spawn, 'convert '+dir+'rad_irr_'+date+string(time[tm],format='(F4.1)')+'.ps '+dir+'rad_irr_'+date+string(time[tm],format='(F4.1)')+'.png'
  endfor
endif
;stop


;plot on a 3d surface the values above
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold'
 device, filename=dir+'rad_irr_3d_'+date+'.ps'
 device,/color,bits_per_pixel=8., xsize=20, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=0 & !x.margin=[6,9]
  vl=500.
nul=min(abs(vl-lambda),nl)
nul=min(abs(vl-tau.wavelength),tl)
nul=min(abs(vl-nadlambda),nl)
nul=min(abs(vl-zenlambda),zl)
lvl=asy[0:*:2]
lbl=string(lvl,format='(F4.2)');+ 'ASY'
ssar=fltarr(n_elements(ssa),n_elements(asy))
asyr=fltarr(n_elements(ssa),n_elements(asy))
for i=0,n_elements(ssa)-1 do begin
  for j=0,n_elements(asy)-1 do begin
    ssar[i,j]=ssa[i]
    asyr[i,j]=asy[j]
  endfor
endfor

;make array of tau indices
taus=intarr(ntime)
for tm=0, ntime-1 do begin
  nul=min(abs(aod-aot[tm,tl]),t)
  taus[tm]=t
endfor

;get the limits
ymin=min(irr[*,taus,*,*])
ymin=min([ymin,nadsp[*,nl]])*0.98
ymax=max(irr[*,taus,*,*])
ymax=max([ymax,nadsp[*,nl]])*1.02
xmin=min(rad[*,taus,*,*])
xmin=min([xmin,zensp[*,zl]])*0.98
xmax=max(rad[*,taus,*,*])
xmax=max([xmax,zensp[*,zl]])*1.02

;ymin=1.1
;xmin=0.05
;set up the 3d
;exchange radiance and irradiance

surface, dist(10),/nodata, ytitle='Radiance (W/m!U2!N nm sr)',xtitle='Irradiance (W/m!U2!N nm)',$
 title='Radiance and irradiance at 500 nm', xrange=[ymin,ymax],yrange=[xmin,xmax],$
 ztitle='SZA (degrees)',zrange=[75.,30.],/save, ystyle=9,xstyle=9,xticklen=1,yticklen=1,XGridStyle=1, YGridStyle=1,charsize=2.5,yticks=4 ;set up the axis

;t3d,/xyexch;rotate=[0,0,90]
 
 ;now iterate through all contour levels
for tm=0, ntime-1 do begin
  p=!p & x=!x & y=!y & z=!z
    contour, ssar,irr[tm,taus[tm],*,*],rad[tm,taus[tm],*,*],/noerase,/fill,nlevels=10, /t3d, zvalue=sza[tm]/(-45.)+75./45.,xstyle=4,ystyle=4,yrange=[xmin,xmax],xrange=[ymin,ymax]
  !p=p & !x=x & !y=y & !z=z
    contour, asyr,irr[tm,taus[tm],*,*],rad[tm,taus[tm],*,*],/noerase,color=255, c_annotation=lbl,c_charsize=2.8,level=lvl,/t3d,zvalue=sza[tm]/(-45.)+75./45.,xstyle=4,ystyle=4,yrange=[xmin,xmax],xrange=[ymin,ymax]
  !p=p & !x=x & !y=y & !z=z
  if nadsp[tm,nl] ge ymin and nadsp[tm,nl] le ymax and zensp[tm,zl] ge xmin and zensp[tm,zl] le xmax then begin
      oplot, [nadsp[tm,nl],nadsp[tm,nl]],[zensp[tm,zl],zensp[tm,zl]], psym=2, color=0, thick=5, symsize=2.0,/t3d,zvalue=sza[tm]/(-45.)+75./45.
    !p=p & !x=x & !y=y & !z=z
      plots, [nadsp[tm,nl],nadsp[tm,nl]], [zensp[tm,zl],zensp[tm,zl]],[sza[tm],75.],/t3d
  endif
  !p=p & !x=x & !y=y & !z=z
  ;  xyouts,zensp[tm,zl]+0.005,nadsp[tm,nl],string(time[tm],format='(F5.2)')+'UTC',charsize=1.2
  ;  errplot, zensp[tm,zl],nadsp[tm,nl]*(0.95),nadsp[tm,nl]*(1.05)
  ;  errploty,nadsp[tm,nl],zensp[tm,nl]*(0.95),zensp[tm,nl]*(1.05)
endfor
 contour, findgen(10,10),/nodata,/noerase,nlevels=15,zvalue=1, /t3d, yrange=[xmin,xmax],xrange=[ymin,ymax],charsize=2.5,xticks=4 
colorbar, minrange=min(ssa),maxrange=max(ssa), title='Single Scattering Albedo',/vertical,/right,format='(F4.2)', position=[0.82,0.13,0.85,0.93]
device, /close
;spawn, 'convert '+dir+'rad_irr_3d_'+date+'.ps '+dir+'rad_irr_3d_'+date+'_large.png'
spawn, 'convert '+dir+'rad_irr_3d_'+date+'.ps '+dir+'rad_irr_3d_'+date+'_flip.png'


ymin=1.1
xmin=0.05
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold'
 device, filename=dir+'rad_irr_3d_'+date+'.ps'
 device,/color,bits_per_pixel=8., xsize=20, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=0 & !x.margin=[6,9]
surface, dist(10),/nodata, xtitle='Radiance (W/m!U2!N nm sr)',ytitle='Irradiance (W/m!U2!N nm)',$
 title='Radiance and irradiance at 500 nm', yrange=[ymin,ymax],xrange=[xmin,xmax],$
 ztitle='SZA (degrees)',zrange=[75.,30.],/save, ystyle=9,xstyle=9,xticklen=1,yticklen=1,XGridStyle=1, YGridStyle=1,charsize=2.5,xticks=4 ;set up the axis

;t3d,/xyexch;rotate=[0,0,90]
 
 ;now iterate through all contour levels
for tm=0, ntime-1 do begin
  p=!p & x=!x & y=!y & z=!z
    contour, ssar,rad[tm,taus[tm],*,*],irr[tm,taus[tm],*,*],/noerase,/fill,nlevels=10, /t3d, zvalue=sza[tm]/(-45.)+75./45.,xstyle=4,ystyle=4,xrange=[xmin,xmax],yrange=[ymin,ymax]
  !p=p & !x=x & !y=y & !z=z
    contour, asyr,rad[tm,taus[tm],*,*],irr[tm,taus[tm],*,*],/noerase,color=255, c_annotation=lbl,c_charsize=2.8,level=lvl,/t3d,zvalue=sza[tm]/(-45.)+75./45.,xstyle=4,ystyle=4,xrange=[xmin,xmax],yrange=[ymin,ymax]
  !p=p & !x=x & !y=y & !z=z
  if nadsp[tm,nl] ge ymin and nadsp[tm,nl] le ymax and zensp[tm,zl] ge xmin and zensp[tm,zl] le xmax then begin
      oplot,[zensp[tm,zl],zensp[tm,zl]], [nadsp[tm,nl],nadsp[tm,nl]], psym=2, color=0, thick=5, symsize=2.0,/t3d,zvalue=sza[tm]/(-45.)+75./45.
    !p=p & !x=x & !y=y & !z=z
      plots, [zensp[tm,zl],zensp[tm,zl]], [nadsp[tm,nl],nadsp[tm,nl]],[sza[tm],75.],/t3d
  endif
  !p=p & !x=x & !y=y & !z=z
  ;  xyouts,zensp[tm,zl]+0.005,nadsp[tm,nl],string(time[tm],format='(F5.2)')+'UTC',charsize=1.2
  ;  errplot, zensp[tm,zl],nadsp[tm,nl]*(0.95),nadsp[tm,nl]*(1.05)
  ;  errploty,nadsp[tm,nl],zensp[tm,nl]*(0.95),zensp[tm,nl]*(1.05)
endfor
 contour, findgen(10,10),/nodata,/noerase,nlevels=15,zvalue=1, /t3d, xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=2.5,xticks=4 
colorbar, minrange=min(ssa),maxrange=max(ssa), title='Single Scattering Albedo',/vertical,/right,format='(F4.2)', position=[0.82,0.13,0.85,0.93]
device, /close
spawn, 'convert '+dir+'rad_irr_3d_'+date+'.ps '+dir+'rad_irr_3d_'+date+'_large.png'
;spawn, 'convert '+dir+'rad_irr_3d_'+date+'.ps '+dir+'rad_irr_3d_'+date+'.png'
stop
end
