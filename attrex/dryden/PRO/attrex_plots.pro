;+
; NAME:
;   attrex_plots
;
; PURPOSE:
;   plot all the data produced by attrex_online
;
; CATEGORY:
;   ATTREX online processing, plotting
;
; CALLING SEQUENCE:
;   attrex_plots, tmhrs_big,lat_big,lon_big,nad_spect,zen_spect,nadlambda,zenlambda,alt_big,ncg4_big,zcg4_big,i_start,i_last,cod,ref,dirout,flight,plot_wl,attcorr,mu,ts,sp,pr,rt
;   - where tmhrs_big is the entire range of utc time up to now (hours)
;   - lat_big is the Global Hawk (GH) latitude (all values up to now) 
;   - lon_big is the longitude
;   - nad_spect is the nadir irradiance spectra
;   - zen_spect is the zenith irradiance spectra
;   - nadlambda is the corresponding nadir wavelengths
;   - zenlambda is the corresponding zenith wavelengths
;   - alt_big is all the altitude values up to now of the GH
;   - ncg4_big is the nadir cg4 irradiance values
;   - zcg4_big is the zenith cg4 irradiance values 
;   - i_start is the starting index for the most current file
;   - i_last is the ending index for the most current file
;   - cod is the cloud optical depth
;   - ref is the cloud drop effective radius
;   - dirout is the saving directory
;   - flight is the title of the flight (for saving purposes)
;   - plot_wl is the wavelengths to plot over
;   - attcorr is the indicator if the cosine correction was applied
;   - mu is the mean airmass factor for the last file
;   - ts is flag for plotting the time series
;   - sp is flag for plotting the spectra
;   - pr is flag for plotting the profile
;   - rt is flag for plotting the retrieved properties
;
; OUTPUT:
;   plots
;
; KEYWORDS:
;   - none
;
; DEPENDENCIES:
;   - legend.pro
;   
; NEEDED FILES:
;  none
;  
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
; Written:  Samuel LeBlanc, LASP CU Boulder, September 8th, 2011, Boulder
; Modified: 
;          
;---------------------------------------------------------------------------
@legend.pro

pro attrex_plots, tmhrs_big,lat_big,lon_big,nad_spect,zen_spect,nadlambda,zenlambda,alt_big,ncg4_big,zcg4_big,i_start,i_last,cod_ice,ref_ice,cod_liquid,ref_liquid,res_ice,res_liquid,dirout,flight,plot_wl,attcorr,mu,ts,sp,pr,rt

;set to plot the last x time
t_plot=2.0 ;in hours
t_plotp=0.5 ;in hours for the profile
fl=where(tmhrs_big ge max(tmhrs_big,/nan)-t_plot,n)
flp=where(tmhrs_big ge max(tmhrs_big,/nan)-t_plotp,n)
if n lt 1 then begin
  print, '**** plotting outside time range ****'
  goto, ends
endif
if attcorr then begin
  print, 'All irradiances have been Cosine response corrected'
  ac='_attcorr'
endif else ac=''
spawn, 'killall display'

if ts then begin
; time series plot of a few wavelength, both nadir and zenith
print, 'Plotting time series'
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=dirout+flight+ac+'_TS.ps'
 device, xsize=20, ysize=20
 !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8 & !p.multi=0 & !x.margin=[7,2]
 nul=min(abs(nadlambda-plot_wl[0]),in)
 nul=min(abs(zenlambda-plot_wl[0]),iz)
 plot, tmhrs_big[fl],zen_spect[fl,iz],title='Time series from '+string(tmhrs_big[i_last]-t_plot,format='(F5.2)')+' to '+string(tmhrs_big[i_last],format='(F5.2)')+' UTC(hours)',$
  xtitle='UTC Time (hours)',ytitle='Irradiance (W/m!U2!N nm)',$
  xrange=[max(tmhrs_big,/nan)-t_plot,max(tmhrs_big,/nan)],yrange=[0,2.5],ystyle=9,xmargin=[7,7]
 oplot, tmhrs_big[fl],nad_spect[fl,in],color=250
 lines=0
 leg_tit=strtrim(fix(plot_wl[0]),2)
 cl=indgen(n_elements(plot_wl))*250/(n_elements(plot_wl)-1)
 for i=1, n_elements(plot_wl)-1 do begin
  nul=min(abs(nadlambda-plot_wl[i]),in)
  nul=min(abs(zenlambda-plot_wl[i]),iz)
  oplot, tmhrs_big[fl],zen_spect[fl,iz],color=cl[i]
  oplot, tmhrs_big[fl],nad_spect[fl,in],color=cl[i],linestyle=2
  lines=[lines,0]
  leg_tit=[leg_tit,strtrim(fix(plot_wl[i]),2)]
 endfor
 
 legend, [leg_tit+' nm','CG4'], linestyle=[lines,1], pspacing=1.6,/right,box=0,color=[cl,50],/data,position=[max(tmhrs_big,/nan)-0.5,2.27]
 legend, replicate('|',n_elements(plot_wl)+1),color=[cl,200],box=0,linestyle=[lines+2,1],pspacing=1.6,/data,/right,position=[max(tmhrs_big,/nan)-0.15,2.27]
 xyouts,max(tmhrs_big,/nan)-0.55,2.3,'Zenith | Nadir',alignment=0.5,/data

 axis, yaxis=1,ystyle=1, yrange=[0.0,650.],/save,ytitle='Long wave irradiance (W/m!U2!N)'
 oplot, tmhrs_big[fl],ncg4_big[fl],color=200,linestyle=1
 oplot, tmhrs_big[fl],zcg4_big[fl],color=50, linestyle=1
 
 device, /close
spawn, 'convert "'+dirout+flight+ac+'_TS.ps" "'+dirout+flight+ac+'_TS.png"'
spawn, 'convert -alpha off "'+dirout+flight+ac+'_TS.ps" "'+dirout+flight+ac+'_TS.png"'
spawn, 'rm -f "'+dirout+flight+ac+'_TS.ps"'
spawn, 'display "'+dirout+flight+ac+'_TS.png" &'
spawn, 'scp "'+dirout+flight+ac+'_TS.png" leblanse@atoc.colorado.edu:./public_html/attrex/data/time/'
endif

if pr then begin
; profile plot of net irradiance at a few wavelenghts, and cg4 data
print, 'plotting profile'
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=dirout+flight+ac+'_profile.ps'
 device, xsize=20, ysize=40
 !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8 & !p.multi=[0,1,2]
 
 nul=min(abs(nadlambda-plot_wl[0]),in)
 nul=min(abs(zenlambda-plot_wl[0]),iz) 
 plot, nad_spect[in,flp]-zen_spect[iz,flp],alt_big[flp],title='Profile of net irradiance!C from '+string(tmhrs_big[i_last]-t_plotp,format='(F5.2)')+' to '+string(tmhrs_big[i_last],format='(F5.2)')+' UTC(hours)!C',$
  xtitle='Short wave net irradiance (W/m!U2!N nm)',ytitle='Altitude (m)',color=0, xrange=[-2.0,2.0],xstyle=9,ymargin=[4,5]
 cl=indgen(n_elements(plot_wl))*250/(n_elements(plot_wl)-1)
 for i=1, n_elements(plot_wl)-1 do begin
  nul=min(abs(nadlambda-plot_wl[i]),in)
  nul=min(abs(zenlambda-plot_wl[i]),iz)
  oplot, nad_spect[flp,in]-zen_spect[flp,iz],alt_big[flp],color=cl[i]
 endfor
 
 tvlct, 180,180,180,121
 axis, xaxis=1,xstyle=1, xrange=[-200.,200.],/save,xtitle='Long wave net irradiance (W/m!U2!N)'
 oplot, ncg4_big[flp]-zcg4_big[flp],alt_big[flp], color=121
 legend, [leg_tit+' nm','CG4'], textcolor=[cl,0],color=[cl,121],linestyle=[replicate(0,n_elements(plot_wl)),0],box=0,/right,pspacing=1.5

  nul=min(abs(nadlambda-plot_wl[0]),in)
  nul=min(abs(zenlambda-plot_wl[0]),iz)
  plot, nad_spect[in,flp],alt_big[flp],title='Profile of irradiance!C',$
  xtitle='Short wave irradiance (W/m!U2!N nm)',ytitle='Altitude (m)',color=0, xrange=[0,2.5],xstyle=9, linestyle=1, ymargin=[4,3]
  oplot, zen_spect[iz,flp], alt_big[flp],linestyle=2, color=0
 cl=indgen(n_elements(plot_wl))*250/(n_elements(plot_wl)-1)
 for i=1, n_elements(plot_wl)-1 do begin
  nul=min(abs(nadlambda-plot_wl[i]),in)
  nul=min(abs(zenlambda-plot_wl[i]),iz)
  oplot, nad_spect[flp,in],alt_big[flp],color=cl[i],linestyle=1
  oplot, zen_spect[flp,iz],alt_big[flp],color=cl[i],linestyle=2
 endfor
 
 tvlct, 180,180,180,121
 axis, xaxis=1,xstyle=1, xrange=[0,600.],/save,xtitle='Long wave irradiance (W/m!U2!N)'
 oplot, ncg4_big[flp],alt_big[flp],linestyle=1, color=121
 oplot, zcg4_big[flp],alt_big[flp],linestyle=2, color=121 
 legend, [leg_tit+' nm','CG4'], textcolor=[cl,0],color=[cl,121],linestyle=[replicate(2,n_elements(plot_wl)),2],box=0,/right,pspacing=1.5, /normal, position=[0.75,0.42]
 legend, replicate('|',n_elements(plot_wl)+1),   color=[cl,121],linestyle=[replicate(1,n_elements(plot_wl)),1],box=0,/right,pspacing=1.5, /normal, position=[0.88,0.42]
 xyouts,0.74,0.425,'Zenith | Nadir',alignment=0.5,/normal
 
 device, /close
spawn, 'convert "'+dirout+flight+ac+'_profile.ps" "'+dirout+flight+ac+'_profile.png"'
spawn, 'convert -alpha off "'+dirout+flight+ac+'_profile.ps" "'+dirout+flight+ac+'_profile.png"'
spawn, 'rm -f "'+dirout+flight+ac+'_profile.ps"
spawn, 'display "'+dirout+flight+ac+'_profile.png" &'
spawn, 'scp "'+dirout+flight+ac+'_profile.png" leblanse@atoc.colorado.edu:./public_html/attrex/data/profile/'
endif

if rt then begin
; time series of aod and ref
print, 'plotting cod and ref time series'
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=dirout+flight+ac+'_rt.ps'
 device, xsize=23, ysize=20
 !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8 & !p.multi=[0,1,2]
 plot, tmhrs_big[fl],cod_ice[fl],title='Time series of retrieved properties',$;ytitle='Cloud optical depth',$
  ystyle=9,yrange=[0,100],xmargin=[6,12],psym=1,ymargin=[1,3],xtickname=replicate(' ',7)

loadct, 0, /silent
tvlct,r,g,b,/get
r=reverse(r) & g=reverse(g) & b=reverse(b)
tvlct,r,g,b
 lvls=reverse(10.0^(-findgen(20)/2.))
 contour,[[res_ice[fl]],[res_ice[fl]]],tmhrs_big[fl],[0,100],/cell_fill,levels=lvls,/overplot  ;take this out to omit residual

loadct,39,/silent
 oplot,tmhrs_big[fl],cod_ice[fl],psym=1,color=50
 axis, yaxis=1,ystyle=1, yrange=[0.0,100.],/save;,ytitle='Effective radius (!9m!4m)'
 oplot, tmhrs_big[fl],ref_ice[fl], psym=1,color=250
 legend,['Ice cloud'],textcolors=[10],/right,box=0 
 plot, tmhrs_big[fl],cod_liquid[fl],xtitle='UTC time (hours)',$;ytitle='Cloud optical depth',$
  ystyle=9,yrange=[0,100],xmargin=[6,12],psym=1,ymargin=[3,0]
tvlct,r,g,b
  contour,[[res_liquid[fl]],[res_liquid[fl]]],tmhrs_big[fl],[0,100],/cell_fill,levels=lvls,/overplot  ;take this out to omit residual
loadct,39,/silent
 oplot,tmhrs_big[fl],cod_liquid[fl],psym=1,color=50
 axis, yaxis=1,ystyle=1, yrange=[0.0,30.],/save;,ytitle='Effective radius (!9m!4m)
 oplot, tmhrs_big[fl],ref_liquid[fl], color=250, psym=1
 legend,['Liquid cloud'],textcolors=[10],/right,box=0
 xyouts,0.05,0.5,'Cloud optical depth',/normal,orientation=90,alignment=0.5,color=50
 xyouts,0.86,0.5,'Effective radius (!9m!4m)',/normal,orientation=90,alignment=0.5, color=250
tvlct,r,g,b
 contour, transpose([[lvls],[lvls]]),[0,1],lvls,/ylog,/cell_fill,levels=lvls,color=255,position=[0.88,0.1,0.90,0.9],/normal,/noerase,xticks=1,yticks=1,xtickname=[' ',' '],ytickname=[' ',' ']  ;take this out to omit residual
 axis, yaxis=1,ystyle=1,yrange=[min(lvls),max(lvls)],ytitle='Residual',/ylog,color=255
loadct,39,/silent
 tvlct,120,120,120,200
 tvlct,160,0,0,251 
 ;axis,0.9, yaxis=1,ystyle=1,yrange=[0E-7,1],/ylog,/save,ytitle='Residuals',/normal,color=200
 ;oplot, tmhrs_big[fl],res_ice[fl], color=200, thick=0.5
 ;oplot, tmhrs_big[fl],res_liquid[fl], color=251, thick=0.5
 
; legend, ['Ice','Liquid Water'],textcolors=[0,250],/bottom,box=0
; legend, ['Cloud optical depth','Effective radius'],textcolors=[0,250],box=0
 
 device, /close
spawn, 'convert "'+dirout+flight+ac+'_rt.ps" "'+dirout+flight+ac+'_rt.png"'
spawn, 'convert -alpha off "'+dirout+flight+ac+'_rt.ps" "'+dirout+flight+ac+'_rt.png"'
spawn, 'rm -f "'+dirout+flight+ac+'_rt.ps"
spawn, 'display "'+dirout+flight+ac+'_rt.png" &'
spawn, 'scp "'+dirout+flight+ac+'_rt.png" leblanse@atoc.colorado.edu:./public_html/attrex/data/retrieval/'
endif

if sp then begin
; sample spectra of last while
print, 'plotting sample spectra'
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=dirout+flight+ac+'_spect.ps'
 device, xsize=20, ysize=20
 !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8 & !p.multi=0

 avg_nad=nadlambda & avg_zen=zenlambda & std_nad=nadlambda & std_zen=zenlambda
 fl_i=where(tmhrs_big ge tmhrs_big[i_start] and tmhrs_big le tmhrs_big[i_last])
 nul=max(zen_spect[fl_i,100],mx,/nan)
 nul=min(zen_spect[fl_i,100],mn,/nan)
 for i=0,n_elements(nadlambda)-1 do avg_nad[i]=mean(nad_spect[i_start:i_last,i],/nan)
 for i=0,n_elements(zenlambda)-1 do avg_zen[i]=mean(zen_spect[i_start:i_last,i],/nan)
 for i=0,n_elements(nadlambda)-1 do std_nad[i]=stddev(nad_spect[i_start:i_last,i],/nan)
 for i=0,n_elements(zenlambda)-1 do std_zen[i]=stddev(zen_spect[i_start:i_last,i],/nan) 
 plot, zenlambda, avg_zen, title='Spectra from '+string(tmhrs_big[i_start],format='(F5.2)')+' to '+string(tmhrs_big[i_last],format='(F5.2)')+' UTC(H) at !9m!4='+string(mu,format='(F4.2)'),$
  ytitle='Irradiance (W/m!U2!N nm)', xtitle='Wavelength (nm)', yrange=[0,2.5], ystyle=9,xmargin=[6,6]
 oplot, nadlambda, avg_nad, color=250
 
 oplot, zenlambda, avg_zen+std_zen,linestyle=2
 oplot, nadlambda, avg_nad+std_nad,linestyle=2, color=250
 
 oplot, zenlambda, avg_zen-std_zen,linestyle=2
 oplot, nadlambda, avg_nad-std_nad,linestyle=2, color=250

 legend, ['Average','+ !9s!4','- !9s!4','Albedo'], linestyle=[0,2,2,0], pspacing=1.6,/right,box=0,color=[0,0,0,70],position=[1500,2.27],/data
 legend, ['|','|','|'],color=[250,250,250],box=0,position=[1788,2.27],linestyle=[0,2,2],pspacing=1.55,/right,/data
 xyouts,1488,2.3,'Zenith | Nadir   ',alignment=0.5,/data
 
 axis, yaxis=1, ytitle='Albedo',yrange=[0,1.0],ystyle=1,/save
 oplot, zenlambda, avg_nad/avg_zen, color=70
 
 device, /close
spawn, 'convert "'+dirout+flight+ac+'_spect.ps" "'+dirout+flight+ac+'_spect.png"'
spawn, 'convert -alpha off "'+dirout+flight+ac+'_spect.ps" "'+dirout+flight+ac+'_spect.png"'
spawn, 'rm -f "'+dirout+flight+ac+'_spect.ps"
spawn, 'display "'+dirout+flight+ac+'_spect.png" &'
spawn, 'scp "'+dirout+flight+ac+'_spect.png" leblanse@atoc.colorado.edu:./public_html/attrex/data/spectra/'
endif

ends:
end
