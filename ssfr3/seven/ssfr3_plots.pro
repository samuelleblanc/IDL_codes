;+
; NAME:
;   ssfr3_plots
;
; PURPOSE:
;   plot all the data produced by ssfr3_online
;
; CATEGORY:
;   SSFR3 online processing, plotting
;
; CALLING SEQUENCE:
;   ssfr3_plots, tmhrs_big,nad_spect,zen_spect,nadlambda,zenlambda,i_start,i_last,cod,ref,dirout,$
;                flight,plot_wl,attcorr,mu,ts,sp,rt
;   - where tmhrs_big is the entire range of utc time up to now (hours)
;   - nad_spect is the nadir irradiance spectra
;   - zen_spect is the zenith irradiance spectra
;   - nadlambda is the corresponding nadir wavelengths
;   - zenlambda is the corresponding zenith wavelengths
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
; Modified: by Samuel LeBlanc, May 19th, 2012, Boulder, Colorado
;           - modified to plot SSFR3 values instead of ATTREX 
;          
;---------------------------------------------------------------------------
@legend.pro

pro ssfr3_plots, tmhrs_big,nad_spect,zen_spect,nadlambda,zenlambda,i_start,i_last,cod_ice,ref_ice,cod_liquid,ref_liquid,res_ice,$
                 res_liquid,dirout,flight,plot_wl,attcorr,mu,ts,sp,rt

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
;spawn, 'killall display'

;scale the zen_spect for an appropriate plotting
yrad=0.4

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
 plot, tmhrs_big[fl],zen_spect[fl,iz]*2.5/yrad,title='Time series from '+string(tmhrs_big[i_last]-t_plot,format='(F5.2)')+' to '+string(tmhrs_big[i_last],format='(F5.2)')+' UTC(hours)',$
  xtitle='UTC Time (hours)',ytitle='Irradiance (W/m!U2!N nm)',$
  xrange=[max(tmhrs_big,/nan)-t_plot,max(tmhrs_big,/nan)],yrange=[0,2.5],ystyle=9,xmargin=[7,7]
 oplot, tmhrs_big[fl],nad_spect[fl,in],thick=8
 lines=0
 leg_tit=strtrim(fix(plot_wl[0]),2)

cls=findgen(n_elements(plot_wl))
cls=cls*250/max(cls)
 for i=1, n_elements(plot_wl)-1 do begin
  nul=min(abs(nadlambda-plot_wl[i]),in)
  nul=min(abs(zenlambda-plot_wl[i]),iz)
  oplot, tmhrs_big[fl],zen_spect[fl,iz]*2.5/yrad,color=cls[i]
  oplot, tmhrs_big[fl],nad_spect[fl,in],color=cls[i],thick=8
  lines=[lines,0]
  leg_tit=[leg_tit,strtrim(fix(plot_wl[i]),2)]
 endfor
 
 legend, [leg_tit+' nm'], linestyle=[lines], pspacing=1.6,/right,box=0,color=[cls],$
  /data,position=[max(tmhrs_big,/nan)-0.5,2.27]
 legend, replicate('|',n_elements(plot_wl)),color=[cls],box=0,linestyle=[lines],thick=[line]+8, pspacing=1.6,$
  /data,/right,position=[max(tmhrs_big,/nan)-0.16,2.27]
 xyouts,max(tmhrs_big,/nan)-0.545,2.3,'Radiance | Irradiance',alignment=0.5,/data

 axis, yaxis=1,ystyle=1, yrange=[0.0,yrad],/save,ytitle='Radiance (W/m!U2!N nm sr!U-1!N)'
 device, /close

spawn, 'convert +matte "'+dirout+flight+ac+'_TS.ps" "'+dirout+flight+ac+'_TS.png"'
spawn, 'rm -f "'+dirout+flight+ac+'_TS.ps"'
;spawn, 'display "'+dirout+flight+ac+'_TS.png" &'
spawn, 'scp "'+dirout+flight+ac+'_TS.png" leblanse@atoc.colorado.edu:./public_html/ssfr3/data/time/'

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
spawn, 'convert +matte "'+dirout+flight+ac+'_rt.ps" "'+dirout+flight+ac+'_rt.png"'
spawn, 'rm -f "'+dirout+flight+ac+'_rt.ps"'
; spawn, 'display "'+dirout+flight+ac+'_rt.png" &'
 spawn, 'scp "'+dirout+flight+ac+'_rt.png" leblanse@atoc.colorado.edu:./public_html/ssfr3/data/retrieval/'
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
 plot, zenlambda, avg_zen*2.5/yrad, title='Spectra from '+string(tmhrs_big[i_start],$
  format='(F5.2)')+' to '+string(tmhrs_big[i_last],format='(F5.2)')+' UTC(H) at !9m!4='+string(mu,format='(F5.2)'),$
  ytitle='Irradiance (W/m!U2!N nm)', xtitle='Wavelength (nm)', yrange=[0,2.5],ystyle=9,xmargin=[7,7],xrange=[min(zenlambda),1750.]
 oplot, nadlambda, avg_nad, color=250
 
 oplot, zenlambda, (avg_zen+std_zen)*2.5/yrad,linestyle=2
 oplot, nadlambda, avg_nad+std_nad,linestyle=2, color=250
 
 oplot, zenlambda, (avg_zen-std_zen)*2.5/yrad,linestyle=2
 oplot, nadlambda, avg_nad-std_nad,linestyle=2, color=250

 legend, ['Average','+ !9s!4','- !9s!4'], linestyle=[0,2,2], pspacing=1.6,/right,box=0,color=[0,0,0],position=[1300,2.27],/data
 legend, ['|','|','|'],color=[250,250,250],box=0,position=[1588,2.27],linestyle=[0,2,2],pspacing=1.55,/right,/data
 xyouts,1360,2.3,'Radiance | Irradiance  ',alignment=0.5,/data
 axis, yaxis=1,ystyle=1, yrange=[0.0,yrad],/save,ytitle='Radiance (W/m!U2!N nm sr!U-1!N)'
 device, /close
spawn, 'convert +matte "'+dirout+flight+ac+'_spect.ps" "'+dirout+flight+ac+'_spect.png"'
spawn, 'rm -f "'+dirout+flight+ac+'_spect.ps"'
;spawn, 'display "'+dirout+flight+ac+'_spect.png" &'
spawn, 'scp "'+dirout+flight+ac+'_spect.png" leblanse@atoc.colorado.edu:./public_html/ssfr3/data/spectra/'
endif

ends:
end
