; program for outputting special plots for the science meeting on 2011-11-08
; plots the retrievals and others

pro attrex_science
date='20111105'
ll='/'
dir='/data/seven/schmidt/attrex/gh/'
restore, dir+ll+date+ll+date+'_SP.out'
restore, dir+ll+date+ll+date+'_add.out'


; plot the results of the retrieval around the thin cirrus 
fl=where(utc gt 20 and utc lt 26)

if 1 then begin

  set_plot, 'ps'
   loadct, 39, /silent
   device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=dir+ll+date+ll+date+'_rt.ps'
   device, xsize=23, ysize=20
    !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8 & !p.multi=0
     plot, utc[fl],cod_ice[fl],title='Time series of retrieved properties',$
      ystyle=9,yrange=[0,50],xmargin=[6,16],psym=3,ymargin=[3,3],xtitle='UTC time (hours)';xtickname=replicate(' ',7)

     loadct, 0, /silent
     tvlct,r,g,b,/get
     r=reverse(r) & g=reverse(g) & b=reverse(b)
     tvlct,r,g,b
     lvls=reverse(10.0^(-findgen(20)/2.))
     contour,[[residual_ice[fl]],[residual_ice[fl]]],utc[fl],[0,100],/cell_fill,levels=lvls,/overplot  ;take this out to omit residual

     loadct,39,/silent
     oplot,utc[fl],cod_ice[fl],psym=1,color=50,thick=0.5
     axis, yaxis=1,ystyle=1, yrange=[0.0,50.],/save
     oplot, utc[fl],ref_ice[fl], psym=1,color=250,thick=0.5
     legend,['Ice cloud'],textcolors=[10],/right,box=0

    ; plot, utc[fl],cod_liquid[fl],xtitle='UTC time (hours)',$
    ;  ystyle=9,yrange=[0,60],xmargin=[6,12],psym=3,ymargin=[3,0]
    ; tvlct,r,g,b ;load grayscale
    ; contour,[[residual_liquid[fl]],[residual_liquid[fl]]],utc[fl],[0,100],/cell_fill,levels=lvls,/overplot  ;take this out to omit residual
    ; loadct,39,/silent
    ; oplot,utc[fl],cod_liquid[fl],psym=1,color=50,thick=0.5
    ; axis, yaxis=1,ystyle=1, yrange=[0.0,30.],/save;,ytitle='Effective radius (!9m!4m)
    ; oplot, utc[fl],ref_liquid[fl], color=250, psym=4,thick=0.5
    ; legend,['Liquid cloud'],textcolors=[10],/right,box=0
     xyouts,0.05,0.5,'Cloud optical depth',/normal,orientation=90,alignment=0.5,color=50
     xyouts,0.775,0.5,'Effective radius (!9m!4m)',/normal,orientation=90,alignment=0.5, color=250
   
    ;now oplot the altitude   
     axis,0.785,/normal,/save,yaxis=1,ystyle=1,yrange=[min(alt[fl])/1000.,max(alt[fl])/1000.],ytitle='Altitude (km)',color=200
     oplot, utc[fl],alt[fl]/1000.,color=200
     tvlct,r,g,b
     contour, transpose([[lvls],[lvls]]),[0,1],lvls,/ylog,/cell_fill,levels=lvls,color=255,position=[0.88,0.1,0.90,0.9],/normal,/noerase,xticks=1,yticks=1,xtickname=[' ',' '],ytickname=[' ',' ']  ;take this out to omit residual
     axis, yaxis=1,ystyle=1,yrange=[min(lvls),max(lvls)],ytitle='Residual',/ylog,color=255
    device, /close
   spawn, 'convert "'+dir+ll+date+ll+date+'_rt.ps" "'+dir+ll+date+ll+date+'_rt_cirrus.png"'
   spawn, 'convert -alpha off "'+dir+ll+date+ll+date+'_rt.ps" "'+dir+ll+date+ll+date+'_rt_cirrus.png"'
   spawn, 'rm -f "'+dir+ll+date+ll+date+'_rt.ps"'
endif

cg4up=cg4up+50.
cg4dn=cg4dn+50.

; now plot the time series of the Brightness temperature around that time
if 0 then begin
 set_plot, 'ps'
   loadct, 39, /silent
   device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=dir+ll+date+ll+date+'_rt.ps'
   device, xsize=30, ysize=20
    !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8 & !p.multi=0;[0,1,2]
    sigma=5.6704E-8
    bt=(cg4up/sigma)^0.25

    plot, utc[fl],cg4up[fl],title='IR irradiance', xtitle='UTC (Hours)', ytitle='Irradiance (W/m!U2!N)',yrange=[0,400], position=[0.1,0.12,0.7,0.92]
    oplot, utc[fl],cg4dn[fl],color=250
    legend, ['Upwelling','Downwelling'],textcolors=[0,250],box=0

   ; plot, utc[fl],bt[fl],title='Brightness Temperature', xtitle='UTC (Hours)', ytitle='Brightness Temperature (Kelvin)', position=[0.1,0.12,0.7,0.45]

   ; plot, t_ambient[fl]+273.15,alt[fl]/1000.,title='Temperature Profile', xtitle='Temperature (Kelvin)',ytitle='Altitude (km)',/noerase,position=[0.78,0.12,0.98,0.92]

    device, /close
   spawn, 'convert "'+dir+ll+date+ll+date+'_rt.ps" "'+dir+ll+date+ll+date+'_BT.png"'
   spawn, 'convert -alpha off "'+dir+ll+date+ll+date+'_rt.ps" "'+dir+ll+date+ll+date+'_BT.png"'
   spawn, 'rm -f "'+dir+ll+date+ll+date+'_rt.ps"'

endif

;now plot the heating rates
fll=where(utc gt 25.0 and utc lt 25.175)

if 0 then begin
fll=where(utc gt 25.0 and utc lt 25.175)
 set_plot, 'ps'
   loadct, 39, /silent
   device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=dir+ll+date+ll+date+'_rt.ps'
   device, xsize=30, ysize=20
    !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8 & !p.multi=0
    
    hr=nspectra[*,fll]
    ;first interpol nspectra to zspectra
    for i=0, n_elements(fll)-1 do begin
       nspectra[*,fll[i]]=interpol(nspectra[*,fll[i]],nadlambda,zenlambda)
       hr[*,i]=(zspectra[*,fll[i]]-nspectra[*,fll[i]])/(alt[fll[i]]-alt[fll[i]+1])
    endfor
    levels=findgen(30)/300.-0.05
    contour, hr,zenlambda,alt[fll]/1000.,levels=levels,title='Heating rates from solar radiation',ytitle='Altitude (km)',xtitle='Wavelength (nm)', position=[0.1,0.12,0.59,0.92], /cell_fill
   
    contour, transpose([[levels],[levels]]),[0,1],levels,/cell_fill,levels=levels,position=[0.61,0.12,0.63,0.92],/normal,/noerase,xticks=1,yticks=1,xtickname=[' ',' '],ytickname=[' ',' ']  ;take this out to omit residual
     axis, yaxis=1,ystyle=1,yrange=[min(levels),max(levels)],ytitle='Heating Rates (W/m!U3!N)'

    plot, t_ambient[fll]+273.15,alt[fll]/1000.,title='Temperature Profile', xtitle='Temperature (Kelvin)',ytitle='Altitude (km)',/noerase,position=[0.82,0.12,0.98,0.92]

    device, /close
   spawn, 'convert "'+dir+ll+date+ll+date+'_rt.ps" "'+dir+ll+date+ll+date+'_HR.png"'
   spawn, 'convert -alpha off "'+dir+ll+date+ll+date+'_rt.ps" "'+dir+ll+date+ll+date+'_HR.png"'
   spawn, 'rm -f "'+dir+ll+date+ll+date+'_rt.ps"'
endif

;plot the spectral irradiance vs. time and altitude
if 0 then begin
  set_plot, 'ps'
   loadct, 39, /silent
   device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=dir+ll+date+ll+date+'_rt.ps'
   device, xsize=30, ysize=30
    !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8 & !p.multi=[0,1,2]

   wls=[350,500,860,1600]
   cl=[0,70,130,250]
   nz=wls
   nul=min(abs(zenlambda-wls[0]),n)
   nz[0]=n
   plot, utc[fl],nspectra[nz[0],fl]/zspectra[nz[0],fl],title='Albedo',ytitle='Albedo',xtitle='UTC (hours)',yrange=[0,1.0],ymargin=[0,4]
   for i=0, n_elements(wls)-1 do begin
     nul=min(abs(zenlambda-wls[i]),n)
     nz[i]=n
     oplot, utc[fl],nspectra[nz[0],fl]/zspectra[nz[i],fl],color=cl[i]
   endfor
   legend,strtrim(wls)+' nm',textcolors=cl,box=0

;   plot, utc[fl],nspectra[nz[0],fl],title='Upwelling irradiance',ytitle='Irradiance (W/m!U2!N nm)',yticks=1,ytickname=[' ',' '],yrange=[0,1],ymargin=[0,4]
;   for i=0, n_elements(wls)-1 do begin
;     nul=min(abs(zenlambda-wls[i]),n)
;     nz[i]=n
;     oplot, utc[fl],nspectra[nz[i],fl],color=cl[i]
;   endfor
;   legend,strtrim(wls)+' nm',textcolors=cl,box=0

   plot, utc[fl],alt[fl]/1000.,xtitle='UTC (hours)',ytitle='Altitude (km)',ymargin=[4,4]
    device, /close
   spawn, 'convert "'+dir+ll+date+ll+date+'_rt.ps" "'+dir+ll+date+ll+date+'_irr.png"'
   spawn, 'convert -alpha off "'+dir+ll+date+ll+date+'_rt.ps" "'+dir+ll+date+ll+date+'_irr.png"'
   spawn, 'rm -f "'+dir+ll+date+ll+date+'_rt.ps"'

endif

;plot the profiles of net irradiance
if 1 then begin
  set_plot, 'ps'
   loadct, 39, /silent
   device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=dir+ll+date+ll+date+'_rt.ps'
   device, xsize=40, ysize=20
    !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8 & !p.multi=[0,4,1]

   nul=min(abs(zenlambda-500),n)
   plot, zspectra[n,fll]-nspectra[n,fll],alt[fll]/1000.,ytitle='Altitude (km)',xtitle='Net irradiance (W/m!U2!N nm)', title='Profile at 500 nm'
  nul=min(abs(zenlambda-936),n)
   plot, zspectra[n,fll]-nspectra[n,fll],alt[fll]/1000.,ytitle='Altitude (km)',xtitle='Net irradiance (W/m!U2!N nm)', title='Profile at 936 nm' 
nul=min(abs(zenlambda-1600),n)
   plot, zspectra[n,fll]-nspectra[n,fll],alt[fll]/1000.,ytitle='Altitude (km)',xtitle='Net irradiance (W/m!U2!N nm)', title='Profile at 1600 nm'
   plot, cg4dn[fll]-cg4up[fll],alt[fll]/1000.,ytitle='Altitude (km)',xtitle='Net irradiance (W/m!U2!N)', title='Profile in IR'

   spawn, 'convert "'+dir+ll+date+ll+date+'_rt.ps" "'+dir+ll+date+ll+date+'_pro.png"'
   spawn, 'convert -alpha off "'+dir+ll+date+ll+date+'_rt.ps" "'+dir+ll+date+ll+date+'_pro.png"'
   spawn, 'rm -f "'+dir+ll+date+ll+date+'_rt.ps"'

endif
stop
end
