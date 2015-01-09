;plot the contour wavelength over time
@~/IDL/bin/colorbar.pro

pro attrex_contour
date='20111109'
ll='/'
dir='/data/seven/schmidt/attrex/gh/';20111028_SP.out'
restore, dir+ll+date+ll+date+'_SP.out'
set_plot, 'ps'
print, 'plotting contour plot'
    !p.multi=[0,2,1]
    loadct,39
 !p.font=1 & !p.thick=5 & !p.charsize=1.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
    device, /encapsulated
    device, /tt_font, set_font='Helvetica Bold'
    device, filename=dir+date+ll+date+'_contour.ps'
    device,/color,bits_per_pixel=8
    device, xsize=20, ysize=20
 
zero=where(zspectra lt 0)
zspectra[zero]=0.0    
    contour, zspectra, zenlambda, utc, tit='Zenith irradiance',xtit='Wavelength (nm)',ytit='UTC (h)',xr=[350,2200],$
     /cell_fill, max_value=1.8,min_value=0,/xs,/ys,nlevels=25, position=[0.1,0.1,0.45,0.95],c_colors=floor(findgen(25)*10.55+1)
    contour,nspectra,nadlambda, utc, tit='Nadir irradiance',xtit='Wavelength (nm)',ytit='',xr=[350,2200],/cell_fill,/xs,/ys, $
     max_value=1.8,min_value=0,nlevels=25, position=[0.5,0.1,0.85,0.95], c_colors=floor(findgen(25)*10.55+1)
    COLORBAR, POSITION=[0.86, 0.10, 0.88, 0.95],/right,/vertical, range=[0.0,1.8],title='Irradiance (W/m!E2!N nm)',format='(F4.2)'
    device,/close
    spawn,  'convert "'+ dir+date+ll+date+'_contour.ps" "'+dir+date+ll+date+'_contour.png"'
stop
end
