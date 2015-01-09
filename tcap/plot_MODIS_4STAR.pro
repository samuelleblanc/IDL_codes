; program to plot the modis optical depth 
; with the flight path overplotted.


pro plot_MODIS_4STAR
dir='/home/leblanc/TCAP/4STAR/'
restore, dir+'sp_4STAR_20130219.out'
latp=lat & lonp=lon

; now restore the MODIS
restore, dir+'../MODIS/MODIS_20130219.out'


fp=dir+'MODIS_4STAR'
  print, 'making plot :'+fp
  set_plot, 'ps'
  loadct, 39, /silent
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
  device, xsize=25, ysize=18
   !p.font=1 & !p.thick=5 & !p.charsize=2.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
   !p.multi=0 & !x.margin=[6,8] & !y.margin=[3.5,1]


n=where(phase ne 3)
tau[n]=-1.
map_set, /lambert, /continents, 42, -71, 0, limit=[40,-72,45,-67],/hires,xmargin=[2,2],ymargin=[5,2]
contour, tau, lon,lat, nlevels=25, /cell_fill,/overplot, min_value=0,max_value=25
map_grid,/label,/box_axes

oplot, lonp,latp,psym=2,color=250
xyouts, lonp[190],latp[190]-0.35,'G-1 flight leg',charsize=2.8,charthick=8.4,alignment=0.5,color=0
xyouts, lonp[190],latp[190]-0.35,'G-1 flight leg',color=250,charsize=2.8,alignment=0.5

contour, [[findgen(25)],[findgen(25)]],findgen(25),[0,1],/cell_fill,position=[0.03,0.17,0.97,0.19],/normal,$
   /noerase,yticks=1,ytickname=[' ',' '],levels=findgen(25),xtitle='MODIS ice cloud optical depth'
; axis, yaxis=1,ystyle=1,yrange=[0.,25.],ytitle='MODIS ice cloud optical depth',yticks=6


device, /close
spawn, 'convert '+fp+'.ps '+fp+'.png'

end
