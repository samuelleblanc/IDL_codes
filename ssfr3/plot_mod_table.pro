; program that plots the dependance of each factor on the calculation of the area
; makes some sort of lut with three dependance

@colorbar.pro
@legend.pro
pro plot_mod_table

dir='/home/leblanc/SSFR3/data/'

restore, '/argus/SSFR3/model/area_table3_mod.out'

fp=dir+'area_table3_mod_wv1'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=20, ysize=45
  !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,1,3] & !x.margin=[6,9]

yr=[min(area[*,*,*,*,0]),max(area[*,*,*,*,0])]


plot, tau, area[0,*,1,1,0],psym=-2,title='modeled integrated attenuation for 940 nm WV', xtitle='Cloud optical depth',$
 ytitle='Integrated attenuation', yrange=yr
for i=0, n_elements(base)-1 do $
  oplot, tau, area[i,*,1,1,0], psym=-2, color=i*254/n_elements(base)

colorbar, minrange=min(base),maxrange=max(base), title='Cloud base height (km)',/vertical,/right,$
 format='(F4.2)', position=[0.90,0.70,0.92,0.98]


plot, tau, area[1,*,0,1,0],psym=-2,title='modeled integrated attenuation for 940 nm WV', xtitle='Cloud optical depth',$
 ytitle='Integrated attenuation',yrange=yr
for i=0, n_elements(ref)-1 do $
  oplot, tau, area[1,*,i,1,0], psym=-2, color=i*254/n_elements(ref)

colorbar, minrange=min(ref),maxrange=max(ref), title='Effective radius (um)',/vertical,/right,$
 format='(F5.2)', position=[0.90,0.36,0.92,0.65]

plot, tau, area[1,*,1,0,0],psym=-2,title='modeled integrated attenuation for 940 nm WV', xtitle='Cloud optical depth',$
 ytitle='Integrated attenuation',yrange=yr
for i=0, n_elements(water)-1 do $
  oplot, tau, area[1,*,1,i,0], psym=-2, color=i*254/n_elements(water)

colorbar, minrange=min(water),maxrange=max(water), title='Precipitable water vapor (mm)',/vertical,/right,$
 format='(F5.2)', position=[0.90,0.05,0.92,0.31]

device, /close
spawn, 'convert '+fp+'.ps '+fp+'.png'

fp=dir+'area_table3_mod_wv2'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=20, ysize=45
  !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,1,3] & !x.margin=[6,9]

yr=[min(area[*,*,*,*,1]),max(area[*,*,*,*,1])]


plot, tau, area[0,*,1,1,1],psym=-2,title='Modeled integrated attenuation for 1150 nm WV', xtitle='Cloud optical depth',$
 ytitle='Integrated attenuation', yrange=yr

for i=0, n_elements(base)-1 do $
  oplot, tau, area[i,*,1,1,1], psym=-2, color=i*254/n_elements(base)
colorbar, minrange=min(base),maxrange=max(base), title='Cloud base height (km)',/vertical,/right,$
 format='(F4.2)', position=[0.90,0.70,0.92,0.98]

plot, tau, area[1,*,0,1,1],psym=-2,title='Modeled integrated attenuation for 1150 nm WV', xtitle='Cloud optical depth',$
 ytitle='Integrated attenuation',yrange=yr

for i=0, n_elements(ref)-1 do $
  oplot, tau, area[1,*,i,1,1], psym=-2, color=i*254/n_elements(ref)
colorbar, minrange=min(ref),maxrange=max(ref), title='Effective radius (um)',/vertical,/right,$
 format='(F5.2)', position=[0.90,0.36,0.92,0.65]
plot, tau, area[1,*,1,0,1],psym=-2,title='Modeled integrated attenuation for 1150 nm WV', xtitle='Cloud optical depth',$
 ytitle='Integrated attenuation',yrange=yr

for i=0, n_elements(water)-1 do $
  oplot, tau, area[1,*,1,i,1], psym=-2, color=i*254/n_elements(water)
colorbar, minrange=min(water),maxrange=max(water), title='Precipitable water vapor (mm)',/vertical,/right,$
 format='(F5.2)', position=[0.90,0.05,0.92,0.31]

device, /close
spawn, 'convert '+fp+'.ps '+fp+'.png'

fp=dir+'area_table3_mod_wv3'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=20, ysize=45
  !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,1,3] & !x.margin=[6,9]

yr=[min(area[*,*,*,*,2]),max(area[*,*,*,*,2])]


plot, tau, area[0,*,1,1,2],psym=-2,title='Modeled integrated attenuation for 820 nm WV', xtitle='Cloud optical depth',$
 ytitle='Integrated attenuation', yrange=yr

for i=0, n_elements(base)-1 do $
  oplot, tau, area[i,*,1,1,2], psym=-2, color=i*254/n_elements(base)
colorbar, minrange=min(base),maxrange=max(base), title='Cloud base height (km)',/vertical,/right,$
 format='(F4.2)', position=[0.90,0.70,0.92,0.98]

plot, tau, area[1,*,0,1,2],psym=-2,title='Modeled integrated attenuation for 820 nm WV', xtitle='Cloud optical depth',$
 ytitle='Integrated attenuation',yrange=yr

for i=0, n_elements(ref)-1 do $
  oplot, tau, area[1,*,i,1,2], psym=-2, color=i*254/n_elements(ref)
colorbar, minrange=min(ref),maxrange=max(ref), title='Effective radius (um)',/vertical,/right,$
 format='(F5.2)', position=[0.90,0.36,0.92,0.65]

plot, tau, area[1,*,1,0,2],psym=-2,title='Modeled integrated attenuation for 820 nm WV', xtitle='Cloud optical depth',$
 ytitle='Integrated attenuation',yrange=yr

for i=0, n_elements(water)-1 do $
  oplot, tau, area[1,*,1,i,2], psym=-2, color=i*254/n_elements(water)
colorbar, minrange=min(water),maxrange=max(water), title='Precipitable water vapor (mm)',/vertical,/right,$
 format='(F5.2)', position=[0.90,0.05,0.92,0.31]
device, /close
spawn, 'convert '+fp+'.ps '+fp+'.png'

fp=dir+'area_table3_mod_oxa'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=20, ysize=45
  !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,1,3] & !x.margin=[6,9]

yr=[min(area[*,*,*,*,3]),max(area[*,*,*,*,3])]


plot, tau, area[0,*,1,1,3],psym=-2,title='Modeled integrated attenuation for Oxygen-A', xtitle='Cloud optical depth',$
 ytitle='Integrated attenuation', yrange=yr

for i=0, n_elements(base)-1 do $
  oplot, tau, area[i,*,1,1,3], psym=-2, color=i*254/n_elements(base)
colorbar, minrange=min(base),maxrange=max(base), title='Cloud base height (km)',/vertical,/right,$
 format='(F4.2)', position=[0.90,0.70,0.92,0.98]

plot, tau, area[1,*,0,1,3],psym=-2,title='Modeled integrated attenuation for Oxygen-A', xtitle='Cloud optical depth',$
 ytitle='Integrated attenuation',yrange=yr

for i=0, n_elements(ref)-1 do $
  oplot, tau, area[1,*,i,1,3], psym=-2, color=i*254/n_elements(ref)
colorbar, minrange=min(ref),maxrange=max(ref), title='Effective radius (um)',/vertical,/right,$
 format='(F5.2)', position=[0.90,0.36,0.92,0.65]

plot, tau, area[1,*,1,0,3],psym=-2,title='Modeled integrated attenuation for Oxygen-A', xtitle='Cloud optical depth',$
 ytitle='Integrated attenuation',yrange=yr

for i=0, n_elements(water)-1 do $
  oplot, tau, area[1,*,1,i,3], psym=-2, color=i*254/n_elements(water)
colorbar, minrange=min(water),maxrange=max(water), title='Precipitable water vapor (mm)',/vertical,/right,$
 format='(F5.2)', position=[0.90,0.05,0.92,0.31]

device, /close
spawn, 'convert '+fp+'.ps '+fp+'.png'


fp=dir+'area_table3_mod_ref_tau'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=20, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=0 & !x.margin=[6,9]
 clbl=reform(rebin([0,0,1],3,10),30)
lvl3=findgen(30)*(max(area[*,*,*,*,3])-min(area[*,*,*,*,3]))/30.+min(area[*,*,*,*,3])
lvl2=findgen(30)*(max(area[*,*,*,*,2])-min(area[*,*,*,*,2]))/30.+min(area[*,*,*,*,2])
lvl1=findgen(30)*(max(area[*,*,*,*,1])-min(area[*,*,*,*,1]))/30.+min(area[*,*,*,*,1])
lvl0=findgen(30)*(max(area[*,*,*,*,0])-min(area[*,*,*,*,0]))/30.+min(area[*,*,*,*,0])

  contour, area[1,*,*,1,3],tau, ref,levels=lvl3, /cell_fill,xtitle='Cloud optical depth',ytitle='Effective Radius (um)',title='Modeled attenuation sections'
  contour, area[1,*,*,1,2],tau, ref,levels=lvl2, C_thick=[2,2,6],c_label=clbl,c_colors=clbl*0, c_charsize=2.,/overplot
  contour, area[1,*,*,1,1],tau, ref,levels=lvl1, C_thick=[2,2,6],c_label=clbl,c_colors=clbl*0+255, c_charsize=2.,/overplot
  tvlct, 200,200,200,200
  contour, area[1,*,*,1,0],tau, ref,levels=lvl0, C_thick=[2,2,6],c_label=clbl,c_colors=clbl*0+200,c_charsize=2., /overplot
  colorbar, minrange=min(area[*,*,*,*,3]),maxrange=max(area[*,*,*,*,3]), title='Oxygen-A integrated attenuation (W/m!U2!N)',/vertical,/right,$
 format='(F5.2)', position=[0.82,0.12,0.84,0.93]
  legend, ['820 nm','1150 nm','940 nm'],box=0, textcolors=[0,255,200],/right

device, /close
spawn, 'convert '+fp+'.ps '+fp+'.png'

fp=dir+'area_table3_mod_base_wat'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=20, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=0 & !x.margin=[6,9]
 clbl=reform(rebin([0,0,1],3,10),30)
lvl3=findgen(30)*(max(area[*,*,*,*,3])-min(area[*,*,*,*,3]))/30.+min(area[*,*,*,*,3])
lvl2=findgen(30)*(max(area[*,*,*,*,2])-min(area[*,*,*,*,2]))/30.+min(area[*,*,*,*,2])
lvl1=findgen(30)*(max(area[*,*,*,*,1])-min(area[*,*,*,*,1]))/30.+min(area[*,*,*,*,1])
lvl0=findgen(30)*(max(area[*,*,*,*,0])-min(area[*,*,*,*,0]))/30.+min(area[*,*,*,*,0])

  contour, reform(area[*,2,2,*,3]),base, water,levels=lvl3, /cell_fill,xtitle='Cloud base height (km)',ytitle='Precipitable water (mm)',title='Modeled attenuation sections'
  contour, reform(area[*,2,2,*,2]),base, water,levels=lvl2, C_thick=[2,2,6],c_label=clbl,c_colors=clbl*0, c_charsize=2.,/overplot
  contour, reform(area[*,2,2,*,1]),base, water,levels=lvl1, C_thick=[2,2,6],c_label=clbl,c_colors=clbl*0+255, c_charsize=2.,/overplot
  tvlct, 200,200,200,200
  contour, reform(area[*,2,2,*,0]),base, water,levels=lvl0, C_thick=[2,2,6],c_label=clbl,c_colors=clbl*0+200,c_charsize=2., /overplot
  colorbar, minrange=min(area[*,*,*,*,3]),maxrange=max(area[*,*,*,*,3]), title='Oxygen-A integrated attenuation (W/m!U2!N)',/vertical,/right,$
 format='(F5.2)', position=[0.82,0.12,0.84,0.93]
  legend, ['820 nm','1150 nm','940 nm'],box=0, textcolors=[0,255,200],/right

device, /close
spawn, 'convert '+fp+'.ps '+fp+'.png'



stop
end
