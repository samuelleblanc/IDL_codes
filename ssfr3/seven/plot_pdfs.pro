; program to plot the different stages of the pdf optimal estimation retrieval
; uses the output of run_pdfs.pro

@colorbar.pro
pro plot_pdfs
dir='/home/leblanc/SSFR3/data/'
restore, dir+'pdfs.out'

;save, H, Hm, sprio, snorm, spost, spostm, post, post_jpdf, post_meas, meas_pdf, pars_pdf, $
;      prio_pdf, taus, ref, wp,filename='/home/leblan


xtit=['Curvature at 1000 nm','Derivative at 1200 nm','Derivative at 1500 nm','Ratio between 1200 and 1237 nm','Mean normalized radiance at 1250 nm',$
      'Mean normalized radiance at 1600 nm','Mean normalized radiance at 1000 nm','Curvature at 1550 nm','2!Und!N derivative near 1000 nm','2!nd! derivative near 1200 nm',$
      'Slope between 550 and 680 nm','Slope at 1565 to 1634 nm (Same as Mcbride et al., 2011)','Normalized radiance at 480 nm','Normalized radiance at 650 nm']

if 1 then begin
for np=0, 13 do begin
fp=dir+'meas_pdf_'+string(np,format='(I02)')
print, 'making plot :'+fp
set_plot, 'ps'
loadct, 39, /silent
device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
device, xsize=20, ysize=20
!p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
!p.multi=0 & !x.margin=[8,4]

plot, bins[np,*],meas_pdf[np,*],title='Measurement PDF for parameter '+string(np+1,format='(I2)'),xtitle=xtit[np],$
 ytitle='Normalized probability',xrange=[bins[np,150],bins[np,849]]

device, /close
spawn, 'convert '+fp+'.ps '+fp+'.png'

fp=dir+'mod_pdf_'+string(np,format='(I02)')
print, 'making plot :'+fp
set_plot, 'ps'
loadct, 39, /silent
device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
device, xsize=20, ysize=20
!p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
!p.multi=0 & !x.margin=[8,4]

plot, bins[np,*],pars_pdf[0,0,0,np,*],title='Model PDF for parameter '+string(np+1,format='(I2)'),xtitle=xtit[np],$
 ytitle='Normalized probability',xrange=[bins[np,150],bins[np,849]],yrange=[0,max(pars_pdf[*,*,*,np,*])]
for t=0, n_elements(taus)-1,3 do for r=0, n_elements(ref)-1,2 do for w=0,n_elements(wp)-1 do oplot, bins[np,*],pars_pdf[t,r,w,np,*],thick=1

device, /close
spawn, 'convert '+fp+'.ps '+fp+'.png'

fp=dir+'post_jpdf_'+string(np,format='(I02)')
print, 'making plot :'+fp
set_plot, 'ps'
loadct, 39, /silent
device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
device, xsize=20, ysize=20
!p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
!p.multi=0 & !x.margin=[8,4]

plot, bins[np,*],post_jpdf[0,0,0,np,*],title='Post joint PDF for parameter '+string(np+1,format='(I2)'),xtitle=xtit[np],$
 ytitle='Normalized probability',xrange=[bins[np,150],bins[np,849]],yrange=[0,max(post_jpdf[*,*,*,np,*])]
for t=0, n_elements(taus)-1,3 do for r=0, n_elements(ref)-1,2 do for w=0,n_elements(wp)-1 do oplot, bins[np,*],post_jpdf[t,r,w,np,*],thick=1

device, /close
spawn, 'convert '+fp+'.ps '+fp+'.png'

endfor
endif

fp=dir+'post_pdf'
print, 'making plot :'+fp
set_plot, 'ps'
loadct, 39, /silent
device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
device, xsize=40, ysize=20
!p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
!p.multi=[0,2,1] & !x.margin=[7,4]
yr=[0.-max(post)/15.,max(post)]

contour, post[*,*,0],taus,ref,title='Post PDF for liquid cloud',xtitle='Optical depth',ytitle='Effective radius (um)',nlevels=30,min_value=yr[0],max_value=yr[1],/cell_fill
contour, post[*,*,1],taus,ref,title='Post PDF for ice cloud',xtitle='Optical depth',ytitle='Effective radius (um)',nlevels=30,min_value=yr[0],max_value=yr[1],/cell_fill,xmargin=[7,9]
colorbar,minrange=0,maxrange=yr[1],title='Normalized probability',/vertical,/right,position=[0.92,0.07,0.93,0.95],format='(F4.2)'

device, /close
spawn, 'convert '+fp+'.ps '+fp+'.png'

fp=dir+'example_sp_for_pdf'
print, 'making plot :'+fp
set_plot, 'ps'
loadct, 39, /silent
device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
device, xsize=20, ysize=20
!p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
!p.multi=0 & !x.margin=[7,4]

plot, wl, sp, title='Measured normalized radiance',xtitle='Wavelength (nm)',ytitle='Normalized radiance'

device, /close
spawn, 'convert '+fp+'.ps '+fp+'.png'



stop
end
