;program to plot cloud select cloud data
;

@/home/leblanc/IDL/bin/legend.pro
pro plot_cloud
date='20120926'

;restore, '/media/usbdisk3/DC3/SSFR3/'+date+'/out/'+date+'_TS_005.out'
restore, '/media/usbdisk3/DC3/SSFR3/'+date+'/out/'+date+'_calibspcs.out'

fn='/home/leblanc/SSFR3/plots/'+date+'_cloud'
print, 'plotting:'+fn
     set_plot, 'ps'
     loadct, 39, /silent
     device, /encapsulated, /tt_font, set_font='Helvetica Bold'
     device, filename=fn+'.ps'
     device,/color,bits_per_pixel=8., xsize=40, ysize=20
      !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
      !p.multi=[0,2,1] & !x.margin=[6,3]

    plot, zspectra[45,*],nspectra[45,*],title='Cloud radiance and irradiance at 500nm', xtitle='Radiance (W/m!U2!N nm sr)',$
     ytitle='Irradiance (W/!U2!N nm)',psym=3
    oplot, zspectra[45,*],zspectra[45,*]*!PI,color=250, psym=3

legend,['Homogeneous irradiance'],textcolors=250,box=0, /right, /bottom


    plot, zspectra[310,*],nspectra[310,*],title='Cloud radiance and irradiance at 1680nm', xtitle='Radiance (W/m!U2!N nm sr)',$
     ytitle='Irradiance (W/!U2!N nm)',psym=3
    oplot, zspectra[310,*],zspectra[310,*]*!PI,color=250, psym=3

legend,['Homogeneous irradiance'],textcolors=250,box=0, /right, /bottom

     device,/close
     spawn, 'convert '+fn+'.ps '+fn+'.png'

nt=where(finite(tmhrs) eq 1, n)

nul=min(abs(zenlambda-750.),fiz)
nul=min(abs(zenlambda-775.),laz)
nul=min(abs(nadlambda-750.),fin)
nul=min(abs(nadlambda-775.),lan)

radoxa=fltarr(n) ;oxugen a area
irroxa=fltarr(n)
radoxa_l=radoxa ;oxygen a length
irroxa_l=irroxa
radoxa_ln=radoxa ;oxygen a normalized length
irroxa_ln=irroxa
if 0 then begin
set_plot, 'x'
device, decomposed=0
loadct, 39, /silent
window, 0, retain=2, title='Radiance on '+date,xsize=1400,ysize=900
window, 1, retain=2, title='Irradiance on '+date
!p.multi=0
!p.thick=1.5

for i=0, n-1,5 do begin ;plot spectra over all times for making a video

;fn='/home/leblanc/SSFR3/plots/cl_'+string(tmhrs_big[i],format='(F7.4)')
;print, 'saving to '+fn
; set_plot, 'ps'
;     device, /encapsulated, /tt_font, set_font='Helvetica Bold'
;     device, filename=fn+'.ps'
;     device,/color,bits_per_pixel=8., xsize=40, ysize=20
;      !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
;      !p.multi=[0,2,1] & !x.margin=[6,3]
;
wset,0
     plot, zenlambda,zspectra[*,i]/max(zspectra[*,i]),title='Normalized radiance on UTC:'+string(tmhrs_big[i],format='(F7.4)'), $
       xtitle='Wavelength (nm)',ytitle='Normalized radiance at 500nm',yrange=[0,1.2],xrange=[350.,1750.],ystyle=5,psym=3
 
axis, yaxis=1, yrange=[0,0.35],ytitle='Radiance (W/m2 sr nm)',/save
oplot, zenlambda,zspectra[*,i],color=250

;wset,1
;    plot, nadlambda,nad_spect[i,*]/nad_spect[i,45],title='Normalized irradiance on UTC:'+string(tmhrs_big[i],format='(F7.4)'), $
;       xtitle='Wavelength (nm)',ytitle='Normalized irradiance at 500nm',yrange=[0,1.2],xrange=[350.,1750.]
;
;    device, /close
;    spawn, 'convert '+fn+'.ps '+fn+'.png'
;    spawn, 'rm -f '+fn+'.ps'
wait, 0.01
endfor
endif

for i=0, n-1 do begin

   oxar=-zspectra[fiz:laz,i]+interpol(zspectra[[fiz,laz],i],zenlambda[[fiz,laz]],zenlambda[fiz:Laz])
   oxai=-nspectra[fin:lan,i]+interpol(nspectra[[fin,lan],i],nadlambda[[fin,lan]],nadlambda[fin:Lan])

   radoxa[i]=int_tabulated(zenlambda[fin:lan],oxar)
   irroxa[i]=int_tabulated(nadlambda[fin:lan],oxai)

   radoxa_l[i]=mean(zspectra[fiz:laz,i])-min(zspectra[fiz:laz,i])
   irroxa_l[i]=mean(nspectra[fin:lan,i])-min(nspectra[fin:lan,i])

   radoxa_ln[i]=radoxa_l[i]/max(zspectra[*,i])
   irroxa_ln[i]=irroxa_l[i]/max(nspectra[*,i])

endfor

fn='/home/leblanc/SSFR3/plots/oxa_'+date
 set_plot, 'ps'
     loadct, 39, /silent
     device, /encapsulated, /tt_font, set_font='Helvetica Bold'
     device, filename=fn+'.ps'
     device,/color,bits_per_pixel=8., xsize=20, ysize=40
      !p.font=1 & !p.thick=5 & !p.charsize=3.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
      !p.multi=[0,1,3] & !x.margin=[6,3]
     
     plot, tmhrs[nt],radoxa, title='Oxygen-a band area for radiance and irradiance on '+date,xtitle='UTC (h)',ytitle='Oxygen-a area',$
      yrange=[0,4.5]
     oplot, tmhrs[nt],irroxa, color=250
     legend,['Radiance','Irradiance'],textcolors=[0,250],box=0

     plot, tmhrs[nt],radoxa_l, title='Oxygen-a length for radiance and irradiance on '+date,xtitle='UTC (h)',ytitle='Oxygen-a band depth',$
      yrange=[0,0.25]
     oplot, tmhrs[nt],irroxa_l, color=250

     plot, tmhrs[nt],radoxa_ln, title='Oxygen-a normalized length for radiance and irradiance on '+date,xtitle='UTC (h)',ytitle='Oxygen-a band normalized depth',$
      yrange=[0,1.0]
     oplot, tmhrs[nt],irroxa_ln, color=250
     device,/close
     spawn, 'convert '+fn+'.ps '+fn+'.png'

stop
end
