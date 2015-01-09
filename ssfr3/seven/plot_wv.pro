; program to plot the water vapor distance
; 

@legend.pro
pro plot_wv

dir='/home/leblanc/SSFR3/data/'
cloud=0
fac=1

if fac then fa='_fac' else fa=''
if cloud then restore, dir+'wv_oxa_cl'+fa+'.out' else restore, dir+'wv_oxa_clear'+fa+'.out'
;restore, dir+'water_disd.out'
;stop
if cloud then fn=dir+'path_cloud_disd'+fa else fn=dir+'path_clear_disd'+fa
set_plot,'ps'
print, 'making plot :'+fn
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fn+'.ps'
 device, xsize=40, ysize=40
 !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
 !p.multi=[0,2,2] & !x.margin=[7,2] &!x.omargin=[0,0]

if cloud then begin 
  xr=[100,120]
  t=[0,210]
  w=[0,30]
  od=cod
  ar=[0,25]
  tit='Cloudy'
  ytit='Cloud'
endif else begin
  xr=[80,95]
  t=[0,0.14]
  w=[0,15]
  od=tau[250,*]
  ar=[0,1]
  tit='Clear'
  ytit='Aerosol'
endelse  
  
  is=where(disd gt xr[0] and disd lt xr[1])
  plot, disd[is], water[is], psym=2, xrange=xr, xtitle='Water vapor width (nm)',ytitle='Precipitable water depth (mm)',title=tit+' sky water vapor comparison',yrange=w
  dw=linfit(disd[is],water[is])
  r=correlate(disd[is],water[is])
  oplot, findgen(20)+xr[0], (findgen(20)+xr[0])*dw[1]+dw[0], linestyle=2, color=250

  legend,['y='+strtrim(dw[0],2)+'+'+strtrim(dw[1],2)+'x','R='+strtrim(r,2)],box=0,textcolors=[250,250]

  plot, disd[is], od[is], psym=2, xrange=xr, xtitle='Water vapor width (nm)',ytitle=ytit+' optical depth',title='Optical depth vs. water vapor width',yrange=t

  plot, disd[is], oxa[is], psym=2, xrange=xr, xtitle='Water vapor width (nm)',ytitle='Oxygen-a band integrated area',title='Oxygen-a band vs. water vapor width'

  plot, oxa[is], od[is], psym=2,  xtitle='Oxygen-a band integrated area',ytitle=ytit+' optical depth',title='Optical depth vs. Oxygen-a band',yrange=t

 device, /close
 spawn, 'convert '+fn+'.ps '+fn+'.png'


;;;; now plot the relationships for the area;;;;;
if cloud then fn=dir+'path_cloud_area'+fa else fn=dir+'path_clear_area'+fa
set_plot,'ps'
print, 'making plot :'+fn
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fn+'.ps'
 device, xsize=40, ysize=40
 !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
 !p.multi=[0,2,2] & !x.margin=[7,2]
  xr=ar
  is =where(finite(area) eq 1)
  ;is=where(disd gt xr[0] and disd lt xr[1])
  plot, area[is], water[is], psym=2, xrange=xr, xtitle='Water vapor integrated area',ytitle='Precipitable water depth (mm)',title=tit+' sky water vapor comparison',yrange=w
  dw=linfit(area[is],water[is])
  r=correlate(area[is],water[is])
  oplot, findgen(30)+xr[0], (findgen(30)+xr[0])*dw[1]+dw[0], linestyle=2
  
  is=where(sza gt 45.)
  oplot, area2[is],water[is],psym=2, color=70
  dw2=linfit(area2[is],water[is])
  r2=correlate(area2[is],water[is])
  oplot, findgen(30)+xr[0], (findgen(30)+xr[0])*dw2[1]+dw2[0], linestyle=2, color=70
  is=where(finite(area) eq 1 and finite(od) eq 1)
  ;oplot, area2v2,water,psym=2, color=180
  legend,['940 nm Band, R='+strtrim(r,2),'1150 nm Band, R='+strtrim(r2,2)],box=0,textcolors=[0,70]

  plot, area[is], od[is], psym=2, xrange=xr, xtitle='Water vapor integrated area',ytitle=ytit+' optical depth',title='Optical depth vs. water vapor width',yrange=t
  ao=linfit(area[is],od[is])
  rao=correlate(area[is],od[is])
  oplot, findgen(30),findgen(30)*ao[1]+ao[0],linestyle=2

  oplot, area2[is],od[is], psym=2,color=70
  ao2=linfit(area2[is],od[is])
  rao2=correlate(area2[is],od[is])
  oplot, findgen(30),findgen(30)*ao2[1]+ao2[0],linestyle=2,color=70
  
  legend,['940 nm Band,R='+strtrim(rao,2),'1150 nm Band,R='+strtrim(rao2,2)],textcolors=[0,70],box=0
  
  plot, area[is], oxa[is], psym=2, xrange=xr, xtitle='Water vapor integrated area',ytitle='Oxygen-a band integrated area',title='Oxygen-a band vs. water vapor width'
  ax=linfit(area[is],oxa[is])
  rax=correlate(area[is],oxa[is])
  oplot, findgen(30),findgen(30)*ax[1]+ax[0],linestyle=2
  
  oplot, area2[is], oxa[is], psym=2, color=70
  ax2=linfit(area2[is],oxa[is])
  rax2=correlate(area2[is],oxa[is])
  oplot, findgen(30),findgen(30)*ax2[1]+ax2[0],linestyle=2,color=70

  legend,['940 nm Band,R='+strtrim(rax,2),'1150 nm Band,R='+strtrim(rax2,2)],textcolors=[0,70],box=0
  plot, oxa[is], od[is], psym=2,  xtitle='Oxygen-a band integrated area',ytitle=ytit+' optical depth',title='Optical depth vs. Oxygen-a band',yrange=t
  ox=linfit(oxa[is],od[is])
  rox=correlate(od[is],oxa[is])
  oplot, findgen(30),findgen(30)*ox[1]+ox[0],linestyle=2
  legend,['R='+strtrim(rox,2)],textcolors=[0],box=0

 device, /close
 spawn, 'convert '+fn+'.ps '+fn+'.png'


;special plotting
; plot the relationships between the area and area2 with water for each set of mu
if 1 then begin
big=25.
fn=dir+'area_per_mu_'+string(5.0/big,format='(F3.1)')+fa
set_plot,'ps'
print, 'making plot :'+fn
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fn+'.ps'
 device, xsize=40, ysize=40
 !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
 !p.multi=[0,2,2] & !x.margin=[7,2] & !x.omargin=[0,9]

 mu=1./cos(sza*!dtor)
 bins=findgen(big+1)*(max(mu)-min(mu))/big+min(mu)
 r=fltarr(big)
 lin=fltarr(big,2)
 r2=r
 lin2=lin
 mean_mu=fltarr(big)
 mmu=3.5
;area2=area2v2
 plot, water,area,psym=2,title='Water vapor per SZA for 940 nm band', ytitle='Integrated water vapor absorption',xtitle='Precipitable water vapor (mm)'

 for i=0,big-1 do begin
   n=where(mu ge bins[i] and mu lt bins[i+1],num)
   mean_mu[i]=(bins[i]+bins[i+1]+min(mu))/2.
   if num gt 1 then begin
     r[i]=correlate(area[n], water[n])
     l=linfit(area[n],water[n])
     lin[i,*]=l
     oplot, water[n],area[n],psym=2, color=(mean_mu[i]-1.)/(mmu-1.)*255.
     oplot, findgen(15)/10.*l[1]+l[0],findgen(15)/10.,color=(mean_mu[i]-1.)/(mmu-1.)*255.,linestyle=2
   endif    
 endfor
  
  plot, water,area2,psym=2,title='Water vapor per SZA for 1150 nm band', ytitle='Integrated water vapor absorption',xtitle='Precipitable water vapor (mm)'

 for i=0,big-1 do begin
   n=where(mu ge bins[i] and mu lt bins[i+1],num)
   if num gt 1 then begin
     r2[i]=correlate(area[n], water[n])
     l=linfit(area2[n],water[n])
     lin2[i,*]=l
     oplot, water[n],area2[n],psym=2, color=(mean_mu[i]-1.)/(mmu-1.)*255.
     oplot, findgen(15)/15.*l[1]+l[0],findgen(15)/15.,color=(mean_mu[i]-1.)/(mmu-1.)*255.,linestyle=2
   endif
 endfor

 !x.margin=[7,5]
 h1=histogram(mu,nbins=big)
 h2=histogram(mu,nbins=big) 
 plot, mean_mu,r, title='Correlation coefficient for 940 nm band', xtitle='Airmass factor',ytitle='Correlation coefficient',ystyle=9
 axis,yaxis=1,yrange=[0,max(h1)],ytitle='Histogram of points',color=70,/save
 oplot, mean_mu,h1,psym=10,color=70
 plot, mean_mu,r2, title='Correlation coefficient for 1150 nm band', xtitle='Airmass factor',ytitle='Correlation coefficient',ystyle=9
 axis,yaxis=1,yrange=[0,max(h2)],ytitle='Histogram of points',color=70,/save

 oplot, mean_mu,h2,psym=10,color=70

 contour, transpose([[mean_mu],[mean_mu]]),[0,1],mean_mu,/cell_fill,nlevels=20,position=[0.91,0.13,0.93,0.93],/normal,/noerase,xticks=1,yticks=1,xtickname=[' ',' '],ytickname=[' ',' ']  ;take this out to omit residual
 axis, yaxis=1,ystyle=1,yrange=[1,mmu],ytitle='Airmass factor',yticks=6

 device, /close
 spawn, 'convert '+fn+'.ps '+fn+'.png'

endif

stop
end
