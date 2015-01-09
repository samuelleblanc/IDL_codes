; program to plot the contours of ki squared fitting resulting from each iteration of the parameters
; presents the information for each parameter for one sample case

pro plot_kis_samp

date='20120525'
nosum=0


dir='/home/leblanc/SSFR3/plots/kisq/'
restore, '/home/leblanc/SSFR3/data/kis_'+date+'_samp.out'
kis1=kis & sp1=sp
restore, '/home/leblanc/SSFR3/data/kis_'+date+'_samp2.out'
kis2=kis

; plot the spectra
fp=dir+'kis_'+date+'_sample_sp'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=40, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=3.2 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,2,1] & !x.margin=[6,1.5] & !y.margin=[0.155555,0.4] & !y.omargin=[3,1]
  plot, wvl, sp, xtitle='Wavelength (nm)',ytitle='Radiance (Wm!U-2!Nnm!U-1!Nsr!U-1!N)'

  oplot,wvl, sp1,color=50

  plot,wvl,sp/max(sp),xtitle='Wavelength (nm)',ytitle='Normalized radiance'
  oplot, wvl, sp1/max(sp1),color=50

 device, /close
 spawn, 'convert '+fp+'.ps '+fp+'.png'

kp1=kis
kp2=kis

kis1[*,*,1]=0.
kis2[*,*,1]=0.

for p=1,14 do kp1[*,*,p]=total(kis1[*,*,0:p],3)
for p=1,14 do kp2[*,*,p]=total(kis2[*,*,0:p],3)
if n_elements(rri) lt 1 then refs=rr else refs=rri

if nosum then ns='nosum_' else ns=''

; now set up plotting
if 1 then begin
fp=dir+'kis_sample_'+ns+date
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=40, ysize=26
  !p.font=1 & !p.thick=5 & !p.charsize=4.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,4,4] & !x.margin=[6,1.5] & !y.margin=[0.155555,0.4] & !y.omargin=[3,1]

if n_elements(rri) lt 1 then yr=[1,30] else yr=[5,50]

 for p=0, 14 do begin

  if p gt 10 then plot, findgen(10),xtitle='!9t!X',xrange=[1,100],ytitle='r!De!N (!9m!Xm)', xticklen=0.1,yr=yr,/nodata else $
   plot, findgen(10),xtickname=[' ',' ',' ',' ',' ',' '],xrange=[1,100],ytitle='r!De!N (!9m!Xm)', xticklen=0.1,yr=yr,/nodata

  lvls=10.^((findgen(5)-2.5)) ;6.^((findgen(15)-8.)/3.)
  lvls=[0.,lvls]
  lvls=[0.,0.001,0.01,0.05,0.1,0.25,0.5,0.6,0.67,0.75,0.9,1.0,5.0]
  clv=findgen(n_elements(lvls))*254/(n_elements(lvls)-1.)

if nosum then k1=kis1[*,*,p] else k1=kp1[*,*,p]
if nosum then k2=kis2[*,*,p] else k2=kp2[*,*,p]

  contour, k1, taus, refs, /overplot,levels=lvls,c_colors=clv
  contour, k2, taus, refs, /overplot,levels=lvls,c_linestyle=[2,2,2,2,2,2,2,2],c_colors=clv

  z=min(k1,m)
  im=array_indices(kp1[*,*,p],m)
  plots, taus[im[0]],refs[im[1]],psym=7,symsize=3

  z=min(k2,m)
  im=array_indices(kp1[*,*,p],m)
  plots, taus[im[0]],refs[im[1]],psym=1,symsize=3

  xyouts, 70,yr[1]*0.8,'!9h!X!D'+string(p+1,format='(I2)')+'!N',/data,charsize=2.4
  
 endfor

 legend,['First','Second'],textcolors=[0,0],color=[0,0],linestyle=[0,2],pspacing=1.2,box=0,position=[0.82,0.22],/normal,charsize=2.6
 legend,[' ',' '],textcolors=[0,0],color=[0,0],psym=[7,1],pspacing=1.2,box=0,position=[0.8,0.22],/normal,charsize=2.6
 
 contour, [[lvls],[lvls]],lvls,[0,1],/fill,levels=lvls,c_colors=clv,position=[0.78,0.1,0.97,0.12],/normal,xtitle='!9c!X!U2!N',ytickname=[' ',' ',' '],yticks=1,/noerase,/xlog,xr=[lvls[1],lvls[n_elements(lvls)-1]],xticks=3,xticklen=0.3


 device, /close
 spawn, 'convert '+fp+'.ps '+fp+'.png'
endif


stop
end
