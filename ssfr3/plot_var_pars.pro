; program to plot the variation of the spectra due to variations in 
; precipitable water, surface albedo, and cloud height

pro plot_var_pars


lb=['z','pw','ab']
date='20120525'


dir='C:\Users\Samuel\Research\SSFR3\'
print, 'restoring to all sp_par files'
restore, dir+'data\sp_par_'+lb[0]+'_v1_'+date+'.out'
parsz=pars & spz=sp_hiu & nz=n_elements(z)
restore, dir+'data\sp_par_'+lb[1]+'_v1_'+date+'.out'
parsw=pars & spw=sp_hiu & nw=n_elements(pw)
restore, dir+'data\sp_par_'+lb[2]+'_v1_'+date+'.out'
parsa=pars & spa=sp_hiu & na=n_elements(ab)
restore, dir+'data\sp_par_re_v1_'+date+'.out'
parsre=pars & spre=sp_hiu & nre=n_elements(re)


wvl=zenlambda

; make nans the values for parameter 15 that are equal to 0.3 - no more signal
nuls=where(parsz[*,*,*,14,0] eq 0.03)
for n=0, n_elements(nuls)-1 do begin
  nn=array_indices(parsz[*,*,*,14,0],nuls[n])
  parsz[nn[0],nn[1],nn[2],14,*]=!values.f_nan
  parsw[nn[0],nn[1],nn[2],14,*]=!values.f_nan
  parsa[nn[0],nn[1],nn[2],14,*]=!values.f_nan
  parsre[nn[0],nn[1],nn[2],14,*]=!values.f_nan
endfor

; plot the figures for only a few cases
; cases used in last paper:
; ice : tau=8, ref=25
; liq : tau=40,ref=7
; extra cases can be:
; or not liq : tau=3, ref=5
; ice : tau=20,  ref=50

; z, ab, pw

; now set up plotting
if 0 then begin
fp=dir+'plots\p2\sp_var'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=40, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=4.2 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,3,2,0,0] & !x.margin=[6,1.5] & !y.margin=[0.15,0.4] & !y.omargin=[3,1] & !x.omargin=[0,0]
 
  tvlct,0,150,0,150 
  tvlct,100,100,100,100
  t=[4,39,19]
  r=[24,6,49]
  w=[1,0,1]
  cl=[50,250,150]

  ; only show a subset of the modeled 
  z=[0.5,3.0,7.0]
  pw=[5,15,25]

  xtn=[' ',' ',' ',' ',' ',' ',' ',' ']

  ss=fltarr(n_elements(wvl),3)
  for d=0,  2 do ss[*,d]=reform(spz[t[d],r[d],w[d],*,1])

  plot,  wvl,spz[t[0],r[0],w[0],*,0],ytitle='Radiance (Wm!U-2!Nnm!U-1!Nsr!U-1!N)',/nodata,xtickname=xtn
  for d=0, 2 do for i=0, nz-1,2 do oplot, wvl,spz[t[d],r[d],w[d],*,i],color=cl[d],linestyle=i
  legend, string(z,format='(F3.1)')+' km',linestyle=[0,2,4],box=0,/right,pspacing=1.2,charsize=1.8

  plot,  wvl,spa[t[0],r[0],w[0],*,0],ytitle='Radiance (Wm!U-2!Nnm!U-1!Nsr!U-1!N)',/nodata,xtickname=xtn
  for d=0, 2 do for i=0, na-1 do oplot, wvl,spa[t[d],r[d],w[d],*,i],color=cl[d],linestyle=i
  legend,ab,linestyle=indgen(na),box=0,/right,pspacing=1.2,charsize=1.8

  plot,  wvl,spw[t[0],r[0],w[0],*,0],ytitle='Radiance (Wm!U-2!Nnm!U-1!Nsr!U-1!N)',/nodata,xtickname=xtn
  for d=0, 2 do for i=0, nw-1,2 do oplot, wvl,spw[t[d],r[d],w[d],*,i],color=cl[d],linestyle=i
  legend,string(pw,format='(I2)')+' mm',linestyle=[0,2,4],box=0,/right,pspacing=1.2,charsize=1.8

  plot,  wvl,(spz[t[0],r[0],w[0],*,0]-ss[*,0])/max(ss[*,0])*100.,ytitle='Radiance difference (%)',$
    /nodata,xtitle='Wavelength (nm)',yr=[-3,8]
  for d=0, 2 do for i=0, nz-1 ,2 do oplot, wvl,(spz[t[d],r[d],w[d],*,i]-ss[*,d])/max(ss[*,d])*100.,color=cl[d],linestyle=i 
  ;legend, ['!9t!X=4, r!De!N=25 !9m!Xm, ice','!9t!X=40, r!De!N=7 !9m!Xm, liquid','!9t!X=20, r!De!N=50 !9m!Xm, ice'],$
  ; box=0, charsize=1.8, textcolors=cl
  oplot, wvl,wvl*0.,linestyle=1,color=100
 
  plot,  wvl,(spa[t[0],r[0],w[0],*,0]-ss[*,0])/max(ss[*,0])*100.,ytitle='Radiance difference (%)',$
   /nodata,xtitle='Wavelength (nm)',yr=[-2,3]
  for d=0, 2 do for i=1, na-1 do oplot, wvl,(spa[t[d],r[d],w[d],*,i]-ss[*,d])/max(ss[*,d])*100.,color=cl[d],linestyle=i 
  oplot,wvl,wvl*0.,linestyle=1,color=100
  legend, ['A - !9t!X=40, r!De!N=7 !9m!Xm, liquid','B - !9t!X=20, r!De!N=50 !9m!Xm, ice','C - !9t!X=5, r!De!N=25 !9m!Xm, ice'],$
   box=0, charsize=1.8, textcolors=[250,150,50],/right,/bottom
 
  plot,  wvl,(spw[t[0],r[0],w[0],*,0]-ss[*,0])/max(ss[*,0])*100.,ytitle='Radiance difference (%)',$
   /nodata,xtitle='Wavelength (nm)',yr=[-10,10]
  for d=0, 2 do for i=0, nw-1 ,2 do oplot, wvl,(spw[t[d],r[d],w[d],*,i]-ss[*,d])/max(ss[*,d])*100.,color=cl[d],linestyle=i 
  oplot,wvl,wvl*0.,linestyle=1,color=100
device, /close
spawn, 'convert '+fp+'.ps '+fp+'.png'
stop
endif

if 1 then begin
fp=dir+'plots\p2\sp_par'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=35, ysize=15
  !p.font=1 & !p.thick=5 & !p.charsize=2.5 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,2,1,0,0] & !x.margin=[6,0.2] & !y.margin=[0.15,0.4] & !y.omargin=[3,1] & !x.omargin=[0.5,1]
  
  tvlct,0,150,0,150
  tvlct,100,100,100,100

  t=[4,39,19]
  r=[24,6,49]
  w=[1,0,1] 
  cl=[50,250,150]

  plot, wvl, spz[t[0],r[0],w[0],*,1], ytitle='Radiance (Wm!U-2!Nnm!U-1!Nsr!U-1!N)',xtitle='Wavelength (nm)',/nodata
  for i=0, 2 do oplot, wvl, spz[t[i],r[i],w[i],*,1],color=cl[i]
  ;i=0 & oplot, wvl, spz[t[i],r[i],1,*,1],color=cl[i],linestyle=2
  ;i=2 & oplot, wvl, spz[t[i],r[i],1,*,1],color=cl[i],linestyle=2
  legend,['A - !9t!X=40, r!De!N=7 !9m!Xm, liquid','B - !9t!X=20, r!De!N=50 !9m!Xm, ice','C - !9t!X=5, r!De!N=25 !9m!Xm, ice'],/right,$
   textcolors=[250,150,50],color=[255,255,255],charsize=1.8,box=0


  plot, wvl, spz[t[0],r[0],0,*,1]/max(spz[t[0],r[0],0,*,1]), ytitle='Normalized radiance',xtitle='Wavelength (nm)',/nodata
  for i=0, 2 do oplot, wvl, spz[t[i],r[i],w[i],*,1]/max(spz[t[i],r[i],w[i],*,1]),color=cl[i]
  ;for i=0, 2 do oplot, wvl, spz[t[i],r[i],1,*,1]/max(spz[t[i],r[i],1,*,1]),color=cl[i],linestyle=2


if 1 then begin
loadct, 0, /silent
  ;par 11
  polyfill, [550.,550.,680.,680.,550.],[0,0.91,0.91,0,0],color=230
  xyouts, 651., 0.88,'}',charsize=4.7,orientation=90,color=230,alignment=0.
  xyouts, 555., 0.95,'!9h!X!D11!N',color=200

  ;par7
  polyfill, [1000.,1000.,1050.,1050.,1000.],[0,0.85,0.85,0,0],color=244
  xyouts, 1041.,0.835,'}',charsize=2.0,orientation=90.,color=244,alignment=0.
  xyouts, 1000.,0.88,'!9h!X!D7!N',color=200
  ;par9
  polyfill, [1000.,1000.,1077.,1077.,1000.],[0,0.75,0.75,0,0],color=230
  xyouts, 1061.,0.73,'}',charsize=3.0,orientation=90.,color=230,alignment=0.
  xyouts, 1018.,0.78,'!9h!X!D9!N',color=200
  ;par1
  polyfill, [1000.,1000.,1077.,1077.,1000.],[0,0.65,0.65,0,0],color=215
  xyouts, 1061.,0.63,'}',charsize=3.0,orientation=90.,color=215,alignment=0.
  xyouts, 1018.,0.68,'!9h!X!D1!N',color=200
  ;par 13
  plots,[1000.,1000.,1032.,1065.,1065.],[0.,0.565,0.585,0.565,0.],color=180
  xyouts, 1012.,0.595,'!9h!X!D13!N',color=180
  ;par 12
  plots,[1040.,1040.],[0.,0.495],color=150
  xyouts, 1006.,0.515,'!9h!X!D12!N',color=150

  ;par 14
  plots,[600.,600.,735.,870.,870.],[0.,0.87,0.90,0.87,0.],color=200
  xyouts, 712.,0.92,'!9h!X!D14!N',color=200

  ;par5
  polyfill, [1248.,1248.,1270.,1270.,1248.],[0,0.55,0.55,0,0],color=240
  xyouts, 1225.,0.568,'!9h!X!D5!N',color=200
  ;par 10
  polyfill, [1200.,1200.,1300.,1300.,1200.],[0,0.45,0.45,0,0],color=215
  xyouts, 1278.,0.428,'}',charsize=3.7,orientation=90,color=215,alignment=0.
  xyouts, 1208.,0.488,'!9h!X!D10!N',color=215
  ;par4
  plots,[1200.,1200.,1219.,1237.,1237.],[0.,0.34,0.36,0.34,0.],color=150
  xyouts, 1198.,0.38,'!9h!X!D4!N',color=150
  ;par2 
  plots,[1193.,1193.],[0,0.63],color=100
  xyouts,1174.,0.65,'!9h!X!D2!N',color=150

    ;par 15
  polyfill, [1565.,1565.,1634.,1634.,1565.],[0,0.43,0.43,0,0],color=244
  xyouts, 1620.,0.420,'}',charsize=2.7,orientation=90,color=244,alignment=0.
  xyouts, 1570.,0.46,'!9h!X!D15!N',color=200
  ;par 6
  polyfill, [1565.,1565.,1644.,1644.,1565.],[0,0.33,0.33,0,0],color=230
  xyouts, 1626.,0.320,'}',charsize=2.77,orientation=90,color=230,alignment=0.
  xyouts, 1580.,0.36,'!9h!X!D6!N',color=200
  ;par8
  polyfill, [1493.,1493.,1600.,1600.,1493.],[0,0.25,0.25,0,0],color=215
  xyouts, 1578.,0.225,'}',charsize=4.0,orientation=90,color=215,alignment=0.
  xyouts, 1508.,0.288,'!9h!X!D8!N',color=200
  ;par3
  plots, [1492,1492],[0,0.4],color=150
  xyouts,1477.,0.42,'!9h!X!D3!N',color=200
endif
loadct, 39
  tvlct, 0, 150,0,150
  for i=0, 2 do oplot, wvl, spz[t[i],r[i],w[i],*,1]/max(spz[t[i],r[i],w[i],*,1]),color=cl[i]
  ;for i=0, 2 do oplot, wvl, spz[t[i],r[i],1,*,1]/max(spz[t[i],r[i],1,*,1]),color=cl[i],linestyle=2

device, /close
spawn, 'convert '+fp+'.ps '+fp+'.png'
endif

if 0 then begin
fp=dir+'plots\p2\par_var'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=40, ysize=48
  !p.font=1 & !p.thick=8 & !p.charsize=6.5 & !x.style=1 & !y.style=0 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,3,15,0,1] & !x.margin=[0,0.8] & !y.margin=[0.15,0] & !y.omargin=[3,5] & !x.omargin=[10,8]
 
  tvlct,0,150,0,150
  tvlct,100,100,100,100

  xte=[' ',' ',' ',' ',' ',' ',' ']
  for i=0, 14 do begin
   if i eq 14 then begin
     xtit='!9t!X' & xtn=['','','','','','','']
   endif else begin
     xtit=' ' & xtn=[' ',' ',' ',' ',' ',' ',' ']
   endelse
     h='!9h!X!D'+string(i+1,format='(I2)')+'!N'
     yr=[min(parsz[*,r,w,i,*]),max(parsz[*,r,w,i,*])]
     plot, taus, parsz[*,r[0],w[0],i,0],xtitle=xtit,ytitle=h,yr=yr,/nodata,xtickname=xtn,ticklen=0.1,yticks=2
     for k=0, nz-1, 2 do for j=0,2 do oplot, taus, parsz[*,r[j],w[j],i,k], color=cl[j],linestyle=k
  endfor  

   for i=0, 14 do begin 
   if i eq 14 then begin 
     xtit='!9t!X' & xtn=['','','','','','',''] 
   endif else begin 
     xtit=' ' & xtn=[' ',' ',' ',' ',' ',' ',' '] 
   endelse 
     h='!9h!X!D'+string(i+1,format='(I2)')+'!N' 
     yr=[min(parsz[*,r,w,i,*]),max(parsz[*,r,w,i,*])] 
     plot, taus, parsa[*,r[0],w[0],i,0],xtitle=xtit,yr=yr,/nodata,xtickname=xtn,ytickname=xte,ticklen=0.1,yticks=2
     for k=0, nz-1, 2 do for j=0,2 do oplot, taus, parsa[*,r[j],w[j],i,k], color=cl[j],linestyle=k 
  endfor

   for i=0, 14 do begin 
   if i eq 14 then begin 
     xtit='!9t!X' & xtn=['','','','','','',''] 
   endif else begin 
     xtit=' ' & xtn=[' ',' ',' ',' ',' ',' ',' '] 
   endelse 
     h='!9h!X!D'+string(i+1,format='(I2)')+'!N' 
     yr=[min(parsz[*,r,w,i,*]),max(parsz[*,r,w,i,*])] 
     plot, taus, parsw[*,r[0],w[0],i,0],xtitle=xtit,yr=yr,/nodata,xtickname=xtn,ytickname=xte,ticklen=0.1,yticks=2
     for k=0, nz-1, 2 do for j=0,2 do oplot, taus, parsw[*,r[j],w[j],i,k], color=cl[j],linestyle=k 
  endfor
  legend, ['liquid','  r!De!N=7 !9m!Xm','ice','  r!De!N=50 !9m!Xm','ice','  r!De!N=25 !9m!Xm'],$
   box=0, charsize=3.0, textcolors=[cl[1],cl[1],cl[2],cl[2],cl[0],cl[0]],position=[0.82,0.7],/normal
  legend,['0.5 km','3.0 km ','7.0 km'],linestyle=[0,2,4],position=[0.2,0.96],/normal,box=0,charsize=2.5,pspacing=	1.2
  legend,['2012-05-24','2012-08-04','2012-08-12','2012-08-20','2012-09-13'],linestyle=[0,1,2,3,4],$
   box=0,position=[0.4,0.99],/normal,charsize=2.5,pspacing=1.2
  legend,['5 mm','15 mm','25 mm'],linestyle=[0,2,4],box=0,position=[0.65,0.96],/normal,charsize=2.5,pspacing=1.2


device, /close
spawn, 'convert '+fp+'.ps '+fp+'.png'
endif


if 0 then begin
fp=dir+'plots\p2\sp_dref'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=35, ysize=10
  !p.font=1 & !p.thick=5 & !p.charsize=3.5 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,3,1,0,0] & !x.margin=[6,1.0] & !y.margin=[0.15,0.4] & !y.omargin=[3,1] & !x.omargin=[0,0]

  tvlct,0,150,0,150
  tvlct,100,100,100,100
  t=[4,39,19]
  r=[24,6,49]
  w=[1,0,1] 
  cl=[50,250,150]
  s="265B  ;"
  
  zs=findgen(11)*0.15
  res=zs/0.15+5.
  plot,res,zs,psym=-7,linestyle=2,ytitle='In cloud height (km)', yr=[0,1.5],xr=[3,17],xtickv=[5,10,15],$
   xtickname=['r!De!N/2','r!De!N','3r!De!N/2'],xticks=2,symsize=0.5
  oplot,zs*0+10.,zs,psym=-7,symsize=0.5
  legend,['Constant r!De!N','r!De!N !9'+string(s)+'!X z'],box=0,position=[10,0.4],charsize=1.6,pspacing=1.2,linestyle=[0,2]


  ss=fltarr(n_elements(wvl),3) 
  for d=0,  2 do ss[*,d]=reform(spz[t[d],r[d],w[d],*,1])

  plot,  wvl,spre[t[0],r[0],w[0],*,0],ytitle='Radiance (Wm!U-2!Nnm!U-1!Nsr!U-1!N)',/nodata,$
   xtitle='Wavelength (nm)',xr=[400,1750]
  for d=0, 2 do for i=0, nre-1 do oplot, wvl,spre[t[d],r[d],w[d],*,i],color=cl[d],linestyle=i*2
  legend, ['A - !9t!X=40, r!De!N=7 !9m!Xm, liquid','B - !9t!X=20, r!De!N=50 !9m!Xm, ice','C - !9t!X=5, r!De!N=25 !9m!Xm, ice'],$
   box=0, charsize=1.5, textcolors=[250,150,50],/right

  plot,  wvl,(spre[t[0],r[0],w[0],*,0]-ss[*,0])/max(ss[*,0])*100.,ytitle='Radiance difference (%)',$
    /nodata,xtitle='Wavelength (nm)',yr=[-5,2]
  for d=0, 2 do for i=0, nre-1 do oplot, wvl,(spre[t[d],r[d],w[d],*,i]-ss[*,d])/max(ss[*,d])*100.,color=cl[d],linestyle=i*2
  oplot, wvl,wvl*0.,linestyle=1,color=100

device, /close
spawn, 'convert '+fp+'.ps '+fp+'.png'
endif




fp=dir+'plots\p2\par_var2'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=33, ysize=25
  !p.font=1 & !p.thick=8 & !p.charsize=3.8 & !x.style=1 & !y.style=0 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,3,3,0,0] & !x.margin=[6,0.4] & !y.margin=[0,0.5] & !y.omargin=[6,1] & !x.omargin=[1,3]

  tvlct,0,150,0,150
  tvlct,200,200,200,100
  tvlct,230,230,230,221

  t=[39,19,4]
  r=[6,49,24]
  w=[0,1,1]
  cl=[250,150,50]  

  ; get the percent differences of each parameter evaluated at the 3 cases
  ; as a function of changes to cloud base height, surface albedo, and precipitable water
  dparz=fltarr(nz,3,15)
  dpara=fltarr(na,3,15)
  dparw=fltarr(nw,3,15)
  pp=fltarr(3,15)

  pw=[5.,10.,15.,20.,25.,30.]
  z=[0.5,1.5,3.0,5.0,7.0,9.0]

  for p=0, 14 do begin
    for i=0, 2 do begin
      pp[i,p]=parsa[t[i],r[i],w[i],p,0]
      for j=0, nz-1 do dparz[j,i,p]=abs(parsz[t[i],r[i],w[i],p,j]-pp[i,p])/(max(parsz[*,*,*,p,*],/nan)-min(parsz[*,*,*,p,*],/nan))*100.
      for j=0, na-1 do dpara[j,i,p]=abs(parsa[t[i],r[i],w[i],p,j]-pp[i,p])/(max(parsa[*,*,*,p,*],/nan)-min(parsa[*,*,*,p,*],/nan))*100.
      for j=0, nw-1 do dparw[j,i,p]=abs(parsw[t[i],r[i],w[i],p,j]-pp[i,p])/(max(parsw[*,*,*,p,*],/nan)-min(parsw[*,*,*,p,*],/nan))*100.
    endfor
  endfor  
  wti=['2012-05-24','2012-08-04','2012-08-12','2012-08-20','2012-09-13']
  xtn=[' ',' ',' ',' ',' ',' ',' ',' '] & xtf=xtn
  cl=fix(findgen(15)/14.*250.)
  leg=['A - !9t!X=40, r!De!N=7 !9m!Xm, liquid','B - !9t!X=20, r!De!N=50 !9m!Xm, ice','C - !9t!X=5, r!De!N=25 !9m!Xm, ice']
  xtit=' '

  for n=0, 2 do begin
    xtz=xtit & xta=xtit & xtw=xtit
    if n eq 2 then begin 
      xtf=['','']
      xtz='Cloud base height (km)'
      xta='!C!CSurface albedo'
      xtw='Precipitable water (mm)' 
    endif else wtit=xtn
    plot,z, dparz[*,n,0],ytitle='Spectral parameter!CVariation (%)',yr=[0,10],/nodata,xtickname=xtf,ticklen=0.08,xtitle=xtz
    for p=0,14 do begin
      oplot, z,dparz[*,n,p],color=cl[p],psym=-7 
    if p eq 8 or p eq 9 then $
       xyouts,z[nz-2]-p*0.15,dparz[nz-2,n,p]+0.5,'!9h!X!D'+string(p+1,format='(I2)')+'!N',color=cl[p],charsize=2.0
    endfor
    cc=[250,150,50]
    legend,leg[n],charsize=1.8,box=0,position=[0.5,10],textcolors=cc[n]
    polyfill, [z,reverse(z)],[0.*z,2.+0.*z],color=221,spacing=0.14,orientation=45
    oplot,z,z*0.+2.,color=100,linestyle=1
 
    plot, dpara[*,n,0],ytitle='Variation (%)',yr=[0,6],/nodata,xtickname=wtit,ticklen=0.08,xtitle=xta,xminor=1
    if n eq 2 then for o=0,na-1 do xyouts,o-0.2,-0.5,wti[o],alignment=0,orientation=-45,charsize=1.8
    for p=0,14 do begin
      oplot, dpara[*,n,p],color=cl[p],psym=-7
    if p eq 13 or p eq 11 or p eq 6 or p eq 10 then $
      xyouts,na-2-p*.08,dpara[na-2,n,p]+0.8,'!9h!X!D'+string(p+1,format='(I2)')+'!N',color=cl[p],charsize=2.0 
    endfor
    dd=indgen(5)
    polyfill, [dd,reverse(dd)],[0.*dd,2.+0.*dd],color=221,spacing=0.14,orientation=45
    oplot,dd,dd*0.+2.,color=100,linestyle=1  

    plot,pw, dparw[*,n,0],ytitle='Variation (%)',yr=[0,20],/nodata,xtickname=xtf,ticklen=0.08,xtitle=xtw
    for p=0,14 do begin
      oplot, pw,dparw[*,n,p],color=cl[p],psym=-7
     if p eq 8 or p eq 9 or p eq 0 or p eq 1 then $
       xyouts,pw[nw-2]-p*0.3,dparw[nw-2,n,p]+0.9,'!9h!X!D'+string(p+1,format='(I2)')+'!N',color=cl[p],charsize=2.0 
    endfor
    polyfill,[pw,reverse(pw)],[0.*pw,2.+0.*pw],color=221,spacing=0.14,orientation=45
    oplot, pw,pw*0.+2.,color=100,linestyle=1
  endfor
  legend,'!9h!X!D'+string(indgen(15)+1,format='(I2)')+'!N',textcolors=cl,box=0,position=[28,53], charsize=2.8

device, /close
spawn, 'convert '+fp+'.ps '+fp+'.png'




stop
end

