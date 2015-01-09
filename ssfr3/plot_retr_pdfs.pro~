; program to plot a contour of the most important parameter for each regime of tau ref and phase
; uses the results of the retrieve_pdfs_model.out made by retrieve_pdfs in model mode
; parses the 720 points into an array of tau, ref, and phase for each parameter

@legend.pro
pro plot_retr_pdfs
;restore the save file

fn='C:\Users\Samuel\Research\SSFR3\data\retrieved_pdf_20120523_model_limp.out'
restore, fn
hml=hm_rtm


fn='/home/leblanc/SSFR3/data/retrieved_pdf_20120602_model.out'
fn='C:\Users\Samuel\Research\SSFR3\data\retrieved_pdf_20120602_model.out'
print, 'restoring the file: '+fn
restore, fn

fn='C:\Users\Samuel\Research\SSFR3\data\model_taus_ref.out'
print, 'restoring modeled out file: '+fn
  restore, fn

; now make the arrays
wp=[0.,1.]
nt=n_elements(tau)
nr=n_elements(ref)
nl=n_elements(wvl)
nw=n_elements(wp)
Hi=fltarr(nt,nr,nw,16)
Hm=Hi
H=fltarr(nt,nr,nw)
maxhi=intarr(nt,nr,nw)
maxhm=intarr(nt,nr,nw)
hi_m=fltarr(nt,nr,nw)
hm_m=fltarr(nt,nr,nw)
ind=intarr(nt,nr,nw)
hi_m2=hi_m
maxhi2=maxhi

j=0
rbin=fltarr(nr+1)
tbin=fltarr(nt+1)
for t=1,nt-1 do tbin[t]=(tau[t-1]+tau[t])/2.
tbin[t]=tau[t-1]
tbin[0]=tau[0]
for r=1,nr-1 do rbin[r]=(ref[r-1]+ref[r])/2.
rbin[r]=ref[r-1]
rbin[0]=ref[0]
for w=0, nw-1 do begin
  for t=0, nt-1 do begin
    for r=0, nr-1 do begin
      Hi[t,r,w,*]=Hi_rtm[*,j]
      Hm[t,r,w,*]=hm_rtm[*,j]
      H[t,r,w]=H_rtm[j]
      hi_m[t,r,w]=max(Hi[t,r,w,*],z,/nan)
      maxhi[t,r,w]=z
      hm_m[t,r,w]=max(Hm[t,r,w,*],k,/nan)
      maxhm[t,r,w]=k
      ind[t,r,w]=j
      thi=Hi[t,r,w,*]
      thi[z]=0.0
      hi_m2[t,r,w]=max(thi,z2,/nan)
      maxhi2[t,r,w]=z2
      j=j+1
    endfor
  endfor
endfor
;stop
if 1 then begin
hi_stat=fltarr(16,7) ;mean, std, min, max ,25th perentile, 75th percentile, median, for independent
hm_stat=fltarr(16,7) ;for updated prior
hc_stat=fltarr(16,7) ;for updated prior, cumulative
for p=0, 14 do begin
  ;mean
  hi_stat[p,0]=mean(hi_rtm[p,*],/nan) & hm_stat[p,0]=mean(hm_rtm[p,*],/nan) 
  hc_stat[p,0]=mean(total(hm_rtm[0:p,*],1),/nan)
  ;stddev
  hi_stat[p,1]=stddev(hi_rtm[p,*],/nan) & hm_stat[p,1]=stddev(hm_rtm[p,*],/nan) 
  hc_stat[p,1]=stddev(total(hm_rtm[0:p,*],1),/nan)
  ;min
  hi_stat[p,2]=min(hi_rtm[p,*],/nan) & hm_stat[p,2]=min(hm_rtm[p,*],/nan) 
  hc_stat[p,2]=min(total(hm_rtm[0:p,*],1),/nan)
  ;max
  hi_stat[p,3]=max(hi_rtm[p,*],/nan) & hm_stat[p,3]=max(hm_rtm[p,*],/nan) 
  hc_stat[p,3]=max(total(hm_rtm[0:p,*],1),/nan)
  ;25th and 75th percentile
  hhi=hi_rtm[p,sort(hi_rtm[p,*])]
  inhi=n_elements(hhi)/2
  hi_stat[p,4]=median(hhi[0:inhi-1],/even)
  hi_stat[p,5]=median(hhi[inhi:*],/even)
  hhm=hm_rtm[p,sort(hm_rtm[p,*])]
  inhm=n_elements(hhm)/2
  hm_stat[p,4]=median(hhm[0:inhm-1],/even)
  hm_stat[p,5]=median(hhm[inhm:*],/even)
  hc_rtm=total(hm_rtm[0:p,*],1)
  hhc=hc_rtm[sort(hc_rtm)]
  inhc=n_elements(hhc)/2 
  hc_stat[p,4]=median(hhc[0:inhc-1],/even)
  hc_stat[p,5]=median(hhc[inhc:*],/even)
  ;median
  hi_stat[p,6]=median(hi_rtm[p,*])
  hm_stat[p,6]=median(hm_rtm[p,*])
  hc_stat[p,6]=median(hc_rtm[*])
endfor
dir='C:\Users\Samuel\Research\SSFR3\plots\'

if 0 then begin
  fp=dir+'IC_stats'
  print, 'making plot :'+fp
  set_plot, 'ps'
  loadct, 39, /silent
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
  device, xsize=43, ysize=20
   !p.font=1 & !p.thick=5 & !p.charsize=2.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
   !p.multi=0 & !x.margin=[6,12] & !y.margin=[3,3]
 
tvlct,225,90,90,253
tvlct,90,225,90,133
tvlct,90,90,225,40
usersym, [ -1, 1, 1, -1, -1 ,1], [ 1, 1, -1, -1, 1 ,1], /fill
  plot, findgen(15)+1, hi_stat[*,0],psym=2, title='IC dispersion for all calculated values',xtitle='Parameter',$
   ytitle='Information content',xticks=14,xtickformat='(I2)',yrange=[-0.25,1],xrange=[1,15],/nodata,xstyle=3
  plots,[0.7,15.3],[0,0],thick=2,linestyle=2
  for p=0, 14 do begin
    plots, [p+0.8,p+0.8],[hi_stat[p,2],hi_stat[p,3]],thick=5,color=40
    plots, [p+0.8,p+0.8],[hi_stat[p,4],hi_stat[p,5]],thick=39,color=40
    plots, p+0.8,hi_stat[p,0],psym=1
    plots, p+0.8,hi_stat[p,6],psym=6
    plots, [p+1,p+1],[hm_stat[p,2],hm_stat[p,3]],thick=5 ,color=253
    plots, [p+1,p+1],[hm_stat[p,4],hm_stat[p,5]],thick=39,color=253
    plots, p+1,hm_stat[p,0],psym=1
    plots, p+1,hm_stat[p,6],psym=6
    plots, [p+1.2,p+1.2],[hc_stat[p,2],hc_stat[p,3]],thick=5 ,color=133
    plots, [p+1.2,p+1.2],[hc_stat[p,4],hc_stat[p,5]],thick=39,color=133
    plots, p+1.2,hc_stat[p,0],psym=1
    plots, p+1.2,hc_stat[p,6],psym=6
  endfor
  legend,['Independent IC','Updated prior IC','Cumulative IC','Mean','Median'],box=0,pspacing=1,psym=[8,8,8,1,6],$
   textcolors=[40,253,133,0,0],position=[15.4,0.6],color=[40,253,133,0,0],charsize=2.3,/fill
  device, /close
  spawn, 'convert "'+fp+'.ps" "'+fp+'.png"'
endif

if 0 then begin
    fp=dir+'IC_sample'
  ; for a cloud of tau 100, ref 15, wp 0
  t=17 & r=5 & w=0
  print, 'making plot :'+fp
  set_plot, 'ps'
  loadct, 39, /silent
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
  device, xsize=43, ysize=20
   !p.font=1 & !p.thick=5 & !p.charsize=2.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
   !p.multi=0 & !x.margin=[6,12] & !y.margin=[3,3]

tvlct,225,90,90,253
tvlct,90,225,90,133
tvlct,90,90,225,40
usersym, [ -1, 1, 1, -1, -1 ,1], [ 1, 1, -1, -1, 1 ,1], /fill
  plot, findgen(15)+1, hi[*],psym=2, xtitle='Parameter',$
   ytitle='Information content',xticks=14,xtickformat='(I2)',yrange=[-0.1,1],xrange=[1,15],/nodata,xstyle=3
  plots,[0.7,15.3],[0,0],thick=2,linestyle=2
  for p=0, 14 do begin
    plots, [p+0.8,p+0.8],[0.,hi[t,r,w,p]],thick=39,color=40
    plots, [p+1,p+1],[0.,hm[t,r,w,p]],thick=39,color=253
    plots, [p+1.2,p+1.2],[0.,total(hm[t,r,w,0:p])],thick=39,color=133
  endfor
  legend,['Independent IC','Updated prior IC','Cumulative IC'],box=0,pspacing=1,psym=[8,8,8],$
   textcolors=[40,253,133],position=[15.4,0.6],color=[40,253,133],charsize=2.3,/fill
  device, /close
  spawn, 'convert "'+fp+'.ps" "'+fp+'.png"'
stop
endif

if 1 then begin
    fp=dir+'IC_sample_v2'
  ; for a cloud of tau 100, ref 15, wp 0
  t=17 & r=5 & w=0 & t2=8 & w2=1 & r2=12
  print, 'making plot :'+fp
  set_plot, 'ps'
  loadct, 39, /silent
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
  device, xsize=43, ysize=30
   !p.font=1 & !p.thick=5 & !p.charsize=2.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
   !p.multi=[0,1,2] & !x.margin=[6,12] & !y.margin=[3,1]
   
tvlct,225,90,90,253
tvlct,90,225,90,133
tvlct,90,90,225,40
tvlct,100,100,100,254

usersym, [ -1, 1, 1, -1, -1 ,1], [ 1, 1, -1, -1, 1 ,1], /fill
  plot, findgen(15)+1, hi[*],psym=2, xtitle='Parameter',$
   ytitle='Independent IC',xticks=14,xtickformat='(I2)',yrange=[0.,0.7],xrange=[1,15],/nodata,xstyle=3
  ;plots,[0.7,15.3],[0,0],thick=2,linestyle=2
  for p=0, 14 do begin
    plots, [p+0.7,p+0.7],[0.,hi[t,r,w,p]],thick=32,color=40
    plots, [p+0.85,p+0.85],[0.,hi[t2,r,w,p]],thick=32,color=253
    plots, [p+1.0,p+1.0],[0.,hi[t,r,w2,p]],thick=32,color=133
    plots, [p+1.15,p+1.15],[0.,hi[t2,r,w2,p]],thick=32,color=210
    plots, [p+1.3,p+1.3],[0.,hi[t2,r2,w2,p]],thick=32,color=254
  endfor

  hmc=hm
  hcl=hml
  pi=[6,5,2,3,1,11,15]-1
  k=sort(pi)
  hcl[0,*]=hml[4,*]
  for p=1, 14 do hmc[*,*,*,p]=total(hm[*,*,*,0:p],4)
  for p=1, 6 do hcl[p,*]=total(hml[k[0:p],*],1)
  ps=findgen(15)+1

  plot, findgen(15)+1, hi[*],psym=2, xtitle='Parameter',$
   ytitle='Cumulative IC',xticks=14,xtickformat='(I2)',yrange=[0.,1.0],xrange=[1,15],/nodata,xstyle=3
  ;plots,[0.7,15.3],[0,0],thick=2,linestyle=2
  oplot, ps, hmc[t,r,w,*],color=40,psym=-2,thick=10
  oplot, ps, hmc[t2,r,w,*],color=253,psym=-2,thick=10
  oplot, ps, hmc[t,r,w2,*],color=133,psym=-2,thick=10
  oplot, ps, hmc[t2,r,w2,*],color=210,psym=-2,thick=10
  oplot, ps, hmc[t2,r2,w2,*],color=254,psym=-2,thick=10

  oplot, pi[k]+1, hcl[*,ind[t,r,w]],color=40,psym=-2,thick=5, linestyle=2
  oplot, pi[k]+1, hcl[*,ind[t2,r,w]],color=253,psym=-2,thick=5, linestyle=2
  oplot, pi[k]+1, hcl[*,ind[t,r,w2]],color=133,psym=-2,thick=5,linestyle=2
  oplot, pi[k]+1, hcl[*,ind[t2,r,w2]],color=210,psym=-2,thick=5,linestyle=2
  oplot, pi[k]+1, hcl[*,ind[t2,r2,w2]],color=254,psym=-2,thick=5,linestyle=2


 ; for p=0, 14 do begin 
 ;   plots, [p+0.7,p+0.7],[0.,total(hm[t,r,w,0:p])],thick=39,color=40
 ;   plots, [p+0.9,p+0.9],[0.,total(hm[t2,r,w,0:p])],thick=39,color=253
 ;   plots, [p+1.1,p+1.1],[0.,total(hm[t,r,w2,0:p])],thick=39,color=133
 ;   plots, [p+1.3,p+1.3],[0.,total(hm[t2,r,w2,0:p])],thick=39,color=210
 ; endfor 
  legend, ['!9t!X=100, r!Deff!N=15 !9m!Xm liquid','!9t!X=20, r!Deff!N=15 !9m!Xm liquid',$
           '!9t!X=100, r!Deff!N=15 !9m!Xm ice','!9t!X=20, r!Deff!N=15 !9m!Xm ice','!9t!X=20, r!Deff!N=40 !9m!Xm ice'],$
           textcolors=[40,253,133,210,254],box=0,charsize=2.5, /bottom,/right ;position=[15.4,1.6]
  legend,['All parameters','Parameter subset'],linestyle=[0,2],thick=[10,5],pspacing=1.8,box=0,/right,$
           position=[11,0.27],charsize=2.5
;  legend,['Independent IC','Updated prior IC','Cumulative IC'],box=0,pspacing=1,psym=[8,8,8],$
;   textcolors=[40,253,133],position=[15.4,0.6],color=[40,253,133],charsize=2.3,/fill
  device, /close
  spawn, 'convert "'+fp+'.ps" "'+fp+'.png"'
stop
endif



if 0 then begin
print, 'Writing to file'
openw,lun, dir+'IC_stats.dat',/get_lun
printf, lun, 'Independent H'
printf, lun, 'Mean, -Stddev, +Stddev,  min, max'
for p=0, 14 do printf, lun, hi_stat[p,0],hi_stat[p,0]-hi_stat[p,1],hi_stat[p,0]+hi_stat[p,1],hi_stat[p,2],hi_stat[p,3]
printf, lun, 'Updated prior H'
printf, lun, 'Mean, -Stddev, +Stddev,  min, max'
for p=0, 14 do printf, lun, hm_stat[p,0],hm_stat[p,0]-hm_stat[p,1],hm_stat[p,0]+hm_stat[p,1],hm_stat[p,2],hm_stat[p,3]
printf, lun, 'Cumulative H'
printf, lun, 'Mean, -Stddev, +Stddev,  min, max'
for p=0, 14 do printf, lun, hc_stat[p,0],hc_stat[p,0]-hc_stat[p,1],hc_stat[p,0]+hc_stat[p,1],hc_stat[p,2],hc_stat[p,3]
close, lun
free_lun, lun

for p=0, 15 do begin
  openw,lun,dir+'IC_par'+string(p,format='(I02)')+'.dat',/get_lun
  printf, lun, 'Hi, Hm, Hc'
  for i=0, 719 do printf, lun, hi_rtm[p,i],hm_rtm[p,i],total(hm_rtm[0:p,i])
  close, lun
  free_lun,lun 
endfor
endif

;stop
endif

; now run the plotting

if 0 then begin
set_plot, 'win'
device, decomposed=0
loadct, 39
!x.style=1 & !y.style=1
window, 0, retain=2
contour, maxhi[*,*,0],tau,ref,nlevels=16,/cell_fill,title='Liquid water cloud',xtitle='optical depth',ytitle='Effective radius',xrange=[0,max(tbin)],yrange=[0,max(rbin)]
for t=0,nt-1 do begin
  for r=0,nr-1 do begin
    ;plots,tau[t],ref[r],psym=2,symsize=2,color=255*maxhi[t,r,0]/15.
    polyfill,tbin[[t,t+1,t+1,t]],rbin[[r,r,r+1,r+1]],color=255*maxhi[t,r,0]/15.
  endfor
endfor
lvl1=[0.0,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1.0]
contour,hi_m[*,*,0],tau,ref,levels=lvl1,/overplot,/follow,c_thick=[2,2],c_charsize=1.5,c_labels=replicate(1,n_elements(lvl1))


window, 1, retain=2
contour, maxhi[*,*,1],tau, ref, nlevels=16,/cell_fill,title='Ice water cloud',xtitle='Optical depth',ytitle='Effective radius',xrange=[0,max(tbin)],yrange=[0,max(rbin)]
for t=0,nt-1 do begin
  for r=0,nr-1 do begin
    ;plots,tau[t],ref[r],psym=2,symsize=2,color=255*maxhi[t,r,1]/15.
    polyfill,tbin[[t,t+1,t+1,t]],rbin[[r,r,r+1,r+1]],color=255*maxhi[t,r,1]/15.
  endfor
endfor
contour,hi_m[*,*,1],tau,ref,levels=lvl1,/overplot,/follow,c_thick=[2,2],c_charsize=1.5,c_labels=replicate(1,n_elements(lvl1))
;stop
endif

dir='/home/leblanc/SSFR3/plots/'
dir='C:\Users\Samuel\Research\SSFR3\plots\'

if 0 then begin
fp=dir+'IC_regimes'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=40, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=2.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,2,1] & !x.margin=[6,6] & !y.margin=[3,3]
 
 ; mu=155B
  mus='!9m!X'
  contour, maxhi[*,*,0],tau,ref,nlevels=16,/cell_fill,title='Liquid water cloud',xtitle='Optical depth',$
   ytitle='Effective radius ('+mus+'m)',xrange=[0,max(tbin)],yrange=[0,max(rbin)]
for t=0,nt-1 do begin
  for r=0,nr-1 do begin
    ;plots,tau[t],ref[r],psym=2,symsize=2,color=255*maxhi[t,r,0]/15.
    polyfill,tbin[[t,t+1,t+1,t]],rbin[[r,r,r+1,r+1]],color=254*maxhi[t,r,0]/14.
  endfor
endfor
lvl1=[0.0,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1.0]
contour,hi_m[*,*,0],tau,ref,levels=lvl1,/overplot,/follow,c_thick=[2,2],c_charsize=2.5

contour, maxhi[*,*,1],tau, ref, nlevels=16,/cell_fill,title='Ice water cloud',xtitle='Optical depth',$
 ytitle='Effective radius ('+mus+'m)',xrange=[0,max(tbin)],yrange=[0,max(rbin)],xmargin=[3,9]
for t=0,nt-1 do begin
  for r=0,nr-1 do begin
    ;plots,tau[t],ref[r],psym=2,symsize=2,color=255*maxhi[t,r,1]/15.
    polyfill,tbin[[t,t+1,t+1,t]],rbin[[r,r,r+1,r+1]],color=254*maxhi[t,r,1]/14.
  endfor
endfor
contour,hi_m[*,*,1],tau,ref,levels=lvl1,/overplot,/follow,c_thick=[2,2],c_charsize=2.5

bar=findgen(29)/2.+0.5 ;[0.5,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]
lvl=[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]
 contour,transpose([[bar],[bar]]),[0,1],findgen(n_elements(bar)),levels=lvl,/cell_fill,ystyle=9,yticks=14,$
  ytickname=replicate(' ',15),position=[0.9,0.1,0.92,0.9],/normal,/noerase,xticks=1,xtickname=[' ',' ']
 axis,yaxis=1,ytitle='Parameter',yrange=[1,15],yticks=14,ytickformat='(I2)'

 device, /close
 spawn, 'convert "'+fp+'.ps" "'+fp+'.png"'
endif

if 0 then begin
  fp=dir+'IC_regimes_second'
  print, 'making plot :'+fp
  set_plot, 'ps'
  loadct, 39, /silent
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
  device, xsize=40, ysize=20
   !p.font=1 & !p.thick=5 & !p.charsize=2.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
   !p.multi=[0,2,1] & !x.margin=[6,6] & !y.margin=[3,3]

  ; mu=155B
  mus='!9m!X'
  contour, maxhi[*,*,0],tau,ref,nlevels=16,/cell_fill,title='Liquid water cloud',xtitle='Optical depth',$
   ytitle='Effective radius ('+mus+'m)',xrange=[0,max(tbin)],yrange=[0,max(rbin)]
  for t=0,nt-1 do begin
    for r=0,nr-1 do begin
      ;plots,tau[t],ref[r],psym=2,symsize=2,color=255*maxhi[t,r,0]/15.
      polyfill,tbin[[t,t+1,t+1,t]],rbin[[r,r,r+1,r+1]],color=254*maxhi2[t,r,0]/14.
    endfor
  endfor
  lvl1=[0.0,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1.0]
  contour,hi_m2[*,*,0],tau,ref,levels=lvl1,/overplot,/follow,c_thick=[2,2],c_charsize=2.5

  contour, maxhi[*,*,1],tau, ref, nlevels=16,/cell_fill,title='Ice water cloud',xtitle='Optical depth',$
   ytitle='Effective radius ('+mus+'m)',xrange=[0,max(tbin)],yrange=[0,max(rbin)],xmargin=[3,9]
  for t=0,nt-1 do begin
    for r=0,nr-1 do begin
      ;plots,tau[t],ref[r],psym=2,symsize=2,color=255*maxhi[t,r,1]/15.
      polyfill,tbin[[t,t+1,t+1,t]],rbin[[r,r,r+1,r+1]],color=254*maxhi2[t,r,1]/14.
    endfor
  endfor
  contour,hi_m2[*,*,1],tau,ref,levels=lvl1,/overplot,/follow,c_thick=[2,2],c_charsize=2.5

  bar=findgen(29)/2.+0.5 ;[0.5,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]
  lvl=[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]
  contour,transpose([[bar],[bar]]),[0,1],findgen(n_elements(bar)),levels=lvl,/cell_fill,ystyle=9,yticks=14,$
   ytickname=replicate(' ',15),position=[0.9,0.1,0.92,0.9],/normal,/noerase,xticks=1,xtickname=[' ',' ']
  axis,yaxis=1,ytitle='Parameter',yrange=[1,15],yticks=14,ytickformat='(I2)'

  device, /close
  spawn, 'convert "'+fp+'.ps" "'+fp+'.png"'
endif

if 1 then begin

 hl=fltarr(24,15,2)
 for t=0, 23 do for r=0,14 do for w=0,1 do hl[t,r,w]=hcl[6,ind[t,r,w]]

fp=dir+'IC_contour'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=40, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=2.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,2,1] & !x.margin=[6,6] & !y.margin=[3,3]
 
 ; mu=155B
  mus='!9m!X'
  lvls=findgen(30)/29.
  contour, hl[0:21,*,0],tau[0:21],ref,levels=lvls,/cell_fill,title='Liquid water cloud',xtitle='Optical depth',$
   ytitle='Effective radius ('+mus+'m)'
 
contour, hl[0:21,*,1],tau[0:21], ref, levels=lvls,/cell_fill,title='Ice water cloud',xtitle='Optical depth',$
 ytitle='Effective radius ('+mus+'m)',xmargin=[3,9]
 
 contour,transpose([[lvls],[lvls]]),[0,1],lvls,levels=lvls,/cell_fill,ystyle=9,yticks=10,$
  ytickname=replicate(' ',11),position=[0.9,0.1,0.92,0.9],/normal,/noerase,xticks=1,xtickname=[' ',' ']
 axis,yaxis=1,ytitle='IC',yrange=[0,1],yticks=10
 
 device, /close
 spawn, 'convert "'+fp+'.ps" "'+fp+'.png"'
endif


if 0 then begin
  fp=dir+'IC_histogram'
  print, 'making plot :'+fp
  set_plot, 'ps'
  loadct, 39, /silent
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
  device, xsize=40, ysize=20
   !p.font=1 & !p.thick=5 & !p.charsize=2.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
   !p.multi=[0,2,1] & !x.margin=[6,6] & !y.margin=[3,3]

  his=histogram(maxhi)
  his2=histogram(maxhi2)
  plot, findgen(15)+1., his/total(his)*100., psym=10,title='Occurence of parameters with!Cmost information content',$
   xtitle='Parameter',ytitle='Occurences (%)',xticks=14,xtickformat='(I2)'
  plot, findgen(15)+1., his2/total(his2)*100., psym=10,title='Occurence of parameters with!Csecond most information content',$
   xtitle='Parameter',ytitle='Occurences (%)',xticks=14,xtickformat='(I2)'

  device, /close
  spawn, 'convert "'+fp+'.ps" "'+fp+'.png"'
endif

tau_set=100.
ref_set=15.
wp_set=0
nul=min(abs(tau-tau_set),ti)
nul=min(abs(ref-ref_set),ri)
nul=min(abs(wp-wp_set),wi)

if 0 then begin
refo=ref
;restore, dir+'..\data\pars_lut_taus_ref.out'
restore, 'C:\Users\Samuel\Research\SSFR3\data\pars_lut_taus_ref.out'
refs=ref
ref=refo

fp=dir+'Post_model_pdf'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=40, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=2.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,2,1] & !x.margin=[6,6] & !y.margin=[3,3]

  mus='!9m!X'
  ;probs=findgen(41)/40.
  ;probs=(findgen(21)^3.)/(20.^3.)
  probs=(findgen(21)^3.)/(20^3.)*max(post[ind[ti,ri,wi],*,*,0])
  clrs=findgen(n_elements(probs))*254./(n_elements(probs)-1)
  contour, post[ind[ti,ri,wi],*,*,0],taus,refs,levels=probs,title='Liquid water cloud',xtitle='Optical depth',$
   ytitle='Effective radius ('+mus+'m)',xrange=[0,max(taus)],yrange=[0,max(refs)],c_colors=clrs,/cell_fill
  
  if wp_rtm[ind[ti,ri,wi]] eq 0 then plots, tau_rtm[ind[ti,ri,wi]],ref_rtm[ind[ti,ri,wi]],psym=1, color=255
  if wp_set eq 0 then plots, tau_set,ref_set,psym=4,color=254

  contour, post[ind[ti,ri,wi],*,*,1],taus, refs, levels=probs,title='Ice water cloud',xtitle='Optical depth',$
   ytitle='Effective radius ('+mus+'m)',xrange=[0,max(taus)],yrange=[0,max(refs)],xmargin=[3,9],c_colors=clrs,/cell_fill
;  if wp_rtm[ind[ti,ri,wi]] eq 1 then plots, tau_rtm[ind[ti,ri,wi]],ref_rtm[ind[ti,ri,wi]],psym=2
;  if wp_set eq 1 then plots, tau_set,ref_set,psym=4


bar=findgen(29)/2.+0.5 ;[0.5,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]
lvl=[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]
 contour,transpose([[probs],[probs]]),[0,1],findgen(n_elements(probs)),levels=probs,/cell_fill,ystyle=9,yticks=14,$
  ytickname=replicate(' ',15),position=[0.88,0.1,0.9,0.9],/normal,/noerase,xticks=1,xtickname=[' ',' ']
 axis,yaxis=1,ytitle='Probability',yrange=[0,1],yticks=10,ytickname=string(probs[0:*:(n_elements(probs)-1)/10],FORMAT='(F5.3)')
 ;,ytickformat='(F4.2)'

 device, /close
 spawn, 'convert "'+fp+'.ps" "'+fp+'.png"'
endif

if 0 then begin
refo=ref
;restore, dir+'..\data\pars_lut_taus_ref.out'
restore, 'C:\Users\Samuel\Research\SSFR3\data\pars_lut_taus_ref.out'
refs=ref
ref=refo

fp=dir+'Post_model_marginal_pdf'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=40, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=2.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,2,1] & !x.margin=[6,4] & !y.margin=[3.5,1]

  mus='!9m!X'
  probs=findgen(41)/40.
  p=post[ind[ti,ri,wi],*,*,*]
  pref=fltarr(n_elements(refs))
  for r=0, n_elements(refs)-1 do  pref[r]=int_tabulated(taus,p[0,*,r,0])+int_tabulated(taus,p[0,*,r,1])
  refns=where(pref ge max(pref)/100.*25.)
  ref_err=[min(refs[refns]),max(refs[refns])]
  gfitr=gaussfit(refs,pref,coeffr)
  plot, refs,pref,xtitle='Effective radius ('+mus+'m)', ytitle='Probability density'
  tvlct,190,190,190,151
  polyfill,[ref_err[0],ref_err[1],ref_err[1],ref_err[0]],[0,0,max(pref),max(pref)],color=151
  oplot,refs,gfitr,color=250
  plots, [coeffr[1]+coeffr[2],coeffr[1]+coeffr[2]],[0,max(pref)],color=250,linestyle=2
  plots, [coeffr[1]-coeffr[2],coeffr[1]-coeffr[2]],[0,max(pref)],color=250,linestyle=2
  oplot,refs,pref  
  legend,['Uncertainty range','Fitted gaussian','Standard deviation'],color=[151,250,250],linestyle=[0,0,2],$
   thick=[30,5,5],pspacing=1.6,/right,box=0,charsize=2.0


  ptau=fltarr(n_elements(taus))
  for t=0, n_elements(taus)-1 do ptau[t]=int_tabulated(refs,p[0,t,*,0])+int_tabulated(refs,p[0,t,*,1])
  tauns=where(ptau ge max(ptau)/100.*25.)
  tau_err=[min(taus[tauns]),max(taus[tauns])]
  plot, taus, ptau, xtitle='Optical depth',ytitle='Probability density',xrange=[70,130]
  polyfill,[tau_err[0],tau_err[1],tau_err[1],tau_err[0]],[0,0,max(ptau),max(ptau)],color=151
  gfitt=gaussfit(taus,ptau,coefft)
  oplot,taus,gfitt,color=250
  plots, [coefft[1]+coefft[2],coefft[1]+coefft[2]],[0,max(ptau)],color=250,linestyle=2
  plots, [coefft[1]-coefft[2],coefft[1]-coefft[2]],[0,max(ptau)],color=250,linestyle=2
  oplot,taus,ptau 
 ;   legend,['Uncertainty range','Fitted gaussian','Standard deviation'],color=[151,250,250],linestyle=[0,0,2],$
 ;  thick=[30,5,5],pspacing=1.6,/right,box=0,charsize=1.6

 device, /close
 spawn, 'convert "'+fp+'.ps" "'+fp+'.png"'
endif

if 0 then begin
fp=dir+'model_spectra_ex'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=20, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=2.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=0 & !x.margin=[6,4] & !y.margin=[3.5,1]

  plot, wvl,spectram[ind[ti,ri,wi],*]/max(spectram[ind[ti,ri,wi],*]),xtitle='Wavelength (nm)', ytitle='Normalized Radiance'
 device, /close
 spawn, 'convert "'+fp+'.ps" "'+fp+'.png"'
endif



stop
end

