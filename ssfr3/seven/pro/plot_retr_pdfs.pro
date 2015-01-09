; program to plot a contour of the most important parameter for each regime of tau ref and phase
; uses the results of the retrieve_pdfs_model.out made by retrieve_pdfs in model mode
; parses the 720 points into an array of tau, ref, and phase for each parameter

@legend.pro
pro plot_retr_pdfs
;restore the save file
win=0
if win then fn='C:\Users\Samuel\Research\SSFR3\data\retrieved_pdf_20120523_model_limp.out' else $
fn='/argus/roof/SSFR3/retrieved/retrieved_pdf_20120523_model_limp2_v4.out'
;fn='/home/leblanc/SSFR3/data/retrieved_pdf_20120523_model_limp_v2.out'
restore, fn
hml=hm_rtm
rer=ref_err
tar=tau_err
ret=ref_rtm
tat=tau_rtm
htt=h_rtm

if not win then fn='/argus/roof/SSFR3/retrieved/retrieved_pdf_20120523_model_v4.out' else $
fn='C:\Users\Samuel\Research\SSFR3\data\retrieved_pdf_20120602_model.out'
print, 'restoring the file: '+fn
restore, fn
hll=hm_rtm

if win then fn='C:\Users\Samuel\Research\SSFR3\data\model_taus_ref.out' else $
fn='/home/leblanc/SSFR3/data/model_taus_ref.out'
print, 'restoring modeled out file: '+fn
  restore, fn

; now make the arrays
wp=[0.,1.]
nt=n_elements(tau)
nr=n_elements(ref)
nl=n_elements(wvl)
nw=n_elements(wp)
Hi=fltarr(nt,nr,nw,15)
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
if win then dir='C:\Users\Samuel\Research\SSFR3\plots\' else $
dir='/home/leblanc/SSFR3/plots/'

if 0 then begin
  fp=dir+'IC_stats_v4'
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
    fp=dir+'IC_sample_v41'
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
    fp=dir+'IC_sample_v42'
  ; for a cloud of tau 100, ref 15, wp 0
  t=17 & r=5 & w=0 & t2=8 & w2=1 & r2=12 & t3=3
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
tvlct,240,115,235,235 ;purple

usersym, [ -1, 1, 1, -1, -1 ,1], [ 1, 1, -1, -1, 1 ,1], /fill
  plot, findgen(15)+1, hi[*],psym=2, xtitle='Parameter',$
   ytitle='Independent IC',xticks=14,xtickformat='(I2)',yrange=[0.,0.7],xrange=[1,15],/nodata,xstyle=3
  ;plots,[0.7,15.3],[0,0],thick=2,linestyle=2
  for p=0, 14 do begin
    plots, [p+0.7,p+0.7],[0.,hi[t,r,w,p]],thick=28,color=40
    plots, [p+0.82,p+0.82],[0.,hi[t2,r,w,p]],thick=28,color=253
    plots, [p+0.95,p+0.95],[0.,hi[t,r,w2,p]],thick=28,color=133
    plots, [p+1.08,p+1.08],[0.,hi[t2,r,w2,p]],thick=28,color=210
    plots, [p+1.20,p+1.20],[0.,hi[t2,r2,w2,p]],thick=28,color=254
    plots, [p+1.31,p+1.31],[0.,hi[t3,r2,w2,p]],thick=28,color=235
  endfor

  hmc=hm
  hcl=hml
;  pi=[6,5,2,3,1,11,15]-1
;  pi=[6,5,2,3,11,15]-1
;  k=sort(pi)
pi=[6,5,2,3,11,12,14,7]-1
pi=pi[sort(pi)]
k=indgen(n_elements(pi))

  hcl[0,*]=hml[k[0],*]
  for p=1, 14 do hmc[*,*,*,p]=total(hm[*,*,*,0:p],4)
  for p=1, n_elements(pi)-1 do hcl[p,*]=total(hml[k[0:p],*],1)
  ps=findgen(15)+1
  plot, findgen(15)+1, hi[*],psym=2, xtitle='Parameter',$
   ytitle='Cumulative IC',xticks=14,xtickformat='(I2)',yrange=[0.,1.0],xrange=[1,15],/nodata,xstyle=3
  ;plots,[0.7,15.3],[0,0],thick=2,linestyle=2
  oplot, ps, hmc[t,r,w,*],color=40,psym=-2,thick=10
  oplot, ps, hmc[t2,r,w,*],color=253,psym=-2,thick=10
  oplot, ps, hmc[t,r,w2,*],color=133,psym=-2,thick=10
  oplot, ps, hmc[t2,r,w2,*],color=210,psym=-2,thick=10
  oplot, ps, hmc[t2,r2,w2,*],color=254,psym=-2,thick=10
  oplot, ps, hmc[t3,r2,w2,*],color=235,psym=-2,thick=10

  oplot, pi[k]+1, hcl[*,ind[t,r,w]],color=40,psym=-2,thick=5, linestyle=2
  oplot, pi[k]+1, hcl[*,ind[t2,r,w]],color=253,psym=-2,thick=5, linestyle=2
  oplot, pi[k]+1, hcl[*,ind[t,r,w2]],color=133,psym=-2,thick=5,linestyle=2
  oplot, pi[k]+1, hcl[*,ind[t2,r,w2]],color=210,psym=-2,thick=5,linestyle=2
  oplot, pi[k]+1, hcl[*,ind[t2,r2,w2]],color=254,psym=-2,thick=5,linestyle=2
  oplot, pi[k]+1, hcl[*,ind[t3,r2,w2]],color=235,psym=-2,thick=5,linestyle=2

 ; for p=0, 14 do begin 
 ;   plots, [p+0.7,p+0.7],[0.,total(hm[t,r,w,0:p])],thick=39,color=40
 ;   plots, [p+0.9,p+0.9],[0.,total(hm[t2,r,w,0:p])],thick=39,color=253
 ;   plots, [p+1.1,p+1.1],[0.,total(hm[t,r,w2,0:p])],thick=39,color=133
 ;   plots, [p+1.3,p+1.3],[0.,total(hm[t2,r,w2,0:p])],thick=39,color=210
 ; endfor 
  legend, ['!9t!X=100, r!Deff!N=15 !9m!Xm liquid','!9t!X=20, r!Deff!N=15 !9m!Xm liquid',$
           '!9t!X=100, r!Deff!N=15 !9m!Xm ice','!9t!X=20, r!Deff!N=15 !9m!Xm ice',$
           '!9t!X=20, r!Deff!N=40 !9m!Xm ice','!9t!X=3, r!Deff!N=40 !9m!Xm ice'],$
           textcolors=[40,253,133,210,254,235],box=0,charsize=2.4, /bottom,/right ;position=[15.4,1.6]
  legend,['All parameters','Parameter subset'],linestyle=[0,2],thick=[10,5],pspacing=1.8,box=0,/right,$
           position=[11,0.27],charsize=2.5
;  legend,['Independent IC','Updated prior IC','Cumulative IC'],box=0,pspacing=1,psym=[8,8,8],$
;   textcolors=[40,253,133],position=[15.4,0.6],color=[40,253,133],charsize=2.3,/fill
  device, /close
  spawn, 'convert "'+fp+'.ps" "'+fp+'.png"'
stop
endif

if 0 then begin
    fp=dir+'sample_sp_v4'
  ; for a cloud of tau 100, ref 15, wp 0
  t=17 & r=5 & w=0 & t2=8 & w2=1 & r2=12 & t3=1
  print, 'making plot :'+fp
  set_plot, 'ps'
  loadct, 39, /silent
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
  device, xsize=40, ysize=20
   !p.font=1 & !p.thick=5 & !p.charsize=2.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
   !p.multi=[0,2,1] & !x.margin=[6,3] & !y.margin=[3,1]

  tvlct,225,90,90,253
  tvlct,90,225,90,133
  tvlct,90,90,225,40
  tvlct,100,100,100,254

  ; now restore the model spectra
  fn='/argus/roof/SSFR3/model/sp_hires4_20120524.out'
  print, 'restoring : '+fn
  restore, fn
  restore, '/home/leblanc/SSFR3/data/sp_clear.out'
  wvl=zenlambda
  plot, wvl, sp[t,r,*,w],/nodata, yr=[0,0.40],ytitle='Radiance (W/m!U2!N nm sr)',xtit='Wavelength (nm)'
  oplot,wvl, sp[t,r,*,w],color=40
  oplot,wvl, sp[t2,r,*,w],color=253
  oplot,wvl, sp[t,r,*,w2],color=133
  oplot,wvl, sp[t2,r,*,w2],color=210
  oplot,wvl, sp[t2,r2,*,w2],color=254
  oplot,wvl, rad,color=100
  
  legend, ['!9t!X=100, r!Deff!N=15 !9m!Xm liquid','!9t!X=20, r!Deff!N=15 !9m!Xm liquid',$
           '!9t!X=100, r!Deff!N=15 !9m!Xm ice','!9t!X=20, r!Deff!N=15 !9m!Xm ice',$
           '!9t!X=20, r!Deff!N=40 !9m!Xm ice','clear'],$;,'!9t!X=2, r!Deff!N=40 !9m!Xm ice','clear'],$
           textcolors=[40,253,133,210,254,100],box=0,charsize=2.5, /right

  plot, wvl, sp[t,r,*,w],/nodata, yr=[0,1.0],ytitle='Normalized radiance',xtit='Wavelength (nm)'
  oplot,wvl, sp[t,r,*,w]/max(sp[t,r,*,w]),color=40
  oplot,wvl, sp[t2,r,*,w]/max(sp[t2,r,*,w]),color=253
  oplot,wvl, sp[t,r,*,w2]/max(sp[t,r,*,w2]),color=133
  oplot,wvl, sp[t2,r,*,w2]/max(sp[t2,r,*,w2]),color=210
  oplot,wvl, sp[t2,r2,*,w2]/max(sp[t2,r2,*,w2]),color=254
  oplot,wvl, rad/max(rad),color=100

  legend, ['!9t!X=100, r!Deff!N=15 !9m!Xm liquid','!9t!X=20, r!Deff!N=15 !9m!Xm liquid',$
           '!9t!X=100, r!Deff!N=15 !9m!Xm ice','!9t!X=20, r!Deff!N=15 !9m!Xm ice',$
           '!9t!X=20, r!Deff!N=40 !9m!Xm ice','clear'],$ ;,'!9t!X=2, r!Deff!N=40 !9m!Xm ice','clear'],$
           textcolors=[40,253,133,210,254,100],box=0,charsize=2.5, /right

  device, /close
  spawn, 'convert "'+fp+'.ps" "'+fp+'.png"'
stop
endif

if 1 then begin
  fp=dir+'IC_sectors_v4'
  print, 'making plot :'+fp
  set_plot, 'ps'
  loadct, 39, /silent
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
  device, xsize=40, ysize=25
   !p.font=1 & !p.thick=5 & !p.charsize=4.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
   !p.multi=[0,2,3] & !x.margin=[6,1] & !y.margin=[3,1] & !x.omargin=[0,0]

  hcl=hml
  ;pi=[6,5,2,3,1,11,15]-1
  ;pi=[6,5,2,3,11,15]-1
  ;k=sort(pi)
  pi=[6,5,2,3,11,12,14,7]-1
  pi=pi[sort(pi)]
  k=indgen(n_elements(pi))
  
  hcl[0,*]=hml[k[0],*]
  for p=1, n_elements(pi)-1 do hcl[p,*]=total(hml[k[0:p],*],1)
  h_mean=fltarr(n_elements(pi)+1,3,2)
  h_rg=fltarr(n_elements(pi)+1,3,2,2)
  for p=0,n_elements(pi)-1 do begin
    h_mean[p,0,0]=mean(hcl[p,ind[0:9,*,0]],/nan)  ;0:12 for up to 50
    h_mean[p,1,0]=mean(hcl[p,ind[10:17,0:9,0]],/nan) ;13:21 for 50 to 200
    h_mean[p,2,0]=mean(hcl[p,ind[10:17,10:*,0]],/nan)
    h_mean[p,0,1]=mean(hcl[p,ind[0:9,*,1]],/nan)
    h_mean[p,1,1]=mean(hcl[p,ind[10:17,0:9,1]],/nan)
    h_mean[p,2,1]=mean(hcl[p,ind[10:17,10:*,1]],/nan)
    
    h_rg[p,0,0,0]=min(hcl[p,ind[0:9,*,0]],/nan)
    h_rg[p,1,0,0]=min(hcl[p,ind[10:17,0:9,0]],/nan)
    h_rg[p,2,0,0]=min(hcl[p,ind[10:17,10:*,0]],/nan)
    h_rg[p,0,1,0]=min(hcl[p,ind[0:9,*,1]],/nan)
    h_rg[p,1,1,0]=min(hcl[p,ind[10:17,0:9,1]],/nan)
    h_rg[p,2,1,0]=min(hcl[p,ind[10:17,10:*,1]],/nan)
    
    h_rg[p,0,0,1]=max(hcl[p,ind[0:9,*,0]],/nan)
    h_rg[p,1,0,1]=max(hcl[p,ind[10:17,0:9,0]],/nan)
    h_rg[p,2,0,1]=max(hcl[p,ind[10:17,10:*,0]],/nan)
    h_rg[p,0,1,1]=max(hcl[p,ind[0:9,*,1]],/nan)
    h_rg[p,1,1,1]=max(hcl[p,ind[10:17,0:9,1]],/nan)
    h_rg[p,2,1,1]=max(hcl[p,ind[10:17,10:*,1]],/nan)
  endfor

  bins=pi[k]+1.
  u=2.8
  plot, bins,h_mean[*,0,0] , psym=-5, xtitle='Parameter',ytitle='Cumulative IC',xticks=14,xtickformat='(I2)',yr=[0,1],xr=[1,15]
  errplot, bins, h_rg[*,0,0,0],h_rg[*,0,0,1]
  xyouts, 14,0.2,'A - Liquid',charsize=u,alignment=1. 
  plot, bins,h_mean[*,0,1] , psym=-5, xtitle='Parameter',ytitle='Cumulative IC',xticks=14,xtickformat='(I2)',yr=[0,1],xr=[1,15]
  errplot, bins, h_rg[*,0,1,0],h_rg[*,0,1,1]
  xyouts, 14,0.2,'A - Ice',charsize=u,alignment=1.
  
  plot, bins,h_mean[*,1,0] , psym=-5, xtitle='Parameter',ytitle='Cumulative IC',xticks=14,xtickformat='(I2)',yr=[0,1],xr=[1,15]
  errplot, bins, h_rg[*,1,0,0],h_rg[*,1,0,1]
  xyouts, 14,0.2,'B - Liquid',charsize=u,alignment=1.
  plot, bins,h_mean[*,1,1] , psym=-5, xtitle='Parameter',ytitle='Cumulative IC',xticks=14,xtickformat='(I2)',yr=[0,1],xr=[1,15]
  errplot, bins, h_rg[*,1,1,0],h_rg[*,1,1,1]
  xyouts, 14,0.2,'B - Ice',charsize=u,alignment=1.

  plot, bins,h_mean[*,2,0] , psym=-5, xtitle='Parameter',ytitle='Cumulative IC',xticks=14,xtickformat='(I2)',yr=[0,1],xr=[1,15]
  errplot, bins, h_rg[*,2,0,0],h_rg[*,2,0,1]
  xyouts, 14,0.2,'C - Liquid',charsize=u,alignment=1.
  plot, bins,h_mean[*,2,1] , psym=-5, xtitle='Parameter',ytitle='Cumulative IC',xticks=14,xtickformat='(I2)',yr=[0,1],xr=[1,15]
  errplot, bins, h_rg[*,2,1,0],h_rg[*,2,1,1]
  xyouts, 14,0.2,'C - Ice',charsize=u,alignment=1.

  device, /close
  spawn, 'convert "'+fp+'.ps" "'+fp+'.png"'
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

if not win then dir='/home/leblanc/SSFR3/plots/' else $
dir='C:\Users\Samuel\Research\SSFR3\plots\'

if 0 then begin
fp=dir+'IC_regimes_v4'
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
  fp=dir+'IC_regimes_second_v4'
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

if 1 then begin ; begin plotting of information content contour plots

if 0 then begin
hcl=hll
  for p=1, n_elements(hll[*,0])-1 do hcl[p,*]=total(hll[0:p,*],1)
  tx='_all'
endif else tx=''

 hl=fltarr(24,15,2) 
;for t=0, 23 do for r=0,14 do for w=0,1 do hl[t,r,w]=htt[ind[t,r,w]] ;hcl[n_elements(hcl[*,0])-1,ind[t,r,w]]
 hl=htt[ind]
 hlg=hl
 ;hlg[21,12,1]=0.912671 & hlg[21,13,1]=0.934645 & hlg[21,14,1]=0.9566   & hlg[20,14,1]=0.944 ;result of interpolation for the points where there are nans
 ;hlg[21,10,0]=0.732623 & hlg[19,11,0]=0.659973 & hlg[20,11,0]=0.745042 & hlg[17,12,0]=0.811588 & hlg[15,13,0]=0.833928 & hlg[14,14,0]=0.847489 
 hlg[21,*,1]=hlg[21,*,1]-0.1
 hlg[*,14,1]=hlg[*,14,1]-0.1


; get rid of all the problem pieces (nans) by interpolating over it.
 fln=where(finite(hl) ne 1, cnan)
 if cnan gt 0 then begin
   hlgt=hlg
   hlgr=hlg
   for w=0, 1 do begin
     for t=0,23 do $  ;interpol over ref
       hlgt[t,*,w]=interpol(hlg[t,*,w],ref,ref,/nan) 
     for r=0,14 do $  ;interpol over tau
       hlgr[*,r,w]=interpol(hlg[*,r,w],tau,tau,/nan)
     for i=0,cnan-1 do hlg[fln[i]]=mean([hlgt[fln[i]],hlgr[fln[i]]]) ;set to mean of interpol tau or ref
   endfor
 endif


fp=dir+'IC_contour_v41'+tx
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=40, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=2.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,2,1] & !x.margin=[6,6] & !y.margin=[3,3]
 
 ; mu=155B
  mus='!9m!X'
  lvls=findgen(30)/29.+0.001
;stop
  contour, hlg[0:21,*,0],tau[0:21],ref,levels=lvls,/cell_fill,title='Liquid water cloud',xtitle='Optical thickness',$
   ytitle='Effective radius ('+mus+'m)', xrange=[1,100]

plots, [1.2,24.,24.,1.2,1.2],[3.,3.,49.,49.,3.]
plots, [26.,98.,98.,26.,26.],[26.,26.,49.,49.,26.]
plots, [26.,98.,98.,26.,26.],[3.,3.,24.,24.,3.]
xyouts, 12.,25.,'A',alignment=0.5
xyouts, 65.,37.5,'B',alignment=0.5
xyouts, 65.,12.5,'C',alignment=0.5
 
contour, hlg[0:21,*,1],tau[0:21], ref, levels=lvls,/cell_fill,title='Ice water cloud',xtitle='Optical thickness',$
 ytitle='Effective radius ('+mus+'m)',xmargin=[3,9],xrange=[1,100]

plots, [1.2,24.,24.,1.2,1.2],[3.,3.,49.,49.,3.]
plots, [26.,98.,98.,26.,26.],[26.,26.,49.,49.,26.]
plots, [26.,98.,98.,26.,26.],[3.,3.,24.,24.,3.]
xyouts, 12.,25.,'A',alignment=0.5
xyouts, 65.,37.5,'B',alignment=0.5
xyouts, 65.,12.5,'C',alignment=0.5

 contour,transpose([[lvls],[lvls]]),[0,1],lvls,levels=lvls,/cell_fill,ystyle=9,yticks=10,$
  ytickname=replicate(' ',11),position=[0.9,0.1,0.92,0.9],/normal,/noerase,xticks=1,xtickname=[' ',' ']
 axis,yaxis=1,ytitle='IC',yrange=[0,1],yticks=10

 device, /close
 spawn, 'convert "'+fp+'.ps" "'+fp+'.png"'
endif


if 0 then begin
  fp=dir+'IC_histogram_v4'
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

if 0 then begin
  fp=dir+'IC_hist_sectors_v4'
  print, 'making plot :'+fp
  set_plot, 'ps'
  loadct, 39, /silent
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
  device, xsize=40, ysize=25
   !p.font=1 & !p.thick=5 & !p.charsize=4.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
   !p.multi=[0,2,3] & !x.margin=[6,1] & !y.margin=[3,1] & !x.omargin=[0,0]

  his_a_li=histogram(maxhi[0:12,*,0],bins=1,nbins=15,min=0,locations=bins)
  his_b_li=histogram(maxhi[13:21,0:9,0],bins=1,nbins=15,min=0)
  his_c_li=histogram(maxhi[13:21,10:*,0],bins=1,nbins=15,min=0)
  his_a_ic=histogram(maxhi[0:12,*,1],bins=1,nbins=15,min=0)
  his_b_ic=histogram(maxhi[13:21,0:9,1],bins=1,nbins=15,min=0)
  his_c_ic=histogram(maxhi[13:21,10:*,1],bins=1,nbins=15,min=0)

  his2_a_li=histogram(maxhi2[0:12,*,0],bins=1,nbins=15,min=0,locations=bins)
  his2_b_li=histogram(maxhi2[13:21,0:9,0],bins=1,nbins=15,min=0)
  his2_c_li=histogram(maxhi2[13:21,10:*,0],bins=1,nbins=15,min=0)
  his2_a_ic=histogram(maxhi2[0:12,*,1],bins=1,nbins=15,min=0)
  his2_b_ic=histogram(maxhi2[13:21,0:9,1],bins=1,nbins=15,min=0)
  his2_c_ic=histogram(maxhi2[13:21,10:*,1],bins=1,nbins=15,min=0)

  tvlct,200,200,200,155
  bins=bins+1
  u=2.8
  plot, bins, his_a_li/total(his_a_li)*100., psym=10, xtitle='Parameter',ytitle='Occurences (%)',xticks=14,xtickformat='(I2)',yr=[0,100]
  oplot,bins, his2_a_li/total(his2_a_li)*100.,psym=10,color=155
  xyouts, 14,70.,'A - Liquid',charsize=u,alignment=1.
  plot, bins, his_a_ic/total(his_a_ic)*100., psym=10, xtitle='Parameter',ytitle='Occurences (%)',xticks=14,xtickformat='(I2)',yr=[0,100]
  oplot,bins, his2_a_ic/total(his2_a_ic)*100.,psym=10,color=155
  xyouts, 14,70.,'A - Ice',charsize=u,alignment=1.

  legend,['Highest IC','Second highest IC'],textcolors=[0,155],/right,box=0,charsize=u

  plot, bins, his_b_li/total(his_b_li)*100., psym=10, xtitle='Parameter',ytitle='Occurences (%)',xticks=14,xtickformat='(I2)',yr=[0,100]
  oplot,bins, his2_b_li/total(his2_b_li)*100.,psym=10,color=155
  xyouts, 14,70.,'B - Liquid',charsize=u,alignment=1.
  plot, bins, his_b_ic/total(his_b_ic)*100., psym=10, xtitle='Parameter',ytitle='Occurences (%)',xticks=14,xtickformat='(I2)',yr=[0,100]
  oplot,bins, his2_b_ic/total(his2_b_ic)*100.,psym=10,color=155
  xyouts, 14,70.,'B - Ice',charsize=u,alignment=1.

  plot, bins, his_c_li/total(his_c_li)*100., psym=10, xtitle='Parameter',ytitle='Occurences (%)',xticks=14,xtickformat='(I2)',yr=[0,100]
  oplot,bins, his2_c_li/total(his2_c_li)*100.,psym=10,color=155
  xyouts, 14,70.,'C - Liquid',charsize=u,alignment=1.
  plot, bins, his_c_ic/total(his_c_ic)*100., psym=10, xtitle='Parameter',ytitle='Occurences (%)',xticks=14,xtickformat='(I2)',yr=[0,100]
  oplot,bins, his2_c_ic/total(his2_c_ic)*100.,psym=10,color=155
  xyouts, 14,70.,'C - Ice',charsize=u,alignment=1.
  

  device, /close
  spawn, 'convert "'+fp+'.ps" "'+fp+'.png"'
endif

tau_set=100.
ref_set=15.
wp_set=0
nul=min(abs(tau-tau_set),ti)
nul=min(abs(ref-ref_set),ri)
nul=min(abs(wp-wp_set),wi)

tas2=3.
res2=40.
ws2=1
nul=min(abs(tau-tas2),ti2)
nul=min(abs(ref-res2),ri2)
nul=min(abs(wp-ws2),wi2)

if 1 then begin
refo=ref
if not win then restore, dir+'../data/pars_lut_taus_ref.out' else $
restore, 'C:\Users\Samuel\Research\SSFR3\data\pars_lut_taus_ref.out'
taus=findgen(100)+1.
refs=ref
ref=refo

fp=dir+'Post_model_pdf_v4'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=38, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=2.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,2,1] & !x.margin=[6,4] & !y.margin=[3,3]

  taus=findgen(100)+1.
  mus='!9m!X'
  ;probs=findgen(41)/40.
  ;probs=(findgen(21)^3.)/(20.^3.)
  probs=(findgen(21)^3.)/(20^3.)*max(post[ind[[ti,ti2],[ri,ri2],[wi,wi2]],*,*,*])*1.01
  clrs=findgen(n_elements(probs))*254./(n_elements(probs)-1)
  contour, post[ind[ti,ri,wi],*,*,0],taus,refs,levels=probs,xtitle='Optical thickness',title='Liquid case',$
   ytitle='Effective radius ('+mus+'m)',xrange=[1,max(taus)],yrange=[0,max(refs)],c_colors=clrs,/fill,/xlog
  
  if wp_rtm[ind[ti,ri,wi]] eq 0 then plots, tau_rtm[ind[ti,ri,wi]],ref_rtm[ind[ti,ri,wi]],psym=7, color=255,symsize=4, thick=8
  if wp_set eq 0 then plots, tau_set,ref_set,psym=4,color=254, symsize=3, thick=7

 contour, post[ind[ti2,ri2,wi2],*,*,1],taus,refs,levels=probs,xtitle='Optical thickness',title='Ice case',$
   ytitle='Effective radius ('+mus+'m)',xrange=[1,max(taus)],yrange=[0,max(refs)],c_colors=clrs,/fill,/xlog,xmargin=[2,8]

;  contour, post[ind[ti,ri,wi],*,*,1],taus, refs, levels=probs,title='Ice water cloud',xtitle='Optical depth',$
;   ytitle='Effective radius ('+mus+'m)',xrange=[0,max(taus)],yrange=[0,max(refs)],xmargin=[3,9],c_colors=clrs,/cell_fill

  if wp_rtm[ind[ti2,ri2,wi2]] eq 1 then plots, tau_rtm[ind[ti2,ri2,wi2]],ref_rtm[ind[ti2,ri2,wi2]],psym=7,color=255,symsize=4,thick=8
  if ws2 eq 1 then plots, tas2,res2,psym=4,color=254,symsize=3,thick=7


bar=findgen(29)/2.+0.5 ;[0.5,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]
lvl=[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]
 contour,transpose([[probs],[probs]]),[0,1],findgen(n_elements(probs)),levels=probs,/cell_fill,ystyle=9,yticks=14,$
  ytickname=replicate(' ',15),position=[0.88,0.1,0.90,0.9],/normal,/noerase,xticks=1,xtickname=[' ',' ']
 axis,yaxis=1,ytitle='Probability',yrange=[0,1],yticks=5,ytickname=string(probs[0:*:(n_elements(probs)-1)/5],FORMAT='(F5.3)')
 ;,ytickformat='(F4.2)'

 device, /close
 spawn, 'convert "'+fp+'.ps" "'+fp+'.png"'
endif

if 1 then begin
refo=ref
if not win then restore, dir+'../data/pars_lut_taus_ref.out' else $
restore, 'C:\Users\Samuel\Research\SSFR3\data\pars_lut_taus_ref.out'
taus=findgen(100)+1.
refs=ref
ref=refo

fp=dir+'Post_model_marginal_pdf_v4'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=40, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=2.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,2,2] & !x.margin=[6,4] & !y.margin=[3.5,1]

  mus='!9m!X'
  probs=findgen(41)/40.
  p=reform(post[ind[ti,ri,wi],*,*,*])
  pref=fltarr(n_elements(refs))
  pref=total(p[*,*,0],1)+total(p[*,*,1],1)
  ;for r=0, n_elements(refs)-1 do  pref[r]=int_tabulated(taus,p[0,*,r,0])+int_tabulated(taus,p[0,*,r,1])
  refns=where(pref ge max(pref)/100.*25.)
  ref_errf=[min(refs[refns]),max(refs[refns])]
  gfitr=gaussfit(refs,pref,coeffr,nterms=3)
;message, 'doing marginal'
  plot, refs,pref,xtitle='Effective radius ('+mus+'m)', ytitle='Probability density',title='Liquid case',$
   ymargin=[2.5,2],xticklen=0.1,yticks=4,ytickformat='(F4.2)'

  tvlct,190,190,190,151
  polyfill,[ref_errf[0],ref_errf[1],ref_errf[1],ref_errf[0]],[0,0,max(pref),max(pref)],color=151
  oplot,refs,gfitr,color=250
  plots, [coeffr[1]+coeffr[2],coeffr[1]+coeffr[2]],[0,max(pref)],color=250,linestyle=2
  plots, [coeffr[1]-coeffr[2],coeffr[1]-coeffr[2]],[0,max(pref)],color=250,linestyle=2
  oplot,refs,pref ,thick=7
  oplot,refs,gfitr,color=250,thick=3
  legend,['Uncertainty range','Fitted gaussian','Standard deviation'],color=[151,250,250],linestyle=[0,0,2],$
   thick=[30,5,5],pspacing=1.6,/right,box=0,charsize=2.0


  p2=reform(post[ind[ti2,ri2,wi2],*,*,*])
  pref2=fltarr(n_elements(refs))
  pref2=total(p2[*,*,0],1)+total(p2[*,*,1],1)
  ;for r=0, n_elements(refs)-1 do  pref2[r]=int_tabulated(taus,p2[0,*,r,0])+int_tabulated(taus,p2[0,*,r,1])
  refns2=where(pref2 ge max(pref2)/100.*25.)
  ref_errf2=[min(refs[refns2]),max(refs[refns2])]
  gfitr2=gaussfit(refs,pref2,coeffr2,nterms=3)
;message, 'doing marginal'
  plot, refs,pref2,xtitle='Effective radius ('+mus+'m)', ytitle='Probability density',title='Ice case',ymargin=[2.5,2],xticklen=0.1
  tvlct,190,190,190,151
  polyfill,[ref_errf2[0],ref_errf2[1],ref_errf2[1],ref_errf2[0]],[0,0,max(pref2),max(pref2)],color=151
  oplot,refs,gfitr2,color=250
  plots, [coeffr2[1]+coeffr2[2],coeffr2[1]+coeffr2[2]],[0,max(pref2)],color=250,linestyle=2
  plots, [coeffr2[1]-coeffr2[2],coeffr2[1]-coeffr2[2]],[0,max(pref2)],color=250,linestyle=2
  oplot,refs,pref2,thick=7  
  oplot,refs,gfitr2,color=250,thick=3

  ptau=fltarr(n_elements(taus))
  ptau=total(p[*,*,0],2)+total(p[*,*,1],2)
  ;for t=0, n_elements(taus)-1 do ptau[t]=int_tabulated(refs,p[0,t,*,0])+int_tabulated(refs,p[0,t,*,1])
  tauns=where(ptau ge max(ptau)/100.*25.)
  tau_errf=[min(taus[tauns]),max(taus[tauns])]
  plot, taus, ptau, xtitle='Optical thickness',ytitle='Probability density',ymargin=[3,1.5],xticklen=0.1
  polyfill,[tau_errf[0],tau_errf[1],tau_errf[1],tau_errf[0]],[0,0,max(ptau),max(ptau)],color=151
  gfitt=gaussfit(taus,ptau,coefft,nterm=3)
  oplot,taus,gfitt,color=250
  plots, [coefft[1]+coefft[2],coefft[1]+coefft[2]],[0,max(ptau)],color=250,linestyle=2
  plots, [coefft[1]-coefft[2],coefft[1]-coefft[2]],[0,max(ptau)],color=250,linestyle=2
  oplot,taus,ptau ,thick=7
  oplot,taus,gfitt,color=250,thick=3
 ;   legend,['Uncertainty range','Fitted gaussian','Standard deviation'],color=[151,250,250],linestyle=[0,0,2],$
 ;  thick=[30,5,5],pspacing=1.6,/right,box=0,charsize=1.6
  ptau2=fltarr(n_elements(taus))
  ptau2=total(p2[*,*,0],2)+total(p2[*,*,1],2)
  ;for t=0, n_elements(taus)-1 do ptau2[t]=int_tabulated(refs,p2[0,t,*,0])+int_tabulated(refs,p2[0,t,*,1])
  tauns2=where(ptau2 ge max(ptau2)/100.*25.)
  tau_errf2=[min(taus[tauns2])-0.35,max(taus[tauns2])+0.35]
  ptau2[3]=ptau2[3]*40000.
  plot, taus, ptau2, xtitle='Optical thickness',ytitle='Probability density',ymargin=[3,1.5],xrange=[0,20],xticklen=0.1;,xrange=[70,130]
  polyfill,[tau_errf2[0],tau_errf2[1],tau_errf2[1],tau_errf2[0]],[0,0,max(ptau2),max(ptau2)],color=151
  gfitt2=gaussfit(taus,ptau2,coefft2,nterm=3)
  oplot,taus,gfitt2,color=250
  plots, [coefft2[1]+coefft2[2],coefft2[1]+coefft2[2]],[0,max(ptau2)],color=250,linestyle=2
  plots, [coefft2[1]-coefft2[2],coefft2[1]-coefft2[2]],[0,max(ptau2)],color=250,linestyle=2
  oplot,taus,ptau2,thick=7
  oplot,taus,gfitt2,color=250,thick=3
 device, /close
 spawn, 'convert "'+fp+'.ps" "'+fp+'.png"'
endif
stop
if 0 then begin
fp=dir+'model_spectra_ex_v4'
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Making plot of uncertainty percentage per region
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
if 1 then begin

;build the uncertainty arrays
  refc=fltarr(24,15,2)
  refca=refc 
  tauc=refc
  tauca=refc
  for w=0,1 do begin
    for t=0,23 do begin
      for r=0,14 do begin
        refc[t,r,w]=abs(rer[1,ind[t,r,w]]-rer[0,ind[t,r,w]])/ret[ind[t,r,w]]*100./2.
        refca[t,r,w]=abs(ref_err[1,ind[t,r,w]]-ref_err[0,ind[t,r,w]])/ref_rtm[ind[t,r,w]]*100.
        tauc[t,r,w]=abs((tar[1,ind[t,r,w]])-(tar[0,ind[t,r,w]]))/tat[ind[t,r,w]]*100./2.
        tauca[t,r,w]=abs(tau_err[1,ind[t,r,w]]-tau_err[0,ind[t,r,w]])/tau_rtm[ind[t,r,w]]*100.

        if abs((tar[1,ind[t,r,w]])-(tar[0,ind[t,r,w]])) eq 0. then tauc[t,r,w]=abs((tar[1,ind[t,r,w]])-(tar[0,ind[t,r,w]])+0.5)/tat[ind[t,r,w]]*100./2.
        if abs(rer[1,ind[t,r,w]]-rer[0,ind[t,r,w]]) eq 0. then refc[t,r,w]=abs(rer[1,ind[t,r,w]]-rer[0,ind[t,r,w]]+0.5)/ret[ind[t,r,w]]*100./2.
      endfor
      refc[t,*,w]=smooth(refc[t,*,w],1,/nan)
      tauc[t,*,w]=smooth(tauc[t,*,w],1,/nan)
    endfor
  endfor
stop
ns=where(tauc gt 150.)
tauc[ns]=10.
;ns=where(tauc gt 90.)
;tauc[ns]=50.
ns=where(refc ge 50.)
refc[ns]=49.5


fp=dir+'uncertainty_contour_ref'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=40, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=2.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,2,1] & !x.margin=[6,6] & !y.margin=[3,3]

  mus='!9m!X'
  lvls=findgen(30)/29.*50.
  contour, refc[0:21,*,0],tau[0:21],ref,levels=lvls,/cell_fill,title='Liquid water cloud',xtitle='Optical thickness',$
   ytitle='Effective radius ('+mus+'m)',xrange=[1,100] ;,/xlog

  contour, refc[0:21,*,1],tau[0:21], ref, levels=lvls,/cell_fill,title='Ice water cloud',xtitle='Optical thickness',$
   ytitle='Effective radius ('+mus+'m)',xmargin=[3,9],xrange=[1,100] ;,/xlog

 contour,transpose([[lvls],[lvls]]),[0,1],lvls,levels=lvls,/cell_fill,ystyle=9,yticks=10,$
  ytickname=replicate(' ',11),position=[0.9,0.1,0.92,0.9],/normal,/noerase,xticks=1,xtickname=[' ',' ']
 axis,yaxis=1,ytitle='Effective radius uncertainty range (%)',yrange=[min(lvls),max(lvls)],yticks=10

 device, /close
 spawn, 'convert "'+fp+'.ps" "'+fp+'.png"'
 
 fp=dir+'uncertainty_contour_ref_all'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=40, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=2.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,2,1] & !x.margin=[6,6] & !y.margin=[3,3]

  mus='!9m!X'
  lvls=findgen(30)/29.*50.
  contour, refca[0:21,*,0],tau[0:21],ref,levels=lvls,/cell_fill,title='Liquid water cloud',xtitle='Optical thickness',$
   ytitle='Effective radius ('+mus+'m)',xrange=[1,100] ;,/xlog

  contour, refca[0:21,*,1],tau[0:21], ref, levels=lvls,/cell_fill,title='Ice water cloud',xtitle='Optical thickness',$
   ytitle='Effective radius ('+mus+'m)',xmargin=[3,9],xrange=[1,100];,/xlog

 contour,transpose([[lvls],[lvls]]),[0,1],lvls,levels=lvls,/cell_fill,ystyle=9,yticks=10,$
  ytickname=replicate(' ',11),position=[0.9,0.1,0.92,0.9],/normal,/noerase,xticks=1,xtickname=[' ',' ']
 axis,yaxis=1,ytitle='Effective radius uncertainty range (%)',yrange=[min(lvls),max(lvls)],yticks=10

 device, /close
 spawn, 'convert "'+fp+'.ps" "'+fp+'.png"'

fp=dir+'uncertainty_contour_tau'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=40, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=2.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,2,1] & !x.margin=[6,6] & !y.margin=[3,3]

  mus='!9m!X'
  lvls=findgen(30)/29.*50.
  contour, tauc[0:21,*,0],tau[0:21],ref,levels=lvls,/cell_fill,title='Liquid water cloud',xtitle='Optical thickness',$
   ytitle='Effective radius ('+mus+'m)',xrange=[1,100];,/xlog

  contour, tauc[0:21,*,1],tau[0:21], ref, levels=lvls,/cell_fill,title='Ice water cloud',xtitle='Optical thickness',$
   ytitle='Effective radius ('+mus+'m)',xmargin=[3,9],xrange=[1,100];,/xlog

 contour,transpose([[lvls],[lvls]]),[0,1],lvls,levels=lvls,/cell_fill,ystyle=9,yticks=10,$
  ytickname=replicate(' ',11),position=[0.9,0.1,0.92,0.9],/normal,/noerase,xticks=1,xtickname=[' ',' ']
 axis,yaxis=1,ytitle='Optical thickness uncertainty range (%)',yrange=[min(lvls),max(lvls)],yticks=10

 device, /close
 spawn, 'convert "'+fp+'.ps" "'+fp+'.png"'

fp=dir+'uncertainty_contour_tau_all'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=40, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=2.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,2,1] & !x.margin=[6,6] & !y.margin=[3,3]

  mus='!9m!X'
  lvls=findgen(30)/29.*50.
  contour, tauca[0:21,*,0],tau[0:21],ref,levels=lvls,/cell_fill,title='Liquid water cloud',xtitle='Optical thickness',$
   ytitle='Effective radius ('+mus+'m)',xrange=[1,100];,/xlog

  contour, tauca[0:21,*,1],tau[0:21], ref, levels=lvls,/cell_fill,title='Ice water cloud',xtitle='Optical thickness',$
   ytitle='Effective radius ('+mus+'m)',xmargin=[3,9],xrange=[1,100];,/xlog

 contour,transpose([[lvls],[lvls]]),[0,1],lvls,levels=lvls,/cell_fill,ystyle=9,yticks=10,$
  ytickname=replicate(' ',11),position=[0.9,0.1,0.92,0.9],/normal,/noerase,xticks=1,xtickname=[' ',' ']
 axis,yaxis=1,ytitle='Optical thickness uncertainty range (%)',yrange=[min(lvls),max(lvls)],yticks=10

 device, /close
 spawn, 'convert "'+fp+'.ps" "'+fp+'.png"'

endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Making plot of bias per region
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
if 1 then begin

;build the bias arrays
  refb=fltarr(24,15,2)
  refba=refb
  taub=refb
  tauba=refb
  refbb=refb
  taubb=taub
  for w=0,1 do begin
    for t=0,23 do begin
      for r=0,14 do begin
        refb[t,r,w]=(ret[ind[t,r,w]]-ref[r])/ref[r]*100.
        refbb[t,r,w]=(ret[ind[t,r,w]]-ref[r])
        refba[t,r,w]=(ref_rtm[ind[t,r,w]]-ref[r])/ref[r]*100.
        taub[t,r,w]=(tat[ind[t,r,w]]-tau[t])/tau[t]*100.
        taubb[t,r,w]=tat[ind[t,r,w]]-tau[t]
        tauba[t,r,w]=(tau_rtm[ind[t,r,w]]-tau[t])/tau[t]*100.
      endfor
    endfor
  endfor
save, refbb, taubb, tau, ref, filename='~/SSFR3/data/bias.out'

fp=dir+'bias_contour_ref'
print, 'making plot :'+fp
set_plot, 'ps'
; loadct, 39, /silent
p=indgen(128)*2
s=intarr(128)+255
r=[s,reverse(p)] & g=[p,reverse(p)] & b=[p,s]
r[0]=0 & r[255]=255 & g[255]=255
tvlct,r,g,b
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=40, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=2.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,2,1] & !x.margin=[6,6] & !y.margin=[3,3]

  mus='!9m!X'
  lvls=findgen(31)/30.*20.-10.
  contour, refb[0:21,*,0],tau[0:21],ref,levels=lvls,/cell_fill,title='Liquid water cloud',xtitle='Optical thickness',$
   ytitle='Effective radius ('+mus+'m)',xrange=[1,100];,/xlog

  contour, refb[0:21,*,1],tau[0:21], ref, levels=lvls,/cell_fill,title='Ice water cloud',xtitle='Optical thickness',$
   ytitle='Effective radius ('+mus+'m)',xmargin=[3,9],xrange=[1,100];,/xlog

 contour,transpose([[lvls],[lvls]]),[0,1],lvls,levels=lvls,/cell_fill,ystyle=9,yticks=10,$
  ytickname=replicate(' ',11),position=[0.9,0.1,0.92,0.9],/normal,/noerase,xticks=1,xtickname=[' ',' ']
 axis,yaxis=1,ytitle='Effective radius bias (%)',yrange=[-10,10],yticks=10

 device, /close
 spawn, 'convert "'+fp+'.ps" "'+fp+'.png"'

fp=dir+'bias_contour_ref_all'
print, 'making plot :'+fp
set_plot, 'ps'
; loadct, 39, /silent
p=indgen(128)*2
s=intarr(128)+255
r=[s,reverse(p)] & g=[p,reverse(p)] & b=[p,s]
r[0]=0 & r[255]=255 & g[255]=255
tvlct,r,g,b
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=40, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=2.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,2,1] & !x.margin=[6,6] & !y.margin=[3,3]

  mus='!9m!X'
  lvls=findgen(31)/30.*20.-10.
  contour, refba[0:21,*,0],tau[0:21],ref,levels=lvls,/cell_fill,title='Liquid water cloud',xtitle='Optical thickness',$
   ytitle='Effective radius ('+mus+'m)',xrange=[1,100];,/xlog

  contour, refba[0:21,*,1],tau[0:21], ref, levels=lvls,/cell_fill,title='Ice water cloud',xtitle='Optical thickness',$
   ytitle='Effective radius ('+mus+'m)',xmargin=[3,9],xrange=[1,100];,/xlog

 contour,transpose([[lvls],[lvls]]),[0,1],lvls,levels=lvls,/cell_fill,ystyle=9,yticks=10,$
  ytickname=replicate(' ',11),position=[0.9,0.1,0.92,0.9],/normal,/noerase,xticks=1,xtickname=[' ',' ']
 axis,yaxis=1,ytitle='Effective radius bias (%)',yrange=[-10,10],yticks=10

 device, /close
 spawn, 'convert "'+fp+'.ps" "'+fp+'.png"'

fp=dir+'bias_contour_tau'
print, 'making plot :'+fp
set_plot, 'ps'
; loadct, 39, /silent
p=indgen(128)*2
s=intarr(128)+255
r=[s,reverse(p)] & g=[p,reverse(p)] & b=[p,s]
r[0]=0 & r[255]=255 & g[255]=255
tvlct,r,g,b
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=40, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=2.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,2,1] & !x.margin=[6,6] & !y.margin=[3,3]

  mus='!9m!X'
  lvls=findgen(31)/30.*20.-10.
  contour, taub[0:21,*,0],tau[0:21],ref,levels=lvls,/cell_fill,title='Liquid water cloud',xtitle='Optical thickness',$
   ytitle='Effective radius ('+mus+'m)',xrange=[1,100];,/xlog

  contour, taub[0:21,*,1],tau[0:21], ref, levels=lvls,/cell_fill,title='Ice water cloud',xtitle='Optical thickness',$
   ytitle='Effective radius ('+mus+'m)',xmargin=[3,9],xrange=[1,100];,/xlog

 contour,transpose([[lvls],[lvls]]),[0,1],lvls,levels=lvls,/cell_fill,ystyle=9,yticks=10,$
  ytickname=replicate(' ',11),position=[0.9,0.1,0.92,0.9],/normal,/noerase,xticks=1,xtickname=[' ',' ']
 axis,yaxis=1,ytitle='Optical thickness bias (%)',yrange=[-10,10],yticks=10

 device, /close
 spawn, 'convert "'+fp+'.ps" "'+fp+'.png"'

fp=dir+'bias_contour_tau_all'
print, 'making plot :'+fp
set_plot, 'ps'
; loadct, 39, /silent
p=indgen(128)*2
s=intarr(128)+255
r=[s,reverse(p)] & g=[p,reverse(p)] & b=[p,s]
r[0]=0 & r[255]=255 & g[255]=255
tvlct,r,g,b
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=40, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=2.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,2,1] & !x.margin=[6,6] & !y.margin=[3,3]

  mus='!9m!X'
  lvls=findgen(31)/30.*20.-10.
  contour, tauba[0:21,*,0],tau[0:21],ref,levels=lvls,/cell_fill,title='Liquid water cloud',xtitle='Optical thickness',$
   ytitle='Effective radius ('+mus+'m)',xrange=[1,100];,/xlog

  contour, tauba[0:21,*,1],tau[0:21], ref, levels=lvls,/cell_fill,title='Ice water cloud',xtitle='Optical thickness',$
   ytitle='Effective radius ('+mus+'m)',xmargin=[3,9],xrange=[1,100];,/xlog

 contour,transpose([[lvls],[lvls]]),[0,1],lvls,levels=lvls,/cell_fill,ystyle=9,yticks=10,$
  ytickname=replicate(' ',11),position=[0.9,0.1,0.92,0.9],/normal,/noerase,xticks=1,xtickname=[' ',' ']
 axis,yaxis=1,ytitle='Optical thickness bias (%)',yrange=[-10,10],yticks=10

 device, /close
 spawn, 'convert "'+fp+'.ps" "'+fp+'.png"'
endif


stop
end

