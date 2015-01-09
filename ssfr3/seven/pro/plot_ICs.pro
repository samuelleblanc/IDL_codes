; program that plots the sample IC for a few different cases
; 

pro plot_ICs

win=0

if win then dir='C:\Users\Samuel\Research\SSFR3\plots\new\' else $
 dir='/home/leblanc/SSFR3/plots/new/'

if win then fn='C:\Users\Samuel\Research\SSFR3\retrieved\retrieved_pdf_20120523_model_limp2_v4.out' else $
 fn='/argus/roof/SSFR3/retrieved/retrieved_pdf_20120523_model_limp2_v4.out'
print, 'restoring '+fn
restore, fn
hml=hm_rtm ;added from previous
hcl=hml ; cumulative
for p=1,n_elements(hml[*,0])-1 do hcl[p,*]=total(hml[0:p,*],1)

if win then fn='C:\Users\Samuel\Research\SSFR3\retrieved\retrieved_pdf_20120523_model_v4.out' else $
 fn='/argus/roof/SSFR3/retrieved/retrieved_pdf_20120523_model_v4.out'
print, 'restoring '+fn
restore, fn
hi=hi_rtm ;independent
hm=hm_rtm ;added from cumulative
hc=hm ; cumulative
for p=1,n_elements(hm[*,0])-1 do hc[p,*]=total(hm[0:p,*],1)



ind=intarr(24,15,2)
for t=0,23 do for r=0,14 do for w=0,1 do ind[t,r,w]=360*w+t*15+r

; We want to show the evolution of the marginal pdfs per input of parameter
; also show the change in uncertainty in ref, tau, and change in probability of ice/liquid with inputs of parameters
; ind representes the grid at which we report
; 100 & 50 are the tau and ref grid in which we integrate over

refs=findgen(50)+1.
taus=findgen(100)+1.

re_err=fltarr(24,15,2,16,2) ;keeps the uncertainty in ref for each parameter
ta_err=fltarr(24,15,2,16,2) ;keeps the uncertainty in tau
re=fltarr(24,15,2,16) ;keeps the max prob in ref for each parameter
ta=fltarr(24,15,2,16) ;keeps the max prob in tau
ww=fltarr(24,15,2,16) ;keeps the retrieved phase
pts=fltarr(24,15,2,16) ;keeps the tau marginal pdf ic
prs=fltarr(24,15,2,16) ;keeps the ref marginal pdf ic
probi=fltarr(24,15,2,16) ; keeps the ice probability
probl=fltarr(24,15,2,16) ; keeps the liquid probability


re_err[*,*,*,0,0]=0.
re_err[*,*,*,0,1]=50.
ta_err[*,*,*,0,0]=0.
ta_err[*,*,*,0,1]=100.
ta[*,*,*,0]=50.
re[*,*,*,0]=25.
probi[*,*,*,0]=50.
probl[*,*,*,0]=50.

for p=0,14 do begin
  for t=0, 17 do begin
    if t ne 8 and t ne 17 and t ne 3 then continue
    print, p,t
    for r=0, 14 do begin
      for w=0, 1 do begin
        pdf=reform(post_pdf[ind[t,r,w],*,*,*,p])/total(post_pdf[ind[t,r,w],*,*,*,p])
        n=max(pdf,/nan,ii)
        if not finite(n) then continue
        in=array_indices(pdf,ii)
        re[t,r,w,p+1]=refs[in[1]]
        ta[t,r,w,p+1]=taus[in[0]]
        ww[t,r,w,p+1]=in[2]
        marg_postt=total(total(pdf,3),2)
        marg_postr=total(total(pdf,3),1)
        marg_postw=total(total(pdf,2),1)
;        probi[t,r,w,p+1]=marg_postw[1]*100.
;        probl[t,r,w,p+1]=marg_postw[0]*100.
        probi[t,r,w,p+1]=total(post_pdf[ind[t,r,w],*,*,1,p])/total(post_pdf[ind[t,r,w],*,*,*,p])*100.
        probl[t,r,w,p+1]=total(post_pdf[ind[t,r,w],*,*,0,p])/total(post_pdf[ind[t,r,w],*,*,*,p])*100.


;        if t eq 8 and r eq 5 and p eq 0 then stop
        re_err[t,r,w,p+1,*]=err(marg_postr,refs,max=n)
        ta_err[t,r,w,p+1,*]=err(marg_postt,taus,max=n)
        pts[t,r,w,p+1]=sic(marg_postt/total(marg_postt))
        prs[t,r,w,p+1]=sic(marg_postr/total(marg_postr))

      endfor ;w loop
    endfor  ;ref loop
  endfor  ;tau loop
endfor  ;p loop

;stop
if 1 then begin
 fp=dir+'IC_sample_v5'
 ; for a cloud of tau 100, ref 15, wp 0
 t=17 & r=5 & w=0 & t2=8 & w2=1 & r2=12 & t3=3 & r3=2
 print, 'making plot :'+fp
 set_plot, 'ps'
  loadct, 39, /silent
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
  device, xsize=34, ysize=20
   !p.font=1 & !p.thick=5 & !p.charsize=2.5 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
   !p.multi=[0,1,2] & !x.margin=[8,17] & !y.margin=[0.6,1.0] & !y.omargin=[3,0] & !p.symsize=6

  tvlct,225,90,90,253
  tvlct,46,163,84,133 ;green
  tvlct,90,90,225,40
  tvlct,100,100,100,254
  tvlct,240,115,235,235 ;purple
  tvlct,153,216,201,71 ;turquoise
  usersym, [ -1, 1, 1, -1, -1 ,1], [ 1, 1, -1, -1, 1 ,1], /fill

  ts=24

  plot, findgen(15)+1,psym=2,$
   ytitle='Independent IC',xticks=14,xtickname=replicate(' ',15),yrange=[0.,0.7],xrange=[1,15],/nodata,xstyle=3
  for p=0, 14 do begin
    plots, [p+0.65,p+0.65],[0.,hi[p,ind[t,r,w]]],thick=ts,color=40
    plots, [p+0.77,p+0.77],[0.,hi[p,ind[t2,r,w]]],thick=ts,color=253
    plots, [p+0.85,p+0.85],[0.,hi[p,ind[t3,r3,w]]],thick=ts,color=71
    plots, [p+0.95,p+0.95],[0.,hi[p,ind[t,r,w2]]],thick=ts,color=133
    plots, [p+1.08,p+1.08],[0.,hi[p,ind[t2,r,w2]]],thick=ts,color=210
    plots, [p+1.20,p+1.20],[0.,hi[p,ind[t2,r2,w2]]],thick=ts,color=254
    plots, [p+1.31,p+1.31],[0.,hi[p,ind[t3,r2,w2]]],thick=ts,color=235
  endfor

  pind=[6,5,2,3,11,12,14,7]-1
  pind=pind[sort(pind)]
  k=indgen(n_elements(pind))

  ps=findgen(15)+1

  xtit='!9h!X!D'+string(indgen(15)+1,format='(I2)')+'!N'
  plot, findgen(15)+1, hi[*],psym=2, xtickname=xtit,xticklen=0.1,$
   ytitle='Cumulative IC',xticks=14,yrange=[0.,1.0],xrange=[1,15],/nodata,xstyle=3
  ;plots,[0.7,15.3],[0,0],thick=2,linestyle=2
  oplot, ps, hc[*,ind[t,r,w]],color=40,psym=-2,thick=10
  oplot, ps, hc[*,ind[t2,r,w]],color=253,psym=-2,thick=10
  oplot, ps, hc[*,ind[t3,r3,w]],color=71,psym=-2,thick=10
  oplot, ps, hc[*,ind[t,r,w2]],color=133,psym=-2,thick=10
  oplot, ps, hc[*,ind[t2,r,w2]],color=210,psym=-2,thick=10
  oplot, ps, hc[*,ind[t2,r2,w2]],color=254,psym=-2,thick=10
  oplot, ps, hc[*,ind[t3,r2,w2]],color=235,psym=-2,thick=10

  oplot, pind[k]+1, hcl[*,ind[t,r,w]],color=40,psym=-2,thick=5, linestyle=2
  oplot, pind[k]+1, hcl[*,ind[t2,r,w]],color=253,psym=-2,thick=5, linestyle=2
  oplot, pind[k]+1, hcl[*,ind[t3,r3,w]],color=71,psym=-2,thick=5, linestyle=2
  oplot, pind[k]+1, hcl[*,ind[t,r,w2]],color=133,psym=-2,thick=5,linestyle=2
  oplot, pind[k]+1, hcl[*,ind[t2,r,w2]],color=210,psym=-2,thick=5,linestyle=2
  oplot, pind[k]+1, hcl[*,ind[t2,r2,w2]],color=254,psym=-2,thick=5,linestyle=2
  oplot, pind[k]+1, hcl[*,ind[t3,r2,w2]],color=235,psym=-2,thick=5,linestyle=2

  legend, ['!9t!X=100, r!De!N=15 !9m!Xm liquid','!9t!X=20, r!De!N=15 !9m!Xm liquid',$
           '!9t!X=3, r!De!N=7.5 !9m!Xm liquid',$
           '!9t!X=100, r!De!N=15 !9m!Xm ice','!9t!X=20, r!De!N=15 !9m!Xm ice',$
           '!9t!X=20, r!De!N=40 !9m!Xm ice','!9t!X=3, r!De!N=40 !9m!Xm ice'],$
           textcolors=[40,253,71,133,210,254,235],box=0,charsize=2.4,position=[0.72,0.67],/normal
  legend,['All parameters','Parameter subset'],linestyle=[0,2],thick=[10,5],pspacing=1.8,box=0,/right,$
           /bottom,charsize=2.5
 device, /close
 spawn, 'convert "'+fp+'.ps" "'+fp+'.png"'
endif



p=indgen(16)+0
if 0 then begin
;; make a new figure for just the marginal pdfs for these cases
 fp=dir+'IC_marg_sample_v5'
 ; for a cloud of tau 100, ref 15, wp 0
 t=17 & r=5 & w=0 & t2=8 & w2=1 & r2=12 & t3=3
 print, 'making plot :'+fp
 set_plot, 'ps'
  loadct, 39, /silent
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
  device, xsize=33, ysize=20
   !p.font=1 & !p.thick=5 & !p.charsize=6.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
   !p.multi=[0,1,3] & !x.margin=[6,12] & !y.margin=[0.3,0.4] & !y.omargin=[3,0] & !p.charsize=4

  tvlct,225,90,90,253
  tvlct,90,225,90,133
  tvlct,90,90,225,40
  tvlct,100,100,100,254
  tvlct,240,115,235,235 ;purple
  usersym, [ -1, 1, 1, -1, -1 ,1], [ 1, 1, -1, -1, 1 ,1], /fill
 
  plot, p, ta[t,r,w,*],psym=2,xticklen=0.1,yrange=[-2,102],$
   ytitle='!9t!X',xticks=15,xtickname=replicate(' ',16),xrange=[0,15],/nodata,xstyle=3
  oplot,p-0.25,ta[t,r,w,*],psym=-6,color=40,thick=10
  oplot,p-0.15,ta[t2,r,w,*],psym=-6,color=253,thick=10
  oplot,p-0.05,ta[t,r,w2,*],psym=-6,color=133,thick=10
  oplot,p+0.05,ta[t2,r,w2,*],psym=-6,color=210,thick=10
  oplot,p+0.15,ta[t2,r2,w2,*],psym=-6,color=254,thick=10
  oplot,p+0.25,ta[t3,r2,w2,*],psym=-6,color=235,thick=10
 
  errplot,p-0.25,ta_err[t,r,w,*,0],ta_err[t,r,w,*,1],color=40
  errplot,p-0.15,ta_err[t2,r,w,*,0],ta_err[t2,r,w,*,1],color=253
  errplot,p-0.05,ta_err[t,r,w2,*,0],ta_err[t,r,w2,*,1],color=133
  errplot,p+0.05,ta_err[t2,r,w2,*,0],ta_err[t2,r,w2,*,1],color=210
  errplot,p+0.15,ta_err[t2,r2,w2,*,0],ta_err[t2,r2,w2,*,1],color=254
  errplot,p+0.25,ta_err[t3,r2,w2,*,0],ta_err[t3,r2,w2,*,1],color=235

  plot, p, ta[t,r,w,*],psym=2,xticklen=0.1,yrange=[-2,52],$
   ytitle='r!De!N (!9m!Xm)',xticks=15,xtickname=replicate(' ',16),xrange=[0,15],/nodata,xstyle=3
  oplot,p-0.25,re[t,r,w,*],psym=-6,color=40,thick=10
  oplot,p-0.15,re[t2,r,w,*],psym=-6,color=253,thick=10
  oplot,p-0.05,re[t,r,w2,*],psym=-6,color=133,thick=10
  oplot,p+0.05,re[t2,r,w2,*],psym=-6,color=210,thick=10
  oplot,p+0.15,re[t2,r2,w2,*],psym=-6,color=254,thick=10
  oplot,p+0.25,re[t3,r2,w2,*],psym=-6,color=235,thick=10
  
  errplot,p-0.25,re_err[t,r,w,*,0],re_err[t,r,w,*,1],color=40
  errplot,p-0.15,re_err[t2,r,w,*,0],re_err[t2,r,w,*,1],color=253
  errplot,p-0.05,re_err[t,r,w2,*,0],re_err[t,r,w2,*,1],color=133
  errplot,p+0.05,re_err[t2,r,w2,*,0],re_err[t2,r,w2,*,1],color=210
  errplot,p+0.15,re_err[t2,r2,w2,*,0],re_err[t2,r2,w2,*,1],color=254
  errplot,p+0.25,re_err[t3,r2,w2,*,0],re_err[t3,r2,w2,*,1],color=235 

  plot, p, ta[t,r,w,*],psym=2,xticklen=0.1,yrange=[-2,102],xtitle='Spectral parameter',$
   ytitle='Liquid cloud probability (%)',xticks=15,xrange=[0,15],/nodata,xstyle=3
  oplot,p-0.25,probi[t,r,w,*],psym=-6,color=40,thick=10,linestyle=2
  oplot,p-0.15,probi[t2,r,w,*],psym=-6,color=253,thick=10,linestyle=2
  oplot,p-0.05,probi[t,r,w2,*],psym=-6,color=133,thick=10,linestyle=2
  oplot,p+0.05,probi[t2,r,w2,*],psym=-6,color=210,thick=10,linestyle=2
  oplot,p+0.15,probi[t2,r2,w2,*],psym=-6,color=254,thick=10,linestyle=2
  oplot,p+0.25,probi[t3,r2,w2,*],psym=-6,color=235,thick=10,linestyle=2

  oplot,p-0.25,probl[t,r,w,*],psym=-5,color=40,thick=10
  oplot,p-0.15,probl[t2,r,w,*],psym=-5,color=253,thick=10
  oplot,p-0.05,probl[t,r,w2,*],psym=-5,color=133,thick=10
  oplot,p+0.05,probl[t2,r,w2,*],psym=-5,color=210,thick=10
  oplot,p+0.15,probl[t2,r2,w2,*],psym=-5,color=254,thick=10
  oplot,p+0.25,probl[t3,r2,w2,*],psym=-5,color=235,thick=10
  legend,['Liquid','Ice'],linestyle=[0,2],thick=[10,10],/right,box=0,color=[0,0],charsize=2.0,pspacing=1.4
  axis,yaxis=1,ytitle='Ice cloud probability (%)',yr=[0,100]
 
  legend, ['!9t!X=100, r!De!N=15 !9m!Xm liquid','!9t!X=20, r!De!N=15 !9m!Xm liquid',$
           '!9t!X=100, r!De!N=15 !9m!Xm ice','!9t!X=20, r!De!N=15 !9m!Xm ice',$
           '!9t!X=20, r!De!N=40 !9m!Xm ice','!9t!X=3, r!De!N=40 !9m!Xm ice'],$
           textcolors=[40,253,133,210,254,235],box=0,charsize=2.4, position=[0.8,0.7],/normal


 device, /close
 spawn, 'convert "'+fp+'.ps" "'+fp+'.png"'
endif


;; make a new figure for just the marginal pdfs for these cases
 fp=dir+'IC_marg_sample_v6'
 ; for a cloud of tau 100, ref 15, wp 0
 t=17 & r=5 & w=0 & t2=8 & w2=1 & r2=12 & t3=3 & r3=2
 print, 'making plot :'+fp
 set_plot, 'ps'
  loadct, 39, /silent
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
  device, xsize=34, ysize=20
   !p.font=1 & !p.thick=5 & !p.charsize=4.5 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
   !p.multi=[0,1,3] & !x.margin=[8,17] & !y.margin=[0.6,1.0] & !y.omargin=[3,0] & !p.symsize=6
 
  tvlct,225,90,90,253
  tvlct,46,163,84,133 ;green
  tvlct,90,90,225,40
  tvlct,100,100,100,254
  tvlct,240,115,235,235 ;purple
  tvlct,153,216,201,71 ;turquoise
  usersym, [ -1, 1, 1, -1, -1 ,1], [ 1, 1, -1, -1, 1 ,1], /fill
 
  plot, p, ta[t,r,w,*],psym=2,xticklen=0.1,yrange=[0,100],ytickformat='(I6)',$
   ytitle='!9t!X uncertainty!Crange',xticks=15,xtickname=replicate(' ',16),xrange=[0,15],/nodata,xstyle=3
  oplot,p,ta_err[t,r,w,*,1]-ta_err[t,r,w,*,0],color=40,psym=-6,thick=10
  oplot,p,ta_err[t2,r,w,*,1]-ta_err[t2,r,w,*,0],color=253,psym=-6,thick=10
  oplot,p,ta_err[t3,r3,w,*,1]-ta_err[t3,r3,w,*,0],color=71,psym=-6,thick=10
  oplot,p,ta_err[t,r,w2,*,1]-ta_err[t,r,w2,*,0],color=133,psym=-6,thick=10
  oplot,p,ta_err[t2,r,w2,*,1]-ta_err[t2,r,w2,*,0],color=210,psym=-6,thick=10
  oplot,p,ta_err[t2,r2,w2,*,1]-ta_err[t2,r2,w2,*,0],color=254,psym=-6,thick=10
  oplot,p,ta_err[t3,r2,w2,*,1]-ta_err[t3,r2,w2,*,0],color=235,psym=-6,thick=10

  plot, p, ta[t,r,w,*],psym=2,xticklen=0.1,yrange=[0,50],$
   ytitle='r!De!N uncertainty!Crange (!9m!Xm)',xticks=15,xtickname=replicate(' ',16),xrange=[0,15],/nodata,xstyle=3
  oplot,p,re_err[t,r,w,*,1]-re_err[t,r,w,*,0],color=40,psym=-6,thick=10
  oplot,p,re_err[t2,r,w,*,1]-re_err[t2,r,w,*,0],color=253,psym=-6,thick=10
  oplot,p,re_err[t3,r3,w,*,1]-re_err[t3,r3,w,*,0],color=71,psym=-6,thick=10
  oplot,p,re_err[t,r,w2,*,1]-re_err[t,r,w2,*,0],color=133,psym=-6,thick=10
  oplot,p,re_err[t2,r,w2,*,1]-re_err[t2,r,w2,*,0],color=210,psym=-6,thick=10
  oplot,p,re_err[t2,r2,w2,*,1]-re_err[t2,r2,w2,*,0],color=254,psym=-6,thick=10
  oplot,p,re_err[t3,r2,w2,*,1]-re_err[t3,r2,w2,*,0],color=235,psym=-6,thick=10


xtit='!9h!X!D'+string(indgen(15)+1,format='(I2)')+'!N'
xtit=['prior',xtit]
 d=0.0
 ss=1.8
  plot, p, ta[t,r,w,*],psym=2,xticklen=0.1,yrange=[-2,102],ytickformat='(I6)',$
   ytitle='Liquid cloud!Cprobability (%)',xticks=15,xrange=[0,15],/nodata,xstyle=3,xtickname=xtit
  oplot,p-5.*d,probi[t,r,w,*],psym=-6,color=40,thick=10,linestyle=2,symsize=ss
  oplot,p-3.*d,probi[t2,r,w,*],psym=-6,color=253,thick=10,linestyle=2,symsize=ss
  oplot,p-3.*d,probi[t3,r3,w,*],psym=-6,color=71,thick=10,linestyle=2,symsize=ss
  oplot,p-1.*d,probi[t,r,w2,*],psym=-6,color=133,thick=10,linestyle=2,symsize=ss
  oplot,p+1.*d,probi[t2,r,w2,*],psym=-6,color=210,thick=10,linestyle=2,symsize=ss
  oplot,p+3.*d,probi[t2,r2,w2,*],psym=-6,color=254,thick=10,linestyle=2,symsize=ss
  oplot,p+5.*d,probi[t3,r2,w2,*],psym=-6,color=235,thick=10,linestyle=2,symsize=ss

  oplot,p-5.*d,probl[t,r,w,*],psym=-7,color=40,thick=10,symsize=ss
  oplot,p-3.*d,probl[t2,r,w,*],psym=-7,color=253,thick=10,symsize=ss
  oplot,p-3.*d,probl[t3,r3,w,*],psym=-7,color=71,thick=10,symsize=ss
  oplot,p-1.*d,probl[t,r,w2,*],psym=-7,color=133,thick=10,symsize=ss
  oplot,p+1.*d,probl[t2,r,w2,*],psym=-7,color=210,thick=10,symsize=ss
  oplot,p+3.*d,probl[t2,r2,w2,*],psym=-7,color=254,thick=10,symsize=ss
  oplot,p+5.*d,probl[t3,r2,w2,*],psym=-7,color=235,thick=10,symsize=ss
  legend,['Liquid','Ice'],linestyle=[0,2],thick=[10,10],/right,box=0,color=[0,0],charsize=2.0,pspacing=1.4;,psym=[-5,-6],psymsize=[4,4]
  legend,[' ',' '],/right,box=0,color=[0,0],charsize=1.6,pspacing=1.4,psym=[7,6],position=[14.6,86],charthick=10,spacing=2
  axis,yaxis=1,ytitle='Ice cloud!Cprobability (%)',yr=[0,100]
 
  legend, ['!9t!X=100, r!De!N=15 !9m!Xm liquid','!9t!X=20, r!De!N=15 !9m!Xm liquid',$
           '!9t!X=3, r!De!N=7.5 !9m!Xm liquid',$
           '!9t!X=100, r!De!N=15 !9m!Xm ice','!9t!X=20, r!De!N=15 !9m!Xm ice',$
           '!9t!X=20, r!De!N=40 !9m!Xm ice','!9t!X=3, r!De!N=40 !9m!Xm ice'],$
           textcolors=[40,253,71,133,210,254,235],box=0,charsize=2.4, position=[0.74,0.72],/normal
 
 device, /close
 spawn, 'convert "'+fp+'.ps" "'+fp+'.png"'


stop
end

; make a function to calculate the shannon information content
function SIC, post
  ;calculate the shanon information content
  snorm=alog(n_elements(post))/alog(2.)
  prio_pdf=post*0.+1.
  sprio=(-1.)*total(prio_pdf/total(prio_pdf)*alog(prio_pdf/total(prio_pdf))/alog(2.))
  spost=(-1.)*total(post*alog(post+1.E-42)/alog(2.))
  H=(sprio-spost)/snorm
return,h
end


;make a function that returns the uncertainty values from a marginal pdf (at 25% of maximum probability)
function err, pdf, x,max=max
  dx=fltarr(2)
  if n_elements(max) gt 0 then nt=where(pdf ge max*0.25) else $
   nt=where(pdf ge max(pdf,/nan)*0.25)
;  if min(nt) eq 0 and n_elements(nt) gt 1 then nt=nt[1:*]
  dx[0]=min(x[nt])
  dx[1]=max(x[nt])
return,dx
end
