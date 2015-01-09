; program that plots the sample IC for a few different cases
; 

pro plot_ICs

win=1

if win then dir='C:\Users\Samuel\Research\SSFR3\plots\new\' else $
 dir='/home/leblanc/SSFR3/plots/new/'

if win then fn='C:\Users\Samuel\Research\SSFR3\retrieved\retrieved_pdf_20120523_model_limp2_v4.out' else $
 fn='/argus/roof/SSFR3/retrieved/retrieved_pdf_20120523_model_limp2_v4.out'
print, 'restoring '+fn
;restore, fn
;hml=hm_rtm
;rer=ref_err
;tar=tau_err
;ret=ref_rtm
;tat=tau_rtm
;htt=h_rtm

if win then fn='C:\Users\Samuel\Research\SSFR3\retrieved\retrieved_pdf_20120523_model_v4.out' else $
 fn='/argus/roof/SSFR3/retrieved/retrieved_pdf_20120523_model_v4.out'
print, 'restoring '+fn
restore, fn
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
        probi[t,r,w,p+1]=total(post_pdf[ind[t,r,w],*,*,1,p])/total(post_pdf[ind[t,r,w],*,*,*,p])
        probl[t,r,w,p+1]=total(post_pdf[ind[t,r,w],*,*,0,p])/total(post_pdf[ind[t,r,w],*,*,*,p])

;        if t eq 8 and r eq 5 and p eq 0 then stop
        re_err[t,r,w,p+1,*]=err(marg_postr,refs,max=n)
        ta_err[t,r,w,p+1,*]=err(marg_postt,taus,max=n)
        pts[t,r,w,p+1]=sic(marg_postt/total(marg_postt))
        prs[t,r,w,p+1]=sic(marg_postr/total(marg_postr))

      endfor ;w loop
    endfor  ;ref loop
  endfor  ;tau loop
endfor  ;p loop



;Hm=hm_rtm[*,ind]

;stop
if 0 then begin
 fp=dir+'IC_sample_v5'
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

  plot, findgen(15)+1, hi[*],psym=2, ymargin=[1,0],$
   ytitle='Independent IC',xticks=14,xtickname=replicate(15,' '),yrange=[0.,0.7],xrange=[1,15],/nodata,xstyle=3
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
  pind=[6,5,2,3,11,12,14,7]-1
  pind=pind[sort(pind)]
  k=indgen(n_elements(pind))

  hcl[0,*]=hml[k[0],*]
  for p=1, 14 do hmc[*,*,*,p]=total(hm[*,*,*,0:p],4)
  for p=1, n_elements(pind)-1 do hcl[p,*]=total(hml[k[0:p],*],1)
  ps=findgen(15)+1
  plot, findgen(15)+1, hi[*],psym=2, xtitle='Spectral parameter',xticklen=0.1,$
   ytitle='Cumulative IC',xticks=14,xtickformat='(I2)',yrange=[0.,1.0],xrange=[1,15],/nodata,xstyle=3
  ;plots,[0.7,15.3],[0,0],thick=2,linestyle=2
  oplot, ps, hmc[t,r,w,*],color=40,psym=-2,thick=10
  oplot, ps, hmc[t2,r,w,*],color=253,psym=-2,thick=10
  oplot, ps, hmc[t,r,w2,*],color=133,psym=-2,thick=10
  oplot, ps, hmc[t2,r,w2,*],color=210,psym=-2,thick=10
  oplot, ps, hmc[t2,r2,w2,*],color=254,psym=-2,thick=10
  oplot, ps, hmc[t3,r2,w2,*],color=235,psym=-2,thick=10

  oplot, pind[k]+1, hcl[*,ind[t,r,w]],color=40,psym=-2,thick=5, linestyle=2
  oplot, pind[k]+1, hcl[*,ind[t2,r,w]],color=253,psym=-2,thick=5, linestyle=2
  oplot, pind[k]+1, hcl[*,ind[t,r,w2]],color=133,psym=-2,thick=5,linestyle=2
  oplot, pind[k]+1, hcl[*,ind[t2,r,w2]],color=210,psym=-2,thick=5,linestyle=2
  oplot, pind[k]+1, hcl[*,ind[t2,r2,w2]],color=254,psym=-2,thick=5,linestyle=2
  oplot, pind[k]+1, hcl[*,ind[t3,r2,w2]],color=235,psym=-2,thick=5,linestyle=2

  legend, ['!9t!X=100, r!De!N=15 !9m!Xm liquid','!9t!X=20, r!De!N=15 !9m!Xm liquid',$
           '!9t!X=100, r!De!N=15 !9m!Xm ice','!9t!X=20, r!De!N=15 !9m!Xm ice',$
           '!9t!X=20, r!De!N=40 !9m!Xm ice','!9t!X=3, r!De!N=40 !9m!Xm ice'],$
           textcolors=[40,253,133,210,254,235],box=0,charsize=2.4, /bottom,/right ;position=[15.4,1.6]
  legend,['All parameters','Parameter subset'],linestyle=[0,2],thick=[10,5],pspacing=1.8,box=0,/right,$
           position=[11,0.27],charsize=2.5
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
 t=17 & r=5 & w=0 & t2=8 & w2=1 & r2=12 & t3=3
 print, 'making plot :'+fp
 set_plot, 'ps'
  loadct, 39, /silent
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
  device, xsize=34, ysize=20
   !p.font=1 & !p.thick=5 & !p.charsize=3.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
   !p.multi=[0,1,3] & !x.margin=[8,17] & !y.margin=[0.8,1.3] & !y.omargin=[3,0] & !p.symsize=5
 
  tvlct,225,90,90,253
  tvlct,90,225,90,133
  tvlct,90,90,225,40
  tvlct,100,100,100,254
  tvlct,240,115,235,235 ;purple
  usersym, [ -1, 1, 1, -1, -1 ,1], [ 1, 1, -1, -1, 1 ,1], /fill
 
  plot, p, ta[t,r,w,*],psym=2,xticklen=0.1,yrange=[0,100],ytickformat='(I6)',$
   ytitle='!9t!X!Cuncertainty range',xticks=15,xtickname=replicate(' ',16),xrange=[0,15],/nodata,xstyle=3
  oplot,p,ta_err[t,r,w,*,1]-ta_err[t,r,w,*,0],color=40,psym=-6,thick=10
  oplot,p,ta_err[t2,r,w,*,1]-ta_err[t2,r,w,*,0],color=253,psym=-6,thick=10
  oplot,p,ta_err[t,r,w2,*,1]-ta_err[t,r,w2,*,0],color=133,psym=-6,thick=10
  oplot,p,ta_err[t2,r,w2,*,1]-ta_err[t2,r,w2,*,0],color=210,psym=-6,thick=10
  oplot,p,ta_err[t2,r2,w2,*,1]-ta_err[t2,r2,w2,*,0],color=254,psym=-6,thick=10
  oplot,p,ta_err[t3,r2,w2,*,1]-ta_err[t3,r2,w2,*,0],color=235,psym=-6,thick=10

  plot, p, ta[t,r,w,*],psym=2,xticklen=0.1,yrange=[0,50],$
   ytitle='r!De!N!Cuncertainty range (!9m!Xm)',xticks=15,xtickname=replicate(' ',16),xrange=[0,15],/nodata,xstyle=3
  oplot,p,re_err[t,r,w,*,1]-re_err[t,r,w,*,0],color=40,psym=-6,thick=10
  oplot,p,re_err[t2,r,w,*,1]-re_err[t2,r,w,*,0],color=253,psym=-6,thick=10
  oplot,p,re_err[t,r,w2,*,1]-re_err[t,r,w2,*,0],color=133,psym=-6,thick=10
  oplot,p,re_err[t2,r,w2,*,1]-re_err[t2,r,w2,*,0],color=210,psym=-6,thick=10
  oplot,p,re_err[t2,r2,w2,*,1]-re_err[t2,r2,w2,*,0],color=254,psym=-6,thick=10
  oplot,p,re_err[t3,r2,w2,*,1]-re_err[t3,r2,w2,*,0],color=235,psym=-6,thick=10


xtit='!9h!X!D'+string(indgen(15)+1,format='(I2)')+'!N'
xtit=['prior',xtit]
 
  plot, p, ta[t,r,w,*],psym=2,xticklen=0.1,yrange=[-2,102],ytickformat='(I6)',$
   ytitle='Liquid cloud!Cprobability (%)',xticks=15,xrange=[0,15],/nodata,xstyle=3,xtickname=xtit
  oplot,p-0.25,probi[t,r,w,*],psym=-6,color=40,thick=10,linestyle=2
  oplot,p-0.15,probi[t2,r,w,*],psym=-6,color=253,thick=10,linestyle=2
  oplot,p-0.05,probi[t,r,w2,*],psym=-6,color=133,thick=10,linestyle=2
  oplot,p+0.05,probi[t2,r,w2,*],psym=-6,color=210,thick=10,linestyle=2
  oplot,p+0.15,probi[t2,r2,w2,*],psym=-6,color=254,thick=10,linestyle=2

  oplot,p-0.25,probl[t,r,w,*],psym=-5,color=40,thick=10
  oplot,p-0.15,probl[t2,r,w,*],psym=-5,color=253,thick=10
  oplot,p-0.05,probl[t,r,w2,*],psym=-5,color=133,thick=10
  oplot,p+0.05,probl[t2,r,w2,*],psym=-5,color=210,thick=10
  oplot,p+0.15,probl[t2,r2,w2,*],psym=-5,color=254,thick=10
  oplot,p+0.25,probl[t3,r2,w2,*],psym=-5,color=235,thick=10
  legend,['Liquid','Ice'],linestyle=[0,2],thick=[10,10],/right,box=0,color=[0,0],charsize=2.0,pspacing=1.4;,psym=[-5,-6],psymsize=[4,4]
  legend,[' ',' '],/right,box=0,color=[0,0],charsize=2.0,pspacing=1.4,psym=[5,6],position=[14.7,87],charthick=10
  axis,yaxis=1,ytitle='Ice cloud!Cprobability (%)',yr=[0,100]
 
  legend, ['!9t!X=100, r!De!N=15 !9m!Xm liquid','!9t!X=20, r!De!N=15 !9m!Xm liquid',$
           '!9t!X=100, r!De!N=15 !9m!Xm ice','!9t!X=20, r!De!N=15 !9m!Xm ice',$
           '!9t!X=20, r!De!N=40 !9m!Xm ice','!9t!X=3, r!De!N=40 !9m!Xm ice'],$
           textcolors=[40,253,133,210,254,235],box=0,charsize=2.0, position=[0.78,0.65],/normal
 
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
