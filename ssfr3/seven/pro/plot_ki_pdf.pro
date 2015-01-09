; program to plot the comparison between the retrieved values of kisq and pdf

pro plot_ki_pdf,datein
win=0

dates=['20120523',$
       '20120525',$
       '20120602',$
       '20120806',$
       '20120813',$
       '20120816',$
       '20120820',$
       '20120824',$
       '20120912',$
       '20130110',$
       '20130111']


;if n_elements(date) lt 1 then date='20120523'
if win then dir='C:\Users\Samuel\Research\SSFR3\plots\new\' else $
  dir='~/SSFR3/plots/kisq/'

if win then indir='C:\Users\Samuel\Research\SSFR3\retrieved\' else $
  indir='~/SSFR3/data/'

if win then pdfdir=indir else pdfdir='/argus/roof/SSFR3/retrieved/'



tap=0. & rep=0. & wpp=0.
tak=0. & rek=0. & wpk=0.

for i=0, n_elements(dates)-1 do begin
 if n_elements(datein) gt 0 then begin
   i=n_elements(dates)-1 
   date=datein
 endif else date=dates[i] 

restore, pdfdir+'retrieved_pdf_'+date+'_limp2_v4.out'
p=where(tau_rtm lt 98. and tau_rtm gt 1. and ref_rtm gt 1. and ref_rtm lt 48.)
tp=tau_rtm[p] & rp=ref_rtm[p] & wp=wp_rtm[p] & tpr=tau_err[*,p] & rpr=ref_err[*,p] & wpr=wp_err[p] & tm=tmhrs
restore, indir+'retrieved_kisq_'+date+'_v4.out'
n=where(tau_rtm gt 0.) ;where(tau_rtm lt 98. and tau_rtm gt 1. and ref_rtm gt 1. and ref_rtm lt 48.)
tk=tau_rtm[n] & rk=ref_rtm[n] & wk=wp_rtm[n] & tkr=tau_err[n,*] & rkr=ref_err[n,*] & wkr=wp_err[n] & kir=ki_rtm[n] & hr=tmhrs
restore, indir+'retrieved_kisq_'+date+'_v2.out'
n2=where(tau_rtm lt 98. and tau_rtm gt 1. and ref_rtm gt 1. and ref_rtm lt 48.)
tk2=tau_rtm[n2] & rk2=ref_rtm[n2] & wk2=wp_rtm[n2] & tkr2=tau_err[n2,*] & rkr2=ref_err[n2,*] & wkr2=wp_err[n2] & kir2=ki_rtm[n2]
restore, indir+'retrieved_kisq_'+date+'_v3.out'
n3=where(tau_rtm lt 98. and tau_rtm gt 1. and ref_rtm gt 1. and ref_rtm lt 48.)
tk3=tau_rtm[n3] & rk3=ref_rtm[n3] & wk3=wp_rtm[n3] & tkr3=tau_err[n3,*] & rkr3=ref_err[n3,*] & wkr3=wp_err[n3] & kir3=ki_rtm[n3]

 fp=dir+'ki_pdf_'+date
 print, 'making plot :'+fp
 set_plot, 'ps'
  loadct, 39, /silent
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
  device, xsize=20, ysize=30
   !p.font=1 & !p.thick=5 & !p.charsize=2.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
   !p.multi=[0,1,4] & !x.margin=[6,1] & !y.margin=[1,1] & !y.omargin=[3,0]

  plot, tm[p], tp, ytitle='Optical thickness',yr=[0,100],psym=7
  oplot,hr[n], tk, psym=7,color=250
;  oplot,tm[n2], tk2, psym=7,color=70
 ; errplot,tmhrs[p],tpr[0,*],tpr[1,*]
 ; errplot,tmhrs[n],tkr[*,0],tkr[*,1],color=250
 ; errplot,tmhrs[n2],tkr2[*,0],tkr2[*,1],color=70
  oplot,tm[p],tp,psym=7,symsize=2
  oplot,hr[n], tk, psym=7,color=250
;  oplot,tm[n2], tk2, psym=7,color=70
;  oplot,tm[n3], tk3,psym=7,color=150

  
  plot, tm[p], rp,ytitle='Effective radius (!9m!Xm)',yr=[0,50],psym=7 ,symsize=2
  oplot,hr[n], rk, psym=7,color=250
;  oplot,tm[n2], rk2, psym=7,color=70
 ; errplot,tmhrs[p],rpr[0,*],rpr[1,*]
 ; errplot,tmhrs[n],rkr[*,0],rkr[*,1],color=250
 ; errplot,tmhrs[n2],rkr2[*,0],rkr2[*,1],color=70
  oplot,tm[p],rp,psym=7
  oplot,hr[n], rk, psym=7,color=250 
;  oplot,tm[n2], rk2, psym=7,color=70
;  oplot,tm[n3],rk3,psym=7,color=150


  plot, tm[p],wp, ytitle='Phase',yr=[-0.2,1.2],yticks=1,ytickname=['Liquid','Ice'],ytickv=[0,1],psym=7,symsize=2
  oplot,hr[n],wk,psym=7,color=250
;  oplot,tm[n2],wk2,psym=7,color=70
;  oplot,tm[n3],wk3,psym=7,color=150


 ; plot, tmhrs[p], wpr, xtitle='UTC (h)',ytitle='Ice probability',yr=[0,1],psym=-6 
 ; oplot,tmhrs[n], wkr, psym=-4,color=250
   yr=[min([kir,kir2]),max([kir,kir2])]
   plot,hr[n],kir,psym=7,/nodata,ytitle='!9c!X!U2!N residual',xtitle='UTC (h)',yr=yr
   oplot,hr[n],kir,psym=7,color=250
;   oplot,tm[n2],kir2,psym=7,color=70
;   oplot,tm[n3],kir3,psym=7,color=150

; legend,['GENRA','!9c!X!U2!N - 2 step','!9c!X!U2!N - max value','!9c!X!U2!N - even values'],textcolors=[0,250,70,150],box=0,/right,charsize=1.8
legend,['GENRA','!9c!X!U2!N - 2 step'],textcolors=[0,250],box=0,/right,charsize=1.8

  device, /close
  spawn, 'convert '+fp+'.ps '+fp+'.png'

  for x=0, n_elements(p)-1 do begin
    ux=where(n eq p[x],u)
    if u gt 0 then begin
      tap=[tap,tp[x]] & rep=[rep,rp[x]] & wpp=[wpp,wp[x]]
      tak=[tak,tk[u]] & rek=[rek,rk[u]] & wpk=[wpk,wk[u]]
    endif
  endfor

endfor

 fp=dir+'ki_pdf_scatter'
 print, 'making plot :'+fp 
 set_plot, 'ps' 
  loadct, 39, /silent   
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
  device, xsize=40, ysize=20 
   !p.font=1 & !p.thick=5 & !p.charsize=2.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
   !p.multi=[0,3,1] & !x.margin=[6,1] & !y.margin=[3,1] & !y.omargin=[0,0]

   plot, tap, tak , psym=7, xtitle='pdf optical thickness',ytitle='!9c!X!U2!N optical thickness',yr=[0,100],xr=[0,100]
   tl=linfit(tap,tak)
   oplot,tap,tap*tl[1]+tl[0],linestyle=2
   xyouts, 25,65,'r!U2!N='+string(correlate(tap,tak))

   plot, rep, rek , psym=7, xtitle='pdf effective radius (!9m!Xm)',ytitle='!9c!X!U2!N effective radius (!9m!Xm)',xr=[0,50],yr=[0,50]
   rl=linfit(rep,rek)
   oplot,rep,rep*rl[1]+rl[0],linestyle=2
   xyouts,15,40,'r!U2!N='+string(correlate(rep,rek))

   plot, wpp, wpk , psym=7, xtitle='pdf phase',ytitle='!9c!X!U2!N phase',xr=[0,1],yr=[0,1]
  
  device, /close
  spawn, 'convert '+fp+'.ps '+fp+'.png'

stop
end
