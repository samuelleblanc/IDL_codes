; program to make the marginal and posterior pdf plot 
; uses model_limp_v2 retrieval run.

pro plot_post_pdf


;; load the file and set global variables
;restore the save file
win=0
if win then fn='C:\Users\Samuel\Research\SSFR3\data\retrieved_pdf_20120523_model_limp.out' else $
fn='/argus/roof/SSFR3/retrieved/retrieved_pdf_20120523_model_limp2_v2.out'
;fn='/home/leblanc/SSFR3/data/retrieved_pdf_20120523_model_limp_v2.out'
restore, fn

if win then dir='C:\Users\Samuel\Research\SSFR3\plots\' else $
dir='/home/leblanc/SSFR3/plots/'

ind=intarr(24,15,2)
for t=0,23 do for r=0,14 do for w=0,1 do ind[t,r,w]=360*w+t*15+r

tau_set=100. & ref_set=15. & wp_set=0
nul=min(abs(tau-tau_set),ti)
nul=min(abs(ref-ref_set),ri)
nul=min(abs(wp-wp_set),wi)

tas2=3. & res2=40. & ws2=1
nul=min(abs(tau-tas2),ti2)
nul=min(abs(ref-res2),ri2)
nul=min(abs(wp-ws2),wi2)

tas3=20. & res3=40. & ws3=1
nul=min(abs(tau-tas3),ti3)
nul=min(abs(ref-res3),ri3)
nul=min(abs(wp-ws3),wi3)


if 1 then begin
refo=ref
if not win then restore, dir+'../data/pars_lut_taus_ref.out' else $
restore, 'C:\Users\Samuel\Research\SSFR3\data\pars_lut_taus_ref.out'
refs=ref
ref=refo

csize=5.8
cc=255
cc1=255

fp=dir+'Post_model_pdf_v4'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=52, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=csize & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,3,1] & !x.margin=[6,4] & !y.margin=[3,3]

  mus='!9m!X'
  probs=(findgen(21)^3.)/(20^3.)*max(post[ind[[ti,ti2,ti3],[ri,ri2,ri3],[wi,wi2,wi3]],*,*,*])*1.01
  clrs=findgen(n_elements(probs))*254./(n_elements(probs)-1)
  clrs[0]=255
  contour, post[ind[ti,ri,wi],*,*,0],taus,refs,levels=probs,xtitle='Optical thickness',title='Low IC case - Liquid',$
   ytitle='Effective radius ('+mus+'m)',xrange=[50,130],yrange=[1,50],c_colors=clrs,/fill

  if wp_set eq 0 then plots, tau_set,ref_set,psym=4,color=0, symsize=4.2, thick=16
  if wp_set eq 0 then plots, tau_set,ref_set,psym=4,color=cc1, symsize=4.0, thick=10

  if wp_rtm[ind[ti,ri,wi]] eq 0 then plots, tau_rtm[ind[ti,ri,wi]],ref_rtm[ind[ti,ri,wi]],psym=7, color=0,symsize=4.2, thick=16
  if wp_rtm[ind[ti,ri,wi]] eq 0 then plots, tau_rtm[ind[ti,ri,wi]],ref_rtm[ind[ti,ri,wi]],psym=7, color=cc,symsize=4.0, thick=10

contour, post[ind[ti3,ri3,wi3],*,*,1],taus,refs,levels=probs,xtitle='Optical thickness',title='Mid IC case - Ice',$
   ytitle='Effective radius ('+mus+'m)',xrange=[10,30],yrange=[20,50],c_colors=clrs,/fill,xmargin=[2,6]

  if ws3 eq 1 then plots, tas3,res3,psym=4,color=0, symsize=4.2, thick=16
  if ws3 eq 1 then plots, tas3,res3,psym=4,color=cc1, symsize=4.0, thick=10

  if wp_rtm[ind[ti3,ri3,wi3]] eq 1 then plots, tau_rtm[ind[ti3,ri3,wi3]],ref_rtm[ind[ti3,ri3,wi3]],psym=7, color=0,symsize=4.2, thick=16
  if wp_rtm[ind[ti3,ri3,wi3]] eq 1 then plots, tau_rtm[ind[ti3,ri3,wi3]],ref_rtm[ind[ti3,ri3,wi3]],psym=7, color=cc,symsize=4.0, thick=10

 contour, post[ind[ti2,ri2,wi2],*,*,1],taus,refs,levels=probs,xtitle='Optical thickness',title='High IC case - Ice',$
   ytitle='Effective radius ('+mus+'m)',xrange=[1,6],yrange=[20,50],c_colors=clrs,/fill,xmargin=[0,10]

  if ws2 eq 1 then plots, tas2,res2,psym=4,color=0, symsize=4.2, thick=16
  if ws2 eq 1 then plots, tas2,res2,psym=4,color=cc1, symsize=4.0, thick=10

  if wp_rtm[ind[ti2,ri2,wi2]] eq 1 then plots, tau_rtm[ind[ti2,ri2,wi2]],ref_rtm[ind[ti2,ri2,wi2]],psym=7, color=0,symsize=4.2, thick=16
  if wp_rtm[ind[ti2,ri2,wi2]] eq 1 then plots, tau_rtm[ind[ti2,ri2,wi2]],ref_rtm[ind[ti2,ri2,wi2]],psym=7, color=cc,symsize=4.0, thick=10

bar=findgen(29)/2.+0.5 ;[0.5,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]
lvl=[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]
 contour,transpose([[probs],[probs]]),[0,1],findgen(n_elements(probs)),levels=probs,/cell_fill,ystyle=9,yticks=14,$
  ytickname=replicate(' ',15),position=[0.91,0.1,0.92,0.9],/normal,/noerase,xticks=1,xtickname=[' ',' '],c_colors=clrs
 axis,yaxis=1,ytitle='Probability',yrange=[0,1],yticks=5,ytickname=string(probs[0:*:(n_elements(probs)-1)/5],FORMAT='(F5.3)')
 ;,ytickformat='(F4.2)'

 device, /close
 spawn, 'convert "'+fp+'.ps" "'+fp+'.png"'
endif


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; plotting
;;; marginal pdf
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

if 1 then begin
refo=ref
if not win then restore, dir+'../data/pars_lut_taus_ref.out' else $
restore, 'C:\Users\Samuel\Research\SSFR3\data\pars_lut_taus_ref.out'
refs=ref
ref=refo

fp=dir+'Post_model_marginal_pdf_v4'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=52, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=csize & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,3,2] & !x.margin=[6,4] & !y.margin=[3.5,1]

  mus='!9m!X'
  probs=findgen(41)/40.
  p=reform(post[ind[ti,ri,wi],*,*,*])
  pref=total(p[*,*,0],1)+total(p[*,*,1],1)
  refns=where(pref ge max(pref)/100.*25.)
  ref_errf=[min(refs[refns]),max(refs[refns])]
  gfitr=gaussfit(refs,pref,coeffr,nterms=3)
  
  plot, refs,pref,xtitle='Effective radius ('+mus+'m)', ytitle='Probability density',title='Low IC case - Liquid',$
   ymargin=[2.5,2],xticklen=0.1,yticks=4,ytickformat='(F4.2)'

  tvlct,190,190,190,151
  polyfill,[ref_errf[0],ref_errf[1],ref_errf[1],ref_errf[0]],[0,0,max(pref),max(pref)],color=151
  oplot,refs,gfitr,color=250
  plots, [coeffr[1]+coeffr[2],coeffr[1]+coeffr[2]],[0,max(pref)],color=250,linestyle=2
  plots, [coeffr[1]-coeffr[2],coeffr[1]-coeffr[2]],[0,max(pref)],color=250,linestyle=2
  oplot,refs,pref ,thick=7
  oplot,refs,gfitr,color=250,thick=3
;  legend,['Uncertainty range','Fitted gaussian','Standard deviation'],color=[151,250,250],linestyle=[0,0,2],$
;   thick=[30,5,5],pspacing=1.6,/right,box=0,charsize=2.0

  p3=reform(post[ind[ti3,ri3,wi3],*,*,*])
  pref3=total(p3[*,*,0],1)+total(p3[*,*,1],1)
  refns3=where(pref3 ge max(pref3)/100.*25.)
  ref_errf3=[min(refs[refns3]),max(refs[refns3])]
  gfitr3=gaussfit(refs,pref3,coeffr3,nterms=3)
  plot, refs,pref3,xtitle='Effective radius ('+mus+'m)', ytitle='Probability density',title='Mid IC case - Ice',ymargin=[2.5,2],xticklen=0.1
  tvlct,190,190,190,151
  polyfill,[ref_errf3[0],ref_errf3[1],ref_errf3[1],ref_errf3[0]],[0,0,max(pref3),max(pref3)],color=151
  oplot,refs,gfitr3,color=250
  plots, [coeffr3[1]+coeffr3[2],coeffr3[1]+coeffr3[2]],[0,max(pref3)],color=250,linestyle=2
  plots, [coeffr3[1]-coeffr3[2],coeffr3[1]-coeffr3[2]],[0,max(pref3)],color=250,linestyle=2
  oplot,refs,pref3,thick=7
  oplot,refs,gfitr3,color=250,thick=3

  p2=reform(post[ind[ti2,ri2,wi2],*,*,*])
  pref2=total(p2[*,*,0],1)+total(p2[*,*,1],1)
  refns2=where(pref2 ge max(pref2)/100.*25.)
  ref_errf2=[min(refs[refns2]),max(refs[refns2])]
  gfitr2=gaussfit(refs,pref2,coeffr2,nterms=3)
  plot, refs,pref2,xtitle='Effective radius ('+mus+'m)', ytitle='Probability density',title='High IC case - Ice',ymargin=[2.5,2],xticklen=0.1
  tvlct,190,190,190,151
  polyfill,[ref_errf2[0],ref_errf2[1],ref_errf2[1],ref_errf2[0]],[0,0,max(pref2),max(pref2)],color=151
  oplot,refs,gfitr2,color=250
  plots, [coeffr2[1]+coeffr2[2],coeffr2[1]+coeffr2[2]],[0,max(pref2)],color=250,linestyle=2
  plots, [coeffr2[1]-coeffr2[2],coeffr2[1]-coeffr2[2]],[0,max(pref2)],color=250,linestyle=2
  oplot,refs,pref2,thick=7
  oplot,refs,gfitr2,color=250,thick=3

  legend,['Uncertainty range','Fitted gaussian','Standard deviation'],color=[151,250,250],linestyle=[0,0,2],$
   thick=[30,5,5],pspacing=1.6,box=0,charsize=1.8


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
  
  ptau3=total(p3[*,*,0],2)+total(p3[*,*,1],2)
  tauns3=where(ptau3 ge max(ptau3)/100.*25.)
  tau_errf3=[min(taus[tauns3])-0.35,max(taus[tauns3])+0.35]
  plot, taus, ptau3, xtitle='Optical thickness',ytitle='Probability density',ymargin=[3,1.5],xrange=[10,30],xticklen=0.1;,xrange=[70,130]
  polyfill,[tau_errf3[0],tau_errf3[1],tau_errf3[1],tau_errf3[0]],[0,0,max(ptau3),max(ptau3)],color=151
  gfitt3=gaussfit(taus,ptau3,coefft3,nterm=3)
  oplot,taus,gfitt3,color=250
  plots, [coefft3[1]+coefft3[2],coefft3[1]+coefft3[2]],[0,max(ptau3)],color=250,linestyle=2
  plots, [coefft3[1]-coefft3[2],coefft3[1]-coefft3[2]],[0,max(ptau3)],color=250,linestyle=2
  oplot,taus,ptau3,thick=7
  oplot,taus,gfitt3,color=250,thick=3

  ptau2=fltarr(n_elements(taus))
  ptau2=total(p2[*,*,0],2)+total(p2[*,*,1],2)
  ;for t=0, n_elements(taus)-1 do ptau2[t]=int_tabulated(refs,p2[0,t,*,0])+int_tabulated(refs,p2[0,t,*,1])
  tauns2=where(ptau2 ge max(ptau2)/100.*25.)
  tau_errf2=[min(taus[tauns2])-0.35,max(taus[tauns2])+0.35]
  ptau2[3]=ptau2[3]*4000.
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
end
