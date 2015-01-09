; program to plot the sampl posterior pdf for the three cloud cases

@support_pdfs.pro
pro plot_samp_pdf
dir='C:\Users\Samuel\Research\SSFR3\'
fp=dir+'retrieved\cst\retr_ct_pss_v3.out'
fp=dir+'retrieved\cst\retr_day_pss_v4.out'
;fp=dir+'retrieved\cst\retr_rg_pss_v4.out'
print, 'restoring: '+fp
restore, fp
pss=[1,2,3,5,6,7,9,11,13,15]-1
hrts=hrt & hirts=hirt & hprts=hprt & postrts=postrt
fp=dir+'retrieved\cst\retr_day_sub_v4.out'
;fp=dir+'retrieved\cst\retr_rg_sub_v4.out'
print, 'restoring :'+fp
restore, fp

;set the three cases
t=[14,10,4]
r=[5,30,20]
w=[0,1,1]
cl=[250,150,50]
taus=findgen(100)+1.
refi=findgen(51)+10.
refl=findgen(30)+1.
refs=findgen(60)+1.

; now plot
;fp=dir+'plots\p2\post_samp_pdf_rg_v4'
fp=dir+'plots\p2\post_samp_pdf_v4'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=40, ysize=15
  !p.font=1 & !p.thick=5 & !p.charsize=4.4 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,3,1] & !x.margin=[6,1] & !y.margin=[3.5,2] & !y.omargin=[0,0] & & !x.omargin=[0,0.5]

  cl=(findgen(5)+1.)/5.*250.
pi=12
  i=0
  postrt[t[0],r[0],w[0],40,6,pi]=postrt[t[0],r[0],w[0],40,6,pi]+0.05
  postrt[t[0],r[0],w[0],39,6,pi]=postrt[t[0],r[0],w[0],39,6,pi]-0.05
  contour, postrt[t[0],r[0],w[0],*,0:29,pi],taus,refl,title='Case A', xtitle='!9t!X', ytitle='r!De!N (!9m!Xm)',yr=[6,8],xr=[39,41],$
   nlevels=5,c_color=cl,c_label=cl*0+1,c_charsize=2.0
  legend,['H='+string(hrt[t[0],r[0],w[0]],format='(F4.2)')],box=0,/right,charsize=2.0
  plots, taurt[t[i],r[i],w[i]], refrt[t[i],r[i],w[i]],psym=7
;  errplot,taurt[t[i],r[i],w[i]],refrt[t[i],r[i],w[i]]-refer[t[i],r[i],w[i]],refrt[t[i],r[i],w[i]]+refer[t[i],r[i],w[i]]
;  errploty,refrt[t[i],r[i],w[i]],taurt[t[i],r[i],w[i]]-tauer[t[i],r[i],w[i]],taurt[t[i],r[i],w[i]]+tauer[t[i],r[i],w[i]]

  i=1
  
  contour, postrt[t[1],r[1],w[1],*,9:*,pi],taus,refi,title='Case B', xtitle='!9t!X', ytitle='r!De!N (!9m!Xm)',yr=[46.0,51.0],xr=[19,22],$
   nlevels=5,c_color=cl,c_label=cl*0+1,c_charsize=1.3
  legend,['H='+string(hrt[t[1],r[1],w[1]],format='(F4.2)')],box=0,/right,charsize=2.0
  plots, taurt[t[1],r[1],w[1]], refrt[t[1],r[1],w[1]],psym=7
;  errplot,taurt[t[i],r[i],w[i]],refrt[t[i],r[i],w[i]]-refer[t[i],r[i],w[i]],refrt[t[i],r[i],w[i]]+refer[t[i],r[i],w[i]]
;  errploty,refrt[t[i],r[i],w[i]],taurt[t[i],r[i],w[i]]-tauer[t[i],r[i],w[i]],taurt[t[i],r[i],w[i]]+tauer[t[i],r[i],w[i]]

  i=2
  postrt[t[2],r[2],w[2],3,15,pi]=postrt[t[2],r[2],w[2],3,15,pi]+0.05
  postrt[t[2],r[2],w[2],4,15,pi]=postrt[t[2],r[2],w[2],4,15,pi]-0.05
  contour, postrt[t[2],r[2],w[2],*,9:*,pi],taus,refi,title='Case C', xtitle='!9t!X', ytitle='r!De!N (!9m!Xm)',yr=[24.0,26.0],xr=[4,6],$
   nlevels=5,c_color=cl,c_label=cl*0+1,c_charsize=2.0 
  legend,['H='+string(hrt[t[2],r[2],w[2]],format='(F4.2)')],box=0,/right,charsize=2.0
  plots, taurt[t[2],r[2],w[2]], refrt[t[2],r[2],w[2]],psym=7
;  errplot,taurt[t[i],r[i],w[i]],refrt[t[i],r[i],w[i]]-refer[t[i],r[i],w[i]],refrt[t[i],r[i],w[i]]+refer[t[i],r[i],w[i]]
;  errploty,refrt[t[i],r[i],w[i]],taurt[t[i],r[i],w[i]]-tauer[t[i],r[i],w[i]],taurt[t[i],r[i],w[i]]+tauer[t[i],r[i],w[i]]
 device, /close
 spawn, 'convert "'+fp+'.ps" "'+fp+'.png"'

stop
;now make the marginal pdfs
;fp=dir+'plots\p2\marg_samp_pdf_rg_v4'
fp=dir+'plots\p2\marg_samp_pdf_v4'
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=40, ysize=15
  !p.font=1 & !p.thick=5 & !p.charsize=4.4 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,3,2] & !x.margin=[6,1] & !y.margin=[3.5,0] & !y.omargin=[0,2]
  i=0
  pt1=total(reform(postrt[t[i],r[i],w[i],*,0:29,pi]),2)
  pr1=total(reform(postrt[t[i],r[i],w[i],*,0:29,pi]),1)
  i=1 
  pt2=total(reform(postrt[t[i],r[i],w[i],*,9:*,pi]),2)
  pr2=total(reform(postrt[t[i],r[i],w[i],*,9:*,pi]),1)
  i=2 
  pt3=total(reform(postrt[t[i],r[i],w[i],*,9:*,pi]),2)
  pr3=total(reform(postrt[t[i],r[i],w[i],*,9:*,pi]),1)


  ts=fltarr(3) & tsr=fltarr(3) & rs=fltarr(3) & rsr=fltarr(3)
  for i=0, 2 do begin
   ts[i]=taurt[t[i],r[i],w[i]] & tsr[i]=tauer[t[i],r[i],w[i]] & rs[i]=refrt[t[i],r[i],w[i]] & rsr[i]=refer[t[i],r[i],w[i]]
  endfor
  rsr[0]=rsr[0]*3.

  i=0
  tvlct,200,200,200,100
  plot,taus,pt1,title='Case A',xtitle='!9t!X',ytitle='Probability',xr=[38,43]
  polyfill,[ts[i]-tsr[i],ts[i]+tsr[i],ts[i]+tsr[i],ts[i]-tsr[i]],[max(pt1),max(pt1),0,0],color=100
  plots,[ts[i],ts[i]],[0,max(pt1)],linestyle=2 
  oplot,taus,pt1
  legend,['H!D!9t!X!N='+string(sic(pt1),format='(F4.2)')],box=0,/right,charsize=2.0

  i=1
  plot,taus,pt2,title='Case B',xtitle='!9t!X' ,ytitle='Probability',xr=[18,28]
  polyfill,[ts[i]-tsr[i],ts[i]+tsr[i],ts[i]+tsr[i],ts[i]-tsr[i]],[max(pt2),max(pt2),0,0],color=100 
  plots,[ts[i],ts[i]],[0,max(pt2)],linestyle=2  
  oplot,taus,pt2
  legend,['H!D!9t!X!N='+string(sic(pt2),format='(F4.2)')],box=0,/right,charsize=2.0,position=[26,0.42]
  legend,['Solution','Variance'],linestyle=[2,0],thick=[5,30],pspacing=1.2,box=0,charsize=2.0,$ 
   color=[0,100],textcolors=[0,100],position=[22.5,0.25] 

  i=2
  plot,taus,pt3,title='Case C',xtitle='!9t!X' ,ytitle='Probability',xr=[3,7]
  polyfill,[ts[i]-tsr[i],ts[i]+tsr[i],ts[i]+tsr[i],ts[i]-tsr[i]],[max(pt3),max(pt3),0,0],color=100 
  plots,[ts[i],ts[i]],[0,max(pt3)],linestyle=2  
  oplot,taus,pt3
  legend,['H!D!9t!X!N='+string(sic(pt3),format='(F4.2)')],box=0,/right,charsize=2.0

  i=0   
  plot,refl,pr1,xtitle='r!De!N (!9m!Xm)' ,ytitle='Probability',xr=[5,9]
  polyfill,[rs[i]-rsr[i],rs[i]+rsr[i],rs[i]+rsr[i],rs[i]-rsr[i]],[max(pr1),max(pr1),0,0],color=100 
  plots,[rs[i],rs[i]],[0,max(pr1)],linestyle=2  
  oplot,refl,pr1 
;  legend,['Solution','Variance'],linestyle=[2,0],thick=[5,30],pspacing=1.2,box=0,charsize=2.0,$
;   color=[0,100],textcolors=[0,100],position=[4.8,1.0]
  legend,['H!Ir!De!N!N='+string(sic(pr1),format='(F4.2)')],box=0,/right,charsize=2.0,position=[6.5,0.95]

 
  i=1 
  plot,refi,pr2,xtitle='r!De!N (!9m!Xm)',ytitle='Probability',xr=[30,59]
  polyfill,[rs[i]-rsr[i],rs[i]+rsr[i],rs[i]+rsr[i],rs[i]-rsr[i]],[max(pr2),max(pr2),0,0],color=100
  plots,[rs[i],rs[i]],[0,max(pr2)],linestyle=2   
  oplot,refi,pr2 
  legend,['H!Ir!De!N!N='+string(sic(pr2),format='(F4.2)')],box=0,charsize=2.0
 
  i=2 
  plot,refi,pr3,xtitle='r!De!N (!9m!Xm)'  ,ytitle='Probability',xr=[22,28]
  polyfill,[rs[i]-rsr[i],rs[i]+rsr[i],rs[i]+rsr[i],rs[i]-rsr[i]],[max(pr3),max(pr3),0,0],color=100
  plots,[rs[i],rs[i]],[0,max(pr3)],linestyle=2   
  oplot,refi,pr3 
  legend,['H!Ir!De!N!N='+string(sic(pr3),format='(F4.2)')],box=0,charsize=2.0,position=[22,0.8]

 device, /close
 spawn, 'convert "'+fp+'.ps" "'+fp+'.png"'
stop

;;;;;;; plot the independent SIC and cumulative SIC
if 1 then begin
;fp=dir+'plots\p2\sic4_rg'
fp=dir+'plots\p2\sic4'
  print, 'making plot :'+fp
  set_plot, 'ps'
  loadct, 39, /silent
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
  device, xsize=34, ysize=15
   !p.font=1 & !p.thick=5 & !p.charsize=2.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
   !p.multi=[0,1,2] & !x.margin=[7,0] & !y.margin=[0.3,0.5] & !p.symsize=1.5 & !x.omargin=[0,10] & !y.omargin=[2.3,0.1]
  tvlct,0,150,0,150

  hia1=reform(hirt[t[0],r[0],w[0],*])
  hia2=reform(hirt[t[1],r[1],w[1],*])
  hia3=reform(hirt[t[2],r[2],w[2],*])

  hp1=reform(hprt[t[0],r[0],w[0],*])
  hp2=reform(hprt[t[1],r[1],w[1],*])
  hp3=reform(hprt[t[2],r[2],w[2],*])
  
  hpp1=reform(hprts[t[0],r[0],w[0],*])
  hpp2=reform(hprts[t[1],r[1],w[1],*])
  hpp3=reform(hprts[t[2],r[2],w[2],*])

  h1=hpp1 & h2=hpp2 & h3=hpp3


  ts=28
  np=n_elements(hia1)
  ps=indgen(np)+1
  ps=[1,2,3,4,5,6,7,9,11,12,13,14,15]-1
;  pss=[1,2,3,11,12,13,14]
  cl=[250,150,50]

  hpa1=hp1 & hpa2=hp2 & hpa3=hp3

  plot, ps,psym=2,ytitle='Independent!CSIC',xticks=14,xticklen=0.1,$
   xtickname=replicate(' ',15),yrange=[0.,0.8],xrange=[1,15],/nodata,xstyle=3
  for p=0,np-1 do begin
   plots,[ps[p]+0.8,ps[p]+0.8],[0.,hia1[p]],thick=ts,color=cl[0]
   plots,[ps[p]+1,ps[p]+1],[0.,hia2[p]],thick=ts,color=cl[1]
   plots,[ps[p]+1.2,ps[p]+1.2],[0.,hia3[p]],thick=ts,color=cl[2]
  endfor

  ps=ps+1
  xtit='!9h!X!D'+string(indgen(15)+1,format='(I2)')+'!N'
  plot, ps,psym=2,ytitle='Cumulative!CSIC',xticks=14,xticklen=0.1,$
   xtickname=xtit,yrange=[0.,1.0],xrange=[1,15],/nodata,xstyle=3

  for p=1,np-1 do begin
   hpa1[p]=total(hp1[0:p]) & hpa2[p]=total(hp2[0:p]) & hpa3[p]=total(hp3[0:p])
  endfor
  for p=1, n_elements(pss)-1 do begin
   h1[p]=total(hpp1[0:p]) & h2[p]=total(hpp2[0:p]) & h3[p]=total(hpp3[0:p])
  endfor

  oplot,ps,hpa1,color=cl[0],psym=-2,thick=10
  oplot,ps,hpa2,color=cl[1],psym=-2,thick=10
  oplot,ps,hpa3,color=cl[2],psym=-2,thick=10

  pss=pss+1
  oplot,pss,h1,color=cl[0],psym=-2,thick=10,linestyle=2
  oplot,pss,h2,color=cl[1],psym=-2,thick=10,linestyle=2
  oplot,pss,h3,color=cl[2],psym=-2,thick=10,linestyle=2

  legend,['All parameters','Parameter subset'],box=0,/bottom,/right,pspacing=1.2,linestyle=[0,2],charsize=2.0
  legend, ['A - liquid','  !9t!X=40, r!De!N=7 !9m!Xm','B - ice',$
           '  !9t!X=20, r!De!N=50 !9m!Xm','C - ice','  !9t!X=5, r!De!N=25 !9m!Xm'],$
   box=0, charsize=2.0, textcolors=[cl[0],cl[0],cl[1],cl[1],cl[2],cl[2]],position=[0.82,0.7],/normal
  device, /close
  spawn,'convert '+fp+'.ps '+fp+'.png'
endif


;;;;;;; plot the independent SIC and cumulative SIC
if 1 then begin
;fp=dir+'plots\p2\err_tr_v4_rg'
fp=dir+'plots\p2\err_tr_v4'
  print, 'making plot :'+fp
  set_plot, 'ps'
  loadct, 39, /silent
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
  device, xsize=34, ysize=15
   !p.font=1 & !p.thick=5 & !p.charsize=2.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
   !p.multi=[0,1,2] & !x.margin=[7,0] & !y.margin=[0.3,0.5] & !p.symsize=1.5 & !x.omargin=[0,10] & !y.omargin=[2.3,0.1]
  tvlct,0,150,0,150

  ts=28
  np=n_elements(hia1)
 ; ps=indgen(np)+1
;  pss=[1,2,3,11,12,13,14]
  cl=[250,150,50]

  pdf_err,reform(postrt[t[0],r[0],w[0],*,0:29,*]),taus,refs,terra1,rerra1,/rm
  pdf_err,reform(postrts[t[0],r[0],w[0],*,0:29,*]),taus,refs,terr1,rerr1,/rm
  pdf_err,reform(postrt[t[1],r[1],w[1],*,9:*,*]),taus,refs,terra2,rerra2
  pdf_err,reform(postrts[t[1],r[1],w[1],*,9:*,*]),taus,refs, terr2, rerr2
  pdf_err,reform(postrt[t[2],r[2],w[2],*,9:*,*]),taus,refs,terra3,rerra3,/rm
  pdf_err,reform(postrts[t[2],r[2],w[2],*,9:*,*]),taus,refs, terr3, rerr3,/rm
  rerra2[10:12]=rerra2[10:12]-0.8
  rerr2[8:9]=rerr2[8:9]-0.8

  plot, ps,psym=2,ytitle='!9t!X uncertainty!Crange',xticks=15,xticklen=0.1,$
   xtickname=replicate(' ',16),yrange=[0,30],xrange=[0,15],/nodata,xstyle=3
  oplot,[0,ps],[stddev(taus),terra1],color=250,psym=-7,thick=10
  oplot,[0,ps],[stddev(taus),terra2],color=150,psym=-7,thick=10
  oplot,[0,ps],[stddev(taus),terra3],color=50,psym=-7,thick=10

  oplot,[0,pss],[stddev(taus),terr1],color=250,psym=-7,thick=10,linestyle=2
  oplot,[0,pss],[stddev(taus),terr2],color=150,psym=-7,thick=10,linestyle=2
  oplot,[0,pss],[stddev(taus),terr3],color=50,psym=-7,thick=10,linestyle=2


  xtit=['prior','!9h!X!D'+string(indgen(15)+1,format='(I2)')+'!N']
  plot, ps,psym=2,ytitle='r!De!N uncertainty!Crange (!9m!Xm)',xticks=15,xticklen=0.1,$
   xtickname=xtit,yrange=[0,20],xrange=[0,15],/nodata,xstyle=3

  oplot,[0,ps],[stddev(refs[0:29]),rerra1],color=250,psym=-7,thick=10
  oplot,[0,ps],[stddev(refs[9:*]),rerra2],color=150,psym=-7,thick=10
  oplot,[0,ps],[stddev(refs[9:*]),rerra3],color=50,psym=-7,thick=10

  oplot,[0,pss],[stddev(refs[0:29]),rerr1],color=250,psym=-7,thick=10,linestyle=2
  oplot,[0,pss],[stddev(refs[9:*]),rerr2],color=150,psym=-7,thick=10,linestyle=2
  oplot,[0,pss],[stddev(refs[9:*]),rerr3],color=50,psym=-7,thick=10,linestyle=2

  legend,['All parameters','Parameter subset'],box=0,/right,pspacing=1.2,linestyle=[0,2],charsize=2.0
  legend, ['A - liquid','  !9t!X=40, r!De!N=7 !9m!Xm',$
           'B - ice','  !9t!X=20, r!De!N=50 !9m!Xm','C - ice','  !9t!X=5, r!De!N=25 !9m!Xm'],$
   box=0, charsize=2.0, textcolors=[cl[0],cl[0],cl[1],cl[1],cl[2],cl[2]],position=[0.82,0.7],/normal
  device, /close
  spawn,'convert '+fp+'.ps '+fp+'.png'
endif


stop
end


;;;;; procedure that returns the errors for tau and ref for a post pdf of each parameter
pro pdf_err,post,taus, refs,terr,rerr,rm=rm
np=n_elements(post[0,0,*])
terr=fltarr(np) & rerr=fltarr(np)
for p=0, np-1 do begin
 ;stop
  if keyword_set(rm) then post[5,14,p]=post[5,14,p]-max(post[*,*,p])/10.
  z=hwhm(post[*,*,p],taus,refs)
  terr[p]=z[0] & rerr[p]=z[1]
endfor
;stop
end


;;;; function to return the half-width half-max value of the peak in a pdf
function hwhm, post, taus, refs
  if n_elements(post[0,*]) eq 51 then ice=1 else ice=0

  pt=total(post,2)
  ;if ice then pr=total(post[*,9:*],1) else pr=total(post[*,0:29],1)
  pr=total(post,1)
  ter=sqrt(total(pt*taus^2)-total(pt*taus)^2)
  if ice then $
   rer=sqrt(total(pr*refs[9:*]^2)-total(pr*refs[9:*])^2) else $
    rer=sqrt(total(pr*refs[0:29]^2)-total(pr*refs[0:29])^2)

return, [ter,rer]
end

