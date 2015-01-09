; program to plot the pdf of the retrieved values for three combinations of tau and ref
; input of posttm post_pdftm from the retrieve_pdfs_1 value stopped in themiddle of retrieval


pro plot_sic_pdf
dir='C:\Users\Samuel\Research\SSFR3\'


print, 'restoring ice water case, tau=5, ref=25 um'
;restore, '~/SSFR3/data/meas_pdf_liq.out'
restore,dir+'data\v1\post_pdf_t5_r25_i_all.out'
hia1=h_ind & hpa1=cumul(h_par) & ppdfa1=post_pdftm
restore,dir+'data\v1\post_pdf_t5_r25_i.out'
post1=posttm & hi1=h_ind & hp1=cumul(h_par) & ppdf1=post_pdftm
t1=5. & r1=25.
pdf_stat,post1,taus,refs,tt1,rr1,ptau1,pref1

restore,dir+'data\v1\post_pdf_t40_r7_l_all.out'
hia2=h_ind & hpa2=cumul(h_par) & ppdfa2=post_pdftm
restore,dir+'data\v1\post_pdf_t40_r7_l.out'
post2=posttm & hi2=h_ind & hp2=cumul(h_par) & ppdf2=post_pdftm
t2=40. & r2=7.
pdf_stat,post2,taus,refs,tt2,rr2,ptau2,pref2

restore,dir+'data\v1\post_pdf_t20_r50_i_all.out'
hia3=h_ind & hpa3=cumul(h_par) & ppdfa3=post_pdftm
restore,dir+'data\v1\post_pdf_t20_r50_i.out'
post3=posttm & hi3=h_ind & hp3=cumul(h_par) & ppdf3=post_pdftm
t3=20. & r3=50.
pdf_stat,post3,taus,refs,tt3,rr3,ptau3,pref3

if 1 then begin
fp=dir+'plots\p2\post_pdf2'
  print, 'making plot :'+fp
  set_plot, 'ps'
  loadct, 39, /silent
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
  device, xsize=38, ysize=14
   !p.font=1 & !p.thick=5 & !p.charsize=4.2 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
   !p.multi=[0,3,1] & !x.margin=[6,0] & !y.margin=[4,1] & !p.symsize=1.5 & !x.omargin=[0,7]

  nl=8
  lvls=findgen(nl)/(nl-1.)
  cls=lvls*250.
  lvls=[0.025,0.04,0.06,0.12,0.2,0.4,0.5,0.65]
  cc1=255 & cc=255

  contour, post1, taus, refs[9:*],levels=lvls,c_colors=cls,xtitle='!9t!X',ytitle='r!De!N (!9m!Xm)', xr=[3,7],yr=[23,28];,$
  pdf_err,post1,taus, refs,terr1,rerr1 
;   c_charsize=2.0,c_labels=[1,0,0,1,0,0,1]
;  plots, t1,r1,psym=4,color=0, symsize=4.2, thick=16
;  plots, t1,r1,psym=4,color=cc1, symsize=4.0, thick=10

;  plots, tt1,rr1,psym=7, color=0,symsize=4.2, thick=16
;  plots, tt1,rr1,psym=7, color=cc,symsize=4.0, thick=10


  contour, post2, taus, refs[0:29],levels=lvls,c_colors=cls,xtitle='!9t!X',ytitle='r!De!N (!9m!Xm)', xr=[37,46],yr=[4,8];,$
;   c_charsize=2.0,c_labels=[1,0,0,1,0,0,1]
;  plots, t2,r2,psym=4,color=0, symsize=4.2, thick=16
;  plots, t2,r2,psym=4,color=cc1, symsize=4.0, thick=10
 
;  plots, tt2,rr2,psym=7, color=0,symsize=4.2, thick=16
;  plots, tt2,rr2,psym=7, color=cc,symsize=4.0, thick=10

  
  contour, post3, taus, refs[9:*],levels=lvls,c_colors=cls,xtitle='!9t!X',ytitle='r!De!N (!9m!Xm)', xr=[15,30],yr=[30,55];,$
;   c_charsize=2.0,c_labels=[1,0,0,1,0,0,1]
;  plots, t3,r3,psym=4,color=0, symsize=4.2, thick=16
;  plots, t3,r3,psym=4,color=cc1, symsize=4.0, thick=10
 
;  plots, tt3,rr3,psym=7, color=0,symsize=4.2, thick=16
;  plots, tt3,rr3,psym=7, color=cc,symsize=4.0, thick=10

 contour,transpose([[lvls],[lvls]]),[0,1],lvls,xtickname=[' ',' ',' '],xticks=1,ytickname=[' ',' ',' ',' '],yticks=3,$
  c_colors=cls,levels=lvls,/noerase,position=[0.92,0.21,0.93,0.95],/cell_fill,/normal,/ylog,yr=[lvls[0],0.7]
 axis, yaxis=1,yr=[lvls[0],0.7],ytitle='Probability',ytickv=lvls,yticks=nl
device,/close
 spawn,'convert '+fp+'.ps '+fp+'.png'
endif



;;;;;;;; plot of the marginal pdfs
if 0 then begin
fp=dir+'plots\p2\post_mpdf2'
  print, 'making plot :'+fp
  set_plot, 'ps'
  loadct, 39, /silent
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
  device, xsize=38, ysize=20
   !p.font=1 & !p.thick=5 & !p.charsize=4.4 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
   !p.multi=[0,3,2] & !x.margin=[6,0] & !y.margin=[4,1] & !p.symsize=1.5 & !x.omargin=[0,1]

  plot, taus,ptau1,xtitle='!9t!X',ytitle='Probability density',xr=[1,10]
  
  plot, taus,ptau2,xtitle='!9t!X',ytitle='Probability density',xr=[35,50]

  plot, taus,ptau3,xtitle='!9t!X',ytitle='Probability density',xr=[15,30]

  plot, refs[9:*],pref1,xtitle='r!De!N (!9m!Xm)',ytitle='Probability density',xr=[23,28]

  plot, refs[0:29],pref2,xtitle='r!De!N (!9m!Xm)',ytitle='Probability density',xr=[4,8]

  plot, refs[9:*],pref3,xtitle='r!De!N (!9m!Xm)',ytitle='Probability density',xr=[30,55]

  device, /close
  spawn,'convert '+fp+'.ps '+fp+'.png'
endif




;;;;;;; plot the independent SIC and cumulative SIC
if 0 then begin
fp=dir+'plots\p2\sic2'
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
  ps=indgen(np)+1
  pss=[1,2,3,11,12,13,14]
  cl=[50,250,150]

  plot, ps,psym=2,ytitle='Independent!CSIC',xticks=14,xticklen=0.1,$
   xtickname=replicate(' ',15),yrange=[0.,0.8],xrange=[1,15],/nodata,xstyle=3
  for p=0,np-1 do begin
   plots,[p+0.8,p+0.8],[0.,hia1[p]],thick=ts,color=50
   plots,[p+1,p+1],[0.,hia2[p]],thick=ts,color=250
   plots,[p+1.2,p+1.2],[0.,hia3[p]],thick=ts,color=150
  endfor
 
  xtit='!9h!X!D'+string(indgen(15)+1,format='(I2)')+'!N'
  plot, ps,psym=2,ytitle='Cumulative!CSIC',xticks=14,xticklen=0.1,$
   xtickname=xtit,yrange=[0.,1.0],xrange=[1,15],/nodata,xstyle=3 

  oplot,ps,hpa1,color=50,psym=-2,thick=10
  oplot,ps,hpa2,color=250,psym=-2,thick=10
  oplot,ps,hpa3,color=150,psym=-2,thick=10

  oplot,pss,hp1,color=50,psym=-2,thick=10,linestyle=2
  oplot,pss,hp2,color=250,psym=-2,thick=10,linestyle=2
  oplot,pss,hp3,color=150,psym=-2,thick=10,linestyle=2  

  legend,['All parameters','Parameter subset'],box=0,/bottom,/right,pspacing=1.2,linestyle=[0,2],charsize=2.0
  legend, ['ice','  !9t!X=5, r!De!N=25 !9m!Xm','liquid','  !9t!X=40, r!De!N=7 !9m!Xm','ice','  !9t!X=20, r!De!N=50 !9m!Xm'],$
   box=0, charsize=2.0, textcolors=[cl[0],cl[0],cl[1],cl[1],cl[2],cl[2]],position=[0.82,0.7],/normal
  device, /close
  spawn,'convert '+fp+'.ps '+fp+'.png'
endif


;;;;;;; plot the independent SIC and cumulative SIC
if 1 then begin
fp=dir+'plots\p2\err_tr'
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
  ps=indgen(np)+1
  pss=[1,2,3,11,12,13,14]
  cl=[50,250,150]

  pdf_err,ppdfa1,taus,refs,terra1,rerra1
  pdf_err,ppdf1,taus,refs,terr1,rerr1
  pdf_err,ppdfa2,taus,refs,terra2,rerra2
  pdf_err,ppdf2,taus,refs, terr2, rerr2
  pdf_err,ppdfa3,taus,refs,terra3,rerra3
  pdf_err,ppdf3,taus,refs, terr3, rerr3

   
  plot, ps,psym=2,ytitle='!9t!X uncertainty!Crange',xticks=15,xticklen=0.1,$
   xtickname=replicate(' ',16),yrange=[0,100],xrange=[0,15],/nodata,xstyle=3
  oplot,[0,ps],[100.,terra1],color=50,psym=-7,thick=10
  oplot,[0,ps],[100.,terra2],color=250,psym=-7,thick=10
  oplot,[0,ps],[100.,terra3],color=150,psym=-7,thick=10

  oplot,[0,pss],[100.,terr1],color=50,psym=-7,thick=10
  oplot,[0,pss],[100.,terr2],color=250,psym=-7,thick=10
  oplot,[0,pss],[100.,terr3],color=150,psym=-7,thick=10  


  xtit=['prior','!9h!X!D'+string(indgen(15)+1,format='(I2)')+'!N']
  plot, ps,psym=2,ytitle='r!De!N uncertainty!Crange (!9m!Xm)',xticks=15,xticklen=0.1,$
   xtickname=xtit,yrange=[0,60],xrange=[0,15],/nodata,xstyle=3
   
  oplot,[0,ps],[60.,rerra1],color=50,psym=-7,thick=10
  oplot,[0,ps],[60.,rerra2],color=250,psym=-7,thick=10
  oplot,[0,ps],[60.,rerra3],color=150,psym=-7,thick=10
   
  oplot,[0,pss],[60.,rerr1],color=50,psym=-7,thick=10,linestyle=2
  oplot,[0,pss],[60.,rerr2],color=250,psym=-7,thick=10,linestyle=2
  oplot,[0,pss],[60.,rerr3],color=150,psym=-7,thick=10,linestyle=2

  legend,['All parameters','Parameter subset'],box=0,/right,pspacing=1.2,linestyle=[0,2],charsize=2.0
  legend, ['ice','  !9t!X=5, r!De!N=25 !9m!Xm','liquid','  !9t!X=40, r!De!N=7 !9m!Xm','ice','  !9t!X=20, r!De!N=50 !9m!Xm'],$
   box=0, charsize=2.0, textcolors=[cl[0],cl[0],cl[1],cl[1],cl[2],cl[2]],position=[0.82,0.7],/normal
  device, /close
  spawn,'convert '+fp+'.ps '+fp+'.png'
endif


stop
end


;;;; procedure to get multiple values derived from the posterior pdf
;;;; gets the maximum likelihood values, marginal pdf
pro pdf_stat, post, taus,refs,tmax,rmax,mpdf_tau,mpdf_ref
nn=max(post,n,/nan)
in=array_indices(post,n)
tmax=taus[in[0]]
if n_elements(post[0,*]) eq 51 then rmax=refs[in[1]+9] else rmax=refs[in[1]]

mpdf_tau=total(post,2)
mpdf_ref=total(post,1)


end


;;;; function to get the cumulative SIC from the save SIC values
function cumul, h

n=n_elements(h)
hc=h
for i=1,n-1 do hc[i]=total(h[0:i],/nan)


return, hc
end


;;;; function to return the half-width half-max value of the peak in a pdf
function hwhm, post, taus, refs

; get the max likelihood
nn=max(post,n,/nan)
in=array_indices(post,n)
tmax=taus[in[0]]
if n_elements(post[0,*]) eq 51 then rmax=refs[in[1]+9] else rmax=refs[in[1]]
if n_elements(post[0,*]) eq 51 then ref=refs[9:*] else ref=refs[0:29]


; get the half max point in tau direction
dtp=interpol(taus[in[0]:*],post[in[0]:*,in[1]],nn/2.)
dtm=interpol(taus[0:in[0]],post[0:in[0],in[1]],nn/2.)
htp=dtp-tmax & htm=tmax-dtm
ter=(htp+htm)/2.

; get the half max point in ref direction
drp=interpol(ref[in[1]:*],post[in[0],in[1]:*],nn/2.)
drm=interpol(ref[0:in[1]],post[in[0],0:in[1]],nn/2.)
hrp=drp-rmax & hrm=rmax-drm 
rer=(hrp+hrm)/2.

return, [ter,rer]
end


;;;;; procedure that returns the errors for tau and ref for a post pdf of each parameter
pro pdf_err,post,taus, refs,terr,rerr
np=n_elements(post[0,0,*])
terr=fltarr(np) & rerr=fltarr(np)
for p=0, np-1 do begin
  z=hwhm(post[*,*,p],taus,refs)
  terr[p]=z[0] & rerr[p]=z[1]

endfor

end
