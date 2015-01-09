; program to analyse the probabilities of ice or liquid water clouds 
; checks out every parameter with their corresponding probability

pro plot_wp_prob

dir='/home/leblanc/SSFR3/data/'
restore, dir+'retrieved_pdf_20120523_model.out'
restore, dir+'model_taus_ref.out'

ind=intarr(24,15,2)
icprob=fltarr(15,24,15,2)
liprob=fltarr(15,24,15,2)
ic_parms=intarr(24,15,2)
li_parms=intarr(24,15,2)


for w=0,1 do begin
  for t=0,23 do begin
    for r=0,14 do begin
      ind[t,r,w]=w*360+t*15+r
      icprob[*,t,r,w]=reform(ic_prob[0,0:14,ind[t,r,w]])
      liprob[*,t,r,w]=reform(li_prob[0,0:14,ind[t,r,w]])
      nul=max(icprob[*,t,r,w],ns)
      if nul gt 0.95 then ic_parms[t,r,w]=ns else ic_parms[t,r,w]=-1
      nul=max(liprob[*,t,r,w],ns)
      if nul gt 0.95 then li_parms[t,r,w]=ns else li_parms[t,r,w]=-1
;stop
    endfor
  endfor
endfor

fl=intarr(2,15,720)-1
cts=intarr(2,15)
eps=0.95
for p=0,14 do begin 
  m=where(li_prob[0,p,*] ge eps,count)
  fl[0,p,0:count-1]=m
  cts[0,p]=count
  m=where(ic_prob[0,p,*] ge eps,count)
  fl[1,p,0:count-1]=m
  cts[1,p]=count
endfor


set_plot, 'x'
device, decomposed=0
loadct, 39
window,1,retain=2,xsize=1200,ysize=1000
!p.multi=0
plot, [1,0],[1,0],ytitle='ref',xtitle='tau',title='parameter most important',/nodata,yrange=[0,50],xrange=[1,400],/xlog
for t=0, 23 do begin
  for r=0, 14 do begin
    if ic_parms[t,r,1] ge 0 then xyouts,tau[t],ref[r]-0.5,strtrim(ic_parms[t,r,1],2),alignment=0.1,color=70
    if li_parms[t,r,0] ge 0 then xyouts,tau[t],ref[r]-0.5,strtrim(li_parms[t,r,0],2),alignment=0.9,color=150
  endfor
endfor

xyouts,100,52,'liquid',color=150
xyouts,200,52,'ice',color=70

window, 0, retain=2,xsize=1200,ysize=600
lvls=(findgen(41))/80.+0.5
clrs=(lvls-0.5)*2.*254

!p.multi=[0,2,1]
for p=0, 14 do begin

contour, icprob[p,*,*,1],tau,ref,levels=lvls,/cell_fill,title='Ice probabilities for p:'+strtrim(p,2),xtitle='tau',ytitle='ref',c_colors=clrs
contour, liprob[p,*,*,0],tau,ref,levels=lvls,/cell_fill,title='Liquid probabilities for p:'+strtrim(p,2),xtitle='tau',ytitle='ref',xmargin=[3,10],c_colors=clrs

colorbar, /vertical,/right,range=[0.5,1.],position=[0.95,0.1,0.96,0.9],format='(F5.3)'

stop
endfor

if 1 then begin
fp=dir+'wp_probs'
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



stop
end
