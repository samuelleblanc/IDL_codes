; program to plot the contour sic for tau and ref
; 

pro plot_sic_contour

dir='C:\Users\Samuel\Research\SSFR3\'

;restore, dir+'data\v1\retr_mod_all_iceps_v1.out'
;sic=sic_rtm & sicp=sic_par & pos=post
;restore, dir+'data\v1\retr_mod_all_liqps_v1.out'
;restore, dir+'data\v1\mod_all_v1.out'


ll='_ct'
ll='_dref'
;ll='_day'
;ll='_dref_rg'
;ll='_rg'
la='pss'
;fp=dir+'retrieved\cst\retr'+ll+'_pss_v3.out'
fp=dir+'retrieved\cst\retr'+ll+'_'+la+'_v4.out'
print, 'restoring: '+fp
restore, fp
pss=[1,2,3,5,6,7,9,11,13,15]-1



;sic=sic_rtm & sicp=sic_par
sic=hrt & sicp=hprt
pos=postrt[*,*,*,*,*,9]
taus=findgen(100)+1. & refs=findgen(60)+1.

tau=taum; [1.,2.,3.,4.,5.,7.,8.,10.,12.,15.,20.,25.,30.,35.,40.,45.,50.,60.,70.,80.,90.,100.]
ref=refm ;[2.,3.,4.,5.,6.,7.,8.,9.,10.,11.,12.,13.,14.,15.,16.,17.,18.,20.,22.,23.,25.,28.,$
     ;30.,32.,35.,38.,40.,43.,45.,48.,50.,52.,55.,58.,60.]

if 1 then begin
fp=dir+'plots\p2\sic_contour'+ll+'_'+la
  print, 'making plot :'+fp
  set_plot, 'ps'
  loadct, 39, /silent
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
  device, xsize=28, ysize=14
   !p.font=1 & !p.thick=5 & !p.charsize=2.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
   !p.multi=[0,2,1] & !x.margin=[6,0] & !y.margin=[3,2] & !p.symsize=1.5 & !x.omargin=[0,6] & !y.omargin=[0,0]

  nl=20
  lvls=findgen(nl)/(nl-1.)
  cls=lvls*250.

  tvlct, 180,180,180,201

  contour, smooth(sic[*,0:22,0],2), tau, ref[0:22],levels=lvls,c_colors=cls,xtitle='!9t!X',ytitle='r!De!N (!9m!Xm)',$
   xr=[1,100],yr=[2,30],title='Liquid'
  
  plots, [2.,19.,19.,2.,2.],[2.5,2.5,29.5,29.5,2.5],color=201
  plots, [21.,98.,98.,21.,21.],[15.3,15.3,29.5,29.5,15.3],color=201
  plots, [21.,98.,98.,21.,21.],[2.5,2.5,14.7,14.7,2.5],color=201
  xyouts, 12.,15.,'1',alignment=0.5,color=201
  xyouts, 60.,22.5,'2',alignment=0.5,color=201
  xyouts, 60.,8.5,'3',alignment=0.5,color=201

  sic[21,29:34,1]=interpol(sic[21,25:28,1],refm[25:28],refm[29:34])
  sic[20,32:34,1]=interpol(sic[21,28:31,1],refm[28:31],refm[32:34])
  contour, smooth(sic[*,8:*,1],2), tau, ref[8:*],levels=lvls,c_colors=cls,xtitle='!9t!X',ytitle='r!De!N (!9m!Xm)',$
   xr=[1,100],yr=[10,60],title='Ice'
  
  plots, [2.,19.,19.,2.,2.],[11.,11.,59.,59.,11.],color=201
  plots, [21.,98.,98.,21.,21.],[25.5,25.5,59.,59.,25.5],color=201
  plots, [21.,98.,98.,21.,21.],[11.,11.,24.5,24.5,11.],color=201
  xyouts, 12.,30.,'1',alignment=0.5,color=201
  xyouts, 60.,40.5,'2',alignment=0.5,color=201
  xyouts, 60.,15.5,'3',alignment=0.5,color=201

 contour,transpose([[lvls],[lvls]]),[0,1],lvls,xtickname=[' ',' ',' '],xticks=1,ytickname=[' ',' ',' ',' '],yticks=3,$
  c_colors=cls,levels=lvls,/noerase,position=[0.88,0.21,0.89,0.85],/cell_fill,/normal,yr=[0,1]
 axis, yaxis=1,yr=[0.0,1.0],ytitle='SIC'
 device,/close
 spawn,'convert '+fp+'.ps '+fp+'.png'
endif

fl1=where(tau le 20.)
f1l=where(ref le 30.)
f1i=where(ref ge 10.)
f23=where(tau gt 20.)
fll3=where(ref le 15.)
fll2=where(ref gt 15. and ref le 30.)
fli3=where(ref le 25. and ref ge 10.)
fli2=where(ref gt 25. and ref le 60.)

;restore, dir+'data\v1\retr_mod_all_iceps_v1.out'
;sicp=sic_par
;restore, dir+'data\v1\retr_mod_all_liqps_v1.out'
;sicp[*,*,0,*]=sic_par[*,*,0,*]

;pss=[1,2,3,11,12,13,14]-1

for p=0,n_elements(sicp[0,0,0,*])-1 do begin
  sicp[21,29:34,1,p]=interpol(sicp[21,25:28,1,p],refm[25:28],refm[29:34])
  sicp[20,32:34,1,p]=interpol(sicp[21,28:31,1,p],refm[28:31],refm[32:34])
endfor

get_sics, reform(sicp[fl1,f1l,0,*]),hl1,hpl1,hml1
get_sics, reform(sicp[fl1,f1i,1,*]),hi1,hpi1,hmi1

get_sics, reform(sicp[f23,fll2,0,*]),hl2,hpl2,hml2
get_sics, reform(sicp[f23,fli2,1,*]),hi2,hpi2,hmi2

get_sics, reform(sicp[f23,fll3,0,*]),hl3,hpl3,hml3
get_sics, reform(sicp[f23,fli3,1,*]),hi3,hpi3,hmi3

if 1 then begin
fp=dir+'plots\p2\sic_regions'+ll+'_'+la
  print, 'making plot :'+fp
  set_plot, 'ps'
  loadct, 39, /silent
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
  device, xsize=28, ysize=14
   !p.font=1 & !p.thick=5 & !p.charsize=3.4 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
   !p.multi=[0,2,3] & !x.margin=[0,1.4] & !y.margin=[0,0.8] & !p.symsize=1.5 & !x.omargin=[7,1] & !y.omargin=[4,1.0]
 
  xtn=replicate(' ',15)
  xtit='!9h!X!D'+string(indgen(15)+1,format='(I2)')+'!N'
  xv=indgen(15)
  plot,pss,hl1, ytitle='Cumulative!CSIC',xticks=14,xtickname=xtn,xticklen=0.1,psym=-7,yr=[0,1],xr=[-0.5,14.5],xtickv=xv,title='Liquid'
  errplot,pss, hml1,hpl1
  legend, ['1 - liquid'],box=0,charsize=1.8,position=[10,0.3],/data
  
  plot,pss,hi1, ytickname=xtn,xticks=14,xtickname=xtn,xticklen=0.1,psym=-7,yr=[0,1],xr=[-0.5,14.5],xtickv=xv, title='Ice'
  errplot,pss, hmi1,hpi1
  legend, ['1 - ice'],box=0,charsize=1.8,position=[10,0.3],/data

  plot,pss,hl2, ytitle='Cumulative!CSIC',xticks=14,xtickname=xtn,xticklen=0.1,psym=-7,yr=[0,1],xr=[-0.5,14.5],xtickv=xv
  errplot,pss, hml2,hpl2
  legend, ['2 - liquid'],box=0,charsize=1.8,position=[10,0.3],/data

  plot,pss,hi2, ytickname=xtn,xticks=14,xtickname=xtn,xticklen=0.1,psym=-7,yr=[0,1],xr=[-0.5,14.5],xtickv=xv
  errplot,pss, hmi2,hpi2
  legend, ['2 - ice'],box=0,charsize=1.8,position=[10,0.3],/data

  plot,pss,hl3, ytitle='Cumulative!CSIC',xticks=14,xtickname=xtit,xticklen=0.1,psym=-7,yr=[0,1],xr=[-0.5,14.5],xtickv=xv
  errplot,pss, hml3,hpl3
  legend, ['3 - liquid'],box=0,charsize=1.8,position=[10,0.3],/data

  plot,pss,hi3, ytickname=xtn,xticks=14,xtickname=xtit,xticklen=0.1,psym=-7,yr=[0,1] ,xr=[-0.5,14.5],xtickv=xv
  errplot,pss, hmi3,hpi3
  legend, ['3 - ice'],box=0,charsize=1.8,position=[10,0.3],/data

 
 device,/close
 spawn,'convert '+fp+'.ps '+fp+'.png'
endif




;;;r plot the error statistics
taus=findgen(100)+1. & refs=findgen(60)+1.
get_err,pos,tau,ref,taus,refs,terr,rerr,btau,bref,terra=terra,rerra=rerra 
; terr uncertainty in tau, rerr, uncertainty in ref, btau bias in tau, bref bias in ref

if 0 then begin
fp=dir+'plots\p2\terr_contour'+ll
  print, 'making plot :'+fp
  set_plot, 'ps'
  loadct, 39, /silent
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
  device, xsize=28, ysize=14
   !p.font=1 & !p.thick=5 & !p.charsize=2.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
   !p.multi=[0,2,1] & !x.margin=[6,0] & !y.margin=[3,2] & !p.symsize=1.5 & !x.omargin=[0,6] & !y.omargin=[0,0]

  nl=7
  lvls=findgen(nl)/(nl-1.)*15.
  cls=(findgen(nl)+1.)/float(nl)*250.

  contour, terr[*,*,0], tau, ref,levels=lvls,c_colors=cls,xtitle='!9t!X',ytitle='r!De!N (!9m!Xm)',$
   xr=[1,100],yr=[2,30],title='Liquid'
 
  contour, terr[*,*,1], tau, ref,levels=lvls,c_colors=cls,xtitle='!9t!X',ytitle='r!De!N (!9m!Xm)',$
   xr=[1,100],yr=[10,60],title='Ice'

 contour,transpose([[lvls],[lvls]]),[0,1],lvls,xtickname=[' ',' ',' '],xticks=1,ytickname=[' ',' ',' ',' '],yticks=3,$
  c_colors=cls,levels=lvls,/noerase,position=[0.88,0.21,0.89,0.85],/cell_fill,/normal,yr=[0,max(lvls)]
 axis, yaxis=1,yr=[0.0,max(lvls)],ytitle='!9t!X uncertainty (%)'
 device,/close
 spawn,'convert '+fp+'.ps '+fp+'.png'
endif

if 0 then begin
fp=dir+'plots\p2\rerr_contour'+ll
  print, 'making plot :'+fp
  set_plot, 'ps' 
  loadct, 39, /silent 
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
  device, xsize=28, ysize=14
   !p.font=1 & !p.thick=5 & !p.charsize=2.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
   !p.multi=[0,2,1] & !x.margin=[6,0] & !y.margin=[3,2] & !p.symsize=1.5 & !x.omargin=[0,6] & !y.omargin=[0,0]
 
  nl=7 
  ;lvls=findgen(nl)/(nl-1.)*50.
  ;cls=lvls*5
  lvls=findgen(nl)/(nl-1.)*30.
  cls=(findgen(nl)+1.)/float(nl)*250. 

  contour, rerr[*,*,0], tau, ref,levels=lvls,c_colors=cls,xtitle='!9t!X',ytitle='r!De!N (!9m!Xm)',$
   xr=[1,100],yr=[2,30],title='Liquid'
  
  contour, rerr[*,*,1], tau, ref,levels=lvls,c_colors=cls,xtitle='!9t!X',ytitle='r!De!N (!9m!Xm)',$
   xr=[1,100],yr=[10,60],title='Ice'
 
 contour,transpose([[lvls],[lvls]]),[0,1],lvls,xtickname=[' ',' ',' '],xticks=1,ytickname=[' ',' ',' ',' '],yticks=3,$
  c_colors=cls,levels=lvls,/noerase,position=[0.88,0.21,0.89,0.85],/cell_fill,/normal,yr=[0,max(lvls)]
 axis, yaxis=1,yr=[0.0,max(lvls)],ytitle='r!De!N uncertainty (%)'
 device,/close 
 spawn,'convert '+fp+'.ps '+fp+'.png'
endif

terr=terra & rerr=rerra

if 1 then begin
fp=dir+'plots\p2\err_contour'+ll+'_'+la
  print, 'making plot :'+fp
  set_plot, 'ps'
  loadct, 39, /silent
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
  device, xsize=29, ysize=22
   !p.font=1 & !p.thick=5 & !p.charsize=3.4 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
   !p.multi=[0,2,2] & !x.margin=[5,0] & !y.margin=[0,.2] & !p.symsize=1.5 & !x.omargin=[0,7] & !y.omargin=[3,2]

  nl=7
  ;lvls=findgen(nl)/(nl-1.)*3.0 ;7.5
  lvls=[0.1,0.2,0.7,1.2,1.7,2.5,3.0]
  cls=(findgen(nl)+1.)/float(nl)*250.
xtn=[' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' '] 

  contour, smooth(terr[*,*,0],2,/nan), tau, ref,levels=lvls,c_colors=cls,ytitle='r!De!N (!9m!Xm)',$
   xr=[1,100],yr=[2,30],title='Liquid',xtickname=xtn

  tvlct, 180,180,180,201
  plots, [2.,19.,19.,2.,2.],[2.5,2.5,29.5,29.5,2.5],color=201
  plots, [21.,98.,98.,21.,21.],[15.3,15.3,29.5,29.5,15.3],color=201
  plots, [21.,98.,98.,21.,21.],[2.5,2.5,14.7,14.7,2.5],color=201
  xyouts, 12.,15.,'1',alignment=0.5,color=201
  xyouts, 60.,22.5,'2',alignment=0.5,color=201
  xyouts, 60.,8.5,'3',alignment=0.5,color=201

 ; nl=5 
 ; lvls=findgen(nl)/(nl-1.)*7.5
 ; cls=(findgen(nl)+1.)/float(nl)*250
 
  contour, smooth(terr[*,*,1],2,/nan), tau, ref,levels=lvls,c_colors=cls,ytitle='r!De!N (!9m!Xm)',$
   xr=[1,100],yr=[10,60],title='Ice',xtickname=xtn
  
  plots, [2.,19.,19.,2.,2.],[11.,11.,59.,59.,11.],color=201
  plots, [21.,98.,98.,21.,21.],[25.5,25.5,59.,59.,25.5],color=201
  plots, [21.,98.,98.,21.,21.],[11.,11.,24.5,24.5,11.],color=201
  xyouts, 12.,30.,'1',alignment=0.5,color=201
  xyouts, 60.,40.5,'2',alignment=0.5,color=201
  xyouts, 60.,15.5,'3',alignment=0.5,color=201 

 ;lvls=findgen(nl)/(nl-1.)*15.
 lvi=[0.,lvls[1:*],max(lvls)+0.5]

 contour,transpose([[lvi],[lvi]]),[0,1],lvi,xtickname=[' ',' ',' '],xticks=1,ytickname=[' ',' ',' ',' '],yticks=3,$
  c_colors=cls,levels=[0.,lvls[1:*]],/noerase,position=[0.83,0.56,0.84,0.87],/cell_fill,/normal,yr=[0,max(lvi)]
 ;axis, yaxis=1,yr=[0.0,max(lvi)],ytitle='!9t!X uncertainty (%)',yticks=5
 axis, yaxis=1,yr=[0.0,max(lvi)],ytitle='!9t!X uncertainty',yticks=5
  nl=7
  ;lvls=findgen(nl)/(nl-1.)*50.
  ;cls=lvls*5
  ;lvls=findgen(nl)/(nl-1.)*3.0 ; *15.
  lvls=[0.1,0.3,0.8,1.3,2.0,2.8,3.5]
  cls=(findgen(nl)+1.)/float(nl)*250.

  contour, smooth(rerr[*,*,0],2,/nan), tau, ref,levels=lvls,c_colors=cls,xtitle='!9t!X',ytitle='r!De!N (!9m!Xm)',$
   xr=[1,100],yr=[2,30]

    tvlct, 180,180,180,201 
  plots, [2.,19.,19.,2.,2.],[2.5,2.5,29.5,29.5,2.5],color=201
  plots, [21.,98.,98.,21.,21.],[15.3,15.3,29.5,29.5,15.3],color=201
  plots, [21.,98.,98.,21.,21.],[2.5,2.5,14.7,14.7,2.5],color=201
  xyouts, 12.,15.,'1',alignment=0.5,color=201
  xyouts, 60.,22.5,'2',alignment=0.5,color=201
  xyouts, 60.,8.5,'3',alignment=0.5,color=201
 
;  nl=5
;  lvls=findgen(nl)/(nl-1.)*7.5
;  cls=(findgen(nl)+1.)/float(nl)*250
  contour, smooth(rerr[*,*,1],2,/nan), tau, ref,levels=lvls,c_colors=cls,xtitle='!9t!X',ytitle='r!De!N (!9m!Xm)',$
   xr=[1,100],yr=[10,60]

  plots, [2.,19.,19.,2.,2.],[11.,11.,59.,59.,11.],color=201
  plots, [21.,98.,98.,21.,21.],[25.5,25.5,59.,59.,25.5],color=201
  plots, [21.,98.,98.,21.,21.],[11.,11.,24.5,24.5,11.],color=201
  xyouts, 12.,30.,'1',alignment=0.5,color=201
  xyouts, 60.,40.5,'2',alignment=0.5,color=201
  xyouts, 60.,15.5,'3',alignment=0.5,color=201 

 lvi=[0.,lvls[1:*],max(lvls)+0.5]
 contour,transpose([[lvi],[lvi]]),[0,1],lvi,xtickname=[' ',' ',' '],xticks=1,ytickname=[' ',' ',' ',' '],yticks=3,$
  c_colors=cls,levels=[0,lvls[1:*]],/noerase,position=[0.83,0.17,0.84,0.48],/cell_fill,/normal,yr=[0,max(lvi)]
 ;axis, yaxis=1,yr=[0.0,max(lvi)],ytitle='r!De!N uncertainty (%)',yticks=5
 axis, yaxis=1,yr=[0.0,max(lvi)],ytitle='r!De!N uncertainty (!9m!Xm)',yticks=5
 device,/close
 spawn,'convert '+fp+'.ps '+fp+'.png'
endif

stop
if 1 then begin
fp=dir+'plots\p2\bias_contour'+ll+'_'+la
  print, 'making plot :'+fp
  set_plot, 'ps'
  loadct, 39, /silent
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
  device, xsize=28, ysize=22
   !p.font=1 & !p.thick=5 & !p.charsize=3.4 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
   !p.multi=[0,2,2] & !x.margin=[5,0] & !y.margin=[0,.2] & !p.symsize=1.5 & !x.omargin=[0,6] & !y.omargin=[3,2]

  nl=10
  lvls=findgen(nl)/(nl-1.)*60.-30.
  cls=(findgen(nl)+1.)/float(nl)*250.
xtn=[' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ']
loadct, 70
tvlct,0,0,0,0
mm=where(btau gt max(lvls),nmm)
if nmm gt 0 then btau[mm]=max(lvls)
ml=where(btau lt min(lvls),nml)
if nml gt 0 then btau[ml]=min(lvls)
  contour, smooth(btau[*,*,0],2), tau, ref,levels=lvls,c_colors=cls,ytitle='r!De!N (!9m!Xm)',$
   xr=[1,100],yr=[2,30],title='Liquid',xtickname=xtn,/fill

  contour, smooth(btau[*,*,1],2), tau, ref,levels=lvls,c_colors=cls,ytitle='r!De!N (!9m!Xm)',$
   xr=[1,100],yr=[10,60],title='Ice',xtickname=xtn,/fill

 contour,transpose([[lvls],[lvls]]),[0,1],lvls,xtickname=[' ',' ',' '],xticks=1,ytickname=[' ',' ',' ',' '],yticks=3,$
  c_colors=cls,levels=lvls,/noerase,position=[0.86,0.56,0.87,0.87],/cell_fill,/normal,yr=[min(lvls),max(lvls)]
 axis, yaxis=1,yr=[min(lvls),max(lvls)],ytitle='!9t!X bias (%)'

  nl=10
  ;lvls=findgen(nl)/(nl-1.)*50.
  ;cls=lvls*5
  lvls=findgen(nl)/(nl-1.)*60.-30.
  cls=(findgen(nl)+1.)/float(nl)*250.

mm=where(bref gt max(lvls),nmm)
if nmm gt 0 then bref[mm]=max(lvls)
ml=where(bref lt min(lvls),nml)
if nml gt 0 then bref[ml]=min(lvls) 
  contour, smooth(bref[*,*,0],2), tau, ref,levels=lvls,c_colors=cls,xtitle='!9t!X',ytitle='r!De!N (!9m!Xm)',$
   xr=[1,100],yr=[2,30],/fill

  contour, smooth(bref[*,*,1],2), tau, ref,levels=lvls,c_colors=cls,xtitle='!9t!X',ytitle='r!De!N (!9m!Xm)',$
   xr=[1,100],yr=[10,60],/fill

 contour,transpose([[lvls],[lvls]]),[0,1],lvls,xtickname=[' ',' ',' '],xticks=1,ytickname=[' ',' ',' ',' '],yticks=3,$
  c_colors=cls,levels=lvls,/noerase,position=[0.86,0.17,0.87,0.48],/cell_fill,/normal,yr=[min(lvls),max(lvls)]
 axis, yaxis=1,yr=[min(lvls),max(lvls)],ytitle='r!De!N bias (%)'

 device,/close
 spawn,'convert '+fp+'.ps '+fp+'.png'
endif



stop
end


;; procedure to get the mean, and extremes of the sic 
pro get_sics,hpar, hm, hmax,hmin
  nt=n_elements(hpar[*,0,0])
  nr=n_elements(hpar[0,*,0])
  np=n_elements(hpar[0,0,*])
  hc=hpar

  hm=fltarr(np) & hmax=fltarr(np) & hmin=fltarr(np)

  ; change the hpar to cumulative values
  for t=0, nt-1 do for r=0, nr-1 do hc[t,r,*]=cumul(reform(hpar[t,r,*]))

  for p=0, np-1 do begin
    hm[p]=mean(hc[*,*,p],/nan)
    hmin[p]=min(hc[*,*,p],/nan)
    hmax[p]=max(hc[*,*,p],/nan)
  endfor
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
  if n_elements(post[0,*]) eq 51 then ice=1 else ice=0  

  ; get the max likelihood
  nn=max(post,n,/nan)
  in=array_indices(post,n)
  tmax=taus[in[0]]
  if ice then rmax=refs[in[1]+9] else rmax=refs[in[1]]
  if ice then ref=refs[9:*] else ref=refs[0:29]

  pt=total(post,2)
  if ice then pr=total(post[*,9:*],1) else pr=total(post[*,0:29],1)
  ter=sqrt(total(pt*taus^2)-total(pt*taus)^2)
  if ice then $
   rer=sqrt(total(pr*refs[9:*]^2)-total(pr*refs[9:*])^2) else $
    rer=sqrt(total(pr*refs[0:29]^2)-total(pr*refs[0:29])^2)

  ; get the half max point in tau direction
  if in[0] ge n_elements(taus)-1 then dtp=taus[n_elements(taus)-1] else dtp=interpol(taus[in[0]:*],post[in[0]:*,in[1]],nn/2.)
  if in[0] eq 0 then dtm=0. else dtm=interpol(taus[0:in[0]],post[0:in[0],in[1]],nn/2.)
  htp=dtp-tmax & htm=tmax-dtm
  tfr=(htp+htm)/2.
  ; get the half max point in ref direction
  if in[1] ge n_elements(ref)-1 then drp=ref[n_elements(ref)-1] else drp=interpol(ref[in[1]:*],post[in[0],in[1]:*],nn/2.)
  if in[1] eq 0 then drm=0. else drm=interpol(ref[0:in[1]],post[in[0],0:in[1]],nn/2.)
  hrp=drp-rmax & hrm=rmax-drm
  rfr=(hrp+hrm)/2.
return, [ter,rer]
end



;;;;;procedure to get the uncertainty in tau and ref for the entire post cloud property space
pro get_err,post,tau,ref,taus,refs,terr,rerr,btau,bref,terra=terra,rerra=rerra

nt=n_elements(tau)
nr=n_elements(ref)
bref=fltarr(nt,nr,2)
btau=fltarr(nt,nr,2)
terr=fltarr(nt,nr,2)
rerr=fltarr(nt,nr,2)
terra=terr & rerra=rerr

  for t=0,nt-1 do begin
    for r=0,nr-1 do begin
      if ref[r] le 30. then begin
        pp=reform(post[t,r,0,*,0:29])
        pp[5,14]=pp[5,14]-max(pp)/10.
        pdf_stat,pp,taus,refs,tx,rx                       
        btau[t,r,0]=(tx-tau[t])/tau[t]*100.
        bref[t,r,0]=(rx-ref[r])/ref[r]*100.
        z=hwhm(pp,taus,refs)
        terr[t,r,0]=z[0]/tau[t]*100./2.
        rerr[t,r,0]=z[1]/ref[r]*100./2.
        terra[t,r,0]=z[0]/2. & rerra[t,r,0]=z[1]/2.
      endif
      if ref[r] ge 10. then begin
        pp=reform(post[t,r,1,*,9:*]) 
        pp[5,14]=pp[5,14]-max(pp)/10.
        pdf_stat,pp,taus,refs,tx,rx 
        btau[t,r,1]=(tx-tau[t])/tau[t]*100. 
        bref[t,r,1]=(rx-ref[r])/ref[r]*100.
        z=hwhm(pp,taus,refs)
        terr[t,r,1]=z[0]/tau[t]*100./2.
        rerr[t,r,1]=z[1]/ref[r]*100./2.
        terra[t,r,1]=z[0]/2. & rerra[t,r,1]=z[1]/2.
      endif
    endfor
  endfor
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
