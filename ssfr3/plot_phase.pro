; program to plot the probability of retrieving the phase

pro plot_phase
dir='C:\Users\Samuel\Research\SSFR3\'

;restore, dir+'retrieved\cst\phase_prob.out'
restore, dir+'data\full\retr_pha_prob.out'

fp=dir+'plots\p2\phase_prob_v2_rg'
  print, 'making plot :'+fp
  set_plot, 'ps'
  loadct, 39, /silent
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
  device, xsize=26, ysize=10
   !p.font=1 & !p.thick=5 & !p.charsize=2.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
   !p.multi=0 & !x.margin=[7,1] & !y.margin=[0.3,0.5] & !p.symsize=1.5 & !x.omargin=[0,0] & !y.omargin=[2.3,0.1]
  tvlct,0,150,0,150

ps=[1,2,3,4,5,6,7,9,11,12,13,14,15]-1
ps=[1,2,3,5,6,7,9,11,13,15]-1
cl=[250,150,50]
t=[14,10,4]
r=[5,30,20]
w=[0,1,1] 

pl1=[0.5,reform(liq_prob[t[0],r[0],w[0],0,*])]
pl2=[0.5,reform(liq_prob[t[1],r[1],w[1],0,*])]
pl3=[0.5,reform(liq_prob[t[2],r[2],w[2],0,*])]
pl3[[2,3,4]]=1.0-pl3[[2,3,4]]

pi1=reform(ice_prob[t[0],r[0],w[0],0,*])
pi2=reform(ice_prob[t[1],r[1],w[1],0,*])
pi3=reform(ice_prob[t[2],r[2],w[2],0,*])


ps=[0,ps+1]
  xtit=['prior','!9h!X!D'+string(indgen(15)+1,format='(I2)')+'!N']

plot, ps,psym=2,ytitle='Liquid probability',xticks=15,xticklen=0.1,$
   xtickname=xtit,yrange=[-0.1,1.1],xrange=[0,15],/nodata,xstyle=3,ytickv=[0.,0.25,0.5,0.75,1.0]

  oplot,ps,pl1,color=cl[0],psym=-7,thick=10
  oplot,ps,pl2,color=cl[1],psym=-7,thick=10
  oplot,ps,pl3,color=cl[2],psym=-7,thick=10


;  oplot,ps,pi1,color=cl[0],psym=-7,thick=10,linestyle=2
;  oplot,ps,pi2,color=cl[1],psym=-7,thick=10,linestyle=2
;  oplot,ps,pi3,color=cl[2],psym=-7,thick=10,linestyle=2

 ; legend,['All parameters','Parameter subset'],box=0,/bottom,/right,pspacing=1.2,linestyle=[0,2],charsize=2.0
  legend, ['A - !9t!X=40, r!De!N=7 !9m!Xm, liquid','B - !9t!X=20, r!De!N=50 !9m!Xm, ice','C - !9t!X=5, r!De!N=25 !9m!Xm, ice'],$
   box=0, charsize=2.0, textcolors=[cl[0],cl[1],cl[2]],/right,/bottom ;position=[0.82,0.7],/normal
  device, /close
  spawn,'convert '+fp+'.ps '+fp+'.png'
stop
end
