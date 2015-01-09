; program to plot the sensitivity to a 5% change in radiance on g
; simple plotting routine
; v2 - now plotting dg/g and dw/w 


@/home/leblanc/IDL/bin/colorbar.pro
pro plot_dg

F_o=1.8 ;f not

an=fltarr(20,20)
bn=an
P=an
dp=P
dg=fltarr(20,20,20,21)
dw=fltarr(20,20,20,21)
g=findgen(20)/40.+0.5
mu=findgen(20)/10.+1.
tau=findgen(20)/40.+0.001
w=findgen(21)/40.+0.5
for i=0, 19 do begin
  for j=0,19 do begin
    P[i,j]=0.5*(1-g[i]^(2.))*(1.+g[i]^2.-2./mu[j]*g[i])^(-3./2.)
    an[i,j]=(g[i]^(2.))/(2.*g[i]-2./mu[j])
    bn[i,j]=g[i]*(-3.)*(g[i]-1./mu[j])/(1.+g[i]^2.-2./mu[j]*g[i])
;    dp[i,j]=bn[i,j]^(3./2.)*(-3./2.*an[i,j]/bn[i,j]-g[i])^(-1.)
    for k=0,19 do begin
      for l=0, 20 do begin
      dg[i,j,k,l]=((exp((-1.)*mu[j]*tau[k])-exp((-1.)*tau[k]))*tau[k]*w[l])/(bn[i,j]-an[i,j])
;(mu[j]-1.)/(F_o*mu[j])*((exp((-1.)*mu[j]*tau[k])-exp((-1.)*tau[k]))*tau[k]*w[l])^(-1.)*dp[i,j]
      dw[i,j,k,l]=((exp((-1.)*mu[j]*tau[k])-exp((-1.)*tau[k]))*tau[k]*P[i,j])
;(mu[j]-1.)/(F_o*mu[j])*((exp((-1.)*mu[j]*tau[k])-exp((-1.)*tau[k]))*tau[k]*P[i,j])^(-1.)
     ; print, dw[i,j,k,l], dg[i,j,k,l]
 ;     if not finite(dw[i,j,k,l]) and i gt 0 and j gt 0 then stop

      endfor
    endfor
  endfor
endfor
;stop

ta='!9'+string("164B)+'!X';"
wa='!9'+string("166B)+'!X';"
ua='!9'+string("155B)+'!X';"
de='!9'+string("104B)+'!X';"
dd='!9'+string("266B)+'!X';"

!x.omargin=[0,13]

dg=abs(dg)
dw=abs(dw)

;lvls=[exp(findgen(25)/4.),exp(findgen(5)*2.+7.)] ; custom levels
lvls=findgen(30)/250.

nlvl=string(lvls[[0,4,9,14,19,24,29]]*100.,format='(G6.3)')

fn='/home/leblanc/DC3_SEAC4RS/SSFR3/plots/sensitivity_dg_g'
     print, 'plotting:'+fn
     set_plot, 'ps'
     loadct, 39, /silent
     device, /encapsulated, /tt_font, set_font='Helvetica Bold'
     device, filename=fn+'.ps'
     device,/color,bits_per_pixel=8., xsize=40, ysize=40
      !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
      !p.multi=[0,2,2] & !x.margin=[6,0]

mi=-1.E6
ma=2.E6

     contour, reform(dg[10,*,*,19]),mu,tau,xtitle=ua, ytitle=ta, levels=lvls,/cell_fill,$
      title='Sensitivity of '+dd+'g/g to changing '+ua+' and '+ta;, xticks=4,/xlog;,min_value=mi,max_value=ma

     contour, reform(dg[*,*,10,19]),g,mu,xtitle='g', ytitle=ua, levels=lvls,/cell_fill,$
      title='Sensitivity of '+dd+'g/g to changing g and '+ua;, yticks=4,/ylog;,min_value=mi,max_value=ma

     contour, reform(dg[*,10,*,19]),g,tau,xtitle='g', ytitle=ta, levels=lvls,/cell_fill,$
      title='Sensitivity of '+dd+'g/g to changing g and '+ta;, min_value=mi,max_value=ma

     contour, reform(dg[10,10,*,*]),tau,w,xtitle=ta, ytitle=wa, levels=lvls,/cell_fill,$
      title='Sensitivity of '+dd+'g/g to changing '+ta+' and '+wa;, min_value=mi,max_value=ma

;     colorbar, minrange=mi,maxrange=ma, title=de+'g/'+de+'I',/vertical,/right,format='(G10.2)', position=[0.87,0.13,0.89,0.93]

   contour, transpose([[findgen(30)],[findgen(30)]]),[0,1],findgen(30),/cell_fill,nlevels=30,position=[0.87,0.13,0.89,0.93],/normal,/noerase,xticks=1,yticks=1,xtickname=[' ',' '],ytickname=[' ',' ']  ;take this out to omit residual
    axis, yaxis=1,ystyle=1,yrange=[min(lvls),max(lvls)],ytitle='Change in g due to one percent change of I (%)',yticks=6,ytickname=nlvl


    device, /close
    spawn, 'convert '+fn+'.ps '+fn+'.png'
    
    fn='/home/leblanc/DC3_SEAC4RS/SSFR3/plots/sensitivity_dw_ssa'
     print, 'plotting:'+fn
     set_plot, 'ps'
     loadct, 39, /silent
     device, /encapsulated, /tt_font, set_font='Helvetica Bold'
     device, filename=fn+'.ps'
     device,/color,bits_per_pixel=8., xsize=40, ysize=40
      !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
      !p.multi=[0,2,2] & !x.margin=[6,0]
mi=-2.E6
ma=0
     contour, reform(dw[10,*,*,19]),mu,tau,xtitle=ua, ytitle=ta, levels=lvls,/cell_fill,$
      title='Sensitivity of '+dd+wa+'/'+wa+' to changing '+ua+' and '+ta;, xticks=4,/xlog;,min_value=mi,max_value=ma

     contour, reform(dw[*,*,10,19]),g,mu,xtitle='g', ytitle=ua, levels=lvls,/cell_fill,$
      title='Sensitivity of '+dd+wa+'/'+wa+' to changing g and '+ua;, yticks=4,/ylog;,min_value=mi,max_value=ma

     contour, reform(dw[*,10,*,19]),g,tau,xtitle='g', ytitle=ta, levels=lvls,/cell_fill,$
      title='Sensitivity of '+dd+wa+'/'+wa+' to changing g and '+ta;, min_value=mi,max_value=ma

     contour, reform(dw[10,10,*,*]),tau,w,xtitle=ta, ytitle=wa, levels=lvls,/cell_fill,$
      title='Sensitivity of '+dd+wa+'/'+wa+' to changing '+ta+' and '+wa;, min_value=mi,max_value=ma

    ; colorbar, minrange=mi,maxrange=ma, title=de+wa+'/'+de+'I',/vertical,/right,format='(G10.2)', position=[0.87,0.13,0.89,0.93]

contour, transpose([[findgen(30)],[findgen(30)]]),[0,1],findgen(30),/cell_fill,nlevels=30,position=[0.87,0.13,0.89,0.93],/normal,/noerase,xticks=1,yticks=1,xtickname=[' ',' '],ytickname=[' ',' ']  ;take this out to omit residual
    axis, yaxis=1,ystyle=1,yrange=[min(lvls),max(lvls)],ytitle='Change in '+wa+' due to one percent change of I (%)',yticks=6,ytickname=nlvl

    device, /close
    spawn, 'convert '+fn+'.ps '+fn+'.png'
stop
end
