; program to plot the plane parrallel clouds for seeing the relationship between oxa water vapor absorption
; restore the days of 20120813 and 20120602

@mpfitfun.pro
@legend.pro
@expsf.pro
pro plane_cloud

dir='/home/leblanc/SSFR3/data/'

restore, dir+'20120602/20120602_cod_comp.out'

areaa=v2[*,0];area
area2a=v2[*,1];area2
oxaa=v2[*,2];oxa
taua=tau
tmhrsa=tmhrs
disda=disd

restore, dir+'20120813/20120813_cod_comp.out'
area=v2[*,0]
area2=v2[*,1]
oxa=v2[*,2]


fn=dir+'plane_par2'
print, 'plotting:'+fn
 set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold'
 device, filename=fn+'.ps'
 device,/color,bits_per_pixel=8., xsize=20, ysize=45
 !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
 !p.multi=[0,1,3] & !x.margin=[6,3]

a=where(tmhrsa gt 21. and tmhrsa lt 23. and area2a gt 10 and areaa gt -2)
b=where(tmhrs gt 15 and tmhrs lt 19 and area2 gt 10)

plot, areaa[a],taua[a], psym=2, title='Homogeneous clouds for 940 nm',ytitle='Cloud optical depth',$
 xtitle='Normalized integrated water vapor absorption',xrange=[-5,10],yrange=[0,200]
;oplot, area[b],tau[b],psym=2

area_tot=[areaa[a]];,area[b]]
tau_tot=[taua[a]];,tau[b]]

r=correlate(area_tot,tau_tot)
l=linfit(area_tot,tau_tot)

a1=[1.8,38,58]
w=tau_tot*0.+1.0
p=curvefit(tau_tot,area_tot,w,a1,sg,chisq=xx,function_name='exps',status=s,tol=0.000001)

oplot, findgen(300),findgen(300)*l[1]+l[0], linestyle=2,color=250

legend,['R='+strtrim(r,2),'y='+strtrim(l[1],2)+'+'+strtrim(l[0],2)+'x'],textcolors=[0,0],box=0

plot, area2a[a],taua[a], psym=2, title='Homogeneous clouds for 1150 nm',ytitle='Cloud optical depth',$
 xtitle='Normalized integrated water vapor absorption',xrange=[25,40.5],yrange=[0,200]
;oplot, area2[b],tau[b],psym=2

area2_tot=[area2a[a]];,area2[b]]
r2=correlate(area2_tot,tau_tot)
l2=linfit(area2_tot,tau_tot)

oplot, findgen(300),findgen(300)*l2[1]+l2[0], linestyle=2,color=250

legend,['R='+strtrim(r2,2),'y='+strtrim(l2[1],2)+'+'+strtrim(l2[0],2)+'x'],textcolors=[0,0],box=0

plot, oxaa[a],taua[a],psym=2,title='Oxygen-a band in homogeneous clouds',ytitle='Cloud optical depth',$
 xtitle='Normalized integrated Oxygen-a absorption',xrange=[3,7],yrange=[0,200]
;oplot, oxa[b],tau[b], psym=2
oxa_tot=[oxaa[a]];,oxa[b]]
rx=correlate(oxa_tot,tau_tot)
lx=linfit(oxa_tot,tau_tot)
oplot, findgen(300),findgen(300)*lx[1]+lx[0],linestyle=2,color=250

legend,['R='+strtrim(rx,2),'y='+strtrim(lx[1],2)+'+'+strtrim(lx[0],2)+'x'],textcolors=[0,0],box=0
device, /close
spawn, 'convert '+fn+'.ps '+fn+'.png'


fn=dir+'plane_cod2'
print, 'plotting:'+fn
 set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold'
 device, filename=fn+'.ps'
 device,/color,bits_per_pixel=8., xsize=20, ysize=20
 !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
 !p.multi=0 & !x.margin=[6,3]

 s=where(tmhrsa gt 23.0 and tmhrsa lt 25.5)  

 cod=area[b]*l[1]+l[0]
 cod2=area2[b]*l2[1]+l2[0]
 codx=oxa[b]*lx[1]+lx[0]
 plot, cod, taua[b], psym=2, title='Optical depth comparison',xtitle='Optical depth from absorption',$
  ytitle='Cloud optical depth',yrange=[0,200]
 oplot, cod2, taua[b], psym=2, color=70
 oplot, codx, taua[b], psym=2, color=250

 legend, ['Optical depth from 940 nm band','Optical depth from 1150 nm','Optical depth from Oxygen-a band'],$
  textcolors=[0,70,250],box=0


 device, /close
 spawn, 'convert '+fn+'.ps '+fn+'.png'

fn=dir+'cod_to_water'
print, 'plotting:'+fn
 set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold'
 device, filename=fn+'.ps'
 device,/color,bits_per_pixel=8., xsize=20, ysize=25
 !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
 !p.multi=[0,1,2] & !x.margin=[6,3]

  a1=[1.8,38,58]
  w=tau_tot*0.+1.0
  p=mpfitfun('expsf',tau_tot,area_tot,w,a1)

  a2=[-24,38,50]
  p2=mpfitfun('expsf',tau_tot,area2_tot,w,a2)


  tvlct,255,191,108,249
  plot, tau_tot, area2_tot,psym=2, title='Water vapor absorption vs. cloud optical depth',$
   xrange=[0,200],xstyle=4,yrange=[25,40.5],ymargin=[1,6]
  exps, findgen(170),p2,f2
  oplot, findgen(170), f2, color=249,thick=15
  chi="143B ;"
  if p2[0] lt 0 then u='y=e!U(x-'+strtrim(p2[1],2)+'/'+strtrim(p2[2],2)+')!N+'+strtrim((-1.*p2[0]),2) else u='y=e!U(x-'+strtrim(p2[1],2)+'/'+strtrim(p2[2],2)+')!N-'+strtrim(p2[0],2)
  legend, [u],box=0
  axis, xaxis=1,xrange=[0,200],xtickname=[' ',' ',' ',' ',' ',' ']
  legend, ['1150 nm band'],/right,/bottom,box=0

  plot, tau_tot,area_tot, psym=2, ytitle='                                       Normalized integrated absorption',$
   yrange=[-5,10],xrange=[0,200],xstyle=9, xtitle='Cloud optical depth',ymargin=[6,0]
  exps, findgen(170),p,f1
  oplot, findgen(170),f1, color=249,thick=15
  if p[0] lt 0 then u='y=e!U(x-'+strtrim(p[1],2)+'/'+strtrim(p[2],2)+')!N+'+strtrim((-1.*p[0]),2) else u='y=e!U(x-'+strtrim(p[1],2)+'/'+strtrim(p[2],2)+')!N-'+strtrim(p[0],2)
  legend, [u],box=0
  legend, ['940 nm band'],/right,/bottom,box=0
  device, /close
  spawn, 'convert '+fn+'.ps '+fn+'.png'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; cod compared to oxy-a ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

fn=dir+'cod_to_oxy'
print, 'plotting:'+fn
 set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold'
 device, filename=fn+'.ps'
 device,/color,bits_per_pixel=8., xsize=20, ysize=20
 !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
 !p.multi=0 & !x.margin=[6,3]
 ax=[-3.,20.,140.]
 px=mpfitfun('expsf',tau_tot,oxa_tot,w,ax)
  tvlct,191,255,128,100
alpha="141B  ;"
  plot, tau_tot, oxa_tot,psym=2, title='Oxygen-'+string(alpha)+' absorption as indicator of optical depth',ytitle='Normalized integrated Oxygen-'+string(alpha)+' absorption',$
   xrange=[0,200],yrange=[3.,7.],xtitle='Cloud optical depth'
  exps,findgen(170),px,fx
  oplot, findgen(170), fx, color=100,thick=15
  chi="143B ;"
  if px[0] lt 0 then u='y=e!U(x-'+strtrim(px[1],2)+'/'+strtrim(px[2],2)+')!N+'+strtrim((-1.*px[0]),2) else u='y=e!U(x-'+strtrim(px[1],2)+'/'+strtrim(px[2],2)+')!N-'+strtrim(px[0],2)
  legend, [u],box=0
  device, /close
  spawn, 'convert '+fn+'.ps '+fn+'.png'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; cod from oxa and water ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

fn=dir+'cod_from_path'
print, 'plotting:'+fn
 set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold'
 device, filename=fn+'.ps'
 device,/color,bits_per_pixel=8., xsize=20, ysize=20
 !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
 !p.multi=0 & !x.margin=[6,3]

  tvlct,250,128,000,247
  tvlct,255,191,108,248
  tvlct,115,230,000,101
  alpha="141B  ;"
 
  A = FINDGEN(17) * (!PI*2/16.)  
  USERSYM, COS(A), SIN(A), /FILL  
  
  plot, tau[b], p_to_t(oxa[b],px),psym=2, title='Optical depth from different methods',ytitle='Predicted optical depth',$
   xrange=[0,150],yrange=[0,150],xtitle='Cloud optical depth',/nodata
  oplot, tau[b],p_to_t(oxa[b],px),psym=8,color=101,symsize=0.2
  oplot, tau[b],p_to_t(area[b],p),psym=8,color=248,symsize=0.2
  oplot, tau[b],p_to_t(area2[b],p2),psym=8,color=247,symsize=0.2
  oplot, findgen(150),linestyle=2
  
  legend, ['Oxygen-!9'+string(alpha)+'!X','940 nm band','1150 nm band'],textcolors=[101,248,247],box=0,/right,/bottom
  legend, ['1:1    '],box=0,/right
  device, /close
  spawn, 'convert '+fn+'.ps '+fn+'.png'

;;;;;;; plot the histogram of the difference in tau ;;;;;;;;;

fn=dir+'cod_histogram'
print, 'plotting:'+fn
 set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold'
 device, filename=fn+'.ps'
 device,/color,bits_per_pixel=8., xsize=20, ysize=20
 !p.font=1 & !p.thick=10 & !p.charsize=2.3 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
 !p.multi=0 & !x.margin=[6,3]

  tvlct,250,128,000,247
  tvlct,255,191,108,248
  tvlct,115,230,000,101
  alpha="141B  ;"

  dtx=tau[b]-p_to_t(oxa[b],px)
  dtw=tau[b]-p_to_t(area[b],p)
  dtw2=tau[b]-p_to_t(area2[b],p2)

  bins=findgen(50)*8.-200.+4.
  hx=histogram(dtx,nbins=50,min=-200.,max=200.)
  hx=float(hx)/float(max(hx))
  hw=histogram(dtw,nbins=50,min=-200.,max=200.)
  hw=float(hw)/float(max(hw))
  hw2=histogram(dtw2,nbins=50,min=-200.,max=200.)
  hw2=float(hw2)/float(max(hw2)) 
 
  plot, bins, hx,psym=10, title='Differences in optical depth histogram',ytitle='Normalized frequency',$
   xrange=[-200,200],yrange=[0,1],xtitle='Cloud optical depth differences',/nodata
  oplot, bins,hx,psym=10,color=101
  oplot, bins,hw,psym=10,color=248
  oplot, bins,hw2,psym=10,color=247

  legend, ['Oxygen-!9'+string(alpha)+'!X','940 nm band','1150 nm band'],textcolors=[101,248,247],box=0,/right
  device, /close
  spawn, 'convert '+fn+'.ps '+fn+'.png'



stop
end

;;;; function to transform integrated areas(x) to optical depth with the arguments (a)
function p_to_t, x, a

return, a[1]+(a[2]*alog(x+a[0]))
end
