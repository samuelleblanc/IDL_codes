; program to plot the integrated water vapor absorption 
; vs. tau
; vs. oxygen-a

@fe.pro
@legend.pro
@get_precip_water.pro
@get_ceil.pro
pro plot_poster
dir='/home/leblanc/SSFR3/data/'

restore, dir+'tot.out'

; make the tau averages
tbins=findgen(51)*200./50.
tavg=(findgen(50)+0.5)*200./50.
a1=fltarr(50)  & a2=fltarr(50)  & a3=fltarr(50)  & ox=fltarr(50)
sa1=fltarr(50) & sa2=fltarr(50) & sa3=fltarr(50) & sox=fltarr(50)


for j=0,49 do begin
  ns=where(tottau gt tbins[j] and tottau le tbins[j+1],nu)
  if nu gt 0 then begin
    a1[j]=mean(totarea[ns],/nan) & a2[j]=mean(totarea2[ns],/nan) &a3[j]=mean(totarea3[ns],/nan) &ox[j]=mean(totoxa[ns],/nan)
    sa1[j]=stddev(totarea[ns],/nan) & sa2[j]=stddev(totarea2[ns],/nan)
    sa3[j]=stddev(totarea3[ns],/nan) & sox[j]=stddev(totoxa[ns],/nan)
  endif
endfor

fn=dir+'water_bands'
set_plot,'ps'
print, 'making plot :'+fn
loadct, 39, /silent
device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fn+'.ps'
device, xsize=20, ysize=30
 !p.font=1 & !p.thick=5 & !p.charsize=5.5 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
 !p.multi=[0,1,3] & !x.margin=[7,3] &!x.omargin=[0,0]

 tvlct,107,255,117,100
 tvlct,255,117,173,248
 tvlct,241,241,087,247
 tvlct,240,136,000,249

 plot,tottau, totarea3, psym=3, yrange=[0,12],title='Water vapor',xstyle=4,$
  ymargin=[0,3],xrange=[0,200],/nodata
 ;oplot, tavg, a3, psym=6,color=247
 ;errplot, tavg, a3-sa3,a3+sa3,color=247
 app=[1,1,200]
 parinfo = replicate({fixed:0, limited:[0,0], $
                       limits:[0.D,0.D]}, 3)
 parinfo[2].fixed=0
 parinfo[2].limited=[1,1]
 parinfo[2].limits =[125.,275.]

 u=where(tottau gt 0 and tottau lt 200. and finite(tottau) eq 1 and finite(totarea3) eq 1 and totarea3 gt 0 and totarea3 lt 150)
 p3=mpfitfun('fe',tottau[u],totarea3[u],err_tau(tottau[u]),app,parinfo=parinfo)
 p3o=fe(0.,p3)
 a3=a3-p3o
 totarea3=totarea3-p3o
 oplot, tavg, a3, psym=6,color=247
 errplot, tavg, a3-sa3,a3+sa3,color=247
 oplot, tavg, fe(tavg,p3)-p3o,thick=8, color=247
; oplot, tottau, totarea3, psym=3, color=247
 axis, xaxis=1,xrange=[0,200],xtickname=[' ',' ',' ',' ',' ',' ']
 legend, ['810 nm band'],box=0, /right, /bottom,charsize=2
 
 plot,tottau, totarea, psym=3, xstyle=4,ytitle='Relative path integrated attenuation',ymargin=[1,1],$
  xrange=[0,200],yrange=[20,100]-20.,/nodata
 ;oplot, tavg, a1, psym=6,color=248
 ;errplot, tavg, a1-sa1,a1+sa1,color=248
 u=where(tottau gt 0 and tottau lt 200. and finite(tottau) eq 1 and finite(totarea) eq 1 and totarea gt 0 and totarea lt 150)
 p1=mpfitfun('fe',tottau[u],totarea[u],err_tau(tottau[u]),app,parinfo=parinfo)
 p1o=fe(0.,p1)
 a1=a1-p1o
 totarea=totarea-p1o
 oplot, tavg, a1, psym=6,color=248
 errplot, tavg, a1-sa1,a1+sa1,color=248
 oplot, tavg, fe(tavg,p1)-p1o,thick=8, color=248
 legend, ['940 nm band'],box=0, /right,/bottom,charsize=2

 plot,tottau, totarea2, psym=3, xstyle=9,xtitle='Cloud optical depth',xrange=[0,200],ymargin=[3,0],yrange=[20,150]-20.,/nodata
 ;oplot, tavg, a2, psym=6,color=249
 ;errplot, tavg, a2-sa2,a2+sa2,color=249
 u=where(tottau gt 0 and tottau lt 200. and finite(tottau) eq 1 and finite(totarea2) eq 1 and totarea2 gt 0 and totarea2 lt 150)
 p2=mpfitfun('fe',tottau[u],totarea2[u],err_tau(tottau[u]),app,parinfo=parinfo)
 p2o=fe(0.,p2)
 a2=a2-p2o
 totarea2=totarea2-p2o
 oplot, tavg, a2, psym=6,color=249
 errplot, tavg, a2-sa2,a2+sa2,color=249
 oplot, tavg, fe(tavg,p2)-p2o,thick=8, color=249
 legend, ['1150 nm band'],box=0, /right, /bottom,charsize=2

device, /close
spawn, 'convert '+fn+'.ps '+fn+'.png'


fn=dir+'oxa_band'
set_plot,'ps'
print, 'making plot :'+fn
loadct, 39, /silent
device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fn+'.ps'
device, xsize=20, ysize=20
 !p.font=1 & !p.thick=5 & !p.charsize=3.2 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
 !p.multi=0 & !x.margin=[7,3] &!x.omargin=[0,0]

 tvlct,107,255,117,100
 tvlct,255,117,173,248
 tvlct,241,241,087,247
 tvlct,240,136,000,249

 alpha="141b  ;"
 plot,tottau, totoxa, psym=3, yrange=[2,6]-2.5,title='Oxygen-A',$
  xrange=[0,200],ytitle='Relative path integrated attenuation',xtitle='Cloud optical depth',/nodata
; oplot, tavg, ox, psym=6,color=100
; errplot, tavg, ox-sox,ox+sox,color=100
 u=where(tottau gt 0 and tottau lt 200. and finite(tottau) eq 1 and finite(totoxa) eq 1 and totoxa gt 0 and totoxa lt 10)
 px=mpfitfun('fe',tottau[u],totoxa[u],err_tau(tottau[u]),app,parinfo=parinfo)
 pxo=fe(0.,px)
 ox=ox-pxo
 totoxa=totoxa-pxo
 oplot, tavg, ox, psym=6,color=100
 errplot, tavg, ox-sox,ox+sox,color=100
 oplot, tavg, fe(tavg,px)-pxo,thick=8, color=100


device, /close
spawn, 'convert '+fn+'.ps '+fn+'.png'


fn=dir+'water_base'
set_plot,'ps'
print, 'making plot :'+fn
loadct, 39, /silent
device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fn+'.ps'
device, xsize=20, ysize=20
 !p.font=1 & !p.thick=5 & !p.charsize=3.2 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
 !p.multi=0 & !x.margin=[6,7] &!x.omargin=[0,0]


plot, tottau, totarea, psym=3, title='Cloud base height',xtitle='Cloud optical depth',$
 ytitle='Relative path integrated attenuation', xrange=[0,200],yrange=[20,100]-25.,/nodata

;for n=0, n_elements(tottau)-1 do oplot, tottau[[n,n]],totarea[[n,n]],psym=3, color=totbase[n]/30.
wa=fltarr(10,50)
saw=fltarr(10,50)
wbins=findgen(11)*7650./10.
wavg =(findgen(10)+0.5)*7650./10.
for k=0, 9 do begin
  nb=where(totbase gt wbins[k] and totbase le wbins[k+1] and finite(totarea) eq 1 and finite(tottau) eq 1 and totarea lt 150 and totarea gt 0,ub)
  if ub gt 0 then begin
    for j=0, 49 do begin
      ns=where(tottau[nb] gt tbins[j] and tottau[nb] le tbins[j+1],nu)
      if nu gt 0 then begin
        wa[k,j]=mean(totarea[nb[ns]],/nan)
        saw[k,j]=stddev(totarea[nb[ns]],/nan)
      endif else begin
        wa[k,j]=!values.f_nan
        saw[k,j]=!values.f_nan
      endelse
    endfor
  endif
  ;oplot, tavg, wa[k,*], psym=6,color=k*253./9.+1.
  ;errplot, tavg, wa[k,*]-saw[k,*],wa[k,*]+saw[k,*],color=k*253./9.+1.
  p=mpfitfun('fe',tottau[nb],totarea[nb],err_tau(tottau[nb]),app,parinfo=parinfo)
;  wa[k,*]=wa[k,*]-p[0]
 ; p[0]=0.
  oplot, tavg, wa[k,*], psym=6,color=k*253./9.+1.
  errplot, tavg, wa[k,*]-saw[k,*],wa[k,*]+saw[k,*],color=k*253./9.+1.
  oplot, tavg, fe(tavg,p),thick=8, color=k*253./9.+1.
endfor


warr=findgen(112)
 contour, transpose([[warr],[warr]]),[0,1],warr,/cell_fill,nlevels=20,position=[0.79,0.2,0.81,0.9],/normal,$
   /noerase,xticks=1,yticks=1,xtickname=[' ',' '],ytickname=[' ',' ']  
 axis, yaxis=1,ystyle=1,yrange=[0,7.650],ytitle='Cloud base height (km)',yticks=6


device, /close
spawn, 'convert '+fn+'.ps '+fn+'.png'

fn=dir+'water_precip'
set_plot,'ps'
print, 'making plot :'+fn
loadct, 39, /silent
device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fn+'.ps'
device, xsize=20, ysize=20
 !p.font=1 & !p.thick=5 & !p.charsize=3.2 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
 !p.multi=0 & !x.margin=[7,3] &!x.omargin=[0,0]

; tvlct,107,255,117,100
 tvlct,255,117,173,248
; tvlct,245,255,107,247
; tvlct,240,136,000,249
u=where(tottau gt 14. and tottau lt 15.)
u1=where(tottau gt 24. and tottau lt 25.5)
u2=where(tottau gt 49. and tottau lt 51.)
u3=where(tottau gt 98. and tottau lt 102.)


cbins=findgen(26)*(26.-14.)/25.+14.
cavg=(findgen(25)+0.5)*(26.-14.)/25.+14.
ca=fltarr(25,4)*!values.f_nan
sca=fltarr(25,4)*!values.f_nan
for j=0, 24 do begin
  ns=where(totwater[u] gt cbins[j] and totwater[u] le cbins[j+1],nu)
  if nu gt 0 then begin
    ca[j,0]=mean(totarea[u[ns]],/nan)
    sca[j,0]=stddev(totarea[u[ns]],/nan)
  endif
  ns=where(totwater[u1] gt cbins[j] and totwater[u1] le cbins[j+1],nu)
  if nu gt 0 then begin
    ca[j,1]=mean(totarea[u1[ns]],/nan)
    sca[j,1]=stddev(totarea[u1[ns]],/nan)
  endif
  ns=where(totwater[u2] gt cbins[j] and totwater[u2] le cbins[j+1],nu)
  if nu gt 0 then begin
    ca[j,2]=mean(totarea[u2[ns]],/nan)
    sca[j,2]=stddev(totarea[u2[ns]],/nan)
  endif
  ns=where(totwater[u3] gt cbins[j] and totwater[u3] le cbins[j+1],nu)
  if nu gt 0 then begin
    ca[j,3]=mean(totarea[u3[ns]],/nan)
    sca[j,3]=stddev(totarea[u3[ns]],/nan)
  endif

endfor

plot, totwater[u], totarea[u], psym=2, title='Precipitable water content',/nodata,$
 xtitle='Precipitable water content (mm)',ytitle='Relative path integrated attenuation', xrange=[14,26],yrange=[20,80]-25.

oplot, cavg,ca[*,0], psym=6, color=70
errplot,cavg,ca[*,0]-sca[*,0],ca[*,0]+sca[*,0],color=70

r=correlate(totwater[u],totarea[u])
l=linfit(totwater[u],totarea[u])
x=findgen(30)
oplot, x, x*l[1]+l[0],color=70,linestyle=2

oplot, cavg,ca[*,1], psym=6, color=130
errplot,cavg,ca[*,1]-sca[*,1],ca[*,1]+sca[*,1],color=130
r1=correlate(totwater[u1],totarea[u1])
l1=linfit(totwater[u1],totarea[u1])
oplot, x, x*l1[1]+l1[0],color=130,linestyle=2

oplot, cavg,ca[*,2], psym=6, color=201
errplot,cavg,ca[*,2]-sca[*,2],ca[*,2]+sca[*,2],color=201
r2=correlate(totwater[u2],totarea[u2])
l2=linfit(totwater[u2],totarea[u2])
oplot, x, x*l2[1]+l2[0],color=201,linestyle=2

oplot, cavg,ca[*,3], psym=6, color=250
errplot,cavg,ca[*,3]-sca[*,3],ca[*,3]+sca[*,3],color=250
r3=correlate(totwater[u3],totarea[u3])
l3=linfit(totwater[u3],totarea[u3])
oplot, x, x*l3[1]+l3[0],color=250,linestyle=2

ta="164b ;"
legend, ['!9'+string(ta)+'!X=15, R='+string(r,format='(F4.2)'),'!9'+string(ta)+'!X=25, R='+string(r1,format='(F4.2)'),$
         '!9'+string(ta)+'!X=50, R='+string(r2,format='(F4.2)'),'!9'+string(ta)+'!X=100, R='+string(r3,format='(F4.2)')],textcolors=[70,130,201,250],box=0,charsize=2.8
device, /close
spawn, 'convert '+fn+'.ps '+fn+'.png'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;plot examples from the downwind/upwind days
l='/'
date='20120620'

print, 'restoring :'+dir+date+l+date+'_cod_compi_2.out'
restore, dir+date+l+date+'_cod_comp_2.out'
get_precip_water, tmhrs,date,water
get_ceil, tmhrs, date, base


a=where(tmhrs gt 12. and tmhrs lt 14. and finite(area) eq 1) ;from fire
b=where(tmhrs gt 15. and tmhrs lt 18. and finite(area) eq 1)  ;from denver

u=where(totwater gt min(water[[a,b]]) and totwater lt max(water[[a,b]]) and totbase gt min(base[[a,b]]) $
        and totbase lt max(base[[a,b]]))

;stop
fn=dir+'fire_case'
set_plot,'ps'
print, 'making plot :'+fn
loadct, 39, /silent
device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fn+'.ps'
device, xsize=45, ysize=20
 !p.font=1 & !p.thick=5 & !p.charsize=3.2 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
 !p.multi=[0,2,1] & !x.margin=[6,3] &!x.omargin=[0,0]

; tvlct,107,255,117,100
; tvlct,255,117,173,248
; tvlct,245,255,107,247
; tvlct,240,136,000,249
tvlct, 140,140,140,200
plot, tottau[u], totarea[u], psym=3, title='Water vapor',xtitle='Cloud optical depth',$
 ytitle='Path integrated attenuation', xrange=[0,200],yrange=[20,80],/nodata

ba=fltarr(50)
sba=fltarr(50)
aa=fltarr(50)
saa=fltarr(50)
ab=fltarr(50)
sab=fltarr(50)
bx=ba & sbx=sba


for j=0,49 do begin
  ns=where(tottau[u] gt tbins[j] and tottau[u] le tbins[j+1],nu)
  if nu gt 0 then begin
    ba[j]=mean(totarea[u[ns]],/nan) & sba[j]=stddev(totarea[u[ns]],/nan)
    bx[j]=mean(totoxa[u[ns]],/nan)  & sbx[j]=stddev(totoxa[u[ns]],/nan)
  endif
  na=where(tau[a] gt tbins[j] and tau[a] le tbins[j+1],au)
  if au gt 0 then begin
    aa[j]=mean(area[a[na]],/nan) & saa[j]=stddev(area[a[na]],/nan)
  endif
  nb=where(tau[b] gt tbins[j] and tau[b] le tbins[j+1],bu)
  if bu gt 0 then begin
    ab[j]=mean(area[b[nb]],/nan) & sab[j]=stddev(area[b[nb]],/nan) 
  endif
endfor
oplot, tavg,ba,psym=6,color=200
errplot,tavg,ba-sba,ba+sba,color=200
app=[1,1,200]
p=mpfitfun('fe',tottau[u],totarea[u],err_tau(tottau[u]),app,parinfo=parinfo)
oplot, findgen(200),fe(findgen(200),p),color=200,linestyle=2

oplot, tavg, aa, psym=6, color=250
errplot, tavg, aa-saa,aa+saa, color=250
oplot, tavg, ab, psym=6, color=70
errplot, tavg, ab-sab, ab+sab, color=70

paa=mpfitfun('fe',tau[a],area[a],err_tau(tau[a]),app,parinfo=parinfo)
pab=mpfitfun('fe',tau[b],area[b],err_tau(tau[b]),app,parinfo=parinfo)

oplot, tavg, fe(tavg,paa),thick=8, color=250
oplot, tavg, fe(tavg,pab),thick=8, color=70

;oplot, tau[a],area[a],psym=3, color=250
;oplot, tau[b],area[b],psym=3, color=70

legend, ['Baseline','Downwind of fire','Upwind of fire'],box=0, textcolors=[200,250,70],charsize=2.5,/right,/bottom

;now plot the histograms
ha=histogram(area[a]-fe(tau[a],p),nbins=50)
hb=histogram(area[b]-fe(tau[b],p),nbins=50)
bina=findgen(50)*(max(area[a]-fe(tau[a],p))-min(area[a]-fe(tau[a],p)))/50.+min(area[a]-fe(tau[a],p))
binb=findgen(50)*(max(area[b]-fe(tau[b],p))-min(area[b]-fe(tau[b],p)))/50.+min(area[b]-fe(tau[b],p))


ha=float(ha)/float(max(ha))
hb=float(hb)/float(max(hb))

plot,bina,ha,psym=10,title='Distribution of path enhancement    ',xtitle='Difference in path enhancement',$
 ytitle='Normalized Frequency',xrange=[-12,35]
oplot, bina,ha, psym=10, color=250,thick=8
oplot, binb,hb, psym=10, color=70,thick=8

legend, ['Downwind of fire','Upwind of fire'],textcolors=[250,70],box=0,charsize=2.5,/right


device, /close
spawn, 'convert '+fn+'.ps '+fn+'.png'

fn=dir+'water_ox_cod'
set_plot,'ps'
print, 'making plot :'+fn
loadct, 39, /silent
device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fn+'.ps'
device, xsize=20, ysize=20
 !p.font=1 & !p.thick=5 & !p.charsize=3.2 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
 !p.multi=0 & !x.margin=[6,3] &!x.omargin=[0,0]

plot, tottau[u], totarea[u], psym=3, title='Water vapor per Oxygen-A',xtitle='Cloud optical depth',$
 ytitle='Relative path integrated attenuation', xrange=[0,200],yrange=[20,80]-20.,/nodata

oplot, tavg,ba/bx,psym=6
errplot,tavg,ba/bx-sba/sbx,ba/bx+sba/sbx


device, /close
spawn, 'convert '+fn+'.ps '+fn+'.png'



stop
end
