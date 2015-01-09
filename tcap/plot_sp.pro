; program to make plots of spectra from modeled and measured by 4STAR
; plots used to put in proposal...

@/home/leblanc/SSFR3/pro/get_params.pro
pro plot_sp

dir='/home/leblanc/TCAP/4STAR/'

print, 'restoring 4STAR spectra'
restore, dir+'sp_4STAR_20130219.out'
wvm=[wvl[283:1019],wvl[1062:*]]
spi=fltarr(n_elements(wvm),198)
for i=0, 197 do spi[*,i]=[sp[283:1019,i],sp[1062:*,i]]
spm=reform(spi[*,170])
spp=reform(spi[*,90])

; get the response function
print, 'restoring response function'
restore, dir+'4STAR_resp.out'
cal=smooth([resp[283:1019],resp[1062:*]]*1000.,3)

    fp=dir+'sp_comp'
  ; for a cloud of tau 100, ref 15, wp 0
  t=17 & r=5 & w=0 & t2=8 & w2=1 & r2=12 & t3=1
  print, 'making plot :'+fp
  set_plot, 'ps'
  loadct, 39, /silent
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
  device, xsize=40, ysize=20
   !p.font=1 & !p.thick=5 & !p.charsize=2.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
   !p.multi=[0,2,1] & !x.margin=[6,3] & !y.margin=[3.5,1]

  ; prepare the approx slit function
  x=findgen(21) & xo=10. & sig=4./2.35
  f=exp(-(x-xo)^2/(2.*sig^2))/(sig*sqrt(2.*!PI))

  ; now restore the model spectra
  fn='/home/leblanc/TCAP/model/sp_sea_liq.out'
  restore, fn
  wvl_liq=wvl
  sp_liq=sp_liq/1000.
  sp_liq1=sp_liq
  tau_liq=tau & ref_liq=ref

  ; now set the modeled spectra to the same wavelength grid as measurement
  sp_liq2=fltarr(n_elements(tau),n_elements(ref),n_elements(wvm))
  for t=0,n_elements(tau)-1 do begin
    for r=0,n_elements(ref)-1 do begin 
      for i=10, n_elements(wvl_liq)-11 do sp_liq1[t,r,i]=total(sp_liq[t,r,i-10:i+10]*f)
      sp_liq2[t,r,*]=interpol(sp_liq1[t,r,*],wvl,wvm)
    endfor
  endfor
  sp_liq=sp_liq2
 ; wvl_liq=wvm

  spp=spp/cal
  fn='/home/leblanc/TCAP/model/sp_sea_ice.out'
  restore, fn
  wvl_ice=wvl
  sp_ice2=fltarr(n_elements(tau),n_elements(ref),1301)
  sp_ice21=sp_ice2
  sp_ice3=fltarr(n_elements(tau),n_elements(ref),n_elements(wvm))
  for t=0,n_elements(tau)-1 do begin 
    for r=0,n_elements(ref)-1 do begin
      sp_ice2[t,r,*]=interpol(smooth(sp_ice[t,r,*]/1000.,10),wvl_ice,wvl_liq)
      sp_ice21[t,r,*]=sp_ice2[t,r,*]
      for i=10, n_elements(wvl_liq)-11 do sp_ice21[t,r,i]=total(sp_ice2[t,r,i-10:i+10]*f)
      sp_ice3[t,r,*]=interpol(sp_ice21[t,r,*],wvl_liq,wvm)
    endfor
  endfor
  wvl_liq=wvm
  sp_ice=sp_ice3
  wvl_ice=wvl_liq
  restore, '/home/leblanc/SSFR3/data/sp_clear.out'
;  wvl=zenlambda

  plot, wvl_liq, sp_liq[0,0,*],/nodata, yr=[0,1.0],ytitle='Normalized radiance',xtit='Wavelength (nm)',xrange=[400,1700]
  loadct, 0, /silent
  ;par 11
  polyfill, [550.,550.,680.,680.,550.],[0,0.9,0.9,0,0],color=230
  xyouts, 649., 0.875,'}',charsize=5.1,orientation=90,color=230,alignment=0.
  xyouts, 555., 0.95,'!9h!X!D11!N',color=200

  ;par7
;  polyfill, [1000.,1000.,1050.,1050.,1000.],[0,0.75,0.75,0,0],color=244
;  xyouts, 1041.,0.735,'}',charsize=2.2,orientation=90.,color=244,alignment=0.
;  xyouts, 1000.,0.78,'!9h!X!D7!N',color=200
  ;par9
;  polyfill, [1000.,1000.,1077.,1077.,1000.],[0,0.65,0.65,0,0],color=230
;  xyouts, 1061.,0.63,'}',charsize=3.2,orientation=90.,color=230,alignment=0.
;  xyouts, 1018.,0.68,'!9h!X!D9!N',color=200
  ;par1
  polyfill, [1000.,1000.,1077.,1077.,1000.],[0,0.40,0.40,0,0],color=215
  xyouts, 1061.,0.385,'}',charsize=3.10,orientation=90.,color=215,alignment=0.
  xyouts, 1018.,0.43,'!9h!X!D1!N',color=200
  ;par 12
;  plots,[1000.,1000.],[0.,0.47],color=150
;  xyouts, 974.,0.49,'!9h!X!D12!N',color=150
  ;par 13
;  plots,[1040.,1040.],[0.,0.41],color=150
;  xyouts, 1018.,0.43,'!9h!X!D13!N',color=150
  ;par 14
;  plots,[1065.,1065.],[0.,0.35],color=150
;  xyouts, 1042.,0.37,'!9h!X!D14!N',color=150
  
  ;par5
  polyfill, [1248.,1248.,1270.,1270.,1248.],[0,0.25,0.25,0,0],color=240
  xyouts, 1225.,0.268,'!9h!X!D5!N',color=200
  ;par 10
;  polyfill, [1200.,1200.,1300.,1300.,1200.],[0,0.35,0.35,0,0],color=215
;  xyouts, 1278.,0.335,'}',charsize=4.0,orientation=90,color=215,alignment=0.
;  xyouts, 1208.,0.388,'!9h!X!D10!N',color=200 
  ;par4
;  plots,[1200.,1200.,1219.,1237.,1237.],[0.,0.24,0.26,0.24,0.],color=150
;  xyouts, 1196.,0.28,'!9h!X!D4!N',color=150
  ;par2 
  plots,[1193.,1193.],[0,0.33],color=180  
  xyouts,1174.,0.35,'!9h!X!D2!N',color=200

  ;par 15
  polyfill, [1565.,1565.,1634.,1634.,1565.],[0,0.23,0.23,0,0],color=244
  xyouts, 1620.,0.218,'}',charsize=2.9,orientation=90,color=244,alignment=0.
  xyouts, 1570.,0.26,'!9h!X!D15!N',color=200
  ;par 6
  polyfill, [1565.,1565.,1644.,1644.,1565.],[0,0.13,0.13,0,0],color=220
  xyouts, 1626.,0.118,'}',charsize=2.97,orientation=90,color=220,alignment=0.
  xyouts, 1580.,0.16,'!9h!X!D6!N',color=200
  ;par8
;  polyfill, [1493.,1493.,1600.,1600.,1493.],[0,0.15,0.15,0,0],color=215
;  xyouts, 1578.,0.135,'}',charsize=4.2,orientation=90,color=215,alignment=0.
;  xyouts, 1508.,0.188,'!9h!X!D8!N',color=200
  ;par3
  plots, [1492,1492],[0,0.2],color=180
  xyouts,1477.,0.22,'!9h!X!D3!N',color=200


  loadct, 16, /silent
  tvlct,0,200,100,20
  oplot, wvl_liq, smooth(sp_liq[0,1,*]/max(smooth(sp_liq[0,1,*],4)),4),color=20 , thick=5,linestyle=2 
  oplot, wvl_liq, smooth(sp_liq[2,1,*]/max(smooth(sp_liq[2,1,*],4)),4),color=130, thick=5,linestyle=2
  oplot, wvl_liq, smooth(sp_liq[2,3,*]/max(smooth(sp_liq[2,3,*],4)),4),color=250, thick=5,linestyle=2

  oplot, wvl_ice, smooth(sp_ice[1,2,*]/max(smooth(sp_ice[1,2,*],4)),4),color=20 ;, thick=3,linestyle=2
  oplot, wvl_ice, smooth(sp_ice[7,2,*]/max(smooth(sp_ice[7,2,*],4)),4),color=130;, thick=3,linestyle=2
  oplot, wvl_ice, smooth(sp_ice[7,4,*]/max(smooth(sp_ice[7,4,*],4)),4),color=250;, thick=3,linestyle=2
  tvlct, 0, 246,255,100
  oplot,wvl, rad/max(rad),color=100 ;clear
  tvlct,255,0,0,254
  tvlct,0,0,0,0
  oplot,wvm, smooth(spp/max(smooth(spp,3)),3),color=254, thick=6

  legend, ['4STAR TCAP observation','!9t!X=5, r!Deff!N=15 !9m!Xm','!9t!X=60, r!Deff!N=15 !9m!Xm','!9t!X=60, r!Deff!N=25 !9m!Xm','Clear'],$
          textcolors=[254,20,130,250, 100],box=0,charsize=2.2,/right
  legend, ['Ice','Liquid'],linestyle=[0,2],thick=[5,5],box=0,charsize=2.2,/right,/bottom,pspacing=1.2,position=[1680,0.57]

  pars=fltarr(n_elements(tau),n_elements(ref),2,16)+!values.f_nan
  ti=[1,5,6,7]
  ri=[0,2,3,4]

  for ta=0,n_elements(tau)-1 do begin 
    for re=0, n_elements(ref)-1 do begin
 ;       get_params, wvm, smooth(interpol(smooth(sp_liq[ta,re,*],3),wvl_liq,wvm),3),par
        if ta lt 4 and re lt 4 then begin 
          get_params, wvl_liq, smooth(sp_liq[ta,re,*],3),par
          pars[ti[ta],ri[re],0,*]=par
        endif
;        get_params, wvm, smooth(interpol(smooth(sp_ice[ta,re,*],3),wvl_ice,wvm),3),par
        get_params, wvl_ice, smooth(sp_ice[ta,re,*],3),par
        pars[ta,re,1,*]=par
    endfor
  endfor

  parm=fltarr(198,16)
;  spr=fltarr(n_elements(wvl),198)
spr=spi
  for i=0,197 do begin
;    spr[*,i]=spi[*,i] ;interpol(spi[*,i],wvm,wvl)
;    get_params, wvm, smooth(spr[*,i],3),par
    get_params, wvm, smooth(spr[*,i]/cal,3),par
    parm[i,*]=par
  endfor

  for p=0, 14 do if (min(parm[*,p]) ge min(pars[*,*,*,p],/nan) and max(parm[*,p]) le max(pars[*,*,*,p],/nan)) then print, p

;ts=[4,11,13,16] ;5,20,60, 90
;rs=[1,5,7,9] ;5,15,20, 25
;ws=[0,1]
ts=ti;[0,1,2,3]
rs=ri;[0,1,2,3]
p1=4 & p2=5 
loadct, 39,/silent
!y.style=0
!x.style=0
xr=[min(pars[0:5,*,1,p1]),max(pars[0:5,*,1,p1])]
yr=[min(pars[0:5,*,1,p2]),max(pars[0:5,*,1,p2])]
  plot, pars[0:1,0,0,p1],pars[0:1,0,0,p2],xtitle='!9h!X!D'+strtrim(p1+1,2)+'!N - average normalized radiance',ytitle='!9h!X!D'+strtrim(p2+1,2)+'!N - average normalized radiance',yr=yr,xr=xr,/nodata
;  for j=0, 3 do begin
;    oplot, pars[ts[j],rs,0,p1],pars[ts[j],rs,0,p2], color=80, thick=j*2.
;    oplot, pars[ts[j],rs,1,p1],pars[ts[j],rs,1,p2], color=120
;    oplot, pars[ts,rs[j],0,p1],pars[ts,rs[j],0,p2], psym=-5, linestyle=2,color=80,thick=j*2.
;    oplot, pars[ts,rs[j],1,p1],pars[ts,rs[j],1,p2], psym=-5, linestyle=2,color=120
;  endfor
  for ta=0, n_elements(tau)-3 do begin
    oplot, pars[ta,*,1,p1],pars[ta,*,1,p2],color=120;, thick=ta*1.5
    xyouts, pars[ta,0,1,p1],pars[ta,0,1,p2]+0.001,color=120,'!9t!X='+string(tau[ta],format='(I2)'),charsize=2.0,alignment=(ta eq 5)?0.0:1.0
  endfor
  for re=0, n_elements(ref)-1 do begin
    oplot, pars[0:5,re,1,p1],pars[0:5,re,1,p2],psym=-5,linestyle=2.,color=80; , thick=re*2. 
    xyouts, pars[5,re,1,p1]+0.002,pars[5,re,1,p2]-((re le 1)?0.003:0.000),color=80,'r!Deff!N='+string(ref[re],format='(I2)')+' !9m!Xm',charsize=1.8
  endfor

    oplot, parm[*,p1],parm[*,p2],psym=6,color=250
 ; plots,pars[0,3,1,p1],pars[0,3,1,p2],psym=2 
  ; aqua modis tau: 2.03 and ref: 23.2 for index 163 (17.4166 UTC) 
  legend, ['4STAR TCAP observations'],box=0,textcolor=[250],charsize=2.2
  device, /close
  spawn, 'convert "'+fp+'.ps" "'+fp+'.png"'

stop

set_plot, 'x'
loadct, 39,/silent
device, decomposed=0
!p.multi=0
for p=0,14 do begin
  xr=[min([min(pars[*,*,*,p],/nan),min(parm[*,p])]),max([max(pars[*,*,*,p],/nan),max(parm[*,p])])]
  for pp=p+1,14 do begin
    yr=[min([min(pars[*,*,*,pp],/nan),min(parm[*,pp])]),max([max(pars[*,*,*,pp],/nan),max(parm[*,pp])])]
    plot, pars[*,*,*,p],pars[*,*,*,pp],xtitle=strtrim(p),ytitle=strtrim(pp), psym=2,xr=xr,yr=yr
    for i=0,n_elements(tau)-1 do begin
      oplot, pars[i,*,0,p],pars[i,*,0,pp],psym=-2, color=(i+1)*25
      oplot, pars[i,*,1,p],pars[i,*,1,pp],psym=-5, color=(i+1)*25, thick=2, linestyle=2
    endfor
    for j=0, n_elements(ref)-1 do begin
      oplot, pars[*,j,0,p],pars[*,j,0,pp],psym=2, color=(j+1)*25
      oplot, pars[*,j,1,p],pars[*,j,1,pp],psym=5, color=(j+1)*25
    endfor
    oplot,pars[0,2:3,1,p],pars[0,2:3,1,pp],linestyle=4,color=220
    oplot,parm[*,p],parm[*,pp],psym=5, color=250
    print, 'click to continue'
    cursor, x,y
  endfor
endfor
stop
end
