; program built to plot the data from the rads test
pro plot_rtm_rads

dir='/home/leblanc/rtm_aero/data/tests_rad/'
restore, dir+'tests_rad_highres.out'
wvl=findgen(601)+350.
rad=rad/1000.

  set_plot, 'ps'
  loadct, 39,/silent
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8.
  device, filename=dir+'rtm_rad.ps'
  device, xsize=40, ysize=40
  !p.font=1 & !p.thick=5
  !p.charsize=1.8 & !x.style=1
  !y.style=1 & !z.style=1
  !y.thick=1.8 & !x.thick=1.8
  !x.margin=[6,3]
  !y.margin=[3,2]
  !p.multi=[0,2,2]

  plot, tau550, rad[0,0,*,300], title='Radiance vs. tau', ytitle='Radiance (W/m!U2!N sr)', xtitle='Tau', yrange=[0.0,0.25]
  oplot, tau550, rad[1,0,*,300], color=50
  oplot, tau550, rad[3,0,*,300], color=100
  oplot, tau550, rad[5,0,*,300], color=150
  oplot, tau550, rad[7,0,*,300], color=200
  oplot, tau550, rad[9,0,*,300], color=250
  
  legend,['ASY:'+string(asy[[0,1,3,5,7,9]],format='(F4.2)')], textcolors=[0,50,100,150,200,250], box=0, /right,/bottom
  
  plot, tau550, rad[0,0,*,300], title='Radiance vs. tau', ytitle='Radiance (W/m!U2!N sr)', xtitle='Tau', yrange=[0.0,0.25]
  oplot, tau550, rad[0,3,*,300], color=50
  oplot, tau550, rad[0,6,*,300], color=100
  oplot, tau550, rad[0,10,*,300], color=150
  oplot, tau550, rad[0,14,*,300], color=200
  oplot, tau550, rad[0,19,*,300], color=250
  
  legend,['SSA:'+string(ssa[[0,3,6,10,14,19]],format='(F4.2)')], textcolors=[0,50,100,150,200,250], box=0, /right,/bottom

  plot, tau550, irrad[0,0,*,300], title='Irradiance vs. tau', ytitle='Irradiance (W/m!U2!N)', xtitle='Tau', yrange=[0.0,1.5]
  oplot, tau550, irrad[1,0,*,300], color=50
  oplot, tau550, irrad[3,0,*,300], color=100
  oplot, tau550, irrad[5,0,*,300], color=150
  oplot, tau550, irrad[7,0,*,300], color=200
  oplot, tau550, irrad[9,0,*,300], color=250
  
  legend,['ASY:'+string(asy[[0,1,3,5,7,9]],format='(F4.2)')], textcolors=[0,50,100,150,200,250], box=0, /right,/bottom
  
  plot, tau550, irrad[0,0,*,300], title='Irradiance vs. tau', ytitle='Irradiance (W/m!U2!N)', xtitle='Tau', yrange=[0.0,1.5]
  oplot, tau550, irrad[0,3,*,300], color=50
  oplot, tau550, irrad[0,6,*,300], color=100
  oplot, tau550, irrad[0,10,*,300], color=150
  oplot, tau550, irrad[0,14,*,300], color=200
  oplot, tau550, irrad[0,19,*,300], color=250
  
  legend,['SSA:'+string(ssa[[0,3,6,10,14,19]],format='(F4.2)')], textcolors=[0,50,100,150,200,250], box=0, /right,/bottom
  device,/close
  spawn, 'convert '+dir+'rtm_rad.ps '+dir+'rtm_rad_hires.png'
  spawn, 'rm -f '+dir+'rtm_rad.ps'
  
    set_plot, 'ps'
  loadct, 39,/silent
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8.
  device, filename=dir+'rtm_rad.ps'
  device, xsize=40, ysize=40
  !p.font=1 & !p.thick=5
  !p.charsize=1.8 & !x.style=1
  !y.style=1 & !z.style=1
  !y.thick=1.8 & !x.thick=1.8
  !x.margin=[6,3]
  !y.margin=[3,2]
  !p.multi=[0,2,2]
  
  plot, wvl, rad[0,0,0,*], title='Radiance with varrying asymmetry parameters v.s. wvls', xtitle='Wavelengths (nm)', ytitle='Radiance (W/m!U2!N nm sr)', yrange=[0,0.3]
  
  for i=0,19, 4 do begin
  oplot, wvl, rad[0,0,i,*], linestyle=0, color=i*15 
  oplot, wvl, rad[3,0,i,*], linestyle=1, color=i*15
  oplot, wvl, rad[6,0,i,*], linestyle=2, color=i*15
  oplot, wvl, rad[9,0,i,*], linestyle=5, color=i*15
  print, i, i*15
  endfor
  legend, ['ASY:'+string(asy[[0,3,6,9]],format='(F4.2)')],linestyle=[0,1,2,5], box=0
  legend, ['TAU:'+string(tau550[[0,4,9,14,19]],format='(F4.2)')], textcolors=[0,60, 120, 180, 240], box=0, /right
  
    plot, wvl, rad[0,0,0,*], title='Radiance with varrying SSA v.s. wvls', xtitle='Wavelengths (nm)', ytitle='Radiance (W/m!U2!N nm sr)', yrange=[0,0.3]
  
  for i=0,20-1,4 do begin
  oplot, wvl, rad[0,0,i,*], linestyle=0, color=i*15 
  oplot, wvl, rad[0,6,i,*], linestyle=1, color=i*15
  oplot, wvl, rad[0,12,i,*], linestyle=2, color=i*15
  oplot, wvl, rad[0,18,i,*], linestyle=5, color=i*15
  endfor
  legend, ['SSA:'+string(asy[[0,6,12,18]],format='(F4.2)')],linestyle=[0,1,2,5], box=0
  legend, ['TAU:'+string(tau550[[0,4,9,14,19]],format='(F4.2)')], textcolors=[0,60, 120, 180, 240], box=0, /right
  
    plot, wvl, irrad[0,0,0,*], title='Irradiance with varrying asymmetry parameters v.s. wvls', xtitle='Wavelengths (nm)', ytitle='Irradiance (W/m!U2!N nm)', yrange=[0,2.0]
  
  for i=0,20-1,4 do begin
  oplot, wvl, irrad[0,0,i,*], linestyle=0, color=i*15 
  oplot, wvl, irrad[3,0,i,*], linestyle=1, color=i*15
  oplot, wvl, irrad[6,0,i,*], linestyle=2, color=i*15
  oplot, wvl, irrad[9,0,i,*], linestyle=5, color=i*15
  endfor
  legend, ['ASY:'+string(asy[[0,3,6,9]],format='(F4.2)')],linestyle=[0,1,2,5], box=0
  legend, ['TAU:'+string(tau550[[0,4,9,14,19]],format='(F4.2)')], textcolors=[0,60, 120, 180, 240], box=0, /right
  
    plot, wvl, irrad[0,0,0,*], title='Irradiance with varrying SSA v.s. wvls', xtitle='Wavelengths (nm)', ytitle='IIrradiance (W/m!U2!N nm)', yrange=[0,2.0]
  
  for i=0,20-1,4 do begin
  oplot, wvl, irrad[0,0,i,*], linestyle=0, color=i*15 
  oplot, wvl, irrad[0,6,i,*], linestyle=1, color=i*15
  oplot, wvl, irrad[0,12,i,*], linestyle=2, color=i*15
  oplot, wvl, irrad[0,18,i,*], linestyle=5, color=i*15
  endfor
  legend, ['SSA:'+string(asy[[0,6,12,18]],format='(F4.2)')],linestyle=[0,1,2,5], box=0
  legend, ['TAU:'+string(tau550[[0,4,9,14,19]],format='(F4.2)')], textcolors=[0,60, 120, 180, 240], box=0, /right
  
  device,/close
  spawn, 'convert '+dir+'rtm_rad.ps '+dir+'rtm_rad_hires_wvl.png'
  spawn, 'rm -f '+dir+'rtm_rad.ps'
  
  stop

restore, dir+'tests_rad.out'
wvl=findgen(601)+350.
rad=rad/1000.

tau_star=fltarr(5,5,5,601)
names=strarr(5,5,5,601)
for i=0,4 do begin
  for j=0,4 do begin
    for k=0,4 do begin
      tau_star[i,j,k,*]=(1.-asy[i]*ssa[j])*tau550[k]
      names[i,j,k,*]='ASY:'+string(asy[i],format='(F4.2)')+' SSA:'+string(ssa[j],format='(F4.2)')+' TAU:'+string(tau550[k],format='(F4.2)')
    endfor
  endfor
endfor
;stop
  set_plot, 'ps'
  loadct, 39,/silent
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8.
  device, filename=dir+'rtm_rad.ps'
  device, xsize=60, ysize=60
  !p.font=1 & !p.thick=5
  !p.charsize=1.8 & !x.style=1
  !y.style=1 & !z.style=1
  !y.thick=1.8 & !x.thick=1.8
  !x.margin=[3,3]
  !y.margin=[3,2]
  !p.multi=[0,5,5]
  

  ; asy, ssa, tau550
  ; irrad, rad   
  for i=0, 4 do begin
    for j=0,4 do begin
    plot, rad[0,i,j,*], irrad[0,i,j,*], title='SSA:'+string(ssa[i],format='(F4.2)')+' TAU:'+string(tau550[j],format='(F4.2)'), xtitle='Wavelength (nm)', yrange=[0,2.], ystyle=8, xrange=[0,0.3]
    oplot,rad[1,i,j,*], irrad[1,i,j,*], color=70
    oplot,rad[2,i,j,*], irrad[2,i,j,*], color=130
    oplot,rad[3,i,j,*], irrad[3,i,j,*], color=190
    oplot,rad[4,i,j,*], irrad[4,i,j,*], color=250

    ;axis, yaxis=1,ystyle=1, yrange=[0.0,0.3],/save
    ;oplot,wvl, rad[0,i,j,*], linestyle=2
    ;oplot,wvl, rad[1,i,j,*], color=70,  linestyle=2
    ;oplot,wvl, rad[2,i,j,*], color=130, linestyle=2
    ;oplot,wvl, rad[3,i,j,*], color=190, linestyle=2
    ;oplot,wvl, rad[4,i,j,*], color=250, linestyle=2
    endfor
  endfor

     legend, ['rad','irrad'],linestyle=[2,0],box=0, /right, pspacing=0.6
     legend, 'ASY:'+[string(asy[0],format='(F4.2)'),string(asy[1],format='(F4.2)'),string(asy[2],format='(F4.2)'),string(asy[3],format='(F4.2)'),string(asy[4],format='(F4.2)')], box=0, /bottom, textcolors=[0,70,130,190,250]
     device, /close
  spawn, 'convert '+dir+'rtm_rad.ps '+dir+'rtm_rad.png'
  spawn, 'rm -f '+dir+'rtm_rad.ps'
  
    set_plot, 'ps'
  loadct, 39,/silent
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8.
  device, filename=dir+'rtm_rad.ps'
  device, xsize=60, ysize=60
  !p.font=1 & !p.thick=5
  !p.charsize=1.8 & !x.style=1
  !y.style=1 & !z.style=1
  !y.thick=1.8 & !x.thick=1.8
  !x.margin=[3,3]
  !y.margin=[3,2]
  !p.multi=[0,5,5]
  

  ; asy, ssa, tau550
  ; irrad, rad   
  for i=0, 4 do begin
    for j=0,4 do begin
    plot, rad[j,i,0,*], irrad[j,i,0,*], title='SSA:'+string(ssa[i],format='(F4.2)')+' ASY:'+string(asy[j],format='(F4.2)')+' TAU:'+string(tau550[0],format='(F4.2)'), xtitle='Radiance',ytitle='Irradiance', yrange=[0,2.], ystyle=8, xrange=[0,0.2]
    endfor
  endfor

  device, /close
  spawn, 'convert '+dir+'rtm_rad.ps '+dir+'rtm_rad_tau.png'
  spawn, 'rm -f '+dir+'rtm_rad.ps'
  
    set_plot, 'ps'
  loadct, 39,/silent
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8.
  device, filename=dir+'rtm_rad.ps'
  device, xsize=60, ysize=40
  !p.font=1 & !p.thick=5
  !p.charsize=1.8 & !x.style=1
  !y.style=1 & !z.style=1
  !y.thick=1.8 & !x.thick=1.8
  !x.margin=[3,3]
  !y.margin=[3,2]
  !p.multi=[0,3,2]
  
  ii=lindgen(601)*125
  
  i_36=[[29+ii],[69+ii]]
  i_48=[[26+ii],[59+ii]]
  i_52=[[25+ii],[124+ii]]
  i_60=[[53+ii],[119+ii]]
  i_72=[[79+ii],[51+ii]]
  i_80=[[78+ii],[109+ii]]
  ins=[[[i_36]],[[i_48]],[[i_52]],[[i_60]],[[i_72]],[[i_80]]]
  ti=['0.36','0.48','0.52','0.60','0.72','0.80']
  
  for i=0,5 do begin
    plot, rad[ins[*,0,i]], irrad[ins[*,0,i]], title='Tau star: '+ti[i], xtitle='Radiance (W/m!U2!N nm sr)', ytitle='Irradiance (W/m!U2!N nm)', yrange=[0.,2.], xrange=[0.,0.3]
    oplot,rad[ins[*,1,i]], irrad[ins[*,1,i]], color=250
    legend, names[ins[0,*,i]],textcolors=[0,250], box=0
 endfor

     device, /close
  spawn, 'convert '+dir+'rtm_rad.ps '+dir+'rtm_tau_star.png'
  spawn, 'rm -f '+dir+'rtm_rad.ps'

; plot change of irradiance and radiance with change of tau star 
;for ni=140, 160 do begin
ni=150
set_plot, 'ps'
  loadct, 39,/silent
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8.
  device, filename=dir+'rtm_rad.ps'
  device, xsize=40, ysize=60
  !p.font=1 & !p.thick=5
  !p.charsize=1.8 & !x.style=1
  !y.style=1 & !z.style=1
  !y.thick=1.8 & !x.thick=1.8
  !x.margin=[6,6]
  !y.margin=[4,2]
  !p.multi=[0,2,3]
 
  plot, tau_star[*,*,*,ni], irrad[*,*,*,ni],yrange=[1.40,1.75], xtitle='Tau Star', ytitle='Irradiance', psym=1, title='Irradiance at '+string(wvl[ni],format='(F5.1)')+' nm'
  oplot, tau_star[0,*,*,ni],irrad[0,*,*,ni], color=30
  oplot, tau_star[1,*,*,ni],irrad[1,*,*,ni], color=80
  oplot, tau_star[2,*,*,ni],irrad[2,*,*,ni], color=130
  oplot, tau_star[3,*,*,ni],irrad[3,*,*,ni], color=180
  oplot, tau_star[4,*,*,ni],irrad[4,*,*,ni], color=250 

legend, ['ASY:'+string(asy,format='(F4.2)')],box=0, textcolors=[30,80,130,180,250],/bottom
; asy, ssa, tau550

  plot, tau_star[*,*,*,ni], rad[*,*,*,ni], xtitle='Tau Star',yrange=[0.,0.3], ytitle='Radiance', psym=1, title='Radiance at '+string(wvl[ni],format='(F5.1)')+' nm'
  oplot, tau_star[0,*,*,ni],rad[0,*,*,ni], color=30
  oplot, tau_star[1,*,*,ni],rad[1,*,*,ni], color=80
  oplot, tau_star[2,*,*,ni],rad[2,*,*,ni], color=130
  oplot, tau_star[3,*,*,ni],rad[3,*,*,ni], color=180
  oplot, tau_star[4,*,*,ni],rad[4,*,*,ni], color=250

legend, ['ASY:'+string(asy,format='(F4.2)')],box=0, textcolors=[30,80,130,180,250]

  plot, tau_star[*,*,*,ni], irrad[*,*,*,ni], xtitle='Tau Star',yrange=[1.40,1.75], ytitle='Irradiance', psym=1, title='Irradiance at '+string(wvl[ni],format='(F5.1)')+' nm'
  oplot, tau_star[*,0,*,ni],irrad[*,0,*,ni], color=30
  oplot, tau_star[*,1,*,ni],irrad[*,1,*,ni], color=80
  oplot, tau_star[*,2,*,ni],irrad[*,2,*,ni], color=130
  oplot, tau_star[*,3,*,ni],irrad[*,3,*,ni], color=180
  oplot, tau_star[*,4,*,ni],irrad[*,4,*,ni], color=250

legend, ['SSA:'+string(ssa,format='(F4.2)')],box=0, textcolors=[30,80,130,180,250],/bottom
; asy, ssa, tau550

  plot, tau_star[*,*,*,ni], rad[*,*,*,ni], xtitle='Tau Star',yrange=[0.,0.3], ytitle='Radiance', psym=1, title='Radiance at '+string(wvl[ni],format='(F5.1)')+' nm'
  oplot, tau_star[*,0,*,ni],rad[*,0,*,ni], color=30
  oplot, tau_star[*,1,*,ni],rad[*,1,*,ni], color=80
  oplot, tau_star[*,2,*,ni],rad[*,2,*,ni], color=130
  oplot, tau_star[*,3,*,ni],rad[*,3,*,ni], color=180
  oplot, tau_star[*,4,*,ni],rad[*,4,*,ni], color=250

legend, ['SSA:'+string(ssa,format='(F4.2)')],box=0, textcolors=[30,80,130,180,250]

  plot, tau_star[*,*,*,ni], irrad[*,*,*,ni], xtitle='Tau Star',yrange=[1.40,1.75], ytitle='Irradiance', psym=1, title='Irradiance at '+string(wvl[ni],format='(F5.1)')+' nm'
  oplot, tau_star[*,*,0,ni],irrad[*,*,0,ni], color=30
  oplot, tau_star[*,*,1,ni],irrad[*,*,1,ni], color=80
  oplot, tau_star[*,*,2,ni],irrad[*,*,2,ni], color=130
  oplot, tau_star[*,*,3,ni],irrad[*,*,3,ni], color=180
  oplot, tau_star[*,*,4,ni],irrad[*,*,4,ni], color=250

legend, ['TAU:'+string(tau550,format='(F4.2)')],box=0, textcolors=[30,80,130,180,250],/bottom
; asy, ssa, tau550

  plot, tau_star[*,*,*,ni], rad[*,*,*,ni], xtitle='Tau Star',yrange=[0.,0.3], ytitle='Radiance', psym=1, title='Radiance at '+string(wvl[ni],format='(F5.1)')+' nm'
  oplot, tau_star[*,*,0,ni],rad[*,*,0,ni], color=30
  oplot, tau_star[*,*,1,ni],rad[*,*,1,ni], color=80
  oplot, tau_star[*,*,2,ni],rad[*,*,2,ni], color=130
  oplot, tau_star[*,*,3,ni],rad[*,*,3,ni], color=180
  oplot, tau_star[*,*,4,ni],rad[*,*,4,ni], color=250

legend, ['TAU:'+string(tau550,format='(F4.2)')],box=0, textcolors=[30,80,130,180,250]

device, /close  
spawn, 'convert '+dir+'rtm_rad.ps '+dir+'rtm_tau_star_radwvl'+string(fix(wvl[ni]),format='(I03)')+'.png'
  spawn, 'rm -f '+dir+'rtm_rad.ps'
;endfor

set_plot, 'ps'
  loadct, 39,/silent
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8.
  device, filename=dir+'rtm_rad.ps'
  device, xsize=40, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=1.8 & !x.style=1
  !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !x.margin=[6,6] & !y.margin=[4,2]
  !p.multi=[0,2,1]

  plot, wvl, irrad[0,0,0,*], title='Irradiance per tau star', ytitle='Irradiance', xtitle='Wavelength (nm)'
  oplot, wvl, irrad[0,4,1,*], color=70
  oplot, wvl, irrad[0,3,2,*], color=130
  oplot, wvl, irrad[0,2,3,*], color=180
  oplot, wvl, irrad[0,1,4,*], color=250

  legend, ['TAU:'+string(tau550[0,0,*,0],format='(F4.2)')], /right,box=0, textcolors=[0,70,130,180,250]
  
  plot, wvl, rad[0,0,0,*], title='Radiance per tau star', ytitle='Radiance', xtitle='Wavelength (nm)', yrange=[0,0.2]
  oplot, wvl, rad[0,4,1,*], color=70
  oplot, wvl, rad[0,3,2,*], color=130
  oplot, wvl, rad[0,2,3,*], color=180
  oplot, wvl, rad[0,1,4,*], color=250
  legend, ['TAU:'+string(tau550,format='(F4.2)')], /right,box=0, textcolors=[0,70,130,180,250]

device, /close
spawn, 'convert '+dir+'rtm_rad.ps '+dir+'rtm_rad_tau.png'
  spawn, 'rm -f '+dir+'rtm_rad.ps'

set_plot, 'ps'
  loadct, 39,/silent
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8.
  device, filename=dir+'rtm_rad.ps'
  device, xsize=20, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=1.8 & !x.style=1
  !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !x.margin=[6,6] & !y.margin=[4,2]
  !p.multi=0

; slope from 660 to 680
i_sl=where(wvl ge 660. and wvl le 680.,ct)
tu=fltarr(5,5,5)
slope=fltarr(5,5,5,2)
for i=0, 4 do begin
  for j=0, 4 do begin
    for k=0, 4 do begin
      u=linfit(tau_star[i,j,k,i_sl],rad[i,j,k,i_sl])
      slope[i,j,k,0]=u[0]
      slope[i,j,k,1]=u[1]
      tu[i,j,k]=tau550[i]
    endfor
  endfor
endfor

plot, tu[*,*,*], rad[*,*,*,150],title='Radiance at 500 nm vs. tau ', xtitle='Tau ', ytitle='Radiance ', psym=1;,yrange=[0,-10.0E8],xrange=[0,0.3] 

device, /close
spawn, 'convert '+dir+'rtm_rad.ps '+dir+'rad_tau_500.png'
spawn, 'rm -f '+dir+'rtm_rad.ps'

; plot multiple optical depth at different wavelengths

; load up the various values at various different wvls
wws=['495','496','497','498','499','500','501','502','503','504','505']
wvls=float(wws)
o=0
restore, dir+'tests_rad_cst_tau_star_wvl'+wws[o]+'.out'
; irrad, rad, ssa, tau, asy
rads=rad & irrads=irrad & ssas=ssa & taus=tau & asys=asy
for o=1,  10 do begin
  restore, dir+'tests_rad_cst_tau_star_wvl'+wws[o]+'.out'
  rads=[[[rads]],[[rad]]] & irrads=[[[irrads]],[[irrad]]] & ssas=[[[ssas]],[[ssa]]] & taus=[[[taus]],[[tau]]] & asys=[[[asys]],[[asy]]]
endfor


; get the angstrom exponent for each iterations
; set the reference wavelength to 500 nm
wvls=float(wws)
x=wvls
y=taus*!values.f_NAN
for o=0, 10 do begin
  y[*,*,o]=alog(taus[*,*,o]/taus[*,*,5])
  x[o]=alog(wvls[o]/wvls[5])
endfor
alpha=tau*!values.f_nan
for i=0, 97 do begin
  for j=0, 99 do begin
    k=linfit(x,y[i,j,*])
    alpha[i,j]=-k[1]    
  endfor
endfor


  set_plot, 'ps'
  loadct, 39,/silent
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8.
  device, filename=dir+'rtm_rad.ps'
  device, xsize=40, ysize=40
  !p.font=1 & !p.thick=5 & !p.charsize=1.8 & !x.style=1
  !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !x.margin=[7,2]
  !y.margin=[4,2]
  !p.multi=[0,2,2]
  
  A = FINDGEN(17) * (!PI*2/16.)
USERSYM, COS(A), SIN(A), /FILL

;irradiance
  plot, taus[*,*,0], irrads[*,*,0], title='Irradiance', xtitle='Tau',ytitle='Irradiance (W/m!U2!N)', psym=8, xrange=[0,2],yrange=[1.3,1.75],symsize=0.3
  oplot,taus[*,*,1], irrads[*,*,1], psym=8, color=22,symsize=0.3
  oplot,taus[*,*,2], irrads[*,*,2], psym=8, color=50,symsize=0.3
  oplot,taus[*,*,3], irrads[*,*,3], psym=8, color=70,symsize=0.3
  oplot,taus[*,*,4], irrads[*,*,4], psym=8, color=90,symsize=0.3
  oplot,taus[*,*,5], irrads[*,*,5], psym=8, color=112,symsize=0.3
  oplot,taus[*,*,6], irrads[*,*,6], psym=8, color=135,symsize=0.3
  oplot,taus[*,*,7], irrads[*,*,7], psym=8, color=157,symsize=0.3
  oplot,taus[*,*,8], irrads[*,*,8], psym=8, color=180,symsize=0.3
  oplot,taus[*,*,9], irrads[*,*,9], psym=8, color=200,symsize=0.3
  oplot,taus[*,*,10],irrads[*,*,10],psym=8, color=225,symsize=0.3
  
  legend, wws, textcolors=[0,22,50,70,90,112,135,157,180,200,225],box=0,/right
  
; radiance
  plot, taus[*,*,0], rads[*,*,0], title='Radiance', xtitle='Tau',ytitle='Radiance (W/m!U2!N)', psym=8, xrange=[0,2],yrange=[0.,0.4],symsize=0.3
  oplot,taus[*,*,1], rads[*,*,1], psym=8, color=22,symsize=0.3
  oplot,taus[*,*,2], rads[*,*,2], psym=8, color=50,symsize=0.3
  oplot,taus[*,*,3], rads[*,*,3], psym=8, color=70,symsize=0.3
  oplot,taus[*,*,4], rads[*,*,4], psym=8, color=90,symsize=0.3
  oplot,taus[*,*,5], rads[*,*,5], psym=8, color=112,symsize=0.3
  oplot,taus[*,*,6], rads[*,*,6], psym=8, color=135,symsize=0.3
  oplot,taus[*,*,7], rads[*,*,7], psym=8, color=157,symsize=0.3
  oplot,taus[*,*,8], rads[*,*,8], psym=8, color=180,symsize=0.3
  oplot,taus[*,*,9], rads[*,*,9], psym=8, color=200,symsize=0.3
  oplot,taus[*,*,10],rads[*,*,10],psym=8, color=225,symsize=0.3
  legend, wws, textcolors=[0,22,50,70,90,112,135,157,180,200,225],box=0,/right
  
;irradiance
  plot, alpha, irrads[*,*,0], title='Irradiance vs. angstrom', xtitle='Tau',ytitle='Irradiance (W/m!U2!N)', psym=8,yrange=[1.3,1.75],symsize=0.3
  oplot,alpha, irrads[*,*,1], psym=8, color=22,symsize=0.3
  oplot,alpha, irrads[*,*,2], psym=8, color=50,symsize=0.3
  oplot,alpha, irrads[*,*,3], psym=8, color=70,symsize=0.3
  oplot,alpha, irrads[*,*,4], psym=8, color=90,symsize=0.3
  oplot,alpha, irrads[*,*,5], psym=8, color=112,symsize=0.3
  oplot,alpha, irrads[*,*,6], psym=8, color=135,symsize=0.3
  oplot,alpha, irrads[*,*,7], psym=8, color=157,symsize=0.3
  oplot,alpha, irrads[*,*,8], psym=8, color=180,symsize=0.3
  oplot,alpha, irrads[*,*,9], psym=8, color=200,symsize=0.3
  oplot,alpha,irrads[*,*,10],psym=8, color=225,symsize=0.3
  legend, wws, textcolors=[0,22,50,70,90,112,135,157,180,200,225],box=0,/right
  
; radiance
  plot, alpha, rads[*,*,0], title='Radiance vs. angstrom', xtitle='Tau',ytitle='Radiance (W/m!U2!N)', psym=8,yrange=[0.,0.4],symsize=0.3
  oplot,alpha, rads[*,*,1], psym=8, color=22,symsize=0.3
  oplot,alpha, rads[*,*,2], psym=8, color=50,symsize=0.3
  oplot,alpha, rads[*,*,3], psym=8, color=70,symsize=0.3
  oplot,alpha, rads[*,*,4], psym=8, color=90,symsize=0.3
  oplot,alpha, rads[*,*,5], psym=8, color=112,symsize=0.3
  oplot,alpha, rads[*,*,6], psym=8, color=135,symsize=0.3
  oplot,alpha, rads[*,*,7], psym=8, color=157,symsize=0.3
  oplot,alpha, rads[*,*,8], psym=8, color=180,symsize=0.3
  oplot,alpha, rads[*,*,9], psym=8, color=200,symsize=0.3
  oplot,alpha,rads[*,*,10],psym=8, color=225,symsize=0.3
  legend, wws, textcolors=[0,22,50,70,90,112,135,157,180,200,225],box=0,/right
  
  
  
         device, /close
  spawn, 'convert '+dir+'rtm_rad.ps '+dir+'tau_rad_irrad.png'
  spawn, 'rm -f '+dir+'rtm_rad.ps'
  
stop
 
   ; plot single wavelength data.
  ; asy, ssa for rad, irrad, and tau
  ww='505'
  restore, dir+'tests_rad_cst_tau_star_wvl'+ww+'.out'
    set_plot, 'ps'
  loadct, 39,/silent
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8.
  device, filename=dir+'rtm_rad.ps'
  device, xsize=60, ysize=20
  !p.font=1 & !p.thick=5
  !p.charsize=1.8 & !x.style=1
  !y.style=1 & !z.style=1
  !y.thick=1.8 & !x.thick=1.8
  !x.margin=[6,3]
  !y.margin=[4,2]
  !p.multi=[0,3,1]
  
  plot, rad, irrad, title='Constant tau star at 0.1 for '+ww+' nm',xtitle='Radiance', ytitle='Irradiance', psym=3, yrange=[1.4,1.75],xrange=[0.,0.4]
  oplot, rad[20,*],irrad[20,*], color=50
  oplot, rad[40,*],irrad[40,*], color=100
  oplot, rad[60,*],irrad[60,*], color=150
  oplot, rad[80,*],irrad[80,*], color=200
  oplot, rad[95,*],irrad[95,*], color=250

  legend,['ASY:'+string(asy[[20,40,60,80,95]],format='(F4.2)')],textcolors=[50,100,150,200,250], box=0,/bottom, /right

plot, rad, irrad, title='Constant tau star at 0.1 for '+ww+' nm',xtitle='Radiance', ytitle='Irradiance', psym=3, yrange=[1.4,1.75],xrange=[0.,0.4]
  oplot, rad[1:97,20],irrad[1:97,20], color=50
  oplot, rad[1:97,40],irrad[1:97,40], color=100
  oplot, rad[1:97,60],irrad[1:97,60], color=150
  oplot, rad[1:97,80],irrad[1:97,80], color=200
  oplot, rad[1:97,99],irrad[1:97,99], color=250
       
  legend,['SSA:'+string(ssa[[20,40,60,80,99]],format='(F4.2)')],textcolors=[50,100,150,200,250], box=0,/bottom, /right 

plot, rad, irrad, xmargin=[6,15], title='Constant tau star at 0.1 for '+ww+' nm',xtitle='Radiance', ytitle='Irradiance', psym=3, yrange=[1.4,1.75],xrange=[0.,0.4]

;stop
for i=0, 97 do begin
  for j=0, 99 do begin
    nc=fix(tau[i,j]*255./(0.8))
;    print, i,j,nc
    oplot, [rad[i,j],rad[i,j]],[irrad[i,j],irrad[i,j]], psym=3, color=nc
  endfor
endfor

colorbar, position=[0.06,0.96,0.97,0.97] ,/right, /vertical, range=[0,0.8], format='(F6.4)'
       device, /close
  spawn, 'convert '+dir+'rtm_rad.ps '+dir+'rtm_tau_star_cst_'+ww+'.png'
  spawn, 'rm -f '+dir+'rtm_rad.ps'
 
stop

  ; plot single wavelength data.
  ; asy, ssa for rad, irrad, and tau
  restore, dir+'tests_rad_cst_tau_star.out'
    set_plot, 'ps'
  loadct, 39,/silent
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8.
  device, filename=dir+'rtm_rad.ps'
  device, xsize=60, ysize=20
  !p.font=1 & !p.thick=5
  !p.charsize=1.8 & !x.style=1
  !y.style=1 & !z.style=1
  !y.thick=1.8 & !x.thick=1.8
  !x.margin=[6,3]
  !y.margin=[4,2]
  !p.multi=[0,3,1]
  
  plot, rad, irrad, title='Constant tau star at 0.1 for 500 nm',xtitle='Radiance', ytitle='Irradiance', psym=3, yrange=[1.4,1.6],xrange=[0.,0.4]
  oplot, rad[100,*],irrad[100,*], color=50
  oplot, rad[200,*],irrad[200,*], color=100
  oplot, rad[300,*],irrad[300,*], color=150
  oplot, rad[400,*],irrad[400,*], color=200
  oplot, rad[470,*],irrad[470,*], color=250

  legend,['ASY:'+string(asy[[100,200,300,400,470]],format='(F4.2)')],textcolors=[50,100,150,200,250], box=0,/bottom, /right

plot, rad, irrad, title='Constant tau star at 0.1 for 500 nm',xtitle='Radiance', ytitle='Irradiance', psym=3, yrange=[1.4,1.6],xrange=[0.,0.4]
  oplot, rad[1:489,100],irrad[1:489,100], color=50
  oplot, rad[1:489,200],irrad[1:489,200], color=100
  oplot, rad[1:489,300],irrad[1:489,300], color=150
  oplot, rad[1:489,400],irrad[1:489,400], color=200
  oplot, rad[1:489,499],irrad[1:489,499], color=250
       
  legend,['SSA:'+string(ssa[[100,200,300,400,499]],format='(F4.2)')],textcolors=[50,100,150,200,250], box=0,/bottom, /right 

plot, rad, irrad, xmargin=[6,15], title='Constant tau star at 0.1 for 500 nm',xtitle='Radiance', ytitle='Irradiance', psym=3, yrange=[1.4,1.6],xrange=[0.,0.4]

;stop
for i=0, 489 do begin
  for j=0, 499 do begin
    nc=fix(tau[i,j]*255./(0.8))
;    print, i,j,nc
    oplot, [rad[i,j],rad[i,j]],[irrad[i,j],irrad[i,j]], psym=3, color=nc
  endfor
endfor

colorbar, position=[0.06,0.96,0.97,0.97] ,/right, /vertical, range=[0,0.8], format='(F6.4)'
       device, /close
  spawn, 'convert '+dir+'rtm_rad.ps '+dir+'rtm_tau_star_cst.png'
  spawn, 'rm -f '+dir+'rtm_rad.ps'
  
end
