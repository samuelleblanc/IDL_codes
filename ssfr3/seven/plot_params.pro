; program to run and plot the various parameters calculated for a given spectra
; the parameters are used to determine cloud phase, ref, tau, lwp, iwp

@legend.pro
@get_params.pro
pro plot_params

; restore the model data
;restore, '/argus/SSFR3/model/sp_liq_ice_snow.out'
restore, '/argus/roof/SSFR3/model/sp_liq_ice_new3.out'
dir='/home/leblanc/SSFR3/data/'

; run the parameters 
lwp=fltarr(n_elements(tau),n_elements(ref))
iwp=fltarr(n_elements(tau),n_elements(ref))
get_params,zenlambda,sp[0,0,*,0],pars
par=fltarr(n_elements(tau),n_elements(ref),n_elements(pars),2)
for t=0, n_elements(tau)-1 do begin
  for r=0, n_elements(ref)-1 do begin
    lwp[t,r]=2./3.*tau[t]*ref[r]/10.
    iwp[t,r]=2./3.*tau[t]*ref[r]/10.
    get_params,zenlambda,sp[t,r,*,0],partm
    par[t,r,*,0]=partm
    get_params,zenlambda,sp[t,r,*,1],partm
    par[t,r,*,1]=partm
  endfor
endfor

; now run through the plots of each parameter
for i=0, n_elements(partm)-1 do begin
parstr=string(i+1,format='(I02)')
;fp=dir+'mod_snow_params_'+parstr
fp=dir+'mod_params_'+parstr
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=60, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=3.2 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,3,1] & !x.margin=[8,4]

  yr=[min(par[*,*,i,*])*0.95,max(par[*,*,i,*])*1.05d]
  if max(par[*,*,i,*]) lt 0. then yr=[min(par[*,*,i,*])*1.05,max(par[*,*,i,*])*0.95]  

  xr=[min(tau)*0.95,max(tau)*1.05]
  plot, tau, par[*,0,i,0],title='Optical depth per parameter:'+parstr, xtitle='Optical depth', $
   ytitle='parameter '+parstr,psym=-5,yrange=yr,xr=xr,symsize=2
  for j=0, n_elements(ref)-1 do oplot, tau, par[*,j,i,0],psym=-5, symsize=2,color=j*254/(n_elements(ref)-1)
  
  ;oplot,tau, par[*,0,i,1],psym=-6
  for j=0, n_elements(ref)-1 do begin
    oplot, tau, par[*,j,i,1],linestyle=2,color=j*254/(n_elements(ref)-1)
    oplot, tau, par[*,j,i,1],psym=6, symsize=2,color=j*254/(n_elements(ref)-1)
  endfor
  mu="155B ;"
  sm='!9'+string(mu)+'!X'
  
  legend,['r!Deff!N: '+string(ref,format='(F6.2)')+' '+sm+'m'],textcolors=findgen(n_elements(ref))*254/(n_elements(ref)-1),box=0,charsize=2;,/bottom
  legend,['Liquid','Ice'],psym=[5,6],box=0,charsize=2,symsize=[2,2],/bottom,/right

  xr=[min(ref)*0.95,max(ref)*1.05]
  plot, ref, par[0,*,i,0],title='Effective radius per parameter:'+parstr, xtitle='Effective radius (um)',$
   ytitle='parameter '+parstr,psym=-5,yrange=yr,xr=xr,symsize=2
  for j=0, n_elements(tau)-1 do oplot, ref, par[j,*,i,0],psym=-5,symsize=2, color=j*254/(n_elements(tau)-1)

  oplot,ref, par[0,*,i,1],psym=-6,symsize=2

  for j=0, n_elements(tau)-1 do oplot, ref, par[j,*,i,1],psym=-6, symsize=2,color=j*254/(n_elements(tau)-1)
  the="164B ;"
  st='!9'+string(the)+'!X'
  legend,[st+': '+string(tau,format='(F6.2)')],textcolors=findgen(n_elements(tau))*254/(n_elements(tau)-1),box=0,charsize=2,/bottom
  legend,['Liquid','Ice'],psym=[5,6],/right,/bottom,box=0,charsize=2

  xr=[min(lwp)*0.95,max(lwp)*1.05]
  plot, reform(lwp), reform(par[*,*,i,0]),title='Cloud water content per parameter:'+parstr, $
   xtitle='Liquid/Ice Water path (g/m!U2!N)', ytitle='parameter '+parstr,psym=5,yrange=yr,xr=xr,symsize=2
  oplot,reform(iwp), reform(par[*,*,i,1]),psym=6,symsize=2
  legend,['Liquid','Ice'],psym=[5,6],charsize=2,box=0,symsize=[2,2]
device,/close
spawn, 'convert '+fp+'.ps '+fp+'.png'
endfor


;;;;; now calculate these parameters for the measurements
restore, '/home/leblanc/SSFR3/data/phase_example.out'

parx=fltarr(n_elements(lbl),n_elements(pars))
for i=0, n_elements(lbl)-1 do begin
  get_params, wl, sps[*,i],partm
  parx[i,*]=partm
  nul=where(i eq liq,liqi)
  nul=where(i eq mix,mixi)
  nul=where(i eq ice,icei)
  print, lbl[i],partm,tau[i],ref[i],liqi,mixi,icei,format='(A,",",13(F10.5,","))'
endfor



stop
end
