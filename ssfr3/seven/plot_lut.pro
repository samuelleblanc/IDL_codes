; program to go through and plot each parameter vs the other one
; for the entire lut 

pro plot_lut
restore, '/home/leblanc/SSFR3/data/Pars_LUT_hires.out'
tau=taus
if 0 then begin
  set_plot, 'x'
  window, 0, retain=2, xsize=1000, ysize=500
  !p.multi=[0,2,1]
  device, decomposed=0
  loadct, 39
endif
ntau=n_elements(tau)
nref=n_elements(ref)

dir='/home/leblanc/SSFR3/plots/'

for n1=0, 12 do begin
  for n2=n1+1,12 do begin
    if 1 then begin
      fp=dir+'params_lut_'+string(n1,format='(I02)')+'_'+string(n2,format='(I02)')
      print, 'making plot :'+fp
      set_plot, 'ps'
      loadct, 39, /silent
      device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
      device, xsize=40, ysize=20
      !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=0 & !y.style=0 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
      !p.multi=[0,2,1] & !x.margin=[8,4]
    endif

    plot, pars[*,*,0,n1],pars[*,*,0,n2], psym=2, title='Liquid',xtitle='Parameter '+strtrim(n1,2), ytitle='Parameter '+strtrim(n2,2)
    for t=0,ntau-1 do oplot, pars[t,*,0,n1],pars[t,*,0,n2], psym=2, color=(t+1)*254./ntau
    for r=0,nref-1 do oplot, pars[*,r,0,n1],pars[*,r,0,n2], linestyle=2, color=(r+1)*254./nref
    legend,['r!Deff!N='+strtrim(ref[0],2)+'um','r!Deff!N='+strtrim(ref[nref-1],2)+'um'],linestyle=[2,2],pspacing=1.2,textcolors=[ntau,254.],color=[49.,245.],box=0
    legend,['tau='+strtrim(tau[0],2),'tau='+strtrim(tau[(ntau-1)/2],2),'tau='+strtrim(tau[ntau-1],2)],psym=[2,2,2],pspacing=1.2,textcolors=[ntau,254./2.,254],colors=[ntau,254./2., 254],box=0,/right

    plot, pars[*,*,1,n1],pars[*,*,1,n2], psym=2, title='Ice',xtitle='Parameter '+strtrim(n1,2), ytitle='Parameter '+strtrim(n2,2)
    for t=0,ntau-1 do oplot, pars[t,*,1,n1],pars[t,*,1,n2], psym=2, color=(t+1)*254./ntau
    for r=0,nref-1 do oplot, pars[*,r,1,n1],pars[*,r,1,n2], linestyle=2, color=(r+1)*254./nref
    legend,['r!Deff!N='+strtrim(ref[0],2)+'um','r!Deff!N='+strtrim(ref[nref-1],2)+'um'],linestyle=[2,2],pspacing=1.2,textcolors=[ntau,254.],color=[49.,245.],box=0
    legend,['tau='+strtrim(tau[0],2),'tau='+strtrim(tau[(ntau-1)/2],2),'tau='+strtrim(tau[ntau-1],2)],psym=[2,2,2],pspacing=1.2,textcolors=[ntau,254./2.,254],colors=[ntau,254./2.,254.],box=0,/right 

;    stop
    if 1 then begin
      device, /close
      spawn, 'convert '+fp+'.ps '+fp+'.png'
    endif
  endfor
endfor



stop
end
