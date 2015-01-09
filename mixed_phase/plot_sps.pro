;program to interactively plot all the data from the sp_mix3 files

pro plot_sps,lbl

if n_elements(lbl) lt 1 then label='lvls_20120524' else label=lbl

dir='/argus/roof/SSFR3/model/'
restore, dir+'sp_mix3_'+label+'.out'
sps=sp
taus=tau[0:17]

set_plot, 'x'
device, decomposed=0
loadct, 39
window, 0, retain=2
!p.multi=[0,4,2]
for t=0, n_elements(taus)-1 do begin
  for rl=0,n_elements(refl)-1 do begin
    for ri=0, n_elements(refi)-1 do begin

      if t lt 6 then yr=[0,2.2] else yr=[0,0.5]
      tit='tau:'+string(taus[t],format='(I3)')+' refl:'+string(refl[rl],format='(F4.1)')+' refi:'+string(refi[ri],format='(F4.1)')
      plot, zenlambda, sp[t,rl,ri,*,0],title=tit,xtitle='Wavelength',xr=[400,1700],yr=yr,/ysty,/xsty
      for w=1,10 do oplot, zenlambda,sp[t,rl,ri,*,w],color=25*w

    endfor
  endfor
endfor









stop
end
