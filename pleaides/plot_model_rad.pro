; program to plot the modeled radiance spectra, one by one to make sure that all spectra were calculated correctly.
; outputs interactive plots

pro plot_model_rad

ssza='85'


lbl='20130911_ice_sza'+ssza+'_raw'
lbl='20130911_ice_sza'+ssza+'_4STAR'

dir='/home5/sleblan2/4STAR/model/'
fi =dir+'sp_v1_'+lbl+'.out'

print, 'restoring file: '+fi
restore, fi
if n_elements(wvl) lt 1 then wvl=zenlambda

; set up plotting
set_plot, 'x'
device, decomposed=0
!p.multi=[0,2,2] & !y.style=1 & !x.style=1
loadct, 39
ww=['liq','ice']
cl=findgen(n_elements(ref))*250/35
tis=string(ref,format='(F4.1)')+' um'

for t=0, n_elements(tau)-1 do begin
  for z=0, 1 do begin
    plot, wvl, sp[t,0,0,*,z], title=lbl+' '+ww[z]+' mod rad tau:'+strtrim(tau[t]),xtit='Wavelength [nm]',ytit='Radiance [W/m^2 sr nm]', yrange=[0,0.4],xrange=[350,1700]
    for r=0, n_elements(ref)-1 do begin
      oplot, wvl, sp[t,r,0,*,z],color=cl[r]
    endfor
  endfor
  legend,tis,textcolors=cl,box=0,/right
    for z=0, 1 do begin
    plot, wvl, sp[t,0,0,*,z]/max(sp[t,0,0,*,z],/nan), title=lbl+' '+ww[z]+' mod rad tau:'+strtrim(tau[t]),xtit='Wavelength [nm]',$
     ytit='Normalized Radiance', yrange=[0,1.0],xrange=[350,1700]
    for r=0, n_elements(ref)-1 do begin
      oplot, wvl, sp[t,r,0,*,z]/max(sp[t,r,0,*,z],/nan),color=cl[r]
    endfor
  endfor
  write_png,dir+lbl+'_tau'+string(t,format='(I02)')+'.png',tvrd(/true)
endfor
stop

end
