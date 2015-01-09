; program to plot sample SSFR3 spectra of radiance
; takes in the calibspcs.out file and outputs plots for the select day.


@legend.pro
pro ssfr3_plot_sp

date='20120525'

dir='/home/leblanc/SSFR3/data/'+date+'/'
print, 'restoring ...'
restore, '/argus/roof/SSFR3/data/'+date+'/out/'+date+'_calibspcs.out'

utc_lim=[15.0,16.0]

fl=where(tmhrs gt utc_lim[0] and tmhrs lt utc_lim[1],ns)
if ns lt 1 then message, 'no spectra found within the time limits'


times=[15.0,15.25,15.5,15.75,16.0]
cl=times*0.

fp=dir+date+'_sample'
print, 'making plot :'+fp
set_plot, 'ps'
loadct, 39, /silent
device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
device, xsize=20, ysize=20
!p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
!p.multi=0 & !x.margin=[7,4]

plot, zenlambda, zspectra[*,fl[0]],title='SSFR zenith radiance - '+date,xtitle='Wavelength (nm)',ytitle='Radiance (W/m!U2!N nm sr)',yr=[0,0.2],xr=[350,1700],/nodata
for i=0, n_elements(times)-1 do begin
  nul=min(abs(times[i]-tmhrs),nz)
  cl[i]=i*254./(n_elements(times)-1)
  oplot, zenlambda,zspectra[*,nz],color=cl[i]
endfor

legend,[string(times,format='(F5.2)')+' UTC'],textcolors=cl,color=cl,linestyle=[replicate(0,n_elements(times))],/right


device, /close
spawn, 'convert '+fp+'.ps '+fp+'.png'



stop
end
