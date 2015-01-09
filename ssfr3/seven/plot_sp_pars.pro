; program to plot a sample spectra with all the regions of each parameter highlighted
; uses measured spectra as based

pro plot_sp_pars

dir='/home/leblanc/SSFR3/data/'

restore,dir+'20120523_sp_ex.out'

fn=dir+'sp_pars'
set_plot,'ps'
print, 'making plot :'+fn
loadct, 39, /silent
device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fn+'.ps'
device, xsize=20, ysize=20
 !p.font=1 & !p.thick=5 & !p.charsize=2.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
 !p.multi=0 & !x.margin=[3,3] &!x.omargin=[0,0]

 plot, wvl,sp, xtitle='Wavelength (nm)',ytitle='Normalized radiance'
 

device, /close
spawn, 'convert "'+dir+'.ps" "'+dir+'.png"'
stop
end
