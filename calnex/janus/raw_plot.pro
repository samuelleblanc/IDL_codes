pro raw_plot

dir='/home/leblanc/CALNEX/p3/20100519/
restore, dir+'20100519_raw.out'

set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated
 device, /tt_font, set_font='Helvetica Bold'
 device, filename=dir+'raw.ps'
 device,/color,bits_per_pixel=8.
 device, xsize=30, ysize=30
      !p.font=1
      !p.thick=8
      !p.charsize=3.5
      !x.style=1
      !y.style=1 
      !z.style=1
      !y.thick=1.8
      !x.thick=1.8
      !p.multi=0

mm=min(abs(940.-wlvisz),wvz)
mm=min(abs(940.-wlvisn),wvn)
mm=min(abs(940.-wlnirz),wiz)
mm=min(abs(940.-wlnirn),win)

plot, wlvisz, zsispectrum, xrange=[300,2200], yrange=[0,2.2],title='Sample SSFR Calibrated Spectrum',ytitle='Irradiance (W/m!E2!N/nm)', xtitle='Wavelength (nm)', /nodata
oplot, wlvisz[0:wvz], zsispectrum[0:wvz], color=70
oplot, wlvisn[0:wvn], nsispectrum[0:wvn], color=70, linestyle=2
oplot, wlnirz[wiz:*], zirspectrum[wiz:*], color=250
oplot, wlnirn[win:*], nirspectrum[win:*], color=250, linestyle=2

legend, ['Si array','InGaAs array', 'Nadir', 'Zenith'], /right, textcolors=[70,250,0,0], color=[70,250,0,0], linestyle=[0,0,2,0]

device, /close
spawn, 'convert '+dir+'raw.ps '+dir+'raw.png'
spawn, 'rm -f '+dir+'raw.ps'
end
