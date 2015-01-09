; IDL script for Radiative Processes  Mie scattering lab


psfile='\\lasp-smb\leblanc\Colorado\Spring 2010\ATOC 5560\Lab\Mie\phasefunc1.ps'


; This script supports 2 output formats: 
;   X windows (out='x') and Postscript ('ps')
out='ps'

if (out eq 'ps') then begin
  ;  For Postscript use device fonts (pick Helvetica)
  set_plot, 'ps'
  device, filename=psfile, /color, /portrait, /helvetica, $
   	 xsize=18, ysize=24, xoffset=1.5, yoffset=2.0
  !p.font=0
endif
if (out eq 'x') then begin
  ;  For X windows use the Hershey stroked fonts
  set_plot, 'x'
  window, xsize=525, ysize=700, title='ATOC/ASTR 5560 Plots'
  !p.font=-1
endif

; Set other global plotting variables
!p.thick=1.0  &  !x.thick=1.0  &  !y.thick=1.0
!x.ticklen = 0.013  &  !y.ticklen = 0.010
!p.charsize=1.2



;  Plot three phase functions
  phasemax=300.             ; maximum phase function on plot

  readphase, '\\lasp-smb\leblanc\Colorado\Spring 2010\ATOC 5560\Lab\Mie\pha_087.dat', Angle1, Phase1, p12, p33, p44
  readphase, '\\lasp-smb\leblanc\Colorado\Spring 2010\ATOC 5560\Lab\Mie\pha_164.dat', Angle2, Phase2, p12, p33, p44
  readphase, '\\lasp-smb\leblanc\Colorado\Spring 2010\ATOC 5560\Lab\Mie\pha_213.dat', Angle3, Phase3, p12, p33, p44
  Legend1='Cloud r!De!N=10 um  0.87 um'
  Legend2='Cloud r!De!N=10 um  1.64 um'
  Legend3='Cloud r!De!N=10 um  2.13 um'


  plot, Angle1, Phase1, linestyle=0, /ylog, $
	/xstyle, xrange=[0,180], /ystyle, yrange=[1.0E-2,phasemax], $
	xtitle='Scattering Angle (degrees)', $
	ytitle='Phase Function'
  oplot, Angle2, Phase2, linestyle=1
  oplot, Angle3, Phase3, linestyle=2

  x0=60.   &  dx=180
  x1=x0+0.02*dx  &  x2=x0+0.08*dx      &  x3=x0+0.10*dx
  y0=0.01  &  y1=phasemax
  f=0.95 &  y=exp((1-f)*alog(y0)+f*alog(y1))
    oplot, [x1,x2], [y,y], linestyle=0
    xyouts, x3, y, charsize=1.0*!p.charsize, Legend1
	
  f=0.90 &  y=exp((1-f)*alog(y0)+f*alog(y1))
    oplot, [x1,x2], [y,y], linestyle=1
    xyouts, x3, y, charsize=1.0*!p.charsize, Legend2
	
  f=0.85 &  y=exp((1-f)*alog(y0)+f*alog(y1))
    oplot, [x1,x2], [y,y], linestyle=2
    xyouts, x3, y, charsize=1.0*!p.charsize, Legend3
	



if (out ne 'x') then device, /close

end
