; program to plot the covariance matrix of ssa
; using data interpolated from AERONET around the world and various different types of aerosols

@/home/leblanc/IDL/bin/colorbar.pro
pro plot_covariance

;dir='/home/leblanc/DC3_SEAC4RS/library/'
dir='\\lasp-smb\leblanc\DC3_SEAC4RS\library\'
restore, dir+'SSA_covariance.out'

;ssa fltarr(4,12)
;wvl fltarr(4)

;make new wvl array
lambda=findgen(203)*3.2+350. ;build a random wavelength array

;now interpolate or spline
ssa_hi=fltarr(203,12)
for i=0, 11 do begin ;loop over all ssa values
  ssa_hi[*,i]=spline(wvl,ssa[*,i],lambda)
endfor

; now get the covariance matrix
m=correlate(ssa_hi,/covariance)

;save to text
fo=dir+'SSA_cov.txt'
openw, lun, fo, /get_lun
for i=0,202 do begin
printf, lun, m[*,i],format='(203(G14.8,","))'
endfor
close,lun

; now figure out if the covariance matrice can be diagonalised
stop


if 0 then begin
;now plot
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold'
 device, filename=dir+'ssa_covariance.ps'
 device,/color,bits_per_pixel=8., xsize=20, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=0 & !x.margin=[6,10]

print, 'plotting'

contour, m, lambda, lambda, /fill, nlevels=20, xtitle='Wavelength-i (nm)', ytitle='Wavelength-j (nm)',title='Spectral SSA covariance'
colorbar, minrange=min(m),maxrange=max(m),title='Covariance values',/vertical,/right,position=[0.79,0.13,0.82,0.93],format='(F7.4)'

;contour, rad[nl,t,*,*],ssa,asy,/fill,nlevels=15, xtitle='SSA',ytitle='ASY', title='Radiance and irradiance at 500 nm and tau='+string(aod[t],format='(F4.2)')
;contour, irr[nl,t,*,*],ssa,asy,levels=lvl,/overplot,c_annotation=lbl,c_charsize=1.5
;contour, rad_sun[nl,t,*,*],ssa,asy,levels=lvls, /overplot, c_annotation=lbls,c_charsize=1.5,color=255
;colorbar, minrange=0,maxrange=max(rad[nl,t,*,*]), title='Radiance (W/m!U2!Nnm sr)',/vertical,/right,format='(F4.2)', position=[0.82,0.13,0.85,0.93]

device, /close
spawn, 'convert '+dir+'ssa_covariance.ps '+dir+'ssa_covariance.png'
endif


stop
end 
