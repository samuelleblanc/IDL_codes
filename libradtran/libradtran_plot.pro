; plotting of preliminary model output from varrying aerosol content for zenith radiance and irradiance
; find dependance on the optical depth. to help for aerosol measurement on the ship/plane
; future application to roof

restore, '\\lasp-smb\leblanc\libradtran\output\aerosol_1.sav'
tau=[0.,1.,2.,4.,10.,20.,50.,100.]


!p.multi=[0,2,1]

t=n_elements(tau)


set_plot, 'ps'
;device, decomposed=0
  device, /encapsulated
  device, /tt_font, set_font='Helvetica Bold'
  device, filename='\\lasp-smb\leblanc\libradtran\rad-irrad\aerosol_1.ps'
  device,/color,bits_per_pixel=8
  device, xsize=60, ysize=30

loadct, 39
;window, 0, xsize=1600, ysize=800
!p.color=0
!p.background=255
!p.font=1
!p.thick=7
!p.charsize=2.2

plot, wvl, rad[*,0], yrange=[0,max(rad)*1.05], title='Modeled Zenith Radiance Spectra with varrying aerosol optical depth', xtitle='Wavelength', ytitle='Radiance' 
dcol=fix(250./float(t-1))
text_l='No Aerosol'
col_l=0
for i=1, t-1 do begin
  oplot, wvl, rad[*,i], color=dcol*i
  text_l=[text_l,strtrim(string(tau[i]),2)]
  col_l=[col_l,dcol*i]
endfor

legend, ['Multiplication factor of tau',text_l],textcolor=[0,col_l], box=0

plot, wvl, dif_dn[*,0]+dir_dn[*,0], yrange=[0,max(dif_dn+dir_dn)*1.05], title='Modeled Irradiance Spectra with varrying aerosol optical depth', xtitle='Wavelength', ytitle='Irradiance' 
for i=1, t-1 do begin
  oplot, wvl,  dif_dn[*,i]+dir_dn[*,i], color=dcol*i
endfor
device, /close
spawn, 'convert \\lasp-smb\leblanc\libradtran\rad-irrad\aerosol_1.ps \\lasp-smb\leblanc\libradtran\rad-irrad\aerosol_1.png'
stop
end