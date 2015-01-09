@/home/leblanc/libradtran/pro/libradtran_reader.pro
pro cloud_rtm_compare

print, 'restoring'
;restore, '/home/leblanc/libradtran/output/cloud/cloud.sav'
data=libradtran_reader(file='/home/leblanc/libradtran/output/cloud/cloud.out',zout=[0,0.133,0.96],/radiance,/quiet)

restore, '/home/leblanc/CALNEX/p3/20100516/20100516_calibspcs_attcorr.out'
p3_utc=tmhrs & p3_zen=zspectra & p3_nad=nspectra  & p3_zl=zenlambda & p3_nl=nadlambda
restore, '/data/seven/schmidt/calnex/ship/20100516/20100516_calibspcs.out'
dir='/home/leblanc/CALNEX/clouds/'

m=min(abs(19.498889-p3_utc),p3)
;m=min(abs(18.96-p3_utc),p3b)
p3b=3908
m=min(abs(19.45-tmhrs),ship)

c=1.0;0.5E-3;c=7.60172E-4 ;1.875595E-3

print, 'plotting'
set_plot, 'ps'
loadct, 39,/silent
!p.font=1 & !p.thick=5 & !p.charsize=3.5 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
!p.multi=[0,2,2]
device, /encapsulated
device, /tt_font, set_font='Helvetica Bold'
device, filename=dir+'20100516_irr_mod.ps'
device,/color,bits_per_pixel=8.
device, xsize=50, ysize=30

!y.margin=[3,2]
!x.margin=[2,2]
!x.omargin=[4,0]
; the above cloud spectra, both upwelling and downwelling
plot, p3_zl, p3_zen[*,p3], title='Irradiance spectra above cloud', xtitle='wavelength (nm)', ytitle='Irradiance (W/m!E2!N/nm)'
oplot, data.wvl[2,*],data.dif_dn[2,*]+data.dir_dn[2,*], color=150

oplot, p3_nl, p3_nad[*,p3],linestyle=2
oplot, data.wvl[2,*],data.dif_up[2,*], color=150, linestyle=2
legend, ['P-3 Measured','Modeled'], color=[0,150],textcolors=[0,150],linestyle=[0,0],pspacing=1.5,corners=c1,/right,position=[0.35,0.89],box=0,/normal,charsize=1.8 ;0.7
legend,['|','|'], color=[0,150],box=0,/right,pspacing=1.5,linestyle=[2,2],/normal,position=[c1[1]-0.444,c1[3]],charsize=1.8 ;-0.1
xyouts,0.35,0.89,/normal,'Downwelling | Upwelling',alignment=0.585,charsize=1.8 ;0.7,a=0.55

;diff
d_zen=interpol(data.dif_dn[2,*]+data.dir_dn[2,*],data.wvl[2,*],p3_zl)
d_nad=interpol(data.dif_up[2,*],data.wvl[2,*],p3_nl)

plot, p3_zl, p3_zen[*,p3]/d_zen, yrange=[0.5,2],title='Above cloud ratio', xtitle='wavelength (nm)';ytitle='Irradiance (W/m!E2!N)',yticks=6
oplot, p3_nl, p3_nad[*,p3]/d_nad, color=250
oplot, [min(p3_nl),max(p3_nl)],[1.0,1.0], linestyle=2, thick=3.5
legend, ['Downwelling','Upwelling'],textcolors=[0,250],box=0,charsize=1.8

; below cloud
if (0) then begin
plot, p3_zl, p3_zen[*,p3b], title='Irradiance Spectra for Below Cloud', xtitle='wavelength (nm)', ytitle='Irradiance (W/m!E2!N)',yrange=[0,2]
oplot, data.wvl[1,*],data.dif_dn[1,*]+data.dir_dn[1,*], color=150

oplot, p3_nl, p3_nad[*,p3b],linestyle=2
oplot, data.wvl[1,*],data.dif_up[1,*], color=150, linestyle=2
legend, ['P3 Measured Spectra','Model Spectra'], color=[0,150],pspacing=1.5,linestyle=[0,0],corners=c1,/right,position=[0.35,0.6],box=0,/normal,charsize=1.8 ; 0.7
legend,['|','|'], color=[0,150],box=0,/right,pspacing=1.5,linestyle=[2,2],/normal,position=[c1[1]-0.175,c1[3]],charsize=1.8 ;+0.22
xyouts,0.35,0.6,/normal,'Downwelling | Upwelling',alignment=0.59,charsize=1.8 ;0.7,a=0.55

;diff
d_zen=interpol(data.dif_dn[1,*]+data.dir_dn[1,*],data.wvl[1,*],p3_zl)
d_nad=interpol(data.dif_up[1,*],data.wvl[1,*],p3_nl)

plot, p3_zl, abs(p3_zen[*,p3b]-d_zen), title='Below cloud differences', xtitle='wavelength (nm)', ytitle='Irradiance (W/m!E2!N)'
oplot, p3_nl, abs(p3_nad[*,p3b]-d_nad), color=250
legend, ['Downwelling','Upwelling'],textcolors=[0,250],/right,box=0,charsize=1.8
endif

; at ship
plot, zenlambda, zspectra[*,ship], title='Irradiance spectra at ship level', xtitle='wavelength (nm)', yrange=[0,1.5], ytitle='Irradiance (W/m!E2!N/nm)',yticks=5
oplot, data.wvl[0,*],data.dif_dn[0,*]+data.dir_dn[0,*], color=150

;oplot, nadlambda, nspectra[*,ship],linestyle=2
;oplot, data.wvl[0,*],data.rad[0,*]*1.0E-3, color=150, linestyle=2
;legend, ['Ship Measured Spectra','Model Spectra'], color=[0,150],pspacing=1.5,corners=c1,linestyle=[0,0],/right,position=[0.35,0.42],box=0,/normal,charsize=1.8 ;0.7
;legend,['|','|'], color=[0,150],box=0,/right,pspacing=1.5,linestyle=[2,2],/normal,position=[c1[1]+0.02,c1[3]],charsize=1.8 ;+0.55
;xyouts,0.35,0.42,/normal,'Irradiance(W/m!E2!N) | Radiance(W/m!E2!Nstr)',alignment=0.524,charsize=1.8 ;0.7, a=0.47
legend, ['Ship Measured','Modeled'], color=[0,150],textcolors=[0,150], pspacing=1.5,corners=c1,linestyle=[0,0],/right,position=[0.35,0.42],box=0,/normal,charsize=1.8


;diff
d_zen=interpol(data.dif_dn[0,*]+data.dir_dn[0,*],data.wvl[0,*],zenlambda)
d_nad=interpol(data.rad[0,*]*1.0E-3,data.wvl[0,*],nadlambda)

plot, zenlambda, zspectra[*,ship]/d_zen, title='Ship level ratio', xtitle='wavelength (nm)', yrange=[0.5,2];, ytitle='Irradiance (W/m!E2!N)',yticks=6
;oplot, nadlambda, nspectra[*,ship]/d_nad, color=250
oplot, [min(nadlambda),max(nadlambda)],[1.0,1.0], linestyle=2, thick=3.5
;legend, ['Irradiance','Radiance'],textcolors=[0,250],box=0,charsize=1.8
legend, ['Irradiance'],textcolors=[0],box=0,charsize=1.8

device,/close
spawn, 'convert '+dir+'20100516_irr_mod.ps '+dir+'20100516_irr_cloud_norad.png '
spawn, 'rm -f '+dir+'20100516_irr_mod.ps '

end
