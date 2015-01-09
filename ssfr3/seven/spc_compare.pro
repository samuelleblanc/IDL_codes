; program to compare the modeled spectra to measured spectra for a few different days
; the spectra is only compared at the band areas
; outputs plots

pro spc_compare,date,docomp=docomp

;date='20120824'
dir='/argus/SSFR3/data/'+date+'/'

; restore the measured data
restore, '/argus/SSFR3/data/'+date+'/out/'+date+'_calibspcs.out'

; restore the modeled spectra
if n_elements(docomp) gt 0 then restore, '/argus/SSFR3/model/spc_comp4_'+date+'.out'

case date of
  '20120602':fl=where(tmhrs gt 21.  and tmhrs lt 23.);  and area2a gt 10  and areaa  gt -2)
  '20120813':fl=where(tmhrs gt 15.  and tmhrs lt 19.);  and area2b gt 10)
  '20120525':fl=where(tmhrs gt 15.  and tmhrs lt 16.);  and tauc   lt 200 and area2c gt 10.)
  '20120523':fl=where(tmhrs gt 21.  and tmhrs lt 24.);  and taud   lt 200 and area2d gt 30.)
  '20120912':fl=where(tmhrs gt 15.5 and tmhrs lt 18.5); and area2e gt 40  and area2e lt 100. and areae gt 5)
  '20120806':fl=where(tmhrs gt 22.  and tmhrs lt 24.5); used to be 20 to 24.5
  '20120816':fl=where(tmhrs gt 13.5 and tmhrs lt 17.)
  '20120820':fl=where(tmhrs gt 16.  and tmhrs lt 20.);  and area2h gt 10. and areah  lt 4.)
  '20120824':fl=where(tmhrs gt 19.5 and tmhrs lt 23.);  and area2i gt 25)
  '20130110':fl=where(tmhrs gt 15.  and tmhrs lt 22.)
  '20130111':fl=where(tmhrs gt 21.  and tmhrs lt 23.)
endcase
days=[20120602,20120813,20120525,20120523,20120912,20120806,20120816,20120820,20120824]
;stop

set_plot,'x'
loadct, 39
window, 0, retain=2, title='calibrated'
window, 1, retain=2, title='normalized'
device, decomposed=0
!y.style=0 & !x.style=1 & !x.margin=[8,8]

i940=[152,206]
i1150=[211,242]
i810=[133,153]
iox=[120,131]

action =''
print, 'to save spectra press "s"'
for i=0, n_elements(fl)-1,10 do begin
  wset, 0
  if n_elements(docomp) gt 0 then taust=strtrim(tau[i],2) else taust='???'
  plot, zenlambda,zspectra[*,fl[i]],title='Measured vs. modeled spectra, tau='+taust,ytitle='Radiance (W/m!U2!N sr)',$
   xtitle='Wavelength (nm)',psym=-2, ystyle=9
 if n_elements(docomp) gt 0 then  oplot,wvl,spc[i,*],color=250,psym=2
  axis, yaxis=1, yrange=[-0.03,0.03],ytitle='Derivative',/save
  oplot, zenlambda, deriv(zenlambda, zspectra[*,fl[i]]/max(zspectra[*,fl[i]])), linestyle=-5, color=70


  wset, 1
  plot, zenlambda, zspectra[*,fl[i]],title='Measured vs. modeled spectra for normalized values utc:'+strtrim(tmhrs[fl[i]],2),$
   ytitle='Normalized radiance',xtitle='Wavelenght (nm)',/nodata, yrange=[0.,1.2],xrange=[700.,1300.]
  oplot,zenlambda, zenlambda*0.+1., linestyle=2, color=70


  sps=smooth(zspectra[*,fl[i]],3)
  spi0=interpol(sps[i940],zenlambda[i940],zenlambda[i940[0]:i940[1]])
  oplot, zenlambda[i940[0]:i940[1]], sps[i940[0]:i940[1]]/spi0,psym=-2
  oplot, zenlambda[i940[0]:i940[1]], sps[i940[0]:i940[1]]/spi0,psym=2,color=100

  spi1 =interpol(zspectra[i1150,fl[i]],zenlambda[i1150],zenlambda[i1150[0]:i1150[1]])  
  oplot, zenlambda[i1150[0]:i1150[1]], zspectra[i1150[0]:i1150[1],fl[i]]/spi1,psym=-2
  oplot, zenlambda[i1150[0]:i1150[1]], zspectra[i1150[0]:i1150[1],fl[i]]/spi1,psym=2,color=120

  spi2 =interpol(zspectra[iox,fl[i]],zenlambda[iox],zenlambda[iox[0]:iox[1]])
  oplot, zenlambda[iox[0]:iox[1]], zspectra[iox[0]:iox[1],fl[i]]/spi2,psym=-2
  oplot, zenlambda[iox[0]:iox[1]], zspectra[iox[0]:iox[1],fl[i]]/spi2,psym=2,color=140

  spi3 =interpol(zspectra[i810,fl[i]],zenlambda[i810],zenlambda[i810[0]:i810[1]]) 
  oplot, zenlambda[i810[0]:i810[1]], zspectra[i810[0]:i810[1],fl[i]]/spi3,psym=-2
  oplot, zenlambda[i810[0]:i810[1]], zspectra[i810[0]:i810[1],fl[i]]/spi3,psym=2,color=160

   if n_elements(docomp) gt 0 then oplot, wvli, spci[i,*], psym=2, color=250

wait, 0.3
action=get_kbrd(0)
if action eq 's' then begin
  wset,0
  p=tvrd(true=1)
  write_png, '/home/leblanc/SSFR3/data/'+date+'_spc_ex.png',p
  wset,1
  p=tvrd(true=1)
  write_png, '/home/leblanc/SSFR3/data/'+date+'_norm_spc_ex.png',p
  ;continue
  sp=zspectra[*,fl[i]]
  wl=zenlambda
  dsp=deriv(zenlambda, zspectra[*,fl[i]]/max(zspectra[*,fl[i]]))  
  utc=tmhrs[fl[i]]  
  f=file_search('/home/leblanc/SSFR3/data/'+date+'*_sp_ex.out',count=n)
  if n gt 0 then begin
   save, sp,wl,dsp,utc,filename='/home/leblanc/SSFR3/data/'+date+string(n+1,format='(I02)')+'_sp_ex.out'
  endif else $
  save, sp,wl,dsp,utc,filename='/home/leblanc/SSFR3/data/'+date+'_sp_ex.out'
stop
endif
endfor
stop
end
