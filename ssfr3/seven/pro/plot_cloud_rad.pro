;+
; NAME:
;   plot_cloud_rad
;
; PURPOSE:
;   Program to plot multiple cloud spectra, Trying to determine the water vapor absorption band width and oxygen-a band width
;
; CATEGORY:
;   data - photon path length determination
;
; CALLING SEQUENCE:
;   plot_cloud_rad, date
;   - where date is the date in yyyymmdd for the data
;
; OUTPUT:
;   plots 
;
; KEYWORDS:
;   - goes : set if you want to use the goes data
;
; DEPENDENCIES:
; 
;
; EXAMPLE:
; 
;
; MODIFICATION HISTORY:
; Written:  Samuel LeBlanc, ATOC-LASP CU Boulder, Thursday, Nov. 1st, 2012
; Modified: 
;
;---------------------------------------------------------------------------

@legend.pro
@zensun.pro

pro plot_cloud_rad, datein,goes=goes
if n_elements(datein) lt 1 then $
  date='20120926' else $
  date=datein

if !version.os eq 'linux' then begin
  dir='/argus/SSFR3/data/'+date+'/out/'
  dir_dat='/home/leblanc/SSFR3/data/'+date+'/'
  display='x'
  spawn, 'mkdir '+dir_dat
endif else begin
  dir='C:/Users/Samuel/Research/SSFR3/data/'
  dir_dat=dir
  display='win'
endelse

if keyword_set(goes) then goes=1 else goes=0

lat=40.007916667
lon=-105.26825
doy=julian_day(fix(strmid(date,0,4)),fix(strmid(date,4,2)),fix(strmid(date,6,2)))

restore, dir+date+'_calibspcs.out'
zensun, doy, tmhrs,lat, lon, sza, azimuth, solfac

;get the top of atmosphere irradiance to calculate the transmittance
F_o=read_ascii('/home/leblanc/libradtran/libRadtran-1.6-beta/data/solar_flux/kurudz_1.0nm.dat', comment_symbol='#',data_start=12)
vis=read_ascii('/home/leblanc/libradtran/vis_1nm.dat')
nir=read_ascii('/home/leblanc/libradtran/nir_1nm.dat')

F_o.field1[1,*]=F_o.field1[1,*]/1000.
vis.field1[1,*]=vis.field1[1,*]/total(vis.field1[1,*])
nir.field1[1,*]=nir.field1[1,*]/total(nir.field1[1,*])
fvis=fltarr(n_elements(F_o.field1[1,*]))
fnir=fvis
for i=7,n_elements(f_o.field1[0,*])-8 do for j=-7,7 do fvis[i]=fvis[i]+f_o.field1[1,i+j]*vis.field1[1,j]
for i=15,n_elements(f_o.field1[0,*])-16 do for j=-15,15 do fnir[i]=fnir[i]+f_o.field1[1,i+j]*nir.field1[1,j]
foz=[interpol(fvis,F_o.field1[0,*],zenlambda[0:193]),interpol(fnir,F_o.field1[0,*],zenlambda[194:*])]
fon=[interpol(fvis,F_o.field1[0,*],nadlambda[0:193]),interpol(fnir,F_o.field1[0,*],nadlambda[194:*])]

set_plot, display
device, decomposed=0
loadct, 39, /silent
window, 0
window, 1
!x.style=1 & !y.style=1 & !x.margin=[6,6] & !p.multi=0

dist=fltarr(n_elements(tmhrs))
area=fltarr(n_elements(tmhrs))
wvs=fltarr(n_elements(tmhrs),2)

distr=dist
arear=area
wvsr=wvs

;part of code for oxygen a band
nt=where(finite(tmhrs) eq 1, n)

nul=min(abs(zenlambda-750.),fiz)
nul=min(abs(zenlambda-775.),laz)
nul=min(abs(nadlambda-750.),fin)
nul=min(abs(nadlambda-775.),lan)

radoxa=fltarr(n) ;oxugen a area
irroxa=fltarr(n)

;loop through all the data points and display the results on the screen
if 0 then begin
first=1
if first then nwv=[149,169,193,204] else nwv=[198,212,220,232]

for i=5000, n_elements(tmhrs)-8000,5 do begin
  ddrad=deriv(deriv(zspectra[*,i]*!PI/cos(sza[i]*!dtor)/foz))
  dr=deriv(zspectra[*,i]*!PI/cos(sza[i]*!dtor)/foz)
  wset,0
  plot, zenlambda, zspectra[*,i]*!PI/cos(sza[i]*!dtor)/foz, title='Transmittance', ytitle='Transmittance',xtitle='Wavelength (nm)'
  wset,1
  plot, zenlambda, zspectra[*,i]*!PI/cos(sza[i]*!dtor)/foz,title='Cloud transmittance and its derivative  at '+string(tmhrs[i],format='(F6.3)'), $
   ytitle='Transmittance', ystyle=9, xrange=[800,1250]
  axis, yaxis=1, yrange=[-1.0,1.0], ytitle='Derivative',color=250, /save
  oplot, [350,1700],[0,0],linestyle=1,color=250
  oplot, zenlambda, dr/max(dr[nwv[0]:nwv[3]]), color=250
 ; oplot, zenlambda, smooth(deriv(zspectra[*,i]),4)/max(deriv(zspectra[149:204,i])),color=70 ;for the first band
  oplot, zenlambda, ddrad/max(ddrad[nwv[0]:nwv[3]]),color=70
 ; oplot, zenlambda, deriv(deriv(zspectra[*,i])),color=150
  drad=smooth(deriv(zspectra[*,i]*!PI/cos(sza[i]*!dtor)/foz)/max(deriv(zspectra[nwv[0]:nwv[3],i]*!PI/cos(sza[i]*!dtor)/foz[nwv[0]:nwv[3]])),2) 
  
 if first then begin
   nmins=where(drad[nwv[0]:nwv[1]] lt 0.2 and drad[nwv[0]:nwv[1]] gt -0.2 and shift(drad[nwv[0]:nwv[1]],1) gt -0.2 and shift(drad[nwv[0]:nwv[1]],1) lt 0.2)
   ns=indgen(nwv[1]-nwv[0]+1)+nwv[0]
   nmin=max(nmins)
   nmin=ns[nmin]
 endif else begin
   ;nmins=where(dr[nwv[0]:nwv[1]] gt shift(dr[nwv[0]:nwv[1]],1))
   nmins=where(dr[nwv[0]:nwv[1]]/max(dr[nwv[0]:nwv[3]]) gt (shift(dr[nwv[0]:nwv[1]]/max(dr[nwv[0]:nwv[3]]),1)-0.035))
   ns=indgen(nwv[1]-nwv[0]+1)+nwv[0]
   nmin=max(nmins)
   nmin=ns[nmin]
 endelse

 
 nmaxs=where(drad[nwv[2]:nwv[3]] lt 0.2 and drad[nwv[2]:nwv[3]] gt -0.2 and shift(drad[nwv[2]:nwv[3]],1) gt -0.2 and shift(drad[nwv[2]:nwv[3]],1) lt 0.2)
 ns=indgen(nwv[3]-nwv[2]+1)+nwv[2]
 nmax=min(nmaxs)
 nmax=ns[nmax]

 ;overplot the locations of the thickness of the water vapor absorption band
 oplot, [zenlambda[nmin],zenlambda[nmin]],[-1,1], color=150, linestyle=2
 oplot, [zenlambda[nmax],zenlambda[nmax]],[-1,1], color=150, linestyle=2
 wait, 0.01
 dist[i]=zenlambda[nmax]-zenlambda[nmin]
 area[i]=int_tabulated(zenlambda[nmin:nmax],interpol(zspectra[[nmin,nmax],i],zenlambda[[nmin,nmax]],zenlambda[nmin:nmax])-$
	 zspectra[nmin:nmax,i])
endfor
endif


;determine the water vapor and oxygen-a band properties (integrated area, width) for both radiane and irradiance
narear=arear
narea=area
nradoxa=radoxa
nirroxa=irroxa

narear2=arear
narea2=area
arear2=arear
area2=area
wvs2=wvs

for i=0, n_elements(tmhrs)-1 do begin
;for the first water vapor band
  zsp=zspectra[*,i]*!PI/cos(sza[i]*!dtor)/foz
  nsp=nspectra[*,i]*!PI/cos(sza[i]*!dtor)/fon
  drad=smooth(deriv(zsp)/max(deriv(zsp[149:204])),2)  
  dirr=smooth(deriv(nsp)/max(deriv(nsp[149:204])),2)
 nmir=where(dirr[149:169] lt 0.2 and dirr[149:169] gt -0.2 and shift(dirr[149:169],1) gt -0.2 and shift(dirr[149:169],1) lt 0.2)
 nmins=where(drad[149:169] lt 0.2 and drad[149:169] gt -0.2 and shift(drad[149:169],1) gt -0.2 and shift(drad[149:169],1) lt 0.2)
  ns=indgen(21)+149 
  nmin=max(nmins) 
  nmin=ns[nmin] 
  nminr=max(nmir)
  nminr=ns[nminr]
       
 nmaxs=where(drad[193:204] lt 0.2 and drad[193:204] gt -0.2 and shift(drad[193:204],1) gt -0.2 and shift(drad[193:204],1) lt 0.2)
 nmar=where(dirr[193:204] lt 0.2 and dirr[193:204] gt -0.2 and shift(dirr[193:204],1) gt -0.2 and shift(dirr[193:204],1) lt 0.2)

 ns=indgen(12)+193 
 nmaxr=min(nmar)
 nmax=min(nmaxs) 
  nmax=ns[nmax] 
 nmaxr=ns[nmaxr]
   
  distr[i]=nadlambda[nmaxr]-nadlambda[nminr]
  narear[i]=int_tabulated(nadlambda[nminr:nmaxr],interpol(nsp[[nminr,nmaxr]],nadlambda[[nminr,nmaxr]],$
	  nadlambda[nminr:nmaxr])-nsp[nminr:nmaxr])
	arear[i]=int_tabulated(nadlambda[nminr:nmaxr],interpol(nspectra[[nminr,nmaxr],i],nadlambda[[nminr,nmaxr]],$
    nadlambda[nminr:nmaxr])-nspectra[nminr:nmaxr,i])
  wvsr[i,0]=nadlambda[nminr]
  wvsr[i,1]=nadlambda[nmaxr]

  dist[i]=zenlambda[nmax]-zenlambda[nmin] 
  narea[i]=int_tabulated(zenlambda[nmin:nmax],interpol(zsp[[nmin,nmax]],zenlambda[[nmin,nmax]],zenlambda[nmin:nmax])-$
   zsp[nmin:nmax])
  area[i]=int_tabulated(zenlambda[nmin:nmax],interpol(zspectra[[nmin,nmax],i],zenlambda[[nmin,nmax]],zenlambda[nmin:nmax])-$
   zspectra[nmin:nmax,i])
  wvs[i,0]=zenlambda[nmin]
  wvs[i,1]=zenlambda[nmax]


;for the second water vapor band
   nwv=[198,212,220,232]
   dr=deriv(zsp)
   nmins=where(dr[nwv[0]:nwv[1]]/max(dr[nwv[0]:nwv[3]]) gt (shift(dr[nwv[0]:nwv[1]]/max(dr[nwv[0]:nwv[3]]),1)-0.035))
   ns=indgen(nwv[1]-nwv[0]+1)+nwv[0]
   nmin=max(nmins)
   nmin=ns[nmin]
   
   nmaxs=where(drad[nwv[2]:nwv[3]] lt 0.2 and drad[nwv[2]:nwv[3]] gt -0.2 and shift(drad[nwv[2]:nwv[3]],1) gt -0.2 and shift(drad[nwv[2]:nwv[3]],1) lt 0.2)
   ns=indgen(nwv[3]-nwv[2]+1)+nwv[2]
   nmax=min(nmaxs)
   nmax=ns[nmax]

   wvs2[i,0]=zenlambda[nmin]
   wvs2[i,1]=zenlambda[nmax]
  
  nminr=nmin
  nmaxr=nmax
  
  narea2[i]=int_tabulated(zenlambda[nmin:nmax],interpol(zsp[[nmin,nmax]],zenlambda[[nmin,nmax]],zenlambda[nmin:nmax])-$
   zsp[nmin:nmax])
  area2[i]=int_tabulated(zenlambda[nmin:nmax],interpol(zspectra[[nmin,nmax],i],zenlambda[[nmin,nmax]],zenlambda[nmin:nmax])-$
   zspectra[nmin:nmax,i])
  narear2[i]=int_tabulated(nadlambda[nminr:nmaxr],interpol(nsp[[nminr,nmaxr]],nadlambda[[nminr,nmaxr]],$
    nadlambda[nminr:nmaxr])-nsp[nminr:nmaxr])
  arear2[i]=int_tabulated(nadlambda[nminr:nmaxr],interpol(nspectra[[nminr,nmaxr],i],nadlambda[[nminr,nmaxr]],$
    nadlambda[nminr:nmaxr])-nspectra[nminr:nmaxr,i])

;for the oxygen a band
   oxar=-zspectra[fiz:laz,i]+interpol(zspectra[[fiz,laz],i],zenlambda[[fiz,laz]],zenlambda[fiz:Laz])
   oxai=-nspectra[fin:lan,i]+interpol(nspectra[[fin,lan],i],nadlambda[[fin,lan]],nadlambda[fin:Lan])

   radoxa[i]=int_tabulated(zenlambda[fin:lan],oxar)
   irroxa[i]=int_tabulated(nadlambda[fin:lan],oxai)

   noxar=-zsp[fiz:laz]+interpol(zsp[[fiz,laz]],zenlambda[[fiz,laz]],zenlambda[fiz:Laz])
   noxai=-nsp[fin:lan]+interpol(nsp[[fin,lan]],nadlambda[[fin,lan]],nadlambda[fin:Lan])

   nradoxa[i]=int_tabulated(zenlambda[fin:lan],noxar)
   nirroxa[i]=int_tabulated(nadlambda[fin:lan],noxai)

endfor

;window, 1, title='Water vapor distance'
;plot, tmhrs,dist, title='Water vapor distance', xtitle='UTC (h)', ytitle='Distance (nm)',psym=3
;window, 2, title='Water vapor Area'
;plot, tmhrs, area, title='Water vapor area', xtitle='UTC (h)', ytitle='Area',psym=3
;window, 3, title='Water vapor and oxygen a'
;plot, area, radoxa, xtitle='Water vapor area',ytitle='Oxygen-a band area',psym=3


; now do the plot on a ps
if 1 then begin ; pot an example spectra
; now do the plot on a ps 
print, 'making plot :'+dir_dat+date+'_water_bandt.ps' 
set_plot, 'ps' 
 loadct, 39, /silent 
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=dir_dat+date+'_water_bandt.ps'
 device, xsize=20, ysize=20 
  !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8 
  !p.multi=0 & !x.margin=[7,7] 
  
  plot, zenlambda, zspectra[*,10000]*!PI/cos(sza[10000]*!dtor)/foz,title='Cloud transmittance and its derivative  at '+string(tmhrs[10000],format='(F6.3)'), $
	     ytitle='Transmittance',xtitle='Wavelength (nm)', ystyle=9, xrange=[800,1100]
  axis, yaxis=1, yrange=[-1.0,1.0], ytitle='normalized derivative',color=250, /save
  oplot, [800,1100],[0,0],linestyle=1,color=250
  oplot, zenlambda, smooth(deriv(zspectra[*,10000]*!PI/cos(sza[10000]*!dtor)/foz)/max(deriv(zspectra[149:204,10000]*!PI/cos(sza[10000]*!dtor)/foz[149:204])),2), color=250

 ;overplot the locations of the thickness of the water vapor absorption band
  oplot, [wvs2[10000,0],wvs2[10000,0]],[-1,1], color=150, linestyle=2
  oplot, [wvs2[10000,1],wvs2[10000,1]],[-1,1], color=150, linestyle=2

legend,['Radiance','Derivative','Water vapor bounds'],textcolors=[0,250,150],box=0,/right

 device, /close
 spawn, 'convert '+dir_dat+date+'_water_bandt.ps '+dir_dat+date+'_water_bandt.png'

print, 'making plot :'+dir_dat+date+'_2water_bandt.ps' 
set_plot, 'ps' 
 loadct, 39, /silent 
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=dir_dat+date+'_2water_bandt.ps'
 device, xsize=20, ysize=20 
  !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8 
  !p.multi=0 & !x.margin=[7,7] 
  
  plot, zenlambda, zspectra[*,10000]*!PI/cos(sza[10000]*!dtor)/foz,title='Cloud transmittance and its derivative  at '+string(tmhrs[10000],format='(F6.3)'), $
       ytitle='Transmittance',xtitle='Wavelength (nm)', ystyle=9, xrange=[1000,1200]
  axis, yaxis=1, yrange=[-1.0,1.0], ytitle='normalized derivative',color=250, /save
  oplot, [1000,1200],[0,0],linestyle=1,color=250
  oplot, zenlambda, smooth(deriv(zspectra[*,10000]*!PI/cos(sza[10000]*!dtor)/foz)/max(deriv(zspectra[198:232,10000]*!PI/cos(sza[10000]*!dtor)/foz[198:232])),2), color=250

 ;overplot the locations of the thickness of the water vapor absorption band
  oplot, [wvs[10000,0],wvs[10000,0]],[-1,1], color=150, linestyle=2
  oplot, [wvs[10000,1],wvs[10000,1]],[-1,1], color=150, linestyle=2

legend,['Radiance','Derivative','Water vapor bounds'],textcolors=[0,250,150],box=0,/right

 device, /close
 spawn, 'convert '+dir_dat+date+'_2water_bandt.ps '+dir_dat+date+'_2water_bandt.png'

endif


;load the GOES derivated values of water vapor
if goes then restore, dir_dat+date+'_wv_GOES.out'

fl=where(tmhrs gt 14. and tmhrs lt 24.)
print, 'making plot :'+dir_dat+date+'_water_oxy_tran.ps'
set_plot, 'ps'
 loadct, 39, /silent
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=dir_dat+date+'_water_oxy_tran.ps'
   device, xsize=15, ysize=40
   !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
   !p.multi=[0,1,3] & !x.margin=[7,7]

   plot, tmhrs[fl],dist[fl], title='Water vapor absorption band width', xtitle='UTC (h)',ytitle='Width (nm)', psym=3
 if goes then begin
   oplot, utc, wv*max(dist[fl],/nan)/300., color=250, psym=2
   legend, ['Band width','GOES water vapor'],textcolors=[0,250],box=0
 endif
  
   plot, tmhrs[fl],area[fl], title='Water vapor absorption band integrated area', xtitle='UTC (h)', ytitle='Area (W/m!U2!N sr)', psym=3, ystyle=9
   axis,yaxis=1,yrange=[0,max(narea[fl])],ytitle='Normalized area',/save
   oplot, tmhrs[fl], narea[fl], color=70
  ; oplot, tmhrs[fl], narea2[fl],color=170
 if goes then oplot, utc, wv*25./300., color=250, psym=2
   legend, ['Band width','Normalized band width','GOES water vapor'],charsize=1.0,textcolors=[0,70,250],box=0,/bottom
   
   plot, narea[fl], nradoxa[fl], title='Correspondance between Oxygen-a band and Water vapor absorption band',$
	   xtitle='Normalized water vapor band integrated area', ytitle='Normalized oxygen-a band integrated area',$
	   psym=3, yrange=[0,4],xrange=[0,25]
   oplot, narea2[fl],nradoxa[fl],psym=3,color=170

device, /close
spawn, 'convert '+dir_dat+date+'_water_oxy_tran.ps '+dir_dat+date+'_water_oxy_tran.png'

;nn=where(narea2[fl] lt 10.)
;narea2[fl[nn]]=!values.f_nan

print, 'making plot :'+dir_dat+date+'_2water_oxy_tran.ps'
set_plot, 'ps'
 loadct, 39, /silent
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=dir_dat+date+'_2water_oxy_tran.ps'
   device, xsize=15, ysize=40
   !p.font=1 & !p.thick=5 & !p.charsize=2.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
   !p.multi=[0,1,3] & !x.margin=[7,7]

 
   plot, tmhrs[fl],narea[fl], title='Water vapor absorption band normalized integrated area', xtitle='UTC (h)', ytitle='Normalized Area', psym=3, yrange=[0,25]
   oplot, tmhrs[fl], narea2[fl],color=170
  if goes then oplot, utc, wv*25./300., color=250, psym=2
   legend, ['1st Band','2nd Band','GOES water vapor'],charsize=1.0,textcolors=[0,170,250],box=0,/bottom
   
   plot, narea[fl],narea2[fl], title='Correlation between the two water vapor bands',$
    xtitle='Normalized 1st water vapor band integrated area', ytitle='Normalized 2nd water vapor band integrated area',$
     psym=3, yrange=[0,25],xrange=[0,25]
   
   plot, narea[fl], nradoxa[fl], title='Correlation between Oxygen-a and Water vapor band',$
     xtitle='Normalized water vapor band integrated area', ytitle='Normalized oxygen-a band integrated area',$
     psym=3, yrange=[0,4],xrange=[0,25]
   oplot, narea2[fl],nradoxa[fl],psym=3,color=170
   legend,['1st band','2nd band'],textcolors=[0,170],box=0,charsize=1.0

device, /close
spawn, 'convert '+dir_dat+date+'_2water_oxy_tran.ps '+dir_dat+date+'_2water_oxy_tran.png'


set_plot, 'ps'
print, 'making plot :'+dir_dat+date+'_water_oxy_irrn.ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=dir_dat+date+'_water_oxy_irr.ps'
 device, xsize=15, ysize=40
 !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
 !p.multi=[0,1,3] & !x.margin=[7,2]
	     
  plot, tmhrs,distr, title='Water vapor absorption band width', xtitle='UTC (h)',ytitle='Width (nm)', psym=3
  plot, tmhrs,arear, title='Water vapor absorption band integrated area', xtitle='UTC (h)', ytitle='Area (W/m!U2!N)', psym=3
  plot, arear, irroxa, title='Correspondance between Oxygen-a band and Water vapor absorption band',$
        xtitle='Water vapor band integrated area (W/m!U2!N)', ytitle='Oxygen-a band integrated area (W/m!U2!N)',$
        psym=3 
					    
 device, /close 
spawn, 'convert '+dir_dat+date+'_water_oxy_irr.ps '+dir_dat+date+'_water_oxy_irr.png'


save, dist, area, wvs, tmhrs, radoxa,irroxa,distr, arear, wvsr,narea,narea2,nradoxa,nirroxa, filename=dir_dat+date+'_wv.out'
stop
end
