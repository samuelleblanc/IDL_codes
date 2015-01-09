; program to compare the integrated water vapor absorption to the precipitable water vapor measured by GPS
; taken at NOAA Boulder
; only for homogeneously cloudy days.

@zensun.pro
@legend.pro
@colorbar.pro
pro rad_sounding_cl

dir='/home/leblanc/SSFR3/data/'

; get spectra
restore, dir+'sp_at_cl.out'
print, 'restoring spectra file: '+dir+'sp_at_cl.out'

; get aerosol optical depth
;restore, dir+'aero_at_sounding.out'
;print, 'restoring aot file: '+dir+'aero_at_sound.out'

;get sounding
;sound=read_ascii(dir+'water_mm.dat',data_start=1)
;water=reform(sound.field1[1,*])
;print, 'reading file: '+dir+'water_mm.dat'

;get the top of atmosphere irradiance to calculate the transmittance
print, 'getting extra terrestrial irradiance'
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

;build the sza array
print, 'building the sza arrays'
;nd=184
;doy=findgen(nd)+2456048.5
caldat,doys,month,day,year
lat=40.007916667
lon=-105.26825
nd=n_elements(doys)
sza=fltarr(nd)
for i=0, nd-1 do begin
  doyt=julian_day(year[i],month[i],day[i])
  zensun, doyt, (doys[i]-floor(doys[i]))*24.,lat, lon, szat, azimuth, solfac
  sza[i]=szat
endfor

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; now get the integrated water vapor absorption for all spectra;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;prepare arrays
dist=fltarr(nd)
area=fltarr(nd)
wvs=fltarr(nd,2)

distr=dist
arear=area
wvsr=wvs

;part of code for oxygen a band
nt=where(finite(doys) eq 1, n)

nul=min(abs(zenlambda-750.),fiz)
nul=min(abs(zenlambda-775.),laz)
nul=min(abs(nadlambda-750.),fin)
nul=min(abs(nadlambda-775.),lan)

radoxa=fltarr(n) ;oxygen a area
irroxa=fltarr(n)

narear=arear
narea=area
nradoxa=radoxa
nirroxa=irroxa

narear2=arear
narea2=area
arear2=arear
area2=area

for i=0, nd-1 do begin
  print, 'doing day: ',doys[i]
  ;for the first water vapor band
  spz0=spz[*,i]*!PI/cos(sza[i]*!dtor)/foz
  spn0=spn[*,i]*!PI/cos(sza[i]*!dtor)/fon
  
  drad=smooth(deriv(spz0)/max(deriv(spz0[149:204])),2)
  dirr=smooth(deriv(spn0)/max(deriv(spn0[149:204])),2)
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
  narear[i]=int_tabulated(nadlambda[nminr:nmaxr],interpol(spn0[[nminr,nmaxr]],nadlambda[[nminr,nmaxr]],$
          nadlambda[nminr:nmaxr])-spn0[nminr:nmaxr])
        arear[i]=int_tabulated(nadlambda[nminr:nmaxr],interpol(spn[[nminr,nmaxr],i],nadlambda[[nminr,nmaxr]],$
    nadlambda[nminr:nmaxr])-spn[nminr:nmaxr,i])
  wvsr[i,0]=nadlambda[nminr]
  wvsr[i,1]=nadlambda[nmaxr]

  dist[i]=zenlambda[nmax]-zenlambda[nmin]
  narea[i]=int_tabulated(zenlambda[nmin:nmax],interpol(spz0[[nmin,nmax]],zenlambda[[nmin,nmax]],zenlambda[nmin:nmax])-$
   spz0[nmin:nmax])
  area[i]=int_tabulated(zenlambda[nmin:nmax],interpol(spz[[nmin,nmax],i],zenlambda[[nmin,nmax]],zenlambda[nmin:nmax])-$
   spz[nmin:nmax,i])
  wvs[i,0]=zenlambda[nmin]
  wvs[i,1]=zenlambda[nmax]

  ;for the second water vapor band
  nwv=[198,212,220,232]
  dr=deriv(spz0)
  nmins=where(dr[nwv[0]:nwv[1]]/max(dr[nwv[0]:nwv[3]]) gt (shift(dr[nwv[0]:nwv[1]]/max(dr[nwv[0]:nwv[3]]),1)-0.035))
  ns=indgen(nwv[1]-nwv[0]+1)+nwv[0]
  nmin=max(nmins)
  nmin=ns[nmin]
  nmaxs=where(drad[nwv[2]:nwv[3]] lt 0.2 and drad[nwv[2]:nwv[3]] gt -0.2 and shift(drad[nwv[2]:nwv[3]],1) gt -0.2 and shift(drad[nwv[2]:nwv[3]],1) lt 0.2)
  ns=indgen(nwv[3]-nwv[2]+1)+nwv[2]
  nmax=min(nmaxs)
  nmax=ns[nmax]

  nminr=nmin
  nmaxr=nmax

  narea2[i]=int_tabulated(zenlambda[nmin:nmax],interpol(spz0[[nmin,nmax]],zenlambda[[nmin,nmax]],zenlambda[nmin:nmax])-$
   spz0[nmin:nmax])
  area2[i]=int_tabulated(zenlambda[nmin:nmax],interpol(spz[[nmin,nmax],i],zenlambda[[nmin,nmax]],zenlambda[nmin:nmax])-$
   spz[nmin:nmax,i])
  narear2[i]=int_tabulated(nadlambda[nminr:nmaxr],interpol(spn0[[nminr,nmaxr]],nadlambda[[nminr,nmaxr]],$
    nadlambda[nminr:nmaxr])-spn0[nminr:nmaxr])
  arear2[i]=int_tabulated(nadlambda[nminr:nmaxr],interpol(spn[[nminr,nmaxr],i],nadlambda[[nminr,nmaxr]],$
    nadlambda[nminr:nmaxr])-spn[nminr:nmaxr,i])

  ;for the oxygen a band
  oxar=-spz[fiz:laz,i]+interpol(spz[[fiz,laz],i],zenlambda[[fiz,laz]],zenlambda[fiz:Laz])
  oxai=-spn[fin:lan,i]+interpol(spn[[fin,lan],i],nadlambda[[fin,lan]],nadlambda[fin:Lan])

  radoxa[i]=int_tabulated(zenlambda[fin:lan],oxar)
  irroxa[i]=int_tabulated(nadlambda[fin:lan],oxai)

  noxar=-spz0[fiz:laz]+interpol(spz0[[fiz,laz]],zenlambda[[fiz,laz]],zenlambda[fiz:Laz])
  noxai=-spn0[fin:lan]+interpol(spn0[[fin,lan]],nadlambda[[fin,lan]],nadlambda[fin:Lan])

  nradoxa[i]=int_tabulated(zenlambda[fin:lan],noxar)
  nirroxa[i]=int_tabulated(nadlambda[fin:lan],noxai)

endfor

; now filter for only values of sp that contain data
n=where(spz[45,*] ne 0.0)
;build filter of only the clear days
fl_cl=[13,23,38,39,40,41,51,52,53,64,70,71,72,80,109,$
       116,117,126,127,130,131,135,136,151,154,166,169,170]

set_plot, 'ps'
loadct, 39, /silent
fn=dir+'sound_water_pw3_cl'
print, 'plotting: '+fn
device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fn+'.ps'
 device, xsize=20, ysize=40
 !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
 !p.multi=[0,1,2] & !x.margin=[7,2]

;narean[]/tau[250,n]
plot, water[n]/cos(sza[n]*!dtor), narea[n], psym=2, title='Water vapor absorption vs. precipitable content',ytitle='Integrated water vapor absorption per aot',$
 xtitle='Precipitable water content x airmass factor (mm)';,yrange=[0,50],xrange=[0,200]
oplot, water[n]/cos(sza[n]*!dtor), narea2[n], psym=2, color=250
;oplot, water[fl_cl],narea[fl_cl],psym=2,color=70
legend,['1st water band','2nd water band'],textcolors=[0,250],box=0

nl=where(finite(water[fl_cl]) eq 1)
fl_cl=fl_cl[nl]

;narea[fl_cl]/tau[250,fl_cl]
plot, water[fl_cl]/cos(sza[fl_cl]*!dtor),narea[fl_cl],title='Clear days water vapor',xtitle='Precipitable water content x airmass factor (mm)',ytitle='Integrated water vapor absorption per aot',$
 psym=2;, xrange=[0,200]

oplot, water[fl_cl]/cos(sza[fl_cl]*!dtor),narea2[fl_cl],psym=2,color=250

g=linfit(water[fl_cl]/cos(sza[fl_cl]*!dtor),narea[fl_cl])
y=g[1]*water[fl_cl]/cos(sza[fl_cl]*!dtor)+g[0]
oplot, water[fl_cl]/cos(sza[fl_cl]*!dtor),y,linestyle=1,color=70
xi=correlate(water[fl_cl]/cos(sza[fl_cl]*!dtor),narea[fl_cl])
legend,['1st band','2nd band','Fit R='+strtrim(xi)],textcolors=[0,250,70],box=0

device, /close
spawn, 'convert '+fn+'.ps '+fn+'.png'


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; best plot possible

set_plot, 'ps'
loadct, 39, /silent
fn=dir+'pw_abs_cl'
print, 'plotting: '+fn
device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fn+'.ps'
 device, xsize=20, ysize=20
 !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
 !p.multi=0 & !x.margin=[7,9]

plot, water,narea2,psym=2,title='Water vapor absorption vs. precipitable content',ytitle='Integrated water vapor absorption',$
 xtitle='Precipitable water content (mm)'
for i=0, nd-1 do oplot, [water[i],water[i]],[narea2[i],narea2[i]],psym=2,color=sza[i]/90.*255.

fl=where(sza gt 45.)
l=linfit(water[fl],narea2[fl])
r=correlate(water[fl],narea2[fl])
legend,['R='+strtrim(r,2),'y='+strtrim(l[0],2)+'+'+strtrim(l[1],2)+'x'],textcolors=[0,0],box=0,/right
x=findgen(13)
y=l[0]+l[1]*x
oplot,x,y,linestyle=2
contour, transpose([[findgen(90)],[findgen(90)]]),[0,1],findgen(90),/cell_fill,nlevels=20,position=[0.87,0.13,0.89,0.93],/normal,/noerase,xticks=1,yticks=1,xtickname=[' ',' '],ytickname=[' ',' ']  ;take this out to omit residual
axis, yaxis=1,ystyle=1,yrange=[0,90],ytitle='SZA',yticks=6
device, /close
spawn, 'convert '+fn+'.ps '+fn+'.png'

save, nradoxa,nirroxa,narea,narea2,water,filename=dir+'water_abs_sounding3.out'

stop
end 
