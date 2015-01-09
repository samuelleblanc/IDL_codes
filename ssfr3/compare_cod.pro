; program that runs the oxygen-a band and water vapor area calculations
; then uses then relationship determined by homogeneously cloudy areas
; to determine the cloud optical depth determine from these measures
; these optical depths area compare to what is determined by the 'traditional method'

@calc_water_oxy_abs.pro
@zensun.pro
@legend.pro
pro compare_cod,datein,doplot=doplot

if not keyword_set(doplot) then doplot=0 else doplot=1
plotall=0
dir='/home/leblanc/SSFR3/data/'
if n_elements(datein) lt 1 then date='20120926' else date=datein
l='/'

;get the data
print, 'restoring: '+dir+date+l+date+'_calibspcs.out'
restore, '/argus/SSFR3/data/'+date+'/out/'+date+'_calibspcs.out'

fac=1

;build the sza array
print, 'building the sza arrays'
;nd=184
;doy=findgen(nd)+2456048.5
;caldat,doys,month,day,year
lat=40.007916667
lon=-105.26825
nd=n_elements(tmhrs)
doy=julian_day(float(strmid(date,0,4)),float(strmid(4,2)),float(strmid(6,2)))
zensun, doy, tmhrs,lat, lon, sza, azimuth, solfac


;transfer the radiance into transmittance
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
  fo=[interpol(fvis,F_o.field1[0,*],zenlambda[0:193]),interpol(fnir,F_o.field1[0,*],zenlambda[194:*])]

; now setup to the variable arrays to save the values

disd=fltarr(nd)*!values.f_nan
area=fltarr(nd)*!values.f_nan
area2=fltarr(nd)*!values.f_nan
oxa=fltarr(nd)*!values.f_nan
area2v2=fltarr(nd)*!values.f_nan
v2=fltarr(nd,3)

if doplot then begin
  set_plot, 'x'
  device,decomposed=0
  loadct, 39
  !p.multi=[0,1,3] & !p.thick=1
  window, 0, xsize=400,ysize=900
  window, 1, xsize=400,ysize=900
endif

if plotall then begin
  !p.multi=0
  window,2
  ;now restore the optical depth calculated by 'traditional' methods
  restore, '/argus/SSFR3/data/'+date+'/out/'+date+'_cld_parms.out'
endif

print, 'starting the loop'
for i=0, nd-1 do begin
  if sza[i] gt 85. then continue
  if i mod 50 eq 0 then print, 'doing',i,'/',nd-1
  sp0=zspectra[*,i]*!PI/solfac[i]/fo
  if not plotall then begin
    if fac then calc_water_oxy_abs,sp0,zenlambda,sza[i],w1_area,w2_area,oxy_area,dista,w2_areav2=w2_areav2,solfac=solfac[i],/trans,v2=v2t,doplot=doplot else $
     calc_water_oxy_abs,sp0,zenlambda,sza[i],w1_area,w2_area,oxy_area,dista,w2_areav2=w2_areav2,/trans,v2=v2t,doplot=doplot
    disd[i]=dista
    area[i]=w1_area
    area2[i]=w2_area
    oxa[i]=oxy_area
    area2v2[i]=w2_areav2
    v2[i,*]=v2t
  endif else begin
    plot,zenlambda,sp0/mean(sp0[44:46]),title='spectra t:'+strtrim(tau[i],2)+' h:'+strtrim(tmhrs[i],2),yrange=[0,1.5];,xrange=[600.,1400.]
    wait, 0.02
    i=i+10
  endelse
endfor


;now restore the relationships to correlate the water vapor and oxygen-a absorption to cod 
restore, dir+'relations.out'

ao=p & ao2=p2 & ox=px

cod_ar1=area*ao[1]+ao[0]
cod_ar2=area2*ao2[1]+ao2[0]
cod_ox=oxa*ox[1]+ox[0]

;now restore the optical depth calculated by 'traditional' methods
restore, '/argus/SSFR3/data/'+date+'/out/'+date+'_cld_parms.out'

;now plot the differences
fn=dir+date+l+date+'_cod_comp'
set_plot,'ps'
print, 'making plot :'+fn
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fn+'.ps'
 device, xsize=40, ysize=20
 !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
 !p.multi=[0,2,1] & !x.margin=[7,2] &!x.omargin=[0,0]

 plot, tmhrs, tau, yrange=[0,210],title='Cloud optical depth comparison',xtitle='UTC (h)', ytitle='Cloud optical depth'
 oplot,tmhrs, cod_ar1,color=70
 oplot,tmhrs, cod_ar2,color=130
 oplot,tmhrs, cod_ox,color=250
 legend,['Traditional','940 nm band','1150 nm band','Oxygen-a'],textcolors=[0,70,130,250],box=0

 plot, tau, cod_ox, yrange=[0,210],xrange=[0,210],title='Comparison of optical depth', xtitle='Traditional cloud optical depth',ytitle='Enhanced optical depth',psym=3
 oplot, tau, cod_ar1, psym=3, color=70
 oplot, tau, cod_ar2, psym=3, color=130
 oplot, tau, cod_ox, psym=3, color=250
 legend,['940 nm band','1150 nm band','Oxygen-a'],textcolors=[70,130,250],box=0
 
device, /close
spawn, 'convert '+fn+'.ps '+fn+'.png'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; save the values ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
save, tmhrs, v2,sza,cod_ar1,cod_ar2,cod_ox,tau,ref,disd, area,area2,oxa,filename=dir+date+l+date+'_cod_comp.out'


;;;;;;;;;;;;;;;;;set up plotting ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
set_plot, 'x'
device, decomposed=0
loadct, 39
!p.multi=0 & !p.thick=1
window, 1, retain=2
plot, tmhrs, tau, psym=2, yrange=[0,200],title='optical depth over time'
oplot, tmhrs,v2[*,0],psym=2, color=250
oplot, tmhrs,v2[*,1],psym=2, color=130
oplot, tmhrs,v2[*,2],psym=2, color=70
window, 2, retain=2
plot, tau, v2[*,0],psym=2, xrange=[0,200],title='940nm water vapor vs tau'
window, 3, retain=2
plot, tau, v2[*,1],psym=2, xrange=[0,200],title='1150nm water vapor vs tau'
window, 4, retain=2
plot, tau, v2[*,2],psym=2, xrange=[0,200],title='oxygen-a vs tau'

stop
end
