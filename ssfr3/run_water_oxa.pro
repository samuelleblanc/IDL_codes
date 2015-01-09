; program to run through a save file of spectra to calculate the water vapor absorption, area, width and the oxygen a band width
; now hardcorded for only radiance

@calc_water_oxy_abs.pro
@zensun.pro
pro run_water_oxa

dir='/home/leblanc/SSFR3/data/'

cloud=1
fac=1

; get spectra
if cloud then begin
  restore, dir+'sp_at_cl.out'
  print, 'restoring spectra file: '+dir+'sp_at_cl.out'
endif else begin
  restore, dir+'sp_at_wv3.out'
  print, 'restoring spectra file: '+dir+'sp_at_wv3.out'
endelse

;build the sza array
print, 'building the sza arrays'
;nd=184
;doy=findgen(nd)+2456048.5
caldat,doys,month,day,year
lat=40.007916667
lon=-105.26825
nd=n_elements(doys)
sza=fltarr(nd)
solfac=fltarr(nd)
for i=0, nd-1 do begin
  doyt=julian_day(year[i],month[i],day[i])
  zensun, doyt, (doys[i]-floor(doys[i]))*24.,lat, lon, szat, azimuth, solfact
  sza[i]=szat
  solfac[i]=solfact
endfor


; now setup to the variable arrays to save the values

disd=fltarr(nd)
area=fltarr(nd)
area2=fltarr(nd)
oxa=fltarr(nd)
area2v2=fltarr(nd)

for i=0, nd-1 do begin
  print, 'doing',i,'/',nd-1
  if fac then calc_water_oxy_abs,spz[*,i],zenlambda,sza[i],w1_area,w2_area,oxy_area,dista,w2_areav2=w2_areav2,solfac=solfac[i] else $
   calc_water_oxy_abs,spz[*,i],zenlambda,sza[i],w1_area,w2_area,oxy_area,dista,w2_areav2=w2_areav2
  disd[i]=dista
  area[i]=w1_area
  area2[i]=w2_area
  oxa[i]=oxy_area
  area2v2[i]=w2_areav2
endfor

if cloud then fs=dir+'wv_oxa_cl' else fs=dir+'wv_oxa_clear'
if fac then fs=fs+'_fac.out' else fs=fs+'.out'
save, disd, area, area2,oxa, cod, tau, sza, water,area2v2,filename=fs

set_plot, 'x'
device, decomposed=0
loadct, 39
window,0,retain=2
plot, disd,cod,psym=2,xtitle='Water vapor width (nm)',ytitle='Cloud optical depth'
window,1,retain=2
plot, oxa,cod,psym=2,xtitle='Oxygen-a band area',ytitle='Cloud optical depth'


stop
end
