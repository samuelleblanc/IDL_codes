; program that read in alot of data and then view all of it
; purpose is to figure out the dark issues

@get_wvl.pro
@interpol.pro
@cfg.pro

pro ssfr3_analysis

date='20120610'
dir='/argus/roof/SSF3/data/' ;'/data/seven/DC3/SSFR3/'
l='/'

get_wvl, dir+date+l+date+'.cfg', wvl,zenlambda,nadlambda,indices,/reverse

; setup the various variables found in the RT_???.out files
big=15000
nspectra_big  = fltarr(big,n_elements(nadlambda))
zspectra_big  = fltarr(big,n_elements(zenlambda))
spect_big     = fltarr(big,256,4)
spect_cal_big = fltarr(big,256,4)
sza_big       = fltarr(big)
utc_big       = fltarr(big)
ssfr_temp_big = fltarr(big,3)

nspectra_bigdrk  = fltarr(big,n_elements(nadlambda))
zspectra_bigdrk  = fltarr(big,n_elements(zenlambda))
spect_bigdrk     = fltarr(big,256,4)
spect_cal_bigdrk = fltarr(big,256,4)
sza_bigdrk       = fltarr(big)
utc_bigdrk       = fltarr(big)
ssfr_temp_bigdrk = fltarr(big,3)

np=256

f    = file_search(dir+date+l+'outtest/*RT_???.out',count=nf)
fdrk = file_search(dir+date+l+'outtest/*RT_???_drk.out',count=nd)

nf=20 ;set max files just for now
i=0L
id=0L
for ii=0,nf do begin

print, 'restoring file:' +f[ii]
restore, f[ii]

;;;;;;;;;;;;;;;;;;;;;; now start processing file ;;;;;;;;;;;;;;;;;;;;;;;;;;;^M
i_last=n_elements(utc)-1L + i
if i_last ge big-1 then message, 'big not big enough'
utc_big[i:i_last]   = utc   
sza_big[i:i_last]   = sza
for n=0,2 do ssfr_temp_big[i:i_last,n]=ssfr_temp[*,n]
for n=0,np-1 do begin
  for l=0,3 do begin
    spect_big[i:i_last,n,l]     = spect[*,n,l]
    spect_cal_big[i:i_last,n,l] = spect_cal[*,n,l]
  endfor
endfor  
for n=0, n_elements(nadlambda)-1 do nspectra_big[i:i_last,n] = nspectra[*,n]
for n=0, n_elements(zenlambda)-1 do zspectra_big[i:i_last,n] = zspectra[*,n]
i=i_last

print, 'restoring file:'+fdrk[ii]
restore, fdrk[ii]
id_last=n_elements(utc)-1L + id
if id_last ge big-1 then message, 'big not big enough'
utc_bigdrk[id:id_last]   = utc
sza_bigdrk[id:id_last]   = sza
for n=0,2 do ssfr_temp_bigdrk[id:id_last,n]=ssfr_temp[*,n]
for n=0,np-1 do begin
  for l=0,3 do begin
    spect_bigdrk[id:id_last,n,l]     = spect[*,n,l]
    spect_cal_bigdrk[id:id_last,n,l] = spect_cal[*,n,l]
  endfor
endfor
for n=0, n_elements(nadlambda)-1 do nspectra_bigdrk[id:id_last,n] = nspectra[*,n]
for n=0, n_elements(zenlambda)-1 do zspectra_bigdrk[id:id_last,n] = zspectra[*,n]
id=id_last

endfor

;set the variables to their max
utc_big=utc_big[0:i_last-1] & sza_big=sza_big[0:i_last-1]
spect_big=spect_big[0:i_last-1,*,*]
spect_cal_big=spect_cal_big[0:i_last-1,*,*]
ssfr_temp_big=ssfr_temp_big[0:i_last-1,*]
utc_bigdrk=utc_bigdrk[0:id_last-1] & sza_bigdrk=sza_bigdrk[0:id_last-1]
spect_bigdrk=spect_bigdrk[0:id_last-1,*,*]
spect_cal_bigdrk=spect_cal_bigdrk[0:id_last-1,*,*]
ssfr_temp_bigdrk=ssfr_temp_bigdrk[0:id_last-1,*]

nspectra_bigdrk=nspectra_bigdrk[0:id_last-1,*]
zspectra_bigdrk=zspectra_bigdrk[0:id_last-1,*]

save,zspectra_bigdrk,zlambda,utc_bigdrk,filename='/argus/roof/SSFR3/data/'+date+'_drk.out'
stop
; now plot the time trace of these measurements
; at a wavelength near the joinder

set_plot, 'x'
device, decomposed=0
!p.multi=0
!y.style=1 & !x.style=1
loadct, 39

wv=1000.

window, 0, retain=2,title='Radiance Silicon'
nul=min(abs(wvl[*,0]-wv),n0)
nul=min(abs(wvl[*,1]-wv),n1)
nul=min(abs(wvl[*,2]-wv),n2)
nul=min(abs(wvl[*,3]-wv),n3)

plot, utc_bigdrk,spect_bigdrk[*,n0,0],title='Time series of Radiance Silicon at '+string(wv,format='(I3)')+' nm',ytitle='DN',$
 xtitle='UTC (h)',xrange=[min(utc_bigdrk),max(utc_bigdrk)]
oplot,utc_big,spect_big[*,n0,0], color=250
legend,['regular','with darks'],textcolors=[255,250],box=0

window, 1, retain=2,title='Radiance InGaAs'
plot, utc_bigdrk,spect_bigdrk[*,n1,1],title='Time series of Radiance InGaAs at '+string(wv,format='(I3)')+' nm',ytitle='DN',$
 xtitle='UTC (h)',xrange=[min(utc_bigdrk),max(utc_bigdrk)]
oplot,utc_big,spect_big[*,n1,1], color=250
legend,['regular','with darks'],textcolors=[255,250],box=0

window, 2, retain=2,title='Irradiance Silicon'
plot, utc_bigdrk,spect_bigdrk[*,n2,2],title='Time series of Irradiance Silicon at '+string(wv,format='(I3)')+' nm',ytitle='DN',$
 xtitle='UTC (h)',xrange=[min(utc_bigdrk),max(utc_bigdrk)]
oplot,utc_big,spect_big[*,n2,2], color=250
legend,['regular','with darks'],textcolors=[255,250],box=0

window, 3, retain=2,title='Irradiance InGaAs'
plot, utc_bigdrk,spect_bigdrk[*,n3,3],title='Time series of Irradiance InGaAs at '+string(wv,format='(I3)')+' nm',ytitle='DN',$
 xtitle='UTC (h)',xrange=[min(utc_bigdrk),max(utc_bigdrk)]
oplot,utc_big,spect_big[*,n3,3], color=250
legend,['regular','with darks'],textcolors=[255,250],box=0
stop

end
