; Program to build the all wavelength cosine correction files

@interpol_ang_cal.pro
pro cos_lab

lc='LCX'
dirout='/data/seven/schmidt/calnex/ang/'

; set the files to restore
case lc of
  'LC1': begin
    dir=''
    files=['']
    harp=0
  end
  'LC2': begin
    dir='/data/seven/schmidt/calnex/ang/'
    files=['20100301/res_LC-2_plate_6pm.out',$
           '20100301/res_LC-2_plate_12pm.out',$
           '20100301/res_LC-2_plate_3pm.out']
    harp=1
    date='20100301'
    na=13
    lo=0.45 & up=0.7 & mix=0.6
  end
  'LC3': begin
    dir=''
    files=['']
    harp=0
  end
  'LC5': begin
    dir='/data/seven/schmidt/calnex/ang/'
    files=['20100302/res_LC-5_plate_6pm.out',$
           '20100302/res_LC-5_plate_12pm.out',$
           '20100302/res_LC-5_plate_3pm.out']
    harp=1
    date='20100302'
    na=13
    lo=0.65 & up=0.85 & mix=0.7
  end
  'LC6': begin
    dir='/data/seven/schmidt/calnex/ang/'
    files=['20100302/res_LC-6_plate_6pm.out',$
           '20100302/res_LC-6_plate_12pm.out',$
           '20100302/res_LC-6_plate_3pm.out']
    harp=1
    date='20100302'
    na=13
    lo=0.65 & up=0.85 & mix=0.65
  end
  'LC7': begin
    dir='/data/seven/schmidt/calnex/ang/'
    files=['20100301/res_LC-7_plate_6pm.out',$
           '20100301/res_LC-7_plate_12pm.out',$
           '20100301/res_LC-7_plate_3pm.out']
    harp=1
    date='20100301'
    na=12
    lo=0.55 & up=0.8 & mix=0.58
  end
  'LCX': begin
    dir='/data/seven/schmidt/calnex/ang/'
    files=['/20100723/CALNEX03_cos.out',$
           '/20100723/CALNEX06_cos.out',$
	   '/20100723/CALNEX12_cos.out']
    harp=0
    date='20100723'
    na=19
    lo=0.65 & up=0.8 & mix=0.7
    joinder=940.
  end
  else: message, 'Light collector not found'
endcase

if harp then begin
  nf       = n_elements(files)
  ;na       = 12 ; number of angles
  au       = 1 ; angular uncertainty
  ui       = 2 ; irradiance uncertainty [%]
  nwvl     = 1024 ; number of bands
  
  wvl=findgen(nwvl)
  c_wvl=[260.862,0.829021,4.69461E-8,-1.76001E-8]
  wvl=c_wvl[0]+c_wvl[1]*wvl+c_wvl[2]*wvl*wvl+c_wvl[3]*wvl*wvl*wvl
  r=fltarr(nf,na,nwvl)
  a=fltarr(nf,na)
  
  ; loop through all the files for one light collector
  for i=0,nf-1 do begin
    restore,dir+files[i]
    nal=n_elements(ang)
    for j=0,nal-1 do begin
      r[i,j,*]=res[j,*]/res[0,*]
      a[i,j]  =ang[j]
      nul=where(r[i,j,*] eq 0.0, n_mis) ; filter missing data
      if n_mis gt 0 then r[i,j,nul]=!values.f_nan
    endfor
    mu=cos(!pi/180.*a[i,*])
  endfor
  resst=res
  ; get the average cosine response of all the files for one light collector
  for i=0, nal-1 do for ii=0,nwvl-1 do res[i,ii]=mean(r[*,i,ii],/nan)
  for i=0, nal-1 do for ii=0,nwvl-1 do resst[i,ii]=stddev(r[*,i,ii],/nan)
  
  ; set up the variables for the interpolated wavelength dependant response function
  n_int=1000  ; amount of mu values to interpolate the response function to 
  res_wvl=fltarr(n_int+1,nwvl)
  
  ; now loop through all the wavelengths
  for i=0, nwvl-1 do begin
    interpol_ang_cal, mu, res[*,i], mu_interpol, res_interpol,n_int,lo,up,mix
    res_wvl[*,i]=res_interpol    
  endfor  
endif else begin  ; end of harp portion, need to built SSFR portion
  nf       = n_elements(files)
  au       = 1 ; angular uncertainty
  ui       = 2 ; irradiance uncertainty [%]
  nwvl     = 256 ; number of bands

  ; wavelength coefficients for SSFR 5 - use zenith
  cvisz = [301.755,3.32635,4.44514E-04,-1.93826E-6]
  cnirz = [2221.04,-4.35923,-0.00278573,4.47858e-6,-2.48982e-8]
  cvisn = [3.043740643e2,3.304442502,9.061725871e-5,-1.310768888e-6]
  cnirn = [2226.93,-4.48862,-0.000509907,-7.40257e-6,-4.28565e-9]
  
  ; get wavelengths
  wlvisz=fltarr(nwvl)
  wlvisn=fltarr(nwvl)
  wlnirz=fltarr(nwvl)
  wlnirn=fltarr(nwvl)
  for j = 0, nwvl - 1 do begin
    jj=float(j)
    wlvisz[j] = cvisz(0)+jj*(cvisz(1)+jj*(cvisz(2)+jj*cvisz(3)))
    wlvisn[j] = cvisn(0)+jj*(cvisn(1)+jj*(cvisn(2)+jj*cvisn(3)))
    wlnirz[j] = cnirz(0)+jj*(cnirz(1)+jj*(cnirz(2)+jj*(cnirz(3)+jj*cnirz(4))))
    wlnirn[j] = cnirn(0)+jj*(cnirn(1)+jj*(cnirn(2)+jj*(cnirn(3)+jj*cnirn(4))))
  endfor

  wlnirz=reverse(wlnirz)

  rvis=fltarr(nf,na,nwvl)
  rnir=fltarr(nf,na,nwvl)
  a=fltarr(nf,na)
  ; loop through all the files for one light collector
  for i=0,nf-1 do begin
    restore,dir+files[i]
    nal=n_elements(angles)
    for j=0,nal-1 do begin
      rvis[i,j,*]=SI_response[*,j]/SI_response[*,0]
      rnir[i,j,*]=IR_response[*,j]/IR_response[*,0]
      a[i,j]  =angles[j]
    endfor
    mu=cos(!pi/180.*a[i,*])
  endfor
  resstvis=fltarr(nal,nwvl) & resvis=fltarr(nal,nwvl)
  resstnir=fltarr(nal,nwvl) & resnir=fltarr(nal,nwvl)
  ; get the average cosine response of all the files for one light collector
  for i=0, nal-1 do begin
    for ii=0,nwvl-1 do begin 
      resvis[i,ii]=mean(rvis[*,i,ii],/nan)
      resstvis[i,ii]=stddev(rvis[*,i,ii],/nan)
      resnir[i,ii]=mean(rnir[*,i,ii],/nan)
      resstnir[i,ii]=stddev(rnir[*,i,ii],/nan)
    endfor
    resnir[i,*]=reverse(resnir[i,*],2)
    resstnir[i,*]=reverse(resstnir[i,*],2)
  endfor

  ; set up the variables for the interpolated wavelength dependant response function
  n_int=1000  ; amount of mu values to interpolate the response function to 
  res_wvlvis=fltarr(n_int+1,nwvl)
  res_wvlnir=fltarr(n_int+1,nwvl)

  ; now loop through all the wavelengths
  for i=0, nwvl-1 do begin
    interpol_ang_cal, mu, resvis[*,i], mu_interpol, res_interpol,n_int,lo,up,mix
    res_wvlvis[*,i]=res_interpol
    interpol_ang_cal, mu, resnir[*,i], mu_interpol, res_interpol,n_int,lo,up,mix
    res_wvlnir[*,i]=res_interpol
  endfor
endelse

;smooth and filter the response
if harp then begin
for i=0, n_int-1 do res_wvl[i,*]=smooth(res_wvl[i,*],5,/nan) ;smooth in wavelenght domain
fl=where(wvl gt 380. and wvl le 900)
res_wvl=res_wvl[*,fl]
wvl=wvl[fl]
endif  

nul=min(abs(wlvisz-joinder),iv)
nul=min(abs(wlnirz-joinder),ir)
res_wvl=[[res_wvlvis[*,0:iv]],[res_wvlnir[*,ir:*]]]
res=[[resvis[*,0:iv]],[resnir[*,ir:*]]]
resst=[[resstvis[*,0:iv]],[resstnir[*,ir:*]]]
wvl=[wlvisz[0:iv],wlnirz[ir:*]]
fl=where(wvl gt 0.)
save, res_wvl, wvl, mu_interpol, filename=dirout+date+'_'+lc+'.out' ;save the cosine response function
stop
set_plot, 'x'
!p.thick=2
window, 0, xsize=800, ysize=800
loadct, 39,/silent
device,decomposed=0
!p.multi=[0,1,2]
!y.style=1
if harp then wvr=[0,100,200,400,500,600] else wvr=[0,50,100,200,300,400]
corr=fltarr(n_elements(wvr))
imu=intarr(n_elements(mu))
for i=0,n_elements(mu)-1 do begin 
  nul=min(abs(mu[i]-mu_interpol),im)
  imu[i]=im
  for j=0, n_elements(wvr)-1 do corr[j]=correlate(res[*,fl[wvr[j]]],res_wvl[imu,wvr[j]])
endfor
for i=0, n_int-1 do begin

plot, wvl, res_wvl[i,*],title='mu at:'+ string(mu_interpol[i]), yrange=[0,1.3],xtitle='wavelength (nm)',ytitle='radiance'
;oplot, wvl, smooth(res_wvl[i,*],10),color=250,thick=2
if not harp then begin
  oplot, wlvisz,res_wvlvis[i,*],color=250, thick=1
  oplot, wlnirz,res_wvlnir[i,*],color=70,thick=1
endif
plot, mu_interpol, res_wvl[*,wvr[0]], title='mu vs. response', yrange=[0,1.3],xtitle='mu',ytitle='radiance' 
oplot, mu_interpol, res_wvl[*,wvr[1]], color=70
oplot, mu_interpol, res_wvl[*,wvr[2]], color=130
oplot, mu_interpol, res_wvl[*,wvr[3]], color=170
oplot, mu_interpol, res_wvl[*,wvr[4]], color=200
oplot, mu_interpol, res_wvl[*,wvr[5]], color=250

oplot, mu, res[*,fl[wvr[0]]],psym=4
oplot, mu, res[*,fl[wvr[1]]],psym=4,color=70
oplot, mu, res[*,fl[wvr[2]]],psym=4,color=130
oplot, mu, res[*,fl[wvr[3]]],psym=4,color=170
oplot, mu, res[*,fl[wvr[4]]],psym=4,color=200
oplot, mu, res[*,fl[wvr[5]]],psym=4,color=250

errplot, mu, res[*,fl[wvr[0]]]-resst[*,fl[wvr[0]]],res[*,fl[wvr[0]]]+resst[*,fl[wvr[0]]]
errplot, mu, res[*,fl[wvr[1]]]-resst[*,fl[wvr[1]]],res[*,fl[wvr[1]]]+resst[*,fl[wvr[1]]],color=70
errplot, mu, res[*,fl[wvr[2]]]-resst[*,fl[wvr[2]]],res[*,fl[wvr[2]]]+resst[*,fl[wvr[2]]],color=130
errplot, mu, res[*,fl[wvr[3]]]-resst[*,fl[wvr[3]]],res[*,fl[wvr[3]]]+resst[*,fl[wvr[3]]],color=170
errplot, mu, res[*,fl[wvr[4]]]-resst[*,fl[wvr[4]]],res[*,fl[wvr[4]]]+resst[*,fl[wvr[4]]],color=200
errplot, mu, res[*,fl[wvr[5]]]-resst[*,fl[wvr[5]]],res[*,fl[wvr[5]]]+resst[*,fl[wvr[5]]],color=250

oplot, [mu_interpol[i],mu_interpol[i]], [0,1.3],linestyle=2
legend,[string(wvl[wvr])+string(corr)],textcolors=[255,70,130,170,200,250],box=0
wait,0.02

endfor
end
