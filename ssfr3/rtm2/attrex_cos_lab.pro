@legend.pro
pro attrex_cos_lab

!P.multi = 0
loadct, 39
!P.background=255
!P.color=0
device,decomposed=0
dir      =  '/data/seven/schmidt/polar/cos/'
outdir   =  '/data/seven/schmidt/polar/noaa/cal/'

; CHOOSE SENSOR
; ARCTAS : NASA P-3 : LC-3 Zenith  **1**, LC-1 Nadir **2**
; ARCPAC : NOAA P-3 : LC-6 Zenith  **3**, LC-5 Nadir **4**

for sens = 1,4 do begin   ; the number between the ** **

case sens of
1: info     =  [ ['lc312','lc312','lc312/lc312.txt'],$
                 ['lc33' ,'lc33' ,'lc33/lc33.txt'],$
                 ['lc36' ,'lc36' ,'lc36/lc36.txt']]
2: info     =  [ ['lc112','lc112','lc112/lc112.txt'],$
                 ['lc13' ,'lc13' ,'lc13/lc13.txt'],$
                 ['lc16' ,'lc16' ,'lc16/lc16.txt']]
3: info     =  [ ['lc612','lc612','lc612/lc612.txt'],$
                 ['lc63' ,'lc63' ,'lc63/lc63.txt'],$
                 ['lc66' ,'lc66' ,'lc66/lc66.txt']]
4: info     =  [ ['lc512','lc512','lc512/lc512.txt'],$
                 ['lc53' ,'lc53' ,'lc53/lc53.txt'],$
                 ['lc56' ,'lc56' ,'lc56/lc56.txt']]
endcase


verbose  = 1           ; show details (1/0)
iry      = 1           ; plot IR channels
cli      = 40          ; color increment
np       = 256         ; number of data points in a spectrum
use      = [0,1,2]   ; sets of measurements from "dir" that are analyzed (for different azimuth angles)
uw       = 0           ; write results for use[uw]
na =18                 ; number of angles (should be the same in all files)

bn     = 20000L
utc    = fltarr(bn)
zrawsi = fltarr(np,bn)
zrawir = fltarr(np,bn)
nrawsi = fltarr(np,bn)
nrawir = fltarr(np,bn)

nu =n_elements(use)
mu = fltarr(na,nu)          ; mu's
zsi= fltarr(np,na,nu)
zir= fltarr(np,na,nu)
nsi= fltarr(np,na,nu)
nir= fltarr(np,na,nu)

for u=0,nu-1 do begin
; *** insert loop over elements of info here
  pattern = dir+info[0,use[u]]+'/*.OSA'
  file    = file_search(pattern,count=nf,/FOLD_CASE)
  log     = dir+info[2]

  ; get angles
  nl=file_lines(log)
  openr,ul,log,/get_lun
  angles=fltarr(nl)
  for l=0,nl-1 do begin
    readf,ul,a0
    angles[l]=a0
  endfor
  if not eof(ul) then message,'log file not completely read.'
  free_lun,ul
  if nf ne nl+1 and nf ne nl then begin
    print,'Number of angles:',nl
    print,'Number of files: ',nf
    message,'Something wrong.'
  endif
  if nf eq nl   then begin
    dark  = dir+info[1,use[u]]+'/*.OSA'
    print,info[0,use[u]]+': Use darks from '+info[1,use[u]]+'.'
  endif
  if nf eq nl+1 then begin
    nf=nf-1
    dark  = file[nf]
    print,info[0,use[u]]+': Use darks from main folder.'
  endif

  ind0   = fltarr(nf)
  ind1   = fltarr(nf)
  utcavg = fltarr(nf)
  zavgsi = fltarr(np,nf)
  zstdsi = fltarr(np,nf)
  navgsi = fltarr(np,nf)
  nstdsi = fltarr(np,nf)
  zavgir = fltarr(np,nf)
  zstdir = fltarr(np,nf)
  navgir = fltarr(np,nf)
  nstdir = fltarr(np,nf)
  ctr     = 0L
  for ind = 0L, nf - 1 do begin ; loop over OSA files
    openr, lun, file[ind], /get_lun
    if verbose then print,'Open '+file[ind]+'.'
    ind0[ind]=ctr ; starting index of this angle
    while not eof(lun) do begin

      spect = {btime: lonarr(2), bcdtimstp:  bytarr(12), $
               intime1: long(0), $
               intime2: long(0), intime3: long(0), intime4: long(0), $
               accum: long(0), $
               ztemp: ulong(0), ntemp: ulong(0), itemp: ulong(0), $
               zspecsi: intarr(np), zspecir: intarr(np), $
               nspecsi: intarr(np), nspecir: intarr(np)}
      readu, lun, spect

      atime = systime(0, spect.btime(0), /utc)   &    result = strpos(atime(0), ':', 0)
      day =  strmid(atime(0), result - 5, 2) & mon =  strmid(atime(0), result - 9, 3) &  hr = strmid(atime(0), result - 2, 2)          ;find hour
      min = strmid(atime(0), result + 1, 2)  & sec = strmid(atime(0), result + 4, 2)
      year = fix(strmid(atime(0), result + 7, 4))  &  yr2dig = strmid(atime(0), result + 9, 2)  ;find year - last two digits
      adate = day + mon + yr2dig & fdate = string(day) + ' ' + string(mon) + string(year)

      utc[ctr]       = double(hr + (min/60.) + (sec/3600.))   ;put hr in decimal form

      zrawsi[*, ctr] = spect.zspecsi
      zrawir[*, ctr] = spect.zspecir
      nrawsi[*, ctr] = spect.nspecsi
      nrawir[*, ctr] = spect.nspecir
      ctr            = ctr+1L

    endwhile                    ; end WHILE loop
    close,lun
    free_lun, lun

    ind1[ind]=ctr-1    ; stop index of this angle

  endfor                           ;end of primary FOR loop (loop over OSA files)

  for f=0,nf-1 do begin
    utcavg[f]  =mean  (utc[ind0[f]:ind1[f]])
    for p=0,np-1 do begin
      zavgsi[p,f]=mean  (zrawsi[p,ind0[f]:ind1[f]])
      zstdsi[p,f]=stddev(zrawsi[p,ind0[f]:ind1[f]])
      navgsi[p,f]=mean  (nrawsi[p,ind0[f]:ind1[f]])
      nstdsi[p,f]=stddev(nrawsi[p,ind0[f]:ind1[f]])
      zavgir[p,f]=mean  (zrawir[p,ind0[f]:ind1[f]])
      zstdir[p,f]=stddev(zrawir[p,ind0[f]:ind1[f]])
      navgir[p,f]=mean  (nrawir[p,ind0[f]:ind1[f]])
      nstdir[p,f]=stddev(nrawir[p,ind0[f]:ind1[f]])
    endfor
  endfor

  ; now define darks
  pattern = dark
  file    = file_search(pattern,count=nd,/FOLD_CASE)

  zdrsi = fltarr(np,bn)
  zdrir = fltarr(np,bn)
  ndrsi = fltarr(np,bn)
  ndrir = fltarr(np,bn)
  ctr     = 0L
  for ind = 0L, nd - 1 do begin ; loop over OSA files
    openr, ld, file[ind], /get_lun
    if verbose then print,'Open '+file[ind]+'.'
    while not eof(ld) do begin
      spect = {btime: lonarr(2), bcdtimstp:  bytarr(12), $
               intime1: long(0), $
               intime2: long(0), intime3: long(0), intime4: long(0), $
               accum: long(0), $
               ztemp: ulong(0), ntemp: ulong(0), itemp: ulong(0), $
               zspecsi: intarr(np), zspecir: intarr(np), $
               nspecsi: intarr(np), nspecir: intarr(np)}
      readu, ld, spect

      atime = systime(0, spect.btime(0), /utc)   &    result = strpos(atime(0), ':', 0)
      day =  strmid(atime(0), result - 5, 2) & mon =  strmid(atime(0), result - 9, 3) &  hr = strmid(atime(0), result - 2, 2)          ;find hour
      min = strmid(atime(0), result + 1, 2)  & sec = strmid(atime(0), result + 4, 2)
      year = fix(strmid(atime(0), result + 7, 4))  &  yr2dig = strmid(atime(0), result + 9, 2)  ;find year - last two digits
      adate = day + mon + yr2dig & fdate = string(day) + ' ' + string(mon) + string(year)

      zdrsi[*, ctr] = spect.zspecsi
      zdrir[*, ctr] = spect.zspecir
      ndrsi[*, ctr] = spect.nspecsi
      ndrir[*, ctr] = spect.nspecir
      ctr            = ctr+1L
    endwhile                    ; end WHILE loop
    close,ld
    free_lun, ld
  endfor                           ;end of primary FOR loop (loop over OSA files)
  ctr=ctr-1
  zdrsi=zdrsi[*,0:ctr]
  zdrir=zdrir[*,0:ctr]
  ndrsi=ndrsi[*,0:ctr]
  ndrir=ndrir[*,0:ctr]

  zdrksi=fltarr(np,2) ; 255 channels; 0: mean, 1: stddev
  zdrkir=fltarr(np,2)
  ndrksi=fltarr(np,2)
  ndrkir=fltarr(np,2)

  for p=0,np-1 do begin
    zdrksi[p,0]=mean  (zdrsi[p,*])
    zdrksi[p,1]=stddev(zdrsi[p,*])
    zdrkir[p,0]=mean  (zdrir[p,*])
    zdrkir[p,1]=stddev(zdrir[p,*])
    ndrksi[p,0]=mean  (ndrsi[p,*])
    ndrksi[p,1]=stddev(ndrsi[p,*])
    ndrkir[p,0]=mean  (ndrir[p,*])
    ndrkir[p,1]=stddev(ndrir[p,*])
  endfor
  ; end of dark calculation

  ; calculate the actual signal
  for p=0,np-1 do begin
    zsi[p,*,u]=zavgsi[p,*]-zdrksi[p,0]
    zir[p,*,u]=zavgir[p,*]-zdrkir[p,0]
    nsi[p,*,u]=navgsi[p,*]-ndrksi[p,0]
    nir[p,*,u]=navgir[p,*]-ndrkir[p,0]
  endfor

  mu[*,u]=double(cos(!pi/180d*double(angles)))

  if verbose then begin
    window,0,retain=2,tit='Time series channel 100 '+info[0,use[u]],xs=800,ys=500
    !P.multi=[0,2,1]

    plot,utc[ind0[0]:ind1[0]],zrawsi[100,ind0[0]:ind1[0]],xr=[min(utc[ind0]),max(utc[ind1])],xtit='UTC [h]',ytit='channel 100 counts',yr=[min(zrawsi),max(zrawsi)],tit='ZSI'
    ;plots,[mean(utcavg),zdrkir[100,0]]
    oplot,[mean(utcavg)*0.9,mean(utcavg)*1.1],[zdrksi[100,0]-zdrksi[100,1],zdrksi[100,0]-zdrksi[100,1]]
    oplot,[mean(utcavg)*0.9,mean(utcavg)*1.1],[zdrksi[100,0]+zdrksi[100,1],zdrksi[100,0]+zdrksi[100,1]]
    if nf gt 1 then begin
      for f=1,nf-1 do begin
        oplot,utc[ind0[f]:ind1[f]],zrawsi[100,ind0[f]:ind1[f]]
      endfor
    endif
    oploterr,utcavg,zavgsi[100,*],zstdsi[100,*]
    oplot,utcavg,zsi[100,*,u],color=60,psym=2

    plot,utc[ind0[0]:ind1[0]],zrawir[100,ind0[0]:ind1[0]],xr=[min(utc[ind0]),max(utc[ind1])],xtit='UTC [h]',ytit='channel 100 counts',yr=[min(zrawir),max(zrawir)],tit='ZIR'
    ;plots,[mean(utcavg),zdrkir[100,0]]
    oplot,[mean(utcavg)*0.9,mean(utcavg)*1.1],[zdrkir[100,0]-zdrkir[100,1],zdrkir[100,0]-zdrkir[100,1]]
    oplot,[mean(utcavg)*0.9,mean(utcavg)*1.1],[zdrkir[100,0]+zdrkir[100,1],zdrkir[100,0]+zdrkir[100,1]]
    if nf gt 1 then begin
      for f=1,nf-1 do begin
        oplot,utc[ind0[f]:ind1[f]],zrawir[100,ind0[f]:ind1[f]]
      endfor
    endif
    oploterr,utcavg,zavgir[100,*],zstdir[100,*]
    oplot,utcavg,zir[100,*,u],color=60,psym=2

    if 0 then begin ; if nadir used instead
    plot,utc[ind0[0]:ind1[0]],nrawsi[100,ind0[0]:ind1[0]],xr=[min(utc[ind0]),max(utc[ind1])],xtit='UTC [h]',ytit='channel 100 counts',yr=[min(nrawsi),max(nrawsi)],tit='NSI'
    ;plots,[mean(utcavg),zdrkir[100,0]]
    oplot,[mean(utcavg)*0.9,mean(utcavg)*1.1],[ndrksi[100,0]-ndrksi[100,1],ndrksi[100,0]-ndrksi[100,1]]
    oplot,[mean(utcavg)*0.9,mean(utcavg)*1.1],[ndrksi[100,0]+ndrksi[100,1],ndrksi[100,0]+ndrksi[100,1]]
    if nf gt 1 then begin
      for f=1,nf-1 do begin
        oplot,utc[ind0[f]:ind1[f]],nrawsi[100,ind0[f]:ind1[f]]
      endfor
    endif
    oploterr,utcavg,navgsi[100,*],nstdsi[100,*]
    oplot,utcavg,nsi[100,*,u],color=60,psym=2

    plot,utc[ind0[0]:ind1[0]],nrawir[100,ind0[0]:ind1[0]],xr=[min(utc[ind0]),max(utc[ind1])],xtit='UTC [h]',ytit='channel 100 counts',yr=[min(nrawir),max(nrawir)],tit='NIR'
    ;plots,[mean(utcavg),zdrkir[100,0]]
    oplot,[mean(utcavg)*0.9,mean(utcavg)*1.1],[ndrkir[100,0]-ndrkir[100,1],ndrkir[100,0]-ndrkir[100,1]]
    oplot,[mean(utcavg)*0.9,mean(utcavg)*1.1],[ndrkir[100,0]+ndrkir[100,1],ndrkir[100,0]+ndrkir[100,1]]
    if nf gt 1 then begin
      for f=1,nf-1 do begin
        oplot,utc[ind0[f]:ind1[f]],nrawir[100,ind0[f]:ind1[f]]
      endfor
    endif
    oploterr,utcavg,navgir[100,*],nstdir[100,*]
    oplot,utcavg,nir[100,*,u],color=60,psym=2
    endif
;    if u ne nu-1 then begin
;      q=''
;      read,q
;    endif
    !P.multi=0
  endif

endfor ; u-loop

if verbose then begin
window,1,retain=2,tit='COSINE RESPONSE, ch.100/60'
clr=intarr(nu+2)
plot ,mu[*,0],zsi[100,*,0]/zsi[100,0,0],psym=1,xtit='MU',ytit='relative response'
if iry then oplot,mu[*,0],zir[60 ,*,0]/zir[60 ,0,0],psym=2
oplot,[0,1],[0,1],linesty=1
clr[0:2]=255
if nu gt 1 then begin
  for u=1,nu-1 do begin
    oplot,mu[*,u],zsi[100,*,u]/zsi[100,0,u],psym=1,color=cli*u
    if iry then oplot,mu[*,u],zir[60 ,*,u]/zir[60 ,0,u],psym=2,color=cli*u
    clr[u+2]=cli*u
  endfor
endif
legend,['+ SI100','* IR60',reform(info[0,use])],textcolors=clr
;p=tvrd(true=1)
;write_png,outdir+'cos_lab.png',p
endif


; wavelength coefficients for SSFR 3
cvisz = [3.048678591e2,3.287663420,1.975037077e-4,-1.592218480e-6]
cnirz = [9.424712905e2,6.245178015,-9.179161662e-4,-4.206810248e-6]
cvisn = [3.056365589e2,3.281352579,2.301335982e-4,-1.518225206e-6]
cnirn = [9.383239708e2,6.330279881,-2.042871831e-3,1.711020638e-7]

; get wavelengths
wlvisz=fltarr(np)
wlvisn=fltarr(np)
wlnirz=fltarr(np)
wlnirn=fltarr(np)
for j = 0, np - 1 do begin
  jj=float(j)
  wlvisz[j] = cvisz(0)+jj*(cvisz(1)+jj*(cvisz(2)+jj*cvisz(3)))
  wlvisn[j] = cvisn(0)+jj*(cvisn(1)+jj*(cvisn(2)+jj*cvisn(3)))
  wlnirz[j] = cnirz(0)+jj*(cnirz(1)+jj*(cnirz(2)+jj*cnirz(3)))
  wlnirn[j] = cnirn(0)+jj*(cnirn(1)+jj*(cnirn(2)+jj*cnirn(3)))
endfor


;print,'Writing cosine response at: ',wlvisz[100],wlnirz[60],' for ',info[0,use[uw]]
;;print,strcompress(string(fix(wlvisz[100])),/REMOVE_ALL)
;openw,usi,outdir+info[0,use[uw]]+'_cos_lab_'+strcompress(string(fix(wlvisz[100]+0.5)),/REMOVE_ALL)+'.dat',/get_lun
;openw,uir,outdir+info[0,use[uw]]+'_cos_lab_'+strcompress(string(fix(wlnirz[100]+0.5)),/REMOVE_ALL)+'.dat',/get_lun
;for i=0,na-1 do begin
;  printf,usi,mu[i,uw],zsi[100,i,uw]/zsi[100,0,uw]
;  printf,uir,mu[i,uw],zir[100,i,uw]/zir[100,0,uw]
;endfor
;free_lun,usi
;free_lun,uir


wl = [wlvisz,wlnirz]
if verbose then begin
  window,2,retain=2,tit='SPECTRAL COSINE RESPONSE'
  plot,[min(wl),2410],[-0.1,1.5],title='Spectral cosine response',ticklen=1.0,xsty=1,ysty=1,/nodata
  for wink=0,na-1 do begin
   zzz = [ zsi[*,wink,uw]/zsi[*,0,uw],  zir[*,wink,uw]/zir[*,0,uw] ]
   oplot,wl,zzz,color=255*float(wink)/float(na)
   xyouts,1600,1.5-0.05*wink,strtrim(string(round(angles[wink])),2)+'°',color=255*float(wink)/float(na)
  endfor
p=tvrd(true=1)
write_png,outdir+info[0,use[uw]]+'_cos_lab_spectral.png',p
endif

ind1 = where(wlvisz le 950.0)
ind2 = where(wlnirz gt 950.0)

ind3 = (where(wlvisz ge 320.0 and wlvisz lt 325.0))[0]  ; should be 321 nm or whereabouts
uv = where(wlvisz lt 320.0)

ind5 = where(wlvisz lt 500.0)

wlnirz2 = wlnirz[ind2]
ind14 = (where(wlnirz2 gt 1399.0 and wlnirz2 lt 1420.0))[0]   ; should be pretty close to 1400
ind16 = (where(wlnirz2 gt 1599.0 and wlnirz2 lt 1620.0))[0]   ; should be pretty close to 1600
ind21 = where(wlnirz2 gt 1600.0,ni2)

; index J = Joint

wlJ = [ wlvisz[ind1] , wlnirz[ind2] ]
nj = n_elements(wlJ)

cosFAC0 = fltarr(na,nj)

if verbose then begin
  window,2,retain=2,tit='SPECTRAL COSINE RESPONSE'
  plot,[min(wl),2410],[-0.1,1.5],title='Spectral cosine response',ticklen=1.0,xsty=1,ysty=1,/nodata
  for wink=0,na-1 do begin
   ; SMOOTH the lowest wavelengths
   zzz0 = zsi[ind1,wink,uw]/zsi[ind1,0,uw]
    zzz1 = smooth(zzz0,10,/EDGE)
    ;zzz1[uv] = zzz1[ind3]
   ; EXTRAPOLATE the NIR range from 1700 to 2100/2200
   zzz2 = zir[ind2,wink,uw]/zir[ind2,0,uw]
    NIRgradient = ( zzz2[ind16] - zzz2[ind14] ) / ( wlnirz2[ind16] - wlnirz2[ind14] )
   for ll = 0,ni2-1 do zzz2[ind21[ll]] = zzz2[ind16] + NIRgradient * (wlnirz2[ind21[ll]] - wlnirz2[ind16] )

   ; Merge VIS and NIR, and PLOT!
   zzz = [ zzz1 ,  zzz2 ]
   cosFAC0[wink,*] = zzz
   oplot,wlJ,zzz,color=255*float(wink)/float(na)
   xyouts,1600,1.5-0.05*wink,strtrim(string(round(angles[wink])),2)+'°',color=255*float(wink)/float(na)
  endfor
p=tvrd(true=1)
write_png,outdir+info[0,use[uw]]+'_cos_lab_spectral_v2.png',p
endif


print,'Writing cosine response at all wavelengths for ',info[0,use[uw]]
openw,usi,outdir+info[0,use[uw]]+'_cos_lab_SPECTRAL.dat',/get_lun
;openw,uir,outdir+info[0,use[uw]]+'_cos_lab_NIR_SPECTRAL.dat',/get_lun
printf,usi,format='("#        mu",'+strtrim(string(nj),2)+'F11.5)',wlJ
for i=0,na-1 do begin
;  printf,usi,format='(F11.7,'+strtrim(string(np),2)+'F11.7)',mu[i,uw],zsi[*,i,uw]/zsi[*,0,uw]
;  printf,uir,format='(F11.7,'+strtrim(string(np),2)+'F11.7)',mu[i,uw],zir[*,i,uw]/zir[*,0,uw]
   printf,usi,format='(F11.7,'+strtrim(string(nj),2)+'F11.7)',mu[i,uw],cosFAC0[i,*]
endfor
free_lun,usi
;free_lun,uir


thousand = dindgen(1001)/1000d  ; the index is interpreted as 1000*mu = 1000*cos(angle).
                         ; E.g. index 42 is that of angle 87.59°: cos(87.59°)=0.042

zIPsi = fltarr(nj,1001,nu)
zIPir = fltarr(nj,1001,nu)

for i=0,np-1 do begin
  zIPsi[i,*,uw] = interpol(zsi[i,*,uw]/zsi[i,0,uw],mu[*,uw],thousand)
  zIPir[i,*,uw] = interpol(zir[i,*,uw]/zir[i,0,uw],mu[*,uw],thousand)
endfor ;i

; Merge VIS and NIR at 950 nm

wearevis = where(wlvisz le 950.0)
wearenir = where(wlnirz gt 950.0)

cosWL  = wlJ

cosFAC = fltarr(nj,1001)

;for i=0,1000 do cosFAC[*,i] = [reform(zIPsi[wearevis,i,uw]),reform(zIPir[wearenir,i,uw])]
for i=0,nj-1 do cosFAC[i,*] = interpol(cosFAC0[*,i],mu[*,uw],thousand)

; Binary output
save,cosWL,cosFAC,file=outdir+info[0,use[uw]]+'_cos_lab_allWL_1000angles.out'

endfor  ; sens = 1..4 for the 4 optical inlets

end
