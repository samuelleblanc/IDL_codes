@legend.pro
pro calnex_cos_lab

!P.multi = 0
loadct, 27
device,decomposed=0

dir      =  '/data/seven/schmidt/calnex/ang/20100723/'
outdir   =  '/data/seven/schmidt/calnex/ang/20100723/'
; CALNEX : 
; P-3 : LC-2 = Zenith, LC-5 = Nadir (until Apr-25), LC-X = Nadir (from Apr-25)
; ship: LC-7

info     =  [ ['CALNEX03','039','CALNEX03/ang.txt'],$
              ['CALNEX06','019','CALNEX06/ang.txt'],$
              ['CALNEX12','059','CALNEX12/ang.txt']]
system   = 'SSFR-5'

verbose  = 1           ; show details (1/0)
iry      = 1           ; plot IR channels
cli      = 40          ; color increment
np       = 256         ; number of data points in a spectrum
use      = [0,1,2]     ; sets of measurements from "info" that are analyzed
uw       = 0           ; write results for use[uw]
na       = 19          ; number of angles
do_nadir = 0           ; process nadir and/or
do_zenith= 1           ; zenith

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

si_lamda=0 ; define wavelengths
ir_lamda=0 ; define wavlenegths

for u=0,nu-1 do begin
  log     = dir+info[2,use[u]]

  ; get angles
  openr,ul,log,/get_lun
  str=''
  i=0
  while not eof(ul) do begin
    readf,ul,a0,str
    if i eq 0 then begin
      angles=a0
      filep =str
    endif else begin
      angles=[angles,a0]
      filep =[filep,str]
    endelse
    i+=1
  endwhile
  free_lun,ul
  if n_elements(filep) ne na or n_elements(angles) ne na then message,'Something wrong with number of angles.'
  
  file=strarr(na)
  filep=strcompress(filep,/REMOVE_ALL)
  for i=0,na-1 do begin
    file[i]= file_search(dir+info[0,use[u]]+'/*'+filep[i]+'.OSA2',count=nf)
    if nf gt 1 then message,'Problem 1'
  endfor
  dark  = file_search(dir+info[0,use[u]]+'/*'+info[1,use[u]]+'.OSA2',count=nd)
  if nd gt 1 then message,'Problem 2'

  nf=na

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

      spect = {btime: lonarr(2), bcdtimstp:  bytarr(12),intime1: long(0),$
               intime2: long(0), intime3: long(0), intime4: long(0),accum: long(0), shsw:long(0),$
               zsit: ulong(0),nsit: ulong(0),zirt: ulong(0),nirt: ulong(0), zirx: ulong(0), nirx: ulong(0),xt: ulong(0),it: ulong(0),$
               zspecsi: intarr(np), zspecir: intarr(np), nspecsi: intarr(np), nspecir: intarr(np)}

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

  ; now define darks (multiple dark files can also be read)
  nd      = 1    

  zdrsi = fltarr(np,bn)
  zdrir = fltarr(np,bn)
  ndrsi = fltarr(np,bn)
  ndrir = fltarr(np,bn)
  ctr     = 0L
  for ind = 0L, nd - 1 do begin ; loop over OSA files
    openr, ld, dark, /get_lun
    if verbose then print,'Open '+file[ind]+'.'
    while not eof(ld) do begin
      spect = {btime: lonarr(2), bcdtimstp:  bytarr(12),intime1: long(0),$
               intime2: long(0), intime3: long(0), intime4: long(0),accum: long(0), shsw:long(0),$
               zsit: ulong(0),nsit: ulong(0),zirt: ulong(0),nirt: ulong(0), zirx: ulong(0), nirx: ulong(0),xt: ulong(0),it: ulong(0),$
               zspecsi: intarr(np), zspecir: intarr(np), nspecsi: intarr(np), nspecir: intarr(np)}
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

  mu[*,u]=cos(!pi/180.*angles)

  if verbose then begin
    window,u,retain=2,tit='Time series channel 100 '+info[0,use[u]],xs=800,ys=500
    !P.multi=[0,2,1]

    if do_zenith eq 1 then begin
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
    ;if u ne nu-1 then begin
    ;  q=''
    ;  read,q
    ;endif
    !P.multi=0
    si_response=fltarr(np,na)
    ir_response=fltarr(np,na)
    for i=0,np-1 do begin
      si_response[i,*]=reform(zsi[i,*,u]);/zsi[i,0,0]
      ir_response[i,*]=reform(zir[i,*,u]);/zir[i,0,0]
    endfor
    print,'Saving zenith response to:'+outdir+info[0,use[u]]+'_cos.out'
    save,file=outdir+info[0,use[u]]+'_cos.out',angles,si_lamda,ir_lamda,si_response,ir_response
    print,si_response[100,*]
    endif


    if do_nadir eq 1 then begin
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
    ;if u ne nu-1 then begin
    ;  q=''
    ;  read,q
    ;endif
    !P.multi=0
    si_response=fltarr(np,na)
    ir_response=fltarr(np,na)
    for i=0,np-1 do begin
      si_response[i,*]=nsi[i,*,0];/nsi[i,0,0]
      ir_response[i,*]=nir[i,*,0];/nir[i,0,0]
    endfor
    print,'Saving nadir response to:'+outdir+info[0,use[u]]+'_cos.out'
    save,file=outdir+info[0,use[u]]+'_cos.out',angles,si_lamda,ir_lamda,si_response,ir_response
    endif
  endif
endfor ; u-loop
;;;;;; END OF READER - THIS IS WHERE CODE FROM PETRA SHOULD BE INSERTED FOR gluing the responses together. ;;;;;;;
message,'Code to be extended.'





window,1,retain=2,tit='COSINE RESPONSE'
clr=intarr(nu+2)
plot ,mu[*,0],zsi[100,*,0]/zsi[100,0,0],psym=1,xtit='MU',ytit='relative response'
if iry then oplot,mu[*,0],zir[60 ,*,0]/zir[60 ,0,0],psym=2
oplot,[0,1],[0,1],linesty=1
clr[0:2]=255
if nu gt 1 then begin
  for u=1,nu-1 do begin
    oplot,mu[*,u],zsi[100,*,u]/zsi[100,0,u],psym=1,color=cli*u
    if iry then oplot,mu[*,u],zir[100 ,*,u]/zir[100 ,0,u],psym=2,color=cli*u
    clr[u+2]=cli*u
  endfor
endif
legend,['+ SI100','* IR100',reform(info[0,use])],textcolors=clr
;p=tvrd(true=1)
;write_png,outdir+'cos_lab.png',p


; wavelength coefficients for SSFR 3 ; need to get for SSFR 5
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


print,'Writing cosine response at: ',wlvisz[100],wlnirz[60],' for ',info[0,use[uw]]
;print,strcompress(string(fix(wlvisz[100])),/REMOVE_ALL)
openw,usi,outdir+info[0,use[uw]]+'_cos_lab_'+strcompress(string(fix(wlvisz[100]+0.5)),/REMOVE_ALL)+'.dat',/get_lun
openw,uir,outdir+info[0,use[uw]]+'_cos_lab_'+strcompress(string(fix(wlnirz[100]+0.5)),/REMOVE_ALL)+'.dat',/get_lun
for i=0,na-1 do begin
  printf,usi,mu[i,uw],zsi[100,i,uw]/zsi[100,0,uw]
  printf,uir,mu[i,uw],zir[100,i,uw]/zir[100,0,uw]
endfor
free_lun,usi
free_lun,uir
stop
end
