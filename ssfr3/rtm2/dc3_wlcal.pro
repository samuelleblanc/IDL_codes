@legend.pro
pro wlcal

device,decomposed=0
loadct,27,/silent
!P.multi=0

path='/Users/schmidt/data/attrex/cal/ssfr6/wl/20110812/'
ll='/'
dir='hg_6035_lamp'
;dir='NIR'
;dir='HeNe'
dir=path+dir+ll
np=256
nasa=0 ; 1 if we are using SSFR-5 (the NASA P-3 system)
noaa=1 ; 1 if we are using SSFR-6 (the NOAA P-3 system)
donadir =1
dozenith=1

;xrsi=[600,700]
xrsi=[1000,1050]
xrir=[1250,1350]
;xrir=[900,1200]

ctr    = 0L                        ; Counter for nondark spectra
bn     = 20000
length = 100
utc    = fltarr(bn)
darksi = fltarr(length,np) & darksim=fltarr(np)
darkir = fltarr(length,np) & darkirm=fltarr(np)
rawsi  = fltarr(length,np) & rawsim =fltarr(np)
rawir  = fltarr(length,np) & rawirm =fltarr(np)


poshg=strpos(dir,'6035')
posar=strpos(dir,'6030')
posir=strpos(dir,'NIR')
poshe=strpos(dir,'HeNe')
if poshg gt 0 then hgf=1 else hgf=0
if posar gt 0 then arf=1 else arf=0
if posir gt 0 then nir=1 else nir=0
if poshe gt 0 then he =1 else he =0
if hgf and arf then message,'Hg or Ar?'

if donadir then begin
; first read dark files (nadir)
files=file_search(dir+'nadir'+ll+'dark'+ll+'*.OSA2',count=numfiles)
if numfiles lt 1 then message,'No files to read.'
for ind = 0, numfiles -1 do begin
    print, "Opening ",files[ind]
    openr, lun,files[ind], /get_lun
    while not (eof(lun)) do begin ; Read individual spectra ; or ctr ge length
        spect = {btime: lonarr(2), bcdtimstp:  bytarr(12),intime1: long(0),$
                     intime2: long(0), intime3: long(0), intime4: long(0),accum: long(0), shsw:long(0),$
                     zsit: ulong(0),nsit: ulong(0),zirt: ulong(0),nirt: ulong(0), zirx: ulong(0), nirx: ulong(0),xt: ulong(0),it: ulong(0),$
                     zspecsi: intarr(np), zspecir: intarr(np), nspecsi: intarr(np), nspecir: intarr(np)}
        readu, lun, spect

        atime  = systime(0, spect.btime(0), /utc) ; convert date
        result = strpos(atime(0), ':', 0) ; find first incidence of ':'
        day1 =  strmid(atime(0), result - 5, 1) & day2 =  strmid(atime(0), result - 4, 1)
        if(day1 eq ' ') then day = '0' + day2 else day = day1 + day2
        mon =  strmid(atime(0), result - 9, 3) &  year = fix(strmid(atime(0), result + 7, 4)) ;get year
        hh = strmid(atime(0), result - 2, 2) & mm = strmid(atime(0), result + 1, 2) & ss = strmid(atime(0), result + 4, 2)
        utc[ctr] = double(hh) + double(mm)/60. + double(ss)/3600. ; put hr in decimal form
            darksi[ctr,*]=spect.nspecsi
            darkir[ctr,*]=spect.nspecir
            ;darksi[ctr,*]=spect.zspecsi
            ;darkir[ctr,*]=spect.zspecir
          ctr = ctr + 1L      ; count up spectrum number
       endwhile ; while not eof(lun) - runs until all spectra from file are read
    free_lun, lun               ; ...and closes file afterwards
endfor                          ; primary loop - go through files
utc   = utc  [0:ctr-1]
darksi=darksi[0:ctr-1,*]
darkir=darkir[0:ctr-1,*]
for i=0,np-1 do begin
  darksim[i]=mean(darksi[*,i])
  darkirm[i]=mean(darkir[*,i])
endfor

; second read raw files
files=file_search(dir+'nadir'+ll+'cal'+ll+'*.OSA2',count=numfiles)
ctr=0L
for ind = 0, numfiles -1 do begin
    print, "Opening ",files[ind]
    openr, lun,files[ind], /get_lun
    while not (eof(lun)) do begin ; Read individual spectra ; or ctr ge length
        spect = {btime: lonarr(2), bcdtimstp:  bytarr(12),intime1: long(0),$
                     intime2: long(0), intime3: long(0), intime4: long(0),accum: long(0), shsw:long(0),$
                     zsit: ulong(0),nsit: ulong(0),zirt: ulong(0),nirt: ulong(0), zirx: ulong(0), nirx: ulong(0),xt: ulong(0),it: ulong(0),$
                     zspecsi: intarr(np), zspecir: intarr(np), nspecsi: intarr(np), nspecir: intarr(np)}
        readu, lun, spect

        atime  = systime(0, spect.btime(0), /utc) ; convert date
        result = strpos(atime(0), ':', 0) ; find first incidence of ':'
        day1 =  strmid(atime(0), result - 5, 1) & day2 =  strmid(atime(0), result - 4, 1)
        if(day1 eq ' ') then day = '0' + day2 else day = day1 + day2
        mon =  strmid(atime(0), result - 9, 3) &  year = fix(strmid(atime(0), result + 7, 4)) ;get year
        hh = strmid(atime(0), result - 2, 2) & mm = strmid(atime(0), result + 1, 2) & ss = strmid(atime(0), result + 4, 2)
        utc[ctr] = double(hh) + double(mm)/60. + double(ss)/3600. ; put hr in decimal form
            rawsi[ctr,*]=spect.nspecsi
            rawir[ctr,*]=spect.nspecir
            ;rawsi[ctr,*]=spect.zspecsi
            ;rawir[ctr,*]=spect.zspecir
          ctr = ctr + 1L      ; count up spectrum number
       endwhile ; while not eof(lun) - runs until all spectra from file are read
    free_lun, lun               ; ...and closes file afterwards
endfor                          ; primary loop - go through files
utc   = utc  [0:ctr-1]
rawsi=rawsi[0:ctr-1,*]
rawir=rawir[0:ctr-1,*]
for i=0,np-1 do begin
  rawsim[i]=mean(rawsi[*,i])
  rawirm[i]=mean(rawir[*,i])
endfor
sinad=rawsim-darksim
irnad=rawirm-darkirm
n0=stddev(darksim)
endif else begin
  sinad=fltarr(np)
  irnad=fltarr(np)
  n0=fltarr(np)
endelse

if dozenith then begin
ctr=0L
; first read dark files (zenith)
files=file_search(dir+'zenith'+ll+'dark'+ll+'*.OSA2',count=numfiles)
for ind = 0, numfiles -1 do begin
    print, "Opening ",files[ind]
    openr, lun,files[ind], /get_lun
    while not (eof(lun)) do begin ; Read individual spectra ; or ctr ge length
        spect = {btime: lonarr(2), bcdtimstp:  bytarr(12),intime1: long(0),$
                     intime2: long(0), intime3: long(0), intime4: long(0),accum: long(0), shsw:long(0),$
                     zsit: ulong(0),nsit: ulong(0),zirt: ulong(0),nirt: ulong(0), zirx: ulong(0), nirx: ulong(0),xt: ulong(0),it: ulong(0),$
                     zspecsi: intarr(np), zspecir: intarr(np), nspecsi: intarr(np), nspecir: intarr(np)}
        readu, lun, spect

        atime  = systime(0, spect.btime(0), /utc) ; convert date
        result = strpos(atime(0), ':', 0) ; find first incidence of ':'
        day1 =  strmid(atime(0), result - 5, 1) & day2 =  strmid(atime(0), result - 4, 1)
        if(day1 eq ' ') then day = '0' + day2 else day = day1 + day2
        mon =  strmid(atime(0), result - 9, 3) &  year = fix(strmid(atime(0), result + 7, 4)) ;get year
        hh = strmid(atime(0), result - 2, 2) & mm = strmid(atime(0), result + 1, 2) & ss = strmid(atime(0), result + 4, 2)
        utc[ctr] = double(hh) + double(mm)/60. + double(ss)/3600. ; put hr in decimal form
            ;darksi[ctr,*]=spect.nspecsi
            ;darkir[ctr,*]=spect.nspecir
            darksi[ctr,*]=spect.zspecsi
            darkir[ctr,*]=spect.zspecir
          ctr = ctr + 1L      ; count up spectrum number
       endwhile ; while not eof(lun) - runs until all spectra from file are read
    free_lun, lun               ; ...and closes file afterwards
endfor                          ; primary loop - go through files
utc   = utc  [0:ctr-1]
darksi=darksi[0:ctr-1,*]
darkir=darkir[0:ctr-1,*]
for i=0,np-1 do begin
  darksim[i]=mean(darksi[*,i])
  darkirm[i]=mean(darkir[*,i])
endfor

; second read raw files
files=file_search(dir+'zenith'+ll+'cal'+ll+'*.OSA2',count=numfiles)
ctr=0L
for ind = 0, numfiles -1 do begin
    print, "Opening ",files[ind]
    openr, lun,files[ind], /get_lun
    while not (eof(lun)) do begin ; Read individual spectra ; or ctr ge length
        spect = {btime: lonarr(2), bcdtimstp:  bytarr(12),intime1: long(0),$
                     intime2: long(0), intime3: long(0), intime4: long(0),accum: long(0), shsw:long(0),$
                     zsit: ulong(0),nsit: ulong(0),zirt: ulong(0),nirt: ulong(0), zirx: ulong(0), nirx: ulong(0),xt: ulong(0),it: ulong(0),$
                     zspecsi: intarr(np), zspecir: intarr(np), nspecsi: intarr(np), nspecir: intarr(np)}
        readu, lun, spect

        atime  = systime(0, spect.btime(0), /utc) ; convert date
        result = strpos(atime(0), ':', 0) ; find first incidence of ':'
        day1 =  strmid(atime(0), result - 5, 1) & day2 =  strmid(atime(0), result - 4, 1)
        if(day1 eq ' ') then day = '0' + day2 else day = day1 + day2
        mon =  strmid(atime(0), result - 9, 3) &  year = fix(strmid(atime(0), result + 7, 4)) ;get year
        hh = strmid(atime(0), result - 2, 2) & mm = strmid(atime(0), result + 1, 2) & ss = strmid(atime(0), result + 4, 2)
        utc[ctr] = double(hh) + double(mm)/60. + double(ss)/3600. ; put hr in decimal form
            ;rawsi[ctr,*]=spect.nspecsi
            ;rawir[ctr,*]=spect.nspecir
            rawsi[ctr,*]=spect.zspecsi
            rawir[ctr,*]=spect.zspecir
          ctr = ctr + 1L      ; count up spectrum number
       endwhile ; while not eof(lun) - runs until all spectra from file are read
    free_lun, lun               ; ...and closes file afterwards
endfor                          ; primary loop - go through files
utc   = utc  [0:ctr-1]
rawsi=rawsi[0:ctr-1,*]
rawir=rawir[0:ctr-1,*]
for i=0,np-1 do begin
  rawsim[i]=mean(rawsi[*,i])
  rawirm[i]=mean(rawir[*,i])
endfor
sizen=rawsim-darksim
irzen=rawirm-darkirm
z0=stddev(darksim)
endif else begin
  sizen=fltarr(np)
  irzen=fltarr(np)
  z0=fltarr(np)
endelse

; third determine wavelengths
if noaa then cz=[[303.087,3.30588,4.09568E-04,-1.63269E-06,0],[2213.37,-4.46844,-0.00111879,-2.76593E-06,-1.57883E-08]]
if noaa then cn=[[302.255,3.30977,4.38733E-04,-1.90935E-06,0],[2225.74,-4.37926,-0.00220588, 2.80201E-06,-2.2624E-08 ]]
if nasa then cz=[[301.755,3.32635,4.44514E-04,-1.93826E-06,0],[2221.04,-4.35923,-0.00278573, 4.47858e-06,-2.48982e-08]]
if nasa then cn=[[3.043740643E+02,3.304442502,9.061725871E-05,-1.310768888E-06,0],[2226.93,-4.48862,-0.00050990,-7.40257e-06,-4.28565e-09]]

siz=size(cz)
wlsin=fltarr(np) & wlirn=fltarr(np) & nc=siz[1]
wlsiz=fltarr(np) & wlirz=fltarr(np)
for i=0,np-1 do begin
   for j=0,nc-1 do wlsiz[i]     =wlsiz[i]     +cz[j,0]*float(i)^j
   for j=0,nc-1 do wlirz[i]     =wlirz[i]     +cz[j,1]*float(i)^j
   for j=0,nc-1 do wlsin[i]     =wlsin[i]     +cn[j,0]*float(i)^j
   for j=0,nc-1 do wlirn[i]     =wlirn[i]     +cn[j,1]*float(i)^j
endfor


; fourth define center wavelengths of lamps
ar=[355.43,394.9,404.44,415.86,416.4,418.19,419.1,419.8,420.07,425.12,425.94,426.63,427.22,427.4,430.01,432.0,433.36,434.52,641.63,667.73,675.28,687.13,693.77,696.54,$
    703.03,706.72,714.70,727.29,737.21,738.4,750.39,751.46,763.51,772.38,794.82,800.62,801.48,810.37,811.53,826.45,840.82,842.46]
n_ar=n_elements(ar)
hg=[296.73,302.15,312.57,313.17,334.15,365.02,365.48,366.33,404.66,407.78,433.92,434.75,435.48,491.6,546.07,576.96,579.07,1014]
n_hg=n_elements(hg)
ir=[1297]
n_ir=n_elements(ir)
hene=[632.8]
n_he=n_elements(hene)

window,1,tit='SI'


xr=xrsi
flt=where(wlsin ge xr[0] and wlsin le xr[1])
mx=max([max(sinad[flt]),max(sizen[flt])])

plot,wlsin,sinad,xr=xr,yr=[0,mx*1.3],xtit='WL [nm]',ytit='DN',tit=dir,/xs,/ys
oplot,wlsiz,sizen,color=30
oplot,[xr[0],xr[1]],[n0,n0],linesty=1,thick=2
oplot,[xr[0],xr[1]],[z0,z0],linesty=1,color=30,thick=2
if xr[1]-xr[0] le 200 then begin
  oplot,wlsin,sinad,psym=4
  oplot,wlsiz,sizen,psym=4,color=30
endif
if hgf then begin
for i=0,n_hg-1 do begin
  oplot,[hg[i],hg[i]],[0,mx],linesty=1,color=100
endfor
endif
if arf then begin
for i=0,n_ar-1 do begin
  oplot,[ar[i],ar[i]],[0,mx*1.3],linesty=1,color=100
endfor
endif
if he then begin
for i=0,n_he-1 do begin
  oplot,[hene[i],hene[i]],[0,mx*1.3],linesty=1,color=100
endfor
endif

legend,['Nadir','Zenith'],textcolors=[255,30]

cursor,x1,y1
wait,0.2
cursor,x2,y2
fn=where(wlsin ge x1 and wlsin le x2)
res=gaussfit(wlsin[fn],sinad[fn],A,nterms=3)
xx=indgen(101)*(x2-x1)/100.+x1
ff=a[0]*exp(-0.5*((xx-a[1])/a[2])^2)
fwhmn=2*SQRT(2*ALOG(2))*A[2]
an=a[1]
oplot,xx,ff,thick=2
fn=where(wlsiz ge x1 and wlsin le x2)
res=gaussfit(wlsiz[fn],sizen[fn],A,nterms=3)
xx=indgen(101)*(x2-x1)/100.+x1
ff=a[0]*exp(-0.5*((xx-a[1])/a[2])^2)
fwhmz=2*SQRT(2*ALOG(2))*A[2]
az=a[1]
oplot,xx,ff,thick=2,color=30
print,'nadir zenith'
print,an,az
print,fwhmn,fwhmz

p=tvrd(true=1)
write_png,dir+'resp_si.png',p


window,2,tit='IR'


xr=xrir
flt=where(wlirn ge xr[0] and wlirn le xr[1])
mx=max([max(irnad[flt]),max(irzen[flt])])

plot,wlirn,irnad,xr=xr,yr=[0,mx*1.3],xtit='WL [nm]',ytit='DN',tit=dir,/xs,/ys
oplot,wlirz,irzen,color=30
oplot,[xr[0],xr[1]],[n0,n0],linesty=1,thick=2
oplot,[xr[0],xr[1]],[z0,z0],linesty=1,color=30,thick=2
if xr[1]-xr[0] le 200 then begin
  oplot,wlirn,irnad,psym=4
  oplot,wlirz,irzen,psym=4,color=30
endif
if hgf then begin
for i=0,n_hg-1 do begin
  oplot,[hg[i],hg[i]],[0,mx],linesty=1,color=100
endfor
endif
if arf then begin
for i=0,n_ar-1 do begin
  oplot,[ar[i],ar[i]],[0,mx*1.3],linesty=1,color=100
endfor
endif
if nir then begin
for i=0,n_ir-1 do begin
  oplot,[ir[i],ir[i]],[0,mx*1.3],linesty=1,color=100
endfor
endif

legend,['Nadir','Zenith'],textcolors=[255,30]

cursor,x1,y1
wait,0.2
cursor,x2,y2
fn=where(wlirn ge x1 and wlirn le x2)
res=gaussfit(wlirn[fn],irnad[fn],A,nterms=3)
xx=indgen(101)*(x2-x1)/100.+x1
ff=a[0]*exp(-0.5*((xx-a[1])/a[2])^2)
fwhmn=2*SQRT(2*ALOG(2))*A[2]
an=a[1]
oplot,xx,ff,thick=2
fn=where(wlirz ge x1 and wlirn le x2)
res=gaussfit(wlirz[fn],irzen[fn],A,nterms=3)
xx=indgen(101)*(x2-x1)/100.+x1
ff=a[0]*exp(-0.5*((xx-a[1])/a[2])^2)
fwhmz=2*SQRT(2*ALOG(2))*A[2]
az=a[1]
oplot,xx,ff,thick=2,color=30
print,'nadir zenith'
print,an,az
print,fwhmn,fwhmz

p=tvrd(true=1)
write_png,dir+'resp_ir.png',p

stop
end
