@legend.pro
pro plot_cal
np=256

device,decomposed=0
loadct,27
!P.multi=0

fdir = '/data/seven/schmidt/attrex/cal/ssfr6/'
cfil = 'rfs.cfg'     ; cfg file telling the program what to plot

; (1) search for nadir calibration files
openr,ur,fdir+cfil,/get_lun
str0='' & str='' & nad=''
while strlen(str) lt 1 do begin
  readf,ur,str0
  if strcmp(str0,'nvis',4) then str=str0
endwhile
while (not strcmp(str0,'zvis',4)) and (not eof(ur)) do begin
  readf,ur,str0
  if (not strcmp(str0,'#',1)) and (not strcmp(str0,' ',1)) and (strlen(str0) gt 2) and (not strcmp(str0,'zvis',4)) then begin
    nad=[nad,str0]
  endif
endwhile
free_lun,ur
nn=n_elements(nad)-1
if nn lt 1 then message,'Nothing to plot'
nad=nad[1:nn]
nvis_files=strarr(nn)&nnir_files=strarr(nn)&ncolors=intarr(nn)&nlinestyles=intarr(nn)&nthicks=fltarr(nn)
for i=0,nn-1 do begin
  str=strsplit(nad[i],' ,',escape='#',/EXTRACT)
  nvis_files[i]=str[0] & nnir_files[i]=str[1]&ncolors[i]=fix(str[2])&nlinestyles[i]=fix(str[3])&nthicks[i]=float(str[4])
endfor

; (2) search for zenith calibration files
openr,ur,fdir+cfil,/get_lun
str0='' & str='' & zen=''
while strlen(str) lt 1 do begin
  readf,ur,str0
  if strcmp(str0,'zvis',4) then str=str0
endwhile
while (not strcmp(str0,'nvis',4)) and (not eof(ur)) do begin
  readf,ur,str0
  if (not strcmp(str0,'#',1)) and (not strcmp(str0,' ',1)) and (strlen(str0) gt 2) and (not strcmp(str0,'nvis',4)) then begin
    zen=[zen,str0]
  endif
endwhile
free_lun,ur
nz=n_elements(zen)-1
if nz lt 1 then message,'Nothing to plot'
zen=zen[1:nz]
zvis_files=strarr(nz)&znir_files=strarr(nz)&zcolors=intarr(nz)&zlinestyles=intarr(nz)&zthicks=fltarr(nz)
for i=0,nz-1 do begin
  str=strsplit(zen[i],' ,',escape='#',/EXTRACT)
  zvis_files[i]=str[0] & znir_files[i]=str[1]&zcolors[i]=fix(str[2])&zlinestyles[i]=fix(str[3])&zthicks[i]=float(str[4])
endfor

wlvisz = fltarr(np)          ;zenith si wavelength array
wlvisn = fltarr(np)          ;nadir si wavelength array
wlnirz = fltarr(np)          ;zenith ir wavelength array
wlnirn = fltarr(np)          ;nadir ir wavelength array

window,0,tit='zenith '+fdir

zresp_fun=fltarr(n_elements(zvis_files),2,256)
nresp_fun=fltarr(n_elements(nvis_files),2,256)

for ii=0,n_elements(zvis_files)-1 do begin

; ZENITH SILICON
    cal=fdir + zvis_files(ii) ;'20070606_resp2_zensi.dat'
    print, "OPENING ",cal
    openr,zenlun,cal,/get_lun
    for i=0,np-1 do begin
        readf,zenlun,wl,zrespsi
        wlvisz[i]        =wl
        zresp_fun[ii,0,i]=zrespsi
    endfor
    free_lun,zenlun

; ZENITH InGaAs
    cal=fdir + znir_files(ii) ;'20070606_resp2_zensi.dat'
    openr,zenlun,cal,/get_lun
    for i=0,np-1 do begin
        readf,zenlun, wl,zrespir
        wlnirz[i]=wl
        zresp_fun[ii,1,i]=zrespir
    endfor
    free_lun, zenlun

    if(ii eq 0) then begin
        plot, wlvisz,zresp_fun(ii,0,*),ytitle='counts ms!E-1!N / (W m!E-2!N nm!E-1!N)',xtitle='Wavelength [nm]',title='Zenith '+fdir,charsize=1.5,thick=zthicks[ii],/xs,/ys,xrange=[300,2200],color=zcolors[ii],linestyle=zlinestyles[ii],yr=[0,260]
    endif else begin
        oplot,wlvisz,zresp_fun(ii,0,*),color=zcolors[ii],thick=zthicks[ii],linestyle=zlinestyles[ii]
    endelse
    oplot,wlnirz,zresp_fun(ii,1,*),color=zcolors[ii],linestyle=zlinestyles[ii],thick=zthicks[ii]

endfor

items = strmid(zvis_files,0,23)
for i=0,nz-1 do begin
  pos=strpos(zvis_files[i],'resp1')
  if pos gt 0 then items[i]='Primary '+items[i]
endfor
legend,items,textcolors=zcolors,/right,outline_color=5,linestyle=zlinestyles,colors=zcolors

img=tvrd(true=1)
filename = fdir+'zenith_resp_func.png'
write_image,filename,'PNG',img


window,1,tit='nadir '+fdir
for ii=0,n_elements(nvis_files)-1 do begin

; NADIR SILICON
    cal=fdir + nvis_files(ii) ;'20070606_resp2_zensi.dat'
    print, "OPENING ",cal
    openr,nadlun,cal,/get_lun
    for i=0,np-1 do begin
        readf,nadlun,wl,nrespsi
        wlvisn[i]=wl
        nresp_fun[ii,0,i]=nrespsi
    endfor
    free_lun,nadlun

; NADIR InGaAs
    cal=fdir + nnir_files(ii) ;'20070606_resp2_zensi.dat'
    openr,nadlun,cal,/get_lun
    for i=0,np-1 do begin
        readf,nadlun, wl,nrespir
        wlnirn[i]=wl
        nresp_fun[ii,1,i]=nrespir
    endfor
    free_lun, nadlun
    if(ii eq 0) then begin
        plot, wlvisn,nresp_fun(ii,0,*),ytitle='counts ms!E-1!N / (W m!E-2!N nm!E-1!N)',xtitle='Wavelength [nm]',title='Nadir '+fdir,charsize=1.5,/xs,/ys,xrange=[300,2200],yrange=[0,260],color=ncolors[ii],linestyle=nlinestyles[ii],thick=nthicks[ii]
    endif else begin
        oplot,wlvisn,nresp_fun(ii,0,*),color=ncolors[ii],thick=nthicks[ii],linestyle=nlinestyles[ii]
    endelse
    oplot,wlnirn,nresp_fun(ii,1,*),color=ncolors[ii],linestyle=nlinestyles[ii],thick=nthicks[ii]
endfor

items = strmid(nvis_files,0,23)
for i=0,nn-1 do begin
  pos=strpos(nvis_files[i],'resp1')
  if pos gt 0 then items[i]='Primary '+items[i]
endfor
legend,items,textcolors=ncolors,/right,outline_color=5,linestyle=nlinestyles,colors=ncolors
img=tvrd(true=1)
filename = fdir+'nadir_resp_func.png'
write_image,filename,'PNG',img

; get max/min of response function as estimate for SSFR radiometric uncertainty
zvismin=fltarr(np)&zvismax=fltarr(np)&zvisave=fltarr(np)
znirmin=fltarr(np)&znirmax=fltarr(np)&znirave=fltarr(np)
nvismin=fltarr(np)&nvismax=fltarr(np)&nvisave=fltarr(np)
nnirmin=fltarr(np)&nnirmax=fltarr(np)&nnirave=fltarr(np)
openw,uu,fdir+'uncertainty.dat',/get_lun
printf,uu,'ZVIS ZNIR NVIS NNIR'
for i=0,np-1 do begin
  zvismin[i]=min (zresp_fun[*,0,i]) ; VIS
  zvismax[i]=max (zresp_fun[*,0,i]) ; VIS
  zvisave[i]=mean(zresp_fun[*,0,i]) ; VIS
  znirmin[i]=min (zresp_fun[*,1,i]) ; NIR
  znirmax[i]=max (zresp_fun[*,1,i]) ; NIR
  znirave[i]=mean(zresp_fun[*,1,i]) ; NIR
  nvismin[i]=min (nresp_fun[*,0,i]) ; VIS
  nvismax[i]=max (nresp_fun[*,0,i]) ; VIS
  nvisave[i]=mean(nresp_fun[*,0,i]) ; VIS
  nnirmin[i]=min (nresp_fun[*,1,i]) ; NIR
  nnirmax[i]=max (nresp_fun[*,1,i]) ; NIR
  nnirave[i]=mean(nresp_fun[*,1,i]) ; NIR
  printf,uu,(zvismax[i]-zvismin[i])/zvisave[i],(znirmax[i]-znirmin[i])/znirave[i],(nvismax[i]-nvismin[i])/nvisave[i],(nnirmax[i]-nnirmin[i])/nnirave[i]
endfor
free_lun,uu

window,2,tit='SSFR uncertainty'
plot ,wlvisz,(zvismax-zvismin)/zvisave*100,xtit='Wavelength [nm]',ytit='Uncertainty [%]',xr=[300,2180],yr=[0.,50],/xs,tit=fdir
oplot,wlnirz,(znirmax-znirmin)/znirave*100,color=30
oplot,wlvisz,(nvismax-nvismin)/nvisave*100,color=60
oplot,wlnirz,(nnirmax-nnirmin)/nnirave*100,color=90
legend,['ZENITH VIS','ZENITH NIR','NADIR VIS','NADIR NIR'],textcolors=[255,30,60,90]
img=tvrd(true=1)
filename = fdir+'resp_func_uncertainty.png'
write_image,filename,'PNG',img
stop

end
