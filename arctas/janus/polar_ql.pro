; modifiied for use by Samuel LeBlanc 20100827
; not to be used with other arctas data




@cfg.pro
;@C:\polar\pro\disp1.pro
;@C:\polar\pro\nav_noaa.pro
@nav_nasa.pro
;@C:\TC4\pro\archive.pro
@legend.pro
@rayleigh.pro
;@C:\polar\pro\cg4_noaa_p3.pro
@polar_get_cos.pro
;@C:\polar\pro\archive_nasa.pro
;@C:\polar\pro\archive_noaa.pro


pro polar_ql
; windows:
dir='/home/leblanc/arctas/nasa/'
date='20080409'
ll='/' ; directory separator
attcorr=1 ; 0: use uncorrected data, 1: use corrected data.

; *******************************************************************************
; get information from configuration file
; *******************************************************************************
; initialize configuration file
cfg_file=dir+ll+date+ll+'polar.cfg'        ; build cfg file
if file_test(cfg_file) ne 1 then message,'Did not find configuration file.'
plot=cfg(cfg_file,'plot')
; get platform
if strcmp(strmid(plot,0,1),'y',/FOLD_CASE) then plot=1 else plot=0
;plot=0
platform=cfg(cfg_file,'platform')
print,'Process flight from '+date+' onboard '+platform+'.'
c0=cfg(cfg_file,'comment')
if not strcmp(c0,'#') then begin
  print,c0
  comment=c0
endif
; get extraterrestial flux
extra=cfg(cfg_file,'extra') ; extra-terrestial flux
; get time range
i0=cfg(cfg_file,'interval') ; look if we should only use data within a certain time window
uu0=0
if not strcmp(i0,'#') then begin
    uu=strsplit(i0,' ,',escape='#',/EXTRACT)
    uu=float(uu)
    uu0=1
    u0=uu[0] & u1=uu[1]
endif
; get broadband range
bb1=cfg(cfg_file,'bb1')
bb1=strsplit(bb1,' ,',escape='#',/EXTRACT)
bb1=float(bb1)
bb2=cfg(cfg_file,'bb2')
bb2=strsplit(bb2,' ,',escape='#',/EXTRACT)
bb2=float(bb2)
if strcmp(platform,'NOAAP3',3,/FOLD_CASE) then noaa=1 else noaa=0

; *******************************************************************************
; get spectra
; *******************************************************************************
print,'Restore SSFR data...'
if attcorr then begin
  restore, dir+ll+date+ll+date+'_calibspcs_attcorr.out'
endif else begin
  restore, dir+date+ll+date+'_calibspcs.out' ; if no attitude corrected data available
endelse
utc=tmhrs
nn=n_elements(utc)
; make solar broadband
print,'Calculate solar broadband...'
bb1z=where(zenlambda ge bb1[0] and zenlambda le bb1[1]) & bb1n=where(nadlambda ge bb1[0] and nadlambda le bb1[1])
bb2z=where(zenlambda ge bb2[0] and zenlambda le bb2[1]) & bb2n=where(nadlambda ge bb2[0] and nadlambda le bb2[1])
dz1=0.5*(zenlambda[bb1z+1]-zenlambda[bb1z-1])           & dn1=0.5*(nadlambda[bb1n+1]-nadlambda[bb1n-1])
dz2=0.5*(zenlambda[bb2z+1]-zenlambda[bb2z-1])           & dn2=0.5*(nadlambda[bb2n+1]-nadlambda[bb2n-1])
bz1=fltarr(nn) & bn1=fltarr(nn) & bz2=fltarr(nn) & bn2=fltarr(nn)
for i=0,nn-1 do begin
  bz1[i]=total(zspectra[bb1z,i]*dz1)
  bn1[i]=total(nspectra[bb1n,i]*dn1)
  bz2[i]=total(zspectra[bb2z,i]*dz2)
  bn2[i]=total(nspectra[bb2n,i]*dn2)
endfor

; *******************************************************************************
; define labels
; *******************************************************************************

lats  =[71.325   , 70.780855    ,70.772525   ,71.263995  ,70.605925   ,70.280234   ,70.30098   ,70.172765   ,69.997835   ,69.589665   ,69.439725   ,69.617763  ,69.579442  ,68.231875   ,68.298515]
lons  =[-156.433 , -158.7133515 ,-157.8550523,-156.82176 ,-156.0301253,-159.061471 ,-157.493707,-156.9717543,-155.0384983,-157.8050543,-157.4217363,-156.257971,-155.056727,-153.8802113,-154.5218523]
label =['EL'     ,'G1'         ,'G2'        ,'G3'       ,'G4'        ,'B1'        ,'B2'       ,'B3'        ,'B4'        ,'F1'        ,'F2'        ,'F3'       ,'F4'       ,'L1'        ,'L2']
nlabel=n_elements(lats)

; *******************************************************************************
; get NAV data
; *******************************************************************************
nav=cfg(cfg_file,'read_nav')
; get platform
if strcmp(strmid(nav,0,1),'n',/FOLD_CASE) then nav=0 else nav=1
if nav then begin
 if strcmp(platform,'NOAAP3',3,/FOLD_CASE) then begin
   nav_noaa,dir,ll,date,cfg_file,utc,alt,lat,lon,dc,sza,iza,pit=pit,rol=rol
 endif else begin
   if strcmp(platform,'NASAP3',3,/FOLD_CASE) then begin
     nav_nasa,dir,date,cfg_file,utc,alt,lat,lon,dc,sza,iza,lats,lons,label,pit=pit,rol=rol
   endif else begin
     message,'Wrong platform.'
   endelse
 endelse
endif else begin
 navf=file_search(dir+ll+date+ll+'nav.out',count=nnf)
 if nnf eq 1 then nav=1 else nav=0
endelse

if strcmp(platform,'NOAAP3',3,/FOLD_CASE) then begin
   cg4_noaa_p3,dir+date+ll,utc,cg4z,cg4zt,cg4n,cg4nt
   cg4up=5.6704E-8*(cg4nt+273.15)^4 + cg4n & cg4dn=5.6704E-8*(cg4zt+273.15)^4 + cg4z ; convert CG-4 net fluxes to up/downward irradiance
   ind=where(cg4up lt 0. or cg4up gt 2000.,ni)
   if ni gt 0 then cg4up[ind]=0.
   ind=where(cg4dn lt 0. or cg4dn gt 2000.,ni)
   if ni gt 0 then cg4dn[ind]=0.
endif

; *******************************************************************************
; make plots
; *******************************************************************************
if plot then begin
  device,decomposed=0         ; define graphics
  loadct,27
  if uu0 then begin           ; only if time interval defined plot spectra every hour
        !p.multi=[0,1,2]
        window,0,xsize=400,ysize=800,retain=2,tit='Spectra'
        ut0=max([u0,min(utc)])
        count = 0
        num_plots = fix(max(utc)-ut0)+1
        data_times = findgen(num_plots) + ut0
        ymax = 0
        times = strarr(num_plots)
        linestyle=0

        while ut0 lt max(utc) do begin
            mm=min(abs(ut0-utc),ind)
            print,utc[ind]
            ind=ind[0]
            hh=fix(utc[ind]) & mm=fix( (utc[ind]-fix(utc[ind]))*60.+0.5)
            ymax = max([ymax,max(zspectra[*,ind])])
            ut0=ut0+1.
        endwhile

        ut0=max([u0,min(utc)])
        while ut0 lt max(utc) do begin
            mm=min(abs(ut0-utc),ind)
            ind=ind[0]
            hh=fix(utc[ind]) & mm=fix( (utc[ind]-fix(utc[ind]))*60.+0.5)
            tim=strcompress(string(hh,format='(i2.2)'),/remove_all)+strcompress(string(mm,format='(i2.2)'),/remove_all)

            if(count eq 0) then begin
                plot,zenlambda,zspectra[*,ind],tit=platform+' '+date+' SSFR DOWN',xtit='wavelength [nm]',ytit='Irradiance [W m!E-2!N nm!E-1!N]',xr=[350,2200],yr=[0,ymax*1.04],color=255,linestyle=0,/xs,/ys
            endif else begin
                oplot,zenlambda,zspectra[*,ind],color=count*20,linestyle=(count mod 5)*2
            endelse
            ut0=ut0+1.
            times[count] = strmid(tim,0,2) + ':' +  strmid(tim,2,2) + ' UTC'
            count += 1

        endwhile
        legend,times,textcolors=indgen(count)*20,linestyle=(indgen(count) mod 5)*2,/right,colors=indgen(count)*20,outline_color=5

        ut0=max([u0,min(utc)])
        ymax=0
        while ut0 lt max(utc) do begin
            mm=min(abs(ut0-utc),ind)
            ind=ind[0]
            hh=fix(utc[ind]) & mm=fix( (utc[ind]-fix(utc[ind]))*60.+0.5)
            ymax = max([ymax,max(nspectra[*,ind])])
            ut0=ut0+1.
        endwhile

        ut0=max([u0,min(utc)])
        count = 0
        while ut0 lt max(utc) do begin
            mm=min(abs(ut0-utc),ind)
            ind=ind[0]
            hh=fix(utc[ind]) & mm=fix( (utc[ind]-fix(utc[ind]))*60.+0.5)
            tim=strcompress(string(hh,format='(i2.2)'),/remove_all)+strcompress(string(mm,format='(i2.2)'),/remove_all)
            if(count eq 0) then begin
                plot,nadlambda,nspectra[*,ind],tit='SSFR UP',xtit='wavelength [nm]',ytit='Irradiance [W m!E-2!N nm!E-1!N]',xr=[350,2200],yr=[0,ymax*1.04],color=255,linestyle=0,/xs,/ys
            endif else begin
                oplot,nadlambda,nspectra[*,ind],color=count*20,linestyle=(count mod 5)*2
            endelse
            ut0=ut0+1.
            count += 1
        endwhile
        legend,times,textcolors=indgen(count)*20,linestyle=(indgen(count) mod 5)*2,/right,colors=indgen(count)*20,outline_color=5
        ;p=tvrd(true=1)
        ;write_png,dir+date+ll+'SF'+date+'__SP_'+platform+'.PNG',p

        ut0=max([u0,min(utc)])
        !p.multi=0
        window,1,retain=2,tit='Albedo'
        ux=where(utc ge ut0,nx)
        albedo=fltarr(nx,n_elements(zenlambda))
        print,'Calculating albedo...'
        ;flta=flt & nfa=nf
        flta=where(utc gt ut0,nfa)
        for i=0,nfa-1 do begin
          tmp=interpol(nspectra[*,flta[i]],nadlambda,zenlambda)
          ind=where(zspectra[*,flta[i]] gt 0.01,na)
          if na gt 0 and flta[i]+nx-nn ge 0 then albedo[flta[i]+nx-nn,ind]=tmp[ind]/zspectra[ind,flta[i]]
        endfor

        xaxis=utc[ux]
        ;disp,albedo,xaxis,zenlambda,tit=platform+' '+date+': SSFR Albedo',bartit='Albedo',xtit='UTC[h]',ytit='Wavelength [nm]',charsize=1,min=0.,max=1.
        ;p=tvrd(true=1)
        ;write_png,dir+date+ll+'SF'+date+'__AL_'+platform+'.PNG',p

        loadct,27
        if noaa and n_elements(cg4dn) gt 1 then !p.multi=[0,1,3] else !p.multi=[0,1,2]
        if noaa and n_elements(cg4dn) gt 1 then window,2,retain=2,tit='Broadband',ysize=900 else window,2,retain=2,tit='Broadband',ysize=700
        if noaa and n_elements(cg4dn) gt 1 then cs=2 else cs=1
        plot,utc,bz2,psym=3,tit=platform+' '+date+' Broadband shortwave irradiance (SSFR) downward',xtit='UTC[h]',ytit='Irradiance [W m!E-2!N]',/xs,chars=cs,xr=uu
        oplot,utc,bz1,psym=3,color=70
        legend,[string(bb2[0])+'-'+strcompress(string(bb2[1]),/remove_all)+' nm',string(bb1[0])+'-'+strcompress(string(bb1[1]),/remove_all)+' nm'],textcolors=[255,70]
        plot,utc,bn2,psym=3,tit='Broadband shortwave irradiance (SSFR) upward',xtit='UTC[h]',ytit='Irradiance [W m!E-2!N]',/xs,charsize=cs,xr=uu
        oplot,utc,bn1,psym=3,color=70
        if noaa and n_elements(cg4dn) gt 1 then begin
          plot,utc,cg4dn,psym=3,tit='Broadband longwave irradiance (CG-4)',xtit='UTC[h]',ytit='Irradiance [W m!E-2!N]',/xs,yr=[0,500],charsize=cs,xr=uu
          oplot,utc,cg4up,psym=3,color=70
          legend,['downward','upward'],textcolors=[255,70]
        endif
        ;p1=tvrd(true=1)
        ;write_png,dir+date+ll+'SF'+date+'_BB_'+platform+'.png',p1
  endif ; if uu0
endif

; *******************************************************************************
; get temperatures
; *******************************************************************************
temppattern=dir+date+ll+date+'_temp.out'
file=file_search(temppattern,count=tfn)
if tfn eq 1 then begin
print,'Restore TEMP file...'
restore,file[0]
utcd=tmhrs
if plot then begin
window,4,xsize=900,ysize=700,retain=2,ypos=600,tit='Temperatures'
  !P.multi=[0,2,2]
  xr=[max([min(utc),uu0]),max(utc)]
  hot=where(nsitemp gt 100 or zsitemp gt 100,nhot)
  if nhot gt 0 then begin
    nsitemp[hot]=0.
    zsitemp[hot]=0.
  endif
  plot,utcd,bxtemp,tit=platform+' '+date+' computer temperature',xtit='UTC [h]',ytit='T [C]',psym=3,xr=xr,/xstyle,yr=[10,40]
  oplot,utcd,zirxt,color=40
  oplot,utcd,nirxt,color=80
  legend,['Box','Zenith NIR block','Nadir NIR block'],textcolors=[255,40,80]
  plot,utcd,bxcltemp,xtit='UTC [h]',ytit='T [C]',psym=3,xr=xr,/xstyle
  legend,['Ambient Cabin'],textcolors=[255]
  plot,utcd,nsitemp,tit='Silicon temperatures',xtit='UTC [h]',ytit='T [C]',psym=3,xr=xr,yr=[20,max(nsitemp)+3],/xs
  oplot,utcd,zsitemp,color=70,psym=3
  legend,['Nadir','Zenith'],textcolors=[255,70]
  plot,utcd,nirtemp,tit='InGaAs temperature',xtit='UTC [h]',ytit='T [C]',psym=3,yr=[min([min(nirtemp),min(zirtemp)]),0],xr=xr,/xs
  oplot,utcd,zirtemp,color=70,psym=3
  legend,['Nadir','Zenith'],textcolors=[255,70]
  ;p=tvrd(true=1)
  ;write_png,dir+date+ll+date+'_temps.png',p
endif
endif

; *******************************************************************************
; get cosine correction
; *******************************************************************************
wl=cfg(cfg_file,'wlql')
wl=strsplit(wl,' ,',escape='#',/EXTRACT)
wl=float(wl)
nl=n_elements(wl)
if nav and not attcorr then get_cos,dir,date,cfg_file,utc,alt,sza,dc,fac, dz,facn,dn,wl,ray
;            get_cos,dir,date,cfg,     utc,alt,sza,dc,facz,dz,facn,dn,wl,ray

; *******************************************************************************
; filter data
; *******************************************************************************
izamax=cfg(cfg_file,'izamax')
if strmid(izamax,0,1) eq '#' then izamax=-1 else izamax=float(izamax)
if uu0 eq 0 and nav eq 0 then message,'Please give interval in cfg file.'
if uu0 eq 0 then $
  flt=where(iza le izamax,nf) $ ; could also choose different filters
else $
  if nav and izamax gt 0 then flt=where(utc ge uu[0] and utc le uu[1] and iza le izamax,nf) else flt=where(utc ge uu[0] and utc le uu[1],nf)
; get all the data you want out of the restored spectra
nzen=n_elements(zenlambda) & nnad=n_elements(nadlambda)

nind=intarr(nl) & zind=intarr(nl)
for l=0,nl-1 do begin
  mm=min(abs(wl[l]-zenlambda),ind)
  zind[l]=ind
  mm=min(abs(wl[l]-nadlambda),ind)
  nind[l]=ind
endfor
; filter data
utf=utc[flt]
if nav then begin
  dcf=dc [flt]
  alf=alt[flt]
endif
nad=nspectra[*,flt]
nad=nad[nind,*]
zen=zspectra[*,flt]
zen=zen[zind,*]
if n_elements(sat) gt 0 then begin
  satind=where(sat[flt] gt 0,satnb)
  print,'Saturated spectra:',satnb
endif

if plot and nav then begin
  siz=get_screen_size()
  window,3,xs=siz[0],ys=siz[1]*0.8,retain=2,tit='Quicklook'
  cs=1.5
  !p.multi=[0,2,2]
  x0=min(utf) & x1=max(utf) & legendx=max(utf)+0.2
  if attcorr then fac=1. ;fac=fac-fac+1. ; if attitude already corrected in original data, don't use fac
  plot,utf,zen[0,*]*fac[flt],xr=[x0,x1],yr=[0,2.0],xtitle='UTC [h]',ytitle='Irradiance [W m!E-2!N nm!E-1!N]',psym=3,charsize=cs,title=platform+' '+date+' SSFR DOWN ',/xstyle
  ;oplot,utf,zen[0,*],color=200,psym=3
  oplot,utf,rayleigh(sza[flt],20000.,zenlambda[zind[0]],extra),linesty=1,thick=2

  outtxt=['wavelength [nm]',strtrim(string(zenlambda[zind[0]]),2)]
  outclr=[254              ,254]
  for l=1,nl-1 do begin
    oplot,utf,zen[l,*]*fac[flt],color=40*l,psym=3
    oplot,utf,rayleigh(sza[flt],20000.,zenlambda[zind[l]],extra),linesty=1,thick=2,color=40*l
    outtxt=[outtxt,strtrim(string(zenlambda[zind[l]]),2)]
    outclr=[outclr,40*l]
  endfor
  legend,outtxt,textcolors=outclr,charsize=1.0

  plot,utf,nad[0,*],xr=[x0,x1],yr=[0.,max(nad)],xtitle='UTC [h]',ytitle='Irradiance [W m!E-2!N nm!E-1!N]',psym=3,charsize=cs,title='SSFR UP ',/xstyle
  for l=1,nl-1 do begin
    oplot,utf,nad[l,*],color=20*l,psym=3
  endfor

  plot,utc,cos(!pi/180.*sza),xr=[x0,x1],yr=[0,1.25],xtitle='UTC [h]',ytitle='cos(SSFR<->SZA) cos(SZA)',charsize=cs,title='SZA and ATTITUDE',/ystyle,/xstyle
  oplot,utc,dc,psym=3,color=20
  oplot,utf,dc[flt],psym=3,color=200
  oplot,utf,fac[flt],psym=3
  str=strtrim(string(fix(izamax+0.5)),2)

  outtxt=['cos(SZA)','cos(SZA<->SSFR)','cos(SZA<->SSFR<'+str+'deg)','pitch offset='+cfg(cfg_file,'pitchoff'),'roll offset='+cfg(cfg_file,'rolloff')]
  outclr=[255       ,20               ,200                         ,255,255]
  legend,outtxt,textcolors=outclr,charsize=1.0

  plot,utc,alt*0.001,xr=[x0,x1],yr=[0,max(alt*0.001)],xtitle='UTC [h]',ytitle='Altitude [km] ',charsize=cs,title='ALTITUDE',psym=3,/xstyle

  ngeo = n_elements(alt)
  mxa  = max(alt)
  for i=0,ngeo-2 do begin
    cl=fix(alt[i]/mxa*180)
    oplot,utc[i:i+1],alt[i:i+1]*0.001,color=cl,psym=3
  endfor

  ;p1=tvrd(true=1)
  ;write_png,dir+date+ll+'SF'+date+'__QL_'+platform+'.PNG',p1


  window,6,tit='Attitude',ysize=900
  cs=2
  !P.multi=[0,1,3]
  plot,utc,pit,psym=3,tit=date+' '+platform+' pitch',yr=[-4,4],xtit='UTC [h]',ytit='Pitch [deg]',xr=uu,charsize=cs
  plot,utc,rol,psym=3,tit=date+' '+platform+' roll',yr=[-8,8],xtit='UTC [h]',ytit='Roll [deg]',xr=uu,charsize=cs
  if n_elements(sat) eq 0 then begin
    plot,utf,zen[0,*]*fac[flt],tit='downward irradiance',xtit='UTC [h]',ytit='F_dn [W m!E-2!N nm!E-1!N]',xr=uu,psym=3,charsize=cs,yr=[min(zen[0,*])*0.8,max(zen[0,*])*1.2],/ys
    oplot,utf,zen[0,*],psym=3,color=120
  endif else begin
    plot,utc,sat,ytit='Saturation # Zenith',chars=cs,xr=uu
  endelse


  ;legend,['corrected','uncorrected'],textcolors=[255,120]
  ;p1=tvrd(true=1)
  ;write_png,dir+date+ll+'SF'+date+'__ATT_'+platform+'.PNG',p1

  siz=get_screen_size()
  window,5,retain=2,tit='Flight Chart',xs=siz[0],ys=siz[1]*0.8
  !P.multi=0
  maxlon=max(lon[flt]) & maxlat=max(lat[flt])
  minlon=min(lon[flt]) & minlat=min(lat[flt])
  ;if minlat lt 64 then minlat=64
  if minlon lt -180 then minlon=-180
  mlon=(maxlon+minlon)*0.5
  mlat=(minlat+maxlat)*0.5
  rlon=maxlon-mlon & rlat=maxlat-mlat
  rang=max([rlon,rlat])
  if strcmp(date,'20080412') then map_set,70.,-160,/grid,limit=[68,-170,73,-150],/label else map_set,mlat,mlon,/grid,limit=[minlat,minlon,maxlat,maxlon],/label
  map_continents,/usa,/hires,thick=2
  map_continents,/countries,/hires,/coast,thick=2

  oplot,lon,lat,psym=3
  for l=0,nlabel-1 do begin
    xyouts,lons[l],lats[l],label[l],charsize=1.3,color=255
    plots,lons[l],lats[l],psym=6,color=255
  endfor
  xyouts,mlon-rang*0.9,mlat+rang*0.9,date,charsize=2

  mxa=max(alt)
  ngeo=n_elements(alt)
  utc0=0.
  loadct,27,/silent
  for i=0,ngeo-2 do begin
    cl=fix(alt[i]/mxa*180)
    if cl lt 8 then cl=8
    oplot,lon[i:i+1],lat[i:i+1],color=cl,psym=3
    if utc[i] gt utc0 + 0.25 then begin
      xyouts,lon[i:i+1],lat[i:i+1],strmid(strcompress(string(fix(utc[i]*100)*0.01),/REMOVE_ALL),0,5),color=120
      utc0=utc[i]
    endif
  endfor
  ;p2=tvrd(true=1)
  ;write_png,dir+date+ll+date+'_map.png',p2
endif ; if plot & nav

; write ASCII data in ICARTT format
if not nav then izamax=-1
espo=cfg(cfg_file,'espo')
arch=cfg(cfg_file,'archive')
if strcmp(strmid(espo,0,1),'y',/FOLD_CASE) or strcmp(strmid(arch,0,1),'y',/FOLD_CASE) then begin
  if noaa then begin
    if n_elements(cg4up) ne n_elements(utc) then message,'Cannot archive - CG-4 missing.'
    if n_elements(lat) lt 1 then message,'Cannot archive - NAV files missing.'
    archive_noaa,dir,ll,date,utf,nf,zenlambda,zen,bz1,bz2,nadlambda,nad,bn1,bn2,bb1,bb2,wl,nl,zind,nind,cg4up[flt],cg4dn[flt],comment,status,izamax,platform,satnb
  endif else begin
    archive_nasa,dir,ll,date,utf,nf,zenlambda,zen,bz1,bz2,nadlambda,nad,bn1,bn2,bb1,bb2,wl,nl,zind,nind,comment,status,izamax,platform
  endelse
endif


stop


end
