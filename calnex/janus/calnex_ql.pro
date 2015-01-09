;+
; NAME:
;   calnex_ql
;
; PURPOSE:
;   Quick Look Utility for the CALNEX experiment
;
; CATEGORY:
;   CALNEX / Quick look
;
; CALLING SEQUENCE:
;   calnex_ql
;
; OUTPUT:
;   plot of spectrum  in "SP" .png format  with window
;   plot of temperatures in "temps" .png format with window
;   plot of time series in "QL" .png format with window
;   plot of the time series of the attitude values in "ATT" .png format with window
;   map of the path of the plane using the nav data in "_map.png" format with a window
;   archive file (through archive_p3 procedure)
;   plot of the temperatures and Irradiances from the cg4 in "CG4" .png format with windows
;
; KEYWORDS:
;   SSFR, P3, CALNEX, CG4, Quicklook
;
; DEPENDENCIES:
;   cfg.pro       ;config file cheker
;   nav_p3.pro    ;p3 nav program
;   legend.pro    ;program to make the legend
;   cg4_p3.pro    ;program to get cg4 data from the p3
;   get_cos_simple.pro  ;Program getting the cosine response correction
;   archive_p3.pro      ;Program to archive the data in the NOAA style
;   disp.pro      ;unsure about the utility of this program
;     zensun.pro  ; subprogram needed for get_cos_simple
;     muslope.pro ; subprogram needed for get_cos_simple
;     rph2za.pro  ; subprogram needed
;
; NEEDED FILES:
;   - config file for day
;   - calibspcs.out file for the day or for attitude corrected data calibspcs_attcorr.out
;   - temp.out files for the day
;   - nav.out file for the day (generated from nav_p3.pro)
;
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
; Written:  Sebastian Schmidtc, LASP CU Boulder, date unknown
; Modified: wednesday April 21st, 2010
;          -by Samuel LeBlanc
;           changed calls to correct P3 archiving
;           added comments
;           changed the cg4_p3 call to reflect the correct new outputs
; Modified: Thursday, April 22nd, 2010
;          -by Samuel LeBlanc
;           added plotting routines for cg4 temperatures and irradiances
; Modified: Friday, April 23rd, 2010
;          -by Sebastian Schmidt
;           modified nav reader to smooth pitch/roll data
;           added labels for the CalNex map (locations of airports)
; Modified: Wednesday, Cinco de Mayo (May 5th), 2010
;          -by Samuel LeBlanc
;	    Change color scheme and plotting details, location of legends on pots (for CG4)
; Modified: Saturday, May 8th, 2010
;	    -by Samuel LeBlanc
;	    Added contour plots for spectras (upward and downward)
; Modified: Friday, May 21st, 2010
;           -by Samuel LeBlanc and Sebastian Schmidt
;           Added retrieval output program call
; Modified: Sunday, May 23rd, 2010
;           -by Samuel LeBlanc
;           Added plotting for non nav times
;           Added overplot of ship track on p3 flight track, if available 
;---------------------------------------------------------------------------
;@C:\CalNex\pro\cfg.pro
;@C:\CalNex\pro\disp.pro
;@C:\CalNex\pro\nav_p3.pro
;@C:\CalNex\pro\legend.pro
;@C:\CalNex\pro\cg4_p3.pro
;@C:\CalNex\pro\get_cos_simple.pro
;@C:\CalNex\pro\archive_p3.pro
@cfg.pro
@disp.pro
@nav_p3.pro
@legend.pro
@cg4_p3.pro
@get_cos_simple.pro
@archive_p3.pro
@make_p3_retrieval_input.pro

pro calnex_ql,date_inp
if n_elements(date_inp) lt 1 then begin
  date='20100521'
endif else begin
  date=strcompress(string(date_inp),/REMOVE_ALL)
endelse

if (!VERSION.os ne 'linux') then begin
  ; windows:
  dir='C:\CalNex\p3\'
  ll ='\'
endif else begin
  ; linux:
  dir='/data/seven/schmidt/calnex/p3/'
  ll='/' ; directory separator
endelse

attcorr=0 ; 0: use uncorrected data, 1: use corrected data.

; *******************************************************************************
; get information from configuration file
; *******************************************************************************
; initialize configuration file
cfg_file=dir+ll+date+ll+date+'.cfg'        ; build cfg file
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
; get SHIP NAV data (for map overlay)
; *******************************************************************************
ship_overlay=cfg(cfg_file,'ship_overlay')
if strcmp(strmid(ship_overlay,0,1),'n',/FOLD_CASE) then ship_overlay=0 else ship_overlay=1
if ship_overlay then begin
  print, 'Reading Ship path for overlay'
  ship_path=cfg(cfg_file,'ship_path')
  ship_path=file_search(ship_path+ll+date+ll+date+'_RT.out', count=ship_ct)
  if (ship_ct gt 0) then begin
    restore, ship_path
    lat_ship=lat
    lon_ship=lon
    utc_ship=tmhrs
  endif else begin
    print, 'Ship RT file not found'
    ship_overlay=0
  endelse
endif

; *******************************************************************************
; get spectra
; *******************************************************************************
print,'Restore SSFR data...'
if attcorr then begin
  restore, dir+date+ll+date+'_calibspcs_attcorr.out'
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

lats  =[27.9730,39.9088, 34.05, 34.051,32.685 ]
lons  =[-82.535,-105.117,-118.25, -117.6117, -117.134]
label =['Tampa','Jeffco','Los Angeles','Ontario Airport','San Diego' ];,'Boulder','Ontario','San Diego','San Francisco']
nlabel=n_elements(lats)

; *******************************************************************************
; get NAV data
; *******************************************************************************
nav=cfg(cfg_file,'read_nav')
; get platform
if strcmp(strmid(nav,0,1),'n',/FOLD_CASE) then nav=0 else nav=1
if nav then begin
 if strcmp(platform,'NOAAP3',3,/FOLD_CASE) then begin
   nav_p3,dir,ll,date,cfg_file,utc,alt,lat,lon,dc,sza,iza,pit=pit,rol=rol,tem=t_ambient, heading=hed
 endif else begin
     message,'Wrong platform.'
 endelse
endif else begin
 navf=file_search(dir+ll+date+ll+'nav.out',count=nnf)
 if nnf eq 1 then nav=1 else nav=0
 ship_overlay=0
endelse

; *******************************************************************************
; get CG4 data
; *******************************************************************************
iscg4=cfg(cfg_file,'read_cg4')
if strcmp(strmid(iscg4,0,1),'y',/FOLD_CASE) then iscg4=1 else iscg4=0

if strcmp(platform,'NOAAP3',3,/FOLD_CASE) then begin
  if iscg4 then begin
   cg4_p3,dir+date+ll,date,utc,cg4dn,cg4up,zent,nadt
  endif else print,'Warning --- Skipping CG-4 for now.'
endif

if plot then begin ; define graphics
  device,decomposed=0
  ;loadct, 27
  loadct,39
  tvlct, r,g,b, /get
  r=reverse(r) & g=reverse(g) & b=reverse(b)
  tvlct,r,g,b
  !p.thick=2.0 & !p.charsize=1.2
  !x.style=1 & !y.style=1 
  !y.thick=1.2 & !x.thick=1.2
  !p.color=255 & !p.background=0
endif

;plot temperatures and irradiances for cg4s
if plot and iscg4 then begin
  !p.multi=0
  window, 9, title='CG4 Temperatures'
  fc=where(zent gt -200 and nadt gt -200 and zent lt 45 and nadt lt 45 and cg4dn gt -9000 and cg4up gt -9000)
  if n_elements(utc) ne n_elements(cg4dn) then message,'Something wrong with CG-4 reader.'
  plot, utc[fc], zent[fc], title='CG4 Temperatures',ytitle='Temperatures (Centigrade)',xtitle='UTC [h]',$
  linestyle = 0,xr=uu,/xs, yrange=[-30.0,35.0]
  oplot, utc[fc], nadt[fc], linestyle = 3,color=200
  if nav then oplot, utc[fc],t_ambient[fc],psym=3,color=1
  legend, ['Zenith','Nadir','T_ambient'], linestyle=[0,3,0],color=[255,200,1],textcolor=[255,200,1],/bottom, margin=0.5,thick=[1,1,1]
  p=tvrd(true=1)
  write_png,dir+date+ll+date+'_CG4_temps_'+platform+'.PNG',p

  window, 10, title='CG4 Irradiances'
  plot, utc[fc], cg4dn[fc], title='CG4 Irradiances',ytitle='Irradiance [W m!E-2!N]',xtitle='UTC [h]',$
  linestyle =1,xr=uu,/xs,yrange=[0.,550.],color=255
  oplot, utc[fc], cg4up[fc], linestyle=3,color=200
  legend, ['Downwelling', 'Upwelling'],linestyle=[1,3], color=[255,200],textcolor=[255,200],/bottom, margin=0.5,thick=[1,1]
  p=tvrd(true=1)
  write_png,dir+date+ll+date+'_CG4_Irr_'+platform+'.PNG',p
endif

; *******************************************************************************
; make plots
; *******************************************************************************
if plot then begin
  if uu0 then begin           ; only if time interval defined plot spectra every hour
        !p.multi=[0,1,2]
        window,0,xsize=400,ysize=800,retain=2,tit='Spectra'
        ut0=max([u0,min(utc)])
        count = 0
        num_plots = fix(max(utc)-ut0)+1
        data_times = findgen(num_plots) + ut0
        ymax = 0
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
                textcolor=255
                linestyle=0
                leg      =strmid(tim,0,2) + ':' +  strmid(tim,2,2) + ' UTC'
            endif else begin
                oplot,zenlambda,zspectra[*,ind],color=count*40,linestyle=(count mod 5)*2
                textcolor=[textcolor,count*40]
                linestyle=[linestyle,(count mod 5)*2]
                leg      =[leg,strmid(tim,0,2) + ':' +  strmid(tim,2,2) + ' UTC']
            endelse
            ut0=ut0+1.
            count += 1

        endwhile

        legend,leg,textcolors=textcolor,linestyle=linestyle,/right,colors=textcolor,outline_color=255

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
                textcolor=255
                linestyle=0
                leg      =strmid(tim,0,2) + ':' +  strmid(tim,2,2) + ' UTC'
            endif else begin
                oplot,nadlambda,nspectra[*,ind],color=count*40,linestyle=(count mod 5)*2
                textcolor=[textcolor,count*40]
                linestyle=[linestyle,(count mod 5)*2]
                leg      =[leg,strmid(tim,0,2) + ':' +  strmid(tim,2,2) + ' UTC']
            endelse
            ut0=ut0+1.
            count += 1
        endwhile
        legend,leg,textcolors=textcolor,linestyle=linestyle,/right,colors=textcolor,outline_color=255
        p=tvrd(true=1)
        write_png,dir+date+ll+'SF'+date+'__SP_'+platform+'.PNG',p

        ut0=max([u0,min(utc)])
        !p.multi=0
        ux=where(utc ge ut0,nx)

        xaxis=utc[ux]

  endif ; if uu0
endif

; *******************************************************************************
; Make contour plots (wavelength vs. time vs. irradiance)
; *******************************************************************************
contours=0
if plot then begin
  if contours then begin           ; only if contour is running
    print, 'plotting contour plot'
    !p.multi=[0,2,1]
    set_plot,'ps'
    device, /encapsulated
    device, /tt_font, set_font='Helvetica Bold'
    device, filename=dir+date+ll+'SF'+date+'__contour_'+platform+'.ps'
    device,/color,bits_per_pixel=8
    device, xsize=12, ysize=12
        
    contour, zspectra, zenlambda, utc, tit=platform+' '+date+' SSFR DOWN',xtit='wavelength [nm]',ytit='UTC [h]',xr=[350,2200],/cell_fill, max_value=3.,min_value=0.,/xs,/ys,nlevels=15, color=255, thick=4, position=[0.1,0.1,0.45,0.95]
		
    contour,nspectra,nadlambda, utc, tit='SSFR UP',xtit='wavelength [nm]',ytit='',xr=[350,2200],/cell_fill,/xs,/ys, max_value=3.,min_value=0.,nlevels=15,color=255, thick=4, position=[0.5,0.1,0.93,0.95]
		
    COLORBAR, POSITION=[0.95, 0.10, 0.98, 0.95],/right,/vertical, range=[0.,3.],title='Irradiance [W m!E-2!N nm!E-1!N]',color=255
    device,/close
    spawn,  'convert "'+ dir+date+ll+'SF'+date+'__contour_'+platform+'.ps" "'+dir+date+ll+'SF'+date+'__contour_'+platform+'.png"'
    spawn, 'rm '+dir+date+ll+'SF'+date+'__contour_'+platform+'.ps'
    set_plot, 'x'
    print, 'finished contour plot, can be found at: ', dir+date+ll+'SF'+date+'__contour_'+platform+'.png'
  endif ; if contours
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
window,4,xsize=900,ysize=700,retain=2,tit='Temperatures'
  !P.multi=[0,2,2]
  xr=[u0,u1]
  hot=where(nsitemp gt 100 or zsitemp gt 100,nhot)
  if nhot gt 0 then begin
    nsitemp[hot]=0.
    zsitemp[hot]=0.
  endif
  plot,utcd,bxtemp,tit=platform+' '+date+' computer temperature',xtit='UTC [h]',ytit='T [C]',psym=3,xr=xr,/xstyle,yr=[10,40]
  oplot,utcd,zirxt,color=5
  oplot,utcd,nirxt,color=200
  legend,['Box','Zenith NIR block','Nadir NIR block'],textcolors=[255,5,200]
  plot,utcd,bxcltemp,xtit='UTC [h]',ytit='T [C]',psym=3,xr=xr,/xstyle
  legend,['Ambient Cabin'],textcolors=[255]
  plot,utcd,nsitemp,tit='Silicon temperatures',xtit='UTC [h]',ytit='T [C]',psym=3,xr=xr,yr=[20,max(nsitemp)+3],/xs
  oplot,utcd,zsitemp,color=5,psym=3
  legend,['Nadir','Zenith'],textcolors=[255,5]
  plot,utcd,nirtemp,tit='InGaAs temperature',xtit='UTC [h]',ytit='T [C]',psym=3,yr=[min([min(nirtemp),min(zirtemp)]),0],xr=xr,/xs
  oplot,utcd,zirtemp,color=5,psym=3
  legend,['Nadir','Zenith'],textcolors=[255,5]
  p=tvrd(true=1)
  write_png,dir+date+ll+date+'_temps.png',p
endif
endif

; *******************************************************************************
; get cosine correction
; *******************************************************************************
wl=cfg(cfg_file,'wlql')
wl=strsplit(wl,' ,',escape='#',/EXTRACT)
wl=float(wl)
nl=n_elements(wl)
if nav then get_cos,dir,date,cfg_file,utc,alt,sza,dc,zenlambda,fac,dz,nadlambda,facn,dn

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
  if attcorr then fac=fac-fac+1. ; if attitude already corrected in original data, don't use fac
  plot,utf,zen[0,*]*fac[flt],xr=uu,yr=[0,2.0],xtitle='UTC [h]',ytitle='Irradiance [W m!E-2!N nm!E-1!N]',psym=3,charsize=cs,title=platform+' '+date+' SSFR DOWN ',/xstyle
;oplot, utc[flt],fac[flt], color=10, psym=3
  outtxt=['wavelength [nm]',strtrim(string(zenlambda[zind[0]]),2)]
  outclr=[254              ,254]
  for l=1,nl-1 do begin
    oplot,utf,zen[l,*]*fac[flt],color=40*l,psym=3
    outtxt=[outtxt,strtrim(string(zenlambda[zind[l]]),2)]
    outclr=[outclr,40*l]
  endfor
  legend,outtxt,textcolors=outclr, /bottom_legend, charsize=1.5

  print,'DN=',dn
  plot,utf,nad[0,*]*dn/(zen[0,*]*fac[flt]),xr=uu,yr=[0.,1.1],xtitle='UTC [h]',ytitle='Albedo',psym=3,charsize=cs,/xstyle
  for l=1,nl-1 do begin
    oplot,utf,nad[l,*]*dn/(zen[l,*]*fac[flt]),color=40*l,psym=3
  endfor

  plot,utc,cos(!pi/180.*sza),xr=uu,yr=[0,1.25],xtitle='UTC [h]',ytitle='cos(SSFR<->SZA) cos(SZA)',charsize=cs,title='SZA and ATTITUDE',/ystyle,/xstyle
  oplot,utc,dc,psym=3,color=20
  oplot,utf,dc[flt],psym=3,color=200
  if attcorr then oplot,utf,fac[flt],psym=3
  str=strtrim(string(fix(izamax+0.5)),2)

  outtxt=['cos(SZA)','cos(SZA<->SSFR)','cos(SZA<->SSFR<'+str+'deg)','pitch offset='+cfg(cfg_file,'pitchoff'),'roll offset='+cfg(cfg_file,'rolloff')]
  outclr=[255       ,20               ,200                         ,255,255]
  legend,outtxt,textcolors=outclr,charsize=1.5, /bottom_legend

  plot,utc,alt*0.001,xr=uu,yr=[0,max(alt*0.001)],xtitle='UTC [h]',ytitle='Altitude [km] ',charsize=cs,title='ALTITUDE',psym=3,/xstyle

  ngeo = n_elements(alt)
  mxa  = max(alt)
  for i=0,ngeo-2 do begin
    cl=fix(alt[i]/mxa*180)
    oplot,utc[i:i+1],alt[i:i+1]*0.001,color=cl,psym=3
  endfor

  p1=tvrd(true=1)
  write_png,dir+date+ll+'SF'+date+'__QL_'+platform+'.PNG',p1


  window,6,tit='Attitude',ysize=600
  cs=2
  !P.multi=[0,1,2]
  plot,utc,pit,psym=3,tit=date+' '+platform+' pitch',yr=[-4,10],xtit='UTC [h]',ytit='Pitch [deg]',xr=uu,charsize=cs,/xs
  plot,utc,rol,psym=3,tit=date+' '+platform+' roll',yr=[-15,15],xtit='UTC [h]',ytit='Roll [deg]',xr=uu,charsize=cs,/xs
  ;if n_elements(sat) eq 0 then begin
  ;  plot,utf,zen[0,*]*fac[flt],tit='downward irradiance',xtit='UTC [h]',ytit='F_dn [W m!E-2!N nm!E-1!N]',xr=uu,psym=3,charsize=cs,yr=[min(zen[0,*])*0.8,max(zen[0,*])*1.2],/ys
  ;  oplot,utf,zen[0,*],psym=3,color=120
  ;endif else begin
  ;  plot,utc,sat,ytit='Saturation # Zenith',chars=cs,xr=uu
  ;endelse


  ;legend,['corrected','uncorrected'],textcolors=[255,120]
  p1=tvrd(true=1)
  write_png,dir+date+ll+'SF'+date+'__ATT_'+platform+'.PNG',p1

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
                                ;if strcmp(date,'20100420') then
                                ;map_set,30.,-80,/grid,limit=[20,-90,40,-70],/label else 
  map_set,mlat,mlon,/grid,limit=[minlat-0.5,minlon-0.5,maxlat+0.5,maxlon+0.5],/label
  map_continents,/usa,/hires,thick=2
  ;map_continents,/countries,/hires,/coast,thick=2

  ;loadct,39,/silent
  ;plotting of ship track
  if ship_overlay then begin
    oplot, lon_ship, lat_ship, psym=1, color=250
    ngeo_ship=n_elements(lat_ship)
    utc0_ship=0.
    for i=0L, ngeo_ship-2L do begin
      if utc_ship[i] gt utc0_ship+1.0 then begin
        xyouts, lon_ship[i:i+1],lat_ship[i:i+1], strmid(strcompress(string(fix(utc_ship[i]*100)*0.01),/remove_all),0,5),color=250
        utc0_ship=utc_ship[i]
      endif
    endfor
  endif ;ship overlay

  ; plotting of flight track
  oplot,lon,lat,psym=3
  for l=0,nlabel-1 do begin
    xyouts,lons[l],lats[l],label[l],charsize=1.3,color=255
    plots,lons[l],lats[l],psym=6,color=255
  endfor
  xyouts,mlon-rang*0.9,mlat+rang*0.9,date,charsize=2

  mxa=max(alt)
  ngeo=n_elements(alt)
  utc0=0.
  
  for i=0,ngeo-2 do begin
    cl=fix(alt[i]/mxa*180)
    if cl lt 8 then cl=8
    oplot,lon[i:i+1],lat[i:i+1],color=cl,psym=3
    if utc[i] gt utc0 + 0.25 then begin
      xyouts,lon[i:i+1],lat[i:i+1],strmid(strcompress(string(fix(utc[i]*100)*0.01),/REMOVE_ALL),0,5),color=120
      utc0=utc[i]
    endif
  endfor
  
  p2=tvrd(true=1)
  write_png,dir+date+ll+date+'_map.png',p2
endif ; if plot & nav

if plot and not nav then begin
  siz=get_screen_size()
  window,3,xs=siz[0],ys=siz[1]*0.4,retain=2,tit='Quicklook'
  cs=1.5
  !p.multi=[0,2,1]
  x0=min(utf) & x1=max(utf) & legendx=max(utf)+0.2
  plot,utf,zen[0,*],xr=uu,yr=[0,2.0],xtitle='UTC [h]',ytitle='Irradiance [W m!E-2!N nm!E-1!N]',psym=3,charsize=cs,title=platform+' '+date+' SSFR DOWN ',/xstyle
  oplot, utf
  outtxt=['wavelength [nm]',strtrim(string(zenlambda[zind[0]]),2)]
  outclr=[254              ,254]
  for l=1,nl-1 do begin
    oplot,utf,zen[l,*],color=40*l,psym=3
    outtxt=[outtxt,strtrim(string(zenlambda[zind[l]]),2)]
    outclr=[outclr,40*l]
  endfor
  legend,outtxt,textcolors=outclr, /bottom_legend, charsize=1.5

  plot,utf,nad[0,*],xr=uu,yr=[0.,1.1],xtitle='UTC [h]',ytitle='Irradiance [W m!E-2!N nm!E-1!N]',psym=3,charsize=cs,title=platform+' '+date+' SSFR UP ',/xstyle
  for l=1,nl-1 do begin
    oplot,utf,nad[l,*],color=40*l,psym=3
  endfor
  p1=tvrd(true=1)
  write_png,dir+date+ll+'SF'+date+'_QL_'+platform+'.PNG',p1
endif

; *******************************************************************************
; write ASCII data in ICARTT format
; *******************************************************************************
if not nav then izamax=-1
espo=cfg(cfg_file,'espo')
arch=cfg(cfg_file,'archive')
if strcmp(strmid(espo,0,1),'y',/FOLD_CASE) or strcmp(strmid(arch,0,1),'y',/FOLD_CASE) then begin
  if noaa then begin
    if n_elements(cg4up) ne n_elements(utc) then message,'Cannot archive - CG-4 missing.'
    if n_elements(lat) lt 1 then message,'Cannot archive - NAV files missing.'
    archive_p3,dir,ll,date,utf,nf,zenlambda,zen,bz1,bz2,nadlambda,nad,bn1,bn2,bb1,bb2,wl,nl,zind,nind,cg4up[flt],cg4dn[flt],comment,status,izamax,platform,satnb
  endif else begin
    message,'Wrong platform.'
  endelse
endif

if (!VERSION.os eq 'linux') then begin
png=file_search(dir+date+ll+'*.png',count=cpng,/FOLD_CASE)
if cpng gt 0 then begin
for i=0,cpng-1 do begin
  spawn,'chmod a+w "'+png[i]+'"'
  if strlen(png[i]) ne strlen(strcompress(png[i],/REMOVE_ALL)) then begin
  spawn,'"mv" "'+png[i]+'" '+strcompress(png[i],/REMOVE_ALL)
  endif
  ;print,png[i]
endfor
endif
ict=file_search(dir+date+ll+'*.ict',count=cict,/FOLD_CASE)
if cict gt 0 then begin
for i=0,cict-1 do begin
  spawn,'chmod a+w "'+ict[i]+'"'
  if strlen(ict[i]) ne strlen(strcompress(ict[i],/REMOVE_ALL)) then begin
  spawn,'"mv" "'+ict[i]+'" '+strcompress(ict[i],/REMOVE_ALL)
  endif
endfor
endif
endif
;stop
; *******************************************************************************
; Extract data for nadir-irradiance-based retrievals
; *******************************************************************************
if nav then begin
  make_p3_retrieval_input,utc,nadlambda,nspectra,zenlambda,zspectra,sza,alt,lat,lon,dir+ll+date+ll+date,date
endif

save, alt, lat, lon, nadlambda, zenlambda, nspectra, zspectra, sza, utc, fac, dz, facn, dn,rol,pit,hed, filename=dir+ll+date+ll+date+'_SP.out'
print, 'saved to : '+dir+ll+date+ll+date+'_SP.out'

if strcmp(date,'20100504') then begin
  fl0=where(utc ge fdutc[0] and utc le fdutc[1] and lon ge -118.15 and lon lt -117.9)
  window,15,tit='Profile'
  plot,lon[fl0],alt[fl0],psym=3,xtit='Longitude [deg]',ytit='Altitude [km]'
  window,16,tit='Absorption'
  fl1=where(alt[fl0] lt 800)
  fl2=where(alt[fl0] gt 800 and alt[fl0] lt 1300)
  fl3=where(alt[fl0] gt 1300 and alt[fl0] lt 2000)
  fl4=where(alt[fl0] gt 2000 and alt[fl0] lt 3000)
  fl5=where(alt[fl0] gt 3500)
  mm=min(abs(zenlambda-500),z500)
  mm=min(abs(nadlambda-500),n500)
  l1=lon[fl0[fl1]]
  z1=zspectra[z500,fl0[fl1]]
  n1=nspectra[n500,fl0[fl1]]
  l2=lon[fl0[fl2]]
  z2=zspectra[z500,fl0[fl2]]
  n2=nspectra[n500,fl0[fl2]]
  l4=lon[fl0[fl4]]
  z4=zspectra[z500,fl0[fl4]]
  n4=nspectra[n500,fl0[fl4]]

  mm=min(abs(lon[fl0[fl1]]+118.1261),i1)
  zs1=zspectra[*,fl0[fl1[i1]]]
  ns1=nspectra[*,fl0[fl1[i1]]]
  mm=min(abs(lon[fl0[fl4]]+118.1261),i4)
  zs4=zspectra[*,fl0[fl4[i4]]]
  ns4=nspectra[*,fl0[fl4[i4]]]
  plot,zenlambda,(zs4-ns4)-(zs1-ns1),yr=[0,0.4]


  fl0=where(utc ge 19.9 and utc le 20.5 and lon ge -117.6 and lon lt -117.2)
  ;window,15,tit='Profile'
  ;plot,lon[fl0],alt[fl0],psym=3,xtit='Longitude [deg]',ytit='Altitude [km]'
  fl1=where(alt[fl0] lt 1000)
  fl4=where(alt[fl0] gt 3500)
  mm=min(abs(lon[fl0[fl1]]+117.4),i1)
  zs1=zspectra[*,fl0[fl1[i1]]]
  ns1=nspectra[*,fl0[fl1[i1]]]
  mm=min(abs(lon[fl0[fl1]]+117.4),i4)
  zs4=zspectra[*,fl0[fl4[i4]]]
  ns4=nspectra[*,fl0[fl4[i4]]]
  oplot,zenlambda,(zs4-ns4)-(zs1-ns1),color=100


  stop
endif


if strcmp(date,'20100512') then begin
  fl0=where(utc ge 19.2 and utc le 20.4 and lon gt -122.6 and lon lt -122.45) ; 20.4
  window,15,tit='Profile',xsize=950
  !P.multi=[0,3,1]
  cs=1.5
  plot,lat[fl0],alt[fl0],psym=3,xtit='Latitude [deg]',ytit='Altitude [km]',chars=cs
  plot,lat[fl0],zspectra[100,fl0],psym=3,xtit='Longitude [deg]',chars=cs
  plot,lon[fl0],lat[fl0],psym=3,xtit='Longitude [deg]',ytit='Latitude [deg]',yr=[36,38],chars=cs
  fl1=where(alt[fl0] gt 250 and alt[fl0] lt 350)
  mm =min(abs(lon[fl0[fl1]]-36.3),i1)
  fl2=where(alt[fl0] gt 480 and alt[fl0] lt 600)
  mm =min(abs(lon[fl0[fl2]]-36.3),i2)


  window,16,tit='Time Series'
  !P.multi=0
  plot ,utc[fl0],alt[fl0],yr=[0,1500]
  oplot,utc[fl0],zspectra[100,fl0]*1000.,psym=1,color=100
  oplot,utc[fl0],nspectra[100,fl0]*1000.,psym=1,color=120
  oplot,utc[fl0],(lat[fl0]-mean(lat[fl0])+0.4)*1000.,color=70

  window,17,tit='Histograms'

  fl1=where(utc ge 19.4 and utc le 19.5,nfl1)
  below=fltarr(411)
  for l=0,410 do begin
    below[l]=median(zspectra[l,fl1]-nspectra[l,fl1])
  endfor

  fl2=where(utc ge 19.55 and utc le 19.6,nfl2)
  above=fltarr(411)
  up   =fltarr(411)
  dn   =fltarr(411)
  for l=0,410 do begin
    above[l]=median(zspectra[l,fl2]-nspectra[l,fl2])
    up   [l]=median(nspectra[l,fl2])
    dn   [l]=median(zspectra[l,fl2])
  endfor

  plot,nadlambda,up
  oplot,zenlambda,dn,color=70

  mm=min(abs(utc-19.6),i0)
  mm=min(abs(utc-19.85),i1)

  plot,zenlambda,zspectra[*,i1]/zspectra[*,i0],yr=[0,1]
  oplot,zenlambda,zspectra[*,i1],color=120

  ;histogram,zspectra[100,fl1],bin,his
  ;oplot,bin,his


  stop
endif


if strcmp(date,'20100514') then begin
  !P.multi=0
  fl0=where(utc gt 17.8 and utc lt 19.9)
  plot,lat[fl0],alt[fl0]
  fl1=where(alt[fl0] gt 800 and alt[fl0] lt 1200)
  stop
endif

if strcmp(date,'20100516') then begin
  ;flt=where(utc gt 19.3 and utc lt 19.5)
  flt=where(utc gt 18.3 and utc lt 21.5 and lat gt 33.05 and lat lt 33.45 and lon gt -119.4 and lon lt -118.75 and alt gt 300)
  lonf=lon[flt]
  latf=lat[flt]
  utcf=utc[flt]
  szaf=sza[flt]
  altf=alt[flt]
  nadf=nspectra[*,flt]
  save,utcf,lonf,latf,altf,szaf,nadf,nadlambda,file='C:\CalNex\p3\20100516\16.out'
  window,14,tit='Gradient'
  !P.multi=0
  plot,lon[flt],smooth(nspectra[154,flt],10),psym=3,tit='Upward Irradiance @ 865 nm',xtit='Longitude [deg]',ytit='Irradiance [W m!E-2!N nm!E-1!N]'
  p=tvrd(true=1)
  write_png,dir+date+ll+'SF'+date+'_x_'+platform+'.PNG',p
endif


if strcmp(date, '20100608') or strcmp(date, '20100610') then begin

  if uu0 then begin           ; only if time interval defined plot spectra every hour
        !p.multi=[0,1,2]
        
        loadct, 33
        tvlct, r,g,b, /get
        r[0]=255 & g[0]=255 & b[0]=255
        r[255]=0 & g[255]=0 & b[255]=0
        tvlct,r,g,b
        !p.charsize=1.8
        
        window,9,xsize=1000,ysize=1000,retain=2,tit='Albedo'
        ut0=max([u0,min(utc)])
        count = 0
        num_plots = (fix(max(utc)-ut0))*4+1
        data_times = findgen(num_plots)/4. + ut0
        linestyle=0

        ut0=max([u0,min(utc)])
        while ut0 lt max(utc) do begin
            mm=min(abs(ut0-utc),ind)
            ind=ind[0]
            hh=fix(utc[ind]) & mm=fix( (utc[ind]-fix(utc[ind]))*60.+0.5)
            tim=strcompress(string(hh,format='(i2.2)'),/remove_all)+strcompress(string(mm,format='(i2.2)'),/remove_all)

            nsp=interpol(nspectra[*,ind],nadlambda,zenlambda)
            if alt[ind] lt 200.0 then begin
            if(count eq 0) then begin
                plot,zenlambda,nsp*dn/zspectra[*,ind]*fac[ind],tit=platform+' '+date+' SSFR Albedo',xtit='wavelength [nm]',ytit='Albedo',xr=[350,1300],yr=[0,0.2],color=255,linestyle=0,/xs,/ys
                textcolor=255
                linestyle=0
                leg      =strmid(tim,0,2) + ':' +  strmid(tim,2,2) + ' UTC'
            endif else begin
                oplot,zenlambda,nsp*dn/zspectra[*,ind]*fac[ind],color=count*25,linestyle=(count mod 6)
                textcolor=[textcolor,count*25]
                linestyle=[linestyle,(count mod 6)]
                leg      =[leg,strmid(tim,0,2) + ':' +  strmid(tim,2,2) + ' UTC']
            endelse
           
            count += 1
            endif
            ut0=ut0+0.25
        endwhile

        

        ut0=max([u0,min(utc)])
        count=0
        while ut0 lt max(utc) do begin
            mm=min(abs(ut0-utc),ind)
            ind=ind[0]
            hh=fix(utc[ind]) & mm=fix( (utc[ind]-fix(utc[ind]))*60.+0.5)
            tim=strcompress(string(hh,format='(i2.2)'),/remove_all)+strcompress(string(mm,format='(i2.2)'),/remove_all)

            if alt[ind] lt 200.0 then begin
            if(count eq 0) then begin
                plot,zenlambda,zspectra[*,ind]*fac[ind],tit='SSFR Downwelling',xtit='wavelength [nm]',ytit='Irradiance [W m!E-2!N nm!E-1!N]',xr=[350,1300],yr=[0,1.8],color=255,linestyle=0,/xs,/ys
                textcolor=255
                linestyle=0
                leg      =strmid(tim,0,2) + ':' +  strmid(tim,2,2) + ' UTC'
            endif else begin
                oplot,zenlambda,zspectra[*,ind]*fac[ind],color=count*25,linestyle=(count mod 6)
                textcolor=[textcolor,count*25]
                linestyle=[linestyle,(count mod 6)]
                leg      =[leg,strmid(tim,0,2) + ':' +  strmid(tim,2,2) + ' UTC']
            endelse
           
            count += 1
            endif
            ut0=ut0+0.25
        endwhile
        legend,leg,textcolors=textcolor,linestyle=linestyle,/right,colors=textcolor,outline_color=255
        
        p=tvrd(true=1)
        write_png,dir+date+ll+date+'_albedo_'+platform+'.png',p

        ut0=max([u0,min(utc)])
        ux=where(utc ge ut0,nx)

        xaxis=utc[ux]

  endif ; if uu0

endif

stop
end

pro histogram ,ts ,bin,hiss
bin=indgen(40)*0.05
hiss=fltarr(40)
nfl1=n_elements(ts)
for n=0,nfl1-1 do begin
    mm=min(abs(ts[n]-bin),bin0)
    hiss[bin0]=hiss[bin0]+1
endfor
return
end
