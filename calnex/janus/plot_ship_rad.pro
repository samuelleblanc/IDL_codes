; routine to plot calnex ship radiances, to check for developping an aerosol retrieval
@zensun.pro
pro plot_ship_rad

restore, '/data/seven/schmidt/calnex/ship/20100522/20100522_calibspcs.out'
print, 'restored data'
;nad - radiance, zen - irradiance

;make tmhrs 24 +
kk=max(tmhrs, ll)
tmhrs_p=tmhrs
tmhrs_p=[tmhrs_p[0:ll], tmhrs_p[ll+1:*]+24.]

  ;get kurucz
  kurucz=read_ascii('/home/leblanc/libradtran/libRadtran-1.5-beta/data/solar_flux/kurudz_1.0nm.dat', comment_symbol='#')
  ; interpol to slit function
  I_nadk=interpol(kurucz.field1[1,*]/1000.,kurucz.field1[0,*],nadlambda)
  I_zenk=interpol(kurucz.field1[1,*]/1000.,kurucz.field1[0,*],zenlambda)
  print, 'kurucz gotten'
  
  ;get modeled radiance without aerosol
  restore, '/home/leblanc/CALNEX/ship/20100522/model_sza.out'
  ;interpol to slit function
  I_nad=fltarr(15,n_elements(nadlambda))
  for i=0, 14 do I_nad[i,*]=interpol(rad[i,*],wvls,nadlambda)

  ; get mu
  date='20100522'
  doy=julday(fix(strmid(date,4,2)),fix(strmid(date,6,2)),fix(strmid(date,0,4)))-julday(1,0,fix(strmid(date,0,4)))
  zensun,doy, tmhrs_p, lat,lon ,sza, azimuth, solfac
  mu=1./cos(sza*!DtoR)
  
  dir='/home/leblanc/CALNEX/ship/20100522/' ;png dir
  ;plot all stuff over 18UTC
  kk=where(tmhrs eq 18.)
    
    set_plot, 'ps'
  loadct, 39,/silent
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8.
  device, filename=dir+'ship_rad.ps'
  device, xsize=60, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=1.8 & !x.style=1
  !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !x.margin=[6,3] & !y.margin=[3,2]
  !p.multi=[0,3,1]
  nadlambda=nadlambda/nadlambda[60]
  I_nadt=I_nad[0,*];I_nadt=(mu[kk[0]]*I_nad[0,*]);I_nadt=(mu[kk[0]]*I_nad)
  plot, alog(nadlambda), reform(alog(nspectra[*,kk[0]]*!PI/I_nadt)), title='Radiance for 20100522 normalized by clear-sky modeled radiance', $
    ytitle='Log(radiance)', xtitle='Log(wavelength)', yrange=[-10.,0.5]
  sl=fltarr(15)
  it=fltarr(15)
  cl=intarr(15)
  ll=strarr(15)
  szas=fltarr(15)
  cl[0]=0
      a=linfit(alog(nadlambda[45:80]), reform(alog(nspectra[45:80,kk[0]]*!PI/I_nadt[45:80])))
    sl[0]=a[1] & it[0]=a[0]
    ll[0]=string(tmhrs_p[kk[0]], format='(F7.4)')+' UTC, sl:'+string(sl[0], format='(F6.3)')+', in:'+string(it[0],format='(F6.3)')
  for i=1, 14 do begin
    j=i*1800+kk[0] 
    cl[i]=i*255/15
    szas[i]=sza[j]
    I_nadt=I_nad[i,*];I_nadt=(mu[j]*I_nad[i,*])
    a=linfit(alog(nadlambda[45:80]), reform(alog(nspectra[45:80,j]*!PI/I_nadt[45:80])))
    sl[i]=a[1] & it[i]=a[0]
    ll[i]=string(tmhrs_p[j], format='(F7.4)')+' UTC, sl:'+string(sl[i], format='(F6.3)')+', in:'+string(it[i],format='(F6.3)')
    print, tmhrs_p[j], '/', tmhrs_p[n_elements(tmhrs_p)-1]
    oplot, alog(nadlambda), reform(alog(nspectra[*,j]*!PI/I_nadt)), color=cl[i]
    print, 'sza:',szas[i]
  endfor   
 ;   oplot, alog(nadlambda),alog(nadlambda^(-4.)*0.01),linestyle=2 
    legend, ll,textcolors=cl, box=0, position=[0.35,0.95],/normal

  plot, [1,1],[1,1]
  plot,tmhrs_p[kk[0]:*],reform(nspectra[60,kk[0]:*]*!PI/(mu[*]*I_nadk[60])),title='time series of Transmittance at 550 nm', xtitle='UTC (hours)', ytitle='Transmittance'
  device,/close
  spawn, 'convert '+dir+'ship_rad.ps '+dir+'ship_rad_sza.png'
  print, 'file made: '+dir+'ship_rad_'+string(i-kk[0], format='(I05)')+'.png'
  spawn, 'rm -f '+dir+'ship_rad.ps'
  ;spawn, 'mogrify -format jpg -quality 90 "'+dir+'ship_rad_'+string(i-kk[0], format='(I04)')+'.png"'

  ;spawn, 'ffmpeg -f image2 -i ship_rad1_%05d.png -r 20 -vcodec msmpeg4v2 video.avi'
stop
end
