; used to compare retrieval results to actual
; compares asymmetry parameter (retrieved) to angstrom coefficient (from aats)

pro arctas_rtm_compare
date='20080709'

dir='/home/leblanc/libradtran/output/aero/'
aats_dir='/data/seven/schmidt/polar/nasa/'
; get rtm results
  ; get tau modified
  f=file_search(dir+'rtm_tauapprox_'+date+'_wvl*.txt')
  f=f[sort(f)]
  print, f
    ;lat, lon, ssa, asy, asy2, albedo, correction, tau modification, flux divergence, model down, model up, tau, Ft_up, Ft_dn, Fb_up, Fb_dn
    
  dir='/home/leblanc/arctas/nasa/'+date+'/'
  wvl0353=read_ascii(f[0], data_start=1)
  wvl0380=read_ascii(f[1], data_start=1)
  wvl0452=read_ascii(f[2], data_start=1)
  wvl0499=read_ascii(f[3], data_start=1)
  wvl0519=read_ascii(f[4], data_start=1)
  wvl0605=read_ascii(f[5], data_start=1)
  wvl0675=read_ascii(f[6], data_start=1)
  wvl0779=read_ascii(f[7], data_start=1)
  wvl0864=read_ascii(f[8], data_start=1)
  wvl1019=read_ascii(f[9], data_start=1)
  wvl1241=read_ascii(f[10], data_start=1)
  wvl1558=read_ascii(f[11], data_start=1)
  wvl2139=read_ascii(f[12], data_start=1)

;get errors in retrievals
error=fltarr(6,13)
openr, 95, '/home/leblanc/arctas/nasa/rtm_error.out'
line=' '
readf, 95, line
readf, 95, error
close,/all
error=error*0.01

; need to get utc to compare
nav_nasa,aats_dir,date,aats_dir+'/'+date+'/'+'polar.cfg',utc,alt,lat,lon,dc,sza,iza,lats,lons,label,roll=rol,pitch=pit
ind=0
for i=0, n_elements(wvl0353.field01[0,*])-1 do begin
  m=min(abs(wvl0380.field01[0,i]-lat)+abs(wvl0380.field01[1,i]-lon),mm)
  ind=[ind,mm]
endfor
ind=ind[1:*]

; get aats 
get_aats_all,aats_dir,date,uth,lambda,tauh,cld,dtauh,coef
  if n_elements(uth) lt 1 then print, '*** No AATS file ***'
tau_aats=fltarr(n_elements(utc[ind]),n_elements(lambda))
dtau_aats=fltarr(n_elements(utc[ind]),n_elements(lambda))
cf=fltarr(n_elements(ind),3)


for ju=0, n_elements(utc[ind])-1 do begin
  smm=min(abs(uth/3600.-utc[ind[ju]]),jk)
  cf[ju,*]=coef[jk,*]
  for ji=0,n_elements(lambda)-1 do begin
    tau_aats[ju,ji]=tauh[jk,ji]
    dtau_aats[ju,ji]=dtauh[jk,ji]
  endfor
endfor

; figure out angstrom
alpha=fltarr(n_elements(ind))
beta=fltarr(n_elements(ind))
for i=0, n_elements(ind)-1 do begin
 garg=linfit([499,500,501], cf[i,0]+cf[i,1]*alog([499,500,501])+cf[i,2]*alog([499,500,501])*alog([499,500,501]))
 alpha[i]=-garg[1]
 beta[i]=garg[0]
endfor 

loadct, 39,/silent
; plot comparison
set_plot, 'ps'
   device, /encapsulated
   device, /tt_font, set_font='Helvetica Bold'
   device, filename=dir+'rtm_angs_asy_comp.ps'
   device,/color,bits_per_pixel=8.
   device, xsize=40, ysize=20
  !p.font=1
  !p.thick=5
  !p.charsize=2.5
  !x.style=1
  !y.style=1 
  !z.style=1
  !y.thick=1.8
  !x.thick=1.8
   ;stop
  !p.multi=[0,2,1]
  ind=[2,3,4,5,7,8]
  nn=[1,2,3,4,5]
  wvl=[353,380,452,499,519,605,675,779,864,1019,1241,1558,2139]
  n=0
  i=1 ;asy
  spectrum=[wvl0353.field01[ind[i],n],wvl0380.field01[ind[i],n],wvl0452.field01[ind[i],n],wvl0499.field01[ind[i],n],wvl0519.field01[ind[i],n],wvl0605.field01[ind[i],n],wvl0675.field01[ind[i],n],wvl0779.field01[ind[i],n],wvl0864.field01[ind[i],n],wvl1019.field01[ind[i],n],wvl1241.field01[ind[i],n],wvl1558.field01[ind[i],n],wvl2139.field01[ind[i],n]]
  asy=wvl0499.field01[ind[i],*]
  lats=wvl0499.field01[0,*]
  plot, lats, asy, title='Particle size comparison', xtitle='Latitude', yrange=[0.3,1.0],/nodata, ystyle=8, xmargin=[4,4]
  oplot, lats, asy, color=150
  oplot, lats, asy, color=150, psym=2
  errplot, lats,asy*(1.-error[nn[i],3]), asy*(1.+error[nn[i],3]),color=150
  
  axis, yaxis=1, ytitle='Angstrom Exponent', color=250, yrange=[min(alpha)*1.10, max(alpha)*1.10], ystyle=1, charsize=1.5, /save
  oplot, lats, alpha, color=250
  oplot, lats, alpha, color=250, psym=2
  
  legend,['Asymmetry Parameter', 'Angstrom Exponent'],textcolors=[150,250],box=0
  
  plot, alpha, asy, ytitle='Asymmetry Parameter',xmargin=[8,2], xtitle='Ansgtrom Exponent',psym=2,yrange=[0.0,1.0],xrange=[-0.007,0.022]

  device, /close
  spawn, 'convert '+dir+'rtm_angs_asy_comp.ps '+dir+'rtm_angs_asy_comp.png'
  spawn, 'rm -f '+dir+'rtm_angs_asy_comp.ps'
stop
end
