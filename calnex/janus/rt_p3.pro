pro rt_p3,date_inp

if n_elements(date_inp) lt 1 then begin
  date='20100516'
endif else begin
  date=strcompress(string(date_inp),/REMOVE_ALL)
endelse

file='/data/seven/schmidt/calnex/p3/'+date+'/'+date+'_RI.out'
restore,file

;szaf=sza
nf=n_elements(szaf)

tauf=fltarr(nf)
reff=fltarr(nf)
surface_albedo=surf_albedo

tau=cld[0] & ref=cld[1] & altc=cld[2] & thick=cld[3]
tau_start=30 & ref_start=10

for j=85,nf-1 do begin
  print,j,'/',nf-1
  nadi=nad[j,*]*1000.*0.93 ;diffuse cosine correction
  print,'albedo',nadi/(zen[j,*]*1000*0.93)
  alti=altf[j]
  alti=3000.

  if tau lt 0.01 or ref lt 0.01 then begin
    tau=tau_start
    ref=ref_start
  endif
  tau0=tau
  ref0=ref
  cont=1
  counter=0
  while cont do begin
    wlrange        =[rt_wl[0] , rt_wl[0]]
    cld =[tau,ref,altc,thick]   ; tau,Reff,cloud_height_km; cloud_thickness_km
    albedo=surface_albedo[1]
    rt,doy,date,alti,albedo,szaf[j],cld,wlrange,wlout,dd,df,up,rd,ru
    print,nadi[0],up[1],ref, tau

    tau=((nadi[0]/up[1])^2)*tau

    wlrange        =[rt_wl[1],rt_wl[1]]
    cld =[tau,ref,altc,thick]   ; tau,Reff,cloud_height_km; cloud_thickness_km
    albedo=surface_albedo[1]
    rt,doy,date,alti,albedo,szaf[j],cld,wlrange,wlout,dd,df,up,rd,ru
    ref=((up[1]/nadi[1])^2)*ref
    
    print, ref,tau
 
    if abs(ref/ref0-1) lt 0.03 and abs(tau/tau0-1) lt 0.03 then cont=0
    if ref lt 3.0 or ref gt 55.0 then begin
      cont=0
      ref=0.0
      tau=0.0
    endif
    tau0=tau
    ref0=ref
    
    counter+=1
    if counter gt 20 then begin
      tau=0
      ref=0
      cont=0
    endif
  endwhile

  print,utcf[j],tau,ref
  tauf[j]=tau
  reff[j]=ref
; stop
endfor

file='/data/seven/schmidt/calnex/p3/'+date+'/'+date+'_RI_processed.out'
save,utcf,lonf,latf,altf,szaf,nadf,nadlambda,tauf,reff,file=file

stop
end



pro rt,doy,date,alt,surface_albedo,sza,cld,wlrange,wlout,dd,df,up,rd,ru

wlr=wlrange
zout=[0,alt/1000.,100] ; RTM calculations
;print,zout


if n_elements(cld) gt 0 then begin
  lwp=2./3.*cld[1]*cld[0]
  lwc=lwp/cld[3]*0.001
  ref=cld[1]
  openw ,uc,'/data/seven/schmidt/calnex/rt/cld.dat',/get_lun
  printf,uc,cld[2]+0.5*cld[3],0  ,0
  printf,uc,cld[2]  ,         lwc,ref
  printf,uc,cld[2]-0.5*cld[3],0  ,0
  free_lun,uc
endif


openw ,ui,'/data/seven/schmidt/calnex/rt/'+date+'_vis.inp',/get_lun
printf,ui,'atmosphere_file    /data/seven/schmidt/RTM/libRadtran-1.3/data/atmmod/afglus.dat'
printf,ui,'data_files_path    /data/seven/schmidt/RTM/libRadtran-1.3/data/'
printf,ui,'solar_file         /data/seven/schmidt/RTM/libRadtran-1.3/data/solar_flux/kurudz_1.0nm.dat'
printf,ui,'day_of_year '+string(doy)
printf,ui,'albedo '+string(surface_albedo)
printf,ui,'sza '+string(sza)
printf,ui,'umu -1 1 # -1: downward, +1 upward radiance'
printf,ui,'phi  0.  # Azimuth angle instrument'
printf,ui,'rte_solver disort2'
if n_elements(cld) gt 0 then begin
  printf,ui,'wc_file /data/seven/schmidt/calnex/rt/cld.dat'
  printf,ui,'wc_layer'
endif
printf,ui,'nstr  16'
printf,ui,'wvn '+string(wlr[0])+' '+string(wlr[1])
printf,ui,'slit_function_file /data/seven/schmidt/petra/rt/vis_1nm.dat' ;    #(Si spectrometer)
printf,ui,'correlated_k SBDART'
printf,ui,'zout '+strjoin(string(zout))
printf,ui,'quiet'
free_lun,ui

spawn,'/data/seven/schmidt/RTM/libRadtran-1.3/tools/uvspec < '+'/data/seven/schmidt/calnex/rt/'+date+'_vis.inp '+'> /data/seven/schmidt/calnex/rt/'+date+'_vis.out'
openr,uo,'/data/seven/schmidt/calnex/rt/'+date+'_vis.out',/get_lun
nl = 0
nz = n_elements(zout)
str= ''
ll=fltarr(2000)
dd=fltarr(2000,nz)
df=fltarr(2000,nz)
up=fltarr(2000,nz)
rd=fltarr(2000,nz)
ru=fltarr(2000,nz)
while not eof(ui) do begin
  for z=0,nz-1 do begin
    readf,uo,wl0,dd0,df0,up0
    ll[nl]  =wl0
    dd[nl,z]=dd0
    df[nl,z]=df0
    up[nl,z]=up0
    readf,uo,str
    readf,uo,d0,rd0
    rd[nl,z]=rd0
    readf,uo,d1,ru0
    ru[nl,z]=ru0
  endfor
  nl=nl+1
endwhile
free_lun,uo
ll=ll[0:nl-1]
dd=dd[0:nl-1,*]
df=df[0:nl-1,*]
up=up[0:nl-1,*]
rd=rd[0:nl-1,*]
ru=ru[0:nl-1,*]
wlout=ll

;z=2 - TOA
;z=0 - SUR

;NIR
;#wavelength 951 2153     # Wavelength range [nm]
;#printf,ui#slit_function_file /data/seven/schmidt/petra/rt/nir_1nm.dat  #(InGaAs spectrometer)

end
