pro cloud_rt
device,decomposed=0
loadct,27

doy            = 136 ; 16 May
surface_albedo = 0.03
sza            = 30
alt            = 3
thick          = 0.8

;287 (865)
;76 (1600)
;{10,5}

nad=[287,76]

tau=50
ref=10
tau0=tau
ref0=ref
cont=1
while cont do begin

wlrange        =[865 , 865]
cld =[tau,ref,alt,thick]   ; tau,Reff,cloud_height_km; cloud_thickness_km
rt,doy,surface_albedo,sza,cld,wlrange,wlout,dd,df,up,rd,ru
tau=((nad[0]/up[1])^2)*tau

wlrange        =[1600,1600]
cld =[tau,ref,alt,thick]   ; tau,Reff,cloud_height_km; cloud_thickness_km
rt,doy,surface_albedo,sza,cld,wlrange,wlout,dd,df,up,rd,ru
ref=((up[1]/nad[1])^2)*ref

if abs(ref/ref0-1) lt 0.03 and abs(tau/tau0-1) lt 0.03 then cont=0
print,tau,ref

tau0=tau
ref0=ref

endwhile

stop
end

pro rt,doy,surface_albedo,sza,cld,wlrange,wlout,dd,df,up,rd,ru





wlr=wlrange
zout=[0,5,100] ; RTM calculations

if n_elements(cld) gt 0 then begin
  lwp=2./3.*cld[1]*cld[0]
  lwc=lwp/cld[3]*0.001
  ;print,'LWC=',lwc
  ;print,'alt=',cld[3]
  ref=cld[1]
  openw ,uc,'/data/seven/schmidt/calnex/rt/cld.dat',/get_lun
  printf,uc,cld[2]+0.5*cld[3],0  ,0
  printf,uc,cld[2]  ,         lwc,ref
  printf,uc,cld[2]-0.5*cld[3],0  ,0
  free_lun,uc
endif


openw ,ui,'/data/seven/schmidt/calnex/rt/vis.inp',/get_lun
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
  ;printf,ui,'wc_properties mie'
  ;printf,ui,'wc_properties_interpolate'
endif
printf,ui,'nstr  16'
printf,ui,'wvn '+string(wlr[0])+' '+string(wlr[1])
printf,ui,'slit_function_file /data/seven/schmidt/petra/rt/vis_1nm.dat' ;    #(Si spectrometer)
printf,ui,'correlated_k SBDART'
printf,ui,'zout '+strjoin(string(zout))
printf,ui,'quiet'
free_lun,ui

spawn,'/data/seven/schmidt/RTM/libRadtran-1.3/tools/uvspec < '+'/data/seven/schmidt/calnex/rt/vis.inp '+'> /data/seven/schmidt/calnex/rt/vis.out'

openr,uo,'/data/seven/schmidt/calnex/rt/vis.out',/get_lun
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