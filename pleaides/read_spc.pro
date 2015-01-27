; program to read in the values outputted by the uvspec program on the janus cluster
; used to build the look up tables for radiance/irradiance retrieval of aerosol properties at Skywatch

@/projects/leblanse/cloud/pro/zensun.pro
@/home/leblanse/libradtran/pro/libradtran_reader.pro

pro read_spc,date

;set the proper directories
;date='20120824'
outdir='/lustre/janus_scratch/leblanse/cloud/output/'+date+'/';'/scratch/stmp00/leblanse/cloud_rad/output/'
dir   ='/projects/leblanse/cloud_rad/'

;;;;; restore the total file
restore, dir+'cloudy.out'
;date='20120820'
n=where(totday eq double(date),ns)
base=totbase[n]/1000.
tau=tottau[n]
ref=totref[n]
water=totwater[n]

lat=40.007916667
lon=-105.26825

doy=julian_day(strmid(date, 0, 4),strmid(date,4,2),strmid(date,6,2))
zensun, doy,tottmhrs[n],lat, lon, szas, azi, solfac

;set up the model run
zout=[0.02]

area=fltarr(ns,4)

wvs=[[842.,1074.],[1079.,1295.],[780.,859.],[738.,852.2]]

; get a list of the proper wavelengths used for each band
sl=read_ascii('/projects/leblanse/libradtran/solar_SSFR.dat')
lambda=sl.field1[0,*]

wvl=0.
wvli=0.
nws=0.
for i=0, 3 do begin
  u=where(lambda gt wvs[0,i] and lambda lt wvs[1,i],nv)
  wvl=[wvl,lambda[u]]
  nws=[nws,nws[i]+nv]
  wvli=[wvli,lambda[u[2:n_elements(u)-3]]]
endfor
wvl=wvl[1:*]
wvli=wvli[1:*]
;stop
spc=fltarr(ns,n_elements(wvl))
spcs=spc
spci=fltarr(ns,n_elements(wvl)-4*4)
labl=['wv2','wv3','wv1','oxa']
for i=0, ns-1 do begin
ii=0.
  for j=0,3 do begin
    fn='cloud_'+string(i,format='(I05)')+'_'+labl[j]
    outf=outdir+fn+'.out'
    fi=file_lines(outf)
    if fi le 1 then continue
    output=libradtran_reader(file=outf,/radiance,zout=zout,/quiet)
    print, tottmhrs[n[i]],tau[i],ref[i],labl[j],i,ns
    v=n_elements(output.wvl)
    if j eq 0 then sps=smooth(output.rad,3) else sps=output.rad
    spi0 =interpol(sps[[2,v-3]],output.wvl[[2,v-3]],output.wvl[2:v-3])
    area[i,j]=int_tabulated(output.wvl[2:v-3],1.-sps[2:v-3]/spi0)
    spc[i,nws[j]:nws[j+1]-1]=output.rad
    spcs[i,nws[j]:nws[j+1]-1]=sps
    spci[i,ii:ii+n_elements(spi0)-1]=sps[2:v-3]/spi0
    ii=ii+n_elements(spi0)
;stop
  endfor
endfor

area2=reform(area[*,1])
area3=reform(area[*,2])
oxa=reform(area[*,3])
area=reform(area[*,0])

save, area, area2, area3, oxa, tau, ref, base, water, szas, doy, filename=dir+'area_comp4_'+date+'.out'
save, area, area2, area3, oxa, tau, ref, base, water, szas, doy, spc,spcs,spci,wvl, wvli, filename=dir+'spc_comp4_'+date+'.out'

stop
end
