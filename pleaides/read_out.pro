; program to read in the values outputted by the uvspec program on the janus cluster
; used to build the look up tables for radiance/irradiance retrieval of aerosol properties at Skywatch

@zensun.pro
@/home/leblanse/libradtran/pro/libradtran_reader.pro

pro read_out

;set the proper directories
date='20120912'
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

labl=['wv2','wv3','wv1','oxa']

for i=0, ns-1 do begin
  for j=0,3 do begin
    fn='cloud_'+string(i,format='(I05)')+'_'+labl[j]
    outf=outdir+fn+'.out'
    fi=file_lines(outf)
    if fi le 1 then continue
    output=libradtran_reader(file=outf,/radiance,zout=zout,/quiet)
    print, tottmhrs[n[i]],tau[i],ref[i],labl[j],i,ns
    v=n_elements(output.wvl)
    sps=smooth(output.rad,3)
    spi0 =interpol(sps[[2,v-3]],output.wvl[[2,v-3]],output.wvl[2:v-3])
    area[i,j]=int_tabulated(output.wvl[[2,v-3]],1.-sps[[2,v-3]]/spi0)
;stop
  endfor
endfor

area2=reform(area[*,1])
area3=reform(area[*,2])
oxa=reform(area[*,3])
area=reform(area[*,0])

save, area, area2, area3, oxa, tau, ref, base, water, szas, doy, filename=dir+'area_comp2_'+date+'.out'

stop
end
