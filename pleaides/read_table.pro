; program to read in the values outputted by the uvspec program on the janus cluster
; used to build the look up tables for radiance/irradiance retrieval of aerosol properties at Skywatch

@zensun.pro
@/home/leblanse/libradtran/pro/libradtran_reader.pro

pro read_table

;set the proper directories
date='20120824'
outdir='/lustre/janus_scratch/leblanse/cloud/output/table/';'/scratch/stmp00/leblanse/cloud_rad/output/'
dir   ='/projects/leblanse/cloud_rad/'
restore, dir+'20120523_calibspcs.out'

;;;;; restore the total file
base=(findgen(14)+1.)/2.
tau=(findgen(20)+1.)*10.
ref=(findgen(10)+1.)*2.5
water=findgen(22)/2.+15.

base=(findgen(7)+1.)
tau=(findgen(10)+1.)*20.
ref=(findgen(10)+1.)*2.5
water=findgen(11)+15.

lat=40.007916667
lon=-105.26825

doy=julian_day(strmid(date, 0, 4),strmid(date,4,2),strmid(date,6,2))
zensun, doy,20.0,lat, lon, sza, azi, solfac

;set up the model run
zout=[0.02]

area=fltarr(n_elements(base),n_elements(tau),n_elements(ref),n_elements(water),4)

wvs=[[842.,1074.],[1079.,1295.],[780.,859.],[738.,852.2]]
labl=['wv2','wv3','wv1','oxa']

;set the wavelenghts to be interpolated
n1=where(zenlambda gt wvs[0,2] and zenlambda lt wvs[1,2])
wv1=zenlambda[n1]
n2=where(zenlambda gt wvs[0,0] and zenlambda lt wvs[1,0])
wv2=zenlambda[n2]
n3=where(zenlambda gt wvs[0,1] and zenlambda lt wvs[1,1])
wv3=zenlambda[n3]
nx=where(zenlambda gt wvs[0,3] and zenlambda lt wvs[1,3])
oxa=zenlambda[nx]

for b=0, n_elements(base)-1 do begin
  for t=0, n_elements(tau)-1 do begin
    for r=0, n_elements(ref)-1 do begin
      for w=0, n_elements(water)-1 do begin
        for j=0,3 do begin
          case j of
          0: wv=wv2
          1: wv=wv3
          2: wv=wv1
          3: wv=oxa
          endcase
          fn='cloud_ba'+string(b,format='(I02)')+'_ta'+string(t,format='(I02)')+'_re'+string(r,format='(I02)')+'_wa'+string(w,format='(I02)')+labl[j]
          outf=outdir+fn+'.out'
          fi=file_lines(outf)
          if fi le 1 then continue
          output=libradtran_reader(file=outf,/radiance,zout=zout,/quiet)
          print, base[b],tau[t],ref[r],water[w],labl[j]
          rad=interpol(output.rad,output.wvl,wv)
          v=n_elements(wv)
          if j eq 0 then sps=smooth(rad,3) else sps=rad
          spi0 =interpol(sps[[2,v-3]],wv[[2,v-3]],wv[2:v-3])
          area[b,t,r,w,j]=int_tabulated(wv[2:v-3],1.-sps[2:v-3]/spi0)
        endfor ; label loop
      endfor ; water loop
    endfor ;ref loop
  endfor  ;tau loop
endfor  ;base loop

;area2=reform(area[*,1])
;area3=reform(area[*,2])
;oxa=reform(area[*,3])
;area=reform(area[*,0])

;save, area, area2, area3, oxa, tau, ref, base, water, szas, doy, filename=dir+'area_comp_'+date+'.out'

save, area, doy, sza, tau, ref, base, water, filename=dir+'area_table3_mod.out'

stop
end
