; program to read in the values outputted by the uvspec program on the janus cluster
; used to build the look up tables for radiance/irradiance retrieval of aerosol properties at Skywatch

@zensun.pro
@/home/leblanse/libradtran/pro/libradtran_reader.pro

pro read_ice_liq_sp

;set the proper directories
date='20120824'
outdir='/lustre/janus_scratch/leblanse/cloud/output/ice_liq_new/';'/scratch/stmp00/leblanse/cloud_rad/output/'
dir   ='/projects/leblanse/cloud_rad/'
restore, dir+'20120523_calibspcs.out'

;;;;; restore the total file
;tau=(findgen(5)+1.)*50.
;ref=(findgen(5)+1.)*5.

tau=100
ref=25

lat=40.007916667
lon=-105.26825

doy=julian_day(strmid(date, 0, 4),strmid(date,4,2),strmid(date,6,2))
zensun, doy,20.0,lat, lon, sza, azi, solfac

;set up the model run
zout=[0.02]

fns=['cloud_ice_liq_00','cloud_ice_liq_10','cloud_ice_liq_20','cloud_ice_liq_30','cloud_ice_liq_40',$
     'cloud_ice_liq_50','cloud_ice_liq_60','cloud_ice_liq_70','cloud_ice_liq_80','cloud_ice_liq_90','cloud_ice_liq_xx']
fns=outdir+fns

ice=[0,10,20,30,40,50,60,70,80,90,100]

;set the wavelenghts to be interpolated
u=where(zenlambda ge 400. and zenlambda lt 2200.)
sp=fltarr(n_elements(fns),n_elements(zenlambda[u]))


  for s=0, n_elements(fns)-1 do begin
          fn=reform(fns[s])
;          outf=outdir+fn+'.out'
          outf=fn+'.out'
          fi=file_lines(outf)
          if fi le 1 then continue
          output=libradtran_reader(file=outf,/radiance,zout=zout,/quiet)
          sp[s,*]=output.rad
          print, fn
  endfor  ;sza loop

zenlambda=zenlambda[u]
save, zenlambda, tau, ref, ice, sp, filename=dir+'sp_ice_liq_mix_new.out'

stop
end
