; program to read in the values outputted by the uvspec program on the janus cluster
; used to build the look up tables for radiance/irradiance retrieval of aerosol properties at Skywatch

@zensun.pro
@/home/leblanse/libradtran/pro/libradtran_reader.pro

pro read_ref_sp

;set the proper directories
date='20120824'
outdir='/lustre/janus_scratch/leblanse/cloud/output/tablesp/';'/scratch/stmp00/leblanse/cloud_rad/output/'
dir   ='/projects/leblanse/cloud_rad/'
restore, dir+'20120523_calibspcs.out'

;;;;; restore the total file
;tau=(findgen(5)+1.)*50.
;ref=(findgen(5)+1.)*5.

tau=150
ref=[10.,25.,50.,75.]

lat=40.007916667
lon=-105.26825

doy=julian_day(strmid(date, 0, 4),strmid(date,4,2),strmid(date,6,2))
zensun, doy,20.0,lat, lon, sza, azi, solfac

;set up the model run
zout=[0.02]

;set the wavelenghts to be interpolated
u=where(zenlambda ge 400. and zenlambda lt 2200.)
sp=fltarr(n_elements(ref),n_elements(zenlambda[u]))

fns=['cloud_tb_ta03_re01_ice','cloud_tb_ta03_re04_ice','cloud_tb_ta03_re05_ice','cloud_tb_ta03_re06_ice']

  for s=0, n_elements(ref)-1 do begin
          fn=reform(fns[s])
          outf=outdir+fn+'.out'
          fi=file_lines(outf)
          if fi le 1 then continue
          output=libradtran_reader(file=outf,/radiance,zout=zout,/quiet)
          sp[s,*]=output.rad
          print, fn
  endfor  ;sza loop

zenlambda=zenlambda[u]
save, zenlambda, tau, ref, sp, filename=dir+'sp_ice_ref.out'

stop
end
