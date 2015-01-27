; program to read in the values outputted by the uvspec program on the janus cluster
; used to build the look up tables for radiance/irradiance retrieval of aerosol properties at Skywatch

@zensun.pro
@/home/leblanse/libradtran/pro/libradtran_reader.pro

pro read_zna

;set the proper directories
date='20120824'
outdir='/lustre/janus_scratch/leblanse/cloud/output/zna/';'/scratch/stmp00/leblanse/cloud_rad/output/'
dir   ='/projects/leblanse/cloud_rad/'
restore, dir+'20120523_calibspcs.out'

;;;;; restore the total file
;tau=(findgen(5))*50.
;tau[0]=25.
;tau=(findgen(5)+1.)*5.
;ref=(findgen(5)+1.)*5.

tau=[10]
ref=[5]

lat=40.007916667
lon=-105.26825

doy=julian_day(strmid(date, 0, 4),strmid(date,4,2),strmid(date,6,2))
zensun, doy,20.0,lat, lon, sza, azi, solfac
;sza=findgen(20)+17.0
sza=0.
zna=findgen(19)*5.
;set up the model run
zout=[0.02]

;set the wavelenghts to be interpolated
u=where(zenlambda ge 500. and zenlambda lt 504.)
sp=fltarr(n_elements(zna),n_elements(zenlambda[u]),2)


  for t=0, n_elements(tau)-1 do begin
    for r=0, n_elements(ref)-1 do begin
      for s=0, n_elements(zna)-1 do begin
          fn='cloud_tb_ta'+string(t,format='(I02)')+'_re'+string(r,format='(I02)')+'_sza'+string(s,format='(I02)')
          outf=outdir+fn+'.out'
          fi=file_lines(outf)
          if fi le 1 then goto,ef
          output=libradtran_reader(file=outf,/radiance,zout=zout,/quiet)
          sp[s,*,0]=output.rad
ef:
          fn='cloud_tb_ta'+string(t,format='(I02)')+'_re'+string(r,format='(I02)')+'_sza'+string(s,format='(I02)')+'_ice'
          outf=outdir+fn+'.out'
          fi=file_lines(outf)
          if fi le 1 then continue
          output=libradtran_reader(file=outf,/radiance,zout=zout,/quiet)
          sp[s,*,1]=output.rad
          print, fn
      endfor ; sza loop
    endfor ;ref loop
  endfor  ;tau loop

zenlambda=zenlambda[u]
save, zenlambda, tau, ref, sza,zna,sp, filename=dir+'zna_liq_ice.out'

stop
end
