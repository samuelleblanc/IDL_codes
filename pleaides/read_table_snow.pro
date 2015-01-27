; program to read in the values outputted by the uvspec program on the janus cluster
; used to build the look up tables for radiance/irradiance retrieval of aerosol properties at Skywatch

@zensun.pro
@/home/leblanse/libradtran/pro/libradtran_reader.pro

pro read_table_snow

;set the proper directories
date='20120824'
outdir='/lustre/janus_scratch/leblanse/cloud/output/tablesp_snow/';'/scratch/stmp00/leblanse/cloud_rad/output/'
dir   ='/projects/leblanse/cloud_rad/'
restore, dir+'20120523_calibspcs.out'

;;;;; restore the total file
tau=(findgen(5))*50.
tau[0]=25.
tau=[10.,tau]
ref=(findgen(5)+1.)*5.

lat=40.007916667
lon=-105.26825

doy=julian_day(strmid(date, 0, 4),strmid(date,4,2),strmid(date,6,2))
zensun, doy,20.0,lat, lon, sza, azi, solfac

;set up the model run
zout=[0.02]

;set the wavelenghts to be interpolated
u=where(zenlambda ge 400. and zenlambda lt 2200.)
sp=fltarr(n_elements(tau),n_elements(ref),n_elements(zenlambda[u]),2)
sp_irrup=fltarr(n_elements(tau),n_elements(ref),n_elements(zenlambda[u]),2)
sp_irrdn=fltarr(n_elements(tau),n_elements(ref),n_elements(zenlambda[u]),2)

  for t=0, n_elements(tau)-1 do begin
    for r=0, n_elements(ref)-1 do begin
          fn='cloud_sn_ta'+string(t,format='(I02)')+'_re'+string(r,format='(I02)')
          outf=outdir+fn+'.out'
          fi=file_lines(outf)
          if fi le 1 then goto,ef
          output=libradtran_reader(file=outf,/radiance,zout=zout,/quiet)
          sp[t,r,*,0]=output.rad
          sp_irrup[t,r,*,0]=output.dif_up
          sp_irrdn[t,r,*,0]=output.dir_dn+output.dif_dn

ef:
          fn='cloud_sn_ta'+string(t,format='(I02)')+'_re'+string(r,format='(I02)')+'_ice'
          outf=outdir+fn+'.out'
          fi=file_lines(outf)
          if fi le 1 then continue
          output=libradtran_reader(file=outf,/radiance,zout=zout,/quiet)
          sp[t,r,*,1]=output.rad
          sp_irrup[t,r,*,1]=output.dif_up
          sp_irrdn[t,r,*,1]=output.dir_dn+output.dif_dn
          print, fn
    endfor ;ref loop
  endfor  ;tau loop

zenlambda=zenlambda[u]
save, zenlambda, tau, ref, sp,sp_irrdn,sp_irrup, filename=dir+'sp_liq_ice_snow.out'

stop
end
