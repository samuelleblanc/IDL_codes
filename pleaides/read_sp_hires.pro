; program to read in the values outputted by the uvspec program on the janus cluster
; used to build the look up tables for radiance/irradiance retrieval of aerosol properties at Skywatch

@zensun.pro
@/home/leblanse/libradtran/pro/libradtran_reader.pro

pro read_sp_hires

;set the proper directories
date='20120824'
outdir='/lustre/janus_scratch/leblanse/cloud/output/sp_hires/';'/scratch/stmp00/leblanse/cloud_rad/output/'
dir   ='/projects/leblanse/cloud_rad/'
restore, dir+'20120523_calibspcs.out'

;;;;; restore the total file
tau=[1.,2.,3.,4.,5.,7.5,10.,15.,20.,25.,30.,40.,50.,60., 70.,80.,90.,100.,125.,150.,175.,200.]
ref=[2.5,5.,7.5,10.,12.5,15.,17.5,20.,22.5,25.]

lat=40.007916667
lon=-105.26825

doy=julian_day(strmid(date, 0, 4),strmid(date,4,2),strmid(date,6,2))
zensun, doy,20.0,lat, lon, sza, azi, solfac

;set up the model run
zout=[0.02]

;set the wavelenghts to be interpolated
u=where(zenlambda ge 400. and zenlambda lt 2200.)
sp=fltarr(n_elements(tau),n_elements(ref),n_elements(zenlambda[u]),2)


  for t=0, n_elements(tau)-1 do begin
    for r=0, n_elements(ref)-1 do begin
          fn='cloud_tb_ta'+string(t,format='(I02)')+'_re'+string(r,format='(I02)')
          outf=outdir+fn+'.out'
          fi=file_lines(outf)
          if fi le 1 then goto,ef
          output=libradtran_reader(file=outf,/radiance,zout=zout,/quiet)
          sp[t,r,*,0]=output.rad
ef:
          fn='cloud_tb_ta'+string(t,format='(I02)')+'_re'+string(r,format='(I02)')+'_ice'
          outf=outdir+fn+'.out'
          fi=file_lines(outf)
          if fi le 1 then continue
          output=libradtran_reader(file=outf,/radiance,zout=zout,/quiet)
          sp[t,r,*,1]=output.rad
          print, fn
    endfor ;ref loop
  endfor  ;tau loop

zenlambda=zenlambda[u]
save, zenlambda, tau, ref, sp, filename=dir+'sp_liq_ice_hires.out'

stop
end
