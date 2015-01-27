; program to read in the values outputted by the uvspec program on the janus cluster
; used to build the look up tables for radiance/irradiance retrieval of aerosol properties at Skywatch

@zensun.pro
@/home/leblanse/libradtran/pro/libradtran_reader.pro

pro read_nowv

;set the proper directories
date='20120824'
outdir='/lustre/janus_scratch/leblanse/cloud/output/tablesp_new3/';'/scratch/stmp00/leblanse/cloud_rad/output/'
dir   ='/projects/leblanse/cloud_rad/'
restore, dir+'20120523_calibspcs.out'

;;;;; restore the total file
tau=[25.,100.]
ref=[5.,25.]

lat=40.007916667
lon=-105.26825

doy=julian_day(strmid(date, 0, 4),strmid(date,4,2),strmid(date,6,2))
zensun, doy,20.0,lat, lon, sza, azi, solfac

;set up the model run
zout=[0.02]

;set the wavelenghts to be interpolated
u=where(zenlambda ge 400. and zenlambda lt 2200.)
sp=fltarr(2,2,n_elements(zenlambda[u]),2)


          fn='cloud_tb_ta02_re04'
          outf=outdir+fn+'_wv34.out'
          fi=file_lines(outf)
          output=libradtran_reader(file=outf,/radiance,zout=zout,/quiet)
          sp[0,0,*,0]=output.rad

          fnw=fn+'_wv01'
          outf=outdir+fnw+'.out'
          fi=file_lines(outf)
          output=libradtran_reader(file=outf,/radiance,zout=zout,/quiet)
          sp[0,1,*,0]=output.rad

          fn=fn+'_ice'
          outf=outdir+fn+'_wv34.out'
          fi=file_lines(outf)
          output=libradtran_reader(file=outf,/radiance,zout=zout,/quiet)
          sp[0,0,*,1]=output.rad
          print, fn

         fn=fn+'_wv01'
          outf=outdir+fn+'.out'
          fi=file_lines(outf)
          output=libradtran_reader(file=outf,/radiance,zout=zout,/quiet)
          sp[0,1,*,1]=output.rad

          fn='cloud_tb_ta00_re00'
          outf=outdir+fn+'_wv34.out'
          fi=file_lines(outf)
          output=libradtran_reader(file=outf,/radiance,zout=zout,/quiet)
          sp[1,0,*,0]=output.rad

          fnw=fn+'_wv01'
          outf=outdir+fnw+'.out'
          fi=file_lines(outf)
          output=libradtran_reader(file=outf,/radiance,zout=zout,/quiet)
          sp[1,1,*,0]=output.rad

          fn=fn+'_ice'
          outf=outdir+fn+'_wv34.out'
          fi=file_lines(outf)
          output=libradtran_reader(file=outf,/radiance,zout=zout,/quiet)
          sp[1,0,*,1]=output.rad
          print, fn

         fn=fn+'_wv01'
          outf=outdir+fn+'.out'
          fi=file_lines(outf)
          output=libradtran_reader(file=outf,/radiance,zout=zout,/quiet)
          sp[1,1,*,1]=output.rad



zenlambda=zenlambda[u]
save, zenlambda, tau, ref, sp, filename=dir+'sp_wv.out'

stop
end
