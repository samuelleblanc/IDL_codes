; program to read in the values outputted by the uvspec program on the janus cluster
; used to build the look up tables for radiance/irradiance retrieval of aerosol properties at Skywatch

@zensun.pro
@/projects/leblanse/libradtran/pro/libradtran_reader.pro

pro read_phase_liq

name='hbr000'
;phase_name='aggregate_columns050.leg'
phase_name='liquid water clouds - Mie interpolated'
nstreams='DISORT 2.0, 28'

;set the proper directories
date='20120824'
outdir='/lustre/janus_scratch/leblanse/cloud/output/phase/'+name+'/';'/scratch/stmp00/leblanse/cloud_rad/output/'
dir   ='/projects/leblanse/cloud/phase/'
phase =dir+'hollow_bullet_rosettes000_small.leg'

alb_file='/lustre/janus_scratch/leblanse/sea_albedo.dat'
solar_file='/projects/leblanse/libradtran/solar_eMAS.dat'
;;;;; restore the total file
tau=[0.,1.,2.,3.,4.,5.,7.5,10.,15.,20.,25.,30.,40.,50.,60., 70.,80.,90.,100.,125.,150.,175.,200.,300.,400.]
;ref=[2.5,5.,7.5,10.,12.5,15.,17.5,20.,22.5,25.]

restore, phase
rs=where(ref le 25.)
ref=ref[rs]

lat=40.007916667
lon=-105.26825

doy=julian_day(strmid(date, 0, 4),strmid(date,4,2),strmid(date,6,2))
zensun, doy,20.0,lat, lon, sza, azi, solfac

sza=35.

;set up the model run
zout=[0.02,5.]

;set the wavelenghts to be interpolated
;u=where(zenlambda ge 400. and zenlambda lt 2200.)

sp_radup=fltarr(n_elements(tau),n_elements(ref),n_elements(wvl),2)
sp_raddn=fltarr(n_elements(tau),n_elements(ref),n_elements(wvl),2)
sp_irrup=fltarr(n_elements(tau),n_elements(ref),n_elements(wvl),2)
sp_irrdn=fltarr(n_elements(tau),n_elements(ref),n_elements(wvl),2)

  for t=1, n_elements(tau)-1 do begin
    for r=0, n_elements(ref)-1 do begin
      if t eq 0 then r=n_elements(ref)-1 ; only do one go around for a clear sky
;          fn='cloud_tb_ta'+string(t,format='(I02)')+'_re'+string(r,format='(I02)')
;          outf=outdir+fn+'.out'
;          fi=file_lines(outf)
;          if fi le 1 then goto,ef
;          output=libradtran_reader(file=outf,/radiance,zout=zout,/quiet)
;          sp[t,r,*,0]=output.rad
;ef:
          fn='cloud_'+name+'_ta'+string(t,format='(I02)')+'_re'+string(r,format='(I02)')
          outf=outdir+fn+'.out'
          fi=file_lines(outf)
          if fi le 1 then continue
          output=libradtran_reader(file=outf,radiance=2,zout=zout,/quiet)
          for z=0,n_elements(zout)-1 do begin
            sp_radup[t,r,*,z]=output.rad[z,*,1]
            sp_raddn[t,r,*,z]=output.rad[z,*,0]
            sp_irrup[t,r,*,z]=output.dif_up[z,*]*1000.
            sp_irrdn[t,r,*,z]=(output.dir_dn[z,*]+output.dif_dn[z,*])*1000.
          endfor
          ;stop
          print, fn
    endfor ;ref loop
  endfor  ;tau loop

alb=read_ascii(alb_file)
surface_albedo=reform(alb.field1[1,*]) ;interpol(alb.field1[1,*],alb.field1[0,*],wvl)
sol=read_ascii(solar_file)
solar=reform(sol.field1[1,*])

save, surface_albedo,solar,wvl, tau, ref, sp_radup,sp_raddn,sp_irrup,sp_irrdn, sza,zout,phase_name,nstreams,filename=dir+'sp_liq_Sea.out'

stop
end
