; program to read in the values outputted by the uvspec program on the janus cluster
; used to build the look up tables for radiance/irradiance retrieval of aerosol properties at Skywatch

@zensun.pro
@/projects/leblanse/libradtran/pro/libradtran_reader.pro

pro read_phase

;name='hbr050_v2'
;name='ghm_sr'
;name='hoc000_v2'
;name='sbr050_v2'
name='agc003_v2'
;phase_name='aggregate_columns050.leg'
;phase_name='v2/large_aggregate_plates050.leg'
phase_name='v2/aggregate_columns003.leg'
;phase_name='GeneralHabitMixture_SeverelyRough.leg'
;phase_name='v2/hollow_columns000.leg'
;phase_name='v2/large_aggregate_plates000.leg'
;phase_name='solid_bullet_rosettes050_small.leg'
;phase_name='v2/aggregate_columns050.leg' ;'small_aggregate_plates050_small.leg'
nstreams='DISORT 2.0, 28'

;set the proper directories
date='20120824'
outdir='/lustre/janus_scratch/leblanse/cloud/output/phase/'+name+'/';'/scratch/stmp00/leblanse/cloud_rad/output/'
dir   ='/projects/leblanse/cloud/phase/'
phase =dir+phase_name

;alb_file='/lustre/janus_scratch/leblanse/albedo_eMAS.dat'
alb_file='/lustre/janus_scratch/leblanse/sea_albedo.dat'
solar_file='/projects/leblanse/libradtran/solar_eMAS.dat'
;;;;; restore the total file
tau=[0.,1.,2.,3.,4.,5.,7.5,10.,15.,20.,25.,30.,40.,50.,60., 70.,80.,90.,100.,125.,150.,175.,200.,300.,400.]
;ref=[2.5,5.,7.5,10.,12.5,15.,17.5,20.,22.5,25.]

print, 'restoring :'+phase
restore, phase

lat=40.007916667
lon=-105.26825

doy=julian_day(strmid(date, 0, 4),strmid(date,4,2),strmid(date,6,2))
zensun, doy,20.0,lat, lon, sza, azi, solfac

sza=35.

;set up the model run
zout=[0.02,5.]

;set the wavelenghts to be interpolated
;u=where(zenlambda ge 400. and zenlambda lt 2200.)
wvl=[470.000     , 550.000  ,    660.000  ,    700.000   ,   740.000,      870.000 ,     1000.00  ,    1050.00  ,$
    1180.00  ,    1210.00  ,    1240.00  ,    1280.00  ,    1560.00 ,     1610.00  ,    1660.00 ,     1720.00  ,$
    1770.00  ,    2030.00  ,    2080.00  ,    2130.00,$
      2180.00     , 2230.00  ,    2250.00 ,     2280.00 ,     2330.00 ,2370.00]
sp_radup=fltarr(n_elements(tau),n_elements(ref),n_elements(wvl),2)
sp_raddn=fltarr(n_elements(tau),n_elements(ref),n_elements(wvl),2)
sp_irrup=fltarr(n_elements(tau),n_elements(ref),n_elements(wvl),2)
sp_irrdn=fltarr(n_elements(tau),n_elements(ref),n_elements(wvl),2)

  for t=0, n_elements(tau)-1 do begin
    for r=0, n_elements(ref)-1 do begin
      if t eq 0 then r=n_elements(ref)-1 ; only do one go around for a clear sky
;          fn='cloud_tb_ta'+string(t,format='(I02)')+'_re'+string(r,format='(I02)')
;          outf=outdir+fn+'.out'
;          fi=file_lines(outf)
;          if fi le 1 then goto,ef
;          output=libradtran_reader(file=outf,/radiance,zout=zout,/quiet)
;          sp[t,r,*,0]=output.rad
;ef:
          fn='cloud_'+name+'_ta'+string(t,format='(I02)')+'_re'+string(r,format='(I03)')+'_ice'
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
surface_albedo=reform(alb.field1[1,*])
sol=read_ascii(solar_file)
solar=reform(sol.field1[1,*])

save, surface_albedo,solar,wvl, tau, ref, sp_radup,sp_raddn,sp_irrup,sp_irrdn, sza,zout,phase_name,nstreams,filename=dir+'sp_'+name+'_Sea.out'

stop
end
