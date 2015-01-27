; program to read in the values outputted by the uvspec program on the janus cluster
; used to build the look up tables for radiance/irradiance retrieval of aerosol properties at Skywatch

@zensun.pro
@/home/leblanse/libradtran/pro/libradtran_reader.pro

pro read_sp_mix

;set the proper directories
date='20120525'
lbl='sp_mix2_lvls';_lay'
lbl='sp_mix2'
lbl='sp_mix2_lay'


outdir='/lustre/janus_scratch/leblanse/cloud/output/'+lbl+'/';'/scratch/stmp00/leblanse/cloud_rad/output/'
dir   ='/projects/leblanse/cloud_rad/'
restore, dir+'20120523_calibspcs.out'

;;;;; restore the total file
;tau=[1.,2.,3.,4.,5.,7.5,10.,15.,20.,25.,30.,40.,50.,60., 70.,80.,90.,100.,125.,150.,175.,200.,300.,400.]
;ref=[2.5,5.,7.5,10.,12.5,15.,17.5,20.,22.5,25.,30.,35.,40.,45.,50.]
tau=[1.,5.,10.,20.,50.,100]
refl=[5.,15.,25.]
refi=[15.,25.,50.]
wp=[0.,0.25,0.5,0.75,1.0] ; percentage of optical depth that is contributed by ice

lat=40.007916667
lon=-105.26825

doy=julian_day(strmid(date, 0, 4),strmid(date,4,2),strmid(date,6,2))
zensun, doy,20.0,lat, lon, sza, azi, solfac
sza=30.

;set up the model run
zout=[0.02]

;set the wavelenghts to be interpolated
u=where(zenlambda ge 400. and zenlambda lt 2200.)
sp=fltarr(n_elements(tau),n_elements(refl),n_elements(refi),n_elements(zenlambda[u]),n_elements(wp))
sp_irrdn=sp
sp_irrup=sp


  for t=0, n_elements(tau)-1 do begin
    for r=0, n_elements(refl)-1 do begin
      for ri=0, n_elements(refi)-1 do begin
        for w=0, n_elements(wp)-1 do begin
          fn='cloud_tb_ta'+string(t,format='(I02)')+'_rl'+string(r,format='(I02)')+'_ri'+string(ri,format='(I02)')+'_wp'+string(w,format='(I02)')
          outf=outdir+fn+'.out'
          fi=file_test(outf)
          if fi ne 1 then continue
          fi=file_lines(outf)
          if fi le 1 then continue
          output=libradtran_reader(file=outf,/radiance,zout=zout,/quiet)
          sp[t,r,ri,*,w]=output.rad
          sp_irrdn[t,r,ri,*,w]=output.dir_dn+output.dif_dn
          sp_irrup[t,r,ri,*,w]=output.dif_up
          print, fn
        endfor ; wp loop
      endfor ; refi loop
    endfor ;ref loop
  endfor  ;tau loop

zenlambda=zenlambda[u]
save, zenlambda, tau, refl, refi, wp, sp, sp_irrdn,sp_irrup,sza,filename=dir+lbl+'_v2.out'

stop
end
