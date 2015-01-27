; program to read in the values outputted by the uvspec program on the janus cluster
; used to build the look up tables for radiance/irradiance retrieval of aerosol properties at Skywatch

@zensun.pro
@/home/leblanse/libradtran/pro/libradtran_reader.pro

pro read_sp_hires6

;set the proper directories
date='20130110_snd2'
lbl=date
dir   ='/projects/leblanse/cloud_rad/'
restore, dir+'20120523_calibspcs.out'
outdir='/projects/leblanse/cloud/rt_files/output/sp_hires6_'+date+'/'
dir   ='/projects/leblanse/cloud/'


outdir='/lustre/janus_scratch/leblanse/cloud/output/sp_hires6_'+date+'/'


;make the table spacing
; for v6
tau=[1.,2.,3.,4.,5.,7.,8.,10.,12.,15.,20.,25.,30.,35.,40.,45.,50.,60.,70.,80.,90.,100.]
ref=[2.,3.,4.,5.,6.,7.,8.,9.,10.,11.,12.,13.,14.,15.,16.,17.,18.,20.,22.,23.,25.,28.,30.,32.,35.,38.,40.,42.,45.,48.,50.]
;ref=[2.,3.,4.,5.,6.,7.,8.,9.,10.,11.,12.,13.,14.,15.,16.,17.,18.,20.,22.,23.,25.,28.,30.,32.,35.,40.,45.,50.] ; 0806

lat=40.007916667
lon=-105.26825

doy=julian_day(strmid(date, 0, 4),strmid(date,4,2),strmid(date,6,2))
zensun, doy,20.0,lat, lon, sza, azi, solfac
sza=50. ;0523,0525,0816
sza=42. ; 0602
sza=55. ;0806
sza=37. ;0813
sza=31. ;0820
sza=40. ;0824
sza=45. ;0912
sza=63. ;20130110
sza=73. ;20130111
sza=64. ;0816
sza=46.57 ; 0806 snd3
sza=45. ; 0824 snd2, 0525 snd2
;sza=70. ;0816 snd2
;sza=54. ;0816 snd3
;sza=40. ;0525 snd2 ;0813 snd3
sza=66. ;0110 snd2

;set up the model run
zout=[0.02]

;set the wavelenghts to be interpolated
u=where(zenlambda ge 400. and zenlambda lt 1700.)
sp=fltarr(n_elements(tau),n_elements(ref),n_elements(zenlambda[u]),2)
sp_irrdn=sp
sp_irrup=sp


  for t=0, n_elements(tau)-1 do begin
    for r=0, n_elements(ref)-1 do begin
          fn='cloud_tb_ta'+string(t,format='(I02)')+'_re'+string(r,format='(I02)')
          outf=outdir+fn+'.out'
          fi=file_test(outf)
          if fi ne 1 then goto,ef
          fi=file_lines(outf)
          if fi le 1 then goto,ef
          output=libradtran_reader(file=outf,/radiance,zout=zout,/quiet)
          sp[t,r,*,0]=output.rad*1000.
          sp_irrdn[t,r,*,0]=output.dir_dn+output.dif_dn
          sp_irrup[t,r,*,0]=output.dif_up
ef:
          fn='cloud_tb_ta'+string(t,format='(I02)')+'_re'+string(r,format='(I02)')+'_ice'
          outf=outdir+fn+'.out'
          fi=file_lines(outf)
          if fi le 1 then continue
          output=libradtran_reader(file=outf,/radiance,zout=zout,/quiet)
          sp[t,r,*,1]=output.rad*1000.
          sp_irrdn[t,r,*,1]=output.dir_dn+output.dif_dn
          sp_irrup[t,r,*,1]=output.dif_up
          print, fn
    endfor ;ref loop
  endfor  ;tau loop

zenlambda=zenlambda[u]
save, zenlambda, tau, ref, sp, sp_irrdn,sp_irrup,sza,filename=dir+'/model/sp_hires6_'+lbl+'.out'

stop
end
