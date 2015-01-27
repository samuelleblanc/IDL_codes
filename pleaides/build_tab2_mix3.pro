; program to build files for modeling cloud properties
; build a whole reference table
; particular for a mix of liquid and ice water content in cloud

@zensun.pro
;@make_ic_files.pro
@write_inp_mix.pro
pro build_tab2_mix3

nofile=0 ;write or not the files
sets=2   ;0=normal, 1=layers, 2=levels

;set the proper directories
date='20120524'
case sets of
  0: lbl='sp_mix3_'+date
  1: lbl='sp_mix3_lay_'+date
  2: lbl='sp_mix3_lvls_'+date
endcase

;lbl='sp_mix3_iz01_'+date
;z=01.0
lbl='tab2_mix3_'+date

  ;set precipitable water
;pw=05. ;40 mm of precipitable water


;lbl='sp_mix2' ;_lay' ;_lvls'
;lbl='sp_mix2_lay'
;lbl='sp_mix2_lvls'

indir ='/lustre/janus_scratch/leblanse/cloud/input/'+lbl+'/';'/scratch/stmp00/leblanse/cloud/input/'
outdir='/lustre/janus_scratch/leblanse/cloud/output/'+lbl+'/';'/scratch/stmp00/leblanse/cloud/output/'
dir   ='/projects/leblanse/cloud/'

spawn,'mkdir '+indir
spawn,'mkdir '+outdir
;spawn,'mkdir '+dir
list_file='/projects/leblanse/cloud/run_cloud_'+lbl+'.sh'

alb=0.2

;make the table spacing
;tau=[1.,2.,3.,4.,5.,7.5,10.,15.,20.,25.,30.,40.,50.,60.,70.,80.,90.,100.,125.,150.,175.,200.]
;refl=[2.5,5.,7.5,10.,12.5,15.,17.5,20.,22.5,25.,30.,35.,40.,45.,50.]
;tau=[1.,5.,10.,20.,50.,100]
;refl=[5.,15.,25.]
;refi=[15.,20.,25.,30.,35.,40.,45.,50.]
refl=7.5
refi=42.5
tau=15.


lat=40.007916667
lon=-105.26825

doy=julian_day(strmid(date,0,4),strmid(date,4,2),strmid(6,2))
sza=30.
azi=0.
print, 'opening list_file:'+list_file
;wp=[0.,0.25,0.5,0.75,1.0] ; percentage of water content that is ice
;wp=[0.,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0]
wp=0.1

uu=97
openw,uu,list_file

atm_file='/lustre/janus_scratch/leblanse/wx/afglms.dat' ;'/projects/leblanse/libradtran/libRadtran-1.6-beta/data/atmmod/afglms.dat'
;atm_file='/lustre/janus_scratch/leblanse/wx/afglmw.dat'
alb_file='/lustre/janus_scratch/leblanse/albedo_2012-05-24.dat'
;alb_file='/lustre/janus_scratch/leblanse/albedo_2012-08-04.dat'
;alb_file='/lustre/janus_scratch/leblanse/albedo_2012-08-12.dat'
;alb_file='/lustre/janus_scratch/leblanse/albedo_2012-08-20.dat'
;alb_file='/lustre/janus_scratch/leblanse/albedo_2012-09-13.dat'
;alb_file='/lustre/janus_scratch/leblanse/albedo_2013-01-10.dat'
;alb_file='/lustre/janus_scratch/leblanse/albedo_2013-01-11.dat'
;alb_file='/lustre/janus_scratch/leblanse/snow_albedo.dat'
zout=[0.02]
z=[1.5,1.65,1.8,1.95,2.1,2.25,2.4,2.55,2.7,2.85,3.0]
for i=0,n_elements(z)-1 do begin
          fn='cloud_tb_ta'+string(i,format='(I02)')          
          outf=outdir+fn+'.out'
          inf =indir +fn+'.in'
          cloudl=[tau,refl,1.5+1.68,1.5] 
          cloudi=[tau,refi,z[i]+1.68,0.15]
          wvl=[400.,2200.]
          albedo=0.2
          print, z[i]
          write_inp_mix, doy, sza, atm_file, inf, azi, wvl, zout, $
           /quiet, cloudl=cloudl,alb_file=alb_file,cloudi=cloudi, wp=wp,pw=pw ;,nofile=nofile
          printf, uu, '/projects/leblanse/libradtran/libRadtran-1.7/bin/uvspec < '+inf+' > '+outf
          print, inf
endfor ;z loop
free_lun,uu

end


