; program to read in the values outputted by the uvspec program on the janus cluster
; used to build the look up tables for radiance/irradiance retrieval of aerosol properties at Skywatch

@zensun.pro
@/home/leblanse/libradtran/pro/libradtran_reader.pro

pro read_sp_mix

;set the proper directories
sets=2   ;0=normal, 1=layers, 2=levels

;set the proper directories
date='20120820_alb'
case sets of
  0: lbl='sp_mix3_'+date
  1: lbl='sp_mix3_lay_'+date
  2: lbl='sp_mix3_lvls_'+date
endcase

lbl='sp_mix3_'+date

outdir='/lustre/janus_scratch/leblanse/cloud/output/'+lbl+'/';'/scratch/stmp00/leblanse/cloud_rad/output/'
indir='/lustre/janus_scratch/leblanse/cloud/input/'+lbl+'/'
dir   ='/projects/leblanse/cloud_rad/'
restore, dir+'20120523_calibspcs.out'

;;;;; restore the total file
tau=[1.,2.,3.,4.,5.,7.5,10.,15.,20.,25.,30.,40.,50.,60.,70.,80.,90.,100.,125.,150.,175.,200.]
refl=[2.5,5.,7.5,10.,12.5,15.,17.5,20.,22.5,25.,30.,35.,40.,45.,50.]
refi=[15.,20.,25.,30.,35.,40.,45.,50.]
wp=[0.,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0]

lat=40.007916667
lon=-105.26825

doy=julian_day(strmid(date, 0, 4),strmid(date,4,2),strmid(date,6,2))
zensun, doy,20.0,lat, lon, sza, azi, solfac
sza=30.

;set up the model run
zout=[0.02]
;zout=[0.02,10.]

;set the wavelenghts to be interpolated
u=where(zenlambda ge 400. and zenlambda lt 2200.)
;sp=fltarr(n_elements(tau),n_elements(refl),n_elements(refi),n_elements(zenlambda[u]),n_elements(wp),2)
sp=fltarr(n_elements(tau),n_elements(refl),n_elements(refi),n_elements(zenlambda[u]),n_elements(wp))
sp_irrdn=sp
sp_irrup=sp
list_file='/projects/leblanse/cloud/run2_'+lbl+'.sh'
uu=97
openw,uu,list_file
bad=0 & tot=0
  for t=0, n_elements(tau)-1 do begin
    for r=0, n_elements(refl)-1 do begin
      for ri=0, n_elements(refi)-1 do begin
        for w=0, n_elements(wp)-1 do begin
          fn='cloud_tb_ta'+string(t,format='(I02)')+'_rl'+string(r,format='(I02)')+'_ri'+string(ri,format='(I02)')+'_wp'+string(w,format='(I02)')
          outf=outdir+fn+'.out'
          inf=indir+fn+'.in'
          tot=tot+1
          fi=file_test(outf)
          if fi ne 1 then begin
            printf, uu, '/projects/leblanse/libradtran/libRadtran-1.7/bin/uvspec < '+inf+' > '+outf
            bad=bad+1
            continue
          endif
          fi=file_lines(outf)
          if fi le 1 then begin
            printf, uu, '/projects/leblanse/libradtran/libRadtran-1.7/bin/uvspec < '+inf+' > '+outf
            bad=bad+1
            continue
          endif
          output=libradtran_reader(file=outf,/radiance,zout=zout,/quiet)
          sp[t,r,ri,*,w,*]=output.rad
          sp_irrdn[t,r,ri,*,w,*]=output.dir_dn+output.dif_dn
          sp_irrup[t,r,ri,*,w,*]=output.dif_up
          print, lbl+'/'+fn
        endfor ; wp loop
      endfor ; refi loop
    endfor ;ref loop
  endfor  ;tau loop
free_lun,uu
print, 'bad points: ',bad,'/',tot
zenlambda=zenlambda[u]
dir='/projects/leblanse/cloud/model/'
save, zenlambda, tau, refl, refi, wp, sp, sp_irrdn,sp_irrup,sza,filename=dir+lbl+'.out'

stop
end
