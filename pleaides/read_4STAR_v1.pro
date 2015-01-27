
; program to read in the values outputted by the uvspec program on the janus cluster
; used to build the look up tables for radiance/irradiance retrieval of aerosol properties at Skywatch

@zensun.pro
@/home/leblanse/libradtran/pro/libradtran_reader.pro

pro read_4star_v1

;set the proper directories
;date='20120912_5'
;date='20120806_3'
;date='20130110_2'
;date='20130913'
;date='20130911_ice'
date='20130911_snow_ice'
;date='20130911_sea';snow_ice'
vv='v1'

lbl=date
dir   ='/projects/leblanse/cloud_rad/'
restore, dir+'20120523_calibspcs.out'
; get the wavelength arrays in form of zenlambda

dir   ='/projects/leblanse/cloud/'
outdir='/lustre/janus_scratch/leblanse/cloud/output/'+vv+'_'+date+'/'


;make the table spacing
tau=[1.,2.,3.,4.,5.,7.,8.,10.,12.,15.,20.,25.,30.,35.,40.,45.,50.,60.,70.,80.,90.,100.]
ref=[2.,3.,4.,5.,6.,7.,8.,9.,10.,11.,12.,13.,14.,15.,16.,17.,18.,20.,22.,23.,25.,28.,$
     30.,32.,35.,38.,40.,43.,45.,48.,50.,52.,55.,58.,60.]

lat=29.609106 ;40.0079166
lon=-95.168302 ;-105.26825
doy=julian_day(strmid(date,0,4),strmid(date,4,2),strmid(6,2))
zensun, doy,18.41,lat, lon, sza, azi, solfac
;lat=40.007916667
;lon=-105.26825

;doy=julian_day(strmid(date, 0, 4),strmid(date,4,2),strmid(date,6,2))
;zensun, doy,20.0,lat, lon, sza, azi, solfac
;sza=50. ;0525 snd, 0806 snd2, 0523 snd, 0912 snd
;sza=45. ;0525 snd2, 0806 snd3, 0523 snd2, 0912 snd2
;sza=55. ;0525 snd3, 0806 snd, 0523 snd3, 0912 snd 3
;sza=66. ;0110 snd
;sza=63. ;0110 snd2
;sza=40. ;0525 snd4, 0523 snd4, 0912 snd4
;sza=37. ;0523 snd5
;sza=60. ;0523 snd6
;sza=35. ;0912 snd5

; z0=0.5, z1=1.5 or normal, z2=3, z3=5, z4=7, z5=9
; w0=5mm, w1=10mm, w2=15mm, w3=20mm, w4=25mm, w5=30mm
; changes in surface albedo
; ab1=2012-08-04, ab2=2012-08-12, ab3=2012-08-20, ab4=2012-09-13
z=9.0 & pw=6.06 ; 6.06
ab='20130913'

;set up the model run
zout=[8.0,19.0] ;zout=[0.02]
zout=[0.2,3.0]
;set the wavelenghts to be interpolated
;u=where(zenlambda ge 400. and zenlambda lt 1700.)
;zenlambda=zenlambda[u]
fn='cld_ta00_re00_v0' & outf=outdir+fn+'.out'
ov0=libradtran_reader(file=outf,/radiance,zout=zout,/quiet)
fn='cld_ta00_re00_v1' & outf=outdir+fn+'.out'
ov1=libradtran_reader(file=outf,/radiance,zout=zout,/quiet)
wvl=[reform(ov0.wvl[0,*]),reform(ov1.wvl[0,*])] & nv0=n_elements(ov0.wvl[0,*])
sp=fltarr(n_elements(tau),n_elements(ref),n_elements(zout),n_elements(wvl),2)
sp_irrdn=sp & sp_irrup=sp
spn=fltarr(n_elements(tau),n_elements(ref),n_elements(zenlambda),2)
spn_irrdn=spn & spn_irrup=spn

  for t=0, n_elements(tau)-1 do begin
    for r=0, n_elements(ref)-1 do begin
      if ref[r] le 30. then begin  
        fn='cld_ta'+string(t,format='(I02)')+'_re'+string(r,format='(I02)')+'_v0'
          outf=outdir+fn+'.out'
          fi=file_test(outf)
          if fi ne 1 then goto,ef
          fi=file_lines(outf)
          if fi le 1 then goto,ef
          output=libradtran_reader(file=outf,/radiance,zout=zout,/quiet)
          sp[t,r,*,0:nv0-1,0]=output.rad;*1000.
          sp_irrdn[t,r,*,0:nv0-1,0]=output.dir_dn+output.dif_dn
          sp_irrup[t,r,*,0:nv0-1,0]=output.dif_up
          fn='cld_ta'+string(t,format='(I02)')+'_re'+string(r,format='(I02)')+'_v1'
          outf=outdir+fn+'.out'
          fi=file_lines(outf) 
          if fi le 1 then goto, ef
          output=libradtran_reader(file=outf,/radiance,zout=zout,/quiet)
          sp[t,r,*,nv0:*,0]=output.rad;*1000.
          sp_irrdn[t,r,*,nv0:*,0]=output.dir_dn+output.dif_dn
          sp_irrup[t,r,*,nv0:*,0]=output.dif_up
;          spn[t,r,*,0]=interpol(sp[t,r,*,0],wvl,zenlambda)
;          spn_irrdn[t,r,*,0]=interpol(sp_irrdn[t,r,*,0],wvl,zenlambda)
;          spn_irrup[t,r,*,0]=interpol(sp_irrup[t,r,*,0],wvl,zenlambda)
print,outf
       endif

ef:
       if ref[r] ge 5. then begin
          fn='cloud_tb_ta'+string(t,format='(I02)')+'_re'+string(r,format='(I02)')+'_v0_ice'
          outf=outdir+fn+'.out'
          fi=file_lines(outf)
          if fi le 1 then continue
          output=libradtran_reader(file=outf,/radiance,zout=zout,/quiet)
          sp[t,r,*,0:nv0-1,1]=output.rad;*1000.
          sp_irrdn[t,r,*,0:nv0-1,1]=output.dir_dn+output.dif_dn
          sp_irrup[t,r,*,0:nv0-1,1]=output.dif_up
          fn='cloud_tb_ta'+string(t,format='(I02)')+'_re'+string(r,format='(I02)')+'_v1_ice'
          outf=outdir+fn+'.out'
          fi=file_lines(outf)
          if fi le 1 then continue
          output=libradtran_reader(file=outf,/radiance,zout=zout,/quiet)
          sp[t,r,*,nv0:*,1]=output.rad;*1000.
          sp_irrdn[t,r,*,nv0:*,1]=output.dir_dn+output.dif_dn
          sp_irrup[t,r,*,nv0:*,1]=output.dif_up
;          spn[t,r,*,1]=interpol(sp[t,r,*,1],wvl,zenlambda)
;          spn_irrdn[t,r,*,1]=interpol(sp_irrdn[t,r,*,1],wvl,zenlambda)
;          spn_irrup[t,r,*,1]=interpol(sp_irrup[t,r,*,1],wvl,zenlambda)
       endif
          print, fn
    endfor ;ref loop
  endfor  ;tau loop

;zenlambda=wvl;zenlambda[u]
save, wvl,z,pw,ab, tau, ref, sp, sp_irrdn,sp_irrup,sza,filename=dir+'/model/sp_'+vv+'_'+lbl+'_raw.out'
;sp=spn & sp_irrdn=spn_irrdn & sp_irrup=spn_irrup
;save, zenlambda, z,pw,ab,tau, ref, sp, sp_irrdn,sp_irrup,sza,filename=dir+'/model/sp_'+vv+'_'+lbl+'.out'
stop
end
