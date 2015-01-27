; program to read in the values outputted by the uvspec program on the janus cluster
; used to build the look up tables for radiance/irradiance retrieval of aerosol properties at Skywatch

@zensun.pro
@/home/leblanse/libradtran/pro/libradtran_reader.pro

pro read_io

;set the proper directories

outdir='/scratch/stmp00/leblanse/cloud_rad/output/'
dir   ='/projects/leblanse/cloud_rad/'

;set the albedo
wvls=[515,1565,1571,1577,1582,1588,1594,1600,1605,1611,1617,1623,1628,1634]
alb =[0.0511493,0.171991,0.176302,0.180426,0.18360,0.188400,0.192140,0.196528,$
      0.201539,0.205835,0.208625,0.212767,0.216909,0.219916]

;make arrays of tau and reff
;constant cloud height and thickness of 1km ; 1km

;taus = [0.5,1.,2.,5.,7.5,10.,20.,40.,80.,100.]
;refs = [3.,5.,8.,10.,15.,20.,25.]
;taus = [2.,5.,7.5,10.,20.,30.,40.,60.,80.,100.,120.,140.,160.,200.]
;refs = [1.,3.,5.,8.,10.,12.,15.,18.,20.,22.,25.]
taus = [2.,5.,7.5,10.,20.,30.,40.,60.,80.,100.,120.,140.,160.,200.]
refs = [1.,2.,3.,4.,5.,7.5,10.,12.,15.,18.,20.,22.,25.]

szas = findgen(18)*5.
doy=270 ;20120926

ntau=n_elements(taus)
nref=n_elements(refs)
nsza=n_elements(szas)
nwvl=n_elements(wvls)

;set the constants
date='20120926'

;coordinates for skywatch observatory (top of Duane, Boulder)
lat=40.007916667
lon=-105.26825

;set up the model run
zout=[0.02,5.0,30.0]

rad=fltarr(3,nwvl,ntau,nref,nsza)
irr=fltarr(3,nwvl,ntau,nref,nsza)
irr_df=fltarr(3,nwvl,ntau,nref,nsza)
irr_up=fltarr(3,nwvl,ntau,nref,nsza)


for i=0, nsza-1 do begin
  for j=0, ntau-1 do begin
    for k=0, nref-1 do begin
      for l=0, nwvl-1 do begin
        fn='cloud_sza'+string(szas[i],format='(I02)')+'_tau'+string(taus[j],format='(F05.1)')+$
           '_ref'+string(refs[k],format='(I02)')+'_wvl'+string(wvls[l],format='(I04)')
        outf=outdir+fn+'.out'
        output=libradtran_reader(file=outf,/radiance,zout=zout,/quiet)
        print, szas[i],taus[j],refs[k],wvls[l]
        rad[*,l,j,k,i]=output.rad
        irr[*,l,j,k,i]=output.dif_dn+output.dir_dn
        irr_up[*,l,j,k,i]=output.dif_up
        irr_df[*,l,j,k,i]=output.dif_dn
      endfor
    endfor
  endfor
endfor

save, rad,irr,irr_df,irr_up,alb,wvls,doy,szas,refs,taus,filename=dir+'CLD_LIQ_LUT_BOULDER_sun5.out'

stop
end
