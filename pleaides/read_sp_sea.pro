; program to read out the results of sp_sea modelling

@~/libradtran/pro/libradtran_reader.pro

pro read_sp_sea

dirout='/lustre/janus_scratch/leblanse/cloud/output/sp_sea/'
;f=file_search(dirout+'model*.out')

tau=[3.,5.,8.,10.,15.,20.,60.,90.]
ref=[5.,10.,15.,20.,25.]
wp=[1,0]

ntau=n_elements(tau)
nref=n_elements(ref)
nwp=n_elements(wp)
sp_ice=fltarr(ntau,nref,13001)
for t=0, ntau-1 do begin
  for r=0, nref-1 do begin
    fn=dirout+'cloud_tb_ta'+string(t,format='(I02)')+'_re'+string(r,format='(I02)')+'_ice.out'
    print, 'reading file: '+fn
    d=libradtran_reader(file=fn,zout=[0.35],/rad,/quiet)
    wvl=reform(d.wvl)
    sp_ice[t,r,*]=reform(d.rad)
  endfor
endfor

save, sp_ice, tau, ref, wvl, filename='$H2/cloud/model/sp_sea_ice.out'

stop
end

