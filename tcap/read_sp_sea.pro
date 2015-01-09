; program to read out the results of sp_sea modelling

@~/libradtran/pro/libradtran_reader.pro

pro read_sp_sea

dirout='~/libradtran/output/cloud/'
;f=file_search(dirout+'model*.out')

tau=[5.,20.,60.,90.]
ref=[5.,15.,20.,25.]
wp=[1,0]

ntau=n_elements(tau)
nref=n_elements(ref)
nwp=n_elements(wp)
sp_liq=fltarr(ntau,nref,1301)
for t=0, ntau-1 do begin
  for r=0, nref-1 do begin
    fn=dirout+'cloud_t'+strtrim(t+1,2)+'_r'+strtrim(r+1,2)+'.out'
    print, 'reading file: '+fn
    d=libradtran_reader(file=fn,zout=[0.35],/rad,/quiet)
    wvl=reform(d.wvl)
    sp_liq[t,r,*]=reform(d.rad)
  endfor
endfor

save, sp_liq, tau, ref, wvl, filename='~/TCAP/model/sp_sea_liq.out'

stop
end
