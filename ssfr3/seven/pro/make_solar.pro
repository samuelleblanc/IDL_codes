; program to build a solar file, based on kurudz, at SSFR3 wavelengths

pro make_solar

;restore, '/argus/SSFR3/data/20120813/out/20120813_calibspcs.out'

restore, '/home/leblanc/libradtran/aggregate_columns000.leg'
print, 'restoring'

;lambda=zenlambda
lambda=wvl
  F_o=read_ascii('/home/leblanc/libradtran/libRadtran-1.6-beta/data/solar_flux/kurudz_1.0nm.dat', comment_symbol='#',data_start=12)
  vis=read_ascii('/home/leblanc/libradtran/vis_1nm.dat')
  nir=read_ascii('/home/leblanc/libradtran/nir_1nm.dat')

  F_o.field1[1,*]=F_o.field1[1,*]/1000.
  vis.field1[1,*]=vis.field1[1,*]/total(vis.field1[1,*])
  nir.field1[1,*]=nir.field1[1,*]/total(nir.field1[1,*])
  fvis=fltarr(n_elements(F_o.field1[1,*]))
  fnir=fvis
  for i=7,n_elements(f_o.field1[0,*])-8 do for j=-7,7 do fvis[i]=fvis[i]+f_o.field1[1,i+j]*vis.field1[1,j]
  for i=15,n_elements(f_o.field1[0,*])-16 do for j=-15,15 do fnir[i]=fnir[i]+f_o.field1[1,i+j]*nir.field1[1,j]
;  fo=[interpol(fvis,F_o.field1[0,*],lambda[0:193]),interpol(fnir,F_o.field1[0,*],lambda[194:*])]

fo=[interpol(fvis,F_o.field1[0,*],lambda[0:5]),interpol(fnir,F_o.field1[0,*],lambda[6:*])]


;file='/home/leblanc/libradtran/solar_SSFR.dat'
file='/home/leblanc/libradtran/solar_eMAS.dat'
print, 'saving to '+file
openw, lun, file, /get_lun
for i=0, n_elements(fo)-1 do begin
  printf, lun, lambda[i],fo[i]
endfor
close, lun
stop


end
