;simple file to make multiple files input

pro print_sza_file,lbl=lbl

dir='/projects/leblanse/cloud/idls/'
if n_elements(lbl) lt 1 then lbl='20120913'
uu=97
openw,uu,dir+'idl_sza_'+lbl+'.sh'

for i=0, 19 do begin
openw,lun,dir+'idl_sza_'+lbl+string(i,format='(I02)')+'.sh',/get_lun
printf,lun,'.r /projects/leblanse/cloud/pro/zensun.pro'
printf,lun,'.r /projects/leblanse/cloud/pro/make_ic_files.pro'
printf,lun,'.r /projects/leblanse/cloud/pro/make_lc_files.pro'
printf,lun,'.r /projects/leblanse/cloud/pro/write_inp_mix.pro'
printf,lun,'.r /projects/leblanse/cloud/pro/build_sza_'+lbl+'.pro'
printf,lun,'build_sp_mix3_sza,mu='+string(i,format='(I2)')
close,lun
free_lun,lun
printf,uu,'idl < '+dir+'idl_sza_'+lbl+string(i,format='(I02)')+'.sh > '+dir+'idl_sza_'+lbl+string(i,format='(I02)')+'.out'
print,'idl < '+dir+'idl_sza_'+lbl+string(i,format='(I02)')+'.sh > '+dir+'idl_sza_'+lbl+string(i,format='(I02)')+'.out'
endfor
close,uu
free_lun,uu
end
