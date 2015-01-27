;simple file to make multiple files input

pro print_sza_file,lbl=lbl

dir='/projects/leblanse/cloud/idls/read/'
if n_elements(lbl) lt 1 then lbl='20120913'
uu=97
openw,uu,dir+'idl_read_sza_'+lbl+'.sh'

for i=0, 19 do begin
  openw,lun,dir+'idl_read_sza_'+lbl+string(i,format='(I02)')+'.sh',/get_lun
  printf,lun,'.r /projects/leblanse/cloud/pro/zensun.pro'
  printf,lun,'.r /projects/leblanse/libradtran/pro/libradtran_reader.pro'
  printf,lun,'.r /projects/leblanse/cloud/pro/read_sza_mix3.pro'
  printf,lun,'read_sza_mix3,mu='+string(i,format='(I2)')+", '"+lbl+"'"
  close,lun
free_lun,lun
printf,uu,'idl < '+dir+'idl_read_sza_'+lbl+string(i,format='(I02)')+'.sh > '+dir+'idl_read_sza_'+lbl+string(i,format='(I02)')+'.out'
print,'idl < '+dir+'idl_read_sza_'+lbl+string(i,format='(I02)')+'.sh > '+dir+'idl_read_sza_'+lbl+string(i,format='(I02)')+'.out'
endfor
close,uu
free_lun,uu
end
