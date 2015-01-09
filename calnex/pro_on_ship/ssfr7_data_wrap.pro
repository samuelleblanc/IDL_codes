pro ssfr7_data_wrap

HOME = '/home/calnex/atlantis/'
!PATH = HOME+'/pro/:' + !PATH

base_dir = HOME + 'raw/'
cd, base_dir

filename = 'input_parameters.txt'

indir=""
date=""
openr,lun,filename,/get_lun
readf,lun,indir,date,doplot
close,lun
free_lun,lun

if(not(doplot)) then begin
    Set_Plot,'NULL'
endif

ssfr7_data,indir,date,doplot
end
