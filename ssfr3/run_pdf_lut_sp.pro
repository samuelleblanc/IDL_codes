;program to run the pdf_lut_sp procedure
; its for splitting into multiple processes
; because monte carlo processing is hard :(

@support_pdfs_v2.pro
@get_params_opm.pro

pro run_pdf_lut_sp, is,ice=ice
case is of
  0:tr=[0,9]
  1:tr=[10,19]
  2:tr=[20,29]
  3:tr=[30,39]
  4:tr=[40,49]
  5:tr=[50,59]
  6:tr=[60,69]
  7:tr=[70,79]
  8:tr=[80,89]
  9:tr=[90,99]
end

if n_elements(ice) lt 1 then ice=0
namelut='/projects/leblanse/cloud/data/sp_std_day_v1.out'
namelut='C:\Users\Samuel\Research\SSFR3\data\sp_std_day_v1.out'

if ice then begin
restore, namelut+'_ice.out'
pari=pdf_lut_sp(spi, sp_stdi, wvl,bmaxi,bmini,bins=bini,nb=nb,nn=20000,tr=tr)

fp='/projects/leblanse/cloud/data/pars_pdf_day_v1_'+strtrim(is,2)+'_ice.out'
print, 'saving to:'+fp
save,filename=fp, binsi,tau,refi,pari
endif else begin
restore, namelut+'_liq.out'
parl=pdf_lut_sp(spl, sp_stdl, wvl,bmaxl,bminl,bins=binl,nb=nb,nn=20000,tr=tr)

fp='/projects/leblanse/cloud/data/sp_std_day_v1_'+strtrim(is,2)+'_liq.out'
print, 'saving to:'+fp
save, filename=fp,binsl,tau,refl,parl
endelse


end
