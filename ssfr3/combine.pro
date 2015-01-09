; program to combine the output of retrieve_pdfs

pro combine
dir='/projects/leblanse/cloud/data/'

;restore, dir+'retr_mod_ab_iceps_v3.out'
restore, dir+'retr_mod_all_ice_drefps_v3.out'
ti=tau_rtm & ri=ref_rtm & tir=tau_err & rir=ref_err & pmi=post_meas_pdf & wi=wp_rtm 
spi=sic_par & sii=sic_par_ind & si=sic_rtm & sri=sic_ref_rtm & sti=sic_tau_rtm

;restore, dir+'retr_mod_ab_liqps_v3.out'
restore, dir+'retr_mod_all_liq_drefps_v3.out'
tau_rtm[*,*,1]=ti[*,*,1] & ref_rtm[*,*,1]=ri[*,*,1] & tau_err[*,*,1,*]=tir[*,*,1,*] & ref_err[*,*,1,*]=rir[*,*,1,*]
post_meas_pdf[*,*,1,*,*,*]=pmi[*,*,1,*,*,*] & wp_rtm[*,*,1]=wi[*,*,1] & sic_par[*,*,1,*]=spi[*,*,1,*]
sic_par_ind[*,*,1,*]=sii[*,*,1,*] & sic_rtm[*,*,1]=si[*,*,1] & sic_ref_rtm[*,*,1]=sri[*,*,1] & sic_tau_rtm[*,*,1]=sti[*,*,1]

save, tau_rtm,ref_rtm,tau_err,ref_err,post_meas_pdf,wp_rtm,sic_par,sic_par_ind,sic_rtm,sic_tau_rtm,sic_ref_rtm,taum,refm,$
filename=dir+'retr_dref_v3.out' ;  
;filename=dir+'retr_ct_v3.out'

stop


end
