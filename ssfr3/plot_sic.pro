; program to plot the sic from the modeled runs
pro plot_sic

dir='C:\Users\Samuel\Research\SSFR3\'
ll='all';'all'
dl='';'dref_'

restore, dir+'model\v1\sp_v1_20120525.out'
restore, dir+'data\v1\retr_mod_'+ll+'_ice_'+dl+'v1.out'
iref_err=ref_err & iref_rtm=ref_rtm
isic_par=sic_par & isic_par_ind=sic_par_ind
isic_ref_rtm=sic_ref_rtm & isic_rtm=sic_rtm
isic_tau_rtm=sic_tau_rtm & itau_err=tau_err
itau_rtm=tau_rtm & iwp_rtm=wp_rtm & ipar_arr=par_arr
restore, dir+'data\v1\retr_mod_'+ll+'_liq_v1.out'
par_arr[*,*,1,*]=ipar_arr[*,*,1,*] &  ref_err[*,*,1,*]=iref_err[*,*,1,*] & ref_rtm[*,*,1]=iref_rtm[*,*,1]
sic_par[*,*,1,*]=isic_par[*,*,1,*] &  tau_err[*,*,1,*]=itau_err[*,*,1,*] & tau_rtm[*,*,1]=itau_rtm[*,*,1]
sic_par_ind[*,*,1,*]=isic_par_ind[*,*,1,*] &  sic_tau_rtm[*,*,1]=isic_tau_rtm[*,*,1] & sic_ref_rtm[*,*,1]=isic_ref_rtm[*,*,1]
wp_rtm[*,*,1]=iwp_rtm[*,*,1] & sic_rtm[*,*,1]=isic_rtm[*,*,1]





set_plot, 'win'
loadct, 39
device, decomposed=0
window, 0, xsize=900
!p.multi=[0,2,1]
lvls=findgen(21)/20.
cl=lvls*250.
contour, sic_rtm[*,*,0],tau,ref,levels=lvls,c_color=cl,title='sic - liquid'
contour,sic_rtm[*,*,1],tau,ref,levels=lvls,c_color=cl,title='sic - ice'
p=tvrd(/true)
write_png, dir+'plots\v1\sic_'+ll+dl+'.png',p


; get bias
bref=ref_rtm
btau=tau_rtm
for t=0,n_elements(tau)-1 do for w=0,1 do bref[t,*,w]=(ref_rtm[t,*,w] -ref)/ref*100.
for r=0,n_elements(ref)-1 do for w=0,1 do btau[*,r,w]=(tau_rtm[*,r,w]-tau)/tau*100.
window, 1, xsize=900
lvls=(findgen(21)/10.-1.)*50.
contour, bref[*,*,0],tau,ref,levels=lvls,c_color=cl,title='ref bias - liquid'
contour,bref[*,*,1],tau,ref,levels=lvls,c_color=cl,title='ref bias - ice'
p=tvrd(/true)
write_png, dir+'plots\v1\bref_'+ll+dl+'.png',p
 

 window,2,xsize=900
contour, btau[*,*,0],tau,ref,levels=lvls,c_color=cl,title='tau bias - liquid'
contour,btau[*,*,1],tau,ref,levels=lvls,c_color=cl,title='tau bias - ice'
p=tvrd(/true)
write_png, dir+'plots\v1\btau_'+ll+dl+'.png',p
 

; get uncertainty
eref=ref_rtm
etau=tau_rtm
for t=0,n_elements(tau)-1 do for w=0,1 do eref[t,*,w]=abs(ref_err[t,*,w,0]-ref_err[t,*,w,1])/2./ref*100.
for r=0,n_elements(ref)-1 do for w=0,1 do etau[*,r,w]=abs(tau_err[*,r,w,0]-tau_err[*,r,w,1])/2./tau*100.
window, 4, xsize=900 
lvls=(findgen(21)/0.4)
contour, eref[*,*,0],tau,ref,levels=lvls,c_color=cl,title='ref error - liquid'
contour,eref[*,*,1],tau,ref,levels=lvls,c_color=cl,title='ref error - ice'
p=tvrd(/true) 
write_png, dir+'plots\v1\eref_'+ll+dl+'.png',p
  
 
 window,5,xsize=900 
contour, etau[*,*,0],tau,ref,levels=lvls,c_color=cl,title='tau error - liquid'
contour,etau[*,*,1],tau,ref,levels=lvls,c_color=cl,title='tau error - ice'
p=tvrd(/true) 
write_png, dir+'plots\v1\etau_'+ll+dl+'.png',p




stop




end
