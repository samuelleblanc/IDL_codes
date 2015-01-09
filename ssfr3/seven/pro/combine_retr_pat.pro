; program to make a out file that has the same format as the retrieved_pdfs_date_2wvl.out
; but uses the output from Patrick's retrieval that was run in realtime

pro combine_retr_pat,date

;date='20120523'

dates=['20120523','20120525','20120602','20120806','20120813','20120816','20120820','20120824','20120912','20130110','20130111']
for d=0,n_elements(dates)-1 do begin
;d=1
date=dates[d]
print, 'doing :'+date
dir='/argus/roof/SSFR3/retrieved/' ;'/home/leblanc/SSFR3/data/'
print, 'restoring'
restore, dir+'retrieved_pdf_'+date+'_pat.out'
tmhrsi=tmhrs

restore, '/argus/roof/SSFR3/data/'+date+'/out/'+date+'_cld_parms2_v4.out' ;_sp20.out'
;tau_rtm=interpol(tau,tmhrs,tmhrsi)
;ref_rtm=interpol(ref,tmhrs,tmhrsi)

tau_rtm=fltarr(n_elements(tmhrsi))
ref_rtm=fltarr(n_elements(tmhrsi))

;now calculate the uncertainty
dr=[0.712,0.518,0.143,0.14,0.2]
dt=[0.149,0.068,0.023,0.03,0.2]
dtau=[10,20,40,60,200]
dt1=15
;ref_err[2,num], bottom, top
for i=0, n_elements(tmhrsi)-1 do begin
  nul=min(abs(tmhrs-tmhrsi[i]),it)
  if i eq 0 then dt0=0 else dt0=15
  if it+dt1 gt n_elements(tau)-1 then dt1=n_elements(tau)-1-it else dt1=15
 ; tau_rtm[i]=tau[it] ;mean(tau[it-dt0:it+dt1],/nan)
 ; ref_rtm[i]=tau[it] ;mean(ref[it-dt0:it+dt1],/nan)
  tau_rtm[i]=mean(tau[it-dt0:it+dt1],/nan)
  ref_rtm[i]=mean(ref[it-dt0:it+dt1],/nan) 
  if finite(tau_rtm[i]) ne 1 then message,'problem with non finite tau'

  ref_err[0,i]=ref_rtm[i]-interpol(dr*ref_rtm[i],dtau,tau_rtm[i])
  ref_err[1,i]=ref_rtm[i]+interpol(dr*ref_rtm[i],dtau,tau_rtm[i])
  tau_err[0,i]=tau_rtm[i]-interpol(dt*tau_rtm[i],dtau,tau_rtm[i])
  tau_err[1,i]=tau_rtm[i]+interpol(dt*tau_rtm[i],dtau,tau_rtm[i])
endfor

nul=where(finite(tau_rtm) eq 1 and finite(ref_rtm) eq 1,good)
wp_rtm=tau_rtm*0.+1.
wp_rtm[nul]=0.
wp_err=wp_rtm

tmhrs=tmhrsi
print, 'saving'
save, filename=dir+'retrieved_'+date+'_pat_v4.out',tmhrs,tau_rtm,ref_rtm,ref_err,tau_err,wp_rtm,wp_err
;stop
endfor
end
