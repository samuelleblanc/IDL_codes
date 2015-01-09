; program to make the avg and std for each parameter, then build

@get_params_mix.pro
pro make_lut_pdf_mix_sza

dir='/argus/roof/SSFR3/model/'
f=file_search(dir+'mix_hi_sza_*.out')
nf=n_elements(f)
if nf lt 1 then message, 'no file found'

f=f[sort(f)]
n=0
stop

restore, f[0]
rads=fltarr(n_elements(tau_hires),n_elements(refl_hires),n_elements(refi_hires),n_elements(wp),n_elements(mus),nf)
for i=0, nf-1 do begin
  print, 'restoring :'+f[i]
  restore, f[i]
  rads[*,*,*,*,*,i]=rad_hi
endfor
;n=0 ; lvls_20120524 
rad=rads[*,*,*,*,*,n]
stdr=stddev(rads,dimension=6)

tau=tau_hires
refl=refl_hires
refi=refi_hires

save, rad,stdr,tau,refl,refi,wp,mus,filename=dir+'sza_mix_std.out'

;stop
np=n_elements(rads[0,0,0,0,*,0])
nb=1000
binsr=fltarr(np,nb)
;pars_pdf=fltarr(n_elements(tau_hires),n_elements(refl_hires),n_elements(refi_hires),n_elements(wp),np,nb)

;save, avg,std,bins,tau,refi,refl,wp,filename=dir+'pars_pdf_mix.out'
;stop
;loop through each parameter to build the pdfs
for p=0, np-1 do begin
  pmin=min(rads[*,*,*,*,p])
  pmax=max(rads[*,*,*,*,p])
  dp=(pmax-pmin)/(nb*0.6)
  binsr[p,*]=findgen(nb)*dp+pmin-(dp*nb*0.3) ;make the bins larger than the extent of the modeled parameter by 22.5% on either side

print, p
endfor
save, rad,stdr,binsr,tau,refi,refl,wp,filename=dir+'sza_pdf_mix.out'
stop


 
;  ;now loop through all the lut to calculate the normalized pdf
;  for t=0, n_elements(tau)-1 do begin
;    for rl=0, n_elements(refl)-1 do begin
;      for ri=0, n_elements(refi)-1 do begin
;        for w=0, n_elements(wp)-1 do begin
;          mu=avg[t,rl,ri,w,p]
;          if std[t,rl,ri,w,p] lt bins[p,1]-bins[p,0] then sig=bins[p,1]-bins[p,0] else sig=std[t,rl,ri,w,p] ; set the error of the pdf to be the standard deviation from the restored file.
;          pars_pdf[t,rl,ri,w,p,*]=exp((bins[p,*]-mu)^(2.)/(-2.*sig^(2.)))/(sig*sqrt(2.*!PI))
;          k=total(pars_pdf[t,rl,ri,w,p,*]) ;get the normalization coefficient
;          if finite(k) ne 1 then message, 'k is not finite'
;          pars_pdf[t,rl,ri,w,p,*]=pars_pdf[t,rl,ri,w,p,*]/k  ;now normalize
;        endfor ;w loop
;      endfor ; refi loop
;    endfor ; refl loop
;  endfor ;tau loop
;endfor


s;ave, pars_pdf,bins,tau,refi,refl,wp,filename=dir+'pars_pdf_mix.out'

stop
end
