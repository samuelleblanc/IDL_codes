; program to run through multiple measured spectra. Calculates the post pdf for each spectra in ref, tau, and phase space
; calls the outside program run_pdfs (which is the actual GENRA code)
; uses the data measured from SSFR3 on the roof.

@run_pdfs_v7.pro
;@get_params_test2.pro
@get_params4.pro
@make_meas_pdf.pro

pro retrieve_pdfs

dir='C:\Users\Samuel\Research\SSFR3\'

model=1
test=0
sno=0
ps=[6,5,2,3,11,12,14,7]-1 ;limited p range, indices, not parameters
limp=0 ;use the limited p range

ps=ps[sort(ps)] ; sort the bands

;restore files to use with make_meas_pdf
print, 'restoring files for make_meas_pdf'
restore, '/home/leblanc/SSFR3/data/dark_sample.out'
dark_s={z_dk:z_dk,zenlambda:zenlambda}
restore, '/home/leblanc/SSFR3/data/resps.out'
resps={zenlambda:zenlambda,zresp1:zresp1,zresp2:zresp2,zresp3:zresp3,zresp4:zresp4}

;load up the measured spectra
;datesr=['20120523','20120525','20120602','20120806','20120813','20120816','20120820','20120824','20120912','20130110','20130111']
;sn=[0,0,0,0,0,0,0,0,0,1,1]
;for ki=9, n_elements(datesr)-1 do begin
;ki=1
;  sno=sn[ki]
;  date=datesr[ki]

;if not model then print, 'restoring data for day: '+date
;dir='/home/leblanc/SSFR3/'
;date='20120525'
; restore the measured data
;restore, '/argus/roof/SSFR3/data/'+date+'/out/'+date+'_calibspcs.out'

jj=0

if model then begin
  fp=dir+'model\v1\sp_v1_20120525.out'
  print, 'restoring modeled out file'
  restore, fp
  ind=indgen(n_elements(taus),n_elements(())  

  nul=min(abs(ref-25),nrm)
  fl=findgen(n_elements(sp[*,*,0,*]))
  spectram=fltarr(n_elements(fl),n_elements(lambda))
  realm=fltarr(n_elements(fl),3)
  print, 'building the set of spectra'
  j=0
  wp=[0.,1.0]
  for w=0, n_elements(wp)-1 do begin
    for t=0, n_elements(tau)-1 do begin
      if w eq 0 then for l=0,n_elements(lambda)-1 do sp[t,*,l,w]=interpol(sp[t,0:nrm,l,w],ref[0:nrm],ref)
      for r=0, n_elements(ref)-1 do begin
        spectram[j,*]=reform(sp[t,r,*,w],1,n_elements(lambda))
        realm[j,*]=[[tau[t],ref[r],float(w)]]
        j=j+1
        print, w,t,r,max(spectram[j-1,*]),max(sp[t,r,*,w])
;        if tau[t] eq 150 and ref[r] eq 20. and w eq 0 then jj=j-1
      endfor ; ref loop
    endfor ;tau loop
  endfor ; wp loop
refo=ref
;stop
endif else begin
  case date of
    '20120602':fl=where(tmhrs gt 21.  and tmhrs lt 23.);  and area2a gt 10  and areaa  gt -2)
    '20120813':fl=where(tmhrs gt 15.  and tmhrs lt 19.);  and area2b gt 10)
    '20120525':fl=where(tmhrs gt 15.  and tmhrs lt 16.);  and tauc   lt 200 and area2c gt 10.)
    '20120523':fl=where(tmhrs gt 21.  and tmhrs lt 24.);  and taud   lt 200 and area2d gt 30.)
    '20120912':fl=where(tmhrs gt 15.5 and tmhrs lt 18.5); and area2e gt 40  and area2e lt 100. and areae gt 5)
    '20120806':fl=where(tmhrs gt 22.  and tmhrs lt 24.5); used to be 20 to 24.5
    '20120816':fl=where(tmhrs gt 13.5 and tmhrs lt 17.)
    '20120820':fl=where(tmhrs gt 16.  and tmhrs lt 20.);  and area2h gt 10. and areah  lt 4.)
    '20120824':fl=where(tmhrs gt 19.5 and tmhrs lt 23.);  and area2i gt 25)
    '20130110':fl=where(tmhrs gt 15.  and tmhrs lt 22.)
    '20130111':fl=where(tmhrs gt 21.  and tmhrs lt 23.)
    else: message, 'wrong date'
  endcase
  days=[20120602,20120813,20120525,20120523,20120912,20120806,20120816,20120820,20120824]
endelse
num=n_elements(tmhrs[fl]) ; the total number of points to go through
wvl=zenlambda

; restore the parameters lut pdf
print, 'restoring LUT'
restore, '/argus/roof/SSFR3/model/Pars_pdf_LUT_v3.out'

if not limp then npar=n_elements(pars_pdf[0,0,0,*,0])-1 else npar=n_elements(ps)
if not limp then ps=indgen(npar)
nbin=n_elements(bins[0,*])
sz_bin=bins[*,1]-bins[*,0]


;set up the variable to keep the retrieved values
tau_rtm=fltarr(num)
ref_rtm=fltarr(num)
wp_rtm=fltarr(num)
; the shannon information content
H_rtm=fltarr(num)
Hi_rtm=fltarr(npar,num)
Hm_rtm=fltarr(npar,num)
; the variables holding the uncertainties
tau_err=fltarr(2,num) ;lower bound, upper bound
ref_err=fltarr(2,num) ; lower bound, upper bound
wp_err=fltarr(num)

ic_prob=fltarr(2,npar,num)
li_prob=fltarr(2,npar,num)

;make arrays to store the spectra
if test gt 0 then wvl=lambda
if model then wvl=lambda
sp_arr=fltarr(n_elements(wvl),num)
par_arr=fltarr(n_elements(pars_pdf[0,0,0,*,0]),num)

post=fltarr(num,n_elements(taus),n_elements(ref),n_elements(wp))
post_pdf=fltarr(num,n_elements(taus),n_elements(ref),n_elements(wp),npar)
; restore the pdf for each measurement value
;print, 'restoring measurement pdf'
;restore, dir+'data/pars_error_v2.out'

; pars_pdf_meas and pars
; m_pdf=pars_pdf_meas
; meas_pdf=bins*0.d
; now start the main loop over time
if model then di=1 else di=30
for i=jj, num-1,di do begin
  if test gt 0 then begin
    sp=spectra
    wvl=lambda
  endif else begin
    if model then begin
      sp=reform(spectram[i,*])
      wvl=lambda
      if max(sp) eq 0. then continue
    endif else begin
     ;sp=mean(zspectra[*,fl[i:((i+di gt num-1)?num-1:i+di)]],dimension=2)
     sp=zspectra[*,fl[i]]
     spd=stddev(zspectra[*,fl[i:((i+di gt num-1)?num-1:i+di)]]/max(sp),dimension=2) 
   endelse
  endelse
  ; put in the spectrum into the spectra array
  sp_arr[*,i]=sp
  ; build the measurement pdf
  get_params, wvl, sp, par
  par_arr[*,i]=par
  make_meas_pdf, sp, wvl, bins, meas_pdf, sno, ps=ps,dark_s=dark_s,resps=resps
  
  if (i mod 1) eq 0 then print, 'On the ',i,' iteration of ',num

;; now run the pdfs ;;
  if finite(total(pars_pdf)) eq 0 then message, 'pars_pdf is not a finite number'
  run_pdfs,pars_pdf,bins,taus,ref,wp,meas_pdf,post_tau,post_ref,post_wp,H,H_ind,H_par,terr,rerr,werr,posttm,$
   ic_prob=icp,li_prob=lip,ps=ps,post_pdf=post_pdftm
  post[i,*,*,*]=posttm
  tau_rtm[i]=post_tau
  ref_rtm[i]=post_ref
  wp_rtm[i]=post_wp
  H_rtm[i]=H
  Hi_rtm[*,i]=H_ind
  Hm_rtm[*,i]=H_par
  tau_err[*,i]=terr
  ref_err[*,i]=rerr
  wp_err[i]=werr
  ic_prob[*,*,i]=icp
  li_prob[*,*,i]=lip
  post_pdf[i,*,*,*,*]=post_pdftm
if jj gt 0 then stop
endfor ; end of time loop

tmhrs=tmhrs[fl]
if test lt 1 and model lt 1 then begin
  is=indgen(num)
  kl=where(is mod 30 eq 0)
  post=post[kl,*,*,*]     & tau_rtm=tau_rtm[kl]   & ref_rtm=ref_rtm[kl] & wp_rtm=wp_rtm[kl]
  tmhrs=tmhrs[kl]         & H_rtm=H_rtm[kl]       & Hi_rtm=Hi_rtm[*,kl] & Hm_rtm=Hm_rtm[*,kl]
  tau_err=tau_err[*,kl]   & ref_err=ref_err[*,kl] & wp_err=wp_err[kl]
  ic_prob=ic_prob[*,*,kl] & li_prob=li_prob[*,*,kl]
  sp_arr=sp_arr[*,kl]     & par_arr=par_arr[*,kl] & post_pdf=post_pdf[kl,*,*,*,*]
  nans=where((tau_rtm le 1. or tau_rtm ge 198.) or (ref_rtm le 2. or ref_rtm ge 50.),ct)
  if ct gt 0 then begin
    tau_rtm[nans]=!values.f_nan & ref_rtm[nans]=!values.f_nan & wp_rtm[nans]=!values.f_nan
  endif
endif
stop
if limp then lp='_limp2' else lp=''

if test eq 1 then fn=dir+'data/retrieved_pdf_'+date+'_test'+lp+'.out' else $
 if test eq 2 then fn=dir+'data/retrieved_pdf_'+date+'_test2'+lp+'.out' else $
  if model then fn=dir+'data/retrieved_pdf_'+date+'_model'+lp+'_v2.out' else $
   fn=dir+'data/retrieved_pdf_'+date+lp+'_v3.out'
if model then ref=refo  
if model then save, tau_rtm,ref_rtm,wp_rtm,H_rtm,Hi_rtm,Hm_rtm,tau_err,ref_err,wp_err,real,post,spectram,wvl, realm,tau,ref,wp,ic_prob,li_prob,sp_arr,par_arr,wvl,post_pdf,filename=fn else $
save, tmhrs, tau_rtm,ref_rtm,wp_rtm,H_rtm,Hi_rtm,Hm_rtm,tau_err,ref_err,wp_err,real,post,ic_prob,li_prob,sp_arr,par_arr,wvl,post_pdf,filename=fn
if model or test gt 0 then break
;endfor
stop
end


;;;;; function to make a pdf of the bins of the parameter and std
function pdf_lut, avg,std,bmax,bmin,bins=bins

nt=n_elements(avg[*,0,0,0])
nr=n_elements(avg[0,*,0,0])
nw=n_elements(avg[0,0,*,0])
np=n_elements(avg[0,0,0,*])
nb=1000

pdf=fltarr(nt,nr,nw,np,nb)

; now transform the lut into a lut with pdf with 100 bins for each parameter
bins=fltarr(np,nb)

print, 'starting to build the pars pdf'
;loop through each parameter to build the pdfs
for p=0, np-1 do begin
  dp=(dmax[p]-dmin[p])/nb
  bins[p,*]=findgen(np)*dp+dmin[p] ;make the bins larger than the extent of the modeled parameter by 22.5% on either side


  ;now loop through all the lut to calculate the normalized pdf
  for t=0, nt-1 do begin
    for r=0, nr-1 do begin
      for w=0, nw-1 do begin
      mu=avg[t,r,w,p]
      if std[t,r,w,p] lt bins[p,1]-bins[p,0] then sig=bins[p,1]-bins[p,0] else sig=std[t,r,w,p] ; set the error of the pdf to be the standard deviation from the restored file.
      pdf[t,r,w,p,*]=exp((bins[p,*]-mu)^(2.)/(-2.*sig^(2.)))/(sig*sqrt(2.*!PI))
      k=total(pdf[t,r,w,p,*]) ;get the normalization coefficient
      if finite(k) ne 1 then message, 'k is not finite'
      pdf[t,r,w,p,*]=pdf[t,r,w,p,*]/k  ;now normalize
      endfor
    endfor
  endfor
endfor

return,pdf
end

