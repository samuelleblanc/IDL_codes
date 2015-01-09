; program to run through multiple measured spectra. Calculates the post pdf for each spectra in ref, tau, and phase space
; calls the outside program run_pdfs (which is the actual GENRA code)
; uses the data measured from SSFR3 on the roof.

@run_pdfs_v7.pro
;@get_params_test2.pro
@get_params.pro
@make_meas_pdf.pro


pro retrieve_pdfs
model=1
test=0
sno=0
;ps=[6,5,2,3,11,15]-1 ;limited p range, indices, not parameters
ps=[6,5,2,3,11,12,14,7,15]-1
;ps=[4,6,10]

limp=0 ;use the limited p range
;load up the measured spectra
;load up the measured spectra
datesr=['20120523','20120525','20120602','20120806','20120813','20120816','20120820','20120824','20120912','20130110','20130111']
sn=[0,0,0,0,0,0,0,0,0,1,1]
for ki=0, n_elements(datesr)-1 do begin
;ki=1
  sno=sn[ki]
  date=datesr[ki]

if not model then print, 'restoring data for day: '+date
dir='/home/leblanc/SSFR3/'
;date='20120525'
; restore the measured data
restore, '/argus/roof/SSFR3/data/'+date+'/out/'+date+'_calibspcs.out'

if test gt 0 then begin
  ; for testing
  ; use a model spectra input
  ; restore the spectra
  print, 'in testing mode, restoring a modeled out file'
  restore, '/argus/roof/SSFR3/model/sp_hires_20120524.out'
  t=11 & r=1
  spectra=reform(sp[11,1,*,0])
  real='tau:'+strtrim(tau[t],2)+' ref:'+strtrim(ref[r],2)+' wp: liquid'
  lambda=zenlambda
  print, 'real '+real
endif

if model then begin
  print, 'restoring modeled out file'
  restore, '/argus/roof/SSFR3/model/sp_hires_20120524.out'
;stop  
;t=5 & r=1
 ; spectra=reform(sp[t,r,*,0])
 ; real='tau:'+strtrim(tau[t],2)+' ref:'+strtrim(ref[r],2)+' wp: liquid'
  lambda=zenlambda
 ; print, 'real '+real
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
;        print, w,t,r,max(spectram[j-1,*]),max(sp[t,r,*,w])
      endfor ; ref loop
    endfor ;tau loop
  endfor ; wp loop
refo=ref
fl=15*9+5
fl=15*17+5
;fl=15*21+12
;fl=15*21+12+360
;fl=15*13+7
;fl=360+15*2+12
fl=360+15*8+12 ; tau 20, ref 40, ice
fl=17*15+5+360
fl=8*15+5+360
fl=3*15+5+360
fl=17*15+12+360
fl=8*15+12+360
spectram[0,*]=spectram[fl,*]
print, realm[fl,*]
stop
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
if test eq 1 then restore, dir+'data/Pars_pdf_LUT_summer_test.out' else $
 if test eq 2 then restore, dir+'data/Pars_pdf_LUT_summer_test2.out' else $
  if sno then restore, '/argus/roof/SSFR3/model/Pars_pdf_LUT_snow.out' else $
   restore, '/argus/roof/SSFR3/model/Pars_pdf_LUT.out'

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
for i=0, num-1,di do begin
  if test gt 0 then begin
    sp=spectra
    wvl=lambda
  endif else begin
    if model then begin
      sp=reform(spectram[i,*])
      wvl=lambda
      if max(sp) eq 0. then continue
    endif else begin
     sp=mean(zspectra[*,fl[i:((i+di gt num-1)?num-1:i+di)]],dimension=2)
     spd=stddev(zspectra[*,fl[i:((i+di gt num-1)?num-1:i+di)]]/max(sp),dimension=2) 
   endelse
  endelse
  ; put in the spectrum into the spectra array
  sp_arr[*,i]=sp
  ; build the measurement pdf
  get_params, wvl, sp, par
  par_arr[*,i]=par
  make_meas_pdf, sp, wvl, bins, meas_pdf, sno, ps=ps,std=std

  save, bins,meas_pdf, filename='~/SSFR3/data/meas_pdf_liq.out'

print, par
print, std

stop
;  for pi=0, npar-1 do begin
;    p=ps[pi]
;    if test gt 0 then begin
;      mu=par[p]
;      if test eq 1 then if p eq 0 then sig=par[p]*0.05 else sig=par[p]*0.001 else $
;        sig=par[p]*0.05
;      if sig lt bins[p,1]-bins[p,0] then sig=bins[p,1]-bins[p,0]
;      meas_pdf[p,*]=exp((bins[p,*]-mu)^(2.)/(-2.*sig^(2.)))/(sig*sqrt(2.*!PI))
;    endif else begin
;      m_pdf[*,p]=pars_pdf_meas[*,p]+par[p]
;      meas_pdf[p,*]=histogram(m_pdf[*,p],binsize=sz_bin[p],nbins=nbin,min=bins[p,0]-sz_bin[p]/2.)
;    endelse
;    ;now normalized
;    ;k=int_tabulated(bins[p,*],meas_pdf[p,*]) ;get the normalization coefficien 
;    k=total(meas_pdf[p,*])
;    if k eq 0. then k=0.1 ;message, 'problem with normalization in measurement pdf'
;    meas_pdf[p,*]=meas_pdf[p,*]/k  ;now normalize
;  endfor
;save, ps,meas_pdf, bins, filename=dir+'data/meas_pdf_ex.out'

;stop  
  ;now run the pdfs
  if (i mod 1) eq 0 then print, 'On the ',i,' iteration of ',num
  ;if i gt 60 then begin
  ;wset,0
  ;plot, wvl, spd, title='standard deviation'
  ;wset,1
  ;plot, wvl, sp, title='normalized radiance'
  ;for j=i,i+di do oplot, wvl, zspectra[*,fl[j]]/max(zspectra[*,fl[j]])
  ;stop
  ;endif

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
  print, 'finished the run_pdfs'
stop
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
  nans=where(tau_rtm le 1. || tau_rtm ge 198. || ref_rtm le 2. || ref_rtm ge 50.,ct)
  if ct gt 0 then begin
    tau_rtm[nans]=!values.f_nan & ref_rtm[nans]=!values.f_nan & wp_rtm[nans]=!values.f_nan
  endif
endif

stop

if limp then lp='_limp' else lp=''

if test eq 1 then fn=dir+'data/retrieved_pdf_'+date+'_test'+lp+'.out' else $
 if test eq 2 then fn=dir+'data/retrieved_pdf_'+date+'_test2'+lp+'.out' else $
  if model then fn=dir+'data/retrieved_pdf_test_'+date+'_model'+lp+'_v2.out' else $
   fn=dir+'data/retrieved_pdf_test_'+date+lp+'_v2.out'
if model then ref=refo  
if model then save, tau_rtm,ref_rtm,wp_rtm,H_rtm,Hi_rtm,Hm_rtm,tau_err,ref_err,wp_err,real,post,spectram,wvl, realm,tau,ref,wp,ic_prob,li_prob,sp_arr,par_arr,wvl,post_pdf,filename=fn else $
save, tmhrs, tau_rtm,ref_rtm,wp_rtm,H_rtm,Hi_rtm,Hm_rtm,tau_err,ref_err,wp_err,real,post,ic_prob,li_prob,sp_arr,par_arr,wvl,post_pdf,filename=fn
if model or test gt 0 then break
endfor
stop
end
