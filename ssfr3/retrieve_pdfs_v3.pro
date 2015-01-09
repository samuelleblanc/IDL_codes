; program to run the retrieval of optical depth, effective radius and phase
; using the pdf methodology
; using the new v1 model radiances and parameters
; determine shannon information content

@make_meas_pdf.pro
@get_params4.pro
@run_pdfs_v9.pro
@support_pdfs.pro

pro retrieve_pdfs,ll,md,ice=ice,liq=liq,tsub=tsub,lim=lim,pha=pha
if n_elements(ll) lt 1 then ll='ab'
if n_elements(md) lt 1 then md=''
if n_elements(pha) lt 1 then pha=0 else pha=1
win=1
test=0
t1=11 & r1=30 & w1=1

if win then begin
  dir='C:\Users\Samuel\Research\SSFR3\'
  l='\'
endif else begin
  dir='/projects/leblanse/cloud/'
  l='/'
endelse

sno=0
if n_elements(lim) gt 0 then ps=[1,2,3,4,11,12,13,14]-1 

;restore files to use with make_meas_pdf
print, 'restoring files for make_meas_pdf'
restore, dir+'data'+l+'dark_sample.out'; '/home/leblanc/SSFR3/data/dark_sample.out'
dark_s={z_dk:z_dk,zenlambda:zenlambda}
restore, dir+'data'+l+'resps.out' ;'/home/leblanc/SSFR3/data/resps.out'
resps={zenlambda:zenlambda,zresp1:zresp1,zresp2:zresp2,zresp3:zresp3,zresp4:zresp4}

; get the spectra
name=dir+'model'+l+'v1'+l+'sp_v1_20120525'+md+'.out'
get_sp,name,taum,refm,wpm,spm,wvl

; get the lut
namelut=dir+'data'+l+'par_std_'+ll+'_v1.out'
namelut=dir+'data'+l+'sp_std_day_v1.out'
get_lut_v2,namelut,taus,refi,refl,pari,parl,bini,binl,np,refs,wps,pars, pexs,pha=pha, para=para, bina=bina,stds=0.01
; refi, refl arrays of possible ref values for ice or liquid
; pari, parl lut binned, for ice or liquid
; bini, binl bins of ice or liquid
; taus, refs, wps, set of all values
; pars, non binned value of parameters, np - number of parameters
; para is parameters in bin space with combined liquid and ice
; bina is bin space for combined liquid and ice parameters
; pha is keyword that if set returns the para and bina values 
; pexs array of extremes 0; min liq, 1: max liq, 2: min ice, 3: max ice

; set up the values to store the retrieved properties
nt=n_elements(taum)
nr=n_elements(refm)
nw=n_elements(wpm)
if n_elements(ps) gt 0 then np1=n_elements(ps) else np1=np

tau_rtm=fltarr(nt,nr,nw) & ref_rtm=fltarr(nt,nr,nw) & wp_rtm=fltarr(nt,nr,nw)
sic_rtm=fltarr(nt,nr,nw) & sic_ref_rtm=fltarr(nt,nr,nw) & sic_tau_rtm=fltarr(nt,nr,nw)
sic_par=fltarr(nt,nr,nw,np1) & sic_par_ind=fltarr(nt,nr,nw,np1)
tau_err=fltarr(nt,nr,nw,2) & ref_err=fltarr(nt,nr,nw,2) & wp_prob=fltarr(nt,nr,nw,np)
liq_prob=fltarr(nt,nr,nw,2,np1) & ice_prob=fltarr(nt,nr,nw,2,np1)

; make arrays to store intermediate values
post_pdf=fltarr(nt,nr,nw,n_elements(taus),n_elements(refs),np1)
post_meas_pdf=fltarr(nt,nr,nw,n_elements(taus),n_elements(refs),np1)
post=fltarr(nt,nr,nw,n_elements(taus),n_elements(refs))
par_arr=fltarr(nt,nr,nw,np)

print, 'start of loop'

; start of loops
for w=0, nw-1 do begin
  if n_elements(ice) gt 0 and w eq 0 then continue
  if n_elements(liq) gt 0 and w eq 1 then continue
  if w eq 1 then print, 'In ice' else print, 'In liq'
  for t=0, nt-1 do begin
    if n_elements(tsub) gt 0 then begin
       if tsub eq 0 then if t gt 5 then continue
       if tsub eq 1 then if t gt 10 or t lt 6 then continue
       if tsub eq 2 then if t gt 15 or t lt 11 then continue
       if tsub eq 3 then if t gt 20 or t lt 16 then continue
       if tsub eq 4 then if t lt 21 then continue
    endif
    for r=0,nr-1 do begin
      if w eq 0 and refm[r] gt 30. then continue
      if w eq 1 and refm[r] lt 10. then continue
       
      if test then begin
        t=t1 & r=r1 & w=w1
      endif
      sp=reform(spm[t,r,*,w])
      
      ; get pars
      get_params,wvl,sp,par
      par_arr[t,r,w,*]=par
      
      ; get phase
      get_phase, pars,par,phase
     
      wp_rtm[t,r,w]=phase
      if pha then begin  ;run the non phase segregated method
        ;make the measurement pdf with larger bin size
        make_meas_pdf, sp,wvl,bina,meas_pdf,sno,ps=ps,dark_s,resps=resps
        
        ; run the inverse generalise pdf, for large pdf space
        run_pdfs_all,para,bina,taus,refs,wp,meas_pdf,post_tau,post_ref,post_wp,H,H_ind,H_par,terr,rerr,werr,posttm,$
         ic_prob=icp,li_prob=lip,ps=ps,post_pdf=post_pdftm, h_tau, h_ref,ptot=ptotl,post_meas=post_meas

        ; put in variables
        post_meas_pdf[t,r,w,*,*,*]=post_meas
        post_pdf[t,r,w,*,*,*]=post_pdftm
        post[t,r,w,*,*]=posttm
      endif else begin ; run the phase segregated pdfs
        if phase eq 0 then begin  ; liquid
          ;make the measurement pdf
          make_meas_pdf, sp,wvl,binl, meas_pdf, sno, ps=ps,dark_s=dark_s, resps=resps
       
          ; run the inverse generalise pdf 
          run_pdfs,parl,binl,taus,refl,wp,meas_pdf,post_tau,post_ref,post_wp,H,H_ind,H_par,terr,rerr,werr,posttm,$
           ic_prob=icp,li_prob=lip,ps=ps,post_pdf=post_pdftm, h_tau, h_ref,ptot=ptotl,post_meas=post_meas, pexs[0,*],pexs[1,*]

          ; put to variables
          post_meas_pdf[t,r,w,*,0:29,*]=post_meas
          post_pdf[t,r,w,*,0:29,*]=post_pdftm
          post[t,r,w,*,0:29]=posttm
        endif else begin ; ice
          ;make the measurement pdf
          make_meas_pdf, sp,wvl,bini, meas_pdf, sno, ps=ps,dark_s=dark_s, resps=resps
 
          ; run the invers generalize pdf
          run_pdfs,pari,bini,taus,refi,wp,meas_pdf,post_tau,post_ref,post_wp,H,H_ind,H_par,terr,rerr,werr,posttm,$
           ic_prob=icp,li_prob=lip,ps=ps,post_pdf=post_pdftm, h_tau, h_ref,ptot=ptoti,post_meas=post_meas,pexs[2,*],pexs[3,*]

          ; put to variables
          post_meas_pdf[t,r,w,*,9:59,*]=post_meas
          post_pdf[t,r,w,*,9:59,*]=post_pdftm 
          post[t,r,w,*,9:59]=posttm 
        endelse
      endelse ;end of else for phase or not phase segregated
      ; put to variables
      tau_rtm[t,r,w]=post_tau & ref_rtm[t,r,w]=post_ref   
      tau_err[t,r,w,*]=terr & ref_err[t,r,w,*]=rerr  
      sic_par[t,r,w,*]=H_par & sic_par_ind[t,r,w,*]=H_ind & sic_rtm[t,r,w]=H 
      sic_tau_rtm[t,r,w]=h_tau & sic_ref_rtm[t,r,w]=h_ref 
     
      print, t,r,w, post_tau, post_ref, phase,h,h_tau,h_ref
      if test then stop
    endfor ; end ref loop
  endfor ; end tau loop
endfor ; end wp loop

if n_elements(ice) gt 0 then li='_ice' else li=''
if n_elements(liq) gt 0 then li='_liq'
    if n_elements(tsub) gt 0 then begin
      case tsub of
       0:li=li+'_0'
       1:li=li+'_1'
       2:li=li+'_2'
       3:li=li+'_3'
       4:li=li+'_4'
      endcase
    endif

if n_elements(ps) gt 0 then md=md+'ps'
if pha then md=md+'pha'
fp=dir+'data'+l+'retr_mod_'+ll+li+md+'_v1.out'
save, tau_rtm,ref_rtm, tau_err, ref_err, sic_par, sic_par_ind, sic_rtm,$
       post, post_meas_pdf,post_pdf,sic_tau_rtm, sic_ref_rtm, par_arr, wp_rtm, taum, refm, liq_prob, ice_prob,filename=fp

stop
end
