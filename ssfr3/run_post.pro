; program to take the likelihood functions to build the posterior pdf
; is use so that we can easily change the order and which parameters to use

@run_pdfs_v10.pro
pro run_post
dir='C:\Users\Samuel\Research\SSFR3\'

ll='dref_rg'
;ll='dref'
;ll='ct'
;ll='day'
;ll='rg'
;la='all'
;la='sub'
la='pss'


fp=dir+'retrieved\cst\retr_'+ll+'_v3.out'
fp=dir+'data\full\retr_'+ll+'_v4.out'
print, 'restoring: '+fp
restore, fp

; post_meas_pdf[22,35,2,100,60,16]
; refm[35]
; taum[22]

;ps=[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]-1
ps=[1,2,3,4,5,6,7,9,11,12,13,14,15]-1
ps=[1,2,3,5,6,7,9,11,13,15]-1
;ps=[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]-1
nt=n_elements(taum)
nr=n_elements(refm)
nw=2
npar=n_elements(ps)

prio_pdf=fltarr(100,60,npar+1)+1.
post_ind=fltarr(100,60,npar)
taus=findgen(100)+1.
ref=findgen(60)+1.


hrt=fltarr(nt,nr,nw)
taurt=fltarr(nt,nr,nw)
refrt=fltarr(nt,nr,nw)
tauer=fltarr(nt,nr,nw)
refer=fltarr(nt,nr,nw)
postrt=fltarr(nt,nr,nw,100,60,npar)
hprt=fltarr(nt,nr,nw,npar)
hirt=fltarr(nt,nr,nw,npar)

for w=0, nw-1 do begin
  for t=0, nt-1 do begin
    for r=0, nr-1 do begin
      if w eq 0 and r gt 22 then continue  
      if w eq 1 and r lt 8 then continue
      if w eq 0 then begin
        post_meas=reform(post_meas_pdf[t,r,w,*,0:29,ps])
        prio=reform(prio_pdf[*,0:29,[ps,15]])
        p_ind=reform(post_ind[*,0:29,ps])
        rr=ref[0:29]
      endif
      if w eq 1 then begin
        post_meas=reform(post_meas_pdf[t,r,w,*,9:*,ps])     
        prio=reform(prio_pdf[*,9:*,[ps,15]])
        p_ind=reform(post_ind[*,9:*,ps])
        rr=ref[9:*]
      endif
      ; now homogenize all joint of each parameter to each other
      homogenize, post_meas, prio,p_ind,npar
      ; now set the posterior pdfs, normalize it, and get the post_tau, and post_ref
      post_norm, prio,post,post_pdf,post_tau,post_ref, npar,taus,rr,failed=failed
 ;     if failed then goto, fail
      ;calculate the shanon information content for pdfs
      sic_pdf, prio, post_pdf, p_ind, post, npar, ic_prob,li_prob, H_par,H_ind,H
      ; calculate the marginal pdfs, and the shannon information content from each marginal pdf
      marg_pdf, post, prio, taus, rr, post_t, post_r, h_tau, h_ref
      ; now determine the effective uncertainty for tau and ref from the marginal pdfs
      uncertainty, post_t,post_r,taus,rr, tau_err, ref_err

      ;now put the vaules to arrays
      hrt[t,r,w]=h & taurt[t,r,w]=post_tau & refrt[t,r,w]=post_ref & tauer[t,r,w]=tau_err & refer[t,r,w]=ref_err
      hprt[t,r,w,*]=h_par & hirt[t,r,w,*]=h_ind
      if w eq 0 then postrt[t,r,w,*,0:29,*]=post_pdf else postrt[t,r,w,*,9:*,*]=post_pdf
      print,t,r,w
    endfor ; ref loop
  endfor ;tau loop
endfor ; w loop

fn=dir+'retrieved\cst\retr_'+ll+'_'+la+'_v4.out'
save, hrt,taurt,refrt,tauer,refer,hprt,hirt,postrt,taum,refm,ps,filename=fn

fn=dir+'retrieved\cst\H_'+ll+'_'+la+'_v4.out'
save, hrt,taurt,refrt,tauer,refer,hprt,hirt,taum,refm,ps,filename=fn
stop
end
