; program to run the retrieval of optical depth, effective radius and phase
; using the pdf methodology
; using the new v1 model radiances and parameters
; determine shannon information content

@make_meas_pdf.pro
@get_params4.pro
@run_pdfs_v8_test.pro
@support_pdfs.pro

pro retrieve_pdfs,ll,md,ice=ice,liq=liq,tsub=tsub,lim=lim,pha=pha
if n_elements(ll) lt 1 then ll='ab'
if n_elements(md) lt 1 then md=''
if n_elements(pha) lt 1 then pha=0 else pha=1
dir='C:\Users\Samuel\Research\SSFR3\'
;dir='/projects/leblanse/cloud/'
l='\'
;l='/'

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
namelut=dir+'model\OC\MODRES_SGP_LUT.dat' ; 'data'+l+'par_std_'+ll+'_v1.out'
get_lut_test,namelut,taus,refi,refl,pari,parl,bini,binl,np,refs,wps,pars;,/int
; taui, taul, refi, refl arrays of possible tau and ref values for ice or liquid
; pari, parl lut binned, for ice or liquid
; bini, binl bins of ice or liquid
; taus, refs, wps, set of all values
; pars, non binned value of parameters, np - number of parameters


; first test stop
if 1 then begin
  set_plot, 'win' & loadct, 39 & device,decomposed=0 & !y.style=1 & !x.style=1 & !p.background=255 & !p.color=0
   !p.thick=3 & !p.charsize=1.5 & !p.multi=0
  rs=[7,11] ;for ref of 8 and 12 um
  ts=[8,12] ; for tau of 5 and 15
  plot, binl[1,*],parl[ts[0],rs[0],1,*],title='Model pdf of slope at 1600 nm at mu=0.866',xr=[-0.1,0.1]
  oplot,binl[1,*],parl[10,9,1,*],color=0
  oplot,binl[1,*],parl[ts[0],rs[0],1,*],color=250,psym=-1
  oplot,binl[1,*],parl[ts[1],rs[0],1,*],color=100,psym=-1
  oplot,binl[1,*],parl[ts[0],rs[1],1,*],color=50,psym=-1
  oplot,binl[1,*],parl[ts[1],rs[1],1,*],color=150,psym=-1
  legend,['tau=10, ref=10 um','tau=8, ref=8 um','tau=8, ref=12 um','tau=12, ref=8 um','tau=12, ref=12 um'],$
   textcolors=[0,250,50,100,150],box=0,/right 
  p=tvrd(/true)
  write_png, dir+'plots\test\model_slope_const.png',p
;stop
 ;  window, 1
 ; plot, binl[1,*],parl[ts[0],rs[0],1,*],title='Model pdf of slope at 1600 nm at mu=0.866',yr=[0,0.4]
 ; oplot,binl[1,*],parl[ts[1],rs[0],1,*],color=50
 ; oplot,binl[1,*],parl[ts[0],rs[1],1,*],color=150
 ; oplot,binl[1,*],parl[ts[1],rs[1],1,*],color=250
 ; legend,['tau=5, ref=8 um','tau=5, ref=12 um','tau=15, ref=8 um','tau=15, ref=12 um'],textcolors=[255,150,50,250],box=0,/right
 ; p=tvrd(/true)
 ; write_png, dir+'plots\test\model_slope.png',p
  ;stop
endif




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
      sp=reform(spm[t,r,*,w])
      
      ; get pars
;      get_params,wvl,sp,par
;      par_arr[t,r,w,*]=par
      
      ; get phase
 ;     get_phase, pars,par,phase
     
 ;     wp_rtm[t,r,w]=phase
phase=0
ps=[0,1]
      if phase eq 0 then begin  ; liquid
        ;make the measurement pdf
        make_meas_pdf, sp,wvl,binl, meas_pdf, sno, ps=ps,dark_s=dark_s, resps=resps,/xplot

        meas_pdf=reform(pari[10,9,*,*]) 
stop
        meas_pdf[1,*]=reform(parl[10,9,1,*])     
 
        ; run the inverse generalise pdf 
        run_pdfs,parl,binl,taus,refl,wp,meas_pdf,post_tau,post_ref,post_wp,H,H_ind,H_par,terr,rerr,werr,posttm,$
         ic_prob=icp,li_prob=lip,ps=ps,post_pdf=post_pdftm, h_tau, h_ref,ptot=ptotl,p_likely=p_likely,$
         /xplot,post_ind=post_ind;,/int
      if 1 then begin
        j=1
        oplot,binl[j,*],p_likely[ts[0],rs[0],j,*]*10.,color=250,psym=-1
        oplot,binl[j,*],p_likely[ts[1],rs[0],j,*]*10.,color=100,psym=-1
        oplot,binl[j,*],p_likely[ts[0],rs[1],j,*]*10.,color=50,psym=-1
        oplot,binl[j,*],p_likely[ts[1],rs[1],j,*]*10.,color=150,psym=-1
        write_png, dir+'plots\test\model_likely_slope.png',tvrd(/true)
;        stop
        window, 1
        !p.color=0 & !p.background=255
        pp=posttm;post_pdftm[*,*,1]
        cl=findgen(10)/9.*255
        lvl=findgen(10)/9.*max(pp)
        contour,pp,taus,refl,nlevels=10,c_color=cl,xtitle='tau',ytitle='ref',title='Post PDF - Xmit 515 nm + Slope',$
          xr=[0,20],c_labels=[1,1,1,1,1,1,1,1,1]
     ;   xyouts, 15,15,'SIC='+string(h)
        legend,['SIC='+string(h),'SIC tau='+string(h_tau), 'SIC ref='+string(h_ref)],/right,/bottom
       write_png, dir+'plots\test\post_pdf_all.png',tvrd(/true)
        window,2
        ptau=total(pp,2)
        pref=total(pp,1)
        plot, taus,ptau,title='marginal pdf - tau',xtitle='tau'
        write_png,dir+'plots\test\pmarg_tau_all.png',tvrd(/true)
        window,3
        plot, refl,pref,title='marginal pdf - ref',xtitle='ref'
        write_png,dir+'plots\test\pmarg_ref_all.png',tvrd(/true)
        stop
        window,5
        ppi=post_ind[*,*,1]
        lvl=findgen(20)/19.*(max(ppi)-min(ppi))+min(ppi)
        cl=findgen(20)/19.*250.
        contour,ppi,taus,refl,nlevels=20,c_color=cl,xtitle='tau',ytitle='ref',title='Post PDF - Slope',$
          xr=[0,20],c_labels=cl*0+1
        ptaui=total(ppi,2)
        prefi=total(ppi,1)
        stau=sic(ptaui/total(ptaui))
        sref=sic(prefi/total(prefi))        
        legend,['SIC='+string(h_ind[1]),'SIC tau='+string(stau), 'SIC ref='+string(sref)],/right,/bottom      
        print, 'max and location of max'
        print, max(ppi,m),array_indices(ppi,m)
     write_png, dir+'plots\test\post_pdf_slope.png',tvrd(/true)
        window,6
        plot, taus,ptaui,title='Slope - marginal pdf - tau',xtitle='tau'
        write_png,dir+'plots\test\pmarg_tau_slope.png',tvrd(/true)
        window,7
        plot, refl,prefi,title='Slope - marginal pdf - ref',xtitle='ref'
        write_png,dir+'plots\test\pmarg_ref_slope.png',tvrd(/true)

        window,4
        rs=[7,11] ;for ref of 8 and 12 um
  ts=[8,12] ; for tau of 5 and 15
  plot, binl[j,*],p_likely[10,9,j,*],title='Likelihood function slope at 1600 nm at mu=0.866',$
   xr=[-0.01,0.01];yr=[0,0.00040],xr=[0.5,1.0]
  legend,['tau=10, ref=10 um','tau=8, ref=8 um','tau=8, ref=12 um','tau=12, ref=8 um','tau=12, ref=12 um'],$
   textcolors=[0,250,50,100,150],box=0,/right

        oplot,binl[j,*],p_likely[ts[0],rs[0],j,*],color=250,psym=-1
        oplot,binl[j,*],p_likely[ts[1],rs[0],j,*],color=100,psym=-1
        oplot,binl[j,*],p_likely[ts[0],rs[1],j,*],color=50,psym=-1
        oplot,binl[j,*],p_likely[ts[1],rs[1],j,*],color=150,psym=-1
        write_png, dir+'plots\test\likely_slope.png',tvrd(/true)

        stop

      endif

        ; put to variables
        post_pdf[t,r,w,*,0:29,*]=post_pdftm
        post[t,r,w,*,0:29]=posttm
      endif else begin ; ice
        ;make the measurement pdf
        make_meas_pdf, sp,wvl,bini, meas_pdf, sno, ps=ps,dark_s=dark_s, resps=resps
 
        ; run the invers generalize pdf
        run_pdfs,pari,bini,taus,refi,wp,meas_pdf,post_tau,post_ref,post_wp,H,H_ind,H_par,terr,rerr,werr,posttm,$
         ic_prob=icp,li_prob=lip,ps=ps,post_pdf=post_pdftm, h_tau, h_ref,ptot=ptoti

        ; put to variables
        post_pdf[t,r,w,*,9:59,*]=post_pdftm 
        post[t,r,w,*,9:59]=posttm 
      endelse
      ; put to variables
      tau_rtm[t,r,w]=post_tau & ref_rtm[t,r,w]=post_ref   
      tau_err[t,r,w,*]=terr & ref_err[t,r,w,*]=rerr  
      sic_par[t,r,w,*]=H_par & sic_par_ind[t,r,w,*]=H_ind & sic_rtm[t,r,w]=H 
      sic_tau_rtm[t,r,w]=h_tau & sic_ref_rtm[t,r,w]=h_ref 
     
      ; rerun the pdfs with the other phase to get the probability of either phase 
      if pha then begin
        if phase eq 0 then begin
          liq_prob[t,r,w,*,*]=lip
          make_meas_pdf, sp,wvl,bini, meas_pdf, sno, ps=ps,dark_s=dark_s, resps=resps
          ; run the invers generalize pdf
          run_pdfs,pari,bini,taus,refi,wp,meas_pdf,post_tau,post_ref,post_wp,H,H_ind,H_par,terr,rerr,werr,posttm,$
           ic_prob=icp,li_prob=lip,ps=ps,post_pdf=post_pdftm, h_tau, h_ref,ptot=ptoti
          ;get the homogenization coefficient
          po=total(parl,/nan)*30./total(pari,/nan)/51.
          ice_prob[t,r,w,*,*]=icp*po 
          if n_elements(liq) gt 0 and w eq 0 then post_pdf[t,r,1,*,9:59,*]=post_pdftm*po
        endif else begin
          ice_prob[t,r,w,*,*]=icp
          ;make the measurement pdf
          make_meas_pdf, sp,wvl,binl, meas_pdf, sno, ps=ps,dark_s=dark_s, resps=resps
          ; run the inverse generalise pdf 
          run_pdfs,parl,binl,taus,refl,wp,meas_pdf,post_tau,post_ref,post_wp,H,H_ind,H_par,terr,rerr,werr,posttm,$
           ic_prob=icp,li_prob=lip,ps=ps,post_pdf=post_pdftm, h_tau, h_ref,ptot=ptotl
          ;get the homogenization coefficient
          po=total(pari,/nan)*51./total(parl,/nan)/30.
          liq_prob[t,r,w,*,*]=lip*po
          if n_elements(ice) gt 0 and w eq 1 then post_pdf[t,r,0,*,0:29,*]=post_pdftm*po
        endelse
;        stop
      endif

      print, t,r,w, post_tau, post_ref, phase,h,h_tau,h_ref
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
       post, post_pdf,sic_tau_rtm, sic_ref_rtm, par_arr, wp_rtm, taum, refm, liq_prob, ice_prob,filename=fp

stop
end
