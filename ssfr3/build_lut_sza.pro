; program that builds the look up table (lut) for spectral retrieval of transmitted radiance through cloud
; input is the various spectra, output is effective radius, optical depth, and number 0 to 1 
; for portion of liquid to ice water content

;@get_params_2wvl.pro
;@get_params_test.pro
;@get_params.pro
;@get_params2.pro
;@get_params3.pro
@get_params4.pro
pro build_lut_sza ;,date

vv='v1'
win=1
dates=['20120525','20120806','20130110']
;dates=['20120525']
for d=0, n_elements(dates)-1 do begin
date=dates[d]


case date of 
 '20120523':begin
   lbl=['20120523_snd','20120523_snd2']
   szas=[50.,40.] & mulim=[0.5,0.8]
;   lbl=['20120523_snd','20120824_snd','20120813_snd2','20120806_snd','20120816_snd'] & szas=[50.,40.,37.,55.,64.]
   lbl=lbl[0] & szas=szas[0]
  end
  '20120525':begin
   lbl=['20120525_snd','20120525_snd2'] 
   szas=[50.,45.] & mulim=[0.6,0.75]
;   lbl=['20120525_snd','20120824_snd2','20120816_snd3'] & szas=[50.,45.,54.]
;  lbl=lbl[0] & szas=szas[0]
   lbl=['v1_20120525','v1_20120525_2','v1_20120525_3','v1_20120525_4'] & szas=[50.,45.,55.,40.]
  end
  '20120602':begin
    lbl=['20120602_snd','20120602_snd2','20120602_snd3'] 
   szas=[50.,42.,32.] & mulim=[0.6,0.85]
;   lbl=['20120602_snd','20120602_snd2','20120813_snd2','20120816_snd3'] & szas=[50.,42.,37.,54.]
    lbl=lbl[[0,1]] & szas=szas[[0,1]] & mulim=[0.65,0.75]
  end
  '20120806':begin
   lbl='20120806_snd'+['','3'] 
   szas=[55.,46.57] & mulim=[0.6,0.7]
   lbl=['v1_20120806','v1_20120806_2','v1_20120806_3'] & szas=[50.,55.,45.]
  end
  '20120813': begin
   lbl='20120813_snd'+['','2','3','4'] 
   szas=[50.,37.,40.,28.] & mulim=[0.75,0.9]
;   lbl=['20120813_snd','20120813_snd2','20120824_snd'] & szas=[50.,37.,40.]
   lbl=lbl[[0,1]] & szas=szas[[0,1]] & mulim=[0.65,0.8]
  end
  '20120816':begin
   lbl='20120816_snd'+['','2','3','4','5'] 
   szas=[64.,70.,54.,58.,49.] & mulim=[0.5,0.65]
;   lbl=['20120816_snd','20120816_snd2','20120816_snd3','20120806_snd','20120813_snd'] & szas=[64.,70.,54.,55.,50.]
   lbl=lbl[[0,1,2]] & szas=szas[[0,1,2]] & mulim=[0.45,0.6]
  end
  '20120824':begin
   lbl='20120824_snd'+['','2','3','4','5'] 
   szas=[40.,45.,54.,38.,50.] & mulim=[0.65,0.85]
;   lbl=['20120824_snd','20120824_snd2','20120824_snd5','20120813_snd2'] & szas=[40.,45.,50.,37.]
   lbl=lbl[[0,1,4]] & szas=szas[[0,1,4]] & mulim=[0.65,0.75]
  end
  '20120912':begin
   lbl='20120912_snd'+['','2'] 
   szas=[45.,39.] & mulim=[0.6,0.8]
;   lbl=['20120912_snd','20120813_snd2','20120816_snd3'] & szas=[45.,37.,54.]
   lbl=lbl[0] & szas=szas[0]
  end
  '20130110':begin
   lbl='20130110_snd'+['','2'] 
   szas=[63.,66.] & mulim=[0.4,0.45]
   ;lbl=['20130110_snd','20130111_snd'] & szas=[63.,73.]
   lbl=['v1_20130110','v1_20130110_2'] & szas=[66.,63.]
  end
  '20130111':begin
    lbl='20130111_snd'+['','2'] 
   szas=[73.,66.] & mulim=[0.15,0.4]
   ;lbl=['20130111_snd','20130110_snd'] & szas=[73.,63.]
   lbl=lbl[0] & szas=szas[0]
  end
  else: message, 'wrong date'
endcase

mus=cos(szas*!dtor)
ii=sort(mus)
mus=mus[ii]
lbl=lbl[ii]
n=n_elements(ii)

for i=0, n-1 do begin
  print, 'restoring:'+lbl[i]
  ;if win then restore, 'C:\Users\Samuel\Research\SSFR3\model\hires6\sp_hires6_'+lbl[i]+'.out' else $
  if win then restore, 'C:\Users\Samuel\Research\SSFR3\model\v1\sp_'+lbl[i]+'.out' else $
    restore, '/argus/roof/SSFR3/model/sp_'+lbl[i]+'.out'

;  if n_elements(ref) gt 28 then begin
;    ref=[2.,3.,4.,5.,6.,7.,8.,9.,10.,11.,12.,13.,14.,15.,16.,17.,18.,$
;          20.,22.,23.,25.,28.,30.,32.,35.,40.,45.,50.]
;    ir=[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,26,28,30]
;    sp=sp[*,ir,*,*] 
;  endif

  if i eq 0 then $
    spa=fltarr(n_elements(tau),n_elements(ref),n_elements(zenlambda),2,n)
  spa[*,*,*,*,i]=sp
endfor
print, 'got mu grid of:', mus

if n gt 1 then begin
  print, 'now interpolating to a finer mu grid'
  dmu=0.05
  nu=fix((mulim[1]-mulim[0])/dmu+1.1)
  muss=findgen(nu)*dmu+mulim[0]
  nt=n_elements(tau) & nr=n_elements(ref) & nz=n_elements(zenlambda) & nw=2
  spu=fltarr(nt,nr,nz,2,nu)
  for t=0,nt-1 do for r=0,nr-1 do for z=0,nz-1 do for w=0,nw-1 do spu[t,r,z,w,*]=interpol(spa[t,r,z,w,*],mus,muss,/nan)
endif else begin
  spu=spa
  muss=mus
  nu=n
endelse
;now interpol to finer sp and pars grid

if 1 then begin
  tau_hires=findgen(100)+1.
  ref_hires=findgen(60)+1.
  sp_hit=fltarr(n_elements(tau_hires),n_elements(ref),2,n_elements(zenlambda))
  sp_hi=fltarr(n_elements(tau_hires),n_elements(ref_hires),2,n_elements(zenlambda))
  sp_hiu=fltarr(n_elements(tau_hires),n_elements(ref_hires),2,n_elements(zenlambda),nu)
  pars=fltarr(n_elements(tau_hires),n_elements(ref_hires),2,16)
  par_hires=fltarr(n_elements(tau_hires),n_elements(ref_hires),2,16,nu)
print, 'interpolating the spectra, then calculating paramters'
for u=0, nu-1 do begin
print, 'on mu of:', muss[u]
  for z=0,n_elements(zenlambda)-1 do $
    for w=0,1 do $
      for r=0, n_elements(ref)-1 do sp_hit[*,r,w,z]=interpol(spu[*,r,z,w,u],tau,tau_hires,/nan)
      ;for t=0, n_elements(tau)-1 do sp_hi[t,*,w,z]=interpol(sp_hit[t,*,w,z],ref,ref_hires,/nan)
   for z=0,n_elements(zenlambda)-1 do $ 
    for w=0,1 do $
      for t=0, n_elements(tau_hires)-1 do sp_hi[t,*,w,z]=interpol(sp_hit[t,*,w,z],ref,ref_hires,/nan)
  for w=0,1 do begin
      for r=0, n_elements(ref_hires)-1 do begin
        for t=0, n_elements(tau_hires)-1 do begin
          get_params,zenlambda,sp_hi[t,r,w,*],partm
          pars[t,r,w,*]=partm
        endfor
      endfor
  endfor
par_hires[*,*,*,*,u]=pars
sp_hiu[*,*,*,*,u]=sp_hi
endfor
wp=[0,1] 
mu=muss
 
pars=par_hires
taus=tau_hires
refs=ref_hires


if win then fn='C:\Users\Samuel\Research\SSFR3\data\par_sza_'+vv+'_'+date+'.out' else $
 fn='/home/leblanc/SSFR3/data/Pars2_LUT_'+label+'.out'
print, 'saving to:'+fn
save, pars,taus,refs,wp,mu,filename=fn
save, pars,taus,refs,wp,mu,sp_hiu,zenlambda,filename='C:\Users\Samuel\Research\SSFR3\data\sp_par_sza_'+vv+'_'+date+'.out'
endif 

endfor
stop
end
