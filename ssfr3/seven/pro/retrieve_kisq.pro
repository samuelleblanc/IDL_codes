; program to retrieve optical depth, effective radius, and phase using ki squared technique


@get_params.pro

pro retrieve_kisq,model=model,date=date

if n_elements(model) lt 1 then model=0 else model=1
if n_elements(date) lt 1 then date='20120523'

vv='_v5'

dir='/home/leblanc/SSFR3/'

ps=[2,3,5,6,7,11,12,14]-1

if model then begin
  print, 'restoring modeled out file'
  restore, '/argus/roof/SSFR3/model/sp_hires5_20120524.out'
  wvl=zenlambda
  fl=findgen(n_elements(sp[*,*,0,*]))
  spectram=fltarr(n_elements(fl),n_elements(wvl))
  print, 'building the set of spectra'
  j=0
  wp=[0,1]
  ind=intarr(n_elements(tau),n_elements(ref),n_elements(wp))
  for w=0, n_elements(wp)-1 do begin
    for t=0, n_elements(tau)-1 do begin
      for r=0, n_elements(ref)-1 do begin
        spectram[j,*]=reform(sp[t,r,*,w],1,n_elements(wvl))
        print, w,t,r,max(spectram[j-1,*]),max(sp[t,r,*,w])
        ind[t,r,w]=j
        j=j+1
      endfor ; ref loop
    endfor ;tau loop
  endfor ; wp loop
  refo=ref
  num=n_elements(fl)
endif else begin
  fn='/argus/roof/SSFR3/data/'+date+'/out/'+date+'_calibspcs.out'
  print, 'restoring '+fn
  restore, fn
  wvl=zenlambda
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
  num=n_elements(tmhrs[fl]) ; the total number of points to go through
endelse

if date eq '20130110' or date eq '20130111' then sno='_snow' else sno=''
fn='/home/leblanc/SSFR3/data/Pars_std_meas_lut'+sno+'_v4.out' ;'/argus/roof/SSFR3/model/pars_std_v3'+sno+'.out'
print, 'restoring lut file: '+fn
restore, fn
taus=tau_hires[0:99] & refs=ref_hires & avg=avg[0:99,*,*,*] & std=std[0:99,*,*,*] & wp=[0,1]

tau_rtm=fltarr(num)
tau_err=fltarr(num,2)
ref_rtm=fltarr(num)
ref_err=fltarr(num,2)
wp_rtm=fltarr(num)
wp_err=fltarr(num)
ki_rtm=fltarr(num)
if model then kis_rtm=fltarr(num,n_elements(taus),n_elements(refs),n_elements(wp),n_elements(avg[0,0,0,*]))

; build the slopes of the lut
slopes=laplace(avg,taus,refs)

if model then di=1 else di=30
for i=0, num-1,di do begin ;time loop

  if model then sp=reform(spectram[i,*]) else sp=zspectra[*,fl[i]]
  ;stop
  get_params,wvl,sp,par

  ; run first to get phase
  ki=kisq(par,avg,kis=kis,std=std)
  nul=min(ki,m)
  im=array_indices(ki,m)
  wp_rtm[i]=wp[im[2]]

  if im[2] eq 0 then begin ; for liquid
    ; subset the array for liquid 
    ; only use up to 30 microns
    arr=reform(avg[*,0:29,0,*])
    rr=refs[0:29]
    
    ss=reform(std[*,0:29,0,*])
    sls=reform(slopes[*,0:29,0,*])
    ; get the coefficient and addition values for each parameter range
    ;coef=1./(max(max([arr+ss,arr-ss],dimension=2,/nan),dimension=1)-min(min([arr+ss,arr-ss],dimension=2,/nan),dimension=1))
    ;add=min(min([arr+ss,arr-ss],dimension=2,/nan),dimension=1)*coef
    
    ;ki=kisq(par,arr,kis=kis,coef=coef,add=add);,psub=ps)
    ki=kisq_std(par,arr,ss,sls,kis=kis)
    ki_rtm[i]=min(ki,n)
    in=array_indices(ki,n)
    tau_rtm[i]=taus[in[0]]
    ref_rtm[i]=rr[in[1]]
    if model then kis_rtm[i,*,0:29,0,*]=kis
  endif else begin  ;for ice
    ; subset the array for ice
    ; only use from 5 microns
    arr=reform(avg[*,4:*,1,*])
    rri=refs[4:*]

    ss=reform(std[*,4:*,1,*])
    sls=reform(slopes[*,4:*,1,*])
    ; get the coefficient and addition values for each parameter range
    ;coef=1./(max(max([arr+ss,arr-ss],dimension=2,/nan),dimension=1)-min(min([arr+ss,arr-ss],dimension=2,/nan),dimension=1))
    ;add=min(min([arr+ss,arr-ss],dimension=2,/nan),dimension=1)*coef

    ;ki=kisq(par,arr,kis=kis,coef=coef,add=add);,psub=ps)
    ki=kisq_std(par,arr,ss,sls,kis=kis)
    ki_rtm[i]=min(ki,n)
    in=array_indices(ki,n)
    tau_rtm[i]=taus[in[0]]
    ref_rtm[i]=rri[in[1]]
    if model then kis_rtm[i,*,4:*,1,*]=kis
  endelse
if i eq 630 then stop
;if ref_rtm[i] lt 10. then stop
;if tau_rtm[i] lt 40. and tau_rtm[i-di] gt 70. then stop
;  kierp=kisq(par,avg+std)
;  kierm=kisq(par,avg-std)

  ; for error
;  nul=min(kierp,nrp)
;  inp=array_indices(kierp,nrp) 
;  nul=min(kierm,nrm)
;  inm=array_indices(kierm,nrm)
  tau_err[i,*]=0. ;[taus[min([inp[0],inm[0]])],taus[max([inp[0],inm[0]])]]
  ref_err[i,*]=0. ;[refs[min([inp[1],inm[1]])],refs[max([inp[1],inm[1]])]]
  wp_err[i]=0. ;total(ki[*,*,1],/nan)/total(ki,/nan)

  print, i,'/',num,ki_rtm[i],tau_rtm[i],ref_rtm[i],wp_rtm[i]

endfor ;end of time loop

if not model then begin
  is=indgen(num)
  kl=where(is mod 30 eq 0)
  ki_rtm=ki_rtm[kl] & tau_rtm=tau_rtm[kl] & ref_rtm=ref_rtm[kl] & wp_rtm=wp_rtm[kl] & tmhrs=tmhrs[fl[kl]]
  tau_err=tau_err[kl,*] & ref_err=ref_err[kl,*] & wp_err=wp_err[kl]
endif

if model then md='model' else md=date
fn=dir+'data/retrieved_kisq_'+md+vv+'.out'
print, 'saving data to: '+fn
if model then save, tau_rtm, ref_rtm, wp_rtm, ki_rtm, tau,ref,wp,ind, kis_rtm, ref_err,tau_err,wp_err,filename=fn else $
 save, tau_rtm, ref_rtm, wp_rtm, ki_rtm, tmhrs, ref_err,tau_err,wp_err,filename=fn

stop
end

; function that does the ki squared calculation, returns array, size of lut, of ki square values
function kisq, par, pars_lut,kis=kis,psub=psub,coef=coef,add=add,std=std
  ss=n_elements(size(pars_lut,/dimensions))
  if ss eq 4 then npar=n_elements(pars_lut[0,0,0,*]) else npar=n_elements(pars_lut[0,0,*])
if n_elements(coef) lt 1 and n_elements(add) lt 1 then begin
    coef=fltarr(npar) & add=fltarr(npar) & ca=1
endif else ca=0

  if ss eq 4 then ki=pars_lut[*,*,*,0]*0. else ki=pars_lut[*,*,0]*0.
  kis=pars_lut*0.
  s=fltarr(npar)
  for p=0, npar-2 do begin ; loop through the parameters
    if n_elements(psub) gt 0 then if where(p eq psub) lt 0 then continue
    if ss eq 4 then begin
      if ca then begin
        coef[p]=1./(max(pars_lut[*,*,*,p],/nan)-min(pars_lut[*,*,*,p],/nan))
        add[p]=min(pars_lut[*,*,*,p]*coef[p])
      endif

;    s[p]=stddev(pars_lut[*,*,*,p])
;    s[p]=max(abs(pars_lut[*,*,*,p]),/nan)
;    kis[*,*,*,p]=(par[p]-pars_lut[*,*,*,p])^2./s[p]
      ps=pars_lut[*,*,*,p]*coef[p]-add[p]
      pp=par[p]*coef[p]-add[p]
      kis[*,*,*,p]=(pp-ps)^2./std[*,*,*,p]^2.
      ki=ki+kis[*,*,*,p]
    endif else begin
      if ca then begin
        coef[p]=1./(max(pars_lut[*,*,p],/nan)-min(pars_lut[*,*,p],/nan))
        add[p]=min(pars_lut[*,*,p]*coef[p])
      endif
  ;    if p eq 1 and n_elements(pars_lut) le (100.*30.*16.) then begin
  ;      coef[p]=0.170372 & add[p]=-1.03533
  ;      ; for parameter 2 - liquid, change the scaling so that it presents the sensitivity better (almost no sensitivity)
  ;    endif

      ps=pars_lut[*,*,p]*coef[p]-add[p]
      pp=par[p]*coef[p]-add[p]
      kis[*,*,p]=(pp-ps)^2.
      ki=ki+kis[*,*,p]
    endelse
  
  endfor ; end of parameter loop
return, ki
end


; function that does the ki squared calculation, returns array, size of lut, of ki square values
function kisq_std, par, pars_lut,std,slopes,kis=kis,psub=psub
  npar=n_elements(pars_lut[0,0,*])

  ki=pars_lut[*,*,0]*0.
  kis=pars_lut*0.
  s=fltarr(npar)
  for p=0, npar-2 do begin ; loop through the parameters
    if n_elements(psub) gt 0 then if where(p eq psub) lt 0 then continue
    ps=pars_lut[*,*,p]
    pp=par[p]
    ; the weights
    w=slopes[*,*,p]*slopes[*,*,p]/(std[*,*,p]*std[*,*,p])
    kis[*,*,p]=w*(pp-ps)^2.
    ki=ki+kis[*,*,p]
;stop
  endfor ; end of parameter loop
return, ki
end


; function to build the slopes at each point of the lut
; makes a laplace value for each point of the lut
function laplace, arr,x1,x2
slo=arr
slot=arr
slor=arr
 for w=0, 1 do begin
   for p=0, n_elements(arr[0,0,0,*])-2 do begin
     for t=0, n_elements(x1)-1 do $
       slor[t,*,w,p]=deriv(arr[t,*,w,p],x2);,x2)
       if not finite(total(slor[*,*,w,p])) then stop
     for r=0, n_elements(x2)-1 do $
       slot[*,r,w,p]=deriv(arr[*,r,w,p],x1);,x1)
       if not finite(total(slot[*,*,w,p])) then stop
   endfor
 endfor
slo=(slor+slot)/1000.
; normalized the slopes?
;stop

return, slo
end

