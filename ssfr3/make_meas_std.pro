; program used in retrieve_pdf to build the measurement pdf of the parameters
; uncertainty is based on variance due to dark noise
; and variability due to spectral changes in response functions

; program to determine the uncertainty associated with each parameter
; uses the sample darks spectra measured with the SSFR3
; calculates the parameters for each dark spectra
; then returns the max,min, and stddev of each parameter

;@get_params.pro
pro make_meas_std,sp,wvl,std_par,win=win ,xplot=xplot,dark_s=dark_s,resps=resps,par=par,norm=norm,co=co,ad=ad,weight=weight

if n_elements(norm) lt 1 then norm=0
;win for winter
if n_elements(win) lt 1 then win=0


if n_elements(dark_s) lt 1 then restore, '/home/leblanc/SSFR3/data/dark_sample.out' else begin
  z_dk=dark_s.z_dk
  zenlambda=dark_s.zenlambda
endelse
if n_elements(resps) lt 1 then restore, '/home/leblanc/SSFR3/data/resps.out' else begin
  zenlambda=resps.zenlambda
  zresp1=resps.zresp1
  zresp2=resps.zresp2
  zresp3=resps.zresp3
  zresp4=resps.zresp4
endelse

if win then zresp_ref=zresp4 else zresp_ref=zresp2
if win then ref=2 else ref=0

if n_elements(xplot) lt 1 then xplot=0 else xplot=1

zresp=zresp1
n=3 ; number of response functions to evaluate
if n_elements(wvl) ne n_elements(zenlambda) then spz=interpol(sp,wvl,zenlambda) else spz=sp ;spz[*,59]

get_params, zenlambda, spz,par
;if norm then par=normalize_par(par,co,ad)

;ps is the array of parameter values used
if n_elements(ps) lt 1 then ps=indgen(n_elements(par))

ns=n_elements(z_dk[*,0])*n ; number of dark spectra (could be set to 100)
pars=fltarr(ns,n_elements(par))

if xplot then sps=fltarr(ns,n_elements(zenlambda))
; need to make noise into noise equivalent radiance

;make mean dark
mdk=fltarr(n_elements(zenlambda))
for v=0, n_elements(zenlambda)-1 do mdk[v]=mean(abs(z_dk[*,v])) 

; now go through each response functions
for j=0, n-1 do begin
  case j of
    0:zresp=zresp2
    1:zresp=zresp3
    2:zresp=zresp4
    ;3:zresp=zresp4
  endcase

  ; now go through each set of dark spectra
  for i=0, (ns/n)-1 do begin
    dk=[reform(z_dk[i,0:193]),abs(reform(z_dk[i,194:*]))] ; weird inverse dark issue
    dk=dk-mdk ; substract mean dark
    spc=dk/zresp+spz*zresp_ref/zresp  
    ; build spectrum based on a change of dark, and the ratio of the two response functions used
   if xplot then  sps[i+(j*n_elements(z_dk[*,0])),*]=spc
    get_params, zenlambda,reform(spc), partm
    if norm then pars[i+(j*n_elements(z_dk[*,0])),*]=normalize_par(partm,co,ad) else pars[i+(j*n_elements(z_dk[*,0])),*]=partm
  endfor
  if j eq ref then $
    prs=pars[j*(ns/n):(j+1)*(ns/n)-1,*] ; set for the reference
endfor

; now get the standard deviation of the parameters
std_par=stddev(pars,dimension=1)
;pa=fltarr(n_elements(par),n_elements(pars[*,0]))
;for p=0,n_elements(par)-1 do pa[p,*]=pars[*,p]
cov=correlate(transpose(pars),/covariance)
;stop
;weight=(cov^2.)^(-0.25)
end

;funtcion to return the normalized parameter values
; uses output of normalize
function normalize_par,par,co,ad
parn=par*co-ad
return, parn
end

