; program used in retrieve_pdf to build the measurement pdf of the parameters
; uncertainty is based on variance due to dark noise
; and variability due to spectral changes in response functions

; program to determine the uncertainty associated with each parameter
; uses the sample darks spectra measured with the SSFR3
; calculates the parameters for each dark spectra
; then returns the max,min, and stddev of each parameter

@get_params_mix.pro
pro make_meas_pdf,sp,wvl,bins,pdf,win ,ps=ps,std=std,xplot=xplot,dark_s=dark_s,resps=resps


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
;restore, '/home/leblanc/SSFR3/data/2012080604_sp_ex.out'; sp_at_wv.out'

if win then zresp_ref=zresp4 else zresp_ref=zresp2
if win then ref=2 else ref=0

if n_elements(xplot) lt 1 then xplot=0 else xplot=1

zresp=zresp1
n=3
if n_elements(wvl) ne n_elements(zenlambda) then spz_clear=interpol(sp,wvl,zenlambda) else spz_clear=sp ;spz[*,59]
if n_elements(wvl) ne n_elements(zenlambda) then spz_clear[0:14]=fltarr(15)

get_params, zenlambda, spz_clear,par
;stop
;ps is the array of parameter values used
if n_elements(ps) lt 1 then ps=indgen(n_elements(par))

ns=n_elements(z_dk[*,0])*n
pars=fltarr(ns,n_elements(par))

if xplot then sps=fltarr(ns,n_elements(zenlambda))
;wvls=fltarr(ns)
; need to make noise into noise equivalent radiance

;make mean dark
mdk=fltarr(n_elements(zenlambda))
for v=0, n_elements(zenlambda)-1 do mdk[v]=mean(abs(z_dk[*,v])) 

for j=0, n-1 do begin

  case j of
    0:zresp=zresp2
    1:zresp=zresp3
    2:zresp=zresp4
    ;3:zresp=zresp4
  endcase

  for i=0, (ns/n)-1 do begin
    dk=[reform(z_dk[i,0:193]),abs(reform(z_dk[i,194:*]))]
    dk=dk-mdk ; substract mean dark
    spc=dk/zresp+spz_clear*zresp_ref/zresp
   if xplot then  sps[i+(j*n_elements(z_dk[*,0])),*]=spc
    get_params, zenlambda,reform(spc), partm
   ; if j gt 0 and i eq 2 then stop
   ; nul=max(sp,nl)
   ; wvls[i+(j*n_elements(z_dk[*,0]))]=zenlambda[nl]
    pars[i+(j*n_elements(z_dk[*,0])),*]=partm
  endfor
  if j eq ref then $
    prs=pars[j*(ns/n):(j+1)*(ns/n)-1,*]
endfor
std=par
pars_pdf_meas=prs ;set the shape of the pdf 
pdf=fltarr(n_elements(partm),n_elements(bins[0,*]))
for p=0, n_elements(par)-1 do begin
  if where(ps eq p) ge 0 then begin
  ;print, 'for parameter:',p+1, ' minimum: ',min(pars[*,p]), 'maximum: ',max(pars[*,p]),' diff:',max(pars[*,p])-min(pars[*,p])
  ;print, 'stddev: ',stddev(pars[*,p])
  ;pars_pdf_meas[*,p]=prs[*,p];-mean(prs[*,p]) ;set pdf shape same as used for proper calibration

  ; now scale shape to account for differences in wavelength calibration
  ;df2=max(pars[*,p])-min(pars[*,p])
  ;df1=max(prs[*,p])-min(prs[*,p])
  ;slope=df2/df1
  ;offset=((min(pars[*,p])*max(prs[*,p]))-(max(pars[*,p])*min(prs[*,p])))/df1
  ;if p ne 15 then pars_pdf_meas[*,p]=pars_pdf_meas[*,p]*slope+offset ; now scale the single spread of darks to all the different calibrations
  ;print, 'ratio at :',p,'     ...',df2/df1
  
  ; extend only one side of the distribution to encompass 
  mm=par[p]
  if max(pars[*,p]) gt max(prs[*,p]) then begin
    u=where(prs[*,p] gt mm)
    pars_pdf_meas[u,p]=(prs[u,p]-mm)*(max(pars[*,p])-mm)/(max(prs[*,p])-mm)+mm
  endif else begin
    u=where(prs[*,p] lt mm)
    pars_pdf_meas[u,p]=(prs[u,p]-mm)*(min(pars[*,p])-mm)/(min(prs[*,p])-mm)+mm
  endelse

  ; now make sure that the mean is equal to the mean from the first run
;  pars_pdf_meas[*,p]=pars_pdf_meas[*,p]-mean(pars_pdf_meas[*,p])+par[p]
  std[p]=stddev(pars_pdf_meas[*,p],/nan)
  pdf[p,*]=histogram(pars_pdf_meas[*,p],binsize=bins[p,1]-bins[p,0],nbins=n_elements(bins[p,*]),min=bins[p,0]-(bins[p,1]-bins[p,0])/2.)
  k=total(pdf[p,*],/nan)
  if k ne 0.0 then pdf[p,*]=pdf[p,*]/k else begin 
    pdf[p,*]=fltarr(n_elements(pdf[p,*]))+1E-45
    print, 'problem with measurement pdf of parameter:', p
  endelse
;  stop
  if not finite(total(pdf[p,*])) then message, 'problem with measurement pdf not being finite' 
  endif
endfor
;stop

if xplot then begin
  sp_std=fltarr(n_elements(zenlambda))
  for v=0, n_elements(zenlambda)-1 do sp_std[v]=stddev(sps[*,v])
  set_plot, 'x'
  device, decomposed=0
  loadct,39
  window, 0, retain=2
  plot, zenlambda, spz_clear/sp_std, title='signal to noise ratio',xtitle='Wavelength (nm)',ytitle='ratio'
  ;stop

endif
end

