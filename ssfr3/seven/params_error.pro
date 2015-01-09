; program to determine the uncertainty associated with each parameter
; uses the sample darks spectra measured with the SSFR3
; calculates the parameters for each dark spectra
; then returns the max,min, and stddev of each parameter

@get_params.pro
pro params_error

restore, '/home/leblanc/SSFR3/data/dark_sample.out'
restore, '/home/leblanc/SSFR3/data/resp.out'
restore, '/home/leblanc/SSFR3/data/sp_at_wv.out'

spz_clear=spz[*,59]


get_params, zenlambda, z_dk[0,*],par

ns=n_elements(z_dk[*,0])
pars=fltarr(ns,n_elements(par))

; need to make noise into noise equivalent radiance
; add a very small, sample spectra on top, for proper normalization... maybe clear sky spectra

window, 0, retain=2
plot, zenlambda, spz_clear
oplot,zenlambda, spz_clear+z_dk[0,*]/zresp

for i=0, ns-1 do begin
  dk=[reform(z_dk[i,0:193]),reform(z_dk[i,194:*]+1300.)]
  sp=dk/zresp+spz_clear
  get_params, zenlambda,sp, partm
  pars[i,*]=partm
endfor

pars_pdf_meas=pars
for p=0, n_elements(par)-1 do begin

  print, 'for parameter:',p+1
  print, 'minimum: ',min(pars[*,p]), 'maximum: ',max(pars[*,p]),' diff:',max(pars[*,p])-min(pars[*,p])
  print, 'stddev: ',stddev(pars[*,p])
  pars_pdf_meas[*,p]=pars[*,p]-mean(pars[*,p])
endfor

i=0
plot, findgen(100)*(max(pars_pdf_meas[*,i])-min(pars_pdf_meas[*,i]))/100.+min(pars_pdf_meas[*,i]),histogram(pars_pdf_meas[*,i],nbins=100)


save, pars_pdf_meas,pars, filename='/home/leblanc/SSFR3/data/pars_error.out'
stop

end
