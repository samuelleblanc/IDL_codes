; program to determine the uncertainty associated with each parameter
; uses the sample darks spectra measured with the SSFR3
; calculates the parameters for each dark spectra
; then returns the max,min, and stddev of each parameter

@get_params.pro
pro params_error

restore, '/home/leblanc/SSFR3/data/dark_sample.out'
restore, '/home/leblanc/SSFR3/data/resps.out'
restore, '/home/leblanc/SSFR3/data/2012080604_sp_ex.out'; sp_at_wv.out'


zresp=zresp1
n=4
spz_clear=sp ;spz[*,59]


get_params, zenlambda, z_dk[0,*],par

ns=n_elements(z_dk[*,0])*n
pars=fltarr(ns,n_elements(par))
wvls=fltarr(ns)
; need to make noise into noise equivalent radiance
; add a very small, sample spectra on top, for proper normalization... maybe clear sky spectra

window, 0, retain=2
!p.multi=0
plot, zenlambda, spz_clear
oplot,zenlambda, spz_clear+z_dk[0,*]/zresp

for j=0, n-1 do begin

  case j of
    0:zresp=zresp1
    1:zresp=zresp2
    2:zresp=zresp3
    3:zresp=zresp4
  endcase

  for i=0, (ns/n)-1 do begin
    dk=[reform(z_dk[i,0:193]),reform(z_dk[i,194:*]+1300.)]
    sp=dk/zresp+spz_clear
    get_params, zenlambda,sp, partm
    nul=max(sp,nl)
    wvls[i+(j*n_elements(z_dk[*,0]))]=zenlambda[nl]
    pars[i+(j*n_elements(z_dk[*,0])),*]=partm
  endfor
  if j eq 1 then begin
    prs=pars[j*(ns/n):(j+1)*(ns/n)-1,*]
  endif
endfor

pars_pdf_meas=prs
for p=0, n_elements(par)-1 do begin
  print, 'for parameter:',p+1, ' minimum: ',min(pars[*,p]), 'maximum: ',max(pars[*,p]),' diff:',max(pars[*,p])-min(pars[*,p])
  print, 'stddev: ',stddev(pars[*,p])
  pars_pdf_meas[*,p]=prs[*,p]-mean(prs[*,p]) ;set pdf shape same as used for proper calibration
  ;now spread shape to account for differences in wavelength calibration
  df2=max(pars[*,p])-min(pars[*,p])
  df1=max(prs[*,p])-min(prs[*,p])
  pars_pdf_meas[*,p]=pars_pdf_meas[*,p]*df2/df1 ;ratio of differences in max-min from multiple cal vs. single shape (to reduce multimodal peaks)
  print, 'ratio at :',p,'     ...',df2/df1
endfor

window, 3, retain=2, xsize=1000,ysize=900
!p.multi=[0,4,4]
for i=0,15 do $
plot, findgen(100)*(max(pars_pdf_meas[*,i])-min(pars_pdf_meas[*,i]))/100.+min(pars_pdf_meas[*,i]),histogram(pars_pdf_meas[*,i],nbins=100,max=max(pars_pdf_meas[*,i]),min=min(pars_pdf_meas[*,i])),title=string(i)

stop
save, pars_pdf_meas,pars, filename='/home/leblanc/SSFR3/data/pars_error_v2.out'
stop

end
