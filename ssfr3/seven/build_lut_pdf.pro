; program to build the pdf of the lut
; uses the output of the pars_lut
; which takes in multiple lut and finds the standard deviation and mean for each set of parameter
; is used to build a more accurate pdf lut

pro build_lut_pdf

test=1
snow=0
twowvl=0

if snow then lbsn='_snow' else lbsn=''

if test eq 1 then fr='/home/leblanc/SSFR3/data/pars_std'+lbsn+'_test.out' else $
 if test eq 2 then fr='/home/leblanc/SSFR3/data/pars_std'+lbsn+'_test2.out' else $
  fr='/home/leblanc/SSFR3/data/pars_std'+lbsn+'.out'
if twowvl then fr='/home/leblanc/SSFR3/data/pars_std'+lbsn+'_2wvl.out'

restore, fr
ref=ref_hires
taus=tau_hires
pars=avg
partm=avg[0,0,0,*]

; now transform the lut into a lut with pdf with 100 bins for each parameter
pars_pdf=fltarr(n_elements(taus),n_elements(ref),n_elements(wp),n_elements(partm),1000)
bins=fltarr(n_elements(partm),1000)
stop
;loop through each parameter to build the pdfs
for p=0, n_elements(partm)-1 do begin
  pmin=min(pars[*,*,*,p])
  pmax=max(pars[*,*,*,p])
  dp=(pmax-pmin)/550.
  if p eq 15 then dp=0.01
  bins[p,*]=findgen(1000)*dp+pmin-(dp*225.) ;make the bins larger than the extent of the modeled parameter by 22.5% on either side
  

  ;now loop through all the lut to calculate the normalized pdf
  for t=0, n_elements(taus)-1 do begin
    for r=0, n_elements(ref)-1 do begin
      for w=0, n_elements(wp)-1 do begin
      mu=pars[t,r,w,p]
      if std[t,r,w,p] lt bins[p,1]-bins[p,0] then sig=bins[p,1]-bins[p,0] else sig=std[t,r,w,p] ; set the error of the pdf to be the standard deviation from the restored file.
      pars_pdf[t,r,w,p,*]=exp((bins[p,*]-mu)^(2.)/(-2.*sig^(2.)))/(sig*sqrt(2.*!PI))
      k=int_tabulated(bins[p,*],pars_pdf[t,r,w,p,*]) ;get the normalization coefficient
      if finite(k) ne 1 then message, 'k is not finite'
      pars_pdf[t,r,w,p,*]=pars_pdf[t,r,w,p,*]/k  ;now normalize
      endfor
    endfor
  endfor
endfor

if test eq 1then fn='/home/leblanc/SSFR3/data/Pars_pdf_LUT'+lbsn+'_test.out' else $
 if test eq 2 then fn='/home/leblanc/SSFR3/data/Pars_pdf_LUT'+lbsn+'_test2.out' else $
  fn='/argus/roof/SSFR3/model/Pars_pdf_LUT'+lbsn+'.out'
if twowvl then fn='/argus/roof/SSFR3/model/Pars_pdf_LUT'+lbsn+'_2wvl.out'


save, pars_pdf, taus, ref, wp, bins, filename=fn

stop
end
