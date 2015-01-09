; program that builds the look up table (lut) for spectral retrieval of transmitted radiance through cloud
; input is the various spectra, output is effective radius, optical depth, and number 0 to 1 
; for portion of liquid to ice water content

;@get_params_2wvl.pro
;@get_params_test.pro
@get_params.pro
pro build_lut

test=0
twowvl=0
snow=0

;restore, '/argus/roof/SSFR3/model/sp_liq_ice_new3.out'
;ntau=tau
;nref=ref
;nsp=sp
;restore, '/argus/roof/SSFR3/model/sp_liq_ice_new3_low.out'
;sps=fltarr(9,n_elements(ref),n_elements(zenlambda),2)
;taus=[tau[0:3],ntau]
;ref=ref
;sps[0:3,*,*,*]=sp[0:3,*,*,*]
;sps[4:8,*,*,*]=nsp

  if snow then begin
  lbl=['hires4_20130110.out',$
     'hires4_20130111.out',$
     'hires4_snow.out']
  endif else begin
  lbl=['hires4_20120524.out',$
       'hires4_20120804.out',$
       'hires4_20120812.out',$
       'hires4_20120820.out',$
       'hires4_20120913.out',$
       'hires4_20120525_win.out',$
       'hires4_20120525_high.out',$
       'hires4_20120525_low.out',$
       'hires4_20120523_snd.out',$
       'hires4_20120525_snd.out',$
       'hires4_20120602_snd.out',$
       'hires4_20120806_snd.out',$
       'hires4_20120813_snd.out',$
       'hires4_20120816_snd.out',$
       'hires4_20120820_snd.out',$
       'hires4_20120824_snd.out',$
       'hires4_20120912_snd.out']
  endelse

for j=0, n_elements(lbl)-1 do begin

label=lbl[j] ;'hires_20120523_snd' ;'hires_snow'

restore, '/argus/roof/SSFR3/model/sp_'+label
sps=sp
taus=tau
wp=[0.,1.]
get_params, zenlambda, sps[0,0,*,0],partm
pars=fltarr(n_elements(taus),n_elements(ref),n_elements(wp),n_elements(partm))

tau_hires=findgen(200)+1.
ref_hires=findgen(50)+1.
par_hires=fltarr(n_elements(tau_hires),n_elements(ref_hires),n_elements(wp),n_elements(partm))
par_hitm=fltarr(n_elements(tau_hires),n_elements(ref),n_elements(wp),n_elements(partm))
par_hirm=fltarr(n_elements(tau),n_elements(ref_hires),n_elements(wp),n_elements(partm))
for w=0, n_elements(wp)-1 do begin
  for t=0, n_elements(taus)-1  do begin
    for r=0, n_elements(ref)-1 do begin
      get_params, zenlambda, sps[t,r,*,w],partm
      if finite(total(partm)) ne 1 then begin
        for p=0, n_elements(partm)-1 do pars[t,r,w,p]=interpol(pars[t,[r-2,r-1],w,p],[r-2,r-1],[r])
        ;message, 'not finite partm' 
      endif else pars[t,r,w,*]=partm
    endfor
  endfor
endfor

; for special considerations because liquid water is not present above 25.
nrmax=where(ref eq 25.)

  for w=0, n_elements(wp)-1 do begin
    for p=0, n_elements(partm)-1 do begin
      for t=0, n_elements(tau)-1 do begin
        if w eq 0 then par_hirm[t,*,w,p]=interpol(pars[t,0:nrmax,w,p],ref[0:nrmax],ref_hires) $ ; for the newest versions of sp_hires4, water is calculated above 25 microns
        else par_hirm[t,*,w,p]=interpol(pars[t,*,w,p],ref,ref_hires)
        if finite(total(par_hirm[t,*,w,p])) ne 1 then message, 'par_hirm not finite'
      endfor
      for r=0, n_elements(ref_hires)-1 do begin
        par_hires[*,r,w,p]=interpol(par_hirm[*,r,w,p],taus,tau_hires)
        if finite(total(par_hires[*,r,w,p])) ne 1 then message, 'Par_hires not finite'
      endfor
   endfor
  endfor


;stop
if test eq 1 then label=label+'_test'
if test eq 2 then label=label+'_test2'
if twowvl then label=label+'_2wvl'
save, par_hires, tau_hires,ref_hires, pars, taus, ref, wp, filename='/argus/roof/SSFR3/model/Pars_LUT_v4_'+label ;'/home/leblanc/SSFR3/data/Pars_LUT_'+label+'.out'
endfor
stop

if 0 then begin
ref=ref_hires
taus=tau_hires
pars=par_hires
; now transform the lut into a lut with pdf with 100 bins for each parameter
pars_pdf=fltarr(n_elements(taus),n_elements(ref),n_elements(wp),n_elements(partm),1000)
bins=fltarr(n_elements(partm),1000)

;loop through each parameter to build the pdfs
for p=0, n_elements(partm)-1 do begin
  pmin=min(pars[*,*,*,p])
  pmax=max(pars[*,*,*,p])
  dp=(pmax-pmin)/550.
  if p eq 15 then dp=0.01
  bins[p,*]=findgen(1000)*dp+pmin-(dp*225.) ;make the bins larger than the extent of the modeled parameter by 22.5% on either side
  sig=max([abs(pmin),abs(pmax)])*0.02 ;set an error for each pdf at constant 2% of highest value
  
  ;now loop through all the lut to calculate the normalized pdf
  for t=0, n_elements(taus)-1 do begin
    for r=0, n_elements(ref)-1 do begin
      for w=0, n_elements(wp)-1 do begin
      mu=pars[t,r,w,p]
      pars_pdf[t,r,w,p,*]=exp((bins[p,*]-mu)^(2.)/(-2.*sig^(2.)))/(sig*sqrt(2.*!PI))
      k=int_tabulated(bins[p,*],pars_pdf[t,r,w,p,*]) ;get the normalization coefficient
      pars_pdf[t,r,w,p,*]=pars_pdf[t,r,w,p,*]/k  ;now normalize
      endfor
    endfor
  endfor 
endfor

save, pars_pdf, taus, ref, wp, bins, filename='/home/leblanc/SSFR3/data/Pars_pdf_LUT_'+label+'.out'
endif
stop
end
