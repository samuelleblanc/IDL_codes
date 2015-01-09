; program to compare parameters luts create via differente surface albedos

pro pars_lut

dir='/home/leblanc/SSFR3/data/'
test=0
twowvl=1
snow=0

if test eq 1 then begin
lbl=[$;'Pars_LUT_hires_20120524_test.out',$
;     'Pars_LUT_hires_20120804_test.out',$
;     'Pars_LUT_hires_20120812_test.out',$
;     'Pars_LUT_hires_20120820_test.out',$
;     'Pars_LUT_hires_20120913_test.out',$
     'Pars_LUT_hires_20130110_test.out',$
     'Pars_LUT_hires_20130111_test.out',$
;     'Pars_LUT_hires_win_test.out',$
     'Pars_LUT_hires_snow_test.out'];,$
;     'Pars_LUT_hires_high_test.out']
endif else begin
if test eq 2 then begin
lbl=[$;'Pars_LUT_hires_20120524_test2.out',$
   ;  'Pars_LUT_hires_20120804_test2.out',$
  ;   'Pars_LUT_hires_20120812_test2.out',$
 ;    'Pars_LUT_hires_20120820_test2.out',$
;     'Pars_LUT_hires_20120913_test2.out',$
     'Pars_LUT_hires_20130110_test2.out',$
     'Pars_LUT_hires_20130111_test2.out',$
;     'Pars_LUT_hires_win_test2.out',$
     'Pars_LUT_hires_snow_test2.out'] ;,$
;     'Pars_LUT_hires_high_test2.out']
endif else begin
lbl=[$;'Pars_LUT_hires_20120524.out',$
    ; 'Pars_LUT_hires_20120804.out',$
    ; 'Pars_LUT_hires_20120812.out',$
    ; 'Pars_LUT_hires_20120820.out',$
    ; 'Pars_LUT_hires_20120913.out',$
     'Pars_LUT_hires_20130110.out',$
     'Pars_LUT_hires_20130111.out',$
;     'Pars_LUT_hires_win.out',$
     'Pars_LUT_hires_snow.out'];,$
;     'Pars_LUT_hires_high.out']
  endelse
endelse
if twowvl then begin
  if snow then begin
  lbl=['Pars_LUT_hires_20130110_2wvl.out',$
     'Pars_LUT_hires_20130111_2wvl.out',$
     'Pars_LUT_hires_snow_2wvl.out']
  endif else begin
  lbl=['Pars_LUT_hires_20120524_2wvl.out',$
       'Pars_LUT_hires_20120804_2wvl.out',$
       'Pars_LUT_hires_20120812_2wvl.out',$
       'Pars_LUT_hires_20120820_2wvl.out',$
       'Pars_LUT_hires_20120913_2wvl.out',$
       'Pars_LUT_hires_win_2wvl.out',$
       'Pars_LUT_hires_high_2wvl.out']
  endelse
endif



restore, dir+lbl[0]
;save, par_hires, tau_hires,ref_hires, pars, taus, ref, wp

par=fltarr(n_elements(tau_hires),n_elements(ref_hires),n_elements(wp),n_elements(par_hires[0,0,0,*]),n_elements(lbl))

for i=0, n_elements(lbl)-1 do begin
  restore, dir+lbl[i]
  par[*,*,*,*,i]=par_hires
endfor

std=par_hires*0.
mn=par_hires*0.
mx=par_hires*0.
avg=par_hires*0.
for t=0, n_elements(tau_hires)-1 do begin
  for r=0, n_elements(ref_hires)-1 do begin
    for w=0, n_elements(wp)-1 do begin
      for p=0, n_elements(par_hires[0,0,0,*])-1 do begin
        std[t,r,w,p]=stddev(par[t,r,w,p,*],/nan)
        mn[t,r,w,p]=min(par[t,r,w,p,*])
        mx[t,r,w,p]=max(par[t,r,w,p,*])
        avg[t,r,w,p]=mean(par[t,r,w,p,*],/nan)
      endfor
    endfor
  endfor
endfor


if snow then lbsn='_snow' else lbsn=''
if test eq 1 then fn=dir+'pars_std'+lbsn+'_test.out' else if test eq 2 then fn=dir+'pars_std'+lbsn+'_test2.out' else fn=dir+'pars_std'+lbsn+'.out'
if twowvl then fn=dir+'pars_std'+lbsn+'_2wvl.out'
save, std,avg,tau_hires,ref_hires,wp,filename=fn

stop


end
