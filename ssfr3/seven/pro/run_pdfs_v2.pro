; program to run through the bayes theorem by multiplying the different pdfs
; uses the pdf lut for the various parameters
; try with a measurement
;
; post_pdf=prior_pdf*meas_pdf*model_pdf
;

@get_params.pro
pro run_pdfs

;load the look up table pdfs
restore, '/home/leblanc/SSFR3/data/Pars_pdf_LUT_hires.out'

;load the measurement
restore, '/home/leblanc/SSFR3/data/20120820_sp_ex.out'
; probably liquid
get_params, wl, sp, par
; par is the array with 11 parameters
bins=double(bins)
; make the pdf of measurement and prior
meas_pdf=double(bins)*0.d
xs=fltarr(1000.)
prio_pdf  =dblarr(n_elements(taus),n_elements(ref),n_elements(wp),n_elements(par)+1)+1.d0
post_jpdf =dblarr(n_elements(taus),n_elements(ref),n_elements(wp),n_elements(par),1000.)
post_pdf  =dblarr(n_elements(taus),n_elements(ref),n_elements(wp),1000.)
post      =dblarr(n_elements(taus),n_elements(ref),n_elements(wp))
post_meas =dblarr(n_elements(taus),n_elements(ref),n_elements(wp),n_elements(par))
post_ind  = post_meas
for p=0, n_elements(par)-1 do begin
  mu=double(par[p])
  sig=abs(mu)*0.005d
  if sig lt 1E-3 then sig = 1E-3
  meas_pdf[p,*]=exp((bins[p,*]-mu)^(2.d)/(-2.d*sig^(2.d)))/(sig*sqrt(2.d*!DPI))
  ; now normalize
  k=int_tabulated(bins[p,*],meas_pdf[p,*]) ;get the normalization coefficien 
;  if p eq 5 then stop
  if k eq 0. then message, 'problem with normalization in measurement pdf'
  meas_pdf[p,*]=meas_pdf[p,*]/k  ;now normalize
;endfor

for t=0, n_elements(taus)-1 do begin
  for r=0, n_elements(ref)-1 do begin
    for w=0, n_elements(wp)-1 do begin
      ; now do the multiplication
  ;    for p=0, n_elements(par)-1 do begin
        post_jpdf[t,r,w,p,*]=meas_pdf[p,*]*pars_pdf[t,r,w,p,*]
        post_meas[t,r,w,p]=total(post_jpdf[t,r,w,p,*])
        if not finite(post_meas[t,r,w,p]) then message, 'problem with inifinite parameter post pdf value'
 ;     endfor
    endfor
  endfor
endfor

; now homogenize ice and liquid
;for p=0, n_elements(par)-1 do begin
  post_meas[*,*,1,p]=post_meas[*,*,1,p]*total(post_meas[*,*,0,p])/total(post_meas[*,*,1,p])
;endfor

 ; now pass the prior to the solution
 prio_pdf[*,*,*,p+1]=post_meas[*,*,*,p]*prio_pdf[*,*,*,p]
 post_ind[*,*,*,p]=post_meas[*,*,*,p]*prio_pdf[*,*,*,0]
endfor ;end par loop

post_pdf=prio_pdf[*,*,*,1:*]
for p=0, n_elements(par)-1 do begin
  post_pdf[*,*,*,p]=post_pdf[*,*,*,p]/total(post_pdf[*,*,*,p])
  post_ind[*,*,*,p]=post_ind[*,*,*,p]/total(post_ind[*,*,*,p])
endfor
post=post_pdf[*,*,*,p-1]

;now normalize the post probabilities
post=post/total(post)
n=max(post,nmax)
ns=array_indices(post, nmax)
print, 'from total: tau:'+strtrim(taus[ns[0]],2)+' ref:'+strtrim(ref[ns[1]],2)+' wp:'+strtrim(wp[ns[2]],2)

;calculate the shanon information content
snorm=alog(n_elements(prio_pdf[*,*,*,0]))/alog(2.)
sprio=(-1.)*total(prio_pdf[*,*,*,0]/total(prio_pdf[*,*,*,0])*alog(prio_pdf[*,*,*,0]/total(prio_pdf[*,*,*,0])+1.E-42)/alog(2.))
spost=(-1.)*total(post*alog(post+1.E-42)/alog(2.))
spostm=fltarr(n_elements(par))
spriom=fltarr(n_elements(par))
sposti=fltarr(n_elements(par))
for p=0, n_elements(par)-1 do begin
  spostm[p]=(-1.)*total(post_pdf[*,*,*,p]/total(post_pdf[*,*,*,p])$
   *alog(post_pdf[*,*,*,p]/total(post_pdf[*,*,*,p])+1.E-42)/alog(2.))
  spriom[p]=(-1.)*total(prio_pdf[*,*,*,p]/total(prio_pdf[*,*,*,p])$
   *alog(prio_pdf[*,*,*,p]/total(prio_pdf[*,*,*,p])+1.E-42)/alog(2.))
  sposti[p]=(-1.)*total(post_ind[*,*,*,p]/total(post_ind[*,*,*,p])$
   *alog(post_ind[*,*,*,p]/total(post_ind[*,*,*,p])+1.E-42)/alog(2.))
endfor
H=(sprio-spost)/snorm
Hm=(spriom-spostm)/snorm
Hi=(sprio-sposti)/snorm
print, H
print, Hm
print, Hi


;save, wl,sp,H, Hm, sprio, snorm, spost, spostm, post, post_jpdf, post_meas, meas_pdf, pars_pdf, $
;      prio_pdf, taus, ref, wp,bins,filename='/home/leblanc/SSFR3/data/pdfs.out'

set_plot, 'x'
window, 0, retain=2
loadct, 39
device, decomposed=0
minv=min(post)
maxv=max(post)
contour, post[*,*,0],taus,ref,title='Liquid',xtitle='tau',ytitle='ref (um)',nlevels=25,/cell_fill,min_value=minv,max_value=maxv
window,1,retain=2
contour, post[*,*,1],taus,ref,title='Ice',xtitle='tau',ytitle='ref (um)',nlevels=25,/cell_fill,min_value=minv,max_value=maxv


stop
end
