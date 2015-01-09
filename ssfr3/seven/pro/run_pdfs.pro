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
prio_pdf  =dblarr(n_elements(taus),n_elements(ref),n_elements(wp))
post_jpdf =dblarr(n_elements(taus),n_elements(ref),n_elements(wp),n_elements(par),1000.)
post_pdf  =dblarr(n_elements(taus),n_elements(ref),n_elements(wp),1000.)
post      =dblarr(n_elements(taus),n_elements(ref),n_elements(wp))
post_max  =dblarr(n_elements(taus),n_elements(ref),n_elements(wp))
post_std  =dblarr(n_elements(taus),n_elements(ref),n_elements(wp))
post_ctr  =dblarr(n_elements(taus),n_elements(ref),n_elements(wp))
post_hgt  =dblarr(n_elements(taus),n_elements(ref),n_elements(wp))
post_meas =dblarr(n_elements(taus),n_elements(ref),n_elements(wp),n_elements(par))
for p=0, n_elements(par)-1 do begin
  mu=double(par[p])
  sig=abs(mu)*0.005d
  if sig lt 1E-3 then sig = 1E-3
  meas_pdf[p,*]=exp((bins[p,*]-mu)^(2.d)/(-2.d*sig^(2.d)))/(sig*sqrt(2.d*!DPI))
  ; now normalize
  k=int_tabulated(bins[p,*],meas_pdf[p,*]) ;get the normalization coefficient
;  if p eq 5 then stop
  if k eq 0. then message, 'problem with normalization in measurement pdf'
  meas_pdf[p,*]=meas_pdf[p,*]/k  ;now normalize
endfor

; now make the prior
; set it constant everywhere to start
for t=0, n_elements(taus)-1 do begin
  for r=0, n_elements(ref)-1 do begin
    for w=0, n_elements(wp)-1 do begin
      prio_pdf[t,r,w]=1./(n_elements(taus)*n_elements(ref)*n_elements(wp))
       if ref[r] le 5. and w eq 1 then prio_pdf[t,r,w]=0.0/(n_elements(taus)*n_elements(ref)*n_elements(wp))
      ; now do the multiplication
      for p=0, n_elements(par)-1 do begin
        post_jpdf[t,r,w,p,*]=prio_pdf[t,r,w]*meas_pdf[p,*]*pars_pdf[t,r,w,p,*]
        post_meas[t,r,w,p]=total(post_jpdf[t,r,w,p,*])
        if not finite(post_meas[t,r,w,p]) then message, 'problem with inifinite parameter post pdf value'
      endfor
 ;     for i=0, 999 do begin
 ;       post_pdf[t,r,w,i]=total(post_jpdf[t,r,w,*,i])
 ;       xs[i]=total(bins[*,i])
 ;     endfor
  ;    post[t,r,w]=total(post_pdf[t,r,w,*]) ; this is the total likelyhood
     ; post_max[t,r,w]=max(post_pdf[t,r,w,*]) ; this returns the maximum likelyhood
     ; gauss=gaussfit(xs,reform(post_pdf[t,r,w,*]),a)
     ; post_std[t,r,w]=a[2]
     ; post_ctr[t,r,w]=a[1]
     ; post_hgt[t,r,w]=a[0]
    endfor
  endfor
endfor

gs=fltarr(n_elements(par))
gs[0]=total(post_meas[*,*,*,0])
for p=1, n_elements(par)-1 do begin ; homogenization
  gs[p]=total(post_meas[*,*,*,p])
  post_meas[*,*,*,p]=post_meas[*,*,*,p]*gs[0]/gs[p] 
endfor
stop

;integral over all pns
for t=0, n_elements(taus)-1 do begin
  for r=0, n_elements(ref)-1 do begin
    for w=0, n_elements(wp)-1 do begin
      post[t,r,w]=total(post_meas[t,r,w,*])
    endfor
  endfor
endfor

;now normalize the post probabilities
post=post/total(post)
;post_max=post_max/total(post_max)
;for p=0,n_elements(par)-1 do post_meas[*,*,*,p]=post_meas[*,*,*,p]/total(post_meas[*,*,*,p])
n=max(post,nmax)
ns=array_indices(post, nmax)
print, 'from total: tau:'+strtrim(taus[ns[0]],2)+' ref:'+strtrim(ref[ns[1]],2)+' wp:'+strtrim(wp[ns[2]],2)
;n=max(post_max,nmax)
;ns=array_indices(post, nmax)
;print, 'from maximum: tau:'+strtrim(taus[ns[0]],2)+' ref:'+strtrim(ref[ns[1]],2)+' wp:'+strtrim(wp[ns[2]],2)
;n=min(post_std,nmin)
;ns=array_indices(post, nmin)
;print, 'from smallest std: tau:'+strtrim(taus[ns[0]],2)+' ref:'+strtrim(ref[ns[1]],2)+' wp:'+strtrim(wp[ns[2]],2)
;n=max(post_hgt,nmax)
;ns=array_indices(post, nmax)
;print, 'from max gauss height: tau:'+strtrim(taus[ns[0]],2)+' ref:'+strtrim(ref[ns[1]],2)+' wp:'+strtrim(wp[ns[2]],2)

;calculate the shanon information content
snorm=alog(n_elements(prio_pdf))/alog(2.)
sprio=(-1.)*total(prio_pdf*alog(prio_pdf+1.E-42)/alog(2.))
spost=(-1.)*total(post*alog(post+1.E-42)/alog(2.))
spostm=fltarr(n_elements(par))
for p=0, n_elements(par)-1 do spostm[p]=(-1.)*total(post_meas[*,*,*,p]/total(post_meas[*,*,*,p])*alog(post_meas[*,*,*,p]/total(post_meas[*,*,*,p])+1.E-42)/alog(2.))
H=(sprio-spost)/snorm
Hm=(sprio-spostm)/snorm
print, H
print, Hm


save, wl,sp,H, Hm, sprio, snorm, spost, spostm, post, post_jpdf, post_meas, meas_pdf, pars_pdf, $
      prio_pdf, taus, ref, wp,bins,filename='/home/leblanc/SSFR3/data/pdfs.out'

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
