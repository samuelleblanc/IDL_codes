; file to contain the set of support procedures and functions to use with retrieve_pdfs_v1
; contains get_sp, get_phase, ...


;; get phase from the values of the parameters
; based on ki square
; pars - lut not binned
; par, value of the 'measurement derived' parameters
; phase - result of the phase
pro get_phase, pars_lut,par,phase
  ic=0 & li=0

  npar=n_elements(pars_lut[0,0,0,*])

  ki=pars_lut[*,*,*,0]*0.
  kis=pars_lut*0.
  coef=fltarr(npar) & add=fltarr(npar)
  for p=0, npar-2 do begin ; loop through the parameters
    coef[p]=1./(max(pars_lut[*,*,*,p],/nan)-min(pars_lut[*,*,*,p],/nan))
    add[p]=min(pars_lut[*,*,*,p]*coef[p],/nan)

    ps=pars_lut[*,*,*,p]*coef[p]-add[p]
    pp=par[p]*coef[p]-add[p]
    w=1.
    kis[*,*,*,p]=w*(pp-ps)^2.
    ki=ki+kis[*,*,*,p]
  endfor ; end of parameter loop

if par[1] gt max(pars_lut[*,9:*,1,1],/nan) then li=1
if par[1] lt min(pars_lut[*,0:29,0,1],/nan) then ic=1

if par[0] lt min(pars_lut[*,0:29,0,0],/nan) then ic=1
if par[8] gt max(pars_lut[*,0:29,0,8],/nan) then ic=1
if par[9] gt max(pars_lut[*,0:29,0,9],/nan) then ic=1

if par[0] gt max(pars_lut[*,9:*,1,0],/nan) then li=1
if par[8] lt min(pars_lut[*,9:*,1,8],/nan) then li=1
if par[9] lt min(pars_lut[*,9:*,1,9],/nan) then li=1

if ic then ki[*,*,0]=ki[*,*,0]*5.
if li then ki[*,*,1]=ki[*,*,1]*5.

nul=min(ki,nn,/nan)
in=array_indices(ki,nn)

phase=in[2]
end



;; get the spectra from the modeled values
pro get_sp,name,taum,refm,wpm,spm,wvl
 restore, name
 taum=tau
 refm=ref
 wpm=[0,1]
 wvl=zenlambda
 spm=sp
end


;; get the lut and make bins out of it
; taui, taul, refi, refl arrays of possible tau and ref values for ice or liquid
; pari, parl lut binned, for ice or liquid
; bini, binl bins of ice or liquid
; taus, refs, wps, set of all values
; pars, non binned value of parameters, np - number of parameters
; nan where par lut is not valid (either small ref ice, or large ref liquid)
pro get_lut,namelut,tau,refi,refl,pari,parl,bini,binl,np,refs,wps,pars,pha=pha,para=para,bina=bina,stds=stds

restore, namelut
; gives the parameter averages, in pars, and standard deviation, std, for the file
; gives taus, refs
tau=taus
refi=refs[9:*]
refl=refs[0:29]
np=n_elements(pars[0,0,0,*])
nb=1400

binsi=fltarr(np,nb)
binsl=fltarr(np,nb)
bmaxi=fltarr(np) & bmini=fltarr(np) & bmaxl=fltarr(np) & bminl=fltarr(np)

for p=0,np-1 do begin
  print, p,mean(std[*,9:*,1,p],/nan),mean(std[*,*,*,p],/nan)/(max(pars[*,*,*,p])-min(pars[*,*,*,p]))*100.,$
           mean(stds*(max(pars[*,9:*,1,p])-min(pars[*,9:*,1,p])))
  print, p,mean(std[*,0:29,0,p],/nan), mean(std[*,0:29,0,p],/nan)/(max(pars[*,0:29,0,p])-min(pars[*,0:29,0,p]))*100.,$
           mean(stds*(max(pars[*,0:29,0,p])-min(pars[*,0:29,0,p])))
;  stop
  if n_elements(stds) gt 0 then std[*,9:*,1,p]=stds*(max(pars[*,9:*,1,p])-min(pars[*,9:*,1,p]))
  pmini=min(pars[*,9:*,1,p]-4.*std[*,9:*,1,p])
  pmaxi=max(pars[*,9:*,1,p]+4.*std[*,9:*,1,p])
  dpi=(pmaxi-pmini)/float(nb)
  if p eq 15 and max(pars[*,9:*,1,p]) eq 1. then dpi=0.01
  binsi[p,*]=findgen(nb)*dpi+pmini;-(dpi*225.) 
  ;make the bins larger than the extent of the modeled parameter by 22.5% on either side

  if n_elements(stds) gt 0 then std[*,0:29,0,p]=stds*(max(pars[*,0:29,0,p])-min(pars[*,0:29,0,p]))
  pminl=min(pars[*,0:29,0,p]-4.*std[*,0:29,0,p])
  pmaxl=max(pars[*,0:29,0,p]+4.*std[*,0:29,0,p])
  dpl=(pmaxl-pminl)/float(nb) ;550. 
  if p eq 15 and max(pars[*,0:29,0,p]) eq 1. then dpi=0.01 
  binsl[p,*]=findgen(nb)*dpl+pminl;-(dpl*225.)
  bmaxi[p]=max(binsi[p,*]) & bmini[p]=min(binsi[p,*])
  bmaxl[p]=max(binsl[p,*]) & bminl[p]=min(binsl[p,*])
  ;if p eq 15 then stop
  ;if p eq 4 then bmaxi=5.0

endfor

pari=pdf_lut(reform(pars[*,9:*,1,*]), reform(std[*,9:*,1,*]), bmaxi,bmini,bins=bini,nb=nb)
parl=pdf_lut(reform(pars[*,0:29,0,*]), reform(std[*,0:29,0,*]), bmaxl,bminl,bins=binl,nb=nb)


if pha then begin
  bmaxa=bmaxi & bmina=bmini
  for i=0,1 do begin
   bmaxa[i]=max([bmaxi[i],bmaxl[i]])
   bmina[i]=min([bmini[i],bminl[i]])
  endfor  

  parai=pdf_lut(reform(pars[*,9:*,1,*]), reform(std[*,9:*,1,*]), bmaxa,bmina,bins=bina,nb=nb)
  paral=pdf_lut(reform(pars[*,0:29,0,*]), reform(std[*,0:29,0,*]), bmaxa,bmina,bins=bina,nb=nb)

  para=fltarr(n_elements(pars[*,0,0,0]),n_elements(pars[*,0,0,0]),2,np,nb)
  para[*,9:*,1,*,*]=parai
  para[*,0:29,0,*,*]=paral
  
endif
end



;; get the lut and make bins out of it
; taui, taul, refi, refl arrays of possible tau and ref values for ice or liquid
; pari, parl lut binned, for ice or liquid
; bini, binl bins of ice or liquid
; taus, refs, wps, set of all values
; pars, non binned value of parameters, np - number of parameters
; nan where par lut is not valid (either small ref ice, or large ref liquid)
pro get_lut_v2,namelut,tau,refi,refl,pari,parl,bini,binl,np,refs,wps,pars,pexs,pha=pha,para=para,bina=bina,stds=stds

restore, namelut
sp_std=std
std=stdp 
pars=par

; gives the parameter averages, in pars, and standard deviation, std, for the file
; gives taus, refs
tau=taus
refi=refs[9:*]
refl=refs[0:29]
np=n_elements(pars[0,0,0,*])
nz=n_elements(zenlambda)
nb=1400
nn=20000
binsi=fltarr(np,nb)
binsl=fltarr(np,nb)
bmaxi=fltarr(np) & bmini=fltarr(np) & bmaxl=fltarr(np) & bminl=fltarr(np)
pexs=fltarr(4,np)

; build the bin sizes
for p=0,np-1 do begin
  print, p,mean(std[*,9:*,1,p],/nan),mean(std[*,*,*,p],/nan)/(max(pars[*,*,*,p])-min(pars[*,*,*,p]))*100.,$
           mean(stds*(max(pars[*,9:*,1,p])-min(pars[*,9:*,1,p])))
  print, p,mean(std[*,0:29,0,p],/nan), mean(std[*,0:29,0,p],/nan)/(max(pars[*,0:29,0,p])-min(pars[*,0:29,0,p]))*100.,$
           mean(stds*(max(pars[*,0:29,0,p])-min(pars[*,0:29,0,p])))
  if n_elements(stds) gt 0 then std[*,9:*,1,p]=stds*(max(pars[*,9:*,1,p])-min(pars[*,9:*,1,p]))
  pmini=min(pars[*,9:*,1,p]-4.*std[*,9:*,1,p])
  pmaxi=max(pars[*,9:*,1,p]+4.*std[*,9:*,1,p])
  pexs[2,p]=min(pars[*,9:*,1,p],/nan)
  pexs[3,p]=max(pars[*,9:*,1,p],/nan)
  dpi=(pmaxi-pmini)/float(nb)
  if p eq 15 and max(pars[*,9:*,1,p]) eq 1. then dpi=0.01
  binsi[p,*]=findgen(nb)*dpi+pmini;-(dpi*225.) 
  ;make the bins larger than the extent of the modeled parameter by 22.5% on either side

  if n_elements(stds) gt 0 then std[*,0:29,0,p]=stds*(max(pars[*,0:29,0,p])-min(pars[*,0:29,0,p]))
  pminl=min(pars[*,0:29,0,p]-4.*std[*,0:29,0,p])
  pmaxl=max(pars[*,0:29,0,p]+4.*std[*,0:29,0,p])
  pexs[0,p]=min(pars[*,0:29,0,p],/nan)
  pexs[1,p]=max(pars[*,0:29,0,p],/nan)
  dpl=(pmaxl-pminl)/float(nb) ;550. 
  if p eq 15 and max(pars[*,0:29,0,p]) eq 1. then dpi=0.01
  binsl[p,*]=findgen(nb)*dpl+pminl;-(dpl*225.)
  bmaxi[p]=max(binsi[p,*]) & bmini[p]=min(binsi[p,*])
  bmaxl[p]=max(binsl[p,*]) & bminl[p]=min(binsl[p,*])
  ;if p eq 15 then stop
  ;if p eq 4 then bmaxi=5.0
endfor

; now make the luts
pari=pdf_lut_sp(reform(sp[*,9:*,1,*]),reform(sp_std[*,9:*,1,*]), wvl,bmaxi,bmini,bins=bini,nb=nb,nn=nn)
parl=pdf_lut_sp(reform(sp[*,0:29,0,*]), reform(sp_std[*,0:29,0,*]), wvl,bmaxl,bminl,bins=binl,nb=nb,nn=nn)
 
if pha then begin
  bmaxa=bmaxi & bmina=bmini
  for i=0,1 do begin
   bmaxa[i]=max([bmaxi[i],bmaxl[i]])
   bmina[i]=min([bmini[i],bminl[i]])
  endfor
  parai=pdf_lut(reform(pars[*,9:*,1,*]), reform(std[*,9:*,1,*]), bmaxa,bmina,bins=bina,nb=nb)
  paral=pdf_lut(reform(pars[*,0:29,0,*]), reform(std[*,0:29,0,*]), bmaxa,bmina,bins=bina,nb=nb)
  para=fltarr(n_elements(pars[*,0,0,0]),n_elements(pars[*,0,0,0]),2,np,nb)
  para[*,9:*,1,*,*]=parai
  para[*,0:29,0,*,*]=paral 
endif
end




;; function to build the pdf from a set of sp
function pdf_lut_sp, sp,std,wvl,bmax,bmin,bins=bins,nb=nb,nn=nn

nt=n_elements(sp[*,0,0]) & nr=n_elements(sp[0,*,0]) & nv=n_elements(wvl)
get_params,wvl,sp[0,0,*],par
np=n_elements(par)
pars=fltarr(nt,nr,np,nb)

spp=fltarr(nv,nn)
parr=fltarr(np,nn)
bins=fltarr(np,nb)

for t=0,nt-1 do begin
  for r=0, nr-1 do begin
    for v=0, nv-1 do spp[v,*]=gsim(sp[t,r,v],std[t,r,v],nn)
    for e=0, nn-1 do begin
      get_params,wvl,spp[*,e],par
      parr[*,e]=par
    endfor
    for p=0, np-1 do begin
      pars[t,r,p,*]=histogram(parr[p,*],nbins=nb,min=bmin[p],max=bmax[p],locations=bi)
      bins[p,*]=bi
    endfor
   ; stop
    print, t,r
  endfor ;ref loop
endfor ;tau loop
stop
return, pars
end




;; get the lut and make bins out of it
; taui, taul, refi, refl arrays of possible tau and ref values for ice or liquid
; pari, parl lut binned, for ice or liquid
; bini, binl bins of ice or liquid
; taus, refs, wps, set of all values
; pars, non binned value of parameters, np - number of parameters
; nan where par lut is not valid (either small ref ice, or large ref liquid)
pro get_lut_test,namelut,tau,refi,refl,pari,parl,bini,binl,np,refs,wps,pars,int=int

; for testing, boht ice and liquid are the same

restore, namelut
; gives the parameter averages, in pars, and standard deviation, std, for the file
; gives taus, refs
tau=tausmodresliq ;taus
refi=reffsmodresliq ; refs[9:*]
refl=reffsmodresliq ;refs[0:29]
refs=refl
np=2 ;n_elements(pars[0,0,0,*])
nb=4000

imu=18 ; for mu of 0.85

pars=[[[reform(xmitmodresliq515[imu,*,*])]],[[reform(slopemodresliq1600[imu,*,*])]]]
std=reform(abs(pars*0.d0+0.03d))
std[*,*,1]=0.003d ; max(pars[*,*,1])*0.03
std[*,*,0]=0.03d
;stop
binsi=fltarr(np,nb)
binsl=fltarr(np,nb)
bmaxi=fltarr(np) & bmini=fltarr(np) & bmaxl=fltarr(np) & bminl=fltarr(np)
for p=0,np-1 do begin
  pmini=min(pars[*,*,p]-4.*std[*,*,p])
  pmaxi=max(pars[*,*,p]+4.*std[*,*,p])
  dpi=(pmaxi-pmini)/float(nb)
  if p eq 15 then dpi=0.01
  binsi[p,*]=findgen(nb)*dpi+pmini;-(dpi*225.) ;make the bins larger than the extent of the modeled parameter by 22.5% on either side

;  pminl=min(pars[*,0:29,0,p])
;  pmaxl=max(pars[*,0:29,0,p])
;  dpl=(pmaxl-pminl)/550.
;  if p eq 15 then dpi=0.01
;  binsl[p,*]=findgen(1000)*dpl+pminl-(dpl*225.)
  
  bmaxi[p]=max(binsi) & bmini[p]=min(binsi)
  bmaxl[p]=max(binsi) & bminl[p]=min(binsi)
endfor

bmaxl=bmaxi & bminl=bmini

;pari=pdf_lut(pars, reform(abs(pars*0.03)), bmaxi,bmini,bins=bini)
pari=pdf_lut(pars, std, bmaxi,bmini,bins=bini,int=int,nb=nb)
parl=pdf_lut(pars, std, bmaxi,bmini,bins=bini,int=int,nb=nb)
;parl=pdf_lut(reform(pars), reform(abs(pars*0.03)), bmaxl,bminl,bins=binl)
binl=bini
end


;;;;; function to make a pdf of the bins of the parameter and std
function pdf_lut, avg,std,dmax,dmin,bins=bins,int=int,nb=nb

if n_elements(int) lt 1 then int=0
if int then print, 'integrating and not summing'
nt=n_elements(avg[*,0,0])
nr=n_elements(avg[0,*,0])
;nw=n_elements(avg[0,0,*])
np=n_elements(avg[0,0,*])
if n_elements(nb) lt 1 then nb=1000

pdf=fltarr(nt,nr,np,nb)

; now transform the lut into a lut with pdf with 100 bins for each parameter
bins=fltarr(np,nb)

print, 'starting to build the pars pdf'
;loop through each parameter to build the pdfs
for p=0, np-1 do begin
  dp=(dmax[p]-dmin[p])/nb
  bins[p,*]=findgen(nb)*dp+dmin[p] ;make the bins larger than the extent of the modeled parameter by 22.5% on either side


  ;now loop through all the lut to calculate the normalized pdf
  for t=0, nt-1 do begin
    for r=0, nr-1 do begin
      mu=avg[t,r,p]
      if std[t,r,p] lt bins[p,1]-bins[p,0] or not finite(std[t,r,p]) then sig=bins[p,1]-bins[p,0] else sig=std[t,r,p] 
       ; set the error of the pdf to be the standard deviation from the restored file.
      pdf[t,r,p,*]=exp((bins[p,*]-mu)^(2.)/(-2.*sig^(2.)))/(sig*sqrt(2.*!PI))
      if not int then k=total(pdf[t,r,p,*]) else k=int_tabulated(bins[p,*],pdf[t,r,p,*]) ;get the normalization coefficient
      if finite(k) ne 1 then message, 'k is not finite'
      pdf[t,r,p,*]=pdf[t,r,p,*]/k  ;now normalize
;  if n_elements(where(pdf[t,r,p,*]/max(pdf[t,r,p,*]) gt 0.05)) lt 6 then message, 'less than 6 points in model pdf more than 5%'
    endfor
  endfor
endfor

return,pdf
end

; make a function to calculate the shannon information content
function SIC, post
  ;calculate the shanon information content
  snorm=alog(double(n_elements(post)))/alog(2.d)
  prio_pdf=post*0.d0+1.d0
  sprio=(-1.d)*total(prio_pdf/total(prio_pdf)*alog(prio_pdf/total(prio_pdf))/alog(2.d))
  spost=(-1.d)*total(post*alog(post)/alog(2.d),/nan)
  H=(sprio-spost)/snorm
return,h
end


;function to return a gaussian
function gsim, mu,sig,n
v=randomu(seed,n,/normal)
v=v*sig
k=v+mu
return,k
end

