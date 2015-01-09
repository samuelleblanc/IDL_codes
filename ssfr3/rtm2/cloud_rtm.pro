; program to run the cloud transmission retrieval technique descirbed by Patrick
; based on the retrieve_cloud_params from calnex
; added only tau keyword for when I only want to obtain tau


pro cloud_rtm, sza, spectra, lambda, tau, ref, irr=irr, tauonly=tauonly, hires=hires,twowvl=twowvl,lut=lut, dtau=dtau,dref=dref

iw=12 ; wavelength index
;iw=5 ; new wavelength index

if keyword_set(twowvl) then twowvl=1 else twowvl=0
if keyword_set(irr) then irr=1 else irr=0
if keyword_set(tauonly) then tauonly=1 else tauonly=0
if keyword_set(hires) then hires=1 else hires=0

if twowvl then lbl='_2wvl' else lbl=''

if n_elements(lut) lt 1 then begin
;restore the lut from file
if hires then restore, '/argus/roof/SSFR3/model/CLD_LUT_HIRES_v2'+lbl+'.out' else $  ;'/home/leblanc/DC3_SEAC4RS/library/CLD_LUT_HIRES2'+lbl+'.out' else $
              restore, '/argus/roof/SSFR3/model/CLD_LUT_v2'+lbl+'.out'  ;'/home/leblanc/DC3_SEAC4RS/library/CLD_LUT'+lbl+'.out'
endif else begin
  ;lut={radhi:radhi,refs_hi:refs_hi,slohi:slohi,sun:sun,szas:szas,taus_hi:taus_hi,wvls:wvls}
  radhi=lut.radhi
  refs_hi=lut.refs_hi
  slohi=lut.slohi
  sun=lut.sun
  szas=lut.szas
  taus_hi=lut.taus_hi
  wvls=lut.wvls
endelse
; now interpolate the measured spectra to the proper wavelengths
sp=interpol(spectra,lambda,wvls)
;stop
; now interpolate the resulting lut to the proper sza
if irr then begin ; set the correct model to look at
  mslo=sloirr
  m515=irr500
endif else begin
  if hires then begin
    mslo=slohi
    m515=radhi
    taus=taus_hi
    refs=refs_hi
  endif else begin
    mslo=slorad
    m515=rad500
  endelse
endelse

if hires then begin
  mslo=slohi
  m515=radhi
  taus=taus_hi
  refs=refs_hi
endif

;prepare variables to store interpolate values
ms = fltarr(n_elements(taus),n_elements(refs)) ;model slope
ma = fltarr(n_elements(taus),n_elements(refs)) ;model value at 515
su = fltarr(n_elements(wvls)) ; sun value

for i=0,n_elements(taus)-1 do begin
  for j=0, n_elements(refs)-1 do begin
    ms[i,j]=interpol(mslo[i,j,*],szas,sza)
    ma[i,j]=interpol(m515[i,j,*],szas,sza)
  ;if sza lt 84 and i gt 7 then stop
  endfor
endfor

;interpol sun
;for i=0, n_elements(wvls)-1 do if irr then su[i]=interpol(suni[i,*],szas,sza) else su[i]=interpol(sun[i,*],szas,sza)
;'
;change the measured spectra to transmission
sp=sp/sun/cos(sza*!dtor)
;stop
; calculate the slope and value at 515 for the measurement
slp=sp[1:*]/sp[1]
tmp=linfit(wvls[1:*],slp)
if twowvl then slope=sp[iw] else slope=tmp[1]
r515=sp[0];*0.80
deltarad=0.05 ; set the uncertainty

; now determine the xi in each value of the look up table
a_err=((ma-r515)/ma)^2.0
s_err=((ms-slope)/ms)^2.0
total_err=sqrt((a_err+s_err)/2.0)
;stop
; find the ref and tau at the coarser grid
nul=min(abs(total_err),nmin)
ind=array_indices(total_err,nmin)
tau=taus[ind[0]]
ref=refs[ind[1]]

sps=sp[1:*]/sp

if twowvl then get_uncertainty_2wvl, r515,slope,sps,ma,ms,total_err,taus, refs,dtau,dref else $
 get_uncertainty, r515,slope,sps,ma,ms,total_err,taus, refs,dtau,dref

;stop

; now make a subset grid to refine the value at 5% intervals
;rs2=(findgen(50)-25.)*ref*0.015+ref
;ts2=(findgen(50)-25.)*tau*0.015+tau
;r5=fltarr(n_elements(taus),n_elements(rs2))
;s5=fltarr(n_elements(taus),n_elements(rs2))
;rr=fltarr(n_elements(ts2),n_elements(rs2))
;ss=fltarr(n_elements(ts2),n_elements(rs2))
;for i=0, n_elements(taus)-1 do begin
;  r5[i,*]=interpol(ma[i,*],refs,rs2)
;  s5[i,*]=interpol(ms[i,*],refs,rs2)
;endfor
;for i=0, n_elements(rs2)-1 do begin
;  rr[*,i]=interpol(r5[*,i],taus,ts2)
;  ss[*,i]=interpol(s5[*,i],taus,ts2)
;endfor
; now determine the xi in each value of the look up table
;a_err2=((rr-r515)/ma)^2.0
;s_err2=((ss-slope)/ms)^2.0
;total_err2=sqrt((a_err2+s_err2)/2.0)

; find the ref and tau at the coarser grid
;nul=min(abs(total_err2),nmin)
;ind=array_indices(total_err2,nmin)
;tauh=ts2[ind[0]]
;refh=rs2[ind[1]]


; now interpolate to a larger grid
;triangulate, taus, refs,tri
;te=griddata(taus, refs, total_err, dimensions=[70,70],delta=[2.5,0.5],start=[5.,2.],method='NaturalNeighbor',triangle=tri)
;rs=findgen(70)*0.4+2.
;ts=findgen(70)*2.2+5.
;ts=taus
;rs=findgen(10)*2.5+1.25


;tu=fltarr(n_elements(taus),n_elements(rs))
;te=fltarr(n_elements(ts),n_elements(rs))
;for i=0,n_elements(taus)-1 do tu[i,*]=interpol(total_err[i,*],refs,rs)
;for i=0,n_elements(rs)-1 do te[*,i]=interpol(tu[*,i],taus,ts)

;trian.gulate, ts, rs, tri
;tee=griddata(ts,rs,te, dimension=[70,70],delta=[2.2,0.4],start=[5.,2.],triangle=tri,method='InverseDistance')

;rs=findgen(70)*0.4+2.
;ts=findgen(70)*2.2+5.

tee=total_err
rs=refs
ts=taus
nul=min(abs(tee),nmin)
ind=array_indices(tee, nmin)

iref=0
itau=0
;taus[itau]=ts[ind[0]]
;refs[iref]=rs[ind[1]]

if 0 then begin
set_plot,'x'
device, decomposed=0
loadct,39
window, 0, retain=2, title='interpolate'
v=(findgen(20)/3.)^2.0
contour,tee, ts, rs, /cell_fill, levels=v
plots, ts[ind[0]],rs[ind[1]],psym=5

window, 1, retain=2, title='original'
v=(findgen(20)/2.)^2.1
contour,total_err,taus, refs, /cell_fill, levels=v
plots, tau, ref, psym=5

;window, 2, retain=2, title='subset'
;contour,total_err2,ts2, rs2, /cell_fill, levels=v
;plots, tauh,refh,psym=5

window, 3, retain=2, title='LUT'
plot, ma,ms, psym=2
;oplot, rr,ss, psym=1
;oplot, rr,ss, psym=1
plots, r515,slope,psym=5, color=250

stop
endif
;taus[itau]=ts[ind[0]]
;refs[iref]=rs[ind[1]]


; now find the location of the minimum xi
;nul=min(total_err,i_min)
  
;iref=floor(i_min/n_elements(taus))
;itau=i_min-n_elements(taus)*iref

if tauonly then begin
  nul=min(abs(refs-10.0),iref)
  ;a_err=((ma[*,iref]-r515)/ma[*,iref])^2.0
  ;nul=min(abs(ma[*,iref]-r515),itau)
  itau=0
  tt=interpol(taus,ma[*,iref],r515)
  taus[itau]=tt
  tau=tt
  ref=10.
endif

;now set the tau ad ref
;tau=taus[itau]
;ref=refs[iref]
end


pro  get_uncertainty, r515,slope,sp,ma,ms,ki,taus,refs,dtau,dref

delr=r515*0.072
dels=slope*0.032
delsp=sp*0.032

akip=((r515+delr)-ma)/ma
akim=((r515-delr)-ma)/ma
skip=((slope+dels)-ms)/ms
skim=((slope-dels)-ms)/ms

kip=sqrt(akip^2.+skip^2.)
kim=sqrt(akim^2.+skim^2.)
dkdr=(kip-kim)/(2*delr)
dkds=(kip-kim)/(2*dels)
kia=(dkdr*delr)^2.

kisp=0.
for i=0, n_elements(sp)-1 do begin
  dkdsp=(kip-kim)/(2*delsp[i])
  kisp=kisp+(dkdsp*delsp[i])^2.
endfor

dk=sqrt(kia+kisp)
nul=min(ki+dk,p)
nul=min(ki-dk,m)
pi=array_indices(ki+dk,p)
mi=array_indices(ki-dk,m)
dtau=abs(taus[pi[0]]-taus[mi[0]])/2.
dref=abs(refs[pi[1]]-refs[mi[1]])/2.
;stop

end

pro  get_uncertainty_2wvl, r515,slope,sp,ma,ms,ki,taus,refs,dtau,dref

delr=r515*0.072
dels=slope*0.072


akip=((r515+delr)-ma)/ma
akim=((r515-delr)-ma)/ma
skip=((slope+dels)-ms)/ms
skim=((slope-dels)-ms)/ms

kip=sqrt(akip^2.+skip^2.)
kim=sqrt(akim^2.+skim^2.)
;dkdr=(kip-kim)/(2*delr)
;dkds=(kip-kim)/(2*dels)

;delki=(dkdr*delr)^2.+(dkds*dels)^2.

;nul=min(ki+delki,p)
;nul=min(ki-delki,m)
;pi=array_indices(ki+delki,p)
;mi=array_indices(ki-delki,m)
nul=min(kip,p)
nul=min(kim,m)
pi=array_indices(kip,p)
mi=array_indices(kim,m)

dtau=abs(taus[pi[0]]-taus[mi[0]])/2.
dref=abs(refs[pi[1]]-refs[mi[1]])/2.
;stop
end

