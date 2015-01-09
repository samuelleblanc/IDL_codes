@cfg.pro
pro get_cos,dir,date,cfg,utc,alt,sza,dc,wlz,facz,dz,wln,facn,dn

; (2) Get cosine response zenith
cf=cfg(cfg,'coszen')
cosfile=file_search(cf,count=nc)
if nc lt 1 then message,'No cosine response file found.'
if nc gt 1 then message,'More than one cosine response file found.'

cc=indgen(1001)
print,'Open: ',cosfile[0]
restore,cosfile
dcl=mu_mean*1000
ccl=azi_mean
na=n_elements(mu_mean)
nl=n_elements(wl)

print,'CosZenith: Warning - at this point, cosine response at 500 nm only!'

mm=min(abs(wl-500.),i500)
ccl=azi_mean[*,i500]

fcz=interpol(ccl,dcl,cc)
dz=500./int_tabulated(dcl,ccl) ; diffuse correction factor for zenith

; (3) Get cosine response nadir
cf=cfg(cfg,'cosnad')
if strcmp(strmid(cf,0,1),'=') then begin
  dn=float(strmid(cf,1))
endif else begin
cosfile=file_search(cf,count=nc)
if nc lt 1 then message,'No cosine response file found.'
if nc gt 1 then message,'More than one cosine response file found.'
cc=indgen(1001)
print,'Open: ',cosfile[0]
restore,cosfile
dcl=mu_mean*1000

print,'CosNadir: Warning - at this point, cosine response at 500 nm only!'

mm=min(abs(wl-500.),i500)
ccl=azi_mean[*,i500]

ccl=azi_mean[*,i500]

fcn=interpol(ccl,dcl,cc)
dn=500./int_tabulated(dcl,ccl) ; diffuse correction factor for zenith
endelse

; (4) Get correction factors

cf=fix(dc*1000+0.5)
i0=where(cf gt 1000,in) & if in ge 1 then cf[i0]=1000
i0=where(cf lt 0,in)   & if in ge 1 then cf[i0]=0

facz=fcz[cf]
if n_elements(fcn) gt 0 then facn=fcn[cf] else facn=1.

ind=where(facz lt 0.01,n0)
if n0 gt 0 then begin
  facz[ind]=1.
endif
ind=where(facn lt 0.01,n0)
if n0 gt 0 then begin
  facn[ind]=1.
endif

mu=cos(sza*!pi/180.)

mz=mu/facz
ind=where(mz gt 0.85 and mz lt 1.1)
facz=facz*0.
facz[ind]=mz[ind]
;facz=mu/facz ; if cosine response 1:1, then fac=1 everywhere
facn=mu/facn
return
end
