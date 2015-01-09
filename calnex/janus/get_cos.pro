;program for getting the cosine correction factor for the entire spectrum wavelengths 

@cfg.pro
pro get_cos,dir,date,cfg,utc,alt,sza,dc,wlz,facz,dz,wln,facn,dn

; (2) Get cosine response zenith
cfp=cfg(cfg,'coszen')
cosfile=file_search(cfp,count=nc)
if nc lt 1 then message,'No cosine response file found.'
if nc gt 1 then message,'More than one cosine response file found.'

cc=indgen(1001)
print,'Open: ',cosfile[0]
restore,cosfile
dcl=mu_interpol*1000
na=n_elements(mu_interpol)
nl=n_elements(wvl)

facz=dblarr(n_elements(sza),n_elements(wlz))
faczk=dblarr(n_elements(sza),n_elements(wlz))
dz=dblarr(n_elements(wlz))

mu=cos(sza*!pi/180.)
cf=fix(dc*1000+0.5)
i0=where(cf gt 1000,in) & if in ge 1 then cf[i0]=1000
i0=where(cf lt 0,in)   & if in ge 1 then cf[i0]=0
for i=0L, n_elements(wlz)-1L do begin
  mm=min(abs(wvl-wlz[i]),in)
  ccl=res_wvl[*,in]
  
  fcz=interpol(ccl,dcl,cc)
  dz[i]=wlz[i]/int_tabulated(dcl,ccl) ; diffuse correction factor for zenith
  
  faczk[*,i]=fcz[cf]
  ind=where(faczk[*,i] lt 0.01,n0)
  if n0 gt 0 then begin
    faczk[ind,i]=1.
  endif
  facz[*,i]=mu[*]/faczk[*,i] ; if cosine response 1:1, then fac=1 everywhere
endfor

; (3) Get cosine response nadir
cfp=cfg(cfg,'cosnad')
cosfile=file_search(cfp,count=nc)
if nc lt 1 then message,'No cosine response file found.'
if nc gt 1 then message,'More than one cosine response file found.'

cc=indgen(1001)
print,'Open: ',cosfile[0]
restore,cosfile
dcl=mu_interpol*1000

facn=dblarr(n_elements(sza),n_elements(wln))
facnk=dblarr(n_elements(sza),n_elements(wln))
dn=dblarr(n_elements(wln))

for i=0L, n_elements(wln)-1L do begin

  mm=min(abs(wvl-wln[i]),in)
  ccl=res_wvl[*,in]

  fcn=interpol(ccl,dcl,cc)
  dn[i]=wln[i]/int_tabulated(dcl,ccl) ; diffuse correction factor for zenith

  ; (4) Get correction factors

  facnk[*,i]=fcn[cf]

  ind=where(facnk[*,i] lt 0.01,n0)
  if n0 gt 0 then begin
    facnk[ind,i]=1.
  endif
  facn[*,i]=reform(mu[*]/facnk[*,i])
endfor

return
end
