; program to test is the spectral parameters are gaussian

@get_params4.pro

pro test_gauss,p,t,r,w
if n_elements(p) lt 1 then p=1
dir='C:\Users\Samuel\Research\SSFR3\'
restore, dir+'model\v1\sp_v1_20120525.out'
print, 'restored'

if n_elements(t) lt 1 then begin
  t=10 & r=7 & w=0
endif
spp=reform(sp[t,r,*,w])  ;ice tau=10, ref=20
wv=zenlambda
get_params,wv,spp,part
if p eq 9 or p eq 1 then iv=[181,182,183,184,185,186,187,188,189,190,191,192,193,194]
if p eq 11 then begin
  iv=indgen(41)+45
  ss=max(spp,ii)
  iv=[ii,iv]
endif
n=1000000
nv=n_elements(iv)
sps=fltarr(nv,n)
if p eq 1 then st=[0.0041,0.006,0.006,0.004,0.005,0.002,0.0005,0.0004,0.0005,0.001,0.002,0.005,0.01,0.015] else st=iv*0.+0.02
for i=0, nv-1 do sps[i,*]=gsim(spp[iv[i]],spp[iv[i]]*st[i],n)
par=fltarr(n)
print, 'making pars'

if p eq 1 then for j=0, n-1 do par[j]=par1(sps[*,j],wv[iv])
if p eq 9 then for j=0, n-1 do par[j]=par9(sps[*,j],wv[iv])
if p eq 11 then for j=0, n-1 do par[j]=par11(sps[1:*,j],wv[iv[1:*]],sps[0,j])
print, 'plotting'
set_plot, 'win'
!p.multi=0
loadct,39
device, decomposed=0
hh=histogram(par,nbins=200,locations=bi)

if p eq 1 then plot,bi,hh,title='curvature'
if p eq 9 then plot,bi,hh,title='gaussian second derivative'
if p eq 11 then plot, bi,hh,title='Slope in visible'
g=gaussfit(bi,hh,a,nterms=3,chisq=xx)
oplot,bi,g,color=250
oplot,[part[p-1],part[p-1]],[0,max(hh)],linestyle=2,color=90
oplot,[a[1],a[1]],[0,max(hh)],linestyle=2,color=250
oplot,[mean(par),mean(par)],[0,max(hh)],linestyle=2
;legend,['x^2='+string(xx)]

print, part[p-1],mean(par),a[1]

if p eq 9 then f=dir+'plots\test\gauss_p9.png'
if p eq 11 then f=dir+'plots\test\gauss_p11.png'
if p eq 1 then f=dir+'plots\test\gauss_p1.png'

write_png,f,tvrd(/true)

stop
end


;function to calculate parameter 1
function par1,sp,wvl
sp2=sp/sp[0]
ns=n_elements(sp2)
ln=linfit(wvl[[0,ns-1]],sp2[[0,ns-1]])
par=total(sp2-(ln[0]+ln[1]*wvl))
return,par
end


;function to calculate parameter 9
function par9, sp,wvl
dsp=smooth(deriv(wvl/1000.,sp/sp[0]),2)
ln=linfit(wvl,dsp)
par=ln[1]
return,par
end


function par11,sp,wvl,ma
ln=linfit(wvl/1000.,sp/ma)
par=ln[1]
return,par
end


;function to return a gaussian
function gsim, mu,sig,n
v=randomu(seed,n,/normal)
v=v*sig
k=v+mu
return,k
end
