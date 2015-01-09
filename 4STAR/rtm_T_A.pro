; program to retrieve optical optical from the over fly  and under fly

pro rtm_T_A

dw='C:\Users\Samuel\Research\SEAC4RS\'

tau=findgen(30)+1.
g=findgen(10)/50.+0.7
ref=findgen(50)+1.

mu=0.85


; get single scattering albedo
ice=read_ascii(dw+'data\CRYSTALS_H2O-ice_Warren.csv',delimiter=',',data_start=2)
kappa=ice.field1[2,*]*4.*!PI/ice.field1[0,*]*1000000.
kappa=kappa[80:250]/1000./1000.

w=fltarr(n_elements(kappa),n_elements(ref))
for r=0, n_elements(ref)-1 do w[*,r]=1.-kappa*ref[r]/2.

Rr16=fltarr(50,n_elements(g),n_elements(tau))
Tr16=fltarr(50,n_elements(g),n_elements(tau))

Rr165=Rr16
Tr165=Tr16

nul=min(abs(ice.field1[0,80:250]-1.6),v16)
nul=min(abs(ice.field1[0,80:250]-1.65),v165)
stop
for r=0,n_elements(ref)-1 do begin
  for t=0,n_elements(tau)-1 do begin
    for j=0, n_elements(g)-1 do begin
      Rr16[r,j,t]=calc_r(g[j],w[v16,r],tau[t],mu)
      Rr165[r,j,t]=calc_r(g[j],w[v165,r],tau[t],mu)
      Tr16[r,j,t]=calc_t(g[j],w[v16,r],tau[t],mu)
      Tr165[r,j,t]=calc_t(g[j],w[v165,r],tau[t],mu)
    endfor
  endfor
endfor


r16=0.153
t16=0.223
r165=0.171
t165=0.255

ki=(Rr16-r16)^2.+(Rr165-r165)^2.+(Tr16-t16)^2.+(Tr165-t165)^2.

z=min(ki,m)
n=array_indices(ki,m)
print, 'tau:',tau[n[2]]
print, 'ref:',ref[n[0]]
print, 'g:',g[n[1]]


stop
end


;Coakley et al., 1975
function calc_t,g,w,t,m
b=(1.-g)/2. 
u=sqrt(1.-w+2.*w*b)/sqrt(1.-w) 
a=sqrt(1.-w)*sqrt(1-w+2.*w*b) 
 
x=4*u/ $ 
  ((U+1)*(U+1)*exp(a*t/m)-(U-1)*(U-1)*exp(-a*t/m)) 
return,x
end

function calc_r,g,w,t,m
b=(1.-g)/2. 
u=sqrt(1.-w+2.*w*b)/sqrt(1.-w) 
a=sqrt(1.-w)*sqrt(1-w+2.*w*b) 
 
r=((u+1)*(u-1)*(exp(a*t/m)-exp(-a*t/m)))/ $ 
  ((U+1)*(U+1)*exp(a*t/m)-(U-1)*(U-1)*exp(-a*t/m)) 
return,r
end
