; program to plot the results from the model runs on radiance spectra from mixed phase clouds

pro plot_model_mix
dir='/argus/roof/SSFR3/model/'

restore, dir+'sp_mix2_v2.out'
sp_nrml=sp
restore, dir+'sp_mix2_lay_v2.out'
sp_lay=sp
restore, dir+'sp_mix2_lvls_v2.out'
sp_lvls=sp

set_plot, 'x'
loadct, 39
device, decomposed=0

window,0,retain=2
window,1,retain=2
window,2,retain=2


lbls=['normal','layers','levels']

for r=0, 2 do begin

for t=0, 5 do begin

for j=0,2 do begin
wset,j
case j of
  0: sp=sp_nrml
  1: sp=sp_lay
  2: sp=sp_lvls
endcase

plot, zenlambda,sp[t,1,r,*,0],title=lbls[j]+'tau:'+string(tau[t])+' refi:'+string(refi[r])
for i=1,4 do oplot, zenlambda, sp[t,1,r,*,i],color=i*60



endfor

cursor,x,y
endfor
endfor



stop
end
