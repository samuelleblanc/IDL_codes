; program to plot the signal to noise ratio of the different parameters throughout the tau, ref, and phase space


pro plot_signal_noise

dir='/argus/roof/SSFR3/model/'
restore, dir+'pars_std_v3.out'
std_mod=std

restore, '~/SSFR3/data/meas_std.out'
std_meas=std

t=fix(tau[0:21]-1)
r=fix(ref-1)

std_mod=std_mod[t,r,*,*]
std_meas=std_meas[0:21,*,*,*]

par=par_arr[0:21,*,*,*]

ratio=par/sqrt(std_meas*std_meas+std_mod*std_mod)
ratio_mod=par/std_mod
ratio_meas=par/std_meas


t_mean=fltarr(16,2)
r_mean=fltarr(16,2)
for p=0,15 do begin
  z=where(abs(ratio[*,*,0,i]) le 1.)
  if z[0] eq -1 then t_mean[i,0]=200. else begin
    q=array_indices(ratio[*,*,0],z)
    for i=0, n_elements(z)-1 do begin
stop 
 ;    if ratio[q[0,i]-1,q[1,i],0]
    endfor
  endelse


endfor


stop








stop
end
