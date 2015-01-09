; program to read in the ceilometer data files
; then interpolate the lowest cloud base to the times inputed

@read_skywatch.pro
pro get_ceil, tmhrs, date, base, valid=valid

dir='/home/leblanc/SSFR3/ceil/'

fn=dir+'ceil_'+strmid(date, 2,2)+'_'+strmid(date, 4,2)+'_'+strmid(date,6,2)+'.dat'

data=read_skywatch(name=fn,instrument='ceil')

n=where(data.cloud_base1 eq 0.)
data.cloud_base1[n]=7500.

va=where(data.cloud_base2 eq 0.)
valid=fltarr(n_elements(tmhrs))


base=interpol(data.cloud_base1,data.sec_of_day/3600.,tmhrs)

for i=0,n_elements(tmhrs)-1 do begin
  u=min(abs(data.sec_of_day/3600.-tmhrs[i]),in)
  nul=where(va eq in)
  if nul ne -1 then valid[i]=1. else valid[i]=0.
  ;stop
endfor

if 0 then begin
set_plot,'x'
device, decomposed=0
loadct, 39

n=where(data.sec_of_day/3600. gt min(tmhrs) and data.sec_of_day/3600. lt max(tmhrs))
window, 0, retain=2

contour, transpose(data.reflectivity[*,n]),data.sec_of_day[n]/3600.,findgen(770)*10.,/cell_fill
oplot, data.sec_of_day[n]/3600., data.cloud_base1[n],psym=2
oplot, data.sec_of_day[n]/3600., data.cloud_base2[n],psym=4
oplot, data.sec_of_day[n]/3600., data.cloud_base3[n],psym=5

window, 1, retain=2
for j=0, n_elements(n)-1 do begin
i=n[j]
  plot, smooth(data.reflectivity[*,i],10),findgen(770)*10., title=strtrim(data.sec_of_day[i]/3600.,2),xrange=[-6,6]
  oplot, [-6.,6.], data.cloud_base1[[i,i]], color=70
  oplot, [-6.,6.], data.cloud_base2[[i,i]], color=130
  oplot, [-6.,6.], data.cloud_base3[[i,i]], color=250
  wait, 0.02
endfor


stop
endif
end
