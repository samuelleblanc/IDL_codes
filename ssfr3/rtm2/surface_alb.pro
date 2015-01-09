; program to read in the surface albed from the gp avarage from odele.

pro surface_alb

dir='C:\Users\Samuel\Research\DC3\surface_albedo\sgp_avg\'

wvl=fltarr(1851)
alb=fltarr(1851)

for i=0, 1850 do begin
  openr,lun, dir+'sgp_avg_2003_05_14.'+strtrim(string(i+350),2) , /get_lun
  readf, lun, a,b
  wvl[i]=a
  alb[i]=b
  free_lun, lun
endfor

stop
end
