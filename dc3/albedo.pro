; Program to determine the surface albedo from MODIS and average SGP site
; for Boulder
; during the month of may 2012

pro albedo

; get the data of surface albedo from sgp and 
restore, '/home/leblanc/DC3_SEAC4RS/surface_albedo/alb.out'

alb_wvl=[350.,469.,555.,645.,858.,1240.,1640.,2130.]
su_alb =[0.02,0.038955,0.0758601,0.07409,0.26779,0.29993,0.224589,0.124088]

set_plot, 'x'
loadct, 39, /silent
device,decomposed=0
plot, wvl,alb, xtitle='Wavelength (nm)', ytitle='surface albedo'
oplot, alb_wvl, su_alb, color=250, psym=2


stop
end
