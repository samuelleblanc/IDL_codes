; program to read in the few ascii save files created from Matlab with 
; 4STAR data on 20130219 at around 17 UTC
; raw spectra data, with lat lon sza and others

pro read_4STAR_ascii

dir='/home/leblanc/TCAP/4STAR/'
spd=read_ascii(dir+'sp_test.txt')
xtd=read_ascii(dir+'xtra_test.txt')
wvd=read_ascii(dir+'wvl_test.txt')

sp=spd.field0001  ;1556x198, wvlxtime
utc=reform(xtd.field1[0,*])
sza=reform(xtd.field1[1,*])
lat=reform(xtd.field1[2,*])
lon=reform(xtd.field1[3,*])
alt=reform(xtd.field1[4,*])
wvl=wvd.field1  ; wavelength 1556

save, sp, utc, sza, lat, lon, alt, wvl, filename=dir+'sp_4STAR_20130219.out'

end
