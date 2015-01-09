;program to get the desired pressure for the specified times.
; takes in the text file from the weather observations from the roof top
; need to download the text file before hand

pro get_pres, tmhrs, date, pres

dir='/home/leblanc/SSFR3/wx/'

f=dir+strmid(date,0,6)+'/wxobs'+date+'.txt'
print, 'reading: '+f

d=read_ascii(f,data_start=3,delimiter='/')
doy=julday(d.field1[0,*],d.field1[1,*],d.field1[2,*]+2000.)-julday(01,00,2012)
h=read_ascii(f,data_start=3,delimiter=':')
min=h.field1[1,*]
p=read_ascii(f,data_start=3)
hr=p.field01[1,*]
time=findgen(n_elements(hr))*5./60.+6.

doys=time/24.+doy[0]

doyin=julday(float(strmid(date,4,2)),float(strmid(date,6,2)),float(strmid(date,0,4)))-julday(01,00,2012)+tmhrs/24.

pres=interpol(p.field01[16,*],doys,doyin)
;stop
end
