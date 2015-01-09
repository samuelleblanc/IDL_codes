@legend.pro
pro calnex_ang_cal_harp
!P.multi=0

;dir      = '/data/seven/schmidt/calnex/ang/20100301/'
;ang      = [0     ,5     ,10    ,15    ,20    ,25    ,30    ,40    ,50    ,60    ,70    ,80]
;dat      = ['1845','1847','1849','1852','1854','1856','1858','1900','1902','1904','1906','1908','LC-7_plate_12pm']
;dat      = ['2049','2051','2053','2055','2057','2059','2101','2103','2105','2107','2109','2111','LC-7_plate_6pm']
;dat      = ['2023','2025','2027','2029','2031','2033','2035','2037','2039','2041','2043','2045','LC-7_plate_3pm']
;dat      = ['2237','2239','2241','2243','2245','2248','2250','2252','2254','2256','2258','2300','2302','LC-2_plate_12pm']
;dat      = ['2304','2306','2309','2311','2313','2315','2317','2319','2321','2323','2325','2327','2329','LC-2_plate_3pm']
;dat      = ['2332','2334','2336','2338','2340','2342','2344','2346','2348','2350','2352','2354','2356','LC-2_plate_6pm']

dir      = '/data/seven/schmidt/calnex/ang/20100302/'
ang      = [0     ,5     ,10    ,15    ,20    ,25    ,30    ,35    ,40    ,50    ,60    ,70    ,80]
;dat      = ['1832','1834','1836','1838','1840','1843','1845','1847','1849','1851','1853','1855','1857','LC-6_plate_12pm']
;dat      = ['1952','1954','1956','1958','2000','2002','2004','2006','2008','2010','2013','2015','2017','LC-6_plate_3pm']
;dat      = ['2019','2021','2025','2027','2029','2031','2033','2035','2037','2039','2041','2045','2047','LC-6_plate_6pm']
;dat      = ['2156','2158','2200','2202','2204','2206','2208','2210','2212','2214','2216','2218','2220','LC-5_plate_12pm']
;dat      = ['2223','2225','2227','2229','2232','2234','2236','2238','2240','2242','2244','2246','2248','LC-5_plate_3pm']
dat      = ['2250','2252','2255','2257','2259','2301','2303','2305','2307','2309','2311','2313','2315','LC-5_plate_6pm']

na       = n_elements(ang)

npixels  = 1024
nspectra = 24
channel  = 800

res      = fltarr(na,npixels)

errcnt   = 0

for j=0,na-1 do begin
  ;fn0 = dir+'100222_'+dat[j]+'*z03_7-1018_800\100222*800.bin'
  ;fnd0= dir+'100222_'+dat[j]+'*z03_7-1018_800\Dark\100222*DARK.bin'
  ;fn0 = dir+'100223_'+dat[j]+'*z03_7-1018_800\100223*800.bin'
  ;fnd0= dir+'100223_'+dat[j]+'*z03_7-1018_800\Dark\100223*DARK.bin'
  ;fn0 = dir+'100223_'+dat[j]+'*z03_7-1018_800\100223*800.bin'
  ;fnd0= dir+'100223_'+dat[j]+'*z03_7-1018_800\Dark\100223*DARK.bin'
  ;fn0 = dir+'100224_'+dat[j]+'*z03_7-1018_800\100224*800.bin'
  ;fnd0= dir+'100224_'+dat[j]+'*z03_7-1018_800\Dark\100224*DARK.bin'
  ;fn0 = dir+'100225_'+dat[j]+'*z03_7-1018_800\100225*800.bin'
  ;fnd0= dir+'100225_'+dat[j]+'*z03_7-1018_800\Dark\100225*DARK.bin'
  fn0 = dir+'100302_'+dat[j]+'*z03_7-1018_800/100302*800.bin'
  fnd0= dir+'100302_'+dat[j]+'*z03_7-1018_800/Dark/100302*DARK.bin'
  fn  = file_search(fn0,count=n0)
  print,'Process '+fn
  if n0 ne 1 then message,'Something wrong spec.'
  fnd  = file_search(fnd0,count=n0)
  if n0 ne 1 then message,'Something wrong dark.'
  ;goto,here

  ; spectra
  data=fltarr(npixels+4,nspectra)

  openr,us,fn,/get_lun
  on_ioerror,here
  readu,us,data
  free_lun,us
  data=swap_endian(data)
  data=data[4:*,*]

  spc=fltarr(npixels)
  spcs=fltarr(npixels)
  for i=0,npixels-1 do begin
    spc[i]=mean(data[i,*])
    spcs[i]=1./nspectra*stddev(data[i,*])
  endfor

  ; darks

  data=fltarr(npixels+4,nspectra)
  openr,us,fnd,/get_lun
  on_ioerror,here
  readu,us,data,err
  free_lun,us
  data=swap_endian(data)
  data=data[4:*,*]

  drk=fltarr(npixels)
  drks=fltarr(npixels)
  for i=0,npixels-1 do begin
    drk[i]=mean(data[i,*])
    drks[i]=1./nspectra*stddev(data[i,*])
  endfor

  res[j,*]=spc-drk

  goto,jump
  here:
  errcnt=errcnt+1
  jump:

endfor

device,decomposed=0
loadct,27,/silent
window,0,retain=2

throughput=max(res[0,*])
plot,res[0,*],yr=[0,max(res)],xtit='channel #',ytit='dark current corrected counts',tit=dat[na],thick=3
for j=1,na-1 do begin
  oplot,res[j,*],color=j*40
endfor
p=tvrd(true=1)
write_png,dir+'spc_'+dat[na]+'.png',p

window,1,retain=2
plot,cos(!pi/180*ang),res[*,channel[0]]/res[0,channel[0]],xtit='mu',ytit='response at channel #'+strcompress(string(channel[0]),/REMOVE_ALL),psym=2,xr=[0,1],yr=[0,1.2],tit=dat[na]
;oplot,ang,cos(!pi/180.*ang),psym=1
oplot,[0,1],[0,1],linesty=1
if n_elements(channel) gt 1 then begin
  oplot,cos(!pi/180.*ang),res[*,channel[1]]/res[0,channel[1]],psym=2,color=100
  legend,[string(channel[0]),string(channel[1])],textcolor=[255,100]
endif
p=tvrd(true=1)
write_png,dir+'ang_'+dat[na]+'.png',p


print,'err=',errcnt
throughput=fix(throughput+0.5)
print,'throughput=',throughput

label=dat[na]

save,ang,res,errcnt,throughput,label,channel,file=dir+'res_'+label+'.out'

stop

end
