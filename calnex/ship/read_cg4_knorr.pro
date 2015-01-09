pro read_cg4_knorr,pattern,serial,cali,utc,zenv,zent

files=file_search(pattern,count=nf)
; calibration
a=cali[0] & b=cali[1] & c=cali[2]

bn=90000L

tc=1e-6 ; temperature conversion - replace for later processing stage

secs=lonarr(bn)
zenv=fltarr(bn)
zent=fltarr(bn)
zenr=fltarr(bn)

i=0L
for f=0,nf-1 do begin
openu,uu,files[f],/get_lun
print,'Open:',files[f]
while not eof(uu) do begin
  ; data structure of CG4
  block = {cnt: lonarr(1), x1:bytarr(11),$
           status1  : bytarr(1), serial1: intarr(1),x2:bytarr(4),version  : bytarr(1),gain1: bytarr(1),$
           voltage1 : bytarr(4),temperature1:bytarr(4),reartemp1: bytarr(4),systemp1: bytarr(4),caltemp1: bytarr(4),x3: bytarr(3),$
           status2  : bytarr(1), serial2: intarr(1),x4:bytarr(4),versio2  : bytarr(1),gain2: bytarr(1),$
           voltage2 : bytarr(4),temperature2:bytarr(4),reartemp2: bytarr(4),systemp2: bytarr(4),caltemp2: bytarr(4)}
  reread:
  readu,uu,block
  if fix(block.serial1) ne fix(serial) then begin
    if eof(uu) then goto,next else goto,reread
  endif

  secs[i]=block.cnt mod 86400 ; count from beginning of the day
  ;stop
  zenv[i]=(ishft(long(block.voltage1[2]),16)+ishft(long(block.voltage1[1]),8)+long(block.voltage1[0])-'800000'XL)*1.25/float('800000'XL)/float(block.gain1) 
  zent[i]=(ishft(long(block.temperature1[2]),16)+ishft(long(block.temperature1[1]),8)+ishft(long(block.temperature1[0]),0)-'800000'XL)*1.25/float('800000'XL)/float(block.gain1)

  ; calibration
  zenv[i]=1e6/a*zenv[i] ; divide by uV/(W/m2) to get from uV to W/m2
  zent[i]=b+c*1e3*zent[i]

  i+=1L
  next:
endwhile
free_lun,uu
endfor

if(i gt 0) then begin
    secs=secs[0:i-1]
    zenv=zenv[0:i-1]
    zent=zent[0:i-1]
endif

dim=i
utc=secs/3600.
;stop
end
