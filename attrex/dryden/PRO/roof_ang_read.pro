@legend.pro
pro roof_ang_read
!P.multi=0

label=strarr(2)

;dir      = '/data/seven/schmidt/petra/cal/ang_LCX/cone_short/'
;ang      = [0   ,5   ,10  ,15  ,20  ,25  ,30  ,35  ,40  ,50  ,60  ,70  ,80]
;drkx     = ['00','02','04','06','08','10','12','14','16','18','20','22','24'] 
;datx     = ['01','03','05','07','09','11','13','15','17','19','21','23','25']   
;drk      = 'spc000'+drkx+'.OSA2'   ;files for darks
;dat      = 'spc000'+datx+'.OSA2'   ;files for shutter open
;label[0] = 'LCX_cone_short_Si'
;label[1] = 'LCX_cone_short_InGaAs'


dir      = '/data/seven/schmidt/petra/cal/ang_LCX/plate_long/'
ang      = [0   ,5   ,10  ,15  ,20  ,25  ,30  ,35  ,40  ,50  ,60  ,70  ,80]
drkx     = ['00','02','04','06','08','10','12','14','16','18','20','22','24'] 
datx     = ['01','03','05','07','09','11','13','15','17','19','21','23','25']   
drk      = 'spc000'+drkx+'.OSA2'   ;files for darks
dat      = 'spc000'+datx+'.OSA2'   ;files for shutter open
label[0] = 'LCX_plate_long_Si'
label[1] = 'LCX_plate_long_InGaAs'

na       = n_elements(ang)
np       = 256         ;number of channels
channel  = 150
res      = fltarr(na,np,2)
throughput=fltarr(2)

errcnt   = 0

si       = fltarr(25,np)
ir       = fltarr(25,np)
sid      = fltarr(25,np)
ird      = fltarr(25,np)


for j=0,na-1 do begin   ;na-1

  ; spectra
  data = {btime: lonarr(2), bcdtimstp:  bytarr(12),intime1: long(0),$
          intime2: long(0), intime3: long(0), intime4: long(0),accum: long(0),$
          shsw:long(0),zsit: ulong(0),nsit: ulong(0),zirt: ulong(0),nirt: ulong(0), $
          zirx: ulong(0), nirx: ulong(0),xt: ulong(0),it: ulong(0),$
          zspecsi: intarr(np), zspecir: intarr(np), nspecsi: intarr(np),$
          nspecir: intarr(np)}
  fn=dir+dat[j]
  print,'open: ', fn
  openr,us,fn,/get_lun
  i=0
  goto,h1
  h0:
  errcnt++
  h1: 
  while not eof(us) do begin
      on_ioerror,h0
      readu,us,data
      si[i,*]=data.nspecsi
      ir[i,*]=data.nspecir
      stop
      ;print,data.shsw
      i++
  endwhile
  free_lun,us

  spc_si=fltarr(np)
  spcs_si=fltarr(np)
  spc_ir=fltarr(np)
  spcs_ir=fltarr(np)
  for k=0,np-1 do begin         ;get mean over i spectra
    spc_si[k] =mean(si[0:i-1,k])
    spcs_si[k]=1./(i-1)*stddev(si[0:i-1,k])
    spc_ir[k] =mean(ir[0:i-1,k])
    spcs_ir[k]=1./(i-1)*stddev(ir[0:i-1,k])
  endfor


  ; darks
  data={btime: lonarr(2), bcdtimstp:  bytarr(12),intime1: long(0),$
          intime2: long(0), intime3: long(0), intime4: long(0),accum: long(0),$
          shsw:long(0),zsit: ulong(0),nsit: ulong(0),zirt: ulong(0),nirt: ulong(0), $
          zirx: ulong(0), nirx: ulong(0),xt: ulong(0),it: ulong(0),$
          zspecsi: intarr(np), zspecir: intarr(np), nspecsi: intarr(np),$
          nspecir: intarr(np)}
  fnd=dir+drk[j]
  print, 'open dark: ',fnd
  openr,us,fnd,/get_lun
  
  i=0
  goto,h3
  h2:
  errcnt++
  h3:
  while not eof(us) do begin
      on_ioerror,h2
      readu,us,data
      sid[i,*]=data.nspecsi
      ird[i,*]=data.nspecir
      ;print,data.shsw
      i++
  endwhile
  free_lun,us

  drk_si=fltarr(np)
  drks_si=fltarr(np)
  drk_ir=fltarr(np)
  drks_ir=fltarr(np)
  for k=0,np-1 do begin         ;get mean over i spectra
    drk_si[k]=mean(sid[0:i-1,k])
    drks_si[k]=1./(i-1)*stddev(sid[0:i-1,k])
    drk_ir[k] =mean(ird[0:i-1,k])
    drks_ir[k]=1./(i-1)*stddev(ird[0:i-1,k])
  endfor

  res[j,*,0]=spc_si-drk_si
  res[j,*,1]=spc_ir-drk_ir
  
endfor

print,'err=',errcnt

throughput[0]=max(res[0,*,0])     ;define max throughput for angle 0 deg
throughput[1]=max(res[0,*,1])
print,'throughput_Si=',throughput[0]
print,'throughput_InGaAs=',throughput[1]


device,decomposed=0
loadct,27,/silent

window,0,retain=2
plot,res[0,*,0],yr=[0,max(res)],xtit='channel #',ytit='dark current corrected counts',tit=label[0],thick=3
for j=1,na-1 do begin
  oplot,res[j,*,0],color=j*40
endfor
p=tvrd(true=1)
write_png,dir+'spc_'+label[0]+'.png',p


window,1,retain=2
plot,res[0,*,1],yr=[0,max(res)],xtit='channel #',ytit='dark current corrected counts',tit=label[1],thick=3
for j=1,na-1 do begin
  oplot,res[j,*,1],color=j*40
endfor
p=tvrd(true=1)
write_png,dir+'spc_'+label[1]+'.png',p


window,2,retain=2
plot,cos(!pi/180*ang),res[*,channel,0]/res[0,channel,0],xtit='mu',ytit='response at channel #'+strcompress(string(channel),/REMOVE_ALL),psym=2,xr=[0,1],yr=[0,1.2],tit=label[0]
;oplot,ang,cos(!pi/180.*ang),psym=1
oplot,[0,1],[0,1],linesty=1
p=tvrd(true=1)
write_png,dir+'ang_'+label[0]+'.png',p

window,3,retain=2
plot,cos(!pi/180*ang),res[*,channel,1]/res[0,channel,1],xtit='mu',ytit='response at channel #'+strcompress(string(channel),/REMOVE_ALL),psym=2,xr=[0,1],yr=[0,1.2],tit=label[1]
;oplot,ang,cos(!pi/180.*ang),psym=1
oplot,[0,1],[0,1],linesty=1
p=tvrd(true=1)
write_png,dir+'ang_'+label[1]+'.png',p


save,ang,res,errcnt,throughput,label,channel,file=dir+'res_'+label[0]+'.out'


stop

end
