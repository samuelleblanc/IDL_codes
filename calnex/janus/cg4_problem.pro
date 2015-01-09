pro cg4_problem

; cross calibration between 20592 and 20594 on Duane rooftop to transfer NREL calibration


bn=5000000L


L1:
dir    = '/data/seven/schmidt/calnex/p3/20100420/' ; port0=20592, port1=20594
dayc   = 0
f0     = 100

serial = 20592
a=-264.8801 & b=25.402884 & c=-267.04538 & d=25.67703 & e=-270.71132 & f=26.093703

; 20592: P-3 zenith a=-264.8801 b=25.402884
; 20618: P-3 nadir
; 20591: Spare a=-269.42511 b=26.553525
; 20594: Knorr a=-263.69862 b=26.078666

ctr = 0

pattern = dir+'*.CG4'
files   = file_search(pattern,count=nf)

secs = lonarr(bn)
utc  = fltarr(bn)
zenv = dblarr(bn)
zent = dblarr(bn)
zenc = dblarr(bn)
zens = dblarr(bn)
date = strarr(bn)
dax  = intarr(bn)

ctr=0L
for ff=f0,nf-1 do begin
    openu,uu,files[ff],/get_lun
    print,'Open:',files[ff]
    while not eof(uu) do begin
        block = {time1: lonarr(2), cnt:lonarr(1),$
                 status1:bytarr(1), pad1:bytarr(3), serial1:lonarr(1),sys_serial1:intarr(1),version1:bytarr(1),gain1:bytarr(1),$
                 voltage1:lonarr(1),temperature1:lonarr(1),reartemp1:lonarr(1),systemp1:lonarr(1),caltemp1:lonarr(1),$
                 status2:bytarr(1), pad2:bytarr(3), serial2:lonarr(1),sys_serial2:intarr(1),version2:bytarr(1),gain2:bytarr(1),$
                 voltage2:lonarr(1),temperature2:lonarr(1),reartemp2:lonarr(1),systemp2:lonarr(1),caltemp2:lonarr(1)}
        reread:
        readu,uu,block
        if fix(block.serial1) ne fix(serial) then begin
            message,'Wrong serial number: '+string(block.serial1)
            if eof(uu) then goto,next else goto,reread
        endif

        if(block.status1 ne 0) then begin
            ;;stop
            if(block.status1 eq 1) then begin
                if(block.voltage1 eq 'AAAAAA'XL) then begin
                    ;;stop
                    goto,next
                endif else begin
                    if(block.voltage1 eq '555555'XL) then begin
                        ;;stop
                        goto,next
                    endif else begin
                        stop
                    endelse
                endelse
            endif
        endif


        atime  = systime(0, block.time1[0], /utc) ; convert date
        result = strpos(atime[0], ':', 0) ; find first incidence of ':'
        hh = strmid(atime[0], result - 2, 2) & mm = strmid(atime[0], result + 1, 2) & ss = strmid(atime[0], result + 4, 2)
        utc [ctr] = double(hh + (mm/60.) + (ss/3600.)) ; put hr in decimal form

        pos    = strpos(atime,' ')+1
        mid    = strmid(atime,pos)
        pos    = strpos(mid,' ')+1
        month  = strmid(mid,0,pos-1)
        mid    = strmid(mid,pos)
        pos    = strpos(mid,' ')+1
        day    = strmid(mid,0,pos-1)
        mid    = strmid(mid,pos)
        pos    = strpos(mid,' ')+1
        year   = strmid(mid,pos)

        date[ctr]=month+'-'+day+'-'+year
        secs[ctr]=block.cnt ; mod 86400 ; count from beginning of the day
        if ctr ge 1 then begin
          ;if secs[ctr] lt secs[ctr-1] then begin
          if utc[ctr] lt utc[ctr-1] then begin
            dayc=dayc+1
          endif
        endif
        dax [ctr]=dayc
        zenv[ctr]=(block.voltage1 - '800000'XL)*1.25/float('800000'XL)/float(block.gain1)*1e6
        zent[ctr]=1e3*((block.temperature1 - '800000'XL)*1.25/float('800000'XL))/float(block.gain1)
        zenc[ctr]=1e3*((block.reartemp1 - '800000'XL)*1.25/float('800000'XL))/float(block.gain1)
        zens[ctr]=1e3*((block.systemp1  - '800000'XL)*1.25/float('800000'XL))/float(block.gain1)

        ctr+=1L
        next:
    endwhile
  free_lun,uu
endfor
;;stop

secs=secs[0:ctr-1]
dax =dax [0:ctr-1]
utc =utc [0:ctr-1]
date=date[0:ctr-1]
zenv=zenv[0:ctr-1]
zent=zent[0:ctr-1]
zent=a+b*zent
zenc=c+d*zenc[0:ctr-1]
zens=e+f*zens[0:ctr-1]

loadct,27,/silent
device,decomposed=0

window,1,xs=800,ys=600
!P.multi=[0,1,2]
plot,dax*24.+utc,zenv,psym=3, xtit='UTC [h]',ytit='U [uV]', tit=serial
plot,dax*24.+utc,zent,psym=3, xtit='UTC [h]',ytit='T [C]'
oplot,dax*24.+utc,zenc,psym=3,color=70



u1   =utc
d1   =dax
zenv1=zenv
zent1=zent













L2:
dir    = '/data/seven/schmidt/calnex/p3/20100420/'
dayc   = 0

serial = 20618
a=-263.69862& b=26.078666 & c=-267.27036 & d=26.426697 & e=-271.60795 & f=26.96619

ctr = 0

pattern = dir+'*.CG4'
files   = file_search(pattern,count=nf)

secs = lonarr(bn)
utc  = fltarr(bn)
zenv = dblarr(bn)
zent = dblarr(bn)
zenc = dblarr(bn)
zens = dblarr(bn)
date = strarr(bn)
dax  = intarr(bn)

ctr=0L
for ff=f0,nf-1 do begin
    openu,uu,files[ff],/get_lun
    print,'Open:',files[ff]
    while not eof(uu) do begin
        block = {time1: lonarr(2), cnt:lonarr(1),$
                 status1:bytarr(1), pad1:bytarr(3), serial1:lonarr(1),sys_serial1:intarr(1),version1:bytarr(1),gain1:bytarr(1),$
                 voltage1:lonarr(1),temperature1:lonarr(1),reartemp1:lonarr(1),systemp1:lonarr(1),caltemp1:lonarr(1),$
                 status2:bytarr(1), pad2:bytarr(3), serial2:lonarr(1),sys_serial2:intarr(1),version2:bytarr(1),gain2:bytarr(1),$
                 voltage2:lonarr(1),temperature2:lonarr(1),reartemp2:lonarr(1),systemp2:lonarr(1),caltemp2:lonarr(1)}
        reread1:
        readu,uu,block
        if fix(block.serial2) ne fix(serial) then begin
            message,'Wrong serial number: '+string(block.serial2)
            if eof(uu) then goto,next else goto,reread1
        endif

        if(block.status2 ne 0) then begin
            ;;stop
            if(block.status2 eq 1) then begin
                if(block.voltage1 eq 'AAAAAA'XL) then begin
                    ;;stop
                    goto,next1
                endif else begin
                    if(block.voltage2 eq '555555'XL) then begin
                        ;;stop
                        goto,next1
                    endif else begin
                        stop
                    endelse
                endelse
            endif
        endif


        atime  = systime(0, block.time1[0], /utc) ; convert date
        result = strpos(atime[0], ':', 0) ; find first incidence of ':'
        hh = strmid(atime[0], result - 2, 2) & mm = strmid(atime[0], result + 1, 2) & ss = strmid(atime[0], result + 4, 2)
        utc [ctr] = double(hh + (mm/60.) + (ss/3600.)) ; put hr in decimal form

        pos    = strpos(atime,' ')+1
        mid    = strmid(atime,pos)
        pos    = strpos(mid,' ')+1
        month  = strmid(mid,0,pos-1)
        mid    = strmid(mid,pos)
        pos    = strpos(mid,' ')+1
        day    = strmid(mid,0,pos-1)
        mid    = strmid(mid,pos)
        pos    = strpos(mid,' ')+1
        year   = strmid(mid,pos)

        date[ctr]=month+'-'+day+'-'+year
        secs[ctr]=block.cnt ; mod 86400 ; count from beginning of the day
        if ctr ge 1 then begin
          ;if secs[ctr] lt secs[ctr-1] then begin
          if utc[ctr] lt utc[ctr-1] then begin
            dayc=dayc+1
          endif
        endif
        dax [ctr]=dayc
        zenv[ctr]=(block.voltage2 - '800000'XL)*1.25/float('800000'XL)/float(block.gain2)*1e6
        zent[ctr]=1e3*((block.temperature2 - '800000'XL)*1.25/float('800000'XL))/float(block.gain2)
        zenc[ctr]=1e3*((block.reartemp2 - '800000'XL)*1.25/float('800000'XL))/float(block.gain2)
        zens[ctr]=1e3*((block.systemp2  - '800000'XL)*1.25/float('800000'XL))/float(block.gain2)

        ctr+=1L
        next1:
    endwhile
  free_lun,uu
endfor
;stop

secs=secs[0:ctr-1]
dax =dax [0:ctr-1]
utc =utc [0:ctr-1]
date=date[0:ctr-1]
zenv=zenv[0:ctr-1]
zent=zent[0:ctr-1]
zent=a+b*zent
zenc=c+d*zenc[0:ctr-1]
zens=e+f*zens[0:ctr-1]

loadct,27,/silent
device,decomposed=0

window,2,xs=800,ys=600
!P.multi=[0,1,2]
plot,dax*24.+utc,zenv,yr=[-2000,500],psym=3, xtit='UTC [h]',ytit='U [uV]', tit=serial
plot,dax*24.+utc,zent,psym=3, xtit='UTC [h]',ytit='T [C]'
oplot,dax*24.+utc,zenc,psym=3,color=70

u2   =utc
d2   =dax
zenv2=zenv
zent2=zent


stop

end
