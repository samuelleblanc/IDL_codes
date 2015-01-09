pro ini_dark,spc_files,np,darkmin,dark,darku,darki,darkt,aa,bb,zz,utd0,utd1,date
; Read in first dark cycle; if no dark file is found, it is produced by running through all the spectra first.

months = ["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"]
stop_checking = 0

if dark eq 'I' then darkfile=darki else darkfile=darkt
dummy = file_search(darkfile, count = nd, /FOLD_CASE) ; check if relevant dark file exists

if nd ne 1 then begin           ; obtain darks and make dark files when necessary
;if 1 then begin ; always calculate darks

; The program for obtaining the dark currents is similar to the main program. Whenever the shutter
; is closed (this is called dark cycle), the dark spectra are measured, and averaged.

    bignum=120000L       ; max of seconds (spectra), increase if needed
    darkmax=400                ; max of dark cycles, increase if needed
    tmhrs=fltarr(bignum)        ; time in hours
    ind0=lonarr(darkmax)        ; start index of dark cycles
    ind1=lonarr(darkmax)        ; end index of dark cycles
    nf=n_elements(spc_files)

    zsi_hld=fltarr(bignum,np)   ; Si     spectra
    zir_hld=fltarr(bignum,np)   ; InGaAs spectra
    nsi_hld=fltarr(bignum,np)   ; Si     spectra
    nir_hld=fltarr(bignum,np)   ; InGaAs spectra
    bx_hld  =fltarr(bignum)     ; Box    temperature

; further initialization
    slast=0 & darkc=-1 & start=1

; read in dark spectra from all data files
    ctr = 0L                    ; initialize spectrum counter
    for ind = 0, nf - 1 do begin ; go through all raw data files
        openr, lun, spc_files[ind], /get_lun
        while not eof(lun) do begin

     spect = {btime: lonarr(2), bcdtimstp:  bytarr(12), intime1: long(0), $
            intime2: long(0), intime3: long(0), intime4: long(0), $
            accum: long(0), ztemp: ulong(0), ntemp: ulong(0), itemp: ulong(0), $
            zspecsi: intarr(np), zspecir: intarr(np), nspecsi: intarr(np), nspecir: intarr(np)}


            readu, lun, spect   ; read one spectrum
            atime  = systime(0, spect.btime(0), /utc) ; convert date
            result = strpos(atime(0), ':', 0) ; find first incidence of ':'

            day1 =  strmid(atime(0), result - 5, 1)
            day2 =  strmid(atime(0), result - 4, 1)
            if(day1 eq ' ') then begin
                day = '0' + day2
            endif else begin
                day = day1 + day2
            endelse
            mon =  strmid(atime(0), result - 9, 3)
            mon = strtrim(string(where(months eq mon) + 1),1)
            if(mon lt 10) then begin
                mon = '0' + string(mon[0])
            endif else begin
                mon = string(mon[0])
            endelse
            year = fix(strmid(atime(0), result + 7, 4)) ;get year

            mydate = strtrim(string(year,mon,day),1)
            if fix(strmid(date,6,2))+1 eq fix(strmid(mydate,6,2)) then begin
              plus=24.
            endif else begin
              plus=0.
              if((mydate ne date) and (not stop_checking)) then begin
                print, "Skipping ",spc_files(ind),date,' ',mydate
                continue
              endif else begin
                stop_checking = 1
              endelse
            endelse


            hh = strmid(atime(0), result - 2, 2) & mm = strmid(atime(0), result + 1, 2) & ss = strmid(atime(0), result + 4, 2)
            tmhrs[ctr] = double(hh + (mm/60.) + (ss/3600.)) + plus ;put hr in decimal form

            if spect.ztemp eq 0 then continue ;if the line isn't full
                                ; check whether integration times changed and give a warning: the program is not made to handle these!
            if (not start) then begin
                if (t1o ne spect.intime1 or t2o ne spect.intime2) then begin
                    print,'Change in integration time at',tmhrs[ctr],':',t1o,t2o,'->',spect.intime1,spect.intime2
                    message,'Program is not made to handle these!'
                endif
            endif
            start=0 & t1o=spect.intime1 & t2o=spect.intime2

        mmmp=double(mm)+double(ss+10.0)/60.
        mmml=double(mm)+double(ss-10.0)/60.
		sh=0
		if (mmml ge 0.0 and mmmp le 1.0) then sh=1
		if (mmml ge 15.0 and mmmp le 16.0) then sh=1
		if (mmml ge 30.0 and mmmp le 31.0) then sh=1
		if (mmml ge 45.0 and mmmp le 46.0) then sh=1

            ; now check that the levels are low
            if sh then begin
              if spect.zspecsi[45] gt 410 then sh=0
              if spect.nspecsi[45] gt 460 then sh=0            
              if spect.zspecir[45] gt -1000 then sh=0
              if spect.nspecir[45] gt -800 then sh=0
            endif	

		
            if sh then begin ; closed shutter
                if slast eq 0 then begin ; shutter has been open before
                    darkc=darkc+1 ; --> initiate dark cycle:
                    ind0[darkc]=ctr ;     dark cycle start index
                    print,'Dark cycle number=',darkc, '. Starting time=',tmhrs[ctr]
                endif

                                ; spectra
                zsi_hld[ctr,*]=spect.zspecsi ; Zenith si spec
                zir_hld[ctr,*]=spect.zspecir ; Zenith ir spec

                nsi_hld[ctr,*]=spect.nspecsi ; Zenith si spec
                nir_hld[ctr,*]=spect.nspecir ; Zenith ir spec
                ;if(slast eq 0) then begin
                ;    myctr=1
                ;    plot,nir_hld(ctr,*)
                ;endif else begin
                ;    oplot,nir_hld(ctr,*)
                ;    myctr+=1
                ;    print, "Plotting ",myctr
                ;endelse
                ;plot,nir_hld(ctr,*),yrange=[-2000,4000]
                ;wait,.007
                                ; temperature(s)
                it = (spect.itemp*(10./4096.)) ; Box temperature
                                ; convert voltages to temperatures using (hard-wired) thermistor calibration
                rtem          = abs((2000. * it)/(1. - (0.2 * it))) ; Box temperature
                bx_hld[ctr]   = 1./(1.0295e-3+(2.391e-4 * alog(rtem)) + (1.568e-7*(alog(rtem))^3))-273.

                ind1[darkc]=ctr ; dark cycle end index - updated until darkc++
            endif               ; shutter closed
            if(slast eq 1 and sh eq 0) then begin
                tmp = fltarr(np)
                for ii=0,np-1 do begin
                    tmp(ii) = mean(nir_hld(ind0[darkc]:ind1[darkc],ii))
                endfor
                ;oplot,tmp,color=5
                ;stop
            endif

            ctr = ctr + 1L      ; increment spectrum counter
            if ctr ge bignum then message,'Increase bignum!'
            slast=sh            ; shutter status: 0=open, 1=closed
        endwhile                ; read spectra in spc_files[ind]
        free_lun, lun           ; close spc_files[ind]
    endfor                      ; ind= 0, nf-1
    if ind1[darkc] eq 0 then ind1[darkc]=ctr ; if last dark index has not been set




;save, zsi_hld, zir_hld, nsi_hld,  nir_hld, filename='/home/leblanc/CALNEX/dark_ex.out'
;stop


; Processing of all dark that have been measured
    print,'Got',darkc,' dark cycles - now processing...'

    darknum  = max(ind1-ind0)+1 ; maximum length of dark cycles
    darktime = fltarr(darkmax,darknum) ; hold UTC times
    darklong = intarr(darkmax)  ; hold dark cycle length

    good_dark=0 ; creat index array of the darks that are long enough
    for i=0,darkc do begin      ; trim darks
        ind0[i]=ind0[i];+2       ; cut off first two spectra, not needed here
        ind1[i]=ind1[i];-2       ; cut off last  two spectra, not needed here
        darklong[i]=ind1[i]-ind0[i] ; length
        if darklong[i] ge darkmin then begin 
          darktime[i,0:darklong[i]] = tmhrs[ind0[i]:ind1[i]] 
          good_dark=[good_dark,i]
        endif
    endfor

    good_dark=good_dark[1:*]
    darkc=n_elements(good_dark)-1
    ind0=ind0[good_dark]
    ind1=ind1[good_dark]
    darklong=darklong[good_dark]

; Average over darks for each cycle, and calculate linear interpolation coefficients temperature<->darks
; for the temperature correlation method
    if darkc ge 2 then begin
        aa=fltarr(darkc,4,np) & bb=fltarr(darkc,4,np)
        y0=fltarr(darkc,4,np) & y1=fltarr(darkc,4,np)
        y2=fltarr(darkc,4,np)
        z =fltarr(darkc,4,np) ; range of darks throughout the dark period
        for i=0,darkc-1 do begin ; dark cycle loop
            ;stop
            ;if(i eq 0) then plot,nir_hld(ind0[i+1]:ind1[i+1],*) else oplot,nir_hld(ind0[i+1]:ind1[i+1],*)
          ; if darklong[i] ge darkmin and darklong[i+1] ge darkmin then begin
                for l=0,np-1 do begin ; channel loop


                    y2[i,0,l]=stddev(nir_hld [ind0[i]:ind1[i],l],/double)

                    x0=mean(bx_hld[ind0[i]:ind1[i]]) ; hold temperature first dark cycle
                    y0[i,0,l]=mean(zsi_hld [ind0[i]:ind1[i],l]) ; hold Si darks first dark cycle
                    y0[i,1,l]=mean(nsi_hld [ind0[i]:ind1[i],l]) ; hold Si darks first dark cycle
                    y0[i,2,l]=mean(zir_hld [ind0[i]:ind1[i],l]) ; hold InGaAs darks first dark cycle
                    y0[i,3,l]=mean(nir_hld [ind0[i]:ind1[i],l]) ; hold InGaAs darks first dark cycle

                    x1=mean(bx_hld[ind0[i+1]:ind1[i+1]]) ; hold temperature second dark cycle
                    y1[i,0,l]=mean(zsi_hld [ind0[i+1]:ind1[i+1],l]) ; hold Si darks second dark cycle
                    y1[i,1,l]=mean(nsi_hld [ind0[i+1]:ind1[i+1],l]) ; hold Si darks second dark cycle
                    y1[i,2,l]=mean(zir_hld [ind0[i+1]:ind1[i+1],l]) ; hold InGaAs darks second dark cycle
                    y1[i,3,l]=mean(nir_hld [ind0[i+1]:ind1[i+1],l]) ; hold InGaAs darks second dark cycle

                    z [i,0,l]=stddev([zsi_hld [ind0[i]:ind1[i],l],zsi_hld [ind0[i+1]:ind1[i+1],l]]);-min([zsi_hld [ind0[i]:ind1[i],l],zsi_hld [ind0[i+1]:ind1[i+1],l]])
                    z [i,1,l]=stddev([nsi_hld [ind0[i]:ind1[i],l],nsi_hld [ind0[i+1]:ind1[i+1],l]]);-min([nsi_hld [ind0[i]:ind1[i],l],nsi_hld [ind0[i+1]:ind1[i+1],l]])
                    z [i,2,l]=stddev([zir_hld [ind0[i]:ind1[i],l],zir_hld [ind0[i+1]:ind1[i+1],l]]);-min([zir_hld [ind0[i]:ind1[i],l],zir_hld [ind0[i+1]:ind1[i+1],l]])
                    z [i,3,l]=stddev([nir_hld [ind0[i]:ind1[i],l],nir_hld [ind0[i+1]:ind1[i+1],l]]);-min([nir_hld [ind0[i]:ind1[i],l],nir_hld [ind0[i+1]:ind1[i+1],l]])


                    ;print, where(abs(y0[i,3,l] - nir_hld[ind0[i]:ind1[i],l]) ge 3*y2[i,0,l])
                    ;stop

                    if abs(x0-x1) gt 0.2 then begin ; define temperature coefficients
                        for j=0,3 do begin ; only if temperature difference between
                            bb[i,j,l]=(y1[i,j,l]-y0[i,j,l])/(x1-x0) ; two dark cycles larger than 0.2 K
                            aa[i,j,l]=y1[i,j,l]-bb[i,j,l]*x1
                        endfor
                    endif else begin
                        for j=0,3 do begin
                            aa[i,j,l]=0.5*(y0[i,j,l]+y1[i,j,l])
                            bb[i,j,l]=0.
                        endfor
                    endelse
                endfor
                ;if(i eq 0) then plot,y2[i,0,*] else oplot,y2[i,0,*]
                ;stop
          ;  endif
        endfor
    endif
    if darkc eq 1 then begin    ; one dark cycle only available
        y0=fltarr(1,4,np) ; in this case, no temperature correlation possible
        for l=0,np-1 do begin
            y0[0,0,l]=mean(zsi_hld[ind0[0]:ind1[0],l])
            y0[0,1,l]=mean(nsi_hld[ind0[0]:ind1[0],l])
            y0[0,2,l]=mean(zir_hld[ind0[0]:ind1[0],l])
            y0[0,3,l]=mean(nir_hld[ind0[0]:ind1[0],l])
        endfor
        y1=y0
    endif
    if darkc le 0 then message,'No darks available!' ; exit program if no darks were found.

; Output darks to file - there is a file for the linear interpolation method and one for the temperature method.
    if darkc ge 2 then begin
        openw,uaa,darki,/get_lun ; normal linear interpolation file
        openw,uab,darkt,/get_lun ; temperature correlation file
        for i=0,darkc-1 do begin
            printf,uab,darktime[i,0],min(bx_hld[ind0[i]:ind1[i]]),max(bx_hld[ind0[i]:ind1[i]])
            printf,uaa,darktime[i,0],1
            for l=0,np-1 do begin
                printf,uab,form='( 8(x,f17.8))',aa[i,0,l],bb[i,0,l],aa[i,1,l],bb[i,1,l],aa[i,2,l],bb[i,2,l],aa[i,3,l],bb[i,3,l]
                printf,uaa,form='(12(x,f17.8))',y0[i,0,l],y1[i,0,l],y0[i,1,l],y1[i,1,l],y0[i,2,l],y1[i,2,l],y0[i,3,l],y1[i,3,l],z[i,0,l],z[i,1,l],z[i,2,l],z[i,3,l]
            endfor
        endfor
        printf,uab,darktime[i,0],-1,-1
        printf,uaa,darktime[i,0],-1
        free_lun,uab
        free_lun,uaa
        spawn,'chmod a+w '+darki
        spawn,'chmod a+w '+darkt
    endif

    if darkc eq 1 then begin
        openw,uaa,darki,/get_lun ; normal linear interpolation file
        printf,uaa,darktime[0,0],1
        for l=0,np-1 do begin
            printf,uaa,form='(8(x,f17.8))',y0[0,0,l],y1[0,0,l],y0[0,1,l],y1[0,1,l],y0[0,2,l],y1[0,2,l],y0[0,3,l],y1[0,3,l]
        endfor
        printf,uaa,darktime[0,0],-1
        free_lun,uaa
        spawn,'chmod a+w '+darki
    endif

endif          ; if nd ne 1 - end of processing darks and writing file

; read first dark from file
if dark eq 'T' then begin
    openr,darku,darkt,/get_lun
    readf,darku,utd0,t0dark,t1dark ; dark utc, valid temperature range (not used)
    aa=fltarr(4,np) & bb=fltarr(4,np) ; 1st index: 0,1=Si, 2,3=InGaAs, 2nd index: np channels
    for i=0,np-1 do begin
        readf,darku,a0,b0,a1,b1,a2,b2,a3,b3
        aa[0,i]=a0 & aa[1,i]=a1 & aa[2,i]=a2 & aa[3,i]=a3
        bb[0,i]=b0 & bb[1,i]=b1 & bb[2,i]=b2 & bb[3,i]=b3
    endfor
    readf,darku,utd1,t0next,t1next ; next dark time, valid temperature range (not used)
    zz=0.
endif else begin
    openr,darku,darki,/get_lun
    readf,darku,utd0,flag       ; dark utc
    if flag ne 1 then message,'Dark format wrong.'
    aa=fltarr(4,np) & bb=fltarr(4,np) ; 1st index: 0=Si, 1=InGaAs, 2nd index: np channels
    zz=fltarr(4,np) ; stddev of darks from period i-->i+1
    for i=0,np-1 do begin
        readf,darku,a0,b0,a1,b1,a2,b2,a3,b3,z1,z2,z3,z4
        aa[0,i]=a0 & aa[1,i]=a1 & aa[2,i]=a2 & aa[3,i]=a3
        bb[0,i]=b0 & bb[1,i]=b1 & bb[2,i]=b2 & bb[3,i]=b3
        zz[0,i]=z1 & zz[1,i]=z2 & zz[2,i]=z3 & zz[3,i]=z4
                                ;readf,darku,a0,b0,a1,b1
                                ;aa[0,i]=a0 & aa[1,i]=a1
                                ;bb[0,i]=b0 & bb[1,i]=b1
    endfor
    readf,darku,utd1,flag       ; next dark time
    if flag eq -1 then begin    ; no more dark files!
        free_lun,darku
        utd1=100.
    endif
endelse
; transfer to main program: aa,bb,utd0,utd1
end


pro read_drk,np,darkmin,dark,darku,aa,bb,zz,utd0,utd1
; read dark from file
utd0=utd1
if dark eq 'T' then begin
    for i=0,np-1 do begin
        readf,darku,a0,b0,a1,b1,a2,b2,a3,b3
        aa[0,i]=a0 & aa[1,i]=a1 & aa[2,i]=a2 & aa[3,i]=a3
        bb[0,i]=b0 & bb[1,i]=b1 & bb[2,i]=b2 & bb[3,i]=b3
                                ;readf,darku,a0,b0,a1,b1
                                ;aa[0,i]=a0 & aa[1,i]=a1
                                ;bb[0,i]=b0 & bb[1,i]=b1
    endfor
    readf,darku,utd1,t0next,t1next ; next dark time, valid temperature range
    if t0next eq -1 and t1next eq -1 then begin
        free_lun,darku
        utd1=100.
    endif
    zz=0.
endif else begin
    for i=0,np-1 do begin
        readf,darku,a0,b0,a1,b1,a2,b2,a3,b3,z1,z2,z3,z4
        aa[0,i]=a0 & aa[1,i]=a1 & aa[2,i]=a2 & aa[3,i]=a3
        bb[0,i]=b0 & bb[1,i]=b1 & bb[2,i]=b2 & bb[3,i]=b3
        zz[0,i]=z1 & zz[1,i]=z2 & zz[2,i]=z3 & zz[3,i]=z4
                                ;readf,darku,a0,b0,a1,b1
                                ;aa[0,i]=a0 & aa[1,i]=a1
                                ;bb[0,i]=b0 & bb[1,i]=b1
    endfor
    readf,darku,utd1,flag       ; next dark time
    if flag eq -1 then begin    ; no more dark files!
        free_lun,darku
        utd1=100.
    endif
endelse
; transfer to main program: aa,bb,utd0,utd1
end
