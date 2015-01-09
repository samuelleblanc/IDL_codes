pro ini_dark,spc_files,sinp,innp,darkmin,dark,darku,darki,darkt,siaa,sibb,inaa,inbb,utd0,utd1,date
; Read in first dark cycle; if no dark file is found, it is produced by running through all the spectra first.

months = ["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"]
stop_checking = 0

if dark eq 'I' then darkfile=darki else darkfile=darkt
dummy = file_search(darkfile, count = nd, /FOLD_CASE) ; check if relevant dark file exists

if nd ne 1 then begin           ; obtain darks and make dark files

; The program for obtaining the dark currents is similar to the main program. Whenever the shutter
; is closed (this is called dark cycle), the dark spectra are measured, and averaged.

    bignum=100000L       ; max of seconds (spectra), increase if needed
    darkmax=300                ; max of dark cycles, increase if needed
    tmhrs=fltarr(bignum)        ; time in hours
    ind0=lonarr(darkmax)        ; start index of dark cycles
    ind1=lonarr(darkmax)        ; end index of dark cycles
    nf=n_elements(spc_files)

    zsi_hld=fltarr(bignum,sinp)   ; Si     spectra
    zir_hld=fltarr(bignum,innp)   ; InGaAs spectra
    nsi_hld=fltarr(bignum,sinp)   ; Si     spectra
    nir_hld=fltarr(bignum,innp)   ; InGaAs spectra
    bx_hld  =fltarr(bignum)     ; Box    temperature

; further initialization
    slast=0 & darkc=0 & start=1

; read in dark spectra from all data files
    ctr = 0L                    ; initialize spectrum counter
    for ind = 0, nf - 1 do begin ; go through all raw data files
        print,"Opening ",spc_files[ind]
        openr, lun, spc_files[ind], /get_lun
        while not eof(lun) do begin
            spect = {btime: lonarr(2), bcdtimstp:  bytarr(12),intime1: long(0),$
                     intime2: long(0), intime3: long(0), intime4: long(0),$
                     accum: long(0), shsw:long(0),  ad0: ulong(0),ad1: ulong(0),$
                     ad2: ulong(0), ad3: ulong(0), ad4: ulong(0), ad5: ulong(0),$
                     ad6: ulong(0), ad7: ulong(0),$
                     zspecsi: intarr(sinp), zspecir: intarr(sinp), nspecsi: intarr(sinp),$
                     nspecir: intarr(sinp)}

            readu, lun, spect   ; read one spectrum
            spect={btime:spect.btime, zspecir:spect.zspecir[0:innp-1], nspecir:spect.nspecir[0:innp-1], bcdtimstp:spect.bcdtimstp, $
              intime1:spect.intime1,intime2:spect.intime2, intime3:spect.intime3, intime4:spect.intime4, $
              accum: spect.accum, shsw:spect.shsw, ad0: spect.ad0,ad1: spect.ad1,$
                 ad2: spect.ad2, ad3: spect.ad3, ad4:spect.ad4, ad5: spect.ad5,$
                 ad6: spect.ad6, ad7: spect.ad7,$
                 zspecsi: spect.zspecsi, nspecsi: spect.nspecsi}
            atime  = systime(0, spect.btime(0), /utc) ; convert date
            result = strpos(atime(0), ':', 0) ; find first incidence of ':'
            ;print, spect.shsw
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
            datadate = strtrim(string(year,mon,day),1)

            if((datadate ne date) and (not stop_checking)) then begin
                print, "Skipping ",spc_files(ind),date,' ',datadate
                continue
            endif else begin
                stop_checking = 1
            endelse

            hh = strmid(atime(0), result - 2, 2) & mm = strmid(atime(0), result + 1, 2) & ss = strmid(atime(0), result + 4, 2)
            tmhrs[ctr] = double(hh + (mm/60.) + (ss/3600.)) ;put hr in decimal form

                                ; check whether integration times changed and give a warning: the program is not made to handle these!
            if (not start) then begin
                if (t1o ne spect.intime1 or t2o ne spect.intime2) then begin
                    print,'Change in integration time at',tmhrs[ctr],':',t1o,t2o,'->',spect.intime1,spect.intime2
                    message,'Program is not made to handle these!'
                endif
            endif
            start=0 & t1o=spect.intime1 & t2o=spect.intime2
            
            if (spect.shsw eq 1) then begin ; closed shutter
                if slast eq 0 then begin ; shutter has been open before
                    darkc=darkc+1 ; --> initiate dark cycle:
                    ind0[darkc-1]=ctr ;     dark cycle start index
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
                ch2 = ((spect.ad2/2048.)*5.) - 5. ; Box temperature
                                ; convert voltages to temperatures using (hard-wired) thermistor calibration
                rtem          = abs((2000. * ch2)/(1. - (0.2 * ch2))) ; Box temperature
                bx_hld[ctr]   = 1./(1.0295e-3+(2.391e-4 * alog(rtem)) + (1.568e-7*(alog(rtem))^3))-273.

                ind1[darkc-1]=ctr ; dark cycle end index - updated until darkc++
            endif               ; shutter closed
            if(slast eq 1 and spect.shsw eq 0) then begin
                tmp = fltarr(innp)
                for ii=0,innp-1 do begin
                    tmp(ii) = mean(nir_hld(ind0[darkc]:ind1[darkc],ii))
                endfor
                ;oplot,tmp,color=5
                ;stop
            endif

            ctr = ctr + 1L      ; increment spectrum counter
            if ctr ge bignum then message,'Increase bignum!'
            slast=spect.shsw    ; shutter status: 0=open, 1=closed
        endwhile                ; read spectra in spc_files[ind]
        free_lun, lun           ; close spc_files[ind]
    endfor                      ; ind= 0, nf-1
    if ind1[darkc] eq 0 then ind1[darkc]=ctr ; if last dark index has not been set

; Processing of all dark that have been measured
    print,'Got',darkc,' dark cycles - now processing...'

    darknum  = max(ind1-ind0)+1 ; maximum length of dark cycles
    darktime = fltarr(darkmax,darknum) ; hold UTC times
    darklong = intarr(darkmax)  ; hold dark cycle length

    for i=0,darkc do begin      ; trim darks
        ind0[i]=ind0[i]+2       ; cut off first two spectra
        ind1[i]=ind1[i]-2       ; cut off last  two spectra
        darklong[i]=ind1[i]-ind0[i] ; length
        if darklong[i] ge darkmin then darktime[i,0:darklong[i]] = tmhrs[ind0[i]:ind1[i]]
    endfor

; Average over darks for each cycle, and calculate linear interpolation coefficients temperature<->darks
; for the temperature correlation method
    if darkc ge 2 then begin
        siaa=fltarr(darkc,2,sinp) & sibb=fltarr(darkc,2,sinp)
        inaa=fltarr(darkc,2,innp) & inbb=fltarr(darkc,2,innp)
        siy0=fltarr(darkc,2,sinp) & siy1=fltarr(darkc,2,sinp)
        iny0=fltarr(darkc,2,innp) & iny1=fltarr(darkc,2,innp)
        siy2=fltarr(darkc,2,sinp)
        iny2=fltarr(darkc,2,innp)
        for i=0,darkc-1 do begin ; dark cycle loop
            ;stop
            ;if(i eq 0) then plot,nir_hld(ind0[i+1]:ind1[i+1],*) else oplot,nir_hld(ind0[i+1]:ind1[i+1],*)
            if darklong[i] ge darkmin and darklong[i+1] ge darkmin then begin
                for l=0,sinp-1 do begin ; si channel loop

                    siy2[i,0,l]=stddev(nsi_hld [ind0[i]:ind1[i],l],/double) ;changed nir_hld to nsi_hld, may 5th - sam

                    x0=mean(bx_hld[ind0[i]:ind1[i]]) ; hold temperature first dark cycle
                    siy0[i,0,l]=mean(zsi_hld [ind0[i]:ind1[i],l]) ; hold Si darks first dark cycle
                    siy0[i,1,l]=mean(nsi_hld [ind0[i]:ind1[i],l]) ; hold Si darks first dark cycle
                    ;;y0[i,2,l]=mean(zir_hld [ind0[i]:ind1[i],l]) ; hold InGaAs darks first dark cycle
                    ;;y0[i,3,l]=mean(nir_hld [ind0[i]:ind1[i],l]) ; hold InGaAs darks first dark cycle

                    x1=mean(bx_hld[ind0[i+1]:ind1[i+1]]) ; hold temperature second dark cycle
                    siy1[i,0,l]=mean(zsi_hld [ind0[i+1]:ind1[i+1],l]) ; hold Si darks second dark cycle
                    siy1[i,1,l]=mean(nsi_hld [ind0[i+1]:ind1[i+1],l]) ; hold Si darks second dark cycle
                    ;;y1[i,2,l]=mean(zir_hld [ind0[i+1]:ind1[i+1],l]) ; hold InGaAs darks second dark cycle
                    ;;y1[i,3,l]=mean(nir_hld [ind0[i+1]:ind1[i+1],l]) ; hold InGaAs darks second dark cycle

                    ;print, where(abs(y0[i,3,l] - nir_hld[ind0[i]:ind1[i],l]) ge 3*y2[i,0,l])
                    ;stop

                    if abs(x0-x1) gt 0.2 then begin ; define temperature coefficients
                        for j=0,1 do begin ; only if temperature difference between
                            sibb[i,j,l]=(siy1[i,j,l]-siy0[i,j,l])/(x1-x0) ; two dark cycles larger than 0.2 K
                            siaa[i,j,l]=siy1[i,j,l]-sibb[i,j,l]*x1
                        endfor
                    endif else begin
                        for j=0,1 do begin
                            siaa[i,j,l]=0.5*(siy0[i,j,l]+siy1[i,j,l])
                            sibb[i,j,l]=0.
                        endfor
                    endelse
                endfor

                for l=0,innp-1 do begin ; ingaas channel loop


                    iny2[i,0,l]=stddev(nir_hld [ind0[i]:ind1[i],l],/double)

                    x0=mean(bx_hld[ind0[i]:ind1[i]]) ; hold temperature first dark cycle
                    ;;y0[i,0,l]=mean(zsi_hld [ind0[i]:ind1[i],l]) ; hold Si darks first dark cycle
                    ;;y0[i,1,l]=mean(nsi_hld [ind0[i]:ind1[i],l]) ; hold Si darks first dark cycle
                    iny0[i,0,l]=mean(zir_hld [ind0[i]:ind1[i],l]) ; hold InGaAs darks first dark cycle
                    iny0[i,1,l]=mean(nir_hld [ind0[i]:ind1[i],l]) ; hold InGaAs darks first dark cycle

                    x1=mean(bx_hld[ind0[i+1]:ind1[i+1]]) ; hold temperature second dark cycle
                    ;;y1[i,0,l]=mean(zsi_hld [ind0[i+1]:ind1[i+1],l]) ; hold Si darks second dark cycle
                    ;;y1[i,1,l]=mean(nsi_hld [ind0[i+1]:ind1[i+1],l]) ; hold Si darks second dark cycle
                    iny1[i,0,l]=mean(zir_hld [ind0[i+1]:ind1[i+1],l]) ; hold InGaAs darks second dark cycle
                    iny1[i,1,l]=mean(nir_hld [ind0[i+1]:ind1[i+1],l]) ; hold InGaAs darks second dark cycle

                    ;print, where(abs(y0[i,3,l] - nir_hld[ind0[i]:ind1[i],l]) ge 3*y2[i,0,l])
                    ;stop

                    if abs(x0-x1) gt 0.2 then begin ; define temperature coefficients
                        for j=0,1 do begin ; only if temperature difference between
                            inbb[i,j,l]=(iny1[i,j,l]-iny0[i,j,l])/(x1-x0) ; two dark cycles larger than 0.2 K
                            inaa[i,j,l]=iny1[i,j,l]-inbb[i,j,l]*x1
                        endfor
                    endif else begin
                        for j=0,1 do begin
                            inaa[i,j,l]=0.5*(iny0[i,j,l]+iny1[i,j,l])
                            inbb[i,j,l]=0.
                        endfor
                    endelse
                endfor

                ;if(i eq 0) then plot,y2[i,0,*] else oplot,y2[i,0,*]
                ;stop
            endif
        endfor
    endif
    if darkc eq 1 then begin    ; one dark cycle only available
        siy0=fltarr(1,2,sinp) ; in this case, no temperature correlation possible
        iny0=fltarr(1,2,innp) ; in this case, no temperature correlation possible
        for l=0,sinp-1 do begin
            siy0[0,0,l]=mean(zsi_hld[ind0[0]:ind1[0],l])
            siy0[0,1,l]=mean(nsi_hld[ind0[0]:ind1[0],l])
        endfor
        for l=0,innp-1 do begin
            iny0[0,0,l]=mean(zir_hld[ind0[0]:ind1[0],l])
            iny0[0,1,l]=mean(nir_hld[ind0[0]:ind1[0],l])
        endfor
        siy1=siy0
        iny1=iny0
    endif
    if darkc le 0 then message,'No darks available!' ; exit program if no darks were found.

; Output darks to file - there is a file for the linear interpolation method and one for the temperature method.
    if darkc ge 2 then begin
        openw,uaa,darki,/get_lun ; normal linear interpolation file
        openw,uab,darkt,/get_lun ; temperature correlation file
        for i=0,darkc-1 do begin
            printf,uab,darktime[i,0],min(bx_hld[ind0[i]:ind1[i]]),max(bx_hld[ind0[i]:ind1[i]])
            printf,uaa,darktime[i,0],1
            for l=0,sinp-1 do begin
                printf,uab,form='(8(x,f17.8))',siaa[i,0,l],sibb[i,0,l],siaa[i,1,l],sibb[i,1,l]
                printf,uaa,form='(8(x,f17.8))',siy0[i,0,l],siy1[i,0,l],siy0[i,1,l],siy1[i,1,l]
            endfor
            for l=0,innp-1 do begin
                printf,uab,form='(8(x,f17.8))',inaa[i,0,l],inbb[i,0,l],inaa[i,1,l],inbb[i,1,l]
                printf,uaa,form='(8(x,f17.8))',iny0[i,0,l],iny1[i,0,l],iny0[i,1,l],iny1[i,1,l]
            endfor
        endfor
        printf,uab,darktime[i,0],-1,-1
        printf,uaa,darktime[i,0],-1
        free_lun,uab
        free_lun,uaa
    endif

    if darkc eq 1 then begin
        openw,uaa,darki,/get_lun ; normal linear interpolation file
        printf,uaa,darktime[0,0],1
        for l=0,sinp-1 do begin
            printf,uaa,form='(8(x,f17.8))',siy0[0,0,l],siy1[0,0,l],siy0[0,1,l],siy1[0,1,l]
        endfor
        for l=0,innp-1 do begin
            printf,uaa,form='(8(x,f17.8))',iny0[0,0,l],iny1[0,0,l],iny0[0,1,l],iny1[0,1,l]
        endfor
        printf,uaa,darktime[0,0],-1
        free_lun,uaa
    endif

endif          ; if nd ne 1 - end of processing darks and writing file

; read first dark from file
if dark eq 'T' then begin
    openr,darku,darkt,/get_lun
    readf,darku,utd0,t0dark,t1dark ; dark utc, valid temperature range (not used)
    siaa=fltarr(2,sinp) & sibb=fltarr(2,sinp)
    inaa=fltarr(2,innp) & inbb=fltarr(2,innp)
    for i=0,sinp-1 do begin
        readf,darku,a0,b0,a1,b1
        siaa[0,i]=a0 & siaa[1,i]=a1
        sibb[0,i]=b0 & sibb[1,i]=b1
    endfor
    for i=0,innp-1 do begin
        readf,darku,a0,b0,a1,b1
        inaa[0,i]=a0 & inaa[1,i]=a1
        inbb[0,i]=b0 & inbb[1,i]=b1
    endfor
    readf,darku,utd1,t0next,t1next ; next dark time, valid temperature range (not used)
endif else begin
    openr,darku,darki,/get_lun
    readf,darku,utd0,flag       ; dark utc
    if flag ne 1 then message,'Dark format wrong.'
    siaa=fltarr(2,sinp) & sibb=fltarr(2,sinp)
    inaa=fltarr(2,innp) & inbb=fltarr(2,innp)
    for i=0,sinp-1 do begin
        readf,darku,a0,b0,a1,b1
        siaa[0,i]=a0 & siaa[1,i]=a1
        sibb[0,i]=b0 & sibb[1,i]=b1
    endfor
    for i=0,innp-1 do begin
        readf,darku,a0,b0,a1,b1
        inaa[0,i]=a0 & inaa[1,i]=a1
        inbb[0,i]=b0 & inbb[1,i]=b1
    endfor
    readf,darku,utd1,flag       ; next dark time
    if flag eq -1 then begin    ; no more dark files!
        free_lun,darku
        utd1=100.
    endif
endelse
; transfer to main program: aa,bb,utd0,utd1
end


pro read_drk,sinp,innp,darkmin,dark,darku,siaa,sibb,inaa,inbb,utd0,utd1
; read dark from file
utd0=utd1
if dark eq 'T' then begin
    for i=0,sinp-1 do begin
        readf,darku,a0,b0,a1,b1
        siaa[0,i]=a0 & siaa[1,i]=a1
        sibb[0,i]=b0 & sibb[1,i]=b1
    endfor
    for i=0,innp-1 do begin
        readf,darku,a0,b0,a1,b1
        inaa[0,i]=a0 & inaa[1,i]=a1
        inbb[0,i]=b0 & inbb[1,i]=b1
    endfor
    readf,darku,utd1,t0next,t1next ; next dark time, valid temperature range
    if t0next eq -1 and t1next eq -1 then begin
        free_lun,darku
        utd1=100.
    endif
endif else begin
    for i=0,sinp-1 do begin
        readf,darku,a0,b0,a1,b1
        siaa[0,i]=a0 & siaa[1,i]=a1
        sibb[0,i]=b0 & sibb[1,i]=b1
    endfor
    for i=0,innp-1 do begin
        readf,darku,a0,b0,a1,b1
        inaa[0,i]=a0 & inaa[1,i]=a1
        inbb[0,i]=b0 & inbb[1,i]=b1
    endfor
    readf,darku,utd1,flag       ; next dark time
    if flag eq -1 then begin    ; no more dark files!
        free_lun,darku
        utd1=100.
    endif
endelse
; transfer to main program: aa,bb,utd0,utd1
end
