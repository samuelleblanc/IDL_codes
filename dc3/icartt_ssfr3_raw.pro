;;;;;;;;;;;;;;;;;;;; j31_ssfr1_raw.pro ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;This program will read in the data files from the radiometer program.
;It is specifically setup for the SSFR3.
;Note:  Because it uses unsigned long integers, this program only
;       runs on IDL version 5.2 or later.
;
;NOTE:  THINGS TO DO BEFORE RUNNING THIS PROGRAM
;       1.  dir == directory in which the spc* files are placed.
;           Change the directories to the proper directory.
;       2.  The temp.dat file containing temperature conversion
;           data must be included either via the directory  
;           variable, maudir, or be placed within the working
;           directory.
;
;Operation:  Once the directory has been set no further interaction
;with the program is necessary.  The program will cd to the 
;directory containing the spc files, count the number of files,
;store the file names in the variable spc_files, then cd back
;to the working directory.  It will then use both the file count
;(numfiles) and the spec_files array to access and run the
;program.  The zenith and nadir  spectrum will be plotted in their
;respective windows (window 0 for zenith and window 1 for nadir).  
;     No processing of any spectra is done in this program.  Darks
;are not subtracted, the spectra are not normalized by the 
;integration time and they are not splined.  At the conclusion
;of the program plots are made of the time vs. temperature.
;The three temperatures are plotted in a multiplot window.
;
;;;;;;;; Directories, File names and open output file ;;;;;;;;;;;;;;;

!P.multi = 0                     ;Insure window is in single plot
;loadct, 27

dir = '/home/seven/kindel/SSFR/GapConeAZ06h/'
;;;;;;;;;;;;;; Find the number of files in the Directory ;;;;;;;;;;;;;;;

cd, dir, current = curr_path
spc_files = findfile('*.OSA', count = numfiles)
cd, curr_path

if (numfiles eq 0) then begin
   cd, dir, current = curr_path
   spc_files = findfile('*.osa', count = numfiles)
   cd, curr_path
endif

print, 'Number of files: ', numfiles

;;;;;;;;;;;;;;;Define and  initialize variables ;;;;;;;;;;;;;;;;;;;;;;

np = 256                 ;number of data points in a spectrum
fprt = 0                 ;set to one (1) for postscript output

atime = strarr(1)

bignum = 50000L

tmhrs = fltarr(bignum)   ;array to hold time
secs = lonarr(bignum)
ztmp = fltarr(bignum)    ;array to hold temperature for zenith
ntmp = fltarr(bignum)    ;array to hold temperature for nadir
itmp = fltarr(bignum)    ;array to hold the internal temp
zrtmp = ulonarr(bignum)
nrtmp = ulonarr(bignum)
irtmp = ulonarr(bignum)

iz = fltarr(bignum)      ;array to hold temperature for zenith
in = fltarr(bignum)      ;array to hold temperature for nadir

zrawsi = fltarr(np, bignum)
zrawir = fltarr(np, bignum)
nrawsi = fltarr(np, bignum)
nrawir = fltarr(np, bignum)

spec1 = lonarr(np)       ;spectra zenith SI
spec2 = lonarr(np)       ;spectra zenith InGaAs
spec3 = lonarr(np)       ;spectra nadir SI
spec4 = lonarr(np)       ;spectra nadir InGaAs

wlvisz = fltarr(np)      ;wavelength array for SI zenith
wlvisn = fltarr(np)      ;wavelength array for InGaAs zenith
wlnirz = fltarr(np)      ;wavelength array for SI nadir
wlnirn = fltarr(np)      ;wavelength array for InGaAs nadir

fj = fltarr(np)          ;counter used in construction wavelength arrays

cvisz = fltarr(4)        ;array for zenith SI coefficients
cvisn = fltarr(4)        ;array for nadir SI coefficients
cnirz = fltarr(4)        ;array for zenith InGaAs coefficients
cnirn = fltarr(4)        ;array for nadir InGaAs coefficients

;;;;;;;;;;;;; Load coefficients for the SSFR3 ;;;;;;;;;;;;;;;;;;;;;
;;;;;; Coefficients for MMS1 NIR-enh:  Zenith (009159) ;;;;;;;;;;;;;

cvisz(0) = 3.048678591e2
cvisz(1) = 3.287663420
cvisz(2) = 1.975037077e-4
cvisz(3) = -1.592218480e-6

;;;;; Coefficients for MMS NIR tc II:  Zenith (021299) ;;;;;;

cnirz(0) = 9.424712905e2
cnirz(1) = 6.245178015
cnirz(2) = -9.179161662e-4
cnirz(3) = -4.206810248e-6

;;;;; Coefficients for MMS1 NIR-enh:  Nadir (009117) ;;;;;;;

cvisn(0) = 3.056365589e2
cvisn(1) = 3.281352579
cvisn(2) = 2.301335982e-4
cvisn(3) = -1.518225206e-6

;;;;; Coefficients for MMS NIR tc II:  Nadir (005714) ;;;;;;;

cnirn(0) = 9.383239708e2
cnirn(1) = 6.330279881
cnirn(2) = -2.042871831e-3
cnirn(3) = 1.711020638e-7

;;;;;;;;;;;;;;;;;;;;;; Construct Wavelength Arrays ;;;;;;;;;;;;;;;;;;;;

for j = 0, np - 1 do begin

fj(j) = float(j)
wlvisz(j) = cvisz(0)+fj(j)*(cvisz(1)+fj(j)*(cvisz(2)+fj(j)*cvisz(3)))
wlvisn(j) = cvisn(0)+fj(j)*(cvisn(1)+fj(j)*(cvisn(2)+fj(j)*cvisn(3)))
wlnirz(j) = cnirz(0)+fj(j)*(cnirz(1)+fj(j)*(cnirz(2)+fj(j)*cnirz(3)))
wlnirn(j) = cnirn(0)+fj(j)*(cnirn(1)+fj(j)*(cnirn(2)+fj(j)*cnirn(3)))

endfor

;;;;;;;;;;;;;;;;;;;;; Set plot to xterm or ps ;;;;;;;;;;;;;;;;;;;;;;;;

if (fprt eq 0) then begin
    set_plot, 'x'
endif else begin
    set_plot, 'ps'
   print, 'plot is ps'
   device, /times, /landscape, filename = 'zentest.ps'
   device, /inches, xsize = 9.5, ysize = 6.5, yoffset=9.75;,xoffset=-12.5
endelse

;;;;;;;;;;;;;;;; Read Thermistor Voltage-Temperature data ;;;;;;;;;;;;;

npt = 31
tdat = fltarr(2, npt)
revt = fltarr(2, npt)

openr, lun, 'temp.dat', /get_lun
readf, lun, tdat
free_lun, lun

;;;;;;;;;;;;;;;;;;;; reverse order of temp data ;;;;;;;;;;;;;;;;;;;;;;;

for i = 0, npt - 1 do begin
   revt(0, i) = tdat(0, npt -i - 1)
   revt(1, i) = tdat(1, npt -i - 1)
endfor

;;;;;;;;;;;;;; interpolate temp data at fine resolution ;;;;;;;;;;;;;;;

rt = findgen(20000)/10000. + 0.
revts = spline(revt(1, *), revt(0, *), rt)

;;;;;;;;;;;;; Open windows: 0 for zenith, 1 for nadir ;;;;;;;;;;;;;;;;;

win_xsize = 400                                        ;NEW
win_ysize = 300                                        ;NEW
window,/free,/pixmap,xs=win_xsize,ys=win_ysize         ;NEW virtual win
pixmap_window0_id = !D.window                          ;NEW

window, 0, xsize = 400, ysize = 300, xpos = 860, ypos = 720, $
      title = 'Zenith'       
window0_id = !D.window                                 ;NEW
 
window,/free,/pixmap,xs=win_xsize,ys=win_ysize         ;NEW virtual win
pixmap_window1_id = !D.window                          ;NEW
window, 1, xsize = 400, ysize = 300, xpos = 860, ypos = 390, $
      title = 'Nadir'               
window1_id = !D.window                                 ;NEW

ctr = 0L

;;;;;;;;;;;;;;;;;;;;;;;; Start FOR Loop ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

for ind = 0L, numfiles - 1 do begin     ;begin primary FOR loop
;for ind = 100L, numfiles - 1 do begin     ;begin primary FOR loop

   openr, lun, dir + spc_files(ind), /get_lun
   pos=0
;   print, ind, numfiles
;;;;;;;;;;;;;;;;;;;;;;;;; read in data ;;;;;;;;;;;;;;;;;;;;;;;

   while not eof(lun) do begin

   spect = {btime: lonarr(2), bcdtimstp:  bytarr(12), $
            intime1: long(0), $
            intime2: long(0), intime3: long(0), intime4: long(0), $
            accum: long(0), $
;            ztemp: long(0), ntemp: long(0), itemp: long(0), $
            ztemp: ulong(0), ntemp: ulong(0), itemp: ulong(0), $
            zspecsi: intarr(np), zspecir: intarr(np), $
            nspecsi: intarr(np), nspecir: intarr(np)}

   point_lun,lun,pos
;***** error loop start point; fs is file status indicator

   fs=fstat(lun)

;***** test if remaining file size is less than one spectrum size,
;***** approximately 20K bytes

   if fs.size-fs.cur_ptr lt 2100 then begin    ;checks file size (in bytes)

   goto, jump5

   endif else begin  

   readu, lun, spect

;***** Determine current position of file pointer and assign to pos
   point_lun,-lun,pos

;;;;; Convert time from binary to string and prepare for printing ;;;;;;;

    atime = systime(0, spect.btime(0), /utc)      ;convert date

    result = strpos(atime(0), ':', 0)      ;find first incidence of ':'
    day =  strmid(atime(0), result - 5, 2)
    mon =  strmid(atime(0), result - 9, 3)
    hr = strmid(atime(0), result - 2, 2)          ;find hour
    min = strmid(atime(0), result + 1, 2)         ;find minutes
    sec = strmid(atime(0), result + 4, 2)         ;find seconds
    year = fix(strmid(atime(0), result + 7, 4))   ;find year
    yr2dig = strmid(atime(0), result + 9, 2)  ;find year - last two digits
    adate = day + mon + yr2dig
    fdate = string(day) + ' ' + string(mon) + string(year)

    tmhr = double(hr + (min/60.) + (sec/3600.))   ;put hr in decimal form
    secs(ctr) = (long(hr) * 3600L) + (long(min) * 60L) + long(sec)
    tmhrs(ctr) = tmhr
print, ind, secs(ctr)
;;;;;;;;;;;;;;;;; Convert Voltage to Temp ;;;;;;;;;;;;;;;;;;;;

    ztv = 10.*(float(spect.ztemp)/4096.)         ;zenith raw
    zitv = long(ztv * 10000.)     ;convert to integer for use as index

    ntv = 10.*(float(spect.ntemp)/4096.)        ;nadir raw
    nitv = long(ntv * 10000.)     ;convert to integer for use as index

;;;;;;;;;;;; Third thermistor internal to the computer ;;;;;;;;;;;;;

     intertmp = spect.itemp*(10./4096.)

     rtem = abs((2000. * intertmp)/(1. - (0.2 * intertmp)))
     dem = 1.0295e-3 + (2.391e-4 * alog(rtem)) + (1.568e-7 * (alog(rtem))^3)
     intv = float(1./dem)

     if (zitv le 19999) then begin  
        ztmp(ctr) = revts(zitv)       ;load zenith temp into array
     endif
     if (nitv le 19999) then begin
        ntmp(ctr) = revts(nitv)       ;load nadir temp into array
     endif
     itmp(ctr) = float(intv - 273.)         ;holds internal temp 
      
     zrtmp(ctr) = spect.ztemp
     nrtmp(ctr) = spect.ntemp
     irtmp(ctr) = spect.itemp

     zrawsi(*, ctr) = spect.zspecsi
     zrawir(*, ctr) = spect.zspecir
     nrawsi(*, ctr) = spect.nspecsi
     nrawir(*, ctr) = spect.nspecir

     ctr = ctr + 1L

;;;;;;;;;;;;;;;;;;;;;; Plot zenith and nadir spectrum ;;;;;;;;;;;;;;;;;

     wset, pixmap_window0_id  ;resets win as the "active" win NEW

     plot, wlvisz, spect.zspecsi, $
     /xstyle, ystyle = 1, xrange = [300, 1700], yrange = [-2000, 35000], $ 
     title = 'Zenith ' + string(fdate) + ', Time:' + string(tmhr), $
     xtitle='Wavelength', ytitle='Signal';, psym=2, color = 5

     oplot, wlnirz, spect.zspecir;, psym=2, color = 155

     wset, window0_id    ;This is the active window               ;NEW
     device,copy=[0,0,win_xsize,win_ysize,0,0,pixmap_window0_id]  ;NEW

     wset, pixmap_window1_id   ;resets win as the "active" win  ;NEW

     plot, wlvisn, spect.nspecsi, $
     title = 'Nadir ' + string(fdate) + ', Time:' + string(tmhr), $
     /xstyle, ystyle = 1, xrange = [300, 1700], yrange = [-2000, 35000], $ 
     xtitle='Wavelength', ytitle='Signal';,psym=2, color = 5

     oplot, wlnirn, spect.nspecir, color = 155      ;,psym=2

     wset, window1_id       ;this is the active window           ;NEW
     device,copy=[0,0,win_xsize,win_ysize,0,0,pixmap_window1_id] ;NEW

;;;;;;;;;;;;;;;;;;;;;; Determine if printing is needed ;;;;;;;;;;;;;;

     if (fprt ne 0) then begin
        device, /close
        set_plot, 'x'
     endif
     
;;;;;;;;;;;;;;;;;;;;;; End of WHILE loop ;;;;;;;;;;;;;;;;;;;;;;;;;;;

     endelse                     ;end of checking file size
     endwhile                    ;end WHILE loop
     
     jump5: 
     free_lun, lun               ;free logical unit
    
;;;;;;;;;;;;;;;;;;;;;; End of FOR loop ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

endfor                           ;end of primary FOR loop

!P.multi = 0    ;restore sing plt win in IDL
set_plot, 'x'   ;restore X windows in IDL

ctr = ctr - 1
print, 'counter is: ', ctr
secs = secs(0:ctr)
zrawsi = zrawsi(*, 0:ctr)
zrawir = zrawir(*, 0:ctr)
nrawsi = nrawsi(*, 0:ctr)
nrawir = nrawir(*, 0:ctr)

;;;;;;;;;;;;;;;;;;;;; PLOT Time vs Temp ;;;;;;;;;;;;;;;;;;;;;

fprt = 0
if (fprt eq 0) then begin
    set_plot, 'x'
endif else begin
    set_plot, 'ps'
   print, 'plot is ps'
   device, /times, /landscape, filename = 'zentest.ps'
   device, /inches, xsize = 9.5, ysize = 6.5, yoffset=9.75;,xoffset=-12.5
endelse

!P.multi = [0, 1, 3, 0, 1]       ;Set multiple plots, 2 rows and 1 col

if (fprt eq 0) then begin
  window, 2, xsize = 400, ysize = 300, xpos = 455, ypos = 720, $
  title = 'Temperature'
endif

tm = where(tmhrs ne 0)           ;Find index for non zero data points

plot, tmhrs(tm(1:*)), smooth(ztmp(tm), 60), xtitle = 'Time', $
  /xstyle, ystyle = 1, title = 'Zenith Temperature', thick = 1,$
  ytitle = 'Temperature', yrange = [25, 28], charsize = 1.,$
  charthick = 1.

plot, tmhrs(tm(1:*)), smooth(ntmp(tm), 60), xtitle = 'Time', $
  /xstyle, ystyle = 1, title = 'Nadir Temperature', thick = 1,$
  ytitle = 'Temperature', yrange = [25, 28], charsize = 1.,$
  charthick = 1.

plot, tmhrs(tm(1:*)), smooth(itmp(tm), 60), xtitle = 'Time', $
  /xstyle, ystyle = 1, title = 'Nadir Temperature', thick = 1,$
  ytitle = 'Temperature', yrange = [25, 28], charsize = 1.,$
  charthick = 1.
 
if (fprt ne 0) then begin
     device, /close
     set_plot, 'x'
     !P.multi = 0             ;Restore window to single plot     
endif

;;;;;;;;;;;;;;;; END of program ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

!P.multi = 0              ;Restore window to single plot
set_plot, 'x'


;stuff for plotting cosine test from lab cal


data=reform(ZRAWSI,256,25,19)
darkavg=fltarr(256)
dark=data[*,*,18]
datafinal=fltarr(256,18)

for i=0,255 do begin
 temp=mean(dark[i,*])
 darkavg[i]=temp 
endfor 

data=data[*,*,0:17]
dataavg=fltarr(256,18)

 for i=0,255 do begin 
  for j=0,17 do begin
   temp=mean(data[i,*,j])
   dataavg[i,j]=temp
  endfor
 endfor

 for i=0,17 do begin
  datafinal[*,i]=dataavg[*,i]-darkavg  
 endfor
 




angles=indgen(18)*5.
window,5,retain=2

plot,cos(angles*!dtor),datafinal[100,*]/datafinal[100,0],psym=4,title='GapCone'
oplot,[0,1],[0,1]























stop
end
