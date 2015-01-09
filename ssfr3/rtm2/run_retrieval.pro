; program to run the transmission cloud retrieval over all time in a day
; uses the data from the roof top
; uses the calibspcs save files

@zensun.pro
@cloud_rtm.pro

pro run_retrieval, date, two=two
if n_elements(two) lt 1 then twowvl=0 else twowvl=1
if n_elements(date) lt 1 then date='20120525'
twelve=0
avg=0
snow=0
win=1

if date eq '20130110' then snow=1

if snow then ics='_ic' else ics=''

if not win then dir='/argus/roof/SSFR3/data/'+date+'/out/' else $
  dir='C:\Users\Samuel\Research\SSFR3\data\'
restore, dir+date+'_calibspcs.out'

;get the top of atmosphere irradiance to calculate the transmittance
;print, 'getting extra terrestrial irradiance'
;F_o=read_ascii('/home/leblanc/libradtran/libRadtran-1.6-beta/data/solar_flux/kurudz_1.0nm.dat', comment_symbol='#',data_start=12)
;vis=read_ascii('/home/leblanc/libradtran/vis_1nm.dat')
;nir=read_ascii('/home/leblanc/libradtran/nir_1nm.dat')
;
;F_o.field1[1,*]=F_o.field1[1,*]/1000.
;vis.field1[1,*]=vis.field1[1,*]/total(vis.field1[1,*])
;nir.field1[1,*]=nir.field1[1,*]/total(nir.field1[1,*])
;fvis=fltarr(n_elements(F_o.field1[1,*]))
;fnir=fvis
;for i=7,n_elements(f_o.field1[0,*])-8 do for j=-7,7 do fvis[i]=fvis[i]+f_o.field1[1,i+j]*vis.field1[1,j]
;for i=15,n_elements(f_o.field1[0,*])-16 do for j=-15,15 do fnir[i]=fnir[i]+f_o.field1[1,i+j]*nir.field1[1,j]
;foz=[interpol(fvis,F_o.field1[0,*],zenlambda[0:193]),interpol(fnir,F_o.field1[0,*],zenlambda[194:*])]
;fon=[interpol(fvis,F_o.field1[0,*],nadlambda[0:193]),interpol(fnir,F_o.field1[0,*],nadlambda[194:*])]


lat=40.007916667
lon=-105.26825
doy=julian_day(fix(strmid(date,0,4)),fix(strmid(date,4,2)),fix(strmid(date,6,2)))
zensun, doy, tmhrs, lat, lon, sza, azimuth, solfac

if snow then doyref=10 else doyref=145
zensun, doyref, tmhrs, lat, lon, szaref, azimuthref, solfacref

n=n_elements(tmhrs)
tau=fltarr(n) & etau=tau
ref=fltarr(n) & eref=ref
utc=fltarr(n)

;o=min(abs(tmhrs-18.0),tms)
tms=0
case date of
;  '20120602':fl=where(tmhrs gt 21.  and tmhrs lt 23.);  and area2a gt 10  and areaa  gt -2)
;  '20120813':fl=where(tmhrs gt 15.  and tmhrs lt 19.);  and area2b gt 10)
;  '20120525':fl=where(tmhrs gt 15.  and tmhrs lt 16.);  and tauc   lt 200 and area2c gt 10.)
;  '20120523':fl=where(tmhrs gt 21.  and tmhrs lt 24.);  and taud   lt 200 and area2d gt 30.)
;  '20120912':fl=where(tmhrs gt 15.5 and tmhrs lt 18.5); and area2e gt 40  and area2e lt 100. and areae gt 5)
;  '20120806':fl=where(tmhrs gt 22.  and tmhrs lt 24.5); used to be 20 to 24.5
;  '20120816':fl=where(tmhrs gt 13.5 and tmhrs lt 17.)
;  '20120820':fl=where(tmhrs gt 16.  and tmhrs lt 20.);  and area2h gt 10. and areah  lt 4.)
;  '20120824':fl=where(tmhrs gt 19.5 and tmhrs lt 23.);  and area2i gt 25)
;  '20130110':fl=where(tmhrs gt 15.  and tmhrs lt 22.)
;  '20130111':fl=where(tmhrs gt 21.  and tmhrs lt 23.)
    '20120602':fl=where(tmhrs gt 21.  and tmhrs lt 23.);  and area2a gt 10  and areaa  gt -2)
    '20120813':fl=where(tmhrs gt 16.5 and tmhrs lt 18.5);  and area2b gt 10)
    '20120525':fl=where(tmhrs gt 15.  and tmhrs lt 16.);  and tauc   lt 200 and area2c gt 10.)
    '20120523':fl=where(tmhrs gt 21.5 and tmhrs lt 23.5);  and taud   lt 200 and area2d gt 30.)
    '20120912':fl=where(tmhrs gt 16.  and tmhrs lt 18.); and area2e gt 40  and area2e lt 100. and areae gt 5)
    '20120806':fl=where(tmhrs gt 22.  and tmhrs lt 23); used to be 20 to 24.5
    '20120816':fl=where(tmhrs gt 15.  and tmhrs lt 16)
    '20120820':fl=where(tmhrs gt 16.5 and tmhrs lt 18.5);  and area2h gt 10. and areah  lt 4.)
    '20120824':fl=where(tmhrs gt 21.  and tmhrs lt 22.);  and area2i gt 25)
    '20130110':fl=where(tmhrs gt 17.5 and tmhrs lt 19.5)
    '20130111':fl=where(tmhrs gt 21.  and tmhrs lt 23.)
  else: message, 'wrong date'
endcase


di=30
jj=0
num=n_elements(tmhrs[fl])

if avg then begin
tau=fltarr(num/di+1) & etau=tau
ref=fltarr(num/di+1) & eref=ref
utc=fltarr(num/di+1)
di=30
endif else di=1

di=30

if twowvl then lbl='_2wvl' else lbl=''
if twelve then lbl=lbl+'_1227nm' else lbl=lbl+'_1600nm'
if snow then lbl=lbl+'_snow'
if not win then restore, '/argus/roof/SSFR3/model/CLD_LUT_HIRES_t100'+lbl+'.out' else $ 
 restore, dir+'CLD_LUT_HIRES_t100'+ics+lbl+'.out'
 ;'/home/leblanc/DC3_SEAC4RS/library/CLD_LUT_HIRES2'+lbl+'.out' else $
lut={radhi:radhi,refs_hi:refs_hi,slohi:slohi,sun:sun,szas:szas,taus_hi:taus_hi,wvls:wvls}

for j=0, num-1,di do begin
  i=fl[j]
 if not avg then  jj=i
 if avg then  zsp=mean(zspectra[*,fl[j:((j+di gt num-1)?num-1:j+di)]],dimension=2) else $
  zsp=zspectra[*,fl[j]]
  sp=zsp*solfac[i]/cos(sza[i]*!dtor)/solfacref[i]*cos(szaref[i]*!dtor)
  if sza[i] lt 90.0 then begin
    cloud_rtm,sza[i],sp,zenlambda,t,r,/hires,twowvl=twowvl,lut=lut,dtau=dtau,dref=dref ;,/tauonly,/hires
    tau[jj]=t & etau[jj]=dtau
    ref[jj]=r & eref[jj]=dref
  if (i mod 60) eq 0 then print, date,tmhrs[i],t,r
  endif else begin
    tau[jj]=!values.f_nan
    ref[jj]=!values.f_nan
  endelse
  utc[jj]=tmhrs[i]
;i=i+60
print, utc[jj],tau[jj],ref[jj],etau[jj],eref[jj],j,'/',num
if avg then jj=jj+1
  ;if (i mod 60) eq 0 then print, tmhrs[i],t,r 
endfor

;tau=tau[fl]
;ref=ref[fl]

tmhrs=utc

if twowvl then lbl='_2wvl' else lbl=''
if twelve then lbl=lbl+'_1227nm' else lbl=lbl+'_1600nm'
save, tau,ref,tmhrs,etau,eref,filename=dir+date+'_cld_parms3'+ics+lbl+'.out'
stop
end
