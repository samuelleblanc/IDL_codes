; program to read in the lut up table with various wavelengths then ouput a lut with values at 515 and slopes at 1630
; based on Shi's and Patrick's work

pro create_LUT
twowvl=1

dir='/home/leblanc/DC3_SEAC4RS/library/'
restore, dir+'CLD_LIQ_LUT_BOULDER_sun5.out'
ntaus=n_elements(taus)
nrefs=n_elements(refs)
nszas=n_elements(szas)
nwvls=n_elements(wvls)


rad500=fltarr(ntaus,nrefs,nszas)
irr500=fltarr(ntaus,nrefs,nszas)
slorad=fltarr(ntaus,nrefs,nszas)
sloirr=fltarr(ntaus,nrefs,nszas)

; load extra terrestrial source file
;F0=read_ascii('/home/leblanc/libradtran/libRadtran-1.5-beta/data/solar_flux/kurudz_1.0nm.dat',data_start=12)
;sun=interpol(F0.field1[1,*]/1000.,F0.field1[0,*],wvls)

;get the top of atmosphere irradiance to calculate the transmittance
print, 'getting extra terrestrial irradiance'
F_o=read_ascii('/home/leblanc/libradtran/libRadtran-1.6-beta/data/solar_flux/kurudz_1.0nm.dat', comment_symbol='#',data_start=12)
vis=read_ascii('/home/leblanc/libradtran/vis_1nm.dat')
nir=read_ascii('/home/leblanc/libradtran/nir_1nm.dat')

F_o.field1[1,*]=F_o.field1[1,*]/1000.
vis.field1[1,*]=vis.field1[1,*]/total(vis.field1[1,*])
nir.field1[1,*]=nir.field1[1,*]/total(nir.field1[1,*])
fvis=fltarr(n_elements(F_o.field1[1,*]))
fnir=fvis
for i=7,n_elements(f_o.field1[0,*])-8 do for j=-7,7 do fvis[i]=fvis[i]+f_o.field1[1,i+j]*vis.field1[1,j]
for i=15,n_elements(f_o.field1[0,*])-16 do for j=-15,15 do fnir[i]=fnir[i]+f_o.field1[1,i+j]*nir.field1[1,j]

sun=[interpol(fvis,F_o.field1[0,*],wvls[0]),interpol(fnir,F_o.field1[0,*],wvls[1:*])]

;sun=fltarr(nwvls,nszas)
;suni=sun
;
;for i=0,nwvls-1 do begin
;  for j=0,nszas-1 do begin
;    sun[i,j]=mean(rad[1,i,*,*,j])
;    suni[i,j]=mean(irr[1,i,*,*,j])
;  endfor
;endfor



for i=0,ntaus-1 do begin
  for j=0,nrefs-1 do begin
    for k=0, nszas-1 do begin
      rad500[i,j,k]=rad[0,0,i,j,k]/sun[0]/cos(szas[k]*!dtor)
  ;    irr500[i,j,k]=irr[0,0,i,j,k]/suni[0]
      rnorm=rad[0,1:*,i,j,k]/rad[0,1,i,j,k]
  ;    inorm=irr[0,1:*,i,j,k]/irr[0,1,i,j,k]*suni[1]/suni[1:*]

      rtmp=linfit(wvls[1:*],rnorm)
  ;    itmp=linfit(wvls[1:*],inorm)
      if twowvl then slorad[i,j,k]=rad[0,12,i,j,k]/sun[12]/cos(szas[k]*!dtor) else $
      slorad[i,j,k]=rtmp[1]
  ;    sloirr[i,j,k]=itmp[1]

              ; See McBride et al (2011, ACP)
              ; The slope was calculated by fitting a line to the transmittance normalized by the transmittance at 1565 nm.
    endfor
  endfor
endfor

if twowvl then lbl='_2wvl' else lbl=''
;save, wvls, sun, suni, taus,refs,szas,doy,rad500,irr500,slorad,sloirr,doy,filename=dir+'CLD_LUT.out'
save, wvls, sun, taus, refs, szas, doy, rad500, slorad, filename=dir+'CLD_LUT'+lbl+'.out'


; now do the same for a hires LUT
print, 'doing Hires'
tas=findgen(70)*2.8+5.
res=findgen(70)*0.35+0.5
r5=fltarr(ntaus,70)
s5=fltarr(ntaus,70)
rad=fltarr(70,70,nszas)
slo=fltarr(70,70,nszas)
for i=0, nszas-1 do begin
  for j=0, ntaus-1 do begin
    r5[j,*]=interpol(rad500[j,*,i],refs,res)
    s5[j,*]=interpol(slorad[j,*,i],refs,res)
  endfor
  for k=0, n_elements(res)-1 do begin
    rad[*,k,i]=interpol(r5[*,k],taus,tas)
    slo[*,k,i]=interpol(s5[*,k],taus,tas)
  endfor
endfor
taus_hi=tas
refs_hi=res
radhi=rad
slohi=slo

save, wvls, sun, taus_hi, refs_hi, szas, doy, radhi, slohi, filename=dir+'CLD_LUT_HIRES'+lbl+'.out'
stop
end
