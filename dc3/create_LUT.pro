; program to read in the lut up table with various wavelengths then ouput a lut with values at 515 and slopes at 1630
; based on Shi's and Patrick's work

pro create_LUT

dir='/home/leblanc/DC3_SEAC4RS/library/'
restore, dir+'CLD_LIQ_LUT_BOULDER_sun.out'
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

sun=fltarr(nwvls)
suni=sun

for i=0,nwvls-1 do begin
  sun[i]=mean(rad[1,i,*,*,*])
  suni[i]=mean(irr[1,i,*,*,*])
endfor

for i=0,ntaus-1 do begin
  for j=0,nrefs-1 do begin
    for k=0, nszas-1 do begin
      rad500[i,j,k]=rad[0,0,i,j,k]/sun[0]
      irr500[i,j,k]=irr[0,0,i,j,k]/suni[0]
      rnorm=rad[0,1:*,i,j,k]/rad[0,1,i,j,k]*sun[1]/sun[1:*]
      inorm=irr[0,1:*,i,j,k]/irr[0,1,i,j,k]*suni[1]/suni[1:*]

      rtmp=linfit(wvls[1:*],rnorm)
      itmp=linfit(wvls[1:*],inorm)

      slorad[i,j,k]=rtmp[1]
      sloirr[i,j,k]=itmp[1]

              ; See McBride et al (2011, ACPD)
              ; The slope was calculated by fitting a line to the transmittance normalized by the transmittance at 1565 nm.
    endfor
  endfor
endfor


save, wvls, sun, suni, taus,refs,szas,doy,rad500,irr500,slorad,sloirr,doy,filename=dir+'CLD_LUT.out'
stop
end
