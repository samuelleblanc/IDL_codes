; program to create hires lut by interpolating between values of tau and ref
; keep the same sza values

pro lut_hires

restore, '/home/leblanc/DC3_SEAC4RS/library/CLD_LUT.out'

refs_hi=findgen(30)
taus_hi=[findgen(20)+5.,findgen(20)*2.+25.,findgen(20)*10.+65.]
nrhi=n_elements(refs_hi)
nthi=n_elements(taus_hi)

nszas=n_elements(szas)

radhi=fltarr(nthi,nrhi,nszas)
slohi=fltarr(nthi,nrhi,nszas)

rtmp=fltarr(nthi,n_elements(refs))
stmp=fltarr(nthi,n_elements(refs))

for i=0,nszas-1 do begin
  for j=0,n_elements(refs)-1 do begin
    rtmp[*,j]=interpol(rad500[*,j,i],taus,taus_hi)
    stmp[*,j]=interpol(slorad[*,j,i],taus,taus_hi)
  endfor
  for k=0, nthi-1 do begin
    radhi[k,*,i]=interpol(rtmp[k,*],refs,refs_hi)
    slohi[k,*,i]=interpol(stmp[k,*],refs,refs_hi)
  endfor
endfor


save, radhi,slohi,refs_hi,taus_hi,szas,wvls,sun,filename='/home/leblanc/DC3_SEAC4RS/library/CLD_LUT_HI.out'
stop
end
