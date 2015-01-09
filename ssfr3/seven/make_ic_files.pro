; program to make the ic_files
; takes input of optical depth and effective radius to calculate the accurate extension coefficient

pro make_ic_files, file, cloud

;cld should be an array of ;cld =[tau,ref,alt,thick]   ; tau,Reff,cloud_height_km; cloud_thickness_km
  iwp=2./3.*cloud[1]*cloud[0]
  iwc=iwp/cloud[3]*0.001
  refi=cloud[1]

restore, 'baum_2011_ice.out'
pm=fltarr(128)

openw, lun, file, /get_lun
for i=0, n_elements(wvl)-1 do begin
  ex=interpol(ext[*,i],ref,refi)
  ex=ex*iwc
  ss=interpol(ssa[*,i],ref,refi)
  for j=0, 127 do pm[j]=interpol(pmom[j,0,*,i],ref,refi)
printf, lun, wvl[i]*1000.,ex,ss,pm,format='(F6.1," ",F13.11," ",129(F11.7," "))'
endfor
close,lun
end
