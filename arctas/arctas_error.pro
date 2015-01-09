pro arctas_error
close, /all
date='20080709'
dir='/home/leblanc/libradtran/output/aero/'
;dir='\\lasp-smb\leblanc\libradtran\output\aero\'
; get tau modified
f=file_search(dir+'rtm_error_'+date+'_wvl*.txt')
f=f[sort(f)]
print, f

;dir='/home/leblanc/arctas/nasa/'+date+'/'
wvl0353=read_ascii(f[0], data_start=1)
wvl0380=read_ascii(f[1], data_start=1)
wvl0452=read_ascii(f[2], data_start=1)
wvl0499=read_ascii(f[3], data_start=1)
wvl0519=read_ascii(f[4], data_start=1)
wvl0605=read_ascii(f[5], data_start=1)
wvl0675=read_ascii(f[6], data_start=1)
wvl0779=read_ascii(f[7], data_start=1)
wvl0864=read_ascii(f[8], data_start=1)
wvl1019=read_ascii(f[9], data_start=1)
wvl1241=read_ascii(f[10], data_start=1)
wvl1558=read_ascii(f[11], data_start=1)
wvl2139=read_ascii(f[12], data_start=1)

wvl=[353,380,452,499,519,605,675,779,864,1019,1241,1558,2139]
spectrum=fltarr(16,9,13) ;data point, lats, wvl
for i=0, 15 do begin
  for j=0, 8 do begin
  spectrum[i,j,*]=[wvl0353.field01[i,j],wvl0380.field01[i,j],wvl0452.field01[i,j],wvl0499.field01[i,j],wvl0519.field01[i,j],wvl0605.field01[i,j],wvl0675.field01[i,j],wvl0779.field01[i,j],wvl0864.field01[i,j],wvl1019.field01[i,j],wvl1241.field01[i,j],wvl1558.field01[i,j],wvl2139.field01[i,j]];wvl2139.field01[i,*]]
  endfor
endfor
; ssa:2, asy:3, asy2:4, albedo:5, tau:11,    Ft_up:12,   Ft_dn:13,    Fb_up:14,   Fb_dn:15
;minus, 0, plus
na=[7,0,6] ;12
nb=[3,0,2] ; 14
za=[0,8] ; 13
zb=[5,0,4] ;15

ind=[2,3,4,5,11]
names=['SSA','ASY','ASY2','albedo','tau']
dind=fltarr(13,5)
dexp=dind
openw, 98, dir+'rtm_errors.dat'
printf, 98, 'wvl  '+names
for j=0, 12 do begin  ;wvl loop
for i=0, 4 do begin
  ;print, names[i]
  a=linfit(spectrum[12,na,j],spectrum[ind[i],na,j])
  b=linfit(spectrum[14,nb,j],spectrum[ind[i],nb,j])
  c=linfit(spectrum[13,za,j],spectrum[ind[i],za,j])
  d=linfit(spectrum[15,zb,j],spectrum[ind[i],zb,j])
  
  da=spectrum[12,na[1],j]-spectrum[12,na[0],j]
  db=spectrum[14,nb[1],j]-spectrum[14,nb[0],j]
  dc=spectrum[13,za[1],j]-spectrum[13,za[0],j]
  dd=spectrum[15,zb[1],j]-spectrum[15,zb[0],j]
  dind[j,i]=sqrt((a[1]^2 * da^2)+(b[1]^2 * db^2)+(c[1]^2 * dc^2)+(d[1]^2 * dd^2))
  dexp[j,i]=dind[j,i]/spectrum[ind[i],0,j]*100.
  
endfor

print, wvl[j],dind[j,0],dind[j,1],dind[j,2],dind[j,3],dind[j,4], 'relative (%)'
printf, 98, wvl[j],reform(dind[j,*]),'|', reform(dexp[j,*]),format='(I,5F,a,5f)'
endfor

close,/all
stop
end
