@legend.pro
pro wlcal

file='/Users/schmidt/data/attrex/cal/ssfr6/wl/wlcalir.txt'

openr,uf,file,/get_lun
string=''
nf=0
i=0
title=''
while not eof(uf) do begin
  readf,uf,string
  print,string
  if i eq 0 or i eq 1 then begin
    title=title+string
  endif
  i=i+1
  if not strcmp(string,'#',1) then begin
    data=strsplit(string,', ',/EXTRACT)
    if nf eq 0 then begin
      nadir=float(data[0])
      zenith=float(data[1])
      lamp=float(data[2])
      label=data[3]
    endif else begin
      nadir =[nadir,float(data[0])]
      zenith=[zenith,float(data[1])]
      lamp  =[lamp,float(data[2])]
      label =[label,data[3]]
    endelse
    nf=nf+1
  endif
endwhile
free_lun,uf

loadct,27
device,decomposed=0

xx=[min(lamp),max(lamp)]
plot,lamp,nadir,psym=4,xtit='WL lamp',ytit='WL SSFR',tit=title,yr=xx
resn=linfit(lamp,nadir)
oplot,xx,resn[0]+resn[1]*xx
oplot,lamp,zenith,psym=4,color=200
resz=linfit(lamp,zenith)
oplot,xx,resz[0]+resz[1]*xx,color=200
legend,['nadir'+strjoin(string(resn)),'zenith'+strjoin(string(resz))],textcolor=[255,200]

p=tvrd(true=1)
write_png,file+'.png',p

window,1
plot,lamp,nadir-lamp
oplot,lamp,zenith-lamp
stop
end