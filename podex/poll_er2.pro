@webget.pro
@legend.pro
pro poll_er2,hist

url1='http://asp-interface-2.arc.nasa.gov/API/binary_packet_data/N809NA/SSFR_LP_BIN'
url1='https://asp-interface-2.arc.nasa.gov/API/binary_packet_data/N809NA/SSFR_BIN'
bn=20000
if n_elements(hist) lt 1 then hist=0.25

utc = fltarr(bn)
pst = fltarr(bn)
lat = fltarr(bn)
lon = fltarr(bn)
alt = fltarr(bn)
pit = fltarr(bn)
rol = fltarr(bn)
pitm= fltarr(bn)
rolm= fltarr(bn)
pita= fltarr(bn)
rola= fltarr(bn)
t1  = fltarr(bn)

device,decomposed=0
loadct,27

; Beginning: Read file from the start
if 1 then begin
  web=webget(url1+'?Start=0')
  data=(strsplit(web.text,' ,',/EXTRACT))
  len =n_elements(data)
  for j=0,len-2 do begin
    if data[j] gt 0. and data[j+1] gt 0. then begin
      ;print,data[j]
      s=j
      goto,here
      message,'File empty.'
    endif
  endfor
  here:
  i=0
  s=long(s)
  while s+43 lt len do begin
    time=float(data[s+1])/3600.
    while time gt 24. do begin
      time=time-24.
    endwhile
    p0=time-8
    if p0 lt 0. then p0=p0+24 
    utc [i]=time
    pst [i]=p0
    lat [i]=data[s+2]
    lon [i]=data[s+3]
    alt [i]=data[s+4]
    rol [i]=data[s+5]
    pit [i]=data[s+6]
    rolm[i]=data[s+13]
    pitm[i]=data[s+14]
    t1  [i]=data[s+15]
    rola[i]=data[s+27]
    pita[i]=data[s+28]
    ;print,utc[i]
    s=s+42L
    if i gt 0 then begin
      if utc[i] gt utc[i-1] then begin
        i=i+1
      endif
    endif else begin
      i=i+1
    endelse
  endwhile
  n=i
endif

u0=0.
while 1 do begin
  web=webget(url1)
  data=(strsplit(web.text,' ,',/EXTRACT))
  len=n_elements(data)
  if len eq 43 then begin
    time=float(data[1])/3600.
    while time gt 24. do begin
      time=time-24.
    endwhile
    p0=time-8
    if p0 lt 0. then p0=p0+24 
    utc [i]=time
    pst [i]=p0
    lat [i]=data[2]
    lon [i]=data[3]
    alt [i]=data[4]
    rol [i]=data[5]
    pit [i]=data[6]
    rolm[i]=data[13]
    pitm[i]=data[14]
    t1  [i]=data[15]
    rola[i]=data[27]
    pita[i]=data[28]
    if utc[i] gt u0 then begin
      print,utc[i],utc[i-1]
      print,'Current values:'
      print,'UTC   :',utc[i]
      print,'PST   :',pst[i]
      print,'LON   :',lon[i]
      print,'LAT   :',lat[i]
      print,'ALT   :',alt[i]
      print,'PITCH :',pit[i],' (SPAN-CPT) ',pita[i],' (LTN-92) )',pitm[i],' (motor) '
      print,'ROLL  :',rol[i],' (SPAN-CPT) ',rola[i],' (LTN-92) )',rolm[i],' (motor) '
      print,'Heater:',0
      n=i+1
      mx=max(pst)
      !P.multi=[0,1,2]
      plot,pst[0:n-1],pit[0:n-1],psym=3,xtit='PST [h]',ytit='Angles [deg]',yr=[-1,5],xr=[mx-hist,mx],/xs
      oplot,pst[0:n-1],pita[0:n-1],psym=3,color=30
      oplot,pst[0:n-1],pitm[0:n-1],psym=4,color=60
      oplot,pst[0:n-1],rol[0:n-1],psym=3,color=90
      oplot,pst[0:n-1],rola[0:n-1],psym=3,color=120
      oplot,pst[0:n-1],rolm[0:n-1],psym=4,color=150
      legend,['pitch SPAN-CPT','pitch LTN-92','pitch motor',$
         'roll  SPAN-CPT','roll  LTN-92','roll  motor'],$
         textcolor=[255,30,60,90,120,150]
      plot,pst[0:n-1],alt[0:n-1],xr=[mx-hist,mx],/xs,xtit='PST [h]',ytit='ALT [m]'
      u0=utc[i]
      i=i+1
    endif
  endif
  print,'.',form='(a,$)'
  wait,5
  i=i+1
endwhile

stop



end
