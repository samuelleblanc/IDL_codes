pro calnex_oil2

r=findgen(255)
g=findgen(255)
b=findgen(255)


dir='/data/seven/schmidt/calnex/p3/'
SSFR_path=dir+'20100608/20100608_SP.out'
restore, SSFR_path      ;restore ssfr file

set_plot, 'x'

window,6, title='map with color'

device, decomposed=1
tvlct,r,g,b

mm=min(abs(zenlambda-450.),wl_450)
  mm=min(abs(zenlambda-550.),wl_550)
  mm=min(abs(zenlambda-650.),wl_650)
  
  max_450=max(zspectra[wl_450,*])
  max_550=max(zspectra[wl_550,*])
  max_650=max(zspectra[wl_650,*])
  
  ngeo=n_elements(alt)
  plot, lon, lat
    for i=0,ngeo-2 do begin
    cl=[[fix(255.*zspectra[wl_450,i]/max_450)],[fix(255.*zspectra[wl_550,i]/max_550)],[fix(255.*zspectra[wl_650,i]/max_650)]]
    cl_hex=string(cl,format='(Z2.2,Z2.2,Z2.2)')
    tvlct,cl,100
    oplot,lon[i:i+1],lat[i:i+1],color=100,psym=6
    ;if utc[i] gt utc0 + 0.25 then begin
    ;  xyouts,lon[i:i+1],lat[i:i+1],strmid(strcompress(string(fix(utc[i]*100)*0.01),/REMOVE_ALL),0,5),color=120
    ;  utc0=utc[i]
    ;endif
  endfor
  stop
  end