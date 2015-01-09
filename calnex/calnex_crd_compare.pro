; program to compare crd, psap to retrieved aerosol values. on 20100519
@calnex_crd.pro

pro calnex_crd_compare
date= '20100519'
dir = '/home/leblanc/CALNEX/p3/'+date+'/'
restore, '/home/leblanc/CALNEX/p3/'+date+'/'+date+'_spectra_save.out'
restore, '/home/leblanc/CALNEX/p3/'+date+'/'+date+'_forcing.out'

calnex_crd, date, utc_crd, ext, ext_ambient, abs

;get the times of crd that represent the retrieved values
ind=0
for i=0, n_elements(utcbelow)-1 do begin
  mm=min(abs(utc_crd-utcbelow[i]),n)
  ind=[ind,n]
endfor
ind=ind[1:*]

ind_abv=0
for i=0, n_elements(utcabove)-1 do begin
  mm=min(abs(utc_crd-utcabove[i]),n)
  ind_abv=[ind_abv,n]
endfor
ind_abv=ind_abv[1:*]


if (1) then begin
set_plot, 'x'

;window, 0, title='SSA retrieved'
;plot, utcbelow, ssa[3,*]
;window, 1, title='CRD ext'
;plot, utcbelow, ext[ind]
;window, 2, title='PAS abs'
;plot, utcbelow, abs[ind]
 loadct, 39, /silent
 device, decomposed=0
      !p.font=1 & !p.thick=4 & !p.charsize=2.0 & !x.style=1
      !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
window, 0, title='SSA vs. CRD and PAS', xsize=1000, ysize=1000
  plot, lonbelow, ssa[3,*],title='Comparison between retrieved Single Scattering Albedo,!C and in situ measurements', xtitle='Longitude (degrees)',xrange=[-118.14,-118.10],$
   xticks=4, xtickformat='(F8.3)', ytitle='!9'+string(118B)+'!4', ystyle=8, xmargin=[8,12], ymargin=[8,8]
  oplot, lonbelow, (ext[ind]-abs[ind])/ext[ind], color=20
  oplot, lonbelow, (ext[ind_abv]-abs[ind_abv])/ext[ind_abv], color=40
  oplot, lonbelow, ((ext[ind]-abs[ind])/ext[ind]+(ext[ind_abv]-abs[ind_abv])/ext[ind_abv])/2., color=60
  axis, yaxis=1,ystyle=1, yrange=[100,240],/save,color=70, ytitle='!9'+string(115B)+'!4!Iext!N'
  oplot, lonbelow, ext[ind], color=70
  oplot, lonbelow, ext[ind_abv],color=100
  oplot, lonbelow, (ext[ind]+ext[ind_abv])/2., color=150
  axis, 0.92,yaxis=1, ystyle=1, yrange=[5,20],/save,color=250, ytitle='!9'+string(115B)+'!4!Iabs!N',/normal
  oplot, lonbelow, abs[ind], color=200
  oplot, lonbelow, abs[ind_abv],color=220
  oplot, lonbelow, (abs[ind]+abs[ind_abv])/2., color=250
  
legend, ['Retrieved SSA','CRD C_Ext - Below','CRD C_Ext - Above', 'CRD C_ext - Mean','PAS C_abs - Below','PAS C_abs - Above','PAS C_abs - Mean'], box=0, textcolors=[0,70,100,150,200,220,250],/bottom
stop
endif else begin

set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated
 device, /tt_font, set_font='Helvetica Bold'
 device, filename=dir+'rtm_crd_time.ps'
 device,/color,bits_per_pixel=8.
 device, xsize=40, ysize=40
      !p.font=1 & !p.thick=8 & !p.charsize=3.8 & !x.style=1
      !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  plot, lonbelow, ssa[3,*],title='Comparison between retrieved Single Scattering Albedo,!C and in situ measurements', xtitle='Longitude (degrees)',xrange=[-118.14,-118.10],$
   xticks=4, xtickformat='(F8.3)', ytitle='!9'+string(118B)+'!3', ystyle=8, xmargin=[8,8], ymargin=[8,8]
  axis, yaxis=1,ystyle=1, yrange=[100,250],/save,color=70, ytitle='!9'+string(115B)+'!3!Iext!N'
  oplot, lonbelow, ext[ind], color=70
legend, ['Retrieved Single Scattering Albedo','In situ Extinction Coefficient'], box=0, textcolors=[0,70],/bottom

device, /close
spawn, 'convert "'+dir+'rtm_crd_time.ps" "'+dir+'rtm_crd_time.png"'
spawn, 'rm -f "'+dir+'rtm_crd_time.ps"
endelse


end