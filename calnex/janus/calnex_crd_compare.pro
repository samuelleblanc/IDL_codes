; program to compare crd, psap to retrieved aerosol values. on 20100519
@calnex_crd.pro
@read_aeronet.pro

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
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated
 device, /tt_font, set_font='Helvetica Bold'
 device, filename=dir+'rtm_crd_time.ps'
 device,/color,bits_per_pixel=8.
 device, xsize=40, ysize=20

;window, 0, title='SSA retrieved'
;plot, utcbelow, ssa[3,*]
;window, 1, title='CRD ext'
;plot, utcbelow, ext[ind]
;window, 2, title='PAS abs'
;plot, utcbelow, abs[ind]
; loadct, 39, /silent
; device, decomposed=0
      !p.font=1 & !p.thick=5 & !p.charsize=1.8 & !x.style=1
      !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8 &!p.multi=[0,2,1] & !y.omargin=[0,0]
;window, 0, title='SSA vs. CRD and PAS', xsize=1000, ysize=1000
  plot, lonbelow, ssa[3,*],title='Comparison between retrieved Single Scattering Albedo,!C and in situ measurements', xtitle='Longitude (degrees)',xrange=[-118.14,-118.10],$
   xticks=4, xtickformat='(F8.3)', ytitle='!9'+string(118B)+'!4', ystyle=8, xmargin=[6,10], ymargin=[4,4]
   
   ;in situ ssa
  oplot, lonbelow, (ext[ind]-abs[ind])/ext[ind], color=70, linestyle=1
  oplot, lonbelow, (ext[ind_abv]-abs[ind_abv])/ext[ind_abv], color=70, linestyle=2
  oplot, lonbelow, ((ext[ind]-abs[ind])/ext[ind]+(ext[ind_abv]-abs[ind_abv])/ext[ind_abv])/2., color=70, linestyle=5
  oplot, lonbelow, ((ext_ambient[ind]-abs[ind])/ext_ambient[ind]+(ext_ambient[ind_abv]-abs[ind_abv])/ext_ambient[ind_abv])/2., color=70, linestyle=0
  
   ;crd ext
  axis, yaxis=1,ystyle=1, yrange=[120,340],/save,color=150, ytitle='!9'+string(115B)+'!4!Iext!N'
  oplot, lonbelow, ext[ind], color=150, linestyle=1
  oplot, lonbelow, ext[ind_abv],color=150, linestyle=2
  oplot, lonbelow, (ext[ind]+ext[ind_abv])/2., color=150, linestyle=5
  oplot, lonbelow, (ext_ambient[ind]+ext_ambient[ind_abv])/2., color=150, linestyle=0
  
  ; pas abs
  axis, 0.45,yaxis=1, ystyle=1, yrange=[2,15],/save,color=250, ytitle='!9'+string(115B)+'!4!Iabs!N',/normal
  oplot, lonbelow, abs[ind], color=250, linestyle=1
  oplot, lonbelow, abs[ind_abv],color=250, linestyle=2
  oplot, lonbelow, (abs[ind]+abs[ind_abv])/2., color=250, linestyle=5
  
  legend, ['Below','Above','Mean','Mean - Humidified'], linestyle=[1,2,5,0], box=0, /bottom
  legend, ['Retrieved SSA','in situ SSA', 'CRD C_ext', 'PAS C_abs'], textcolors=[0,70,150,250],box=0, /right, /bottom
  
  hi_ssa=histogram(ssa[3,*], nbins=50, max=1.0, min=0.0)
  hi_ins=histogram(((ext_ambient[ind]-abs[ind])/ext_ambient[ind]+(ext_ambient[ind_abv]-abs[ind_abv])/ext_ambient[ind_abv])/2., nbins=50, max=1.0, min=0.0)
  read_aeronet, '/home/leblanc/CALNEX/aeronet/100516_100519_CalTech',j_day, j_day_almu, wvl_aod,wvl_almu, aero_aod,aero_ssa, aero_sza, asy_TT, aot2_T, up_flux_T, down_flux_T, diff_flux_T, forcing, aero_albedo
  hi_aero=histogram(aero_ssa[where(j_day_almu gt 138.)] , nbins=50, max=1.0, min=0.0)
  
  plot, findgen(50)*0.02-0.01, hi_ssa/total(hi_ssa), title='Distribution of single scattering albedo', ytitle='Normalized count', $
   xtitle='Single scattering albedo',/ylog,psym=10,thick=3, yrange=[0.01,1], xrange=[0.5,1.0], xmargin=[6,6], ymargin=[4,4]
  oplot, findgen(50)*0.02-0.01, hi_ins/total(hi_ins), color=250, psym=10
  oplot, findgen(50)*0.02-0.01, hi_aero/total(hi_aero), color=130, psym=10
  
  legend, ['Retrieved', 'In situ - humidified', 'Aeronet'], textcolors=[0,250,130], box=0
  
  
device, /close
spawn, 'convert "'+dir+'rtm_crd_time.ps" "'+dir+'rtm_crd_time_comps.png"'
spawn, 'rm -f "'+dir+'rtm_crd_time.ps"
stop
endif else begin

set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated
 device, /tt_font, set_font='Helvetica Bold'
 device, filename=dir+'rtm_crd_time.ps'
 device,/color,bits_per_pixel=8.
 device, xsize=20, ysize=20
      !p.font=1 & !p.thick=5 & !p.charsize=1.8 & !x.style=1
      !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8 &!p.multi=0 & !y.omargin=[0,5]
  plot, lonbelow, ssa[3,*],title='Along-flight path single scattering albedo at 500 nm', xtitle='Longitude (degrees)',xrange=[-118.14,-118.10],$
   xticks=4, xtickformat='(F8.3)', ystyle=8, xmargin=[8,8], ymargin=[4,0], psym=-1;, ytitle='!9'+string(118B)+'!4'
  axis, yaxis=1,ystyle=1, yrange=[100,250],/save,color=70;, ytitle='!9'+string(115B)+'!4!Iext!N'
  xyouts, 0.09, 0.5,'!9'+string(118B)+'!4', orientation=90,/normal, charsize=3.5
  xyouts, 0.94, 0.5, '!9'+string(115B)+'!4!Iext!N', orientation=90, /normal, charsize=3.5, color=70
  oplot, lonbelow, ext[ind], color=70, psym=-1
;legend, ['Retrieved Single Scattering Albedo','In situ Extinction Coefficient'], box=0, textcolors=[0,70],/bottom
  xyouts, lonbelow[57],217,'Retrieved single scattering albedo', charsize=1.5
  xyouts, lonbelow[55],190,'In-situ extinction coefficient', charsize=1.5, color=70

device, /close
spawn, 'convert "'+dir+'rtm_crd_time.ps" "'+dir+'rtm_crd_time.png"'
spawn, 'rm -f "'+dir+'rtm_crd_time.ps"
endelse


end
