
pro calnex_cal_plot

restore, '/home/leblanc/CALNEX/cal.out'

set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated
 device, /tt_font, set_font='Helvetica Bold'
 device, filename='/home/leblanc/CALNEX/cal.ps'
 device,/color,bits_per_pixel=8.
 device, xsize=25, ysize=20
      !p.color=0
      !p.font=1
      !p.thick=5
      !p.charsize=3.5
      !x.style=0
      !y.style=1 
      !z.style=1
      !y.thick=1.8
      !x.thick=1.8
      !p.multi=0 ;[0,1,2]
if 0 then begin
      ymax = max([max(resp1(*,0)),max(resp1(*,1)),max(resp2_arr(*,0,*)),max(resp2_arr(*,1,*))])*1.04
   
    plot, wlvisz,resp1(*,0),color=0,ytitle='counts/ms W!E-1!N m!E2!N nm',xtitle='Wavelength (nm)',title='Zenith', $   
     charsize=1.5,thick=1.8,/xs,/ys,xrange=[300,2200],yrange=[0,ymax] 
    oplot,wlnirz,resp1[*,1],thick=1.8,color=0, linestyle=0
endif
    
    legend_tit= [primary_cal_date]
    color_s=findgen(num_secondary)*230/num_secondary+25
    
    for i=0,num_secondary-1 do begin
;      oplot,wlvisz,resp2_arr(*,0,i),thick=1.8,color=color_s[i]
;      oplot,wlnirz,resp2_arr(*,1,i),thick=1.8,color=color_s[i]
      legend_tit=[legend_tit,secondary_cal_date[i]]
    endfor
    
;    legend,legend_tit[2:*],textcolors=color_s[1:*],/right,outline_color=0, charsize=2.5

    plot, wlvisz,resp2_arr(*,0,ref),color=0,ytitle='%',xtitle='Wavelength (nm)',title='Percent Difference of Zenith response function', $   
     charsize=1.5,thick=1.8,/xs,/ys,xrange=[300,2200],yrange=[-7,7], /nodata, yticklen=1.0, ygridstyle=1

    for i=ref, num_secondary-1 do begin
      oplot,wlvisz,(resp2_arr(*,0,i)-resp2_arr(*,0,ref))*100./resp2_arr(*,0,ref),thick=1.8,color=color_s[i]
      oplot,wlnirz,(resp2_arr(*,1,i)-resp2_arr(*,1,ref))*100./resp2_arr(*,1,ref),thick=1.8,color=color_s[i]
    endfor

legend,legend_tit[2:*],textcolors=color_s[1:*],/right, charsize=1.8, box=0, position=[0.88,0.94],/normal
device, /close

spawn, 'convert "/home/leblanc/CALNEX/cal.ps" "/home/leblanc/CALNEX/cal.png"'
spawn, 'rm -f /home/leblanc/CALNEX/cal.ps'


end
