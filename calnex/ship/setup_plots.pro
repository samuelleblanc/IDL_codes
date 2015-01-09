pro setup_plots, plotToFile, filename


if (plotToFile eq 0) then begin
    set_plot, 'x'
    device,decomposed=0
    device,retain=2
    loadct, 27
endif else begin
    set_plot, 'ps'
    device,/color,filename=filename
    device,/inches,xsize=7.5,ysize=7.5,yoffset=2.5,/times
endelse

!P.charsize=1.35
!x.charsize = 1.35
!y.charsize = 1.35
!P.thick=1.
!p.charthick = 1.
!x.thick = 1.5
!y.thick = 1.5

end
