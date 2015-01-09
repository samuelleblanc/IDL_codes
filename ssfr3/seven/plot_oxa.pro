; program to plot the oxygen-a band integrated area for times of homogeneously cloudy and hetergeneously cloudy and homogeneously clear
; uses output of the plot_cloud_rad program (date_wv.out files)

pro plot_oxa

dir='/home/leblanc/SSFR3/data/'

fn=dir+'oxa_cloudy_clear'
set_plot,'ps'
print, 'making plot :'+fn
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fn+'.ps'
 device, xsize=20, ysize=20
 !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
 !p.multi=0 & !x.margin=[7,2]

print, 'restoring cloudy'
restore, dir+'20120912/20120912_wv.out'
fla=where(tmhrs gt 14.5 and tmhrs lt 23.5)
plot,nirroxa[fla],nradoxa[fla],psym=3,yrange=[0,10],xrange=[0,45], title='Oxygen-a band transmitance integrated area',xtitle='From irradiance',ytitle='From radiance'
lina=linfit(nirroxa[fla],nradoxa[fla])
tvlct,200,200,200,200
x=findgen(45)
oplot, x,lina[0]+lina[1]*x,linestyle=2,color=200
anirroxa=nirroxa[fla]
anradoxa=nradoxa[fla]


print, 'restoring clear'
restore, dir+'20120909/20120909_wv.out'
flb=where(tmhrs gt 14.5 and tmhrs lt 23.5)
oplot, nirroxa[flb],nradoxa[flb],psym=3,color=70
tvlct,100,100,254,71
linb=linfit(nirroxa[flb],nradoxa[flb])
oplot, x,linb[0]+linb[1]*x,linestyle=2,color=71
bnirroxa=nirroxa[flb]
bnradoxa=nradoxa[flb]

print, 'restoring heterogeneous'
restore, dir+'20120926/20120926_wv.out'
flc=where(tmhrs gt 14.5 and tmhrs lt 23.5)
oplot, nirroxa[flc],nradoxa[flc],psym=3,color=250
tvlct,254,100,100,251
linc=linfit(nirroxa[flc],nradoxa[flc])
oplot, x,linc[0]+linc[1]*x,linestyle=2,color=251
cnirroxa=nirroxa[flc]
cnradoxa=nradoxa[flc]


legend,['Homogeneous Clouds','Heterogeneous Clouds','Clear'],box=0,textcolors=[0,250,70]

device,/close
spawn, 'convert '+fn+'.ps '+fn+'.png'
stop
end
