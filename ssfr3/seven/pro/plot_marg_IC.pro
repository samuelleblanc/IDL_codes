; program to plot the marginal IC calculated from the model limp2 retrieval 
; ..


pro plot_marg_IC

win=0
su=1
xl=0
sect=0

if xl then nl='' else nl='_nolog'

if su then begin
  sub='_sub'+nl
  fls=indgen(100)
  xr=[1,100]
endif else begin
  sub=''+nl
  fls=indgen(200)
  xr=[1,200]
endelse


;; load the file and set global variables
if win then fn='C:\Users\Samuel\Research\SSFR3\data\retrieved_pdf_20120523_model_limp.out' else $
fn='/argus/roof/SSFR3/retrieved/retrieved_pdf_20120523_model_limp2_v2.out'
;fn='/home/leblanc/SSFR3/data/retrieved_pdf_20120523_model_limp_v2.out'
restore, fn

if win then dir='C:\Users\Samuel\Research\SSFR3\plots\' else $
dir='/home/leblanc/SSFR3/plots/'

ind=intarr(24,15,2)
for t=0,23 do for r=0,14 do for w=0,1 do ind[t,r,w]=360*w+t*15+r

H=fltarr(24,15,2)   ; SIC for joint posterior pdf
H_ice=fltarr(24,15) ; SIC for joint post pdf of tau and ref for ice
H_liq=fltarr(24,15) ; SIC for joint post pdf of tau and ref for liquid
 
H_tau=fltarr(24,15,2)  ; SIC for marginal of tau
H_ref=fltarr(24,15,2)  ; SIC for marginal of ref

prob_ic=fltarr(24,15,2)
prob_li=fltarr(24,15,2)

for t=0,23 do begin
  for r=0,14 do begin
    for w=0,1 do begin
      p=reform(post[ind[t,r,w],fls,*,*])
      prob_ic[t,r,w]=total(p[*,*,1])
      prob_li[t,r,w]=total(p[*,*,0])
      p=p/total(p)
      h[t,r,w]=SIC(p)
      

      pt=total(total(p,3),2)
      h_tau[t,r,w]=sic(pt/total(pt))

      pr=total(total(p,3),1)
      h_ref[t,r,w]=sic(pr/total(pr))
    endfor
    pi=reform(post[ind[t,r,1],fls,*,1])
    pl=reform(post[ind[t,r,0],fls,*,0])
    h_ice[t,r]=sic(pi/total(pi))
    h_liq[t,r]=sic(pl/total(pl))
  endfor
  print, t
endfor

;; now make the plots

fp=dir+'IC_contour_v3'+sub
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=40, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=2.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,2,1] & !x.margin=[6,6] & !y.margin=[3,3]

  mus='!9m!X'
  lvls=findgen(30)/29.+0.001

  contour, h[0:21,*,0],tau[0:21],ref,levels=lvls,/cell_fill,title='Liquid water cloud',xtitle='Optical thickness',$
   ytitle='Effective radius ('+mus+'m)',xlog=xl, xrange=xr

if sect then begin
plots, [1.2,48.,48.,1.2,1.2],[3.,3.,49.,49.,3.]
plots, [52.,198.,198.,52.,52.],[26.,26.,49.,49.,26.]
plots, [52.,198.,198.,52.,52.],[3.,3.,24.,24.,3.]
xyouts, 10.,25.,'A',alignment=0.5
xyouts, 100.,37.5,'B',alignment=0.5
xyouts, 100.,12.5,'C',alignment=0.5
endif

contour, h[0:21,*,1],tau[0:21], ref, levels=lvls,/cell_fill,title='Ice water cloud',xtitle='Optical thickness',$
 ytitle='Effective radius ('+mus+'m)',xmargin=[3,9],xlog=xl,xrange=xr

if sect then begin
plots, [1.2,48.,48.,1.2,1.2],[3.,3.,49.,49.,3.]
plots, [52.,198.,198.,52.,52.],[26.,26.,49.,49.,26.]
plots, [52.,198.,198.,52.,52.],[3.,3.,24.,24.,3.]
xyouts, 10.,25.,'A',alignment=0.5
xyouts, 100.,37.5,'B',alignment=0.5
xyouts, 100.,12.5,'C',alignment=0.5
endif

 contour,transpose([[lvls],[lvls]]),[0,1],lvls,levels=lvls,/cell_fill,ystyle=9,yticks=10,$
  ytickname=replicate(' ',11),position=[0.9,0.1,0.92,0.9],/normal,/noerase,xticks=1,xtickname=[' ',' ']
 axis,yaxis=1,ytitle='IC',yrange=[0,1],yticks=10

 device, /close
 spawn, 'convert "'+fp+'.ps" "'+fp+'.png"'

fp=dir+'IC_contour_ice_liq_v3'+sub
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=40, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=2.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,2,1] & !x.margin=[6,6] & !y.margin=[3,3]

  mus='!9m!X'
  lvls=findgen(30)/29.+0.001

  contour, h_liq[0:21,*],tau[0:21],ref,levels=lvls,/cell_fill,title='Liquid water cloud',xtitle='Optical thickness',$
   ytitle='Effective radius ('+mus+'m)',xlog=xl, xrange=xr

if sect then begin
plots, [1.2,48.,48.,1.2,1.2],[3.,3.,49.,49.,3.]
plots, [52.,198.,198.,52.,52.],[26.,26.,49.,49.,26.]
plots, [52.,198.,198.,52.,52.],[3.,3.,24.,24.,3.]
xyouts, 10.,25.,'A',alignment=0.5
xyouts, 100.,37.5,'B',alignment=0.5
xyouts, 100.,12.5,'C',alignment=0.5
endif

contour, h_ice[0:21,*],tau[0:21], ref, levels=lvls,/cell_fill,title='Ice water cloud',xtitle='Optical thickness',$
 ytitle='Effective radius ('+mus+'m)',xmargin=[3,9],xlog=xl,xrange=xr

if sect then begin
plots, [1.2,48.,48.,1.2,1.2],[3.,3.,49.,49.,3.]
plots, [52.,198.,198.,52.,52.],[26.,26.,49.,49.,26.]
plots, [52.,198.,198.,52.,52.],[3.,3.,24.,24.,3.]
xyouts, 10.,25.,'A',alignment=0.5
xyouts, 100.,37.5,'B',alignment=0.5
xyouts, 100.,12.5,'C',alignment=0.5
endif

 contour,transpose([[lvls],[lvls]]),[0,1],lvls,levels=lvls,/cell_fill,ystyle=9,yticks=10,$
  ytickname=replicate(' ',11),position=[0.9,0.1,0.92,0.9],/normal,/noerase,xticks=1,xtickname=[' ',' ']
 axis,yaxis=1,ytitle='IC',yrange=[0,1],yticks=10

 device, /close
 spawn, 'convert "'+fp+'.ps" "'+fp+'.png"'

fp=dir+'IC_contour_tau_v3'+sub
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=40, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=2.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,2,1] & !x.margin=[6,6] & !y.margin=[3,3]
  
  mus='!9m!X' 
  lvls=findgen(30)/29.+0.001

  contour, h_tau[0:21,*,0],tau[0:21],ref,levels=lvls,/cell_fill,title='Liquid water cloud',xtitle='Optical thickness',$
   ytitle='Effective radius ('+mus+'m)',xlog=xl, xrange=xr

if sect then begin
plots, [1.2,48.,48.,1.2,1.2],[3.,3.,49.,49.,3.]
plots, [52.,198.,198.,52.,52.],[26.,26.,49.,49.,26.]
plots, [52.,198.,198.,52.,52.],[3.,3.,24.,24.,3.]
xyouts, 10.,25.,'A',alignment=0.5
xyouts, 100.,37.5,'B',alignment=0.5
xyouts, 100.,12.5,'C',alignment=0.5
endif

contour, h_tau[0:21,*,1],tau[0:21], ref, levels=lvls,/cell_fill,title='Ice water cloud',xtitle='Optical thickness',$
 ytitle='Effective radius ('+mus+'m)',xmargin=[3,9],xlog=xl,xrange=xr

if sect then begin
plots, [1.2,48.,48.,1.2,1.2],[3.,3.,49.,49.,3.]
plots, [52.,198.,198.,52.,52.],[26.,26.,49.,49.,26.]
plots, [52.,198.,198.,52.,52.],[3.,3.,24.,24.,3.]
xyouts, 10.,25.,'A',alignment=0.5
xyouts, 100.,37.5,'B',alignment=0.5
xyouts, 100.,12.5,'C',alignment=0.5
endif

 contour,transpose([[lvls],[lvls]]),[0,1],lvls,levels=lvls,/cell_fill,ystyle=9,yticks=10,$
  ytickname=replicate(' ',11),position=[0.9,0.1,0.92,0.9],/normal,/noerase,xticks=1,xtickname=[' ',' ']
 axis,yaxis=1,ytitle='IC',yrange=[0,1],yticks=10

 device, /close
 spawn, 'convert "'+fp+'.ps" "'+fp+'.png"'

fp=dir+'IC_contour_ref_v3'+sub
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=40, ysize=20
  !p.font=1 & !p.thick=5 & !p.charsize=2.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,2,1] & !x.margin=[6,6] & !y.margin=[3,3]
  
  mus='!9m!X' 
  lvls=findgen(30)/29.+0.001
  
  contour, h_ref[0:21,*,0],tau[0:21],ref,levels=lvls,/cell_fill,title='Liquid water cloud',xtitle='Optical thickness',$
   ytitle='Effective radius ('+mus+'m)',xlog=xl, xrange=xr

if sect then begin
plots, [1.2,48.,48.,1.2,1.2],[3.,3.,49.,49.,3.]
plots, [52.,198.,198.,52.,52.],[26.,26.,49.,49.,26.]
plots, [52.,198.,198.,52.,52.],[3.,3.,24.,24.,3.]
xyouts, 10.,25.,'A',alignment=0.5
xyouts, 100.,37.5,'B',alignment=0.5
xyouts, 100.,12.5,'C',alignment=0.5
endif

contour, h_ref[0:21,*,1],tau[0:21], ref, levels=lvls,/cell_fill,title='Ice water cloud',xtitle='Optical thickness',$
 ytitle='Effective radius ('+mus+'m)',xmargin=[3,9],xlog=xl,xrange=xr

if sect then begin
plots, [1.2,48.,48.,1.2,1.2],[3.,3.,49.,49.,3.]
plots, [52.,198.,198.,52.,52.],[26.,26.,49.,49.,26.]
plots, [52.,198.,198.,52.,52.],[3.,3.,24.,24.,3.]
xyouts, 10.,25.,'A',alignment=0.5
xyouts, 100.,37.5,'B',alignment=0.5
xyouts, 100.,12.5,'C',alignment=0.5
endif

 contour,transpose([[lvls],[lvls]]),[0,1],lvls,levels=lvls,/cell_fill,ystyle=9,yticks=10,$
  ytickname=replicate(' ',11),position=[0.9,0.1,0.92,0.9],/normal,/noerase,xticks=1,xtickname=[' ',' ']
 axis,yaxis=1,ytitle='IC',yrange=[0,1],yticks=10

 device, /close
 spawn, 'convert "'+fp+'.ps" "'+fp+'.png"'


fp=dir+'prob_contour'+sub
print, 'making plot :'+fp
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
 device, xsize=40, ysize=40
  !p.font=1 & !p.thick=5 & !p.charsize=2.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
  !p.multi=[0,2,2] & !x.margin=[6,6] & !y.margin=[3,3]

  mus='!9m!X'
  lvls=findgen(30)/29.+0.001

  contour, prob_li[0:21,*,0],tau[0:21],ref,levels=lvls,/cell_fill,title='Liquid water probabilities',xtitle='Optical thickness',$
   ytitle='Effective radius ('+mus+'m)',xlog=xl, xrange=xr

if sect then begin
plots, [1.2,48.,48.,1.2,1.2],[3.,3.,49.,49.,3.]
plots, [52.,198.,198.,52.,52.],[26.,26.,49.,49.,26.]
plots, [52.,198.,198.,52.,52.],[3.,3.,24.,24.,3.]
xyouts, 10.,25.,'A',alignment=0.5
xyouts, 100.,37.5,'B',alignment=0.5
xyouts, 100.,12.5,'C',alignment=0.5
endif

contour, prob_li[0:21,*,1],tau[0:21], ref, levels=lvls,/cell_fill,title='Ice water probabilities',xtitle='Optical thickness',$
 ytitle='Effective radius ('+mus+'m)',xmargin=[3,9],xlog=xl,xrange=xr

if sect then begin
plots, [1.2,48.,48.,1.2,1.2],[3.,3.,49.,49.,3.]
plots, [52.,198.,198.,52.,52.],[26.,26.,49.,49.,26.]
plots, [52.,198.,198.,52.,52.],[3.,3.,24.,24.,3.]
xyouts, 10.,25.,'A',alignment=0.5
xyouts, 100.,37.5,'B',alignment=0.5
xyouts, 100.,12.5,'C',alignment=0.5
endif

  contour, prob_ic[0:21,*,0],tau[0:21],ref,levels=lvls,/cell_fill,title='Liquid water probabilities',xtitle='Optical thickness',$
   ytitle='Effective radius ('+mus+'m)',xlog=xl, xrange=xr
contour, prob_ic[0:21,*,1],tau[0:21], ref, levels=lvls,/cell_fill,title='Ice water probabilities',xtitle='Optical thickness',$
 ytitle='Effective radius ('+mus+'m)',xmargin=[3,9],xlog=xl,xrange=xr




 contour,transpose([[lvls],[lvls]]),[0,1],lvls,levels=lvls,/cell_fill,ystyle=9,yticks=10,$
  ytickname=replicate(' ',11),position=[0.9,0.1,0.92,0.9],/normal,/noerase,xticks=1,xtickname=[' ',' ']
 axis,yaxis=1,ytitle='IC',yrange=[0,1],yticks=10

 device, /close
 spawn, 'convert "'+fp+'.ps" "'+fp+'.png"'




stop
end


; make a function to calculate the shannon information content
function SIC, post
  ;calculate the shanon information content
  snorm=alog(n_elements(post))/alog(2.)
  prio_pdf=post*0.+1.
  sprio=(-1.)*total(prio_pdf/total(prio_pdf)*alog(prio_pdf/total(prio_pdf))/alog(2.))
  spost=(-1.)*total(post*alog(post+1.E-42)/alog(2.))
  H=(sprio-spost)/snorm
return,h
end
