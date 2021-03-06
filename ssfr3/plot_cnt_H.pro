; program to plot the contour plots of every information content of every parameter
; 

pro plot_cnt_H

dir='C:\Users\Samuel\Research\SSFR3\data\'
restore, dir+'retrieved_pdf_20120523_model.out'
restore, dir+'model_taus_ref.out'

set_plot,'win'
device, decomposed=0
loadct, 39

!p.multi=[0,5,3]
ind=intarr(24,15,2)
h=fltarr(16,24,15,2)
h_tot=fltarr(24,15,2)
r_per=fltarr(24,15,2)
t_per=fltarr(24,15,2)
for w=0,1 do begin
  for t=0,23 do begin
    for r=0,14 do begin
      ind[t,r,w]=w*360+t*15+r
      h[*,t,r,w]=hi_rtm[*,ind[t,r,w]]
      h_tot[t,r,w]=h_rtm[ind[t,r,w]]
      r_per[t,r,w]=(ref_rtm[ind[t,r,w]]-ref_err[0,ind[t,r,w]])/ref_rtm[ind[t,r,w]]
      t_per[t,r,w]=(tau_rtm[ind[t,r,w]]-tau_err[0,ind[t,r,w]])/tau_rtm[ind[t,r,w]]
    endfor
  endfor
endfor

lvls=findgen(30)/29.
!x.margin=[3,1] & !p.charsize=1.5
window, 0, retain=2, ysize=700,xsize=1500,title='ice'
for p=0, 14 do begin
  contour, h[p,0:21,*,1],tau[0:21],ref,/cell_fill,levels=lvls,title=string(p+1),xtitle='tau',ytitle='ref'  
endfor

window,1,retain=2, ysize=700,xsize=1500, title='liquid'
for p=0,14 do contour, h[p,0:21,*,0],tau[0:21],ref,/cell_fill,levels=lvls,title=string(p+1),xtitle='tau',ytitle='ref'

window, 2, ysize=100,xsize=400
!p.multi=0
contour, [[lvls],[lvls]],lvls,[0,1],/cell_fill,levels=lvls,yticks=1

restore, dir+'retrieved_pdf_20120523_model_limp.out'
t2_per=t_per
r2_per=r_per
hl=fltarr(7,24,15,2)
for w=0,1 do begin
  for t=0,23 do begin
    for r=0,14 do begin
      hl[*,t,r,w]=hm_rtm[*,ind[t,r,w]]
      r2_per[t,r,w]=(ref_rtm[ind[t,r,w]]-ref_err[0,ind[t,r,w]])/ref_rtm[ind[t,r,w]] 
      t2_per[t,r,w]=(tau_rtm[ind[t,r,w]]-tau_err[0,ind[t,r,w]])/tau_rtm[ind[t,r,w]]
    endfor
  endfor
endfor

hcl=hl
hcl_per=hcl
hdif=hl
hdifp=hdif
for p=1,6 do begin
  hcl[p,*,*,*]=total(hl[0:p,*,*,*],1)
  hcl_per[p,*,*,*]=hcl[p,*,*,*]/h_tot*100.
  hdif[p,*,*,*]=hcl[p,*,*,*]-h_tot
  hdifp[p,*,*,*]=hdif[p,*,*,*]/h_tot*100.
endfor

print, mean(hcl_per[1,*,*,*],/nan)
print, max(hdif[6,*,*,*]), max(hdifp[6,*,*,*])
print, max(t_per),max(t2_per)
print, max(r_per),max(r2_per)

window, 3, ysize=700,xsize=1500, title='ice cumulative'
!p.multi=[0,4,2]
for p=0,6 do contour, hcl[p,0:21,*,1],tau[0:21],ref,/cell_fill,levels=lvls,title=string(p+1),xtitle='tau',ytitle='ref'

window, 4, ysize=700,xsize=1500, title='liquid cumulative'
for p=0,6 do contour, hcl[p,0:21,*,0],tau[0:21],ref,/cell_fill,levels=lvls,title=string(p+1),xtitle='tau',ytitle='ref'

stop
end
