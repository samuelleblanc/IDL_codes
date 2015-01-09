; program to view the parameters determined for different sza

pro view_pars,date,lb

dir='C:\Users\Samuel\Research\SSFR3\'
restore, dir+'data\sp_par_'+lb+'_v1_'+date+'.out'

set_plot, 'win'
loadct, 39
device, decomposed=0
!y.style=1

case lb of
'sza':mu=mu
'z':mu=z
'pw':mu=pw
'ab':mu=ab
endcase


nu=n_elements(mu)
ir=[0,20,30,49]
cr=[255,50,150,250]


window, 0, xsize=900,ysize=800
!p.multi=[0,4,4]
for p=0, 15 do begin


yr=[min(pars[*,*,*,p,*]),max(pars[*,*,*,p,*])]
if p eq 3 then yr=[0.5,1.5]
plot, taus, pars[*,0,0,p,0],xtitle='tau',yr=yr
for u=0,nu-1 do begin

for i=0,3 do oplot, taus, pars[*,ir[i],0,p,u],color=cr[i],thick=u/2+1
for i=0,3 do oplot, taus, pars[*,ir[i],1,p,u],color=cr[i],thick=u/2+1,linestyle=1

endfor
endfor
;plot,findgen(10),/nodata
legend,string(mu),/right,box=0,thick=findgen(nu)/2+1


window,1,xsize=1200
!p.multi=[0,3,1]
;for u=0,nu-1 do begin
wvl=zenlambda
yr=[0,max(sp_hiu[0,*,*,*,*])]
plot, wvl,sp_hiu[0,0,0,*,0],ytitle='rad',xtitle='wavelength',title='tau=1',yr=yr
for u=0,nu-1 do begin
  for i=0,3 do oplot,wvl,sp_hiu[0,ir[i],0,*,u],thick=u/2+1,color=cr[i]
  for i=0,3 do oplot,wvl,sp_hiu[0,ir[i],1,*,u],thick=u/2+1,color=cr[i],linestyle=1
endfor
yr=[0,max(sp_hiu[10,*,*,*,*])]
plot, wvl,sp_hiu[10,0,0,*,0],ytitle='rad',xtitle='wavelength',title='tau=11' ,yr=yr
for u=0,nu-1 do begin 
  for i=0,3 do oplot,wvl,sp_hiu[10,ir[i],0,*,u],thick=u/2+1,color=cr[i] 
  for i=0,3 do oplot,wvl,sp_hiu[10,ir[i],1,*,u],thick=u/2+1,color=cr[i],linestyle=1 
endfor 
yr=[0,max(sp_hiu[20,*,*,*,*])]
plot, wvl,sp_hiu[20,0,0,*,0],ytitle='rad',xtitle='wavelength',title='tau=21' ,yr=yr
for u=0,nu-1 do begin 
  for i=0,3 do oplot,wvl,sp_hiu[20,ir[i],0,*,u],thick=u/2+1,color=cr[i] 
  for i=0,3 do oplot,wvl,sp_hiu[20,ir[i],1,*,u],thick=u/2+1,color=cr[i],linestyle=1 
endfor
legend,string(refs[ir]),textcolors=cr,box=0,/right 
;endfor




stop
end
