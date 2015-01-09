; Program to read in the binary data from the limit switch test

pro read_limit,file

data=dblarr(7)

f=file_search(file)
print, 'opening file:'+f[0]

openr,lun,f[0],/get_lun
readu,lun,data
dat=[[data]]
i=0
while not eof(lun) do begin
  readu,lun,data
  dat=[[dat],[data]]
  i=i+1
endwhile

nul=where(dat eq -999.,m)
if m gt 0 then dat[nul]=!values.f_nan

angle=dat[0,*]
pos=dat[2,*]
time=dat[1,*]
speed=dat[3,*]
move=dat[4,*]
pitch=dat[5,*]
roll=dat[6,*]
close,lun

; let the plotting begin
set_plot, 'ps'
loadct, 39, /silent
device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8.
device, filename='../files/calibration.ps',xsize=20,ysize=20
!p.font=1 & !p.thick=5 & !p.charsize=1.8 & !x.style=1 & !y.style=1 & !z.style=1
!y.thick=1.8 & !x.thick=1.8 & !p.multi=0 & !y.omargin=[0,0] & !x.omargin=[0,4]

plot, angle, pos, title='Calibration curve with inclinometer',xtitle='Angle (degree)',ytitle='counts',ystyle=8,psym=1
l=linfit(angle,pos)
oplot,angle,l[0]+l[1]*angle,linestyle=2,thick=2,color=250
r=correlate(angle,pos)
print, 'linear fit coefficients:',l
print, 'correlation coefficient',r

axis,yaxis=1,ystyle=1,yrange=[-10,10],/save,ytitle='Mini-INS pitch'
oplot,angle,-pitch,psym=7,color=150
li=linfit(angle,-pitch)
ri=correlate(angle,-pitch)
print, 'linear fit coefficients for mini-INS:',li
print, 'correlation coeeficient for mini-INS:',ri

legend,['Motor Position','linear fit','Mini-INS pitch'],textcolors=[0,250,150],box=0
legend,['y='+string(l[0],format='(F8.1)')+' + '+string(l[1],format='(F8.1)')+'x','R='+string(r,format='(F8.6)'),$
        'y='+string(li[0],format='(F8.5)')+' + '+string(li[1],format='(F8.5)')+'x','R='+string(ri,format='(F8.6)')],$
        textcolors=[250,250,150,150],box=0,/right,/bottom

device, /close
spawn,'convert ../files/calibration.ps ../files/calibrations_2.png'
spawn,'rm -f ../files/calibration.ps'

stop
end
