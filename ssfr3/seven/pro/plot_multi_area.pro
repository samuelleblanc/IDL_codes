; program to plot the plane parrallel clouds for seeing the relationship between oxa water vapor absorption
; restore multiple different days to see thier effects

@get_pres.pro
@mpfitfun.pro
@legend.pro
@fe.pro
@get_precip_water.pro
@get_ceil.pro
pro plot_multi_area

dir='/home/leblanc/SSFR3/data/'

dx='_2'
fwat=1

restore, dir+'20120602/20120602_cod_comp'+dx+'.out'
if fwat then get_precip_water,tmhrs, '20120602',watera else watera=tmhrs*0.+1.
get_ceil, tmhrs, '20120602',basea,valid=valida
get_pres, tmhrs, '20120602',presa 
areaa=v2[*,0];/watera ;area
area2a=v2[*,1];/watera ;area2
oxaa=v2[*,2];oxa
taua=tau
tmhrsa=tmhrs
disda=disd
area3a=area2v2
refa=ref
daya=tmhrs*0+20120602

restore, dir+'20120813/20120813_cod_comp'+dx+'.out'
if fwat then get_precip_water,tmhrs, '20120813',waterb else waterb=tmhrs*0.+1.
get_ceil, tmhrs, '20120813',baseb,valid=validb
get_pres, tmhrs, '20120813',presb
areab=v2[*,0];/waterb
area2b=v2[*,1];/waterb
oxab=v2[*,2]
taub=tau
refb=ref
tmhrsb=tmhrs
disdb=disd
area3b=area2v2
dayb=tmhrs*0+20120813

restore, dir+'20120525/20120525_cod_comp'+dx+'.out'
if fwat then get_precip_water,tmhrs, '20120525',waterc else waterc=tmhrs*0.+1.
get_ceil, tmhrs, '20120525',basec, valid=validc
get_pres, tmhrs, '20120525',presc
areac=v2[*,0];/waterc
area2c=v2[*,1];/waterc
oxac=v2[*,2]
tauc=tau
refc=ref
tmhrsc=tmhrs
disdc=disd
area3c=area2v2
dayc=tmhrs*0+20120525

restore, dir+'20120523/20120523_cod_comp'+dx+'.out'
if fwat then get_precip_water,tmhrs, '20120523',waterd else waterd=tmhrs*0.+1.
get_ceil, tmhrs, '20120523',based,valid=validd
get_pres, tmhrs, '20120523',presd
aread=v2[*,0];/waterd
area2d=v2[*,1];/waterd
oxad=v2[*,2]
taud=tau
refd=ref
tmhrsd=tmhrs
disdd=disd
area3d=area2v2
dayd=tmhrs*0+20120523

restore, dir+'20120912/20120912_cod_comp'+dx+'.out'
if fwat then get_precip_water,tmhrs, '20120912',watere else watere=tmhrs*0.+1.
get_ceil, tmhrs, '20120912',basee, valid=valide
get_pres, tmhrs, '20120912',prese
areae=v2[*,0];/watere
area2e=v2[*,1];/watere
oxae=v2[*,2]
taue=tau
refe=ref
tmhrse=tmhrs
disde=disd
area3e=area2v2
daye=tmhrs*0+20120912

restore, dir+'20120806/20120806_cod_comp'+dx+'.out'
if fwat then get_precip_water,tmhrs, '20120806',waterf else waterf=tmhrs*0.+1.
get_ceil, tmhrs, '20120806',basef,valid=validf
get_pres, tmhrs, '20120806',presf
areaf=v2[*,0];/waterf
area2f=v2[*,1];/waterf
oxaf=v2[*,2]
tauf=tau
reff=ref
tmhrsf=tmhrs
disdf=disd
area3f=area2v2
dayf=tmhrs*0+20120806

restore, dir+'20120816/20120816_cod_comp'+dx+'.out'
if fwat then get_precip_water,tmhrs, '20120816',waterg else waterg=tmhrs*0.+1.
get_ceil, tmhrs, '20120816',baseg,valid=validg
get_pres, tmhrs, '20120816',presg
areag=v2[*,0];/waterg
area2g=v2[*,1];/waterg
oxag=v2[*,2]
taug=tau
refg=ref
tmhrsg=tmhrs
disdg=disd
area3g=area2v2
dayg=tmhrs*0+20120816

restore, dir+'20120820/20120820_cod_comp'+dx+'.out'
if fwat then get_precip_water,tmhrs, '20120820',waterh else waterh=tmhrs*0.+1.
get_ceil, tmhrs, '20120820',baseh,valid=validh
get_pres, tmhrs, '20120820',presh
areah=v2[*,0];/waterh
area2h=v2[*,1];/waterh
oxah=v2[*,2]
tauh=tau
refh=ref
tmhrsh=tmhrs
disdh=disd
area3h=area2v2
dayh=tmhrs*0+20120820

restore, dir+'20120824/20120824_cod_comp'+dx+'.out'
if fwat then get_precip_water,tmhrs, '20120824',wateri else wateri=tmhrs*0.+1.
get_ceil, tmhrs, '20120824',basei,valid=validi
get_pres, tmhrs, '20120824',presi
areai=v2[*,0];/wateri
area2i=v2[*,1];/wateri
oxai=v2[*,2]
taui=tau
refi=ref
tmhrsi=tmhrs
disdi=disd
area3i=area2v2
dayi=tmhrs*0+20120824

if fwat then w='_wat' else w='' ;'
fn=dir+'area_comp'+dx+w
print, 'plotting:'+fn
 set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold'
 device, filename=fn+'.ps'
 device,/color,bits_per_pixel=8., xsize=20, ysize=50
 !p.font=1 & !p.thick=5 & !p.charsize=4.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
 !p.multi=[0,1,4] & !x.margin=[6,3]

; set the proper homogeneously cloudy regions to plot
if dx eq '_2' or fwat then begin
  a=where(tmhrsa gt 21.  and tmhrsa lt 23.);  and area2a gt 10  and areaa  gt -2)
  b=where(tmhrsb gt 15.  and tmhrsb lt 19.);  and area2b gt 10)
  c=where(tmhrsc gt 15.  and tmhrsc lt 16.);  and tauc   lt 200 and area2c gt 10.)
  d=where(tmhrsd gt 21.  and tmhrsd lt 24.);  and taud   lt 200 and area2d gt 30.)
  e=where(tmhrse gt 15.5 and tmhrse lt 18.5); and area2e gt 40  and area2e lt 100. and areae gt 5)
  f=where(tmhrsf gt 20.  and tmhrsf lt 24.5); and area2f gt 25.)
  g=where(tmhrsg gt 13.5 and tmhrsg lt 17.)
  h=where(tmhrsh gt 16.  and tmhrsh lt 20.);  and area2h gt 10. and areah  lt 4.)
  i=where(tmhrsi gt 19.5 and tmhrsi lt 23.);  and area2i gt 25)
  if fwat then begin
    if dx eq '_2' then begin
      xr1=[25,90] & xr2=[25,130] & xr3=[3,6] & xr4=[2.,7.]
    endif else begin
      xr1=[-1,1.5] & xr2=[0,3] & xr3=[3,8] & xr4=[2.,10.]
    endelse
  endif else begin 
    xr1=[25,90] & xr2=[25,130] & xr3=[3,6] & xr4=[2.,7.]
  endelse
endif else begin
  a=where(tmhrsa gt 21.  and tmhrsa lt 23.  and area2a gt 10  and areaa  gt -2)
  b=where(tmhrsb gt 15.  and tmhrsb lt 19.  and area2b gt 10)
  c=where(tmhrsc gt 15.  and tmhrsc lt 16.  and tauc   lt 200 and area2c gt 10.)
  d=where(tmhrsd gt 21.  and tmhrsd lt 24.  and taud   lt 200 and area2d gt 30.)
  e=where(tmhrse gt 15.5 and tmhrse lt 18.5 and area2e gt 40  and area2e lt 100. and areae gt 5)
  f=where(tmhrsf gt 20.  and tmhrsf lt 24.5 and area2f gt 25.)
  g=where(tmhrsg gt 13.5 and tmhrsg lt 17.)
  h=where(tmhrsh gt 16.  and tmhrsh lt 20.  and area2h gt 10. and areah  lt 4.)
  i=where(tmhrsi gt 19.5 and tmhrsi lt 23.  and area2i gt 25)
  xr1=[-5,20]
  xr2=[25,60]
  xr3=[3,8]
endelse

;set new variables of only the good times
totarea =[areaa[a],areab[b],areac[c],aread[d],areae[e],areaf[f],areag[g],areah[h],areai[i]]
totarea2=[area2a[a],area2b[b],area2c[c],area2d[d],area2e[e],area2f[f],area2g[g],area2h[h],area2i[i]]
totarea3=[area3a[a],area3b[b],area3c[c],area3d[d],area3e[e],area3f[f],area3g[g],area3h[h],area3i[i]]
tottau  =[taua[a],taub[b],tauc[c],taud[d],taue[e],tauf[f],taug[g],tauh[h],taui[i]]
totpres =[presa[a],presb[b],presc[c],presd[d],prese[e],presf[f],presg[g],presh[h],presi[i]]
totwater=[watera[a],waterb[b],waterc[c],waterd[d],watere[e],waterf[f],waterg[g],waterh[h],wateri[i]]
totoxa  =[oxaa[a],oxab[b],oxac[c],oxad[d],oxae[e],oxaf[f],oxag[g],oxah[h],oxai[i]]
tottmhrs=[tmhrsa[a],tmhrsb[b],tmhrsc[c],tmhrsd[d],tmhrse[e],tmhrsf[f],tmhrsg[g],tmhrsh[h],tmhrsi[i]]
totbase =[basea[a],baseb[b],basec[c],based[d],basee[e],basef[f],baseg[g],baseh[h],basei[i]]
totday  =[daya[a],dayb[b],dayc[c],dayd[d],daye[e],dayf[f],dayg[g],dayh[h],dayi[i]]
totref  =[refa[a],refb[b],refc[c],refd[d],refe[e],reff[f],refg[g],refh[h],refi[i]]
totvalid=[valida[a],validb[b],validc[c],validd[d],valide[e],validf[f],validg[g],validh[h],validi[i]]
save, totarea,totarea2,totarea3,tottau,totpres,totwater,totoxa,tottmhrs,totbase,totday,totref,totvalid,$
      filename='/home/leblanc/SSFR3/cloudy.out'
print, 'saved to :/home/leblanc/SSFR3/cloudy.out'
stop
;now determine the fit to each of the parameters
app=[1.,1.,150.]
parinfo = replicate({fixed:0, limited:[0,0], $
                       limits:[0.D,0.D]}, 3)
parinfo[2].fixed=1
parinfo[1].limited=[0,0]
parinfo[1].limits =[200.,650.]

p1a=mpfitfun('fe',taua[a],areaa[a],err_tau(taua[a]),app,parinfo=parinfo)
p1b=mpfitfun('fe',taub[b],areab[b],err_tau(taub[b]),app,parinfo=parinfo)
p1c=mpfitfun('fe',tauc[c],areac[c],err_tau(tauc[c]),app,parinfo=parinfo)
p1d=mpfitfun('fe',taud[d],aread[d],err_tau(taud[d]),app,parinfo=parinfo)
p1e=mpfitfun('fe',taue[e],areae[e],err_tau(taue[e]),app,parinfo=parinfo)
p1f=mpfitfun('fe',tauf[f],areaf[f],err_tau(tauf[f]),app,parinfo=parinfo)
p1g=mpfitfun('fe',taug[g],areag[g],err_tau(taug[g]),app,parinfo=parinfo)
p1h=mpfitfun('fe',tauh[h],areah[h],err_tau(tauh[h]),app,parinfo=parinfo)
p1i=mpfitfun('fe',taui[i],areai[i],err_tau(taui[i]),app,parinfo=parinfo)

p2a=mpfitfun('fe',taua[a],area2a[a],err_tau(taua[a]),app,parinfo=parinfo)
p2b=mpfitfun('fe',taub[b],area2b[b],err_tau(taub[b]),app,parinfo=parinfo)
p2c=mpfitfun('fe',tauc[c],area2c[c],err_tau(tauc[c]),app,parinfo=parinfo)
p2d=mpfitfun('fe',taud[d],area2d[d],err_tau(taud[d]),app,parinfo=parinfo)
p2e=mpfitfun('fe',taue[e],area2e[e],err_tau(taue[e]),app,parinfo=parinfo)
p2f=mpfitfun('fe',tauf[f],area2f[f],err_tau(tauf[f]),app,parinfo=parinfo)
p2g=mpfitfun('fe',taug[g],area2g[g],err_tau(taug[g]),app,parinfo=parinfo)
p2h=mpfitfun('fe',tauh[h],area2h[h],err_tau(tauh[h]),app,parinfo=parinfo)
p2i=mpfitfun('fe',taui[i],area2i[i],err_tau(taui[i]),app,parinfo=parinfo)
app=[1.,1.,200.]
;app,parinfo=parinfo=[10.,1000.]
p3a=mpfitfun('fe',taua[a],area3a[a],err_tau(taua[a]),app,parinfo=parinfo)
p3b=mpfitfun('fe',taub[b],area3b[b],err_tau(taub[b]),app,parinfo=parinfo)
p3c=mpfitfun('fe',tauc[c],area3c[c],err_tau(tauc[c]),app,parinfo=parinfo)
p3d=mpfitfun('fe',taud[d],area3d[d],err_tau(taud[d]),app,parinfo=parinfo)
p3e=mpfitfun('fe',taue[e],area3e[e],err_tau(taue[e]),app,parinfo=parinfo)
p3f=mpfitfun('fe',tauf[f],area3f[f],err_tau(tauf[f]),app,parinfo=parinfo)
p3g=mpfitfun('fe',taug[g],area3g[g],err_tau(taug[g]),app,parinfo=parinfo)
p3h=mpfitfun('fe',tauh[h],area3h[h],err_tau(tauh[h]),app,parinfo=parinfo)
p3i=mpfitfun('fe',taui[i],area3i[i],err_tau(taui[i]),app,parinfo=parinfo)
;app,parinfo=parinfo=[-3.,120.]
pxa=mpfitfun('fe',taua[a],oxaa[a],err_tau(taua[a]),app,parinfo=parinfo)
pxb=mpfitfun('fe',taub[b],oxab[b],err_tau(taub[b]),app,parinfo=parinfo)
pxc=mpfitfun('fe',tauc[c],oxac[c],err_tau(tauc[c]),app,parinfo=parinfo)
pxd=mpfitfun('fe',taud[d],oxad[d],err_tau(taud[d]),app,parinfo=parinfo)
pxe=mpfitfun('fe',taue[e],oxae[e],err_tau(taue[e]),app,parinfo=parinfo)
pxf=mpfitfun('fe',tauf[f],oxaf[f],err_tau(tauf[f]),app,parinfo=parinfo)
pxg=mpfitfun('fe',taug[g],oxag[g],err_tau(taug[g]),app,parinfo=parinfo)
pxh=mpfitfun('fe',tauh[h],oxah[h],err_tau(tauh[h]),app,parinfo=parinfo)
pxi=mpfitfun('fe',taui[i],oxai[i],err_tau(taui[i]),app,parinfo=parinfo)

r2=fltarr(9,4)
r2[0,0]=1.-1./total((areaa[a]-mean(areaa[a]))^2.,/doub)*total((areaa[a]-fe(taua[a],p1a))^2.,/doub)
r2[1,0]=1.-1./total((areab[b]-mean(areab[b]))^2.,/doub)*total((areab[b]-fe(taub[b],p1b))^2.,/doub)
r2[2,0]=1.-1./total((areac[c]-mean(areac[c]))^2.,/doub)*total((areac[c]-fe(tauc[c],p1c))^2.,/doub)
r2[3,0]=1.-1./total((aread[d]-mean(aread[d]))^2.,/doub)*total((aread[d]-fe(taud[d],p1d))^2.,/doub)
r2[4,0]=1.-1./total((areae[e]-mean(areae[e]))^2.,/doub)*total((areae[e]-fe(taue[e],p1e))^2.,/doub)
r2[5,0]=1.-1./total((areaf[f]-mean(areaf[f]))^2.,/doub)*total((areaf[f]-fe(tauf[f],p1f))^2.,/doub)
r2[6,0]=1.-1./total((areag[g]-mean(areag[g]))^2.,/doub)*total((areag[g]-fe(taug[g],p1g))^2.,/doub)
r2[7,0]=1.-1./total((areah[h]-mean(areah[h]))^2.,/doub)*total((areah[h]-fe(tauh[h],p1h))^2.,/doub)
r2[8,0]=1.-1./total((areai[i]-mean(areai[i]))^2.,/doub)*total((areai[i]-fe(taui[i],p1i))^2.,/doub)
r2[0,1]=1.-1./total((area2a[a]-mean(area2a[a]))^2.,/doub)*total((area2a[a]-fe(taua[a],p2a))^2.,/doub)
r2[1,1]=1.-1./total((area2b[b]-mean(area2b[b]))^2.,/doub)*total((area2b[b]-fe(taub[b],p2b))^2.,/doub)
r2[2,1]=1.-1./total((area2c[c]-mean(area2c[c]))^2.,/doub)*total((area2c[c]-fe(tauc[c],p2c))^2.,/doub)
r2[3,1]=1.-1./total((area2d[d]-mean(area2d[d]))^2.,/doub)*total((area2d[d]-fe(taud[d],p2d))^2.,/doub)
r2[4,1]=1.-1./total((area2e[e]-mean(area2e[e]))^2.,/doub)*total((area2e[e]-fe(taue[e],p2e))^2.,/doub)
r2[5,1]=1.-1./total((area2f[f]-mean(area2f[f]))^2.,/doub)*total((area2f[f]-fe(tauf[f],p2f))^2.,/doub)
r2[6,1]=1.-1./total((area2g[g]-mean(area2g[g]))^2.,/doub)*total((area2g[g]-fe(taug[g],p2g))^2.,/doub)
r2[7,1]=1.-1./total((area2h[h]-mean(area2h[h]))^2.,/doub)*total((area2h[h]-fe(tauh[h],p2h))^2.,/doub)
r2[8,1]=1.-1./total((area2i[i]-mean(area2i[i]))^2.,/doub)*total((area2i[i]-fe(taui[i],p2i))^2.,/doub)
r2[0,2]=1.-1./total((area3a[a]-mean(area3a[a]))^2.,/doub)*total((area3a[a]-fe(taua[a],p3a))^2.,/doub)
r2[1,2]=1.-1./total((area3b[b]-mean(area3b[b]))^2.,/doub)*total((area3b[b]-fe(taub[b],p3b))^2.,/doub)
r2[2,2]=1.-1./total((area3c[c]-mean(area3c[c]))^2.,/doub)*total((area3c[c]-fe(tauc[c],p3c))^2.,/doub)
r2[3,2]=1.-1./total((area3d[d]-mean(area3d[d]))^2.,/doub)*total((area3d[d]-fe(taud[d],p3d))^2.,/doub)
r2[4,2]=1.-1./total((area3e[e]-mean(area3e[e]))^2.,/doub)*total((area3e[e]-fe(taue[e],p3e))^2.,/doub)
r2[5,2]=1.-1./total((area3f[f]-mean(area3f[f]))^2.,/doub)*total((area3f[f]-fe(tauf[f],p3f))^2.,/doub)
r2[6,2]=1.-1./total((area3g[g]-mean(area3g[g]))^2.,/doub)*total((area3g[g]-fe(taug[g],p3g))^2.,/doub)
r2[7,2]=1.-1./total((area3h[h]-mean(area3h[h]))^2.,/doub)*total((area3h[h]-fe(tauh[h],p3h))^2.,/doub)
r2[8,2]=1.-1./total((area3i[i]-mean(area3i[i]))^2.,/doub)*total((area3i[i]-fe(taui[i],p3i))^2.,/doub)
r2[0,3]=1.-1./total((oxaa[a]-mean(oxaa[a]))^2.,/doub)*total((oxaa[a]-fe(taua[a],pxa))^2.,/doub)
r2[1,3]=1.-1./total((oxab[b]-mean(oxab[b]))^2.,/doub)*total((oxab[b]-fe(taub[b],pxb))^2.,/doub)
r2[2,3]=1.-1./total((oxac[c]-mean(oxac[c]))^2.,/doub)*total((oxac[c]-fe(tauc[c],pxc))^2.,/doub)
r2[3,3]=1.-1./total((oxad[d]-mean(oxad[d]))^2.,/doub)*total((oxad[d]-fe(taud[d],pxd))^2.,/doub)
r2[4,3]=1.-1./total((oxae[e]-mean(oxae[e]))^2.,/doub)*total((oxae[e]-fe(taue[e],pxe))^2.,/doub)
r2[5,3]=1.-1./total((oxaf[f]-mean(oxaf[f]))^2.,/doub)*total((oxaf[f]-fe(tauf[f],pxf))^2.,/doub)
r2[6,3]=1.-1./total((oxag[g]-mean(oxag[g]))^2.,/doub)*total((oxag[g]-fe(taug[g],pxg))^2.,/doub)
r2[7,3]=1.-1./total((oxah[h]-mean(oxah[h]))^2.,/doub)*total((oxah[h]-fe(tauh[h],pxh))^2.,/doub)
r2[8,3]=1.-1./total((oxai[i]-mean(oxai[i]))^2.,/doub)*total((oxai[i]-fe(taui[i],pxi))^2.,/doub)

x=findgen(180)
plot, taua[a],areaa[a], psym=3, title='Homogeneous clouds for 940 nm',xtitle='Cloud optical depth',$
 ytitle='Normalized integrated water vapor absorption',xrange=[0,200],yrange=xr1
oplot, taub[b],areab[b],psym=3,color=70
oplot, tauc[c],areac[c],psym=3,color=130
oplot, taud[d],aread[d],psym=3,color=180
oplot, taue[e],areae[e],psym=3,color=210
oplot, tauf[f],areaf[f],psym=3,color=250
oplot, taug[g],areag[g],psym=3,color=100
oplot, tauh[h],areah[h],psym=3,color=30
oplot, taui[i],areai[i],psym=3,color=150

oplot, x, fe(x,p1a),color=0
oplot, x, fe(x,p1b),color=70
oplot, x, fe(x,p1c),color=130
oplot, x, fe(x,p1d),color=180
oplot, x, fe(x,p1e),color=210
oplot, x, fe(x,p1f),color=250
oplot, x, fe(x,p1g),color=100
oplot, x, fe(x,p1h),color=30
oplot, x, fe(x,p1i),color=150

legend, ['20120602','20120813','20120525','20120523','20120912','20120806','20120816','20120820','20120824'],textcolors=[0,70,130,180,210,250,100,30,150],box=0,/right,/bottom,charsize=1.0

plot, taua[a],area2a[a], psym=3, title='Homogeneous clouds for 1150 nm',xtitle='Cloud optical depth',$
 ytitle='Normalized integrated water vapor absorption',xrange=[0,200],yrange=xr2
oplot, taub[b],area2b[b],psym=3,color=70
oplot, tauc[c],area2c[c],psym=3,color=130
oplot, taud[d],area2d[d],psym=3,color=180
oplot, taue[e],area2e[e],psym=3,color=210
oplot, tauf[f],area2f[f],psym=3,color=250
oplot, taug[g],area2g[g],psym=3,color=100
oplot, tauh[h],area2h[h],psym=3,color=30
oplot, taui[i],area2i[i],psym=3,color=150

oplot, x, fe(x,p2a),color=0
oplot, x, fe(x,p2b),color=70
oplot, x, fe(x,p2c),color=130
oplot, x, fe(x,p2d),color=180
oplot, x, fe(x,p2e),color=210
oplot, x, fe(x,p2f),color=250
oplot, x, fe(x,p2g),color=100
oplot, x, fe(x,p2h),color=30
oplot, x, fe(x,p2i),color=150
area2_tot=[area2a[a]];,area2[b]]

legend, ['20120602','20120813','20120525','20120523','20120912','20120806','20120816','20120820','20120824'],textcolors=[0,70,130,180,210,250,100,30,150],box=0,/right,/bottom,charsize=1.0

plot, taua[a],oxaa[a],psym=3,title='Oxygen-a band in homogeneous clouds',xtitle='Cloud optical depth',$
 ytitle='Normalized integrated Oxygen-a absorption',yrange=xr3,xrange=[0,200]
oplot, taub[b],oxab[b], psym=3,color=70
oplot, tauc[c],oxac[c], psym=3,color=130
oplot, taud[d],oxad[d], psym=3,color=180
oplot, taue[e],oxae[e], psym=3,color=210
oplot, tauf[f],oxaf[f], psym=3,color=250
oplot, taug[g],oxag[g], psym=3,color=100
oplot, tauh[h],oxah[h], psym=3,color=30
oplot, taui[i],oxai[i],psym=3,color=150

oplot, x, fe(x,pxa),color=0
oplot, x, fe(x,pxb),color=70
oplot, x, fe(x,pxc),color=130
oplot, x, fe(x,pxd),color=180
oplot, x, fe(x,pxe),color=210
oplot, x, fe(x,pxf),color=250
oplot, x, fe(x,pxg),color=100
oplot, x, fe(x,pxh),color=30
oplot, x, fe(x,pxi),color=150
oxa_tot=[oxaa[a]];,oxa[b]]
legend, ['20120602','20120813','20120525','20120523','20120912','20120806','20120816','20120820','20120824'],textcolors=[0,70,130,180,210,250,100,30,150],box=0,/right,/bottom,charsize=1.0

plot, taua[a],area3a[a], psym=3, title='Homogeneous clouds for 810 nm',xtitle='Cloud optical depth',$
 ytitle='Normalized integrated water vapor absorption',xrange=[0,200],yrange=xr4
oplot, taub[b],area3b[b],psym=3,color=70
oplot, tauc[c],area3c[c],psym=3,color=130
oplot, taud[d],area3d[d],psym=3,color=180
oplot, taue[e],area3e[e],psym=3,color=210
oplot, tauf[f],area3f[f],psym=3,color=250
oplot, taug[g],area3g[g],psym=3,color=100
oplot, tauh[h],area3h[h],psym=3,color=30
oplot, taui[i],area3i[i],psym=3,color=150

oplot, x, fe(x,p3a),color=0
oplot, x, fe(x,p3b),color=70
oplot, x, fe(x,p3c),color=130
oplot, x, fe(x,p3d),color=180
oplot, x, fe(x,p3e),color=210
oplot, x, fe(x,p3f),color=250
oplot, x, fe(x,p3g),color=100
oplot, x, fe(x,p3h),color=30
oplot, x, fe(x,p3i),color=150
area2_tot=[area2a[a]];,area2[b]]

legend, ['20120602','20120813','20120525','20120523','20120912','20120806','20120816','20120820','20120824'],textcolors=[0,70,130,180,210,250,100,30,150],box=0,/right,/bottom,charsize=1.0

device, /close
spawn, 'convert '+fn+'.ps '+fn+'.png'

fn=dir+'offset_wat'+dx+w
print, 'plotting:'+fn
 set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold'
 device, filename=fn+'.ps'
 device,/color,bits_per_pixel=8., xsize=20, ysize=20
 !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
 !p.multi=0 & !x.margin=[6,3]

p1=[p1a[0],p1b[0],p1c[0],p1d[0],p1e[0],p1f[0],p1g[0],p1h[0],p1i[0]]
p2=[p2a[0],p2b[0],p2c[0],p2d[0],p2e[0],p2f[0],p2g[0],p2h[0],p2i[0]]
p3=[p3a[0],p3b[0],p3c[0],p3d[0],p3e[0],p3f[0],p3g[0],p3h[0],p3i[0]]
px=[pxa[0],pxb[0],pxc[0],pxd[0],pxe[0],pxf[0],pxg[0],pxh[0],pxi[0]]

wv=[mean(watera[a]),mean(waterb[b]),mean(waterc[c]),mean(waterd[d]),$
    mean(watere[e]),mean(waterf[f]),mean(waterg[g]),mean(waterh[h]),mean(wateri[i])]

plot, wv, p1, psym=2, title='Fit offset per water vapor',ytitle='Fit offset',xtitle='Mean precipitable water vapor (mm)',xrange=[13,25],yrange=[0,400]
oplot,wv, p2, psym=2, color=70
oplot,wv, p3, psym=2, color=250
oplot,wv, px, psym=2, color=130

legend, ['940 nm band','1150 nm band','810 nm band','Oxygen-a'],textcolors=[0,70,250,130],box=0

device, /close
spawn, 'convert '+fn+'.ps '+fn+'.png'


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; plot all the values oen at a time
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; and colorcode for water content and cloud base height

fn=dir+'area_940nm'
print, 'plotting:'+fn
 set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold'
 device, filename=fn+'.ps'
 device,/color,bits_per_pixel=8., xsize=20, ysize=40
 !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
 !p.multi=[0,1,2] & !x.margin=[6,10]

 plot, tottau, totarea, xrange=[0,200],yrange=[0,100],psym=3,title='Integrated water vapor absorption at 940 nm', $
  xtitle='Cloud optical depth',ytitle='Integrated water vapor absorption'
 for n=0, n_elements(tottau)-1 do oplot, tottau[[n,n]],totarea[[n,n]],psym=3, color=(totwater[n]-14.)*22.

 warr=findgen(112)/10.+14.
 contour, transpose([[warr],[warr]]),[0,1],warr,/cell_fill,nlevels=20,position=[0.85,0.53,0.87,0.97],/normal,$
   /noerase,xticks=1,yticks=1,xtickname=[' ',' '],ytickname=[' ',' ']  ;take this out to omit residual
 axis, yaxis=1,ystyle=1,yrange=[14.,26.75],ytitle='Precipitable water content (mm)',yticks=6

  plot, tottau, totarea, xrange=[0,200],yrange=[0,100],psym=3,title='Coloured by cloud base height', $
  xtitle='Cloud optical depth',ytitle='Integrated water vapor absorption'
 for n=0, n_elements(tottau)-1 do oplot, tottau[[n,n]],totarea[[n,n]],psym=3, color=totbase[n]/30.
 
 contour, transpose([[warr],[warr]]),[0,1],warr,/cell_fill,nlevels=20,position=[0.85,0.07,0.87,0.47],/normal,$
   /noerase,xticks=1,yticks=1,xtickname=[' ',' '],ytickname=[' ',' ']  ;take this out to omit residual
 axis, yaxis=1,ystyle=1,yrange=[0,7650.],ytitle='Cloud base height (m)',yticks=6

device, /close
spawn, 'convert '+fn+'.ps '+fn+'.png'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; for the second area
fn=dir+'area_1150nm'
print, 'plotting:'+fn
 set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold'
 device, filename=fn+'.ps'
 device,/color,bits_per_pixel=8., xsize=20, ysize=40
 !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
 !p.multi=[0,1,2] & !x.margin=[6,10]

 plot, tottau, totarea2, xrange=[0,200],yrange=[0,150],psym=3,title='Integrated water vapor absorption at 1150 nm', $
  xtitle='Cloud optical depth',ytitle='Integrated water vapor absorption'
 for n=0, n_elements(tottau)-1 do oplot, tottau[[n,n]],totarea2[[n,n]],psym=3, color=(totwater[n]-14.)*22.

 warr=findgen(112)/10.+14.
 contour, transpose([[warr],[warr]]),[0,1],warr,/cell_fill,nlevels=20,position=[0.85,0.53,0.87,0.97],/normal,$
  /noerase,xticks=1,yticks=1,xtickname=[' ',' '],ytickname=[' ',' ']  ;take this out to omit residual
 axis, yaxis=1,ystyle=1,yrange=[14.,26.75],ytitle='Precipitable water content (mm)',yticks=6

  plot, tottau, totarea2, xrange=[0,200],yrange=[0,150],psym=3,title='Coloured by cloud base height', $
  xtitle='Cloud optical depth',ytitle='Integrated water vapor absorption'
 for n=0, n_elements(tottau)-1 do oplot, tottau[[n,n]],totarea2[[n,n]],psym=3, color=totbase[n]/30.

 contour, transpose([[warr],[warr]]),[0,1],warr,/cell_fill,nlevels=20,position=[0.85,0.07,0.87,0.47],/normal,$
   /noerase,xticks=1,yticks=1,xtickname=[' ',' '],ytickname=[' ',' ']  ;take this out to omit residual
 axis, yaxis=1,ystyle=1,yrange=[0,7650.],ytitle='Cloud base height (m)',yticks=6

device, /close
spawn, 'convert '+fn+'.ps '+fn+'.png'


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; for the third area
fn=dir+'area_810nm'
print, 'plotting:'+fn
 set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold'
 device, filename=fn+'.ps'
 device,/color,bits_per_pixel=8., xsize=20, ysize=40
 !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
 !p.multi=[0,1,2] & !x.margin=[6,10]

 plot, tottau, totarea3, xrange=[0,200],yrange=[0,15],psym=3,title='Integrated water vapor absorption at 810 nm', $
  xtitle='Cloud optical depth',ytitle='Integrated water vapor absorption'
 for n=0, n_elements(tottau)-1 do oplot, tottau[[n,n]],totarea3[[n,n]],psym=3, color=(totwater[n]-14.)*22.

 warr=findgen(112)/10.+14.
 contour, transpose([[warr],[warr]]),[0,1],warr,/cell_fill,nlevels=20,position=[0.85,0.53,0.87,0.97],/normal,$
  /noerase,xticks=1,yticks=1,xtickname=[' ',' '],ytickname=[' ',' ']  ;take this out to omit residual
 axis, yaxis=1,ystyle=1,yrange=[14.,26.75],ytitle='Precipitable water content (mm)',yticks=6

  plot, tottau, totarea3, xrange=[0,200],yrange=[0,15],psym=3,title='Coloured by cloud base height', $
  xtitle='Cloud optical depth',ytitle='Integrated water vapor absorption'
 for n=0, n_elements(tottau)-1 do oplot, tottau[[n,n]],totarea3[[n,n]],psym=3, color=totbase[n]/30.

 contour, transpose([[warr],[warr]]),[0,1],warr,/cell_fill,nlevels=20,position=[0.85,0.07,0.87,0.47],/normal,$
   /noerase,xticks=1,yticks=1,xtickname=[' ',' '],ytickname=[' ',' ']  ;take this out to omit residual
 axis, yaxis=1,ystyle=1,yrange=[0,7650.],ytitle='Cloud base height (m)',yticks=6

device, /close
spawn, 'convert '+fn+'.ps '+fn+'.png'


;;;;;;;;;;;;;;;;;;;;;;;;;;;; for the oxygen-a band
fn=dir+'area_oxa'
print, 'plotting:'+fn
 set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated, /tt_font, set_font='Helvetica Bold'
 device, filename=fn+'.ps'
 device,/color,bits_per_pixel=8., xsize=20, ysize=40
 !p.font=1 & !p.thick=5 & !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
 !p.multi=[0,1,2] & !x.margin=[6,10]

 plot, tottau, totoxa, xrange=[0,200],yrange=[3,6],psym=3,title='Integrated oxygen-a band absorption', $
  xtitle='Cloud optical depth',ytitle='Integrated oxygen-a band absorption'
 for n=0, n_elements(tottau)-1 do oplot, tottau[[n,n]],totoxa[[n,n]],psym=3, color=(totpres[n]-820.)*12.

 warr=findgen(112)/10.+14.
 contour, transpose([[warr],[warr]]),[0,1],warr,/cell_fill,nlevels=20,position=[0.85,0.53,0.87,0.97],/normal,$ 
  /noerase,xticks=1,yticks=1,xtickname=[' ',' '],ytickname=[' ',' ']  ;take this out to omit residual
 axis, yaxis=1,ystyle=1,yrange=[820.,841.25],ytitle='Surface pressure (hPa)',yticks=6

  plot, tottau, totoxa, xrange=[0,200],yrange=[3,6],psym=3,title='Coloured by cloud base height', $
  xtitle='Cloud optical depth',ytitle='Integrated oxygen-a band absorption'
 for n=0, n_elements(tottau)-1 do oplot, tottau[[n,n]],totoxa[[n,n]],psym=3, color=totbase[n]/30.

 contour, transpose([[warr],[warr]]),[0,1],warr,/cell_fill,nlevels=20,position=[0.85,0.07,0.87,0.47],/normal,$
   /noerase,xticks=1,yticks=1,xtickname=[' ',' '],ytickname=[' ',' ']  ;take this out to omit residual
 axis, yaxis=1,ystyle=1,yrange=[0,7650.],ytitle='Cloud base height (m)',yticks=6

device, /close
spawn, 'convert '+fn+'.ps '+fn+'.png'


set_plot, 'x'
device, decomposed=0
!p.multi=[0,1,2] & !p.thick=1 & !x.margin=[6,3]
loadct, 39
window, 0, retain=2,xsize=400,ysize=900
window, 1, retain=2,xsize=400,ysize=900

num=25.
wi=findgen(num+1)*(max(totwater)-min(totwater))/num+min(totwater)
pi=findgen(num+1)*(max(totpres)-min(totpres))/num+min(totpres)
bi=findgen(num+1)*(max(totbase)-min(totbase))/num+min(totbase)
r=fltarr(num,12)
ll=fltarr(num,2,12)
nul=min(taua[a],aii)
nul=min(taub[b],bi)
nul=min(tauc[c],ci)
nul=min(taud[d],di)
nul=min(taue[e],ei)
nul=min(tauf[f],fi)
nul=min(taug[g],gi)
nul=min(tauh[h],hi)
nul=min(taui[i],ii)

aa=areaa[a]/areaa[a[aii]]
ab=areab[b]/areab[b[bi]]
ac=areac[c]/areac[c[ci]]
ad=aread[d]/aread[d[di]]
ae=areae[e]/areae[e[ei]]
af=areaf[f]/areaf[f[fi]]
ag=areag[g]/areag[g[gi]]
ah=areah[h]/areah[h[hi]]
ai=areai[i]/areai[i[ii]]

as=[aa,ab,ac,ad,ae,af,ag,ah,ai]
window, 2, retain=2
!p.multi=0
plot, tottau,[aa,ab,ac,ad,ae,af,ag,ah,ai],psym=2,yrange=[0,20],title='divided'
stop
!p.multi=[0,1,2]
for n=0, num-1 do begin
  u=where(totwater gt wi[n] and totwater le wi[n+1])
  v=where(totpres gt pi[n] and totpres le pi[n+1])
  y=where(totbase gt bi[n] and totbase le bi[n+1])
  lu=linfit(tottau[u],totarea[u])
  lv=linfit(tottau[v],totarea[v])
  ly=linfit(tottau[y],totarea[y])
  lu2=linfit(tottau[u],totarea2[u])
  lv2=linfit(tottau[v],totarea2[v])
  ly2=linfit(tottau[y],totarea2[y])
  lu3=linfit(tottau[u],totarea3[u])
  lv3=linfit(tottau[v],totarea3[v])
  ly3=linfit(tottau[y],totarea3[y])
  lux=linfit(tottau[u],totoxa[u])
  lvx=linfit(tottau[v],totoxa[v])
  lyx=linfit(tottau[y],totoxa[y])
  r[n,0]=1.-total((totarea[u]-(tottau[u]*lu[1]+lu[0]))^2.)/total((totarea[u]-mean(totarea[u]))^2.)
  r[n,1]=1.-total((totarea[v]-(tottau[v]*lv[1]+lv[0]))^2.)/total((totarea[v]-mean(totarea[v]))^2.)
  r[n,2]=1.-total((totarea[y]-(tottau[y]*ly[1]+ly[0]))^2.)/total((totarea[y]-mean(totarea[y]))^2.)
  r[n,3]=1.-total((totarea[u]-(tottau[u]*lu2[1]+lu2[0]))^2.)/total((totarea[u]-mean(totarea[u]))^2.)
  r[n,4]=1.-total((totarea[v]-(tottau[v]*lv2[1]+lv2[0]))^2.)/total((totarea[v]-mean(totarea[v]))^2.)
  r[n,5]=1.-total((totarea[y]-(tottau[y]*ly2[1]+ly2[0]))^2.)/total((totarea[y]-mean(totarea[y]))^2.)
  r[n,6]=1.-total((totarea[u]-(tottau[u]*lu3[1]+lu3[0]))^2.)/total((totarea[u]-mean(totarea[u]))^2.)
  r[n,7]=1.-total((totarea[v]-(tottau[v]*lv3[1]+lv3[0]))^2.)/total((totarea[v]-mean(totarea[v]))^2.)
  r[n,8]=1.-total((totarea[y]-(tottau[y]*ly3[1]+ly3[0]))^2.)/total((totarea[y]-mean(totarea[y]))^2.)
  r[n,9]=1.-total((totarea[u]-(tottau[u]*lux[1]+lux[0]))^2.)/total((totarea[u]-mean(totarea[u]))^2.)
  r[n,10]=1.-total((totarea[v]-(tottau[v]*lvx[1]+lvx[0]))^2.)/total((totarea[v]-mean(totarea[v]))^2.)
  r[n,11]=1.-total((totarea[y]-(tottau[y]*lyx[1]+lyx[0]))^2.)/total((totarea[y]-mean(totarea[y]))^2.)
  ll[n,*,*]=[[lu],[lv],[ly],[lu2],[lv2],[ly2],[lu3],[lv3],[ly3],[lux],[lvx],[lyx]]
  wset,0
  plot, tottau[v],totarea[v],psym=2,title='940 nm'
  oplot,tottau[v],tottau[v]*lv[1]+lv[0],color=250
  plot, tottau[v],totarea2[v],psym=2,title='1150 nm'
  oplot, tottau[v],tottau[v]*lv2[1]+lv2[0],color=250
  wset,1
  plot, tottau[v],totarea3[v],psym=2, title='810 nm'
  oplot, tottau[v],tottau[v]*lv3[1]+lv3[0], color=250
  plot, tottau[v],totoxa[v],psym=2,title='Oxygen-a'
  oplot, tottau[v],tottau[v]*lvx[1]+lvx[0],color=250
  wait, 0.02
endfor

stop
end

;;;; function to transform integrated areas(x) to optical depth with the arguments (a)
function p_to_t, x, a
  return, a[1]+(a[2]*alog(x+a[0]))
end

function err_tau, x
  return, 0.02-0.0004*(x-100.)+0.0000085*(x-50.)^2.
end
