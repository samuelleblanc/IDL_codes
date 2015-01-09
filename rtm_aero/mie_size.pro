; code to run mie code from a size distribution of particles.
@mie_single.pro

pro mie_size

solver='libradtran'
figure=0
caltech=1
figs=0
RH_fact=1

r=read_ascii('/home/leblanc/CALNEX/aerosol_size/20100519/total_dndlogd_noNaNs.txt',data_start=1)
g=read_ascii('/home/leblanc/CALNEX/aerosol_size/20100519/AOCTimewave++.txt',data_start=1)
ams=read_ascii('/home/leblanc/CALNEX/aerosol_size/20100519/AMS_NP3_20100519_R0.ict',data_start=40)
BC_dat=read_ascii('/home/leblanc/CALNEX/aerosol_size/20100519/SP2_NP3_20100519_R0.ict',data_start=36)

; get met file
f=file_search('/home/leblanc/CALNEX/p3/20100519/*20100519*.ict.met', count=ct)
if ct lt 1 then message, 'met file not found!'
length=[0.,0.]
openu, lun, f[0], /get_lun
readf, lun, length
free_lun, lun
close, /all
met=read_ascii(f[0],data_start= length[0])
temp=met.field01[1,*] +273.15 ;temperature (Kelvin)
RH=met.field01[6,*]    ;Relative humidity (%)

kk=where(ams.field1 eq -9999., cts)
if cts gt 0 then ams.field1[kk]=0.
kk=where(BC_dat.field1 eq -9999., cts)
if cts gt 0 then BC_dat.field1[kk]=0.
BC=BC_dat.field1[2,*]/1000.
t=ams.field1[0,*]
total=ams.field1[6,*]+BC
kk=where(ams.field1[6,*] eq 0., ct)
if ct gt 0 then begin
  SO4=ams.field1[1,*]
  NO3=ams.field1[2,*]
  Chl=ams.field1[3,*]
  NH4=ams.field1[4,*]
  Org=ams.field1[5,*]
  kp=where(ams.field1[6,*] ne 0., cts)
  SO4[kp]=SO4[kp]/total[kp]
  NO3[kp]=NO3[kp]/total[kp]
  Chl[kp]=Chl[kp]/total[kp]
  NH4[kp]=NH4[kp]/total[kp]
  Org[kp]=Org[kp]/total[kp]
  BC[kp]=BC[kp]/total[kp]
  SO4=interpol(SO4[kp],t[kp],t)
  NO3=interpol(NO3[kp],t[kp],t)
  Ch1=interpol(Chl[kp],t[kp],t)
  NH4=interpol(NH4[kp],t[kp],t)
  Org=interpol(Org[kp],t[kp],t)
  BC=interpol(BC[kp],t[kp],t)
endif else begin
  SO4=ams.field1[1,*]/total
  NO3=ams.field1[2,*]/total
  Chl=ams.field1[3,*]/total
  NH4=ams.field1[4,*]/total
  Org=ams.field1[5,*]/total
  BC=BC/total
endelse

kk=where(r.field001 eq -9999.,ct)
if ct gt 0 then begin
if figure then r.field001[kk] = !values.f_nan else r.field001[kk] = 0.
endif
distri=r.field001[26:*,*]
ss=size(distri,/dimensions)
m=[1.56,-0.01]

SO4_m=[[1.452,-1E-8],[1.44,-1E-8],[1.432,-1E-8],[1.431,-1E-8],[1.428,-1.99E-8],[1.425,-1.79E-7],[1.423,-1.5E-6]]
BC_m=[[1.85,-0.725],[1.85,-0.72],[1.85,-0.715],[1.85,-0.71],[1.85,-0.69],[1.85,-0.695],[1.86,-0.70]]
NO3_m=[[1.47,-1.0E-8],[1.47,-1.0E-8],[1.47,-1.0E-8],[1.47,-1.0E-8],[1.47,-1.0E-8],[1.47,-2.46E-7],[1.47,-2.01E-6]]
ChL_m=[[1.58,-1.0E-7],[1.57,-1.0E-7],[1.57,-1.0E-7],[1.55,-1.0E-7],[1.55,-1.0E-7],[1.53,-1.0E-7],[1.53,-1.0E-7]]
NH4_m=[[1.53,-1.0E-7],[1.54,-1.0E-7],[1.55,-1.0E-7],[1.53,-1.0E-7],[1.52,-1.0E-7],[1.52,-1.0E-7],[1.51,-1.0E-7]]
Org_m=[[1.4,-0.0],[1.4,-0.0],[1.4,-0.0],[1.4,-0.0],[1.4,-0.0],[1.4,-0.0],[1.4,-0.0]]

diam=g.field1[1,26:138]
d_diam=g.field1[1,26:138]*10^g.field1[2,26:138]-g.field1[1,26:138]

wvl=[340.,380.,440.,500.,675.,870.,1020.]/1000.
n_wvl=n_elements(wvl)
asy=fltarr(ss[1],n_wvl)
ssa=fltarr(ss[1],n_wvl)
ext=fltarr(ss[1],n_wvl)
sca=fltarr(ss[1],n_wvl)

if figure then begin
set_plot, 'ps'
   loadct, 39,/silent
 device, /encapsulated
 device, /tt_font, set_font='Helvetica Bold'
 device, filename='/home/leblanc/CALNEX/aerosol_size/20100519/size_dist.ps'
 device,/color,bits_per_pixel=8.
 device, xsize=20, ysize=20
   !p.font=1 & !p.thick=5
   !p.charsize=2.0 & !x.style=1
   !y.style=1 & !z.style=1
   !y.thick=1.8 & !x.thick=1.8
  !p.multi=0
  levl=10.^((findgen(30)-5)/6.)
contour, transpose(distri), reform(g.field1[0,*]/3600.), reform(diam), levels=levl, /cell_fill, title='Drop size distribution', $
 xtitle='time', ytitle='diameter (um)', position=[0.17,0.2,0.8,0.9], xticks=4,min_value=min(transpose(distri),/nan), max_value=max(transpose(distri),/nan)  ;,xrange=[-118.13,-118.01]
contour, transpose([[levl],[levl]]),[0,1],levl, levels=levl, /cell_fill, title='', xtitle='', xticks=1, yticks=6, XSTYLE=5, YSTYLE=5, position=[0.83,0.2,0.88,0.9],/ylog,/noerase
plot,levl, [0,1], /nodata,position=[0.83,0.2,0.88,0.9], ystyle=9, xstyle=1, xticks=1, xminor=0, yminor=0,xtickname=[' ',' '],ytickname=[' ',' '],yticks=1,/noerase
axis, yaxis=1, yrange=[min(levl), max(levl)], ystyle=1,/ylog,/save
;colorbar, ,/right,/vertical, tick_names=[string(levl[[0,5,10,15,20,25,29]],format='(F8.2)')], range=[levl[0],levl[29]],/ylog;, format='(F5.2)'
device, /close
spawn, 'convert "/home/leblanc/CALNEX/aerosol_size/20100519/size_dist.ps" "/home/leblanc/CALNEX/aerosol_size/20100519/size_dist.png"'
spawn, 'rm -f "/home/leblanc/CALNEX/aerosol_size/20100519/size_dist.ps"'
stop
endif

dir='/home/leblanc/rtm_aero/size/'
print, 'starting to loop over all points:', ss[1]
for j=0, n_wvl-1 do begin
print, 'At Wavelength:', wvl[j]

if caltech then begin
  restore, '/home/leblanc/CALNEX/aerosol_size/20100519/Caltech_index.out'
  i_0=0
  i_n=n_elements(fl_bot)-1
  ns=fl_bot
  print, 'max number of points:', fl_bot[i_n]
endif else begin
  i_0=0
  i_n=ss[1]-1
endelse

for ii=0, i_n do begin ; loop through all the times
  if caltech then i=ns[ii] else i=ii
  m[0]=1.52;SO4[i]*SO4_m[0,j]+BC[i]*BC_m[0,j]+NO3[i]*NO3_m[0,j]+ChL[i]*ChL_m[0,j]+NH4[i]*NH4_m[0,j]+Org[i]*Org_m[0,j]
  m[1]=-0.0;SO4[i]*SO4_m[1,j]+BC[i]*BC_m[1,j]+NO3[i]*NO3_m[1,j]+ChL[i]*ChL_m[1,j]+NH4[i]*NH4_m[1,j]+Org[i]*Org_m[1,j]
;  print, 'Complex index of Refraction:', m
;  stop

  if RH_fact then begin
    a=6.92;4.92;3.26  ; mixed urban maritime aerosols growth factor parameters from Liu et al. 2007 
    b=5.04;3.85
    diam=g.field1[1,26:138]*(1.+a*(RH[i]/100.)^b)
    d_diam=diam*10^g.field1[2,26:138]-diam
  endif
  
  case solver of
  'fortran':begin
    spawn, '/home/leblanc/rtm_aero/pro/mie_dist '+dir+'scat_'+strtrim(string(i,format='(I06)'),2)+'.out '+$
     dir+'phase_'+strtrim(string(i,format='(I06)'),2)+'.out '+strtrim(wvl[j])+' '+strjoin(strtrim(m,2)+' ')+$
     ' '+strjoin(strtrim(distri[*,i],2)+' '), output
     
    if strmatch(output, '* NOT *') eq 1 or strmatch(output, '* segmentation *') eq 1 then print, 'mie code input failed at ' + strtrim(i,2) else begin
      reads, output, a,b,c
      asy[i,j]=a & ssa[i,j]=b & ext[i,j]=c
      sca[i,j]=ssa[i,j]*ext[i,j]
    endelse
  end
  'idl':begin
    Dx= 2.*!Pi*diam/2./wvl[j] ; set size parameters
    Cm=complex(replicate(m[0],139),replicate(m[1],139)) ;set index of refraction
    Cm_0=complex(m[0],m[1])
    Mie_single,Dx,Cm_0,Dqxt,Dqsc,Dqbk,Dg,/silent ; call mie scattering

  	; evaluate asy, ssa, ext, scat for the average over particle size density
  	; g= int(g(r)*n(r), dr)/int(n(r),dr)
      asy[i,j]=int_tabulated(diam,Dg*distri[*,i]/d_diam)/int_tabulated(diam,distri[*,i]/d_diam)
  	sca[i,j]=int_tabulated(diam/2.,Dqsc*distri[*,i]*D_diam/2.)/int_tabulated(diam/2.,distri[*,i]*D_diam/2.)
  	ext[i,j]=int_tabulated(diam/2.,Dqxt*distri[*,i]*D_diam/2.)/int_tabulated(diam/2.,distri[*,i]*D_diam/2.)
  	ssa[i,j]=sca[i,j]/ext[i,j]
  
    if figs and caltech then begin
      set_plot, 'ps'
      loadct, 39,/silent
     device, /encapsulated,/color,bits_per_pixel=8., /tt_font, set_font='Helvetica Bold'
     device, filename='/home/leblanc/CALNEX/p3/20100519/aero_size_'+strtrim(string(ii,format='(I03)'),2)+'_'+strtrim(string(wvl[j]*1000.,format='(I04)'),2)+'.ps'
     device, xsize=20, ysize=20
     
     !p.font=1 & !p.thick=5
     !p.charsize=2.0 & !x.style=1
     !y.style=1 & !z.style=1
     !y.thick=1.8 & !x.thick=1.8
     !p.multi=0
      plot, diam, distri[*,i]/(-alog(d_diam)), ystyle=8,ytitle='d(N)/d(log D)', title='Particle size distribution',xmargin=[8,8],xtitle='diameter (microns)', xrange=[0,2]
      axis, yaxis=1, ystyle=1, yrange=[0,1],/save,color=70,ytitle='Asymmetry Parameter'
      oplot,diam, Dg, color=70
      xyouts, 0.3,0.3, 'g='+strtrim(string(asy[i,j],format='(F6.3)'),2),color=70
      if RH_fact then xyouts, 0.3,0.4, 'f='+strtrim(string((1.+a*(RH[i]/100.)^b),format='(F6.3)'),2),color=70
     device, /close
     spawn, 'convert "/home/leblanc/CALNEX/p3/20100519/aero_size_'+strtrim(string(ii,format='(I03)'),2)+'_'+strtrim(string(wvl[j]*1000.,format='(I04)'),2)+'.ps" "/home/leblanc/CALNEX/p3/20100519/aero_size_'+strtrim(string(ii,format='(I03)'),2)+'_'+strtrim(string(wvl[j]*1000.,format='(I04)'),2)+'.png"'
     spawn, 'rm -f "/home/leblanc/CALNEX/p3/20100519/aero_size_'+strtrim(string(ii,format='(I03)'),2)+'_'+strtrim(string(wvl[j]*1000.,format='(I04)'),2)+'.ps"'
    endif
  end
  'libradtran':begin
    input_file='/home/leblanc/libradtran/input/mie/mie.inp'
    output_file='/home/leblanc/libradtran/output/mie/mie.out'
    mie_size_file='/home/leblanc/libradtran/input/mie/mie_size.txt'
    
    lun=98
    openw, lun,mie_size_file
    for id=0, n_elements(diam)-1 do printf, lun, diam[id], distri[id,i] 
    free_lun, lun
    
    wvll=[wvl[j],wvl[j]]
    write_mie_input, m, input_file, wvll, mie_size_file,/quiet
    spawn, '/home/leblanc/libradtran/libRadtran-1.5-beta/bin/mie < '+input_file+' > '+output_file
    read_mie_output, output_file, a,b,c,d 
  
    asy[i,j]=a & ssa[i,j]=b & ext[i,j]=c
    sca[i,j]=d
  
  end
  else: message, 'Not supported solver'
  endcase
  print, 'loop at:',i, ' wvl:',wvl[j], ' asy:',asy[i,j], ' ssa:',ssa[i,j],' ext:',ext[i,j]
;  print, strjoin(strtrim(r.field001[*,i],2)+' ')
end ;unit loop
end ;wvl loop

time=g.field1[0,*]
save, time,asy, ssa, ext,sca,wvl,filename=dir+'mie_output_caltech.out' 
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write input file program                                                    ;;
;; used to make the input file for mie                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro write_mie_input, m, input_file,wvl, mie_size_file, quiet=quiet
if m[1] lt 0. then m[1]=-m[1]
ui=99
openw,ui,input_file;,/get_lun
printf,ui,'mie_program MIEV0'
printf,ui,'mimcut 0.0000000001'
printf,ui,'output_user   lambda r_eff refrac_real refrac_imag qext qsca omega gg ' 
printf,ui,'size_distribution_file   '+mie_size_file
printf,ui,'refrac user '+string(m[0])+string(m[1])    ; index of refraction real then imaginary
printf,ui,'wavelength        '+string(wvl[0])+'  '+string(wvl[1]) ; wavelength used for hsrl
printf,ui,'wavelength_step 1.'
free_lun,ui

if not keyword_set(quiet) then print, 'input file saved to: '+input_file

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; read output file program for mie                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro read_mie_output, output_file,asy,ssa,ext, scat
lambda=0. & r_eff=0. & refrac=0. & refrac_img=0. & ext=0. & scat=0. & ssa=0. & asy=0.

ui=97
openr,ui,output_file
readf, ui, lambda, r_eff,refrac, refrac_img, ext, scat, ssa, asy
free_lun,ui

end
