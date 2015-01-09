;+
; NAME:
;   calnex_aerosol_retrieval
;
; PURPOSE:
;   to do the aerosol retrieval on select days 
;
; CATEGORY:
;   CALNEX, Aerosol
;
; CALLING SEQUENCE:
;   calnex_aerosol_retrieval, date
; - date - date of day to check
;
; OUTPUT:
;   plots
;   retrieved values in a text file
;
; KEYWORDS:
;   - plotting:   to plot irradiance and albedo longitudinal (time lapse)
;   - video:      to make consecutive plots of spectras
;   - divergence: to make a plot of the flux divergence
;   - tau_scale:  to scale the retrieval by this amount (can also be used while the multi wavelenght mode is on)
;   - hsrl_only:  to turn off the multi wavelength retrieval and only used the hsrl value at 532 (good for testing)
;   - use_wvl:    set this to the desired wvl index
;   - cluster:    sets the paths to the cluster paths
;   - abs_min:    makes the retrieval find the absolute minimum
;
; DEPENDENCIES:
;   
;   
; NEEDED FILES:
;   - hsrl_p3 out file
;
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
; Written:  Samuel LeBlanc, LASP CU Boulder, August 3rd, 2010
; Modified: August 11th, 2010 by Samuel LeBlanc
;           - added call to write_input 
;           - added plotting keyword if statement, to cicurmvent all plotting
;           - added the writing of the input file for the running of uvspec
; Modified: August 18th, 2010 by Samuel LeBlanc
;           - added wavelength dependence with aeronet reading
;           - added tau_scale, for manually varrying tau
;           - added retrieval result saving to text file
; Modified: August 20th, 2010 by Samuel LeBlanc
;           - added keyword to turn on or off wavelenght dependance
;           - added tau optimization routine (from the difference between the two retrieved g
; Modified: July 5th, 2011 by Samuel LeBlanc
;           - changed error checking routines to make them in line with ARCTAS retrieval
;           - modified some more stringent ones like the second loops in asy2
; Modified: July 19th, 2011 by Samuel LeBlanc
;           - added a keyword for using CRD as optical depth input, instead of AATS
; Modified: January 5th, 2012 by Samuel LeBlanc, On my birthday in Ottawa
;           - added keyword, use_wvl to be used with the cluster
;           - added keyword, cluster to set paths to the cluster
;           - added abs_min that sets the retrieval to get the absolute minimum
;---------------------------------------------------------------------------
@\\lasp-smb\leblanc\CALNEX\pro\write_input.pro
@write_input.pro
@zensun.pro
@\\lasp-smb\leblanc\libradtran\pro\libradtran_reader.pro
@/home/leblanc/libradtran/pro/libradtran_reader.pro
@/projects/leblanse/libradtran/pro/libradtran_reader.pro
@read_aeronet.pro
@\\lasp-smb\leblanc\CALNEX\pro\read_aeronet.pro

pro calnex_aerosol_retrieval, dateinp , second=second, save=save, plotting=plotting, video=video, divergence=divergence,$
 tau_scale=tau_scale, hsrl_only=hsrl_only,tau_approx=tau_approx,map=map, aeronet=aeronet,no_szacorr=no_szacorr,$
 no_tau_loop=no_tau_loop, find_legs=find_legs, tau_above=tau_above, constant_ssa=constant_ssa, sensitivity=sensitivity,$
 choose=choose, angstrom=angstrom, aats=aats, interactive=interactive, interactive_ssa=interactive_ssa, crd=crd,$
 use_wvl=use_wvl,cluster=cluster, abs_min=abs_min

close, /all
;set windows or linux
if !VERSION.OS eq 'linux' then linux=1 else linux=0
if not keyword_set(plotting) then plotting=0
if not keyword_set(aats) then aats=0
if not keyword_set(crd) then crd=0
if keyword_set(second) then secon='_2' else secon=''
if not keyword_set(no_tau_loop) then no_tau_loop=0
if not keyword_set(cluster) then cluster=0
if not keyword_set(abs_min) then abs_min=0
if not plotting then begin
  video=0
  divergence=0
endif
if n_elements(dateinp) lt 1 then begin
  date='20100519'
endif else begin
  date=strcompress(string(dateinp),/REMOVE_ALL)
endelse

if linux then begin
  calnex_dir='/home/leblanc/CALNEX/'
  ll='/'
  dir='/home/leblanc/libradtran/input/aero/'
  dirout='/home/leblanc/libradtran/output/aero/'
endif else begin
  calnex_dir='\\lasp-smb\leblanc\CALNEX\'
  ll='\'
  dir='\\lasp-smb\leblanc\libradtran\input\aero\'
  dirout='\\lasp-smb\leblanc\libradtran\output\aero\'
endelse

if cluster then begin
  calnex_dir='/projects/leblanse/CALNEX/'
  ll='/'
  dir='/scratch/stmp00/leblanse/libradtran/input/aero/'
  dirout='/scratch/stmp00/leblanse/libradtran/output/aero/'
endif

if crd then restore, calnex_dir+'p3'+ll+date+ll+date+'_p3_crd.out' else restore, calnex_dir+'p3'+ll+date+ll+date+'_p3_hsrl.out'

if divergence then begin
  ind=[0,1,20,40,60,80]
  for it=0, 5 do begin
    if it eq 0 then begin
      set_plot, 'ps'
      loadct, 39
      !p.font=1
      !p.thick=5
      !p.charsize=1.8
      !x.style=1
      !y.style=1 
      !z.style=1
      !y.thick=1.8
      !x.thick=1.8
      !p.multi=0
      device, /encapsulated
       device, /tt_font, set_font='Helvetica Bold'
       device, filename=calnex_dir+'p3'+ll+date+ll+date+'_p3_hsrl_0.ps'
       device,/color,bits_per_pixel=8.
       device, xsize=20, ysize=20
            
      plot, nadlambda,  ((near.zabove_attcorr[*,ind[it]]-near.nabove[*,ind[it]])-(near.zbelow_attcorr[*,ind[it]]-near.nbelow[*,ind[it]]))/near.zabove_attcorr[*,ind[it]],$
      title='Flux Divergence spectra for '+date, ytitle='percent absorptance', xtitle='wavelength (nm)', yrange=[-0.1,0.4]
      cl_leg=[0]
      txt_leg=['index:'+strtrim(string(ind[it]),2)]
    endif else begin
      oplot, nadlambda,  ((near.zabove_attcorr[*,ind[it]]-near.nabove[*,ind[it]])-(near.zbelow_attcorr[*,ind[it]]-near.nbelow[*,ind[it]]))/near.zabove_attcorr[*,ind[it]],$
       color=ind[it]*3
      cl_leg=[cl_leg,ind[it]*3]
      txt_leg=[txt_leg,'index:'+strtrim(string(ind[it]),2)]
    endelse
  endfor
  
  legend, txt_leg, textcolors=cl_leg, /box
  
  device, /close
  spawn, 'convert '+calnex_dir+'p3'+ll+date+ll+date+'_p3_hsrl_0.ps '+calnex_dir+'p3'+ll+date+ll+date+'_p3_hsrl_0.png'
  ;spawn, 'display '+calnex_dir+'p3'+ll+date+ll+date+'_p3_hsrl_0.png'
  if linux then spawn, 'rm '+calnex_dir+'p3'+ll+date+ll+date+'_p3_hsrl_0.ps' else spawn, 'del /Q '+calnex_dir+'p3'+ll+date+ll+date+'_p3_hsrl_0.ps'
endif

;;; irradiances for top bottom at both 500 and 1050nm

if plotting then begin
  set_plot, 'ps'
  loadct, 39
  !p.multi=[0,2,2]
  device, /encapsulated
   device, /tt_font, set_font='Helvetica Bold'
   device, filename=calnex_dir+'p3'+ll+date+ll+date+'_p3_hsrl_irr.ps'
   device,/color,bits_per_pixel=8.
   device, xsize=40, ysize=40
endif

zabove=near.zabove_attcorr & zbelow=near.zbelow_attcorr & nabove=near.nabove & nbelow=near.nbelow & leg1=legs.leg1 & leg2=legs.leg2

; filtering routine to make sure that both IR and vis are coherent with each other
; filters out if IR larger and vis good (not coherent) and vice versa
tau=fltarr(n_elements(legs.leg1))  
dtau=tau
wl=500.
mm=min(abs(nadlambda-wl),wl_i)
wll=1050.
mm=min(abs(nadlambda-wll),wl_ii)
fl=0
limit=mean(zabove[wl_i,*],/nan)
limit_b=mean(zbelow[wl_i,*],/nan)
limitt=mean(zabove[wl_ii,*],/nan)
limitt_b=mean(zbelow[wl_ii,*],/nan)
limitn_b=mean(nbelow[wl_i,*],/nan)
tm=min(alt[legs.leg1])
nul=min(abs(z-tm),tmi) 
tmm=max(alt[legs.leg2])
nul=min(abs(z-tmm),tmmi)
z_inds=[tmi,tmmi]
zs=shift(z,-1)
for i=0,n_elements(leg1)-1 do begin
  fl_t=0 & fl_tt=0 & fl_ttt=0 & fl_tttt=0 & fl_ttttt=0 & fl_tn=0 & fl_tau=0 & fl_tau2=0 & fl_div=0
  if zabove[wl_i,i] lt limit-0.2 then fl_bad=1 else fl_t=1  ;0.1
  if zbelow[wl_i,i] lt limit_b-0.2 then fl_bad=1 else fl_tt=1  ;0.1
  if zabove[wl_ii,i] lt limitt-0.1 then fl_bad=1 else fl_ttt=1 ; 0.05
  if (zbelow[wl_ii,i] lt limitt_b-0.1) xor (zbelow[wl_i,i] lt limit_b-1.0) then fl_bad=1 else fl_tttt=1  ;0.05, 0.5
  fl_ttttt=1; if (r_d[i] gt 2000.0) then fl_bad=1 else fl_ttttt=1 ;500.0
  if nbelow[wl_i,i] lt limitn_b-0.2 then fl_bad=1 else fl_tn=1  ;0.1
  if (zabove[wl_i,i]-nabove[wl_i,i])-(zbelow[wl_i,i]-nbelow[wl_i,i]) lt 0 then fl_bad=1 else fl_div=1
  fl_tau=1 ; if total(finite(tau_aats[leg1[i],*])-1) ge -12 then fl_tau=1
  fl_tau2=1 ; if total(finite(tau_aats[near_leg2[i],*])-1) ge -12 then fl_tau2=1
  if (fl_t and fl_tt and fl_ttt and fl_tttt and fl_ttttt and fl_tn and fl_tau and fl_tau2 and fl_div) then fl=[fl,i]
  tau[i]=total((near.extinction[z_inds[0]:z_inds[1],i]*(-z[z_inds[0]:z_inds[1]]+zs[z_inds[0]:z_inds[1]])*0.001),/nan)
endfor
if n_elements(fl) le 1 then print, '*** all points filtered out ***'
fl=fl[1:*]
print, 'number of valid spectras:',n_elements(fl) 

if plotting then begin
  plot, lon[legs.leg1[fl]],  near.zabove_attcorr[wl_i,fl],color=0,$
  title='Irradiance timelapse for '+date+' at '+strtrim(string(nadlambda[wl_i]),2)+'nm', ytitle='Irradiance', xtitle='Longitude', yrange=[0,2]
  oplot, lon[legs.leg1[fl]],  near.nabove[wl_i,fl], color=150
  oplot, lon[legs.leg1[fl]],  near.zabove_attcorr[wl_i,fl]-near.nabove[wl_i,fl], color=240
  
  oplot, lon[legs.leg1[fl]],  near.zbelow_attcorr[wl_i,fl], color=0, linestyle=2
  oplot, lon[legs.leg1[fl]],  near.nbelow[wl_i,fl], color=150, linestyle=2
  oplot, lon[legs.leg1[fl]],  near.zbelow_attcorr[wl_i,fl]-near.nbelow[wl_i,fl], color=240, linestyle=2
  
  legend, ['Downwelling', 'Upwelling','Net','Bottom leg'],textcolors=[0,150,240,0],linestyle=[0,0,0,2],color=[0,150,240,0], box=0, position=[-118.14,1.]
  
  wl=1050.
  mm=min(abs(nadlambda-wl),wl_i)
  plot, lon[legs.leg1[fl]],  near.zabove_attcorr[wl_i,fl],color=0,$
  title='Irradiance timelapse at '+strtrim(string(nadlambda[wl_i]),2)+'nm', ytitle='Irradiance', xtitle='Longitude',yrange=[0,0.75], xmargin=[10,6], ystyle=8
  oplot, lon[legs.leg1[fl]],  near.nabove[wl_i,fl], color=150
  oplot, lon[legs.leg1[fl]],  near.zabove_attcorr[wl_i,fl]-near.nabove[wl_i,fl], color=240
  
  oplot, lon[legs.leg1[fl]],  near.zbelow_attcorr[wl_i,fl], color=0, linestyle=2
  oplot, lon[legs.leg1[fl]],  near.nbelow[wl_i,fl], color=150, linestyle=2
  oplot, lon[legs.leg1[fl]],  near.zbelow_attcorr[wl_i,fl]-near.nbelow[wl_i,fl], color=240, linestyle=2
  
  ;plotting tau
  if crd then ytits='CRD tau' else ytits='HSRL tau'
  axis, yaxis=1, /device, yrange=[0,1.], /save, color=200, ytitle=ytits
  oplot, lon[legs.leg1[fl]], tau[fl], color=200, psym=2
  
  ;plotting of albedo
  wl=650.
  mm=min(abs(nadlambda-wl),wl_i)
  plot, lon[legs.leg1[fl]],  near.nabove[wl_i,fl]/near.zabove_attcorr[wl_i,fl],color=0,$
  title='Albedo at different wavelengths', ytitle='Albedo', xtitle='Longitude',yrange=[0,0.3]
  oplot, lon[legs.leg1[fl]], near.nbelow[wl_i,fl]/near.zbelow_attcorr[wl_i,fl], color=240, linestyle=0
  txt=[strtrim(string(nadlambda[wl_i]),2)+'nm']
  wl=550.
  mm=min(abs(nadlambda-wl),wl_i)
  oplot, lon[legs.leg1[fl]],  near.nabove[wl_i,fl]/near.zabove_attcorr[wl_i,fl], color=0, linestyle=2
  oplot, lon[legs.leg1[fl]],  near.nbelow[wl_i,fl]/near.zbelow_attcorr[wl_i,fl], color=240, linestyle=2
  txt=[txt,strtrim(string(nadlambda[wl_i]),2)+'nm']
  wl=1050.
  mm=min(abs(nadlambda-wl),wl_i)
  oplot, lon[legs.leg1[fl]],  near.nabove[wl_i,fl]/near.zabove_attcorr[wl_i,fl], color=0, linestyle=3
  oplot, lon[legs.leg1[fl]],  near.nbelow[wl_i,fl]/near.zbelow_attcorr[wl_i,fl], color=240, linestyle=3
  txt=[txt,strtrim(string(nadlambda[wl_i]),2)+'nm']
  
  legend, [txt,'above', 'below'], textcolors=[0,0,0,0,240], color=[0,0,0,0,240],linestyle=[0,2,3,0,0], box=0, /bottom
endif

;plotting of distance
; determination of the nearest distance from leg2 to leg1
; writing out the distance
near_leg2=legs.leg1*0
r_d=legs.leg1*0.0
  for i=0, n_elements(legs.leg1)-1 do begin
      ; for second leg onto first
      r=100000.0
      for j=0, n_elements(legs.leg2)-1 do begin
        distance = map_2points(lon[legs.leg1[i]],lat[legs.leg1[i]],lon[legs.leg2[j]],lat[legs.leg2[j]],/meters)
        if distance[0] le r then begin
          near_leg2[i]=legs.leg2[j]
          r=distance[0]
          r_d[i]=distance[0]
        endif
      endfor
  endfor
;stop
if plotting then begin
  plot, lon[legs.leg1[fl]], r_d, title='Distance from the lower leg to the top leg', xtitle='Longitude', ytitle='meters', ystyle=8, xmargin=[10,6]
  oplot, lon[legs.leg1[fl]], findgen(n_elements(legs.leg1[fl]))*10, psym=2, color=70
  for i=0, n_elements(legs.leg1[fl])-1,5 do begin
    xyouts, lon[legs.leg1[fl[i]]],i*10,strtrim(string(fl[i]),2), color=70
  endfor
  
  ;checking angles
  axis, yaxis=1, ytitle='roll', yrange=[-10.,+10.0],/save, color=250
  SSFR_path=calnex_dir+'p3'+ll+date+ll+date+'_SP.out'
  restore, SSFR_path
  oplot, lon[legs.leg1[fl]],rol[legs.leg1[fl]], color=250
  oplot, lon[legs.leg1[fl]],pit[legs.leg1[fl]], color=150
  xyouts, lon[legs.leg1[80]] , pit[legs.leg1[80]],'Pitch', color=150
    
  device, /close
  spawn, 'convert '+calnex_dir+'p3'+ll+date+ll+date+'_p3_hsrl_irr.ps '+calnex_dir+'p3'+ll+date+ll+date+'_p3_hsrl_irr.png'
  if linux then spawn, 'rm '+calnex_dir+'p3'+ll+date+ll+date+'_p3_hsrl_irr.ps' else spawn, 'del /Q '+calnex_dir+'p3'+ll+date+ll+date+'_p3_hsrl_irr.ps'
endif


;plotting for making video
;plot of all the different spectras throughout the leg
if video then begin
  !p.multi=[0,2,1]
  en=n_elements(legs.leg1)-1
  for i=0, en do begin
    set_plot, 'ps'
    loadct, 39, /silent
     device, /encapsulated
      device, /tt_font, set_font='Helvetica Bold'
      device, filename=calnex_dir+'p3'+ll+date+ll+strtrim(string(i,format='(I02.2)'),2)+'.ps'
      device,/color,bits_per_pixel=8.
      device, xsize=40, ysize=20
     
    plot, nadlambda,  near.zabove_attcorr[*,i],$
    title='Spectra for '+date+' Longitude: '+strtrim(string(lon[legs.leg1[i]]),2), ytitle='Irradiance', xtitle='wavelength (nm)', yrange=[0,2.]
    oplot, nadlambda,  near.nabove[*,i], color=150
    oplot, nadlambda,  near.zabove_attcorr[*,i]-near.nabove[*,i], color=240
    
    oplot, nadlambda,  near.zbelow_attcorr[*,i], color=0, linestyle=2
    oplot, nadlambda,  near.nbelow[*,i], color=150, linestyle=2
    oplot, nadlambda,  near.zbelow_attcorr[*,i]-near.nbelow[*,i], color=240, linestyle=2
    
    legend, ['Downwelling', 'Upwelling','Net','Bottom leg'],textcolors=[0,150,240,0],linestyle=[0,0,0,2],color=[0,150,240,0], box=0, /right
    
    plot, nadlambda,  near.nabove[*,i]/near.zabove_attcorr[*,i],color=0,$
    title='Spectral Albedo', ytitle='Albedo', xtitle='Wavelength (nm)',yrange=[0,0.3]
    oplot, nadlambda, near.nbelow[*,i]/near.zbelow_attcorr[*,i], color=0, linestyle=2
    
    device, /close
    spawn, 'convert '+calnex_dir+'p3'+ll+date+ll+strtrim(string(i,format='(I02.2)'),2)+'.ps '+calnex_dir+'p3'+ll+date+ll+strtrim(string(i,format='(I02.2)'),2)+'.jpg'
    if linux then spawn, 'rm '+calnex_dir+'p3'+ll+date+ll+strtrim(string(i,format='(I02.2)'),2)+'.ps' else spawn, 'del /Q '+calnex_dir+'p3'+ll+date+ll+strtrim(string(i,format='(I02.2)'),2)+'.ps'
  endfor
endif

corr_eps=0.70 ; suitable correction factors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Writing of the input file for uvspec                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

atm_in = 'atmos_aero_'+date+'_at.dat'   ;input atmosphere file
;set random starting values for input file
asy=0.6
asy2=asy
ssa=0.9

ssa_arr=fl*0.0 & asy_arr=fl*0.0 & asy2_arr=fl*0.0
albedo_arr=fl*0.0 & correc_fac_arr=fl*0.0 & tau_mod_arr=fl*0.0
dif_flux=fl*0.0 & tau_arr=fl*0.0

if not keyword_set(tau_scale) then tau_s=1.0 else tau_s=tau_scale  ;at normal
if not keyword_set(aeronet) then aeronet=0

if keyword_set(hsrl_only) then begin
  aeronet=0 & wvl_arr=[532.] ; set the wavelength to the hsrl wavelength
  iw_f=0 & ia_arr=[0]
endif ; hsrl keyword

if aeronet then begin
  ; get aeronet reading
  read_aeronet, calnex_dir+'aeronet'+ll+'*CalTech',  jul_day, jul_day_almu, wvl_aod,wvl_almu, aod_aeronet,ssa_aeronet, sza_aeronet, asy_TT_aeronet, aot2_T_aeronet, up_flux_T_aeronet, down_flux_T_aeronet, diff_flux_T_aeronet, forcing_aeronet, albedo_aeronet

  ; find correct times and wavelengths

  ; make a list of good wavelengths from aeronet
  wvl_arr=0.0 & ia_arr=0
  for i_a=0, n_elements(wvl_aod)-1 do begin
    if finite(aod_aeronet[i_a,0]) then begin
      wvl_arr=[wvl_arr,wvl_aod[i_a]] 
      ia_arr=[ia_arr,i_a]
    endif
  endfor
  wvl_arr=wvl_arr[1:*] & ia_arr=ia_arr[1:*]
  iw_f=n_elements(wvl_arr)-1  ;final iw wavelenght index
endif 

if aats then begin
  wvl_arr=lambda
  iw_f=n_elements(lambda)-1
  ia_arr=findgen(n_elements(lambda))
  
  if keyword_set(angstrom) then begin
    wvl_ref=500.
    m=min(abs(wvl_ref-lambda),ref_ind)
    alpha=1.8 ; angstrom coefficient
    for ii=0,n_elements(fl)-1 do begin
      tau_ref=tau[fl[ii],ref_ind]
      for jj=0,n_elements(lambda)-1 do begin
      tau[fl[ii],jj]=tau_ref*(lambda[jj]/lambda[ref_ind])^alpha
      endfor      
    endfor
  endif
  
endif
;[0.580000  ,   0.550000 ,    0.450000 ,    0.400000 ,    0.380000 ,    0.295300 ,    0.237800  ,   0.180800  ,   0.143300   ,  0.103200  ,  0.0668000  ,  0.0315000  ,  0.0247000]
;[  353.500    ,  380.000  ,    452.600  ,    499.400   ,   519.400 ,     605.800   ,   675.100 ,     779.100  ,    864.500  ,    1019.10   ,   1241.30   ,   1558.50  ,    2139.30]

;;plotting of map
if keyword_set(map) then begin
set_plot, 'ps'
  loadct, 39,/silent
  device, /encapsulated
   device, /tt_font, set_font='Helvetica Bold'
   device, filename=arctas_dir+'nasa'+ll+date+ll+date+'_nasa_map.ps'
   device,/color,bits_per_pixel=8.
   device, xsize=40, ysize=40
  !p.multi=0
  
  maxlon=max(lon) & maxlat=max(lat)
  minlon=min(lon) & minlat=min(lat)
  if minlon lt -180 then minlon=-180
  mlon=(maxlon+minlon)*0.5
  mlat=(minlat+maxlat)*0.5

  map_set,mlat,mlon,/grid,limit=[minlat,minlon,maxlat,maxlon],/label,title='ARCTAS on '+date
  map_continents,/usa,/hires,thick=2
  map_continents,/countries,/hires,/coast,thick=2

  oplot, lon, lat, psym=3
  oplot, lon[leg1], lat[leg1], psym=2, color=250
  legend,['flight path', 'aerosol leg'],textcolors=[0,250]
  device,/close
  spawn,'convert '+arctas_dir+'nasa'+ll+date+ll+date+'_nasa_map.ps '+arctas_dir+'nasa'+ll+date+ll+date+'_nasa_map'+secon+'.png'
  spawn,'rm -f '+arctas_dir+'nasa'+ll+date+ll+date+'_nasa_map.ps'

endif

;if not keyword_set(choose) or not keyword_set(find_legs) then ind=findgen(30)
;fl=fl[ind]

;;saving important pieces to a file (for seb)
if keyword_set(save) then begin
  nab=nabove & nbe=nbelow & zab=zabove & zbe=zbelow; & dtaa=dtau_aats & taa=tau_aats 
  nabove=nabove[*,fl] & nbelow=nbelow[*,fl] & wvl=nadlambda
  zabove=zabove[*,fl] & zbelow=zbelow[*,fl] 
  latabove=lat[near_leg2[fl]] & latbelow=lat[leg1[fl]]
  lonabove=lon[near_leg2[fl]] & lonbelow=lon[leg1[fl]]
  altabove=alt[near_leg2[fl]] & altbelow=alt[leg1[fl]]
  szaabove=sza[near_leg2[fl]] & szabelow=sza[leg1[fl]]
  utcabove=utc[near_leg2[fl]] & utcbelow=utc[leg1[fl]]
  i_above=near_leg2[fl] & i_below=leg1[fl]
  ;tau_aats=tau[fl,*] & dtau_aats=dtau[fl,*] & lambda_aats=lambda & 
  save, nabove,nbelow,zabove,zbelow,latabove,latbelow,lonabove,lonbelow,$
   altabove,altbelow,szaabove,szabelow,utcabove,utcbelow,wvl,i_above,i_below,$;tau_aats,lambda_aats,dtau_aats,$
   filename='/home/leblanc/CALNEX/p3/'+date+ll+date+'_spectra_save.out'
  nabove=nab & nbelow=nbe & zabove=zab & zbelow=zbe ;& tau_aats=taa & dtau_aats=dtaa
  if keyword_set(no_szacorr) then begin
    for i=0, n_elements(fl)-1 do begin
      zabove[*,i]=zab[*,fl[i]]*cos(sza_ref*!dtor)/cos(sza[near_leg2[fl[i]]]*!dtor)
      zbelow[*,i]=zbe[*,fl[i]]*cos(sza_ref*!dtor)/cos(sza[leg1[fl[i]]]*!dtor)
    endfor
  endif
  stop
endif

sza_arr=sza
print, 'number of wavelength to iterate:',iw_f
print, 'number of points to iterate:',n_elements(fl)
fl_f=n_elements(fl)-1 ;number of points to go through
fl_s=0; start of points
;ia_arr=[0,1,2,3,4,5,6]
;iw_f=0
;stop
if keyword_set(use_wvl) then begin
  iw_s=use_wvl
  iw_f=use_wvl
endif else iw_s=0
for iw=iw_s, iw_f do begin ;wavelength loop
  aero_wl=ia_arr[iw]
  wl_string=strtrim(string(fix(wvl_aod[aero_wl]),format='(I4.4)'),2)
  if not keyword_set(no_tau_loop) then file_n='rtm_taumod_' else file_n='rtm_'  ; set up file writing routine for retrieval (at every point)
  if keyword_set(angstrom) then file_n='rtm_angstrom_'
  if crd then file_n='rtm_crd_'
  if abs_min then file_n=file_n+'abs_'
  lunn=98  
  openw, lunn, dirout+file_n+date+'_wvl'+wl_string+secon+'.txt';,/get_lun
  printf, lunn, '#lat  lon     ssa   asy       asy2    albedo          correction    tau modification        flux divergence  model down  model up  tau'
  
  for i=fl_s, fl_f do begin ;track loop
    
    index=fl[i]  ; index for the point along track
    tau_out=dir+'aero_'+wl_string+'.level'
    write_input_tau_file, z, near, atm_in=atm_in, index=index, tau_out=tau_out, /quiet, alt=alt, tau_good=tau_alt
    
    input_file=dir+'aero_'+date+'_'+wl_string+'.inp'
    output_file=dirout+'aero_'+date+'_'+wl_string+'.out'
    if date eq '20100519' then doy= 139
    doy=julian_day(strmid(date,0,4),strmid(4,2),strmid(6,2))
    zensun, doy, utc[legs.leg1[index]],lat[legs.leg1[index]], lon[legs.leg1[index]], sza, azimuth, solfac
    zout=[0.,0.5,1.0] ; need to set to right altitudes of flight path currently good for may 19th
    
   if aeronet then  mm=min(abs(jul_day-doy+utc[legs.leg1[index]]/24.0),aeronet_index)  ;get the closest in time aeronet index
    
    wvl=[fix(wvl_arr[iw]),fix(wvl_arr[iw])]
    if fix(wvl_arr[iw]) ne fix(wvl_aod[aero_wl]) then message, 'wvl arrays not equal!'
    mm=max(zout, top)
    bottom=1
    mm=min(abs(nadlambda-wvl[0]),wvl_index)
    
    if aeronet then begin
      ;to find the tau aeronet at 532 (HSRL or CRD)
      tau_532=interpol(alog(aod_aeronet[ia_arr,aeronet_index]),alog(wvl_arr),alog(532.))
      tau_532=exp(tau_532)
      tauscale1=tau_s*aod_aeronet[aero_wl,aeronet_index]/tau_532
    endif else begin
      tauscale1=tau_s ;set it to the scaling factor only
    endelse
    
    ;starting values
    albedo   = near.nbelow[wvl_index, index] / near.zbelow_attcorr[wvl_index,index]
    z_top    = near.zabove_attcorr[wvl_index,index] 
    z_bottom = near.zbelow_attcorr[wvl_index,index]
    n_top    = near.nabove[wvl_index,index]
    n_bottom = near.nbelow[wvl_index,index] 
    correc_fac=1.

    ;make loops that writes input file, then runs uvspec, then reads the output, then modifies the different values
    
    ;tau_loop
    tau_loop=1 & count_tau=0 & tau_mod=1.0 & tau_init=tauscale1 & tau_old=0. & redo_tau=0
      
    tau_mods=findgen(22)*0.04+0.6  ;iterate through +/- 40%
    tau_mods=tau_mods[sort(abs(tau_mods-1.))]
    tau_mods=tau_mods[1:*]
    tau_mods[1]=1.02 & tau_mods[2]=0.98
    
    ;make array to keep g differences for each tau_mods for use with abs_min
    g_diffs=tau_mods*!values.f_nan
    t_mins =tau_mods*!values.f_nan
    ;initialise the different arrays that contains each of the different values associated with a change in tau
    ssa_min=g_diffs & asy_min=g_diffs & asy2_min=g_diffs & alb_min=g_diffs & corr_min=g_diffs & tnow_min=g_diffs & dif_min=g_diffs 

    ;new tau mods system
    ;tau_mods=[1.01,0.99,1.02,0.98,1.03,0.97,1.04,0.96,1.05,0.95,1.07,0.93,1.09,0.91,1.12,0.88,1.16,0.82,
    
    sep_asys=fltarr(21)
    old_tau_mod=fltarr(21)
    while tau_loop do begin
      if count_tau eq 0 then begin
        tauscale=tau_init
        ssa_ct=1
      endif
      count_tau=count_tau+1
    
      big_loop=1 & count_big=0
      old_count_ssa=0 & old_count_asy=0
   
      print, 'starting big loop' 
      ;start of big loop 
      while big_loop do begin
        count_big=count_big +1
        old_albedo=albedo & old_asy=asy & old_asy2=asy2 & old_ssa=ssa
    
        ct_big_asy2=0
      big_asy2:
    
        ; ssa loop
        if ssa_ct then ssa_loop=1 else ssa_loop=0
        count_ssa=0
      
        while ssa_loop do begin   ;in the ssa loop
          count_ssa=count_ssa +1
          if not finite(ssa) then message,'SSA out of range';read, ssa, prompt='SSA out of range! please enter new SSA (0-1): ('+strtrim(string(ssa),2)+') '
          write_input_file, doy, sza, tau_out, dir+atm_in, input_file, azimuth, asy, ssa, wvl, zout,albedo, /radiance,/quiet, tau_scale=tauscale, /slit
          if not cluster then spawn, '/home/leblanc/libradtran/libRadtran-1.5-beta/bin/uvspec < '+input_file+' > '+output_file , message
          if cluster then spawn, '/home/leblanse/libradtran/libRadtran-1.6-beta/bin/uvspec < '+input_file+' > '+output_file, message
          if n_elements(message) gt 10 then begin
            if message[10] eq 'Segmentation fault' then spawn, '/projects/leblanse/libradtran/libRadtran-1.6-beta/bin/uvspec < '+input_file+' > '+output_file,message
            if n_elements(message) gt 10 then message, message[10]
          endif
          output=libradtran_reader(file = output_file,/radiance,zout = zout,/quiet)
      
          ; get correction factor from the modeled downward irradiance (top) to the measured downward irradiance (top)
          correction=(output.dir_dn[top] + output.dif_dn[top])/near.zabove_attcorr[wvl_index,index]
          z_top    = near.zabove_attcorr[wvl_index,index] 
          z_bottom = near.zbelow_attcorr[wvl_index,index]
          n_top    = near.nabove[wvl_index,index]
          n_bottom = near.nbelow[wvl_index,index] 
          
          ;correct measurements to fit into model
          z_top    = z_top * correction
          z_bottom = z_bottom  * correction
          n_top    = n_top * correction
          n_bottom = n_bottom * correction
          correc_fac=correc_fac*correction
          ;to correct the albedo
          model_albedo=output.dif_up[bottom]/(output.dir_dn[bottom]+output.dif_dn[bottom])
          albedo_correction=(n_bottom/z_bottom)/model_albedo
          albedo=albedo*albedo_correction
          if not finite(albedo) then albedo=old_albedo
      
          ; change ssa for next iteration
          model=-(output.dif_up[top] - (output.dir_dn[top] + output.dif_dn[top]))+(output.dif_up[bottom] - (output.dir_dn[bottom] + output.dif_dn[bottom]))
          measured=-(n_top-z_top)+(n_bottom-z_bottom)
          dif_f=model
      
          if abs(model-measured) lt 0.001 or abs(model/measured) lt 0.000001 and correction gt corr_eps then begin ;if within epsilon of model and measured then proceed
            ssa_loop=0  ;converged
          endif else begin
            if model/measured lt 0 || correction le corr_eps then begin 
              if not abs_min then begin
                ssa_loop=0
                ssa=!values.f_nan
                asy=!values.f_nan
                dn=ssa & up=ssa
                tau_mod=ssa
                tau_now=tau_mod
	        print, 'correction factor outside range'
                goto, failed
              endif else begin
                ssa_loop=0
                asy=0.01 & asy2=0.9
                tau_now=!values.f_nan & dn=!values.f_nan & up=!values.f_nan
                goto, outside
              endelse
            endif else begin
	            if keyword_set(interactive_ssa) then begin
	          	 print, 'Curent SSA value at:', ssa, ' next value at:', ssa*(model/measured)^0.08
		           print, 'model value:', model, ' measured:',measured
		           read, ssa, prompt='enter new SSA value:'
              endif else ssa=ssa*(model/measured)^0.08 ;modify ssa for next value
            endelse
	         if not finite(ssa) then begin ;check to make sure the modification did not create a NaN
              if model gt 0 then print, 'model is off' , model
              if measured gt 0 then print, 'measured is off', measured
              ;stop
              if not abs_min then begin
                ssa_loop=0 & big_loop=0
                asy=!values.f_nan & asy2=!values.f_nan
                goto, failed
              endif else begin
                ssa_loop=0 & big_loop=0
                asy=0.1 & asy2=0.9
                tau_now=!values.f_nan & dn=!values.f_nan & up=!values.f_nan
                goto, outside
              endelse
            endif
            if ssa gt 1. then ssa=1.0
            if ssa le 0. then ssa=0.000001
          endelse
          
          if tau[index] le 0. then begin
                        ssa_loop=0
              ssa=!values.f_nan
              asy=!values.f_nan
              dn=ssa & up=ssa
              tau_mod=ssa
              tau_now=tau_mod
	      print, 'tau less than 0'
              ;stop
              goto, failed
          endif
      
          if count_ssa gt 50 then begin
            ssa_loop=0  ;did not converge
            print, 'did not converge'
            print, count_ssa
          endif
          if correction gt 2. then message, 'Correction great than 2 at ssa loop'
        endwhile  ;end of ssa loop 
      if count_ssa gt 50 then print, 'ssa did not converge for index: '+strtrim(string(index),2)

        ; asy loop
        asy_loop=1
        count_asy=0
      
        while asy_loop do begin
        count_asy=count_asy +1
        if use_wvl eq 2 and count_asy eq 1 then print, 'just entering asy loop'
        if not finite(asy) then read, asy, prompt='ASY out of range! please enter new ASY (0-1):'
        write_input_file, doy, sza, tau_out, dir+atm_in, input_file, azimuth, asy, ssa, wvl, zout,albedo, /radiance,/quiet, tau_scale=tauscale, /slit
        if linux and not cluster then spawn, '/home/leblanc/libradtran/libRadtran-1.5-beta/bin/uvspec < '+input_file+' > '+output_file,message
        if cluster then spawn, '/home/leblanse/libradtran/libRadtran-1.6-beta/bin/uvspec < '+input_file+' > '+output_file, message 
        if n_elements(message) gt 10 then begin
            if message[10] eq 'Segmentation fault' then spawn, '/projects/leblanse/libradtran/libRadtran-1.6-beta/bin/uvspec < '+input_file+' > '+output_file,message
            if n_elements(message) gt 10 then message, message[10]
          endif
output=libradtran_reader(file = output_file,/radiance,zout = zout,/quiet)
      
        ; get correction factor from the modeled downward irradiance (top) to the measured downward irradiance (top)
        correction=(output.dir_dn[top] + output.dif_dn[top])/near.zabove_attcorr[wvl_index,index]
        z_top    = near.zabove_attcorr[wvl_index,index] 
        z_bottom = near.zbelow_attcorr[wvl_index,index]
        n_top    = near.nabove[wvl_index,index]
        n_bottom = near.nbelow[wvl_index,index] 
        
        ;correct measurements to fit into model
        z_top    = z_top * correction
        z_bottom = z_bottom  * correction
        n_top    = n_top * correction
        n_bottom = n_bottom * correction
        correc_fac=correc_fac*correction
        
        ;to correct the albedo
        model_albedo=output.dif_up[bottom]/(output.dir_dn[bottom]+output.dif_dn[bottom])
        albedo_correction=(n_bottom/z_bottom)/model_albedo
        albedo=albedo*albedo_correction
        if not finite(albedo) then albedo=albedo_t1  ; set albedo back to initial value for non finite values (for next guess)
 
        ; change asy for next iteration
        model=(output.dir_dn[bottom] + output.dif_dn[bottom])
        measured=z_bottom
        dn=model
         
        if abs(model/measured) lt 0.000001 or abs(model-measured) lt 0.008*measured and correction gt corr_eps then begin
          asy_loop=0  ;converged
        endif else begin
          if correction le corr_eps then begin 
              if not abs_min then begin
                asy_loop=0
                ssa=!values.f_nan & asy=!values.f_nan
                dn=ssa & up=ssa & tau_mod=ssa & tau_now=tau_mod
	        print, 'correction factor is outside range'
                goto, failed
              endif else begin
                asy_loop=0
                asy=0.1 & asy=0.9
                goto, outside
              endelse
          endif
          asy_t=0
          if not asy_t then asy=asy*(measured/model)^1. else read, asy, prompt='asy:'
          if asy ge 0.98 then asy=0.9799
          if asy le 0. then asy=0.000001
        endelse
        if count_asy gt 40 then asy_loop=0  ;did not converge
      endwhile  ;end of asy loop
      if count_asy gt 40 then print, 'asy did not converge for index: '+strtrim(string(index),2)
            
      ; asy second loop to help retrieve tau
      asy2_loop=1 & count_asy2=0
    
      while asy2_loop do begin
        count_asy2=count_asy2 +1
        if not finite(asy) then read, asy2, prompt='ASY2 out of range! please enter new ASY2 (0-1):'
        write_input_file, doy, sza, tau_out, dir+atm_in, input_file, azimuth, asy2, ssa, wvl, zout,albedo, /radiance,/quiet, tau_scale=tauscale, /slit
        if linux and not cluster then spawn, '/home/leblanc/libradtran/libRadtran-1.5-beta/bin/uvspec < '+input_file+' > '+output_file,message
        if cluster then spawn, '/home/leblanse/libradtran/libRadtran-1.6-beta/bin/uvspec < '+input_file+' > '+output_file, message
        if n_elements(message) gt 10 then begin
            if message[10] eq 'Segmentation fault' then spawn, '/projects/leblanse/libradtran/libRadtran-1.6-beta/bin/uvspec < '+input_file+' > '+output_file,message
            if n_elements(message) gt 10 then message, message[10]
          endif
 output=libradtran_reader(file = output_file,/radiance,zout = zout,/quiet)
    
        ; get correction factor from the modeled downward irradiance (top) to the measured downward irradiance (top)
        correction=(output.dir_dn[top] + output.dif_dn[top])/near.zabove_attcorr[wvl_index,index]
        z_top    = near.zabove_attcorr[wvl_index,index] 
        z_bottom = near.zbelow_attcorr[wvl_index,index]
        n_top    = near.nabove[wvl_index,index]
        n_bottom = near.nbelow[wvl_index,index] 
        
        ;correct measurements to fit into model
        z_top    = z_top * correction
        z_bottom = z_bottom  * correction
        n_top    = n_top * correction
        n_bottom = n_bottom * correction
        correc_fac=correc_fac*correction
        
        ;to correct the albedo
        model_albedo=output.dif_up[bottom]/(output.dir_dn[bottom]+output.dif_dn[bottom])
        albedo_correction=(n_bottom/z_bottom)/model_albedo
        albedo=albedo*albedo_correction
        if not finite(albedo) then albedo=albedo_t1
 
        ; change asy2 for next iteration
        model=output.dif_up[top]
        measured=n_top
    
        up=model

        if abs(model/measured) lt 0.000001 or abs(model-measured) lt 0.002 and correction gt corr_eps then begin
          asy2_loop=0  ;converged
        endif else begin
          if correction le corr_eps then begin 
            if not abs_min then begin
              asy2_loop=0
              ssa=!values.f_nan & asy=!values.f_nan
              dn=ssa & up=ssa & tau_mod=ssa & tau_now=tau_mod
              print, 'correction factor is outside range'
	      goto, failed
            endif else begin
              asy2_loop=0
              asy=0.1 & asy2=0.9
              goto, outside
            endelse
          endif
          asy2=asy2*(model/measured)^1.0
          if asy2 ge 0.98 then asy2=0.9799
          if asy2 le 0. then asy2=0.000001
        endelse
        if count_asy2 gt 40 then begin
          asy2_loop=0  ;did not converge
        endif

      endwhile  ;end of asy2 loop
      if count_asy2 gt 40 then begin
        print, 'asy2 did not converge for index: '+strtrim(string(i),2)
          if ct_big_asy2 gt 1 then begin
            print, 'Convergence did not happen because asy2 did not converge on second try'
            goto, outside     
          endif else begin 
            if count_asy lt 40 then begin
              ct_big_asy2= ct_big_asy2 +1
              ;goto, big_asy2
            endif else ct_big_asy2=0
          endelse
      endif else ct_big_asy2=0
      
      print, 'Iteration of big loop: '+strtrim(string(count_big),2)+' on index: '+strtrim(string(i),2)+'/'+strtrim(string(fl_f),2)+' wavelength: '+strtrim(string(wvl[0]),2)
      print, 'ssa count, ssa'
      print, count_ssa, ssa
      print, 'asy count, asy'
      print, count_asy, asy
      print, 'asy2 count, asy2'
      print, count_asy2, asy2
      print, 'correction factor: '+strtrim(string(correction),2)
      if aats then tau_now=tau[index,aero_wl]*tau_mod*tau_init else tau_now=tau[index]*tau_mod*tau_init
      print, 'tau: ',tau_mod*tau_init, '  actual tau:', tau_now, '  - end big loop'
      

      if abs(ssa-old_ssa) lt 0.01 and abs(asy-old_asy) lt 0.01 and count_ssa lt 51 then big_loop=0   ;check for convergence of big loop
      if count_big gt 6 then big_loop=0   ;no convergence
      if old_count_ssa eq 51 and count_ssa eq 51 and old_count_asy eq 41 and count_asy eq 41 then big_loop=0 
      old_count_ssa=count_ssa & old_count_asy=count_asy
    endwhile  ;end if big_loop    
    ssa_ct=1
    if count_big gt 6 then begin
      print, 'convergence did not happen during big loop for index: '+strtrim(string(index),2)
      if not no_tau_loop then begin
        print,'Modifying tau'      
      endif else begin
        if not abs_min then begin
          tau_mod=!values.f_nan & asy=!values.F_NAN & asy2=!values.F_NAN
          ssa=!values.f_nan & tau_now=tau_mod & tau_loop=0
          goto, failed
        endif else begin
          asy=0.1 & asy2=0.9 
          goto,outside
        endelse
      endelse
    endif

    if abs(asy-asy2) gt 0.5 and not abs_min then begin
      print, 'divergence of asy and asy2 too large, retrieval failed: ',abs(asy-asy2) 
      tau_mod=!values.f_nan & asy=!values.F_NAN &  asy2=!values.F_NAN
      ssa=!values.f_nan & tau_now=tau_mod &  tau_loop=0
      goto, failed
    endif
      
    outside:
    if n_elements(count_asy) eq 0 then count_asy=41
    if n_elements(count_asy2) eq 0 then count_asy2=41
    if tau_mod eq 1.0 then begin    ;save first values, for when the tau retrieval fails
      asy_t1=asy & asy2_t1=asy2 & ssa_t1=ssa & albedo_t1=albedo
    endif
    if no_tau_loop then tau_loop=0
    if ct_big_asy2 gt 1 and count_asy lt 40 then begin
      if not abs_min then begin
        tau_mod=!values.f_nan & asy=!values.F_NAN &  asy2=!values.F_NAN
        ssa=!values.f_nan & tau_now=tau_mod &  tau_loop=0
        goto, failed
      endif else begin
        asy=0.0 & asy2=1.0
        tau_mod=!values.f_nan & ssa=!values.f_nan & tau_now=tau_mod
      endelse
    endif
    
    if keyword_set(sensitivity) then begin  ;to write values of asy, asy2 compared to change of tau
      openw, 94, dirout+'tau_'+date+'_ssact_wvl'+strtrim(string(fix(lambda[aero_wl]),format='(I4.4)'),2)+'.dat',/append
      printf, 94, tau_mod, tau_now, ssa, asy, asy2, correction
      close, 94   
    endif
    
    ; part of code to get convergence of asy2 and asy2 by modifying tau slightly
    if (abs(asy-asy2) lt 0.025) and not keyword_set(sensitivity) and not abs_min and count_ssa lt 51 and count_asy lt 41 and count_asy2 lt 41 then begin
      print, 'tau converged at:', tau_mod*tau_init, ' with tau equals:',tau_now
      if keyword_set(constant_ssa) then begin
        ssa_ct=1      ; redo ssa
        tau_loop=1    ; go back to redo tau
        redo_tau=redo_tau+1
        print, 'in constant ssa tau loop, with tau-now, tau_old, diff:',tau_now,tau_old, abs(tau_now-tau_old)

        if abs(tau_now-tau_old) lt 0.05*tau_now then begin 
          tau_loop=0  ;exit new tau loop
        endif
        tau_old=tau_now
      endif else begin
        tau_loop=0 ; exit old tau loop
      endelse
    endif else begin
      if not no_tau_loop then begin 
      ;  ssa=0.9 & asy = 0.6 & asy2 = 0.6
        if keyword_set(constant_ssa) then ssa=ssa_t1
        if keyword_set(interactive) then begin
          print, 'current tau mod set at:', tau_mod, ' which gives a total tau of:', tau_now
          read, tau_mod, prompt='enter new tau mod: '
        endif else begin
        if 0 then begin ;testing of the sep_asys method, disabled for now
          sep_asys[count_tau-redo_tau-1]=abs(asy-asy2)
          old_tau_mod[count_tau-redo_tau-1]=tau_mod
          if count_tau-redo_tau-1 ge 2 then begin
            if count_tau-redo_tau-1 eq 2 then begin
              nul= min(sep_asys[[0,1,2]],iy)
              ist=0 & isp=0
              case iy of
                0:begin
                  nul= min(sep_asys[[1,2]],iy2)
                  if iy2 eq 0 then tau_mod=mean(tau_mods[[0,1]]) else tau_mod=mean(tau_mods[[0,2]])
                  directions=1.0
                end
                1:begin 
                  tau_mod=tau_mod+0.02
                  directions=1.0
                end
                2:begin
                  tau_mod=tau_mod-0.02
                  directions=-1.0
                end
              endcase 
            endif else begin
              if sep_asys[count_tau-redo_tau-1] le sep_asys[count_tau-redo_tau-2] then begin
                if isp gt 10 then begin
		  directions=directions*(-1.0)
                  isp=0
		  tau_mod=1.0+0.04*directions
		endif else begin
		  isp=isp+1
		  tau_mod=old_tau_mod[count_tau-redo_tau-1]+0.04*directions
                endelse
	      endif else begin
                if sep_asys[count_tau-redo_tau-1] le sep_asys[count_tau-redo_tau-3] then begin
		  if ist gt 5 then begin
                    directions=directions*(-1.0)
                    tau_mod=2.0-old_tau_mod[count_tau-redo_tau-1] ;switch directions and check out another point
                  endif else begin   
                    tau_mod=(old_tau_mod[count_tau-redo_tau-1]+old_tau_mod[count_tau-redo_tau-2])/2.             
                    ist=ist+1
                  endelse
		endif else begin
                  tau_mod=(old_tau_mod[count_tau-redu_tau-2]+old_tau_mod[count_tau-redp_tau-3])/2.
		endelse
              endelse
            endelse
          endif else begin         
            tau_mod=tau_mods[count_tau-redo_tau-1]
          endelse
        endif else begin ;endif of the testing of the sep_asys start of the normal routine
          if asy ne 0.9799 and asy2 ne 0.9799 then g_diffs[count_tau-redo_tau-1]=abs(asy-asy2) else $
            g_diffs[count_tau-redo_tau-1]=2.0 ;sets the g_diffs for use with abs_min keyword
          t_mins[count_tau-redo_tau-1]=tau_mod
          asy_min[count_tau-redo_tau-1]=asy
          asy2_min[count_tau-redo_tau-1]=asy2
          ssa_min[count_tau-redo_tau-1]=ssa
          corr_min[count_tau-redo_tau-1]=correction
          alb_min[count_tau-redo_tau-1]=albedo
          dif_min[count_tau-redo_tau-1]=dif_f
          tnow_min[count_tau-redo_tau-1]=tau_now
          tau_mod=tau_mods[count_tau-redo_tau-1]
          print, 'On the '+strtrim(count_tau-redo_tau-1)+'/'+strtrim(n_elements(g_diffs))+' tau iteration of the current point'
          ;stop
        endelse ;endelse of the abs_min section
        endelse ;endelse of the interactive if
        ssa=0.9 & asy = 0.6 & asy2 = 0.6
      endif else tau_mod=1.0
      tauscale=tau_init*tau_mod
    endelse
    if count_tau gt 20 then begin
      tau_loop=0
      if not abs_min then begin
        print, 'tau did not converge for index: '+strtrim(string(index),2)
        tau_mod=!values.f_nan & tau_now=tau_mod & asy=asy_t1
        asy2=asy2_t1 & ssa=ssa_t1 & albedo=albedo_t1 & tau_loop=0
        goto, failed
      endif else goto, failed
    endif        
  endwhile   ; end of tau loop

  failed:
  nul=!values.f_nan
    ; if statement to find the smallest g difference and its equivalent values. must save the values as well...
  if abs_min then begin
    min_g=min(abs(g_diffs),i_min,/nan)
    if min_g gt 0.05 then begin
      ssa    = ssa_min[i_min]  & asy       = asy_min[i_min]  & asy2    = asy2_min[i_min]
      albedo = alb_min[i_min]  & correction= corr_min[i_min] & tau_mod = t_mins[i_min]
      tau_now= tnow_min[i_min] & dif_f     = dif_min[i_min]
    endif else begin
      ssa    = nul & asy       = nul & asy2    = nul
      albedo = nul & correction= nul & tau_mod = nul
      tau_now= nul & dif_f     = nul
    endelse
  endif
  ssa_arr[i]=ssa & asy_arr[i]=asy & asy2_arr[i]=asy2
  albedo_arr[i]=albedo & correc_fac_arr[i]=correction
  tau_mod_arr[i]=tau_mod & tau_arr[i]=tau_now & dif_flux[i] = dif_f
  if count_big gt 6 then begin
    print, 'convergence did not happen during big loop for index: '+strtrim(string(index),2)
    ssa=.9 & asy=0.6 & asy2=asy
  endif
  ssa=.9 & asy=0.6 & asy2=asy
  if keyword_set(constant_ssa) then ssa=ssa_t1
  ; file writing routine for retrieval results
  printf, lunn, lat[legs.leg1[fl[i]]],lon[legs.leg1[fl[i]]], ssa_arr[i], asy_arr[i], asy2_arr[i], albedo_arr[i], correc_fac_arr[i],tau_mod_arr[i], dif_flux[i],dn,up,tau_arr[i], format='(12F)'

endfor  ;longitude loop
 free_lun, lunn ;retrieval output file
endfor ; wavelength loop  
  
;stop
end
  
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write input file program                                                    ;;
;; used to make the input file for uvspec                                      ;;
;; use radiance keyword to set radiance (zenith pointing)                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro write_input_file, doy, sza, tau_file, atm_file, input_file, azimuth, asy, ssa, wvl, zout, albedo, radiance=radiance, quiet=quiet, tau_scale=tau_scale, slit=slit

ui=99
openw,ui,input_file;,/get_lun
printf,ui,'data_files_path   /projects/leblanse/libradtran/libRadtran-1.6-beta/data'
printf,ui,'solar_file        /projects/leblanse/libradtran/libRadtran-1.6-beta/data/solar_flux/kurudz_1.0nm.dat' 
printf,ui,'atmosphere_file   '+atm_file

;printf,ui,'albedo_library    IGBP    # Spectral albedo library '
;printf,ui,'surface_type      13      # urban albedo from library'
if albedo lt 0 then albedo=albedo*(-1.)
if albedo gt 1.0 then albedo=1.0
printf,ui,'albedo            '+string(albedo)  ; 

printf,ui,'rte_solver        disort2'       
printf,ui,'nstr              20'            
printf,ui,'correlated_k      SBDART'        ; pseudo-spectral definition of atmospheric absorption

printf,ui,'sza               '+string(sza)    ; solar zenith angle (deg)
printf,ui,'day_of_year       '+string(doy)    ; day of year for Sun-Earth distance

if keyword_set(radiance) then begin
  printf,ui,'phi0              '+string(azimuth+180.0); solar azimuth angle (deg)
  printf,ui,'umu               -1  # -1:downward radiance; 1:upward radiance; 0:sidward radiance' ;for radiances (ship)
  printf,ui,'phi               270.0   # output azimuth angle (flight-direction: NNW = 350°)' ;for radiances(ship direction)
endif

printf,ui,'aerosol_default'                   ; initialize aerosol
printf,ui,'aerosol_tau_file  '+tau_file
printf,ui,'aerosol_set_ssa   '+string(ssa)  
printf,ui,'aerosol_set_gg    '+string(asy)      
if keyword_set(tau_scale) then printf, ui, 'aerosol_scale_tau   '+string(tau_scale)

printf,ui,'wavelength        '+string(wvl[0])+'  '+string(wvl[1]) ; wavelengths
printf,ui,'altitude          0.0' ;0.263 # elevation (ASL) of CalTech in km'      
printf,ui,'zout              '+string(zout, format='('+strtrim(string(n_elements(zout)),2)+'(" ",F5.1))')

if keyword_set(slit) then begin ; if set, use slit function, must change for IngAas
  if wvl[0] le 940 then printf,ui,'slit_function_file /projects/leblanse/libradtran/vis_1nm.dat' else printf,ui,'slit_function_file /projects/leblanse/libradtran/nir_1nm.dat'
endif

printf,ui,'quiet'
free_lun,ui

if not keyword_set(quiet) then print, 'input file saved to: '+input_file

end

