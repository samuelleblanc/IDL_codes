; program to plot the modeled and measured spectra for each retrieved point
; takes in a modeled spectra lut and interpolates to a smaller grid
; is only used for specific times


pro plot_mod_meas_sp

fn='/argus/roof/SSFR3/model/' ;sp_hires_20120524.out'
dir='/home/leblanc/SSFR3/plots/'

norm=1

restore, fn+'sp_hires4_20120524.out'

;now interpolate the spectra to smaller grid spacing
sps=fltarr(200,50,2,378)
sptm=fltarr(24,50,2,378)

tau_hires=findgen(200)+1.
ref_hires=findgen(50)+1.


; for special considerations because liquid water is not present above 25.
nrmax=where(ref eq 25.)
for v=0,377 do begin
  for w=0,1 do begin
    for t=0,23 do $
      if 0 then sptm[t,*,w,v]=interpol(sp[t,0:nrmax,v,w],ref[0:nrmax],ref_hires,/nan) $
       else sptm[t,*,w,v]=interpol(sp[t,*,v,w],ref,ref_hires,/nan)
    for r=0, 49 do sps[*,r,w,v]=interpol(sptm[*,r,w,v],tau,tau_hires,/nan)
  endfor
endfor 

;load up the retrieved values

;restore the spectra with wavelength
wvl=zenlambda
restore, '/argus/roof/SSFR3/model/20120525_calibspcs.out'
nul=min(abs(tmhrs-15.4),ut)
spu=zspectra[*,ut]

lbls=['20120525'] & p=0
fp='/argus/roof/SSFR3/retrieved/'
restore, fp+'retrieved_pdf_'+lbls[p]+'_limp2_v2.out' ;'_limp.out'
  tau_rtm1=tau_rtm & ref_rtm1=ref_rtm & wp_rtm1=wp_rtm & tau_err1=tau_err & ref_err1=ref_err & wp_err1=wp_err
  restore, fp+'retrieved_'+lbls[p]+'_pat.out' ;_sp20.out'
  tau_rtm2=tau_rtm & ref_rtm2=ref_rtm & wp_rtm2=wp_rtm & tau_err2=tau_err & ref_err2=ref_err & wp_err2=wp_err
  restore, fp+'retrieved_'+lbls[p]+'_2wvl.out' ;_sp20.out'
  tau_rtm3=tau_rtm & ref_rtm3=ref_rtm & wp_rtm3=wp_rtm & tau_err3=tau_err & ref_err3=ref_err & wp_err3=wp_err

  nul=min(abs(tmhrs-15.4),ut)
  ta1=tau_rtm1[ut] & ta2=tau_rtm2[ut] & ta3=tau_rtm3[ut]
  re1=ref_rtm1[ut] & re2=ref_rtm2[ut] & re3=ref_rtm3[ut]

ta1=32.  & re1=9.0
ta2=44.  & re2=4.6
ta3=40.6 & re3=3.3

;stop

  ; for multi
  nul=min(abs(ta1-tau_hires),nt1) & nul=min(abs(re1-ref_hires),nr1)
  sp1=reform(sps[nt1,nr1,0,*])
  sp1=sp1/max(sp1)
  ; for pat
  nul=min(abs(ta2-tau_hires),nt2) & nul=min(abs(re2-ref_hires),nr2)
  sp2=reform(sps[nt2,nr2,0,*])
  sp2=sp2/max(sp2)
  ; for 2 wvl
  nul=min(abs(ta3-tau_hires),nt3) & nul=min(abs(re3-ref_hires),nr3)
  sp3=reform(sps[nt3,nr3,0,*])
  sp3=sp3/max(sp3)


;  restore, '/home/leblanc/SSFR3/data/model_sp_20120525_15.4.out'
;  restore, '~/SSFR3/data/sp_model_20120525_15.4.out'
  restore, '~/SSFR3/data/sp_model_20120525_154.out'
  sp1=sp1/1000.
  sp2=sp2/1000.
  sp3=sp3/1000.


if norm then begin
  sp1=sp1/max(sp1) & sp2=sp2/max(sp2) & sp3=sp3/max(sp3)
endif 

;now do the plotting
    fp=dir+'sp_compare'
  print, 'making plot :'+fp
  set_plot, 'ps'
  loadct, 39, /silent
  device, /encapsulated, /tt_font, set_font='Helvetica Bold',/color,bits_per_pixel=8., filename=fp+'.ps'
  device, xsize=40, ysize=20
   !p.font=1 & !p.thick=6 & !p.charsize=2.8 & !x.style=1 & !y.style=1 & !z.style=1 & !y.thick=1.8 & !x.thick=1.8
   !p.multi=[0,2,1] & !x.margin=[6,1] & !y.margin=[3,1]
 
  tvlct, 150,0,150,40
  tvlct, 150,170,0,253
  tvlct, 255,125,0,133
if norm then spa=sp_arr[*,ut]/max(sp_arr[*,ut]) else spa=sp_arr[*,ut]

  plot, zenlambda, spa, xtitle='Wavelength (nm)',ytitle='Normalized radiance',xr=[400,1700],xmargin=[6,2]
  oplot,wvl,smooth(sp1,18), color=40
  oplot,wvl,smooth(sp2,18), color=253
  oplot,wvl,smooth(sp3,18), color=133

  legend,['Measured','Multi-parameter: ','   !9t!X=32, r!De!N=9 !9m!Xm','Slope: ','   !9t!X=44, r!De!N=5 !9m!Xm','2-wavelengths: ','   !9t!X=41, r!De!N=3 !9m!Xm'],textcolors=[0,40,40,253,253,133,133],box=0,position=[950,0.95]

  tvlct, 200,200,200,200

  plot, zenlambda, spa,/nodata, xtitle='Wavelength (nm)',ytitle='Normalized radiance difference!CModeled-Measured',yr=[-0.1,0.1],xr=[400,1700],xmargin=[7,1]
  polyfill, [550,680,680,550,550],[-0.1,-0.10,0.1,0.1,-0.1], color=200
  plots, [998,998],[-0.1,0.1],color=200
  plots, [1067,1067],[-0.1,0.1],color=200
  plots, [1200,1200],[-0.1,0.1],color=200
  polyfill, [1245,1270,1270,1245],[-0.1,-0.1,0.1,0.1],color=200
  plots, [1500,1500],[-0.1,0.1],color=200
  polyfill, [1565,1640,1640,1565],[-0.1,-0.1,0.1,0.1],color=200

  plots,[400,1700],[0,0],linestyle=2
  spi=interpol(sp_arr[*,ut],zenlambda,wvl)
  spi=spi/max(spi)
  oplot,wvl,smooth(sp1-spi,18), color=40
  oplot,wvl,smooth(sp2-spi,18), color=253
  oplot,wvl,smooth(sp3-spi,18), color=133
 ;  legend,['Multi-parameter','Slope','2 wavelengths'],textcolors=[40,253,133],box=0,/right
 ; usersym, [ -1, 1, 1, -1, -1 ,1], [ 1, 1, -1, -1, 1 ,1], /fill
 ; legend, ['Multi-parameter retrieval wavelengths'],textcolors=[200],color=[200],box=0,/right,psym=[8],/fill

  device, /close
  spawn, 'convert '+fp+'.ps '+fp+'.png'

stop
end
