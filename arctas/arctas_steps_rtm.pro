; to make plots, using retrieval, of step by step...
@write_input_aats.pro
@/home/leblanc/libradtran/pro/libradtran_reader.pro

pro arctas_steps_rtm
date='20080709'
arc_dir='/home/leblanc/arctas/nasa/'+date+'/'
atm_in = 'atmos_aero_'+date+'_at.dat'   ;input atmosphere file
  dir='/home/leblanc/libradtran/input/aero/'
  dirout='/home/leblanc/libradtran/output/aero/'
restore, arc_dir+date+'_spectra_save.out'
restore, arc_dir+date+'_forcing.out'
linux=1

n=137
alt_top=altabove[n]/1000.
alt_bot=altbelow[n]/1000.
zout=[0,alt_bot,alt_top]
doy=182
sza=szabelow[n]

azimuth=0.
top=2
bot=1
;models
z_t=fltarr(13)
z_b=fltarr(13)
n_t=fltarr(13)
n_b=fltarr(13)
alb=fltarr(13)

asy_wl=replicate(0.75,13)
ssa_wl=replicate(0.95,13)
albedo_wl=albedo[*,n]
;asy_wl=asy[*,n]+0.02
;ssa_wl=ssa[*,n] + 0.02
;ssa_wl=[0.886,0.913,0.912,0.896331,0.894346,0.895960,0.871312,0.860194,0.833237,0.845567,0.814467,0.710587,0.707240,0.803557]
;      [0.88,0.91,0.91,0.894331,0.891346,0.897960,0.871312,0.862194,0.833237,0.807567,0.814467,0.710587,0.707240,0.803557]
;wvl loop
for i=0, 12 do begin


wl=wvl_arr[i]
mm=min(abs(wvl-wl),wvl_index)

alb[i]=nbelow[wvl_index,n]/zbelow[wvl_index,n]

write_input_aats, tau_aats[n,i], alt_top, alt_bot, tau_aats[n,i]*0.1 ,$
  atm_in=atm_in, index=index, tau_out=tau_out, /quiet, tau_approx=0
       
    input_file=dir+'aero_'+date+'.inp'
    output_file=dirout+'aero_'+date+'.out'


write_input_file, doy, sza, dir+tau_out, dir+atm_in, input_file, azimuth, asy_wl[i], ssa_wl[i], [wl,wl], zout,alb[i], /radiance,/quiet
        if linux then spawn, '/home/leblanc/libradtran/libRadtran-1.5-beta/bin/uvspec < '+input_file+' > '+output_file,message else message, 'Must be under linux'
        if message ne '' gt 0 then message, message
        output=libradtran_reader(file = output_file,/radiance,zout = zout,/quiet)

		correction=zabove[wvl_index,n]/(output.dir_dn[top] + output.dif_dn[top])
		
		z_t[i]=(output.dif_dn[top]+output.dir_dn[top])*correction
		z_b[i]=(output.dif_dn[bot]+output.dir_dn[bot])*correction
		n_t[i]=output.dif_up[top]*correction
		n_b[i]=output.dif_up[bot]*correction
endfor


set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated
 device, /tt_font, set_font='Helvetica Bold'
 device, filename=arc_dir+'rtm_example_1.ps'
 device,/color,bits_per_pixel=8.
 device, xsize=60, ysize=20
 !x.style=1
 !p.charsize=4.8 
 !p.multi=[0,3,1]
 !p.font=1
 !p.thick=5
 !y.style=1 
 !z.style=1
 !y.thick=1.8
 !x.thick=1.8
 !y.omargin=[0,0]

 ;parameters
 plot, wvl_arr, ssa_wl, title='Input parameters spectra', xtitle='wavelength (nm)', yrange=[0,1], ystyle=8, xmargin=[8,4], xrange=[340.,1020.]
 oplot, wvl_arr, asy_wl, color=150
 oplot, wvl_arr, alb, color=70
 
 oplot, wvl_arr, ssa_wl, psym=7, symsize=1
 oplot, wvl_arr, asy_wl, color=150, psym=7, symsize=1
 oplot, wvl_arr, alb, color=70, psym=7, symsize=1
   
xyouts, 0.03,0.51,'g', orientation=90, /normal, color=150		;asy
xyouts, 0.03,0.4,'!9a!3',orientation=90, /normal, color=70		;albedo
xyouts, 0.03,0.45, '!9'+string(118B)+'!4',orientation=90,/normal	;ssa

axis, yaxis=1,ystyle=1, yrange=[0.0,1.3],/save,color=250
oplot, wvl_arr, tau_aats[n,*],color=250
oplot, wvl_arr, tau_aats[n,*],color=250, psym=7, symsize=1
xyouts, 0.34,0.45, '!9t!4',orientation=90,/normal, color=250		;tau

;flux divergence
plot, wvl_arr, (z_t-n_t)-(z_b-n_b), title='Absorption', xtitle='wavelength (nm)', yrange=[0,0.450],ytitle='Irradiance (w/m!U2!N/nm)',psym=7,symsize=2
oplot, wvl_arr,(z_t-n_t)-(z_b-n_b), thick=0.5
oplot, wvl, (zabove[*,n]-nabove[*,n])-(zbelow[*,n]-nbelow[*,n]) , color=250

;transmitted downwelling
;plot, wvl_arr, z_b, title='Downwelling F!Ibot!N', xtitle='wavelength (nm)', yrange=[0,1.4],ytitle='Irradiance (w/m!U2!N/nm)', psym=7, symsize=2
;oplot, wvl_arr,z_b, thick=0.5
;oplot, wvl, zbelow[*,n] , color=250

legend,['Model','Measured'],textcolors=[0,250],box=0,charsize=1.8

device, /close
spawn, 'convert "'+arc_dir+'rtm_example_1.ps" "'+arc_dir+'rtm_example_1.png"'
spawn, 'rm -f "'+arc_dir+'rtm_example_1.ps"'




end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write input file program                                                    ;;
;; used to make the input file for uvspec                                      ;;
;; use radiance keyword to set radiance (zenith pointing)                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro write_input_file, doy, sza, tau_file, atm_file, input_file, azimuth, asy, ssa, wvl, zout, albedo, radiance=radiance, quiet=quiet, tau_scale=tau_scale
close, /all
ui=99
openw,ui,input_file;,/get_lun
printf,ui,'data_files_path   /home/leblanc/libradtran/libRadtran-1.5-beta/data'
printf,ui,'solar_file        /home/leblanc/libradtran/libRadtran-1.5-beta/data/solar_flux/kurudz_0.1nm.dat' 
printf,ui,'atmosphere_file   '+atm_file

;printf,ui,'albedo_library    IGBP    # Spectral albedo library '
;printf,ui,'surface_type      13      # urban albedo from library'
if albedo lt 0 then albedo=albedo*(-1.)
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

printf,ui,'wavelength        '+string(wvl[0])+'  '+string(wvl[1]) ; wavelength used for hsrl
printf,ui,'altitude          0.0' ;0.263 # elevation (ASL) of CalTech in km'      
printf,ui,'zout              '+string(zout, format='('+strtrim(string(n_elements(zout)),2)+'(" ",F5.1))')

printf,ui,'quiet'
free_lun,ui

if not keyword_set(quiet) then print, 'input file saved to: '+input_file
end
