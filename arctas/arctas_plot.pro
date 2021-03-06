@errploty.pro
pro arctas_plot
date='20080709'
dir='/home/leblanc/libradtran/output/aero/'
;dir='/home/leblanc/arctas/nasa/'+date+'/janus/'

if (1) then begin
  ; get tau modified
  f=file_search(dir+'rtm_hsrl_'+date+'_wvl????.txt')
  f=f[sort(f)]
  print, f
    
  ;dir='/home/leblanc/arctas/nasa/'+date+'/'
  wvl0353=read_ascii(f[0], data_start=1,COMMENT_SYMBOL='#')
  wvl0380=read_ascii(f[1], data_start=1,COMMENT_SYMBOL='#')
  wvl0452=read_ascii(f[2], data_start=1,COMMENT_SYMBOL='#')
  wvl0499=read_ascii(f[3], data_start=1,COMMENT_SYMBOL='#')
  wvl0519=read_ascii(f[4], data_start=1,COMMENT_SYMBOL='#')
  wvl0605=read_ascii(f[5], data_start=1,COMMENT_SYMBOL='#')
  wvl0675=read_ascii(f[6], data_start=1,COMMENT_SYMBOL='#')
  wvl0779=read_ascii(f[7], data_start=1,COMMENT_SYMBOL='#')
  wvl0864=read_ascii(f[8], data_start=1,COMMENT_SYMBOL='#')
  wvl1019=read_ascii(f[9], data_start=1,COMMENT_SYMBOL='#')
  wvl1241=read_ascii(f[10], data_start=1,COMMENT_SYMBOL='#')
  wvl1558=read_ascii(f[11], data_start=1,COMMENT_SYMBOL='#')
  wvl2139=read_ascii(f[12], data_start=1,COMMENT_SYMBOL='#')
  
  ;wvl0353.field01=wvl0353.field01[*,0:29]
  ;wvl0380.field01=wvl0380.field01[*,0:29]
  ;wvl0452.field01=wvl0452.field01[*,0:29]
  ;wvl0499.field01=wvl0499.field01[*,0:29]
  ;wvl0519.field01=wvl0519.field01[*,0:29]
  ;wvl0605.field01=wvl0605.field01[*,0:29]
  ;wvl0675.field01=wvl0675.field01[*,0:29]
  ;wvl0779.field01=wvl0779.field01[*,0:29]
  ;wvl0864.field01=wvl0864.field01[*,0:29]
  ;wvl1019.field01=wvl1019.field01[*,0:29]
  ;wvl1241.field01=wvl1241.field01[*,0:29]
  ;wvl1558.field01=wvl1558.field01[*,0:29]
  ;wvl2=[[wvl2139.field01[*,0:15]],[wvl1558.field01[*,16:*]*!values.f_nan]]
  ;wvl2139={field01:wvl2}

endiF
print, 'getting non tau modified'
if (0) then begin
  ; get non tau modified
  dir='/home/leblanc/libradtran/output/aero/'
  f=file_search(dir+'rtm_'+date+'_wvl*.txt')
  f=f[sort(f)]
 
  print, f 
  dir='/home/leblanc/arctas/nasa/'+date+'/'
  wvl0353_1=read_ascii(f[0], data_start=1)
  wvl0380_1=read_ascii(f[1], data_start=1)
  wvl0452_1=read_ascii(f[2], data_start=1)
  wvl0499_1=read_ascii(f[3], data_start=1)
  wvl0519_1=read_ascii(f[4], data_start=1)
  wvl0605_1=read_ascii(f[5], data_start=1)
  wvl0675_1=read_ascii(f[6], data_start=1)
  wvl0779_1=read_ascii(f[7], data_start=1)
  wvl0864_1=read_ascii(f[8], data_start=1)
  wvl1019_1=read_ascii(f[9], data_start=1)
  wvl1241_1=read_ascii(f[10], data_start=1)
  wvl1558_1=read_ascii(f[11], data_start=1)
  wvl2139_1=read_ascii(f[12], data_start=1)
endif
dir='/home/leblanc/arctas/nasa/'+date+'/'
print, 'getting retrieval error'
;get errors in retrievals
error=fltarr(6,13)
openr, 95, '/home/leblanc/arctas/nasa/rtm_error.out'
line=' '
readf, 95, line
readf, 95, error
close,/all
error=error*0.01+0.1
;error[5,*]=error[5,*]+0.05

;;;; loop to verify that the g and g_hat match within epsilon ;;;;;
if (0) then begin
fl=where(abs(wvl0353.field01[3,*]-wvl0353.field01[4,*]) lt 0.02 and finite(wvl0353.field01[3,*]) eq 1 and finite(wvl0353.field01[4,*]) eq 1)
wvl0353.field01=wvl0353.field01[*,fl]
fl=where(abs(wvl0380.field01[3,*]-wvl0380.field01[4,*]) lt 0.02 and finite(wvl0380.field01[3,*]) eq 1 and finite(wvl0380.field01[4,*]) eq 1)
wvl0380.field01=wvl0380.field01[*,fl]
fl=where(abs(wvl0452.field01[3,*]-wvl0452.field01[4,*]) lt 0.02 and finite(wvl0452.field01[3,*]) eq 1 and finite(wvl0452.field01[4,*]) eq 1)
wvl0452.field01=wvl0452.field01[*,fl]
fl=where(abs(wvl0499.field01[3,*]-wvl0499.field01[4,*]) lt 0.02 and finite(wvl0499.field01[3,*]) eq 1 and finite(wvl0499.field01[4,*]) eq 1)
wvl0499.field01=wvl0499.field01[*,fl]
fl=where(abs(wvl0519.field01[3,*]-wvl0519.field01[4,*]) lt 0.02 and finite(wvl0519.field01[3,*]) eq 1 and finite(wvl0519.field01[4,*]) eq 1)
wvl0519.field01=wvl0519.field01[*,fl]
fl=where(abs(wvl0605.field01[3,*]-wvl0605.field01[4,*]) lt 0.02 and finite(wvl0605.field01[3,*]) eq 1 and finite(wvl0605.field01[4,*]) eq 1)
wvl0605.field01=wvl0605.field01[*,fl]
fl=where(abs(wvl0675.field01[3,*]-wvl0675.field01[4,*]) lt 0.02 and finite(wvl0675.field01[3,*]) eq 1 and finite(wvl0675.field01[4,*]) eq 1)
wvl0675.field01=wvl0675.field01[*,fl]
fl=where(abs(wvl0779.field01[3,*]-wvl0779.field01[4,*]) lt 0.02 and finite(wvl0779.field01[3,*]) eq 1 and finite(wvl0779.field01[4,*]) eq 1)
wvl0779.field01=wvl0779.field01[*,fl]
fl=where(abs(wvl0864.field01[3,*]-wvl0864.field01[4,*]) lt 0.02 and finite(wvl0864.field01[3,*]) eq 1 and finite(wvl0864.field01[4,*]) eq 1)
wvl0864.field01=wvl0864.field01[*,fl]
fl=where(abs(wvl1019.field01[3,*]-wvl1019.field01[4,*]) lt 0.02 and finite(wvl1019.field01[3,*]) eq 1 and finite(wvl1019.field01[4,*]) eq 1)
wvl1019.field01=wvl1019.field01[*,fl]
fl=where(abs(wvl1241.field01[3,*]-wvl1241.field01[4,*]) lt 0.02 and finite(wvl1241.field01[3,*]) eq 1 and finite(wvl1241.field01[4,*]) eq 1)
fl=[0]
wvl1241.field01=wvl1241.field01[*,fl]
fl=where(abs(wvl1558.field01[3,*]-wvl1558.field01[4,*]) lt 0.02 and finite(wvl1558.field01[3,*]) eq 1 and finite(wvl1558.field01[4,*]) eq 1)
fl=[0]
wvl1558.field01=wvl1558.field01[*,fl]
fl=where(abs(wvl2139.field01[3,*]-wvl2139.field01[4,*]) lt 0.02 and finite(wvl2139.field01[3,*]) eq 1 and finite(wvl2139.field01[4,*]) eq 1)
wvl2139.field01=wvl2139.field01[*,fl]
endif

dir = '/home/leblanc/arctas/nasa/'+date+'/'
restore, '/home/leblanc/arctas/nasa/'+date+'/'+date+'_tau_aats.sav'
restore,'/home/leblanc/arctas/nasa/'+date+'/'+date+'_spectra_save.out'
;dtau_aats=dtau_aats*0.0 +0.01
dtau_aats=dtauaats
;dtau_aats ; errors in tau_aats
if (1) then begin
  set_plot, 'ps'
  loadct, 39, /silent
  device, /encapsulated
  device, /tt_font, set_font='Helvetica Bold'
  device, filename=dir+'rtm_tau.ps'
  device,/color,bits_per_pixel=8.
  device, xsize=20, ysize=20
  !p.font=1
  !p.thick=5
  !p.charsize=1.8
  !x.style=0
  !y.style=1 
  !z.style=1
  !y.thick=1.8
  !x.thick=1.8
  !p.multi=0
  !y.omargin=[0,0]
  t=11
  tt=7
	  print, 'starting the plots'
	  
	  ;get the original aats tau.
   wvl0353_1=wvl0353 & wvl0380_1=wvl0380 & wvl0452_1=wvl0452 & wvl0499_1=wvl0499 & wvl0519_1=wvl0519
   wvl0605_1=wvl0605 & wvl0675_1=wvl0675 & wvl0779_1=wvl0779 & wvl0864_1=wvl0864 & wvl1019_1=wvl1019
   wvl1241_1=wvl1241 & wvl1558_1=wvl1558 & wvl2139_1=wvl2139

   wvl0353_1.field01[t,*]=interpol(tauaats[*,0],lonbelow,wvl0353_1.field01[1,*]) ;wvl0353_1.field01[t,*]/wvl0353_1.field01[tt,*] 
   wvl0380_1.field01[t,*]=interpol(tauaats[*,1],lonbelow,wvl0353_1.field01[1,*]);wvl0380_1.field01[t,*]/wvl0380_1.field01[tt,*] 
   wvl0452_1.field01[t,*]=interpol(tauaats[*,2],lonbelow,wvl0353_1.field01[1,*]);wvl0452_1.field01[t,*]/wvl0452_1.field01[tt,*] 
   wvl0499_1.field01[t,*]=interpol(tauaats[*,3],lonbelow,wvl0353_1.field01[1,*]);wvl0499_1.field01[t,*]/wvl0499_1.field01[tt,*] 
   wvl0519_1.field01[t,*]=interpol(tauaats[*,4],lonbelow,wvl0353_1.field01[1,*]);wvl0519_1.field01[t,*]/wvl0519_1.field01[tt,*]
   wvl0605_1.field01[t,*]=interpol(tauaats[*,5],lonbelow,wvl0353_1.field01[1,*]);wvl0605_1.field01[t,*]/wvl0605_1.field01[tt,*]
   wvl0675_1.field01[t,*]=interpol(tauaats[*,6],lonbelow,wvl0353_1.field01[1,*]);wvl0675_1.field01[t,*]/wvl0675_1.field01[tt,*] 
   wvl0779_1.field01[t,*]=interpol(tauaats[*,7],lonbelow,wvl0353_1.field01[1,*]);wvl0779_1.field01[t,*]/wvl0779_1.field01[tt,*]
   wvl0864_1.field01[t,*]=interpol(tauaats[*,8],lonbelow,wvl0353_1.field01[1,*]);wvl0864_1.field01[t,*]/wvl0864_1.field01[tt,*]
   wvl1019_1.field01[t,*]=interpol(tauaats[*,9],lonbelow,wvl0353_1.field01[1,*]);wvl1019_1.field01[t,*]/wvl1019_1.field01[tt,*]
   wvl1241_1.field01[t,*]=interpol(tauaats[*,10],lonbelow,wvl0353_1.field01[1,*]);wvl1241_1.field01[t,*]/wvl1241_1.field01[tt,*]
   wvl1558_1.field01[t,*]=interpol(tauaats[*,11],lonbelow,wvl0353_1.field01[1,*]);wvl1558_1.field01[t,*]/wvl1558_1.field01[tt,*]
   wvl2139_1.field01[t,*]=interpol(tauaats[*,12],lonbelow,wvl0353_1.field01[1,*]);wvl2139_1.field01[t,*]/wvl2139_1.field01[tt,*]

   ;wvl0353_1.field01[t,37:*]=wvl0353_1.field01[t,37:*]+0.2
   ;wvl0380_1.field01[t,37:*]=wvl0380_1.field01[t,37:*]+0.2 
   ;wvl0452_1.field01[t,37:*]=wvl0452_1.field01[t,37:*]+0.1 
   ;wvl0499_1.field01[t,37:*]=wvl0499_1.field01[t,37:*]+0.1 
   ;wvl0519_1.field01[t,37:*]=wvl0519_1.field01[t,37:*]+0.1
   ;wvl0605_1.field01[t,37:*]=wvl0605_1.field01[t,37:*]+0.1
   ;wvl0675_1.field01[t,37:*]=wvl0675_1.field01[t,37:*]+0.1
  
  plot, wvl0353_1.field01[t,*], wvl0353.field01[t,*],yrange=[0,2.0], xrange=[0,2.0], psym=2,$ ; title='SSFR retrieved vs. AATS measured optical depth', 
   xtitle='AATS optical thickness', ytitle='Retrieved optical thickness';,/nodata
	oplot, wvl0380_1.field01[t,*],wvl0380.field01[t,*], color=30, psym=2
	oplot, wvl0452_1.field01[t,*],wvl0452.field01[t,*], color=50, psym=2
	oplot, wvl0499_1.field01[t,*],wvl0499.field01[t,*], color=70, psym=2
	oplot, wvl0519_1.field01[t,*],wvl0519.field01[t,*], color=90, psym=2
	oplot, wvl0605_1.field01[t,*],wvl0605.field01[t,*], color=110, psym=2
	oplot, wvl0675_1.field01[t,*],wvl0675.field01[t,*], color=130, psym=2
	oplot, wvl0779_1.field01[t,*],wvl0779.field01[t,*], color=150, psym=2
	oplot, wvl0864_1.field01[t,*],wvl0864.field01[t,*], color=170, psym=2
	oplot, wvl1019_1.field01[t,*],wvl1019.field01[t,*], color=190, psym=2
	oplot, wvl1241_1.field01[t,*],wvl1241.field01[t,*], color=210, psym=2
	oplot, wvl1558_1.field01[t,*],wvl1558.field01[t,*], color=230, psym=2
	oplot, wvl2139_1.field01[t,*],wvl2139.field01[t,*], color=250, psym=2
   
   ; now put the error bars from aats
    !p.thick=0.2
  errploty, wvl0353.field01[t,*], wvl0353_1.field01[t,*]-dtau_aats[*,0], wvl0353_1.field01[t,*]+dtau_aats[*,0], color=0
  errploty, wvl0380.field01[t,*], wvl0380_1.field01[t,*]-dtau_aats[*,1], wvl0380_1.field01[t,*]+dtau_aats[*,1], color=30
  errploty, wvl0452.field01[t,*], wvl0452_1.field01[t,*]-dtau_aats[*,2], wvl0452_1.field01[t,*]+dtau_aats[*,2], color=50
  errploty, wvl0499.field01[t,*], wvl0499_1.field01[t,*]-dtau_aats[*,3], wvl0499_1.field01[t,*]+dtau_aats[*,3], color=70
  errploty, wvl0519.field01[t,*], wvl0519_1.field01[t,*]-dtau_aats[*,4], wvl0519_1.field01[t,*]+dtau_aats[*,4], color=90
  errploty, wvl0605.field01[t,*], wvl0605_1.field01[t,*]-dtau_aats[*,5], wvl0605_1.field01[t,*]+dtau_aats[*,5], color=110
  errploty, wvl0675.field01[t,*], wvl0675_1.field01[t,*]-dtau_aats[*,6], wvl0675_1.field01[t,*]+dtau_aats[*,6], color=130
  errploty, wvl0779.field01[t,*], wvl0779_1.field01[t,*]-dtau_aats[*,7], wvl0779_1.field01[t,*]+dtau_aats[*,7], color=150
  errploty, wvl0864.field01[t,*], wvl0864_1.field01[t,*]-dtau_aats[*,8], wvl0864_1.field01[t,*]+dtau_aats[*,8], color=170
  errploty, wvl1019.field01[t,*], wvl1019_1.field01[t,*]-dtau_aats[*,9], wvl1019_1.field01[t,*]+dtau_aats[*,9], color=190
  errploty, wvl1241.field01[t,*], wvl1241_1.field01[t,*]-dtau_aats[*,10], wvl1241_1.field01[t,*]+dtau_aats[*,10], color=210
  errploty, wvl1558.field01[t,*], wvl1558_1.field01[t,*]-dtau_aats[*,11], wvl1558_1.field01[t,*]+dtau_aats[*,11], color=230
  errploty, wvl2139.field01[t,*], wvl2139_1.field01[t,*]-dtau_aats[*,12], wvl2139_1.field01[t,*]+dtau_aats[*,12], color=250
      
  ; put error bars from the retrieval
    !p.thick=0.2
  errplot, wvl0353_1.field01[t,*], wvl0353.field01[t,*]-error[5,0]*wvl0353.field01[t,*],wvl0353.field01[t,*]+error[5,0]*wvl0353.field01[t,*] , color=0
  errplot, wvl0380_1.field01[t,*], wvl0380.field01[t,*]-error[5,1]*wvl0380.field01[t,*],wvl0380.field01[t,*]+error[5,1]*wvl0380.field01[t,*], color=30
  errplot, wvl0452_1.field01[t,*], wvl0452.field01[t,*]-error[5,2]*wvl0452.field01[t,*],wvl0452.field01[t,*]+error[5,2]*wvl0452.field01[t,*], color=50
  errplot, wvl0499_1.field01[t,*], wvl0499.field01[t,*]-error[5,3]*wvl0499.field01[t,*],wvl0499.field01[t,*]+error[5,3]*wvl0499.field01[t,*], color=70
  errplot, wvl0519_1.field01[t,*], wvl0519.field01[t,*]-error[5,4]*wvl0519.field01[t,*],wvl0519.field01[t,*]+error[5,4]*wvl0519.field01[t,*], color=90
  errplot, wvl0605_1.field01[t,*], wvl0605.field01[t,*]-error[5,5]*wvl0605.field01[t,*],wvl0605.field01[t,*]+error[5,5]*wvl0605.field01[t,*], color=110
  errplot, wvl0675_1.field01[t,*], wvl0675.field01[t,*]-error[5,6]*wvl0675.field01[t,*],wvl0675.field01[t,*]+error[5,6]*wvl0675.field01[t,*], color=130
  errplot, wvl0779_1.field01[t,*], wvl0779.field01[t,*]-error[5,7]*wvl0779.field01[t,*],wvl0779.field01[t,*]+error[5,7]*wvl0779.field01[t,*], color=150
  errplot, wvl0864_1.field01[t,*], wvl0864.field01[t,*]-error[5,8]*wvl0864.field01[t,*],wvl0864.field01[t,*]+error[5,8]*wvl0864.field01[t,*], color=170
  errplot, wvl1019_1.field01[t,*], wvl1019.field01[t,*]-error[5,9]*wvl1019.field01[t,*],wvl1019.field01[t,*]+error[5,9]*wvl1019.field01[t,*], color=190
  errplot, wvl1241_1.field01[t,*], wvl1241.field01[t,*]-error[5,10]*wvl1241.field01[t,*],wvl1241.field01[t,*]+error[5,10]*wvl1241.field01[t,*], color=210
  errplot, wvl1558_1.field01[t,*], wvl1558.field01[t,*]-error[5,11]*wvl1558.field01[t,*],wvl1558.field01[t,*]+error[5,11]*wvl1558.field01[t,*], color=230
  errplot, wvl2139_1.field01[t,*], wvl2139.field01[t,*]-error[5,12]*wvl2139.field01[t,*],wvl2139.field01[t,*]+error[5,12]*wvl2139.field01[t,*], color=250


	plots, [0,2.0],[0,2.0], linestyle=2, thick=5
	
	legend,['353 nm','380 nm','452 nm','499 nm','519 nm','605 nm','675 nm','779 nm','864 nm','1019 nm','1241 nm','1558 nm','2139 nm'],textcolors=[0,30,50,70,90,110,130,150,170,190,210,230,250]
;	legend, ['779nm','864nm','1019nm','1241nm','1558nm','2139nm'], textcolors=[150,170,190,210,230,250], /right

  device, /close
print, 'finished the plots'
stop
  spawn, 'convert "'+dir+'rtm_tau.ps" "'+dir+'rtm_tau_hsrl.png"'
;  spawn, 'rm -f "'+dir+'rtm_tau.ps"'

;stop

spect=[reform(wvl0353_1.field01[t,*]),reform(wvl0380_1.field01[t,*]),reform(wvl0452_1.field01[t,*]),reform(wvl0499_1.field01[t,*]),reform(wvl0519_1.field01[t,*]),reform(wvl0605_1.field01[t,*]),reform(wvl0675_1.field01[t,*]),reform(wvl0779_1.field01[t,*]),reform(wvl0864_1.field01[t,*]),reform(wvl1019_1.field01[t,*]),reform(wvl1241_1.field01[t,*]),reform(wvl1558_1.field01[t,*]),reform(wvl2139_1.field01[t,*])]
spect_aats=[reform(wvl0353.field01[t,*]),reform(wvl0380.field01[t,*]),reform(wvl0452.field01[t,*]),reform(wvl0499.field01[t,*]),reform(wvl0519.field01[t,*]),reform(wvl0605.field01[t,*]),reform(wvl0675.field01[t,*]),reform(wvl0779.field01[t,*]),reform(wvl0864.field01[t,*]),reform(wvl1019.field01[t,*]),reform(wvl1241.field01[t,*]),reform(wvl1558.field01[t,*]),reform(wvl2139.field01[t,*])]
;spect=[reform(wvl0380_1.field01[t,*]),reform(wvl0452_1.field01[t,*]),reform(wvl0499_1.field01[t,*]),reform(wvl0519_1.field01[t,*]),reform(wvl0605_1.field01[t,*]),reform(wvl0675_1.field01[t,*]),reform(wvl0779_1.field01[t,*]),reform(wvl0864_1.field01[t,*]),reform(wvl1019_1.field01[t,*]),reform(wvl1241_1.field01[t,*]),reform(wvl1558_1.field01[t,*]),reform(wvl2139_1.field01[t,*])]
;spect_aats=[reform(wvl0380.field01[t,*]),reform(wvl0452.field01[t,*]),reform(wvl0499.field01[t,*]),reform(wvl0519.field01[t,*]),reform(wvl0605.field01[t,*]),reform(wvl0675.field01[t,*]),reform(wvl0779.field01[t,*]),reform(wvl0864.field01[t,*]),reform(wvl1019.field01[t,*]),reform(wvl1241.field01[t,*]),reform(wvl1558.field01[t,*]),reform(wvl2139.field01[t,*])]
ids=0
for p=0,n_elements(spect)-1 do begin
  if finite(spect[p]) and finite(spect_aats[p]) then ids=[ids,p]
endfor
ids=ids[1:*]

print, 'correlation coefficient:',correlate(spect[ids], spect_aats[ids])

; determine average distance
dist=abs(spect-spect_aats)/spect_aats
kn=n_elements(wvl0353.field01[t,*])
for i=0, 12 do begin

print, lambda_aats[i],'distances',median(dist[i*kn:(i+1)*kn]),mean(dist[i*kn:(i+1)*kn],/nan), stddev(dist[i*kn:(i+1)*kn],/nan)
endfor
endif
stop

if 1 then begin
 set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated
 device, /tt_font, set_font='Helvetica Bold'
 device, filename=dir+'rtm_results_time.ps'
 device,/color,bits_per_pixel=8.
 device, xsize=40, ysize=40
  !p.font=1
  !p.thick=5
  !p.charsize=2.0
  !x.style=1
  !y.style=1 
  !z.style=1
  !y.thick=1.8
  !x.thick=1.8
  !p.multi=[0,2,2]

  ind=[2,3,4,5]
  name=['SSA','ASY','ASY2','Albedo']
  nn=[1,2,3,4]
  !y.omargin=[0,4]
  for i=0,3 do begin
  plot, wvl0353.field01[0,*], wvl0353.field01[ind[i],*], title=name[i], xtitle='Latitude (degrees)', yrange=[0,1], xticks=4;, xrange=[57.06,57.08]
  oplot, wvl0380.field01[0,*],wvl0380.field01[ind[i],*], color=30
  oplot, wvl0452.field01[0,*],wvl0452.field01[ind[i],*], color=50
  oplot, wvl0499.field01[0,*],wvl0499.field01[ind[i],*], color=70
  oplot, wvl0519.field01[0,*],wvl0519.field01[ind[i],*], color=90
  oplot, wvl0605.field01[0,*],wvl0605.field01[ind[i],*], color=110
  oplot, wvl0675.field01[0,*],wvl0675.field01[ind[i],*], color=130
  oplot, wvl0779.field01[0,*],wvl0779.field01[ind[i],*], color=150
  oplot, wvl0864.field01[0,*],wvl0864.field01[ind[i],*], color=170
  oplot, wvl1019.field01[0,*],wvl1019.field01[ind[i],*], color=190
  oplot, wvl1241.field01[0,*],wvl1241.field01[ind[i],*], color=210
  oplot, wvl1558.field01[0,*],wvl1558.field01[ind[i],*], color=230
  oplot, wvl2139.field01[0,*],wvl2139.field01[ind[i],*], color=250
  !p.thick=0.5
  errplot, wvl0353.field01[0,*], wvl0353.field01[ind[i],*]*(1.-error[nn[i],0]),wvl0353.field01[ind[i],*]*(1.+error[nn[i],0]), color=0
  errplot, wvl0380.field01[0,*], wvl0380.field01[ind[i],*]*(1.-error[nn[i],1]),wvl0380.field01[ind[i],*]*(1.+error[nn[i],1]), color=30
  errplot, wvl0452.field01[0,*], wvl0452.field01[ind[i],*]*(1.-error[nn[i],2]),wvl0452.field01[ind[i],*]*(1.+error[nn[i],2]), color=50
  errplot, wvl0499.field01[0,*], wvl0499.field01[ind[i],*]*(1.-error[nn[i],3]),wvl0499.field01[ind[i],*]*(1.+error[nn[i],3]), color=70
  errplot, wvl0519.field01[0,*], wvl0519.field01[ind[i],*]*(1.-error[nn[i],4]),wvl0519.field01[ind[i],*]*(1.+error[nn[i],4]), color=90
  errplot, wvl0605.field01[0,*], wvl0605.field01[ind[i],*]*(1.-error[nn[i],5]),wvl0605.field01[ind[i],*]*(1.+error[nn[i],5]), color=110
  errplot, wvl0675.field01[0,*], wvl0675.field01[ind[i],*]*(1.-error[nn[i],6]),wvl0675.field01[ind[i],*]*(1.+error[nn[i],6]), color=130
  errplot, wvl0779.field01[0,*], wvl0779.field01[ind[i],*]*(1.-error[nn[i],7]),wvl0779.field01[ind[i],*]*(1.+error[nn[i],7]), color=150
  errplot, wvl0864.field01[0,*], wvl0864.field01[ind[i],*]*(1.-error[nn[i],8]),wvl0864.field01[ind[i],*]*(1.+error[nn[i],8]), color=170
  errplot, wvl1019.field01[0,*], wvl1019.field01[ind[i],*]*(1.-error[nn[i],9]),wvl1019.field01[ind[i],*]*(1.+error[nn[i],9]), color=190  
  errplot, wvl1241.field01[0,*], wvl1241.field01[ind[i],*]*(1.-error[nn[i],10]),wvl1241.field01[ind[i],*]*(1.+error[nn[i],10]), color=210  
  errplot, wvl1558.field01[0,*], wvl1558.field01[ind[i],*]*(1.-error[nn[i],11]),wvl1558.field01[ind[i],*]*(1.+error[nn[i],11]), color=230  
  errplot, wvl2139.field01[0,*], wvl0353.field01[ind[i],*]*(1.-error[nn[i],12]),wvl2139.field01[ind[i],*]*(1.+error[nn[i],12]), color=250 
  !p.thick=5
  endfor
  
  legend,['353nm','380nm','452nm','499nm','519nm','605nm','675nm'],textcolors=[0,30,50,70,90,110,130]
  legend, ['779nm','864nm','1019nm','1241nm','1558nm','2139nm'], textcolors=[150,170,190,210,230,250], /right
  
  xyouts, 0.5,0.95, alignment=0.5, /normal, charsize=4.0, 'Aerosol retrieval latitudes for ARCTAS '+date
  device, /close
  spawn, 'convert "'+dir+'rtm_results_time.ps" "'+dir+'rtm_results_time_1.png" ' ;'
  spawn, 'rm -f "'+dir+'rtm_results_time.ps"'
endif

; plot the time trace (at 500nm) on a single plot, similarly to the wavelength trace
set_plot, 'ps'
 device, /encapsulated
 device, /tt_font, set_font='Helvetica Bold'
 device, filename=dir+'rtm_results_spc'+'for_time.ps'
 device,/color,bits_per_pixel=8.
 device, xsize=60, ysize=20
 !x.style=1
 !p.charsize=3.8
 !y.omargin=[0,3]
!p.multi=[0,3,1]
i=0 ;ssa
ind=[2,3,4,5,11,8]
name=['Single Scattering Albedo','Asymmetry parameter','ASY2','Albedo','Optical Depth']
nn=[1,2,3,5,4]
spectrum=wvl0499.field01[ind[i],*]
lon=wvl0499.field01[0,*]
plot, lon, spectrum, title='Time series of forcing input parameters at 499nm', xtitle='Latitude (degrees)', yrange=[0,1], ystyle=8, xmargin=[8,4], xticks=4
tvlct,200,200,200,200
errplot, lon, spectrum*(1-error[nn[i],3]), spectrum*(1+error[nn[i],3]),color=200
oplot, lon, spectrum
;polyfill, [lon,reverse(lon)],[spectrum*(1-error[nn[i],3]),reverse( spectrum*(1+error[nn[i],3]))],color=200
tvlct, 21,205,0,150
xyouts, 0.03,0.5,'g', orientation=90, /normal, color=150
xyouts, 0.03,0.4,'!9a!3',orientation=90, /normal, color=70
xyouts, 0.03,0.45, '!9'+string(118B)+'!3',orientation=90,/normal

i=1 ;asy
spectrum=wvl0499.field01[ind[i],*]
tvlct, 181,255,150,151
errplot, lon, spectrum*(1-error[nn[i],3]), spectrum*(1+error[nn[i],3]), color=151
oplot, lon, spectrum, color=150
;polyfill, [lon,reverse(lon)],[spectrum*(1-error[nn[i],3]),reverse( spectrum*(1+error[nn[i],3]))],color=151

i=3 ;albedo
spectrum=wvl0499.field01[ind[i],*]
tvlct, 150,222,255,71
errplot, lon, spectrum*(1-error[nn[i],3]), spectrum*(1+error[nn[i],3]),color=71
oplot, lon, spectrum, color=70
;polyfill, [lon,reverse(lon)],[spectrum*(1-error[nn[i],3]),reverse( spectrum*(1+error[nn[i],3]))],color=71

i=4 ;taumod
axis, yaxis=1,ystyle=1, yrange=[0.0,0.8],/save,color=250
spectrum=wvl0499.field01[ind[i],*]
tvlct,255,180,180,251
errplot, lon, spectrum*(1-dtau_aats[3,nn[i]]), spectrum*(1+dtau_aats[3,nn[i]]),color=251
oplot, lon, spectrum,color=250
;polyfill, [lon,reverse(lon)],[spectrum*(1-error[nn[i],3]),reverse( spectrum*(1+error[nn[i],3]))],color=251
xyouts, 0.34,0.45, '!9t!3',orientation=90,/normal, color=250

;legend,name[[0,1,4,3]],textcolors=[0,150,250,70],box=0,charsize=1.8,position=[0.05,0.3],/normal
device, /close
spawn, 'convert '+dir+'rtm_results_spc'+'for_time.ps '+dir+'rtm_for_time_1.png'
spawn, 'rm -f '+dir+'rtm_results_spc'+'for_time.ps'
;stop

;restore, '/home/leblanc/arctas/nasa/20080709/20080709_forcing.out'
wvls=wvl

if (0) then begin
restore, '/home/leblanc/arctas/nasa/'+date+'/'+date+'_forcing.out'
wvls=wvl
set_plot, 'ps'
 loadct, 39, /silent
 device, /encapsulated
 device, /tt_font, set_font='Helvetica Bold'
 device, filename=dir+'rtm_forcing_time.ps'
 device,/color,bits_per_pixel=8.
 device, xsize=20, ysize=20
      !p.font=1 & !p.thick=5
      !p.charsize=2.0 & !x.style=1 & !y.style=1 & !z.style=1
      !y.thick=1.8 & !x.thick=1.8
      !p.multi=0
      !y.omargin=[0,0]
plot, wvl0353.field01[0,*], effp[0,*,0], title='Aerosol direct forcing', xtitle='Latitude (degrees)', ytitle='Relative forcing efficiency (%)',yrange=[-75,15], xticks=4, xtickformat='(F8.3)'
oplot, wvl0380.field01[0,*], effp[1,*,0], color=30
oplot, wvl0452.field01[0,*], effp[2,*,0], color=50
oplot, wvl0499.field01[0,*], effp[3,*,0], color=70
oplot, wvl0519.field01[0,*], effp[4,*,0], color=90
oplot, wvl0605.field01[0,*], effp[5,*,0], color=110
oplot, wvl0675.field01[0,*], effp[6,*,0], color=130
oplot, wvl0779.field01[0,*], effp[7,*,0], color=150
oplot, wvl0864.field01[0,*], effp[8,*,0], color=170
oplot, wvl1019.field01[0,*], effp[9,*,0], color=190
oplot, wvl1241.field01[0,*], effp[10,*,0], color=210
oplot, wvl1558.field01[0,*], effp[11,*,0], color=230
oplot, wvl2139.field01[0,*], effp[12,*,0], color=250

oplot, wvl0353.field01[0,*], effp[0,*,1], linestyle=2
oplot, wvl0380.field01[0,*], effp[1,*,1], color=30, linestyle=2
oplot, wvl0452.field01[0,*], effp[2,*,1], color=50, linestyle=2
oplot, wvl0499.field01[0,*], effp[3,*,1], color=70, linestyle=2
oplot, wvl0519.field01[0,*], effp[4,*,1], color=90, linestyle=2
oplot, wvl0605.field01[0,*], effp[5,*,1], color=110, linestyle=2
oplot, wvl0675.field01[0,*], effp[6,*,1], color=130, linestyle=2
oplot, wvl0779.field01[0,*], effp[7,*,1], color=150, linestyle=2
oplot, wvl0864.field01[0,*], effp[8,*,1], color=170, linestyle=2
oplot, wvl1019.field01[0,*], effp[9,*,1], color=190, linestyle=2
oplot, wvl1241.field01[0,*], effp[10,*,1], color=210, linestyle=2
oplot, wvl1558.field01[0,*], effp[11,*,1], color=230, linestyle=2
oplot, wvl2139.field01[0,*], effp[12,*,1], color=250, linestyle=2

Legend, ['Top of layer', 'Bottom of layer'], linestyle=[2,0], color=[0,0],/right,/bottom

device, /close
spawn, 'convert "'+dir+'rtm_forcing_time.ps" "'+dir+'rtm_forcing_time.png"'
spawn, 'rm -f "'+dir+'rtm_forcing_time.ps"'
endif

if 0 then begin
  ; #lat  lon     ssa   asy       asy2    albedo          correction    tau modification        flux divergence  model down  model up  tau
  ind=[2,3,4,5,7,8]
  name=['SSA','Asymmetry parameter','ASY2','Albedo','Tau modification']
  nn=[1,2,3,4,5]
  for n=0, n_elements(wvl0353.field01[0,*])-1 do begin
  ;n=0
  set_plot, 'ps'
   device, /encapsulated
   device, /tt_font, set_font='Helvetica Bold'
   device, filename=dir+'rtm_results_spc'+strtrim(string(n),2)+'.ps'
   device,/color,bits_per_pixel=8.
   device, xsize=40, ysize=30
   !x.style=1
   print, n_elements(wvl0353.field01[0,*])
   ;stop
   !y.omargin=[2,5]
  !p.multi=[0,3,2]
  i=0 ;ssa
  spectrum=[wvl0353.field01[ind[i],n],wvl0380.field01[ind[i],n],wvl0452.field01[ind[i],n],wvl0499.field01[ind[i],n],wvl0519.field01[ind[i],n],wvl0605.field01[ind[i],n],wvl0675.field01[ind[i],n],wvl0779.field01[ind[i],n],wvl0864.field01[ind[i],n],wvl1019.field01[ind[i],n],wvl1241.field01[ind[i],n],wvl1558.field01[ind[i],n],wvl2139.field01[ind[i],n]]
   wvl=[353,380,452,499,519,605,675,779,864,1019,1241,1558,2139]
  
  plot, wvl, spectrum, title=name[i]+' spectrum for lat: '+strtrim(string(wvl0353.field01[0,n]),2), xtitle='wavelength',xrange=[300,2200], yrange=[0,1]
  errplot, wvl,spectrum*(1.-error[nn[i],*]), spectrum*(1.+error[nn[i],*])
  
  i=1 ;asy
  spectrum=[wvl0353.field01[ind[i],n],wvl0380.field01[ind[i],n],wvl0452.field01[ind[i],n],wvl0499.field01[ind[i],n],wvl0519.field01[ind[i],n],wvl0605.field01[ind[i],n],wvl0675.field01[ind[i],n],wvl0779.field01[ind[i],n],wvl0864.field01[ind[i],n],wvl1019.field01[ind[i],n],wvl1241.field01[ind[i],n],wvl1558.field01[ind[i],n],wvl2139.field01[ind[i],n]]
  plot, wvl, spectrum, title=name[i]+' spectrum for lat: '+strtrim(string(wvl0353.field01[0,n]),2), xtitle='wavelength',xrange=[300,2200], yrange=[0,1],/nodata
  oplot, wvl, spectrum, color=150
  errplot, wvl,spectrum*(1.-error[nn[i],*]), spectrum*(1.+error[nn[i],*]),color=150
  
  i=2;asy2
  spectrum=[wvl0353.field01[ind[i],n],wvl0380.field01[ind[i],n],wvl0452.field01[ind[i],n],wvl0499.field01[ind[i],n],wvl0519.field01[ind[i],n],wvl0605.field01[ind[i],n],wvl0675.field01[ind[i],n],wvl0779.field01[ind[i],n],wvl0864.field01[ind[i],n],wvl1019.field01[ind[i],n],wvl1241.field01[ind[i],n],wvl1558.field01[ind[i],n],wvl2139.field01[ind[i],n]]
  oplot, wvl, spectrum, color=70
  errplot, wvl,spectrum*(1.-error[nn[i],*]), spectrum*(1.+error[nn[i],*]),color=70
  legend,['ASY','ASY2'],textcolors=[150,70],/right
  
  i=3 ;albedo
  spectrum=[wvl0353.field01[ind[i],n],wvl0380.field01[ind[i],n],wvl0452.field01[ind[i],n],wvl0499.field01[ind[i],n],wvl0519.field01[ind[i],n],wvl0605.field01[ind[i],n],wvl0675.field01[ind[i],n],wvl0779.field01[ind[i],n],wvl0864.field01[ind[i],n],wvl1019.field01[ind[i],n],wvl1241.field01[ind[i],n],wvl1558.field01[ind[i],n],wvl2139.field01[ind[i],n]]
  plot, wvl, spectrum, title=name[i]+' spectrum for lat: '+strtrim(string(wvl0353.field01[0,n]),2), xtitle='wavelength',xrange=[300,2200], yrange=[0,1]
  errplot, wvl,spectrum*(1.-error[nn[i],*]), spectrum*(1.+error[nn[i],*])
  
  i=4 ;taumod
  spectrum=[wvl0353.field01[ind[i],n],wvl0380.field01[ind[i],n],wvl0452.field01[ind[i],n],wvl0499.field01[ind[i],n],wvl0519.field01[ind[i],n],wvl0605.field01[ind[i],n],wvl0675.field01[ind[i],n],wvl0779.field01[ind[i],n],wvl0864.field01[ind[i],n],wvl1019.field01[ind[i],n],wvl1241.field01[ind[i],n],wvl1558.field01[ind[i],n],wvl2139.field01[ind[i],n]]
  plot, wvl, (spectrum-1.0)*100., title=name[i]+' spectrum for lat: '+strtrim(string(wvl0353.field01[0,n]),2), xtitle='wavelength',xrange=[300,2200], yrange=[-18,18], ytitle='% deviation of AATS'
  ;errplot, wvl,spectrum*(1.-error[nn[i],*]), spectrum*(1.+error[nn[i],*])
  oplot, [min(wvl),max(wvl)], [0,0], linestyle=1
  !p.thick=0.5
  errplot, wvl, -dtau_aats[n,*]/tau_aats[n,*]*100.,  +dtau_aats[n,*]/tau_aats[n,*]*100.
  !p.thick=5
  i=5
  absorp=[wvl0353.field01[ind[i],n],wvl0380.field01[ind[i],n],wvl0452.field01[ind[i],n],wvl0499.field01[ind[i],n],wvl0519.field01[ind[i],n],wvl0605.field01[ind[i],n],wvl0675.field01[ind[i],n],wvl0779.field01[ind[i],n],wvl0864.field01[ind[i],n],wvl1019.field01[ind[i],n],wvl1241.field01[ind[i],n],wvl1558.field01[ind[i],n],wvl2139.field01[ind[i],n]]
  plot, wvl, absorp, title='Flux divergence (absorption)', xtitle='wavelength', yrange=[0,0.350],ytitle='Irradiance (w/m^2)',xrange=[300,2200]
  oplot, wvl, absorp, psym=2
  
  ;restore, '/home/leblanc/arctas/nasa/'+date+'/'+date+'_spectra_seb.out'
  oplot, wvls, -(nabove[*,n]-zabove[*,n])+(nbelow[*,n]-zbelow[*,n]), color=250
  legend,['model','measured'],textcolors=[0,250], /right
  
  plot, wvls, zabove[*,n], title='Zenith irradiance', xtitle='wavelength', ytitle='Irradiance (W/m^2)', yrange=[0,1.7]
  oplot, wvls, zbelow[*,n], color=70
  oplot, wvls, nabove[*,n], color=0, linestyle=2
  oplot, wvls, nbelow[*,n], color=70, linestyle=2
  legend,['Zenith','Nadir','Above','Below'],/right,color=[0,0,0,70],linestyle=[0,1,0,0]
  
  xyouts, 0.5,0.95, alignment=0.5, /normal, charsize=4.0, 'Aerosol retrieval spectrum for ARCTAS '+date
  device, /close
  spawn, 'convert "'+dir+'rtm_results_spc'+strtrim(string(n),2)+'.ps" "'+dir+'rtm_results_find_spc'+strtrim(string(n,format='(I03)'),2)+'.png"'
  spawn, 'rm -f "'+dir+'rtm_results_spc'+strtrim(string(n),2)+'.ps"'
  endfor
endif


;plotting of results spectrum at each point
if 1 then begin
; for n=0, n_elements(wvl0353.field01[0,*])-1 do begin
n=137
;calnex: 52
set_plot, 'ps'
 device, /encapsulated
 device, /tt_font, set_font='Helvetica Bold'
 device, filename=dir+'rtm_results_spc'+strtrim(string(n),2)+'.ps'
 device,/color,bits_per_pixel=8.
 device, xsize=60, ysize=20
 !x.style=1
 !p.charsize=3.8 
print, n,'/',n_elements(wvl0353.field01[0,*])-1
 ;stop
 !y.omargin=[0,3]
!p.multi=[0,3,1]
i=0 ;ssa
  ;spectrum=[wvl0353.field01[ind[i],n],wvl0380.field01[ind[i],n],wvl0452.field01[ind[i],n],wvl0499.field01[ind[i],n],wvl0519.field01[ind[i],n],wvl0605.field01[ind[i],n],wvl0675.field01[ind[i],n],wvl0779.field01[ind[i],n],wvl0864.field01[ind[i],n],wvl1019.field01[ind[i],n],wvl1241.field01[ind[i],n],wvl1558.field01[ind[i],n],wvl2139.field01[ind[i],n]]
  spectrum=[mean(wvl0353.field01[ind[i],*],/nan),mean(wvl0380.field01[ind[i],*],/nan),mean(wvl0452.field01[ind[i],*],/nan),$
   mean(wvl0499.field01[ind[i],*],/nan),mean(wvl0519.field01[ind[i],*],/nan),mean(wvl0605.field01[ind[i],*],/nan),$
   mean(wvl0675.field01[ind[i],*],/nan),mean(wvl0779.field01[ind[i],*],/nan),mean(wvl0864.field01[ind[i],*],/nan),$
   mean(wvl1019.field01[ind[i],*],/nan),mean(wvl1241.field01[ind[i],*],/nan),mean(wvl1558.field01[ind[i],*],/nan),mean(wvl2139.field01[ind[i],*],/nan)]
   wvl=[353,380,452,499,519,605,675,779,864,1019,1241,1558,2139]
   plot, wvl, spectrum, title='Forcing input parameters spectra', xtitle='wavelength (nm)', yrange=[0,1], ystyle=8, xmargin=[8,4], xrange=[340.,1020.]
;errplot, wvl, spectrum*(1-error[nn[i],*]), spectrum*(1+error[nn[i],*])
tvlct, 200,200,200,200
sst=spectrum*(1-error[nn[i],*])
k=where(sst le 0.0,op)
if op gt 0 then sst[k]=0.
sstt=spectrum*(1+error[nn[i],*])
k=where(sstt ge 1.0,op)
if op gt 0 then sstt[k]=1.0
polyfill, [wvl[0:9],reverse(wvl[0:9])],[sst[0:9],reverse(sstt[0:9])],color=200
oplot, wvl, spectrum
sssa=spectrum
ssta=sst & sstta=sstt
tvlct, 21,205,0,150
xyouts, 0.03,0.5,'g', orientation=90, /normal, color=150
xyouts, 0.03,0.4,'!9a!3',orientation=90, /normal, color=70
xyouts, 0.03,0.45, '!9'+string(118B)+'!3',orientation=90,/normal


i=1 ;asy
tvlct, 21,205,0,150
;spectrum=[wvl0353.field01[ind[i],n],wvl0380.field01[ind[i],n],wvl0452.field01[ind[i],n],wvl0499.field01[ind[i],n],wvl0519.field01[ind[i],n],wvl0605.field01[ind[i],n],wvl0675.field01[ind[i],n],wvl0779.field01[ind[i],n],wvl0864.field01[ind[i],n],wvl1019.field01[ind[i],n],wvl1241.field01[ind[i],n],wvl1558.field01[ind[i],n],wvl2139.field01[ind[i],n]]
  spectrum=[mean(wvl0353.field01[ind[i],*],/nan),mean(wvl0380.field01[ind[i],*],/nan),mean(wvl0452.field01[ind[i],*],/nan),$
   mean(wvl0499.field01[ind[i],*],/nan),mean(wvl0519.field01[ind[i],*],/nan),mean(wvl0605.field01[ind[i],*],/nan),$
   mean(wvl0675.field01[ind[i],*],/nan),mean(wvl0779.field01[ind[i],*],/nan),mean(wvl0864.field01[ind[i],*],/nan),$
   mean(wvl1019.field01[ind[i],*],/nan),mean(wvl1241.field01[ind[i],*],/nan),mean(wvl1558.field01[ind[i],*],/nan),mean(wvl2139.field01[ind[i],*],/nan)]
oplot, wvl, spectrum, color=150
;errplot, wvl, spectrum*(1-error[nn[i],*]), spectrum*(1+error[nn[i],*]), color=150
tvlct, 211,255,190,151
sst=spectrum*(1-error[nn[i],*])
k=where(sst le 0.0,op)
if op gt 0 then sst[k]=0.
sstt=spectrum*(1+error[nn[i],*])
k=where(sstt ge 1.0,op)
if op gt 0 then sstt[k]=1.0
polyfill, [wvl[0:9],reverse(wvl[0:9])],[sst[0:9],reverse(sstt[0:9])], color=151
oplot, wvl, spectrum, color=150
sasy=spectrum
ssty=sst & sstty=sstt

i=3 ;albedo
;spectrum=[wvl0353.field01[ind[i],n],wvl0380.field01[ind[i],n],wvl0452.field01[ind[i],n],wvl0499.field01[ind[i],n],wvl0519.field01[ind[i],n],wvl0605.field01[ind[i],n],wvl0675.field01[ind[i],n],wvl0779.field01[ind[i],n],wvl0864.field01[ind[i],n],wvl1019.field01[ind[i],n],wvl1241.field01[ind[i],n],wvl1558.field01[ind[i],n],wvl2139.field01[ind[i],n]]
  spectrum=[mean(wvl0353.field01[ind[i],*],/nan),mean(wvl0380.field01[ind[i],*],/nan),mean(wvl0452.field01[ind[i],*],/nan),$
   mean(wvl0499.field01[ind[i],*],/nan),mean(wvl0519.field01[ind[i],*],/nan),mean(wvl0605.field01[ind[i],*],/nan),$
   mean(wvl0675.field01[ind[i],*],/nan),mean(wvl0779.field01[ind[i],*],/nan),mean(wvl0864.field01[ind[i],*],/nan),$
   mean(wvl1019.field01[ind[i],*],/nan),mean(wvl1241.field01[ind[i],*],/nan),mean(wvl1558.field01[ind[i],*],/nan),mean(wvl2139.field01[ind[i],*],/nan)]
 oplot, wvl, spectrum, color=70
;errplot, wvl, spectrum*(1-error[nn[i],*]), spectrum*(1+error[nn[i],*]),color=70
tvlct, 150,222,255,71
sst=spectrum*(1-error[nn[i],*])
k=where(sst le 0.0,op)
if op gt 0 then sst[k]=0.
sstt=spectrum*(1+error[nn[i],*])
k=where(sstt ge 1.0,op)
if op gt 0 then sstt[k]=1.0
polyfill, [wvl[0:9],reverse(wvl[0:9])],[sst[0:9],reverse(sstt[0:9])], color=71
oplot, wvl, spectrum, color=70
salb=spectrum

polyfill, [wvl[0:9],reverse(wvl[0:9])],[ssty[0:9],reverse(sstty[0:9])], color=151;,/line_fill, spacing=0.1
polyfill, [wvl[0:9],reverse(wvl[0:9])],[ssta[0:9],reverse(sstta[0:9])], color=200;,/line_fill, spacing=0.1
oplot, wvl, sasy, color=150
oplot, wvl, sssa, color=0
oplot, wvl, salb,color=70

i=4 ;taumod

;spectrum=[wvl0353.field01[ind[i],n],wvl0380.field01[ind[i],n],wvl0452.field01[ind[i],n],wvl0499.field01[ind[i],n],wvl0519.field01[ind[i],n],wvl0605.field01[ind[i],n],wvl0675.field01[ind[i],n],wvl0779.field01[ind[i],n],wvl0864.field01[ind[i],n],wvl1019.field01[ind[i],n],wvl1241.field01[ind[i],n],wvl1558.field01[ind[i],n],wvl2139.field01[ind[i],n]]
  spectrum=[mean(wvl0353.field01[ind[i],*],/nan),mean(wvl0380.field01[ind[i],*],/nan),mean(wvl0452.field01[ind[i],*],/nan),$
   mean(wvl0499.field01[ind[i],*],/nan),mean(wvl0519.field01[ind[i],*],/nan),mean(wvl0605.field01[ind[i],*],/nan),$
   mean(wvl0675.field01[ind[i],*],/nan),mean(wvl0779.field01[ind[i],*],/nan),mean(wvl0864.field01[ind[i],*],/nan),$
   mean(wvl1019.field01[ind[i],*],/nan),mean(wvl1241.field01[ind[i],*],/nan),mean(wvl1558.field01[ind[i],*],/nan),mean(wvl2139.field01[ind[i],*],/nan)]
axis, yaxis=1,ystyle=1, yrange=[0.0,max(wvl0353.field01[ind[i],*],/nan)],/save,color=250
oplot, wvl, spectrum,color=250
;errplot, wvl, spectrum*(1-error[nn[i],*]), spectrum*(1+error[nn[i],*]),color=250
tvlct,255,180,180,251
sst=spectrum*(1-dtau_aats[*,nn[i]])
k=where(sst le 0.0,op)
if op gt 0 then sst[k]=0.
sstt=spectrum*(1+dtau_aats[*,nn[i]])
k=where(sstt ge max(wvl0353.field01[ind[i],*],/nan),op)
if op gt 0 then sstt[k]=max(wvl0353.field01[ind[i],*],/nan)
polyfill, [wvl[0:9],reverse(wvl[0:9])],[sst[0:9],reverse(sstt[0:9])], color=251
;polyfill, [wvl,reverse(wvl)],[ssta,reverse(sstta)], color=200
oplot, wvl, spectrum,color=250
;oplot, wvl, sasy, color=150
;oplot, wvl, sssa, color=0
;oplot, wvl, salb,color=70
xyouts, 0.34,0.45, '!9t!3',orientation=90,/normal, color=250

;legend,name[[0,1,4,3]],textcolors=[0,150,250,70],box=0,charsize=1.8,position=[0.04,0.35],/normal

i=5
;spectrum=[wvl0353.field01[ind[i],n],wvl0380.field01[ind[i],n],wvl0452.field01[ind[i],n],wvl0499.field01[ind[i],n],wvl0519.field01[ind[i],n],wvl0605.field01[ind[i],n],wvl0675.field01[ind[i],n],wvl0779.field01[ind[i],n],wvl0864.field01[ind[i],n],wvl1019.field01[ind[i],n],wvl1241.field01[ind[i],n],wvl1558.field01[ind[i],n],wvl2139.field01[ind[i],n]]
  spectrum=[mean(wvl0353.field01[ind[i],*],/nan),mean(wvl0380.field01[ind[i],*],/nan),mean(wvl0452.field01[ind[i],*],/nan),$
   mean(wvl0499.field01[ind[i],*],/nan),mean(wvl0519.field01[ind[i],*],/nan),mean(wvl0605.field01[ind[i],*],/nan),$
   mean(wvl0675.field01[ind[i],*],/nan),mean(wvl0779.field01[ind[i],*],/nan),mean(wvl0864.field01[ind[i],*],/nan),$
   mean(wvl1019.field01[ind[i],*],/nan),mean(wvl1241.field01[ind[i],*],/nan),mean(wvl1558.field01[ind[i],*],/nan),mean(wvl2139.field01[ind[i],*],/nan)]
plot, wvl, spectrum, title='Flux divergence (absorption)', xtitle='Wavelength (nm)', yrange=[0,0.400],ytitle='Irradiance (w/m!U2!N)'
oplot, wvl, spectrum, psym=2
nab_mean=wvls & nbe_mean=wvls & zab_mean=wvls & zbe_mean=wvls
for ww=0, n_elements(wvls)-1 do begin
  nab_mean[ww]=MEAN(nabove[ww,*],/nan) & nbe_mean[ww]=MEAN(nbelow[ww,*],/nan) & zab_mean[ww]=MEAN(zabove[ww,*],/nan) & zbe_mean[ww]=MEAN(zbelow[ww,*],/nan)
endfor
;oplot, wvls, -(nabove[*,n]-zabove[*,n])+(nbelow[*,n]-zbelow[*,n]), color=250
oplot, wvls, -(nab_mean-zab_mean)+(nbe_mean-zbe_mean), color=250
legend,['Model','Measured'],textcolors=[0,250],box=0,charsize=1.8

if 0 then begin
  plot, wvls, zabove[*,n], title='Measured irradiance', xtitle='Wavelength (nm)', ytitle='Irradiance (W/m!U2!N)', yrange=[0,2.0]
  oplot, wvls, zbelow[*,n], color=70
  oplot, wvls, nabove[*,n], color=0, linestyle=2
  oplot, wvls, nbelow[*,n], color=70, linestyle=2
endif else begin
  plot, wvls, zab_mean, title='Measured irradiance', xtitle='Wavelength (nm)', ytitle='Irradiance (W/m!U2!N)', yrange=[0,2.0]
  oplot, wvls, zbe_mean, color=70
  oplot, wvls, nab_mean, color=0, linestyle=2
  oplot, wvls, nbe_mean, color=70, linestyle=2
endelse
legend, ['Downwelling','Upwelling'], color=[0,0],linestyle=[0,1],pspacing=1.5,corners=c1,/right,position=[0.9,0.75],box=0,/normal,charsize=1.8
legend,['|','|'], color=[70,70],box=0,/right,pspacing=1.5,linestyle=[0,1],/normal,position=[c1[1]+0.262,c1[3]],charsize=1.8
xyouts,0.9,0.75,/normal,'Top | Bottom',alignment=0.485,charsize=1.8

xyouts, 0.5,0.96, alignment=0.5, /normal, charsize=3.0, 'Aerosol retrieval spectrum for ARCTAS !C'+date+' at Latitude: '+strtrim(string(wvl0353.field01[0,n]),2)
device, /close
spawn, 'convert "'+dir+'rtm_results_spc'+strtrim(string(n),2)+'.ps" "'+dir+'rtm_results_spc'+strtrim(string(n,format='(I03)'),2)+'.png"'
spawn, 'rm -f "'+dir+'rtm_results_spc'+strtrim(string(n),2)+'.ps"
;endfor
endif

;; plottting for relative forcing efficiency over time
if 0 then begin
restore, '/home/leblanc/CALNEX/forcing_err.out'
set_plot, 'ps'
  loadct, 39,/silent
  device, /encapsulated
   device, /tt_font, set_font='Helvetica Bold'
   device, filename=dir+date+'_effp_time.ps'
   device,/color,bits_per_pixel=8.
   device, xsize=20, ysize=20
   !p.font=1 & !p.thick=5
   !p.charsize=2.0 & !x.style=1
   !y.style=1 & !z.style=1
   !y.thick=1.8 & !x.thick=1.8
  !p.multi=0
 !y.omargin=[0,3]
  plot, wvl0499.field01[0,*], effp[3,*,1], title='Aerosol direct Forcing',ytitle='Relative forcing efficiency(%)',xmargin=[7,4],xtitle='Latitude (degrees)', yrange=[-75,15],/nodata, xticks=4, xtickformat='(F8.3)'
 !p.thick=10
tvlct, 210,210,210,210
errplot, wvl0499.field01[0,*], effp[3,*,1]-eff_err[3,1],effp[3,*,1]+eff_err[3,1],color=210
;polyfill, [wvl0499.field01[0,*],reverse(wvl0499.field01[0,*])],[effp[3,*,1]-eff_err[3,1],reverse(effp[3,*,1]+eff_err[3,1])],color=200
  oplot, wvl0499.field01[0,*], effp[3,*,1],linestyle=2
  ;errplot, wvl0499.field01[0,*], effp[3,*,1]-eff_err[3,1],effp[3,*,1]+eff_err[3,1]

errplot, wvl0499.field01[0,*], effp[3,*,0]-eff_err[3,0],effp[3,*,0]+eff_err[3,0],color=210
;polyfill, [wvl0499.field01[0,*],reverse(wvl0499.field01[0,*])],[effp[3,*,0]-eff_err[3,0],reverse(effp[3,*,0]+eff_err[3,0])],color=200
  oplot, wvl0499.field01[0,*], effp[3,*,0], linestyle=0
  ;errplot, wvl0499.field01[0,*], effp[3,*,0]-eff_err[3,0],effp[3,*,0]+eff_err[3,0]

  oplot, [wvl0499.field01[0,137],wvl0499.field01[0,137]], [effp[3,137,0],effp[3,137,0]], psym=2, color=250
  oplot, [wvl0499.field01[1,137],wvl0499.field01[1,137]], [effp[3,137,1],effp[3,137,1]], psym=2, color=250
legend,['Top of layer', 'Bottom of layer'], linestyle=[2,0], box=0, pspacing=1.5,/bottom,/right
device, /close
spawn, 'convert '+dir+date+'_effp_time.ps '+dir+date+'_effp_time_r.png'
spawn, 'rm -f '+dir+date+'_effp_time.ps
endif
;stop
; plot a single point spectrum for comparison
if 0 then begin
n=112
set_plot, 'ps'
  loadct, 39,/silent
  device, /encapsulated
   device, /tt_font, set_font='Helvetica Bold'
   device, filename=dir+date+'_effp.ps'
   device,/color,bits_per_pixel=8.
   device, xsize=20, ysize=20
   !p.font=1 & !p.thick=5
   !p.charsize=2.0 & !x.style=1
   !y.style=1 & !z.style=1
   !y.thick=1.8 & !x.thick=1.8
  !p.multi=0

  plot, wvl, effp[*,n,1], title='Aerosol direct forcing spectra',ytitle='Relative forcing efficiency(%)',xmargin=[7,4],xtitle='Wavelength (nm)', yrange=[-75,15],/nodata
 !p.thick=10
  oplot, wvl, effp[*,n,1],linestyle=2
  errplot, wvl, effp[*,n,1]-eff_err[*,1],effp[*,n,1]+eff_err[*,1]
;  tvlct, 196,255,175,151
  ;for i=0, n_elements(effp[0,*,1])-1 do oplot, wvl, effp[*,i,1],color=151
;  tvlct, 0, 150, 0, 150
;  oplot, wvl, effp[*,n,1],color=150

  oplot, wvl, effp[*,n,0], linestyle=0
  errplot, wvl, effp[*,n,0]-eff_err[*,0],effp[*,n,0]+eff_err[*,0]
 ; tvlct, 175,247,255,71
  ;for i=0, n_elements(effp[0,*,0])-1 do oplot, wvl, effp[*,i,0],color=71
;  tvlct, 0,17 ,200,70
;  oplot, wvl, effp[*,n,0], color=70
;  oplot, wvl, effp[*,n,1],color=150

;legend,['Top of Layer', 'Bottom of Layer']
; green strong aborption
tvlct, 155,187,89, 72
openr, 98, '/home/leblanc/CALNEX/schmidt_2010/green-fe.dat'
gn=fltarr(4,12)
readf, 98, gn
close,98

oplot, gn[0,*],gn[1,*], linestyle=0, color=72 ;BOL
oplot, gn[0,*],gn[3,*], linestyle=2, color=72 ;TOL

; red aged aerosol layer
tvlct, 192,80,77,73
openr, 98, '/home/leblanc/CALNEX/schmidt_2010/red-fe.dat'
rd=fltarr(5,12)
readf, 98, rd
close,98

oplot, rd[0,*],rd[1,*], linestyle=0, color=73 ;BOL
oplot, rd[0,*],rd[3,*], linestyle=2, color=73 ;TOL

legend,['CalNex','Strongly absorbing aerosols','Slightly aged aerosols'],textcolors=[0,72,73], /right,/bottom

device, /close
spawn, 'convert '+dir+date+'_effp.ps '+dir+date+'_effp.png'
spawn, 'rm -f '+dir+date+'_effp.ps'
endif

end
