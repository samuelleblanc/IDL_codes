; program to read in the values outputted by the uvspec program on the janus cluster
; used to build the look up tables for radiance/irradiance retrieval of aerosol properties at Skywatch

@zensun.pro
@/home/leblanse/libradtran/pro/libradtran_reader.pro

pro read_sza_mix3,mu=mu,date

if n_elements(mu) lt 1 then mu_stop=0 else mu_stop=1

u=(findgen(20)+1.)/20.
szas=acos(u)/!dtor


;set the proper directories
sets=2   ;0=normal, 1=layers, 2=levels

;set the proper directories
if n_elements(date) lt 1 then date='20120524' else if strmid(date,0,1) ne '2' then date=date+'20120524'
if date eq '20120913' then date='20120913_alb'
case sets of
  0: lbl='sp_mix3_'+date
  1: lbl='sp_mix3_lay_'+date
  2: lbl='sp_mix3_lvls_'+date
endcase

if strmid(date,0,4) eq 'pw05' then lbl='sp_mix3_sza_pw05-20120524' else if strmid(date,0,4) eq 'pw50' then lbl='sp_mix3_sza_pw50-20120524' else $
lbl='sp_mix3_sza_'+date

outdir='/lustre/janus_scratch/leblanse/cloud/output/'+lbl+'/';'/scratch/stmp00/leblanse/cloud_rad/output/'
indir='/lustre/janus_scratch/leblanse/cloud/input/'+lbl+'/'
dir   ='/projects/leblanse/cloud_rad/'
restore, dir+'20120523_calibspcs.out'

;;;;; restore the total file
tau=[1.,2.,3.,4.,5.,7.5,10.,15.,20.,25.,30.,40.,50.,60.,70.,80.,90.,100.,125.,150.,175.,200.]
refl=[2.5,5.,7.5,10.,12.5,15.,17.5,20.,22.5,25.,30.,35.,40.,45.,50.]
refi=[15.,20.,25.,30.,35.,40.,45.,50.]
wp=[0.,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0]

lat=40.007916667
lon=-105.26825
date='20120524'
doy=julian_day(strmid(date, 0, 4),strmid(date,4,2),strmid(date,6,2))
azi=0.

;set up the model run
zout=[0.02]
;zout=[0.02,10.]

;set the wavelenghts to be interpolated
u=where(zenlambda ge 400. and zenlambda lt 2200.)
;sp=fltarr(n_elements(tau),n_elements(refl),n_elements(refi),n_elements(zenlambda[u]),n_elements(wp),2)
sp=fltarr(n_elements(tau),n_elements(refl),n_elements(refi),n_elements(zenlambda[u]),n_elements(wp))
sp_irrdn=sp
sp_irrup=sp
bad=0 & tot=0

if mu_stop then begin
s_start=mu & s_stop=mu
endif else begin
s_start=0 & s_stop=n_elements(szas)-1
endelse
for s=s_start, s_stop do begin
  indir ='/lustre/janus_scratch/leblanse/cloud/input/'+lbl+'/mu'+string(s,format='(I02)')+'/' ;'/scratch/stmp00/leblanse/cloud/input/'
  outdir='/lustre/janus_scratch/leblanse/cloud/output/'+lbl+'/mu'+string(s,format='(I02)')+'/' ;'/scratch/stmp00/leblanse/cloud/output/'
  sza=szas[s]

  for t=0, n_elements(tau)-1 do begin
    for r=0, n_elements(refl)-1 do begin
      for ri=0, n_elements(refi)-1 do begin
        for w=0, n_elements(wp)-1 do begin
          fn='cloud_tb_ta'+string(t,format='(I02)')+'_rl'+string(r,format='(I02)')+'_ri'+string(ri,format='(I02)')+'_wp'+string(w,format='(I02)')
          outf=outdir+fn+'.out'
          inf=indir+fn+'.in'
          tot=tot+1
          fi=file_test(outf)
          if fi ne 1 then begin
            bad=bad+1
            continue
          endif
          fi=file_lines(outf)
          if fi le 1 then begin
            bad=bad+1
            continue
          endif
          output=libradtran_reader(file=outf,/radiance,zout=zout,/quiet)
          sp[t,r,ri,*,w,*]=output.rad
          sp_irrdn[t,r,ri,*,w,*]=output.dir_dn+output.dif_dn
          sp_irrup[t,r,ri,*,w,*]=output.dif_up
         if w eq 0 and ri eq 0 then  print, lbl+'/'+fn
        endfor ; wp loop
      endfor ; refi loop
    endfor ;ref loop
  endfor  ;tau loop
print, 'bad points: ',bad,'/',tot
zenlambda=zenlambda[u]
dir='/projects/leblanse/cloud/model/'
save, zenlambda, mu,s,tau, refl, refi, wp, sp, sp_irrdn,sp_irrup,szas,filename=dir+lbl+'_mu'+string(s,format='(I02)')+'.out'
endfor ; sza loop
stop
end
