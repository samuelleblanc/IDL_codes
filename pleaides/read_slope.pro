; program to read in the values outputted by the uvspec program on the janus cluster
; used to build the look up tables for radiance/irradiance retrieval of aerosol properties at Skywatch

@zensun.pro
@/home/leblanse/libradtran/pro/libradtran_reader.pro

pro read_slope

;set the proper directories
;date='20120524'
date='20120524'
date='20130110'

outdir='/lustre/janus_scratch/leblanse/cloud/output/slope_ic_'+date+'/';'/scratch/stmp00/leblanse/cloud/output/'
dir   ='/projects/leblanse/cloud/model/'

wvls=[515,1565,1634]
alb=[0.0511493,0.27,0.274,0.278,0.282,0.286,0.29,0.294,$
     0.298,0.302,0.306,0.31,0.314,0.32]

taus = [1.,2.,3.,4.,5.,6.,7.,8.,9.,10.,12.,15.,17.,20.,25.,30.,40.,50.,60.,70.,80.,90.,100.]
;refs = [1.,2.,3.,4.,5.,7.,8.,10.,12.,15.,18.,20.,22.,25.,28.,30.]
refs=[10.,11.,12.,13.,14.,15.,16.,17.,18.,20.,22.,23.,25.,28.,$
     30.,32.,35.,38.,40.,43.,45.,48.,50.,52.,55.,58.,60.]

szas = findgen(19)*5.

restore, '/projects/leblanse/cloud_rad/20120523_calibspcs.out'
wv=zenlambda[where(zenlambda gt wvls[1] and zenlambda lt wvls[2])]


ntau=n_elements(taus)
nref=n_elements(refs)
nsza=n_elements(szas)
;nwvl=n_elements(wvls)
nwvl=2

zout=[0.02,5.0,30.0]

rad=fltarr(3,n_elements(wv)+1,ntau,nref,nsza)
irr_dn=rad
irr_up=rad
  for t=0, ntau-1 do begin
    for r=0, nref-1 do begin
      for s=0, nsza-2 do begin
        for v=0, nwvl-1 do begin
          fn='cloud_sl_ta'+string(t,format='(I02)')+'_re'+string(r,format='(I02)')+'_sza'+string(s,format='(I02)')+'_wvl'+string(v,format='(I02)')
          outf=outdir+fn+'.out'
          fi=file_test(outf)
          if fi lt 1 then begin
             print, 'cant find file',outf
             continue
          endif
          fi=file_lines(outf)
          if fi le 1 then message, 'problem with file'
          output=libradtran_reader(file=outf,/radiance,zout=zout,/quiet)
          if v eq 0 then begin
            rad[*,0,t,r,s]=output.rad
            irr_dn[*,0,t,r,s]=output.dir_dn+output.dif_dn
            irr_up[*,0,t,r,s]=output.dif_up
          endif else begin
            for p=0, 2 do begin
              rad[p,1:*,t,r,s]=interpol(output.rad[p,*],output.wvl[p,*],wv)
              irr_dn[p,1:*,t,r,s]=interpol(output.dir_dn[p,*]+output.dif_dn[p,*],output.wvl[p,*],wv)
              irr_up[p,1:*,t,r,s]=interpol(output.dif_up[p,*],output.wvl[p,*],wv)
            endfor

           ; for w=1, n_elements(wvls)-1 do begin
           ;   nul=min(abs(wvls[w]-output.wvl[0,*]),vv)
           ;   rad[*,w,t,r,s]=output.rad[*,vv]
           ;   irr_dn[*,w,t,r,s]=output.dir_dn[*,vv]+output.dif_dn[*,vv]
           ;   irr_up[*,w,t,r,s]=output.dif_up[*,vv]
           ; endfor 
          endelse
          print, outf
        endfor ;wvl loop
      endfor ;sza loop
    endfor ;ref loop
  endfor  ;tau loop

save, wvls, taus, refs, szas,rad, zout,irr_dn,irr_up,filename=dir+'slope_ic_'+date+'.out'

stop
end
