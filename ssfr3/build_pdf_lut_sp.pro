; program to get the standard deviation of the pars_lut

pro build_pdf_lut,ll
dir='C:\Users\Samuel\Research\SSFR3\'

ll='day'
if ll ne 'all' and ll ne 'day' then $
restore, dir+'data\sp_par_'+ll+'_v1_20120525.out'

case ll of
'ab':par=pars[*,*,*,*,0]
'pw':begin
      restore, dir+'data\par_ab_v1_20120525.out'
      par=pars[*,*,*,*,0]
      restore, dir+'data\sp_par_'+ll+'_v1_20120525.out'
     end
'z':par=pars[*,*,*,*,0]
'all':begin
      restore, dir+'data\par_ab_v1_20120525.out'
      par=pars[*,*,*,*,0]
      pars_ab=pars
      restore, dir+'data\par_pw_v1_20120525.out'
      pars_pw=pars
      restore, dir+'data\par_z_v1_20120525.out'
      pars_z=pars
     end
'day':begin
      restore, dir+'data\sp_par_ab_v1_20120525.out'
      par=pars[*,*,*,*,0]
      pars_ab=pars
      sp_ab=sp_hiu
      restore, dir+'data\sp_par_pw_v1_20120525.out'
      pars_pw=pars
      sp_pw=sp_hiu
      restore, dir+'data\sp_par_z_v1_20120525.out'
      pars_z=pars
      sp_z=sp_hiu
     end
endcase

nz=n_elements(zenlambda)

;std=fltarr(n_elements(taus),n_elements(refs),2,nz)
sabt=transpose(sp_ab[*,*,*,*,1:3])
spwt=transpose(sp_pw[*,*,*,*,1:2])
spzt=transpose(sp_z[*,*,*,*,1:2])

tsp=[sabt,spwt,spzt]
sps=transpose(tsp)
std=stddev(sps,dimension=5) 
rg=std
for t=0,n_elements(taus)-1 do for r=0,n_elements(refs)-1 do for w=0, 1 do for v=0, n_elements(zenlambda)-1 do rg[t,r,w,v]=max(abs(sps[t,r,w,v,*]-sp_ab[t,r,w,v,0]),/nan)

sp=sp_ab[*,*,*,*,0]


tpar=[transpose(pars_ab[*,*,*,*,1:3]),transpose(pars_pw[*,*,*,*,1:2]),transpose(pars_z[*,*,*,*,1:2])]
parr=transpose(tpar)
stdp=stddev(parr,dimension=5)

;for t=0, n_elements(taus)-1 do $ ;begin
; for r=0, n_elements(refs)-1 do  $ ;begin
; for w=0, 1 do $ ;begin
; for p=0, nz-1 do $ ;begin       
; if ll eq 'all' then std[t,r,w,p]=stddev([reform(pars_ab[t,r,w,p,*]),reform(pars_pw[t,r,w,p,*]),reform(pars_z[t,r,w,p,*])]) else $
; if ll eq 'day' then std[t,r,w,p]=stddev([reform(pars_ab[t,r,w,p,1:3]),$
;                                          reform(pars_pw[t,r,w,p,1:2]),reform(pars_z[t,r,w,p,1:2])]) else $
;        std[t,r,w,p]=stddev(pars[t,r,w,p,*])
 ;     endfor
 ;   endfor
 ; endfor
;endfor

;pars=par
wvl=zenlambda
save, taus, refs, wp, sp,par,stdp, std,wvl, filename=dir+'data\sp_std_day_v1.out'
save, taus, refs, wp, sp,par,stdp, rg,wvl, filename=dir+'data\sp_rg_day_v1.out'
stop
end
