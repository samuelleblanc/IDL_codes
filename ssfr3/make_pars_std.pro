;program to build the uncertainty in parameters due to uncertainty in measurement

@get_params4.pro
@make_meas_std.pro
pro make_pars_std,date

if n_elements(date) lt 1 then date='20120525'
vv='v1'

if date eq '20130110' then winter=1 else winter=0

dir='C:\Users\Samuel\Research\SSFR3\'

;restore files to use with make_meas_std
print, 'restoring files for make_meas_std'
restore, dir+'data\dark_sample.out'; '/home/leblanc/SSFR3/data/dark_sample.out'
dark_s={z_dk:z_dk[0:100,*],zenlambda:zenlambda} ;only a subset of all the darks
restore, dir+'data\resps.out' ;'/home/leblanc/SSFR3/data/resps.out'
resps={zenlambda:zenlambda,zresp1:zresp1,zresp2:zresp2,zresp3:zresp3,zresp4:zresp4}

;restore luts
fn='C:\Users\Samuel\Research\SSFR3\data\sp_par_sza_'+vv+'_'+date+'.out'
print, 'restoring:'+fn
restore, fn
imu=1 ; reference mu

;set the variables
nt=n_elements(taus)
nr=n_elements(refs)
nw=2
wvl=zenlambda
nu=n_elements(mu)


std=pars*0.

for u=0,nu-1 do begin
  print, 'on mu:',u
  for w=0, nw-1 do begin
    print, 'on phase of:',w
    for r=0, nr-1 do begin
      if w eq 0 and r gt 29 then continue
      if w eq 1 and r lt 9 then continue
      for t=0, nt-1 do begin
        make_meas_std,reform(sp_hiu[t,r,w,*,u]),wvl,std_par,win=winter ,dark_s=dark_s,resps=resps,par=par
        std[t,r,w,*,u]=std_par
        ;stop
        print, u,w,r,t
      endfor ; tau loop
    endfor ;ref loop
  endfor ; phase loop
endfor ;mu loop


fp=dir+'data\par_std_sza_'+vv+'_'+date+'.out'
print, 'saving to ',fp
save, std,pars,mu,refs,taus,filename=fp
stop
end
