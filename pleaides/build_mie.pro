; program to build the multiple mie input files and the run list file

pro build_mie

dirin='/lustre/janus_scratch/leblanse/cloud/input/mie/'
dirout='/lustre/janus_scratch/leblanse/cloud/output/mie/'
spawn, 'mkdir '+dirin
spawn, 'mkdir '+dirout

dir='/projects/leblanse/cloud/'
mie_dir='/projects/leblanse/libradtran/libRadtran-1.7/bin/mie'

fn_li=dir+'run_mie_all_wvu.sh'
openw, lun, fn_li,/get_lun

wvls=findgen(1901)+350.
wvls=findgen(1301)+400.
wvls=findgen(751)*2.+400.

fs=file_search('/lustre/janus_scratch/leblanse/cloud/output/mie/*.cdf')

for i=0, n_elements(wvls)-1 do begin
  fnin=dirin+'mie_all_wv'+string(wvls[i],format='(I04)')+'.in'
  fnout=dirout+'mie_all_wv'+string(wvls[i],format='(I04)')+'.out'
  ff='/lustre/janus_scratch/leblanse/cloud/output/mie/wc.all'+string(wvls[i],format='(I04)')+'.mie.cdf'
  
  n=where(fs eq ff,m)
  if wvls[i] eq 502 then stop
  if m gt 0 then continue

  printf, lun, mie_dir+' < '+fnin+' > '+fnout
  openw, lut, fnin,/get_lun
  printf, lut, 'mie_program MIEV0    # Select Mie code by Wiscombei'
  printf, lut, 'refrac water         # Use refractive index of water'
  printf, lut, 'r_eff 1 30 1         # Specify effective radius grid'
  printf, lut, 'distribution gamma 7 # Specify gamma size distribution (alpha=7)'
  printf, lut, 'wavelength '+string(wvls[[i,i]],format='(I4," ",I4)')
  printf, lut, 'wavelength_step 1'
  printf, lut, 'nstokes 1            # Calculate all phase matrix elements'
  printf, lut, 'nmom 6000            # Number of Legendre terms to be computed'
  printf, lut, 'nmom_netcdf 6000      # Number of Legendre terms to be stored in'
  printf, lut, 'nthetamax 15000       # Maximum number of scattering angles to be'
  printf, lut, 'output_user netcdf   # Write output to netcdf file'
  printf, lut, 'verbose              # Print verbose output'
  printf, lut, 'basename '+dirout+'wc.all'+string(wvls[i],format='(I04)')+'.' 
  close,lut
  free_lun, lut
  print, i+1
endfor
close,lun
free_lun,lun


end


;mie_program MIEV0    # Select Mie code by Wiscombei
;refrac water         # Use refractive index of water
;r_eff 1 50 1         # Specify effective radius grid
;distribution gamma 7 # Specify gamma size distribution (alpha=7)
;wavelength  350  350
;wavelength_step 1
;nstokes 1            # Calculate all phase matrix elements
;nmom 256             # Number of Legendre terms to be computed
;nmom_netcdf 256      # Number of Legendre terms to be stored in
;nthetamax 15000       # Maximum number of scattering angles to be
;#output_user netcdf   # Write output to netcdf file
;output_user lambda r_eff qext qsca omega gg spike pmom
;verbose              # Print verbose output
;basename /lustre/janus_scratch/leblanse/cloud/output/mie/wc.all0001.

