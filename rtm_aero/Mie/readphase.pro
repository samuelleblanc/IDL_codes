PRO readphase, filename, Angle, Phase11, Phase12, Phase33, Phase34
  ;  Read in the phase matrix file output by miegamma.f

  Nmax=1000
  Angle=fltarr(Nmax)
  Phase11=fltarr(Nmax)
  Phase12=fltarr(Nmax)
  Phase33=fltarr(Nmax)
  Phase34=fltarr(Nmax)
  openr,2, filename
  junk=strarr(1)
  readf,2, junk       ; Skip header line
  i = 0
  while not EOF(2) do begin        ; Read in however many angles there are
    readf,2, mu, theta, p11, p12, p33, p34
    Angle[i] = theta
    Phase11[i] = p11
    Phase12[i] = p12
    Phase33[i] = p33
    Phase34[i] = p34
    i = i + 1
    if (i ge Nmax) then begin
      print, 'Number of angles exceeded : ',Nmax
      stop
    endif
  endwhile
  close,2
  Nangle = i
  Angle=reform(Angle[0:Nangle])
  Phase11=reform(Phase11[0:Nangle-1])
  Phase12=reform(Phase12[0:Nangle-1])
  Phase33=reform(Phase33[0:Nangle-1])
  Phase34=reform(Phase34[0:Nangle-1])
  
END
