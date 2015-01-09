function pressure,H
; preliminary: use standard atmosphere - international height formula. H in m.
p=1013.25*(1.-H*22.5577E-6)^5.255 ; hPa
achtung=where(H gt 41000.0,nacht)
if nacht gt 0 then p[achtung]=498.0*exp(-0.000129*H)
return,p
end

function tau_rayleigh,WV,H1,H2
; H1 <- height [m], H1 > H2
; H2 <- height [m]
wv*=0.001
;wavelength in microns
;pressure in mbars (P2 > P1)
num      = 1.0455996 - 341.29061*WV^(-2.) - 0.90230850*WV^(2.)
den      = 1.0 + 0.0027059889*WV^(-2.) - 85.968563*WV^(2.)
tray     = 0.00210966*(num/den)*((pressure(H2) - pressure(H1))/1013.25)
return,tray
end

function rayleigh,sza,alt,wl,extra,DIRDIFF=dd,REALTOA=rt,EXTRA_IS_SPEC=exspec
 ; return global (diffuse+direct) Rayleigh irradiance
 ; Input parameters:
 ;   sza         Solar zenith angle, in degrees
 ;   alt         Altitude, in meter
 ;   wl          Wavelength, in nm
 ;   extra       Path of file that contains extraterrestrial spectrum  (assumed to be in mW m-2 nm-1, libRadtran standard)
 ; Output
 ;   irr         Global downwelling irradiance, in W m-2 nm-1, vector if sza is a vector!!!
 ; OPTIONS
 ;   /DIRDIFF    Instead of irr, return an array that contains irr and the diffuse fraction d_g = diff / (dir+diff)
 ;   /REALTOA    put TOA at 120 km instead of 20 km
h0=20000.0  ; reference height to be used as 'TOA'
if keyword_set(rt) then h0=120000.0

if NOT keyword_set(exspec) then begin
openr,ur,extra,/get_lun
dummy='l'
for l=0,10 do readf,ur,dummy
i=0
while not eof(ur) do begin
  readf,ur,l0,i0
  if i eq 0 then begin
    ll=l0 & ii=i0
  endif else begin
    ll=[ll,l0] & ii=[ii,i0]
  endelse
  i++
endwhile
free_lun,ur
endif else begin   ; calling pro provides spectrum. Faster than reading the extra file over and over again.
 ll=reform(extra[0,*])
 ii=reform(extra[1,*])
endelse

wlr=wl
ii =smooth(ii,7)                        ; smooth to approximately account for SSFR slit function
tau=tau_rayleigh(wlr,h0,alt)            ; tau between h0 (reference) and input alt (altitude)
iu0=interpol(ii,ll,wl)*0.001            ; irradiance at requested wavelength, SCALED to W m-2 nm-1
iu0*=cos(sza*!pi/180.)                  ; multiply with cos(sza)
 iu =exp(-tau/cos(!pi/180.*sza))*iu0     ; transmitted direct
 id =(1-exp(-tau/cos(!pi/180.*sza)))*0.5 ; diffuse forward
    irr = iu + id
if keyword_set(dd) then return,[irr,id/irr] else return,irr
end


