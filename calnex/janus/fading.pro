pro fading

loadct, 39
set_plot, 'x'
device, decomposed=0
   !p.font=1 & !p.thick=5
   !p.charsize=2.0 & !x.style=1
   !y.style=1 & !z.style=1
   !y.thick=1.8 & !x.thick=1.8
  !p.multi=0

window, 0, xsize=800, ysize=900
nan=!values.f_nan
spectrum=[1.,2.,3.,NAN,5.,6.,7.]
error=[0.5,0.4,0.3,0.3,0.9,0.1,0.5]
wvl=[340,380,440,500,675,870,1020]

plot, wvl, findgen(7)+1, /nodata
tvlct,255,180,180,251
sst=spectrum*(1.-error)
k=where(sst le 1.0,op)
if op gt 0 then sst[k]=1.
sstt=spectrum*(1.+error)
k=where(sstt ge 7.0,op)
if op gt 0 then sstt[k]=7.0

kf=where(finite(spectrum) eq 0, ofk)

if (ofk) le 0 then begin
  polyfill, [wvl,reverse(wvl)],[sst,reverse(sstt)],color=251 
endif else begin
kfp=where(finite(spectrum) ne 0, ofp)
wvl_f=wvl[kfp]
sst_f=sst[kfp]
sstt_f=sstt[kfp]
polyfill, [wvl_f,reverse(wvl_f)],[sst_f,reverse(sstt_f)],color=251
endelse
oplot, wvl, spectrum,color=249
stop
end