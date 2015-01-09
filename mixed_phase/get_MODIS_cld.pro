; program to run through a set of retrieved MODIS L2 files to save the proper values of cloud optical depth, effective radius, and phase
; uses geolocation files to get proper pixel

pro get_MODIS_cld
dir='/argus/sat/modis/Boulder/cloud/'
terra=0

if terra then f=file_search(dir+'MOD03*.hdf') else f=file_search(dir+'MYD03*.hdf')
f=f[sort(f)]
nf=n_elements(f)
if terra then f2=file_search(dir+'MOD06*.hdf') else f2=file_search(dir+'MYD06*.hdf')
f2=f2[sort(f2)]

doy=[010,011]
taus=fltarr(nf)
refs=fltarr(nf)
utaus=fltarr(nf)
urefs=fltarr(nf)
phas=fltarr(nf)
muls=fltarr(nf)
tops=fltarr(nf)
phairs=fltarr(nf)

nan=!values.f_nan

obj=Obj_New('NCDF_DATA',f[0])

for i=0, nf-1 do begin
   print,f[i]
   obj -> OpenFile, f[i]
   lats=obj->readvariable('Latitude')
   lons=obj->readvariable('Longitude')

   ; check out if Boulder lat and lon are within this file
   lat=40.007916667
   lon=-105.26825
   nula=min(abs(lats-lat)+abs(lons-lon),nl)
   print, nula, lat,lats[nl],lon,lons[nl]
   stop
   if nula lt 2. then begin
     print,f2[i]  
     obj -> OpenFile, f2[i]
     ref=obj->readvariable('Cloud_Effective_Radius')
;     ref=ref*0.0099999998
     tau=obj->readvariable('Cloud_Optical_Thickness')
;     tau=tau*0.0099999998
     ref_err=obj->readvariable('Cloud_Effective_Radius_Uncertainty')
;     ref_err=ref_err*0.0099999998
     tau_err=obj->readvariable('Cloud_Optical_Thickness_Uncertainty')
;     tau_err=tau_err*0.0099999998
     pha=obj->readvariable('Cloud_Phase_Optical_Properties')
     mul=obj->readvariable('Cloud_Multi_Layer_Flag')
 
     ; the cloud parameters at 5km 
     la=obj->readvariable('Latitude')
     lo=obj->readvariable('Longitude')
     phair=obj->readvariable('Cloud_Phase_Infrared')
     top=obj->readvariable('Cloud_Top_Pressure') ; in hPa
 ;    top=top*0.1
     nulo=min(abs(la-lat)+abs(lo-lon),nll)

     taus[i]=tau[nl] & refs[i]=ref[nl] & utaus[i]=tau_err[nl] & urefs[i]=ref_err[nl] & phas[i]=pha[nl] & muls[i]=mul[nl] & tops[i]=top[nll] & phairs[i]=phair[nll] 

   endif else begin
     taus[i]=nan & refs[i]=nan & utaus[i]=nan & urefs[i]=nan & phas[i]=nan & muls[i]=nan & tops[i]=nan & phairs[i]=nan
   endelse
   print,taus[i],refs[i],utaus[i],urefs[i],phas[i],muls[i],tops[i],phairs[i]
endfor
Obj_Destroy, obj

if terra then fs='Terra_cld.out' else fs='Aqua_cld.out'

save, f2,taus,refs,utaus,urefs,phas,muls,tops,phairs,filename=dir+fs
stop
end
