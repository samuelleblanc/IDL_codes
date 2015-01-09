;+
; NAME:
;   retrieve_cloud_parms
;
; PURPOSE:
;   To retrieve the cloud parameters from transmitted radiance
;
; CATEGORY:
;   SSFR3 roof top real-time processing, cloud retrievals
;
; CALLING SEQUENCE:
;   retrieve_cloud_parms, lutdir,date,obsspectra,obslambda,tmhrs,lats,lons,tau,reff,tau_unc,reff_unc
;   - where lutdir is the directory of the look up directory
;   - date is the date in yyyymmdd format
;   - obsspectra is the array of joined radiance spectra
;   - obslambda is the wavelength array for the radiance spectra
;   - tmhrs is the UTC time in hours
;   - lats is the array of latitudes
;   - lons is the array of longitudes
;   - tau is the returned optical depth of the cloud
;   - reff is the returned effective radius
;   - tau_unc is the returned optical depth uncertainty
;   - reff_unc is hte returned effective radius uncertainty
;
; OUTPUT:
;   arrays of optical depth, effective radius, and their uncertainty
;
; KEYWORDS:
;   - none
;
; DEPENDENCIES:
;   - none
;   
; NEEDED FILES:
;   - look up directory
;  
; EXAMPLE:
;  retrieve_cloud_parms, lutdir,'20120531',obsspectra,obslambda,tmhrs,lats,lons,tau,reff,tau_unc,reff_unc
;
;
; MODIFICATION HISTORY:
; Written:  Patrick McBride, LASP CU, date unknown
; Modified: May 30th, 2012 by Samuel LeBlanc
;           - modified to run in real time with SSFR3 data
;           - added some comments
;-

pro retrieve_cloud_parms,lutdir,date,obsspectra,obslambda,tmhrs,lats,lons,tau,reff,tau_unc,reff_unc

lut_dir = lutdir

;; Define the spectrometer uncertainty
deltarad = 0.05

;; This code was adapted from code that did the retrieval over a time
;; series. So here I tied it to 1 spectrum, but left the array to
;; limit the changes.
numspectra = n_elements(tmhrs)

;; Translate time to mu
mus = double(cos(get_zenangle(date,tmhrs,lats,lons)*!pi/180.0))

;; Array of the available mus in the library look-up-table
libmus = [0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95]

firstlook=1
uniqueflag = fltarr(numspectra)
bftaus = fltarr(numspectra)
bfreffs = fltarr(numspectra)
bflwps = fltarr(numspectra)
avgtaus = fltarr(numspectra)
avgreffs = fltarr(numspectra)
unctaus = fltarr(numspectra)
uncreffs = fltarr(numspectra)
unclwps = fltarr(numspectra)
obsslopes = fltarr(numspectra)
obssigmas = fltarr(numspectra)
obsrad = fltarr(numspectra)
iceflag = fltarr(numspectra)

;; This is more legacy code that will be tied to deal with the one second retrieval
lim1=double(0)
lim2=double(numspectra)

; Wavelength used for tau retrieval
tauwls = [515]

; Wavelengths used for reff retrieval
reffwls = [1565,1571,1577,1582,1588,1594,1600,1605,1611,1617,1623,1628,1634]

; Wavelengths used for phase detection
pwls = [1668,1674,1679,1685,1691,1696]

wls = [tauwls,reffwls]
nwls = n_elements(wls)
npwls=n_elements(pwls)

;; If the library files have wavelengths that are not used in the
;; retrieval, these arrays hold the incides of the library wls that
;; are used. This confuses things but during the develpment of the
;; algorithm it was a pain to ensure that the retrieval code and
;; library matched exactly. Hopefully we can take this out.
modelinds = intarr(nwls)
pmodelinds = intarr(npwls)
libinds = intarr(nwls)
swsrad = fltarr(nwls)
lastmustr = ''

for snum = lim1,lim2-1 do begin

    tmhr = tmhrs[snum]
    mu = double(mus[snum])
    obsspectrum = obsspectra[*,snum]
    
    ;; Print out the time roughly every
    ;; hour to let someone know you're alive
    if((snum MOD 3600) eq 0) then print,'Retrieval time: '+string(tmhr)

    decs = 100.
    rndto = 5
    digits = fix(mu*decs)
    
    if((digits MOD rndto) lt (rndto/2.0)) then begin
        modelmu = (digits-(digits MOD rndto))/100.
        modelmu = libmus[min(abs(libmus-mu),mnind)]
        modelmu = libmus[mnind]
        ;;mustr = 'mu'+string(modelmu ,format='(F5.3)')+'00000'
        mustr = 'mu'+string(modelmu ,format='(F4.2)')
    endif else begin
        modelmu = (digits+(rndto-(digits MOD rndto)))/100.
        modelmu = libmus[min(abs(libmus-mu),mnind)]
        modelmu = libmus[mnind]
        ;;mustr = 'mu'+string( modelmu,format='(F5.3)')+'00000'
        mustr = 'mu'+string( modelmu,format='(F4.2)')
    endelse
    if(abs(mu-modelmu) ge 0.05) then begin
        print,"A mu too far. Skipping"
        if(mu lt 0) then begin
            print,'NEGATIVE MU'
            bftaus[snum]=-9999
            bfreffs[snum]=-9999
            avgtaus[snum]=-9999
            avgreffs[snum]=-9999
            unctaus[snum]=-9999
            uncreffs[snum]=-9999
            stop
            continue
        endif
        stop
    endif
    
    ;;libdir = mainlibdir + season +'/'+doystr+'/'+salb+'/'+aer+'/'+leg_g+'/'+cbasestr+'/'+mustr+'/'
    lut_file = lut_dir+'/slope_lut_'+mustr+'.sav'
    if(mustr ne lastmustr) then begin

        ;restore,libdir+'sws_lib_with_slopes_515_meas_no_err.sav',/verbose
        ;restore,libdir+'sws_lib_phase.sav',/verbose
        restore,lut_file;;,/verbose

        rtrad=fltarr(nradii,ntaus,nwls)
        radii2d = intarr(nradii,ntaus)
        taus2d  = intarr(nradii,ntaus)
        taus2d  = fltarr(nradii,ntaus)

        for ii=0,ntaus-1 do begin
            radii2d[*,ii]=radii
        endfor
        for ii=0,nradii-1 do begin
            taus2d[ii,*]=taus
        endfor

        if(firstlook) then begin
            firstlook=0
            for ii=0,nwls-1 do begin
                modelinds[ii] = min(where(wlmodel ge wls[ii]))
                libinds[ii] = min(where(libwls ge wls[ii]))
                if(abs(libwls[libinds[ii]]-wls[ii]) gt 1) then begin
                    print,"wavelengths too far"
                    stop
                endif
            endfor
            for ii=0,npwls-1 do begin
                pmodelinds[ii] = min(where(wlmodel ge pwls[ii]))
                if(abs(wlmodel[pmodelinds[ii]]-pwls[ii]) gt 1) then begin
                    print,"wavelengths too far"
                    stop
                endif
            endfor
        endif else begin
            tmp=where(abs(libwls[libinds]-wls) gt 1)
            if(tmp[0] ne -1) then stop
        endelse            

        mycloudF0 = fltarr(n_elements(libinds))
        mycloudF0[*] = cloudF0[10,40,libinds]



        for ii=0,n_elements(libinds)-1 do begin
            rtrad[*,*,ii] = !pi*raddata[*,*,libinds[ii]]/(mycloudF0[ii])
        endfor

    endif

    lastmustr = mustr
    
    ;;radint=interpol(data.radiance[*,snum],spectra,wlmodel)
    radint=interpol(obsspectrum,obslambda,wlmodel)
    
    ;; Initialize the important variables for each second's retrieval
    radmin=10.e5
    
    taurads=fltarr(nradii,ntaus)
    slopeerrs=fltarr(nradii,ntaus)
    rtslopeerrs=fltarr(nradii,ntaus)
    toterrs=fltarr(nradii,ntaus)
    raderrs=fltarr(nradii,ntaus)
    
    swsrad[*] = !pi*radint[modelinds]/(mycloudF0)
    taurads = rtrad[*,*,0]
    raderrs[*,*] = ((rtrad[*,*,0]-swsrad[0])/rtrad[*,*,0])^2

    ratios = swsrad[1:*]/swsrad[1]
    tmp = linfit(wls[1:*],ratios,sigma=tmpsigma,yfit=swsfit)
    obsslope = tmp[1]
    obssigma = tmpsigma[1]
    obsslopes[snum] = obsslope
    obssigmas[snum] = obssigma
    obsrad[snum] = swsrad[0]

    phaserad = radint(pmodelinds)
    tmp = linfit(pwls,phaserad/phaserad[0],sigma=p_sigma)
    obsphaseslope = tmp[1]

    slopeerrs[*,*] = ((rtslopes-obsslope)/rtslopes)^2
    toterrs[*,*]   = raderrs + slopeerrs
    toterrs=sqrt(toterrs/2.)
    tmp=min(toterrs,bfind)

    tmp=min(slopeerrs,slopebfind)
    tmp=min(raderrs,radbfind)

    raduncinds = where(taurads ge obsrad[snum]*(1.-deltarad) and taurads le obsrad[snum]*(1.+deltarad))
    slopeuncinds = where(rtslopes ge obsslope-obssigma and rtslopes le obsslope+obssigma)
    uncinds = where((taurads ge obsrad[snum]*(1.-deltarad) and taurads le obsrad[snum]*(1.+deltarad)) and (rtslopes ge obsslope-obssigma and rtslopes le obsslope+obssigma))

    iceflag[snum] = (obsphaseslope ge 0)

    goodretrieval = (uncinds[0] ne -1)
    if(goodretrieval) then begin
        tmp=radii2d[uncinds]
        if(n_elements(tmp) gt 1) then begin
            tmp=tmp[sort(tmp)]
            diff=tmp-shift(tmp,1)
            inds = where(diff[1:*] gt 1)
            unique = (inds[0] eq -1)
        endif else begin
            unique = 1
        endelse


        tmp=taus2d[uncinds]
        if(n_elements(tmp) gt 1 and unique eq 1) then begin
            tmp=tmp[sort(tmp)]
            diff=tmp-shift(tmp,1)
            inds = where(diff[1:*] gt 1)
            unique = (inds[0] eq -1)
        endif else begin
            unique = 1
        endelse

        uniqueflag[snum] = unique

        mxtau=max(taus2d[uncinds])
        mntau=min(taus2d[uncinds])
        mxreff=max(radii2d[uncinds])
        mnreff=min(radii2d[uncinds])
        
        bftaus[snum]=taus2d[bfind]
        bfreffs[snum]=radii2d[bfind]
        avgtaus[snum]=(mxtau+mntau)/2.
        avgreffs[snum]=(mxreff+mnreff)/2.
        unctaus[snum]=(mxtau-mntau)/2.
        uncreffs[snum]=(mxreff-mnreff)/2.
        
        ;;print,bftaus[snum],bfreffs[snum],unctaus[snum],uncreffs[snum]
        ;;stop
        
        lwps = (2./3.)*taus2d[uncinds]*radii2d[uncinds]
        unclwps[snum] = (max(lwps)-min(lwps))/2.
        bflwps[snum] = (2./3.)*bftaus[snum]*bfreffs[snum]
        ;;print,max(lwps),min(lwps),bflwps[snum],unclwps[snum]
        ;;print,mxtau,mntau,mxreff,mnreff
        ;;stop
    endif else begin
        bftaus[snum]=-9999
        bfreffs[snum]=-9999
        avgtaus[snum]=-9999
        avgreffs[snum]=-9999
        unctaus[snum]=-9999
        uncreffs[snum]=-9999
        ;;stop
    endelse
        

    ;;print,uncinds
    ;;print,tmhr
    ;;print,"  BF : ",bftaus[snum],bfreffs[snum]
    ;;print,"  AVG: ",avgtaus[snum],avgreffs[snum]
    ;;print,"  UNC: ",unctaus[snum],uncreffs[snum]
    ;;stop

endfor

tau = bftaus
reff = bfreffs
tau_unc = unctaus
reff_unc = uncreffs

;save,file=filename,tmhrs,taus,radii,data,mus,wlmodel,libwls,lim1,lim2,minhr,maxhr,unctaus,uncreffs,bftaus,bfreffs,avgtaus,avgreffs,bflwps,unclwps,obssigmas,obsslopes,lim1,lim2,obsrad,mus,iceflag,uniqueflag

end
