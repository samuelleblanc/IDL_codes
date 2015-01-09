;+
; NAME:
;   calnex_compare_cal
;
; PURPOSE:
;   Compare 2 response functions from the p3 calnex
;
; CATEGORY:
;   CALNEX / Calibration
;
; CALLING SEQUENCE:
;   calnex_compare_cal, resp2_arr,  wlvisz,wlnirz,wlvisn,wlnirn
;   
; OUTPUT:
;   PLots of comparison
;
; KEYWORDS:
;   SSFR, P3, CALNEX, Calibration, comparison
;
; DEPENDENCIES:
;   legend.pro ; to make legend on graph
;   
; NEEDED FILES:
;   - none
;  
; EXAMPLE:
;   calnex_compare_cal
;
; MODIFICATION HISTORY:
; Written:  Samuel LeBlanc, LASP CU Boulder, May 15th, 2010
; Modified: 
;---------------------------------------------------------------------------

pro calnex_compare_cal, resp, wlvisz,wlnirz,wlvisn,wlnirn, ref, test, date

;;;;;;;;;;;;;;;;;;
;values to be set
;;;;;;;;;;;;;;;;;;

;ref=4
;test=5
title='Difference between calibrations response functions !C'+date[ref]+' and '+date[test]
path='/data/seven/schmidt/calnex/cal'
l='/'

;;;;;;;;;;;;;;;;;
;plotting nadir
;;;;;;;;;;;;;;;;;

window, 5, title='Nadir'
plot, wlvisn,resp(*,2,ref),color=255,ytitle='%',xtitle='Wavelength (nm)',title=title+' - Nadir',charsize=1.5,thick=1.8,/xs,/ys,xrange=[300,2200],yrange=[-6,6], /nodata, position=[0.1,0.2,0.9,0.8]

    oplot,wlvisn,(resp(*,2,test)-resp(*,2,ref))*100./resp(*,2,ref),thick=1.8,color=255
    oplot,wlnirn,(resp(*,3,test)-resp(*,3,ref))*100./resp(*,3,ref),thick=1.8,color=255

    p=tvrd(true=1)
    write_png,path+l+'nadir_compare.png',p


;;;;;;;;;;;;;;;;;
;plotting zenith
;;;;;;;;;;;;;;;;;

window, 6, title='Zenith'
plot, wlvisz,resp(*,0,ref),color=255,ytitle='%',xtitle='Wavelength (nm)',title=title+' - Zenith',charsize=1.5,thick=1.8,/xs,/ys,xrange=[300,2200],yrange=[-6,6], /nodata, position=[0.1,0.2,0.9,0.8]

    oplot,wlvisz,(resp(*,0,test)-resp(*,0,ref))*100./resp(*,0,ref),thick=1.8,color=255
    oplot,wlnirz,(resp(*,1,test)-resp(*,1,ref))*100./resp(*,1,ref),thick=1.8,color=255

    p=tvrd(true=1)
    write_png,path+l+'zenith_compare.png',p


end



