;+
; NAME:
;   read_aeronet
;
; PURPOSE:
;   Reading aeronet data from caltech during calnex
;
; CATEGORY:
;   CALNEX / CalTech Aeronet / Reader 
;
; CALLING SEQUENCE:
;   read_aeronet, filepath, aod, jul_day,
;
; OUTPUT:
;   aerosol optical depth
;
; KEYWORDS:
;   AERONET, CALTECH, CALNEX
;
; DEPENDENCIES:
;
; NEEDED FILES:
;   - Aeronet files (level 1.5)
;   - Aeronet files (level 2)
;
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
; Written:  Samuel LeBlanc, LASP CU Boulder, June 3rd, 2010
;
;- --------------------------------------------------------------------------

pro read_aeronet, filepattern, aod, jul_day

  file=file_search(filepattern+'*',count=nf)

  for i=0, nf-1 do begin
	temp=strsplit(file[i],'.',/extract)
	type=temp[1]
	
	openr, lun, file[i], /get_lun
	str=''
	case type of
		'lev15' : begin ; level 1.5 Aerosol Optical Depth file
			print, 'level 1.5 aerosol optical depth file'
			wvl_aot=[1640.,1020.,870.,675.,667.,555.,551.,532.,531.,500.,490.,443.,440.,412.,380.,340.]
			n_wvl=n_elements(wvl_aot)
			aot_t=fltarr(n_wvl-1)
			readu, lun, str
			readu, lun, str
			readu, lun, str
			readu, lun, str	; 4 lines of header
			readu, lun, str ; line of variable labels
			labels=strsplit(str, ',',/extract)
			number=0
			while not eof(lun) do begin
				readu, lun, str ; data informations
				data=strsplit(str, ',', /extract)
				reads, data[0], day_t,month_t,year_t,format='(I2,x,I2,x,I4)'
				reads, data[1], hour_t, min_t, sec_t, format='(I2,x,I2,x,I2)'
				if number eq 0 then begin
					day=day_t & month=month_t & year=year_t
					hour=hour_t & minute=min_t & second=sec_t
					jul_day=float(data[2])
					for ii=0, n_wvl-1 do begin
						if data[ii+3] ne 'N/A' then aot_t[ii]=float(data[ii+3]) else aot_t[ii]=!values.f_nan
					endfor
					aot=aot_t
				endif else begin
					day=[day,day_t] & month=[month,month_t] & year=[year,year_t]
					hour=[hour,hour_t] & minute=[minute,min_t] & second=[second,sec_t]
					jul_day=[jul_day,float(data[2])]
					for ii=0, n_wvl-1 do begin
						if data[ii+3] ne 'N/A' then aot_t[ii]=float(data[ii+3]) else aot_t[ii]=!values.f_nan
					endfor
					aot=[aot,aot_t]					
				endelse
			endwhile
		end
		
		'asy' : begin ; asymmetry parameter
			print, 'asymettry parameter file'
		
		end
  
		'aot2' : begin ; optical thickness
			print, 'aerosol optical thickness file'
		end
		
		'ssa' : begin ; Single Scattering Albedo
			print, 'Single Scattering Albedo file'
		end
		
		'flux' : begin ; irradiance
			print, 'flux Irradiance file'
		end
		
		'forc' : begin ; forcing
			print, 'Forcing file'
		end
		
	endcase
	
	free_lun, lun
  endfor
end