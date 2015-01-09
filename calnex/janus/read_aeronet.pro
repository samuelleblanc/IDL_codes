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
;   - none
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

pro read_aeronet, filepattern,  jul_day, jul_day_almu, wvl_aod,wvl_almu, aod,ssa, sza, asy_TT, aot2_T, up_flux_T, down_flux_T, diff_flux_T, forcing, albedo

  file=file_search(filepattern+'*',count = nf)

  blah=where(strpos(file, '.ssa') gt 0 ,yes)
  if yes eq 0 then print, 'missing file .ssa'
  blah=where(strpos(file, '.force') gt 0,yes)
  if yes eq 0 then print, 'missing file .force'
    blah=where(strpos(file, '.asy') gt 0,yes)
  if yes eq 0 then print, 'missing file .asy'
    blah=where(strpos(file, '.aot2') gt 0,yes)
  if yes eq 0 then print, 'missing file .aot2'
    blah=where(strpos(file, '.flux') gt 0,yes)
  if yes eq 0 then print, 'missing file .flux'
    blah=where(strpos(file, '.lev15') gt 0,yes)
  if yes eq 0 then print, 'missing file .lev15'
  
  for i=0, nf-1 do begin
    temp=strsplit(file[i],'.',/extract)
    type=temp[1]
    
	openr, lun, file[i], /get_lun
	str=''
	case type of
		'lev15' : begin ; level 1.5 Aerosol Optical Depth file
			print, 'level 1.5 aerosol optical depth file'
			wvl_aod=[1640.,1020.,870.,675.,667.,555.,551.,532.,531.,500.,490.,443.,440.,412.,380.,340.]
			n_wvl=n_elements(wvl_aod)
			aod_t=fltarr(n_wvl)
			triplet_t=fltarr(n_wvl)
			for t=0, 3 do begin  ; 4 lines of header
			  readf, lun, str
			endfor
							
			readf, lun, str ; line of variable labels
			labels_aod=strsplit(str, ',',/extract)
			number=0
			while not eof(lun) do begin
				readf, lun, str ; data informations
				data=strsplit(str, ',', /extract)
				reads, data[0], day_t,month_t,year_t,format='(I2,x,I2,x,I4)'
				reads, data[1], hour_t, min_t, sec_t, format='(I2,x,I2,x,I2)'
				if number eq 0 then begin
					day_aod=day_t & month_aod=month_t & year_aod=year_t
					hour_aod=hour_t & minute_aod=min_t & second_aod=sec_t
					jul_day_aod=float(data[2])
					for ii=0, n_wvl-1 do begin
						if data[ii+3] ne 'N/A' then aod_t[ii]=float(data[ii+3]) else aod_t[ii]=!values.f_nan
					endfor
					aod=aod_t
					water=data[n_wvl+3]
					for ii=n_wvl+4, (2*n_wvl)+3 do begin
            if data[ii] ne 'N/A' then triplet_t[ii-n_wvl-4]=float(data[ii]) else triplet_t[ii-n_wvl-4]=!values.f_nan
          endfor
          triplet=triplet_t
          water_err=data[ii+1]
          sza=data[ii+8]
				endif else begin
					day_aod=[day_aod,day_t] & month_aod=[month_aod,month_t] & year_aod=[year_aod,year_t]
					hour_aod=[hour_aod,hour_t] & minute_aod=[minute_aod,min_t] & second_aod=[second_aod,sec_t]
					jul_day_aod=[jul_day_aod,float(data[2])]
					for ii=0, n_wvl-1 do begin
						if data[ii+3] ne 'N/A' then aod_t[ii]=float(data[ii+3]) else aod_t[ii]=!values.f_nan
					endfor
					aod=[[aod],[aod_t]]		
					water=[water,data[n_wvl]]		
					for ii=n_wvl+4, (2*n_wvl)+3 do begin
            if data[ii] ne 'N/A' then triplet_t[ii-n_wvl-4]=float(data[ii]) else triplet_t[ii-n_wvl-4]=!values.f_nan
          endfor
          triplet=[[triplet],[triplet_t]]
          water_err=[water_err,data[ii+1]]
          sza=[sza,data[ii+8]]
				endelse
				number=number+1
			endwhile
		end
		
		'asy' : begin ; asymmetry parameter
			print, 'asymettry parameter file'
			wvl_asy=[441.,675.,869.,1020.]
      n_wvl=n_elements(wvl_asy)
      asy_t=fltarr(n_wvl)
      for t=0, 2 do begin  ; 3 lines of header
        readf, lun, str
      endfor
              
      readf, lun, str ; line of variable labels
      labels_asy=strsplit(str, ',',/extract)
      number=0
      while not eof(lun) do begin
        readf, lun, str ; data informations
        data=strsplit(str, ',', /extract)
        reads, data[0], day_t,month_t,year_t,format='(I2,x,I2,x,I4)'
        reads, data[1], hour_t, min_t, sec_t, format='(I2,x,I2,x,I2)'
        if number eq 0 then begin
          day_asy=day_t & month_asy=month_t & year_asy=year_t
          hour_asy=hour_t & minute_asy=min_t & second_asy=sec_t
          jul_day_asy=float(data[2])
          for ii=0, n_wvl-1 do begin
            if data[ii+3] ne 'N/A' then asy_t[ii]=float(data[ii+3]) else asy_t[ii]=!values.f_nan
          endfor
          asy_TT=asy_t
          for ii=n_wvl+3, (2*n_wvl)+2 do begin
            if data[ii] ne 'N/A' then asy_t[ii-n_wvl-3]=float(data[ii]) else asy_t[ii-n_wvl-3]=!values.f_nan
          endfor
          asy_F=aot_t
          for ii=(2*n_wvl)+3, (3*n_wvl)+2 do begin
            if data[ii] ne 'N/A' then asy_t[ii-(2*n_wvl)-3]=float(data[ii]) else asy_t[ii-(2*n_wvl)-3]=!values.f_nan
          endfor
          asy_C=aot_t
        endif else begin
          day_asy=[day_asy,day_t] & month_asy=[month_asy,month_t] & year_asy=[year_asy,year_t]
          hour_asy=[hour_asy,hour_t] & minute_asy=[minute_asy,min_t] & second_asy=[second_asy,sec_t]
          jul_day_asy=[jul_day_asy,float(data[2])]
          for ii=0, n_wvl-1 do begin
            if data[ii+3] ne 'N/A' then asy_t[ii]=float(data[ii+3]) else asy_t[ii]=!values.f_nan
          endfor
          asy_TT=[[asy_TT],[aot_t]]   
          for ii=n_wvl+3, (2*n_wvl)+2 do begin
            if data[ii] ne 'N/A' then asy_t[ii-n_wvl-3]=float(data[ii]) else asy_t[ii-n_wvl-3]=!values.f_nan
          endfor 
          asy_F=[[asy_F],[aot_t]]   
          for ii=(2*n_wvl)+3, (3*n_wvl)+2 do begin
            if data[ii] ne 'N/A' then asy_t[ii-(2*n_wvl)-3]=float(data[ii]) else asy_t[ii-(2*n_wvl)-3]=!values.f_nan
          endfor 
          asy_C=[[asy_C],[aot_t]]
        endelse
        number=number+1
      endwhile		
		end
  
		'aot2' : begin ; optical thickness
			print, 'aerosol optical extinction file'
			wvl_aot2=[441.,675.,869.,1020.]
      n_wvl=n_elements(wvl_aot2)
      aot_t=fltarr(n_wvl)
      for t=0, 2 do begin  ; 3 lines of header
        readf, lun, str
      endfor
              
      readf, lun, str ; line of variable labels
      labels_aot2=strsplit(str, ',',/extract)
      number=0
      while not eof(lun) do begin
        readf, lun, str ; data informations
        data=strsplit(str, ',', /extract)
        reads, data[0], day_t,month_t,year_t,format='(I2,x,I2,x,I4)'
        reads, data[1], hour_t, min_t, sec_t, format='(I2,x,I2,x,I2)'
        if number eq 0 then begin
          day_aot2=day_t & month_aot2=month_t & year_aot2=year_t
          hour_aot2=hour_t & minute_aot2=min_t & second_aot2=sec_t
          jul_day_aot2=float(data[2])
          for ii=0, n_wvl-1 do begin
            if data[ii+3] ne 'N/A' then aot_t[ii]=float(data[ii+3]) else aot_t[ii]=!values.f_nan
          endfor
          aot2_T=aot_t
          for ii=n_wvl+3, (2*n_wvl)+2 do begin
            if data[ii] ne 'N/A' then aot_t[ii-n_wvl-3]=float(data[ii]) else aot_t[ii-n_wvl-3]=!values.f_nan
          endfor
          aot2_F=aot_t
          for ii=(2*n_wvl)+3, (3*n_wvl)+2 do begin
            if data[ii] ne 'N/A' then aot_t[ii-(2*n_wvl)-3]=float(data[ii]) else aot_t[ii-(2*n_wvl)-3]=!values.f_nan
          endfor
          aot2_C=aot_t
        endif else begin
          day_aot2=[day_aot2,day_t] & month_aot2=[month_aot2,month_t] & year_aot2=[year_aot2,year_t]
          hour_aot2=[hour_aot2,hour_t] & minute_aot2=[minute_aot2,min_t] & second_aot2=[second_aot2,sec_t]
          jul_day_aot2=[jul_day_aot2,float(data[2])]
          for ii=0, n_wvl-1 do begin
            if data[ii+3] ne 'N/A' then aot_t[ii]=float(data[ii+3]) else aot_t[ii]=!values.f_nan
          endfor
          aot2_T=[[aot2_T],[aot_t]]   
          for ii=n_wvl+3, (2*n_wvl)+2 do begin
            if data[ii] ne 'N/A' then aot_t[ii-n_wvl-3]=float(data[ii]) else aot_t[ii-n_wvl-3]=!values.f_nan
          endfor 
          aot2_F=[[aot2_F],[aot_t]]   
          for ii=(2*n_wvl)+3, (3*n_wvl)+2 do begin
            if data[ii] ne 'N/A' then aot_t[ii-(2*n_wvl)-3]=float(data[ii]) else aot_t[ii-(2*n_wvl)-3]=!values.f_nan
          endfor 
          aot2_C=[[aot2_C],[aot_t]]
        endelse
        number=number+1
      endwhile
		end
		
		'ssa' : begin ; Single Scattering Albedo
			print, 'Single Scattering Albedo file'
			wvl_ssa=[441.,675.,869.,1020.]
      n_wvl=n_elements(wvl_ssa)
      ssa_t=fltarr(n_wvl)
      for t=0, 2 do begin  ; 3 lines of header
        readf, lun, str
      endfor
              
      readf, lun, str ; line of variable labels
      labels_ssa=strsplit(str, ',',/extract)
      number=0
      while not eof(lun) do begin
        readf, lun, str ; data informations
        data=strsplit(str, ',', /extract)
        reads, data[0], day_t,month_t,year_t,format='(I2,x,I2,x,I4)'
        reads, data[1], hour_t, min_t, sec_t, format='(I2,x,I2,x,I2)'
        if number eq 0 then begin
          day_ssa=day_t & month_ssa=month_t & year_ssa=year_t
          hour_ssa=hour_t & minute_ssa=min_t & second_ssa=sec_t
          jul_day_ssa=float(data[2])
          for ii=0, n_wvl-1 do begin
            if data[ii+3] ne 'N/A' then ssa_t[ii]=float(data[ii+3]) else ssa_t[ii]=!values.f_nan
          endfor
          ssa=ssa_t
          
          ;albedo
          for ii=32, n_wvl+31 do begin
            if data[ii] ne 'N/A' then ssa_t[ii-32]=float(data[ii]) else ssa_t[ii-32]=!values.f_nan
          endfor
          albedo=ssa_t
          
        endif else begin
          day_ssa=[day_ssa,day_t] & month_ssa=[month_ssa,month_t] & year_ssa=[year_ssa,year_t]
          hour_ssa=[hour_ssa,hour_t] & minute_ssa=[minute_ssa,min_t] & second_ssa=[second_ssa,sec_t]
          jul_day_ssa=[jul_day_ssa,float(data[2])]
          for ii=0, n_wvl-1 do begin
            if data[ii+3] ne 'N/A' then ssa_t[ii]=float(data[ii+3]) else ssa_t[ii]=!values.f_nan
          endfor
          ssa=[[ssa],[ssa_t]]   
          for ii=32, n_wvl+31 do begin
            if data[ii] ne 'N/A' then ssa_t[ii-32]=float(data[ii]) else ssa_t[ii-32]=!values.f_nan
          endfor
          albedo=[[albedo],[ssa_t]] 
          
        endelse
        number=number+1
      endwhile
		end
		
		'flux' : begin ; irradiance
			print, 'flux Irradiance file'
			wvl_flux=[441.,675.,869.,1020.]
      n_wvl=n_elements(wvl_flux)
      flu_t=fltarr(n_wvl)
      for t=0, 2 do begin  ; 3 lines of header
        readf, lun, str
      endfor
              
      readf, lun, str ; line of variable labels
      labels_flux=strsplit(str, ',',/extract)
      number=0
      while not eof(lun) do begin
        readf, lun, str ; data informations
        data=strsplit(str, ',', /extract)
        reads, data[0], day_t,month_t,year_t,format='(I2,x,I2,x,I4)'
        reads, data[1], hour_t, min_t, sec_t, format='(I2,x,I2,x,I2)'
        if number eq 0 then begin
          day_flux=day_t & month_flux=month_t & year_flux=year_t
          hour_flux=hour_t & minute_flux=min_t & second_flux=sec_t
          jul_day_flux=float(data[2])
          for ii=0, n_wvl-1 do begin
            if data[ii+3] ne 'N/A' then flu_t[ii]=float(data[ii+3]) else flu_t[ii]=!values.f_nan
          endfor
          down_flux_T=flu_t
          for ii=n_wvl+3, (2*n_wvl)+2 do begin
            if data[ii] ne 'N/A' then flu_t[ii-n_wvl-3]=float(data[ii]) else flu_t[ii-n_wvl-3]=!values.f_nan
          endfor
          up_flux_T=flu_t
          for ii=(2*n_wvl)+3, (3*n_wvl)+2 do begin
            if data[ii] ne 'N/A' then flu_t[ii-(2*n_wvl)-3]=float(data[ii]) else flu_t[ii-(2*n_wvl)-3]=!values.f_nan
          endfor
          diff_flux_T=flu_t
        endif else begin
          day_flux=[day_flux,day_t] & month_flux=[month_flux,month_t] & year_flux=[year_flux,year_t]
          hour_flux=[hour_flux,hour_t] & minute_flux=[minute_flux,min_t] & second_flux=[second_flux,sec_t]
          jul_day_flux=[jul_day_flux,float(data[2])]
          for ii=0, n_wvl-1 do begin
            if data[ii+3] ne 'N/A' then flu_t[ii]=float(data[ii+3]) else flu_t[ii]=!values.f_nan
          endfor
          down_flux_T=[[down_flux_T],[flu_t]]   
          for ii=n_wvl+3, (2*n_wvl)+2 do begin
            if data[ii] ne 'N/A' then flu_t[ii-n_wvl-3]=float(data[ii]) else flu_t[ii-n_wvl-3]=!values.f_nan
          endfor 
          up_flux_T=[[up_flux_T],[flu_t]]   
          for ii=(2*n_wvl)+3, (3*n_wvl)+2 do begin
            if data[ii] ne 'N/A' then flu_t[ii-(2*n_wvl)-3]=float(data[ii]) else flu_t[ii-(2*n_wvl)-3]=!values.f_nan
          endfor 
          diff_flux_T=[[diff_flux_T],[flu_t]]
        endelse
        number=number+1
      endwhile
		end
		
		'force' : begin ; forcing
			print, 'Forcing file'
      wvl_force=[441.,675.,869.,1020.]
      n_wvl=n_elements(wvl_force)
      forc_t=fltarr(n_wvl)
      for t=0, 2 do begin  ; 3 lines of header
        readf, lun, str
      endfor
              
      readf, lun, str ; line of variable labels
      labels_force=strsplit(str, ',',/extract)
      number=0
      while not eof(lun) do begin
        readf, lun, str ; data informations
        data=strsplit(str, ',', /extract)
        reads, data[0], day_t,month_t,year_t,format='(I2,x,I2,x,I4)'
        reads, data[1], hour_t, min_t, sec_t, format='(I2,x,I2,x,I2)'
        if number eq 0 then begin
          day_force=day_t & month_force=month_t & year_force=year_t
          hour_force=hour_t & minute_force=min_t & second_force=sec_t
          jul_day_force=float(data[2])
          BOA = data[3]             ; bottom of atmosphere altitude
          TOA = data[4]             ; Top of atmosphere altitude
          down_flux_boa = data[5]   ; downwelling flux at bottom of atmosphere
          down_flux_toa = data[6]   ; downwelling flux at top of atmosphere
          up_flux_boa = data[7]
          up_flux_toa = data[8]
          forcing_boa = data[9]     ; radiative forcing at boa
          forcing_toa = data[10]
          efficiency_boa = data[11] ; forcing efficiency at boa
          efficiency_toa = data[12]
        endif else begin
          day_force=[day_force,day_t] & month_force=[month_force,month_t] & year_force=[year_force,year_t]
          hour_force=[hour_force,hour_t] & minute_force=[minute_force,min_t] & second_force=[second_force,sec_t]
          jul_day_force=[jul_day_force,float(data[2])]
          BOA = [BOA,data[3]]             ; bottom of atmosphere altitude
          TOA = [TOA,data[4]]             ; Top of atmosphere altitude
          down_flux_boa = [down_flux_boa,data[5]]   ; downwelling flux at bottom of atmosphere
          down_flux_toa = [down_flux_toa,data[6]]   ; downwelling flux at top of atmosphere
          up_flux_boa = [up_flux_boa,data[7]]
          up_flux_toa = [up_flux_toa,data[8]]
          forcing_boa = [forcing_boa,data[9]]     ; radiative forcing at boa
          forcing_toa = [forcing_toa,data[10]]
          efficiency_boa = [efficiency_boa,data[11]] ; forcing efficiency at boa
          efficiency_toa = [efficiency_toa,data[12]]
        endelse
        number=number+1
      endwhile
      forcing={BOA:BOA,TOA:TOA,down_flux_boa:down_flux_boa,down_flux_toa:down_flux_toa,up_flux_boa:up_flux_boa, $
              up_flux_toa:up_flux_toa,forcing_boa:forcing_boa,forcing_toa:forcing_toa,efficiency_boa:efficiency_boa,efficiency_toa:efficiency_toa}
		end
		
		else: begin
		  print, 'No Reader for type of file: '+type
		end
		
	endcase
	
	free_lun, lun
  endfor
  jul_day=jul_day_aod
  jul_day_almu=jul_day_ssa
  wvl_almu=wvl_ssa
  ;stop
end