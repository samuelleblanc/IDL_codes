;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                      Function read_skywatch                           ;
;                         by Samuel LeBlanc                             ;
;                                                                       ;
;Used to read in the data found at skywatch.colorado.edu                ;
;takes raw data files, and puts them into a useable IDL structure       ;
;Current instruments include: Pyranometer                               ;
;                             Pyrgeometer                               ;
;                             Ceilometer                                ;
;                             Disdrometer                               ;
;                             Micro Rain Radar                          ;
;                             Sunphotometer                             ;
;                             Ozone Analyzer                            ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;Note: In order to function properly, the file being read must be in    ;
;      the same directory than read_skywatch.pro or put the             ;
;      read_skywatch.pro into the IDL path.                             ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;Usage: In your code in order to call read_skywatch you must put this   ;
;       syntax. You must include the instrument and name statements     ;
;                                                                       ;
;     data = read_skywatch(instrument='inst',name='\somepath\file.dat') ;
;                                                                       ;
;    - data is the variable name you are assigning to this file         ;
;    - inst is the short name of the instrument                         ;
;              where : 'pyra' is for the Pyranometer                    ;
;                      'pyrg' is for the Pyrgeometer                    ;
;                      'ceil' is for the Ceilometer                     ;
;                      'disd' is for the Disdrometer                    ;
;                      'mrr_avg' is for the micro rain radar averaged   ;
;                      'mrr_inst' is for the micro rain radar instant   ;
;                      'sun' is for the sunphotometer                   ;
;                      'mix' is for the Mixing layer height             ;
;                      'ozone' is for the Ozone Analyzer                ;
;    - \somepath\file.dat is the full path to the file                  ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;Data structure and information:                                        ; 
;       The returning object of this function will be a structure.      ;
;       The different instruments will yield a different structure      ;
;       depending on what the instrument is measuring.                  ;
;                                                                       ;
;       In order to call a specific variable in the structure you must  ;
;       use this syntax: name_of_strucuture.variable[index]             ;
;                                                                       ; 
;       for example: a = data.irradiance[1890]                          ;
;                                                                       ;
;       Below is a list of the different structures for each instrument ;
;       and information about the variables                             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                        General time values                            ;
;                        **ALL TIMES IN UTC**                           ;
;                                                                       ;
;Values of time is always calculated from local midnight                ; 
;     - hour: UTC hour of the measurement                               ;
;     - minutes: minutes past the hour of the measurement               ;
;     - seconds: seconds past the minute of the measurement             ;
;     - sec_of_day: seconds past local midnight of the measurement      ; 
;     - day: day of the month                                           ;
;     - month: month of the year                                        ;
;     - year: year                                                      ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                            Pyrgeometer                                ;
;                                                                       ;
;     - irradiance: measure of the incoming irradiance in the IR range  ;
;                   units (W/m^2)                                       ;
;     - voltage: raw voltage for the irradiance measurement             ;
;                calibration value is located in the data file          ;
;                units (V)                                              ;
;     - temperature: temperature measurement of the instrument          ;
;                    units (Kelvin)                                     ;
;     - temp_voltage: raw voltage measurement for the temperature       ;
;                     units (V)                                         ;  
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                                       
;                                                                       ;
;                            Pyranometer                                ; 
;                                                                       ;
;     - irradiance: measure of the incoming irradiance in the visible   ;
;                   range                                               ;
;                   units (W/m^2)                                       ;
;     - voltage: raw voltage measurement for the irradiance             ;
;                calibration value in the data file                     ;                                                                                          
;                units (V)                                              ;                                
;                                                                       ;                               
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                               
;                                                                       ;
;                            Disdrometer                                ;
;                                                                       ;
;     - count: amount of particles that has passed in that interval     ;
;              when it is 0.0, all the other values are set to 0.0      ;
;              unit (#)                                                 ;
;     - error: communication error indicator (1.0 for an error)         ;   
;              (0.0 for normal operation), unitless                     ;
;     - intensity: rain intensity measurement calculated by the parsivel;
;                  unit (mm/h)                                          ;                                            
;     - reflectivity: radar reflectivity calculated by the parsivel     ;                                                                                               
;                     unit (dBz)                                        ;
;     - total_rain: measure of the accumulated rain from start          ;
;                   unit (mm)                                           ;
;     - vis: horizontal visibility calculated by the parsivel           ;
;            unit (m)                                                   ;
;     - weather_code: the SYNOP weather code indicating current weather ;
;                     unitless                                          ;
;     - sensor_temperature: the temperature at the sensor               ;
;                           used for qualitiy control of the instrument ;
;                           unit (°Celsius)                             ;
;     - error_code: parsivel internal error code, see parsivel info     ;
;                   unitless                                            ;
;     - distribution: this is a 32 X 32 matrix that represents          ;                                                                                                    
;                     the binned distribution of the particles          ;
;                     (velocity vs. diameter)                           ;
;                     the following lines describe the mean diameter in ;
;                     mm and mean velocity in m/s equivalent of each bin;
;                     between 1 and 32                                  ;
;                                                                       ;
;diameter = [0.062,0.187,0.312,0.437,0.562,0.687,0.812,0.937,1.062$     ;
;  ,1.187,1.375,1.625,1.875,2.125,2.375,2.75,3.25,3.75,4.25,4.75,5.5$   ;
;  ,6.5,7.5,8.5,9.5,11.0,13.0,15.0,17.0,19.0,21.5,24.5]                 ;                                                       
;                                                                       ; 
;velocity = [0.05,0.15,0.25,0.35,0.45,0.55,0.65,0.75,0.85,0.95,1.1,1.3$ ;
;  ,1.5,1.7,1.9,2.2,2.6,3.0,3.4,3.8,4.4,5.2,6.0,6.8,7.6,8.8,10.4,12.0$  ;
;  ,13.6,15.2,17.6,20.8]                                                ;                                                                                                    
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                             Ceilometer                                ;
;                                                                       ;
;     - cloud_base1: This is the lowest measured cloud base             ;
;                    if none are measured it will indicate 0.0          ;
;                    unit(m)                                            ;
;     - cloud_base2: This is the second lowest measured cloud base      ;
;                    if none are measured it will indicate 0.0          ;
;                    unit(m)                                            ;
;     - cloud_base3: This is the highest measured cloud base            ;
;                    if none are measured it will indicate 0.0          ;
;                    unit(m)                                            ;
;     - reflectivity: This is an array of 770 numbers representing the  ;
;                     reflectivity from the specific height bin.        ;
;                     each height bin is 10m.                           ;
;                     unit (100*srad*km)^-1                             ;                                                                                    
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                         Micro Rain Radar (averaged)                   ;
;                                                                       ;
;     - UTC: hours between local time and UTC                           ;
;            unit (hours)                                               ; 
;     - avg: amount of seconds in each average                          ;
;            unit (seconds)                                             ;
;     - resolution: constant distance between each height bin           ;
;                   unit (m)                                            ;
;     - asl: height above sea level of the instrument                   ;
;            unit (m)                                                   ;
;     - sample_rate: frequency of the sampling rate                     ;
;                    unit (Hz)                                          ;
;     - noise_0: noise level 0 settings                                 ;
;                unitless                                               ;
;     - noise_1: noise level 1 settings                                 ;
;               unitless                                                ;
;     - version_service: Version number of the MRR service program      ;
;                        retrieval program loaded on the computer       ;
;                        unitless                                       ;
;     - version_firmware: Version number of the MRR firmware            ;
;                         loaded on the actual instrument               ;
;                         unitless                                      ;                                                      
;     - serial: Serial Number of the MRR                                ;
;               unitless                                                ;
;     - calibration: Calibration constant for the calculations of       ;
;                    Rain Rate                                          ;
;                    unitless                                           ;
;     - valid_spectra: Percentage of the spectra that is valid          ;
;                      unit (%)                                         ;
;     - height: Array of the differing height values for each bin       ;
;               unit (m)                                                ;
;     - Transfer_function: array of values that corresponds to a        ;
;                          transfer value for each height bin           ;
;                          raw data is divided by this value            ;
;                          unitless                                     ;
;     - Frequency: Matrix of FFT spectra with 64 spectral bins and 31   ;
;                 Height bins. it is spectral Reflectivities            ;
;                 unit (dB)                                             ;
;     - Diameter: Matrix of the center of the diameter of the drop sizes;
;                 of an same volume sphere drop. for each height bin    ;
;                 unit (mm)                                             ;
;     - Number: Matrix of Spectral Drop Densities, N(D), number of drops;
;               corresponding to the size bin (Diameter) used for       ;
;               calculations of the fall velocity for each height       ;
;               unit (m^-3 mm^-1)                                       ;
;     - Path_attenuation: two way Path integrated attenuation,          ;
;                         unit (dB)                                     ;
;     - Reflectivity_attenuated: The radar Reflectivity without         ;
;                                attenuation corrections of each height ;
;                                unit (dBZ)                             ;
;     - Reflectivity: Radar reflectivity of each height                 ;
;                     unit (dBZ)                                        ;
;     - Rain_rate: Rain rate for each height bin                        ;
;                  unit (mm/h)                                          ;
;     - Liquid_water_content: Liquid water content for each height bin  ;
;                             can be negative, for no statistical bias  ;
;                             unit (g/m^3)                              ;
;     - Fall_velocity: Characteristic fall velocity of the height bin   ;
;                      From the first moment of the doppler velocity    ;
;                      each velocity bin is 0.1905 m/s wide             ;
;                      unit (m/s)                                       ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                         Micro Rain Radar (instant)                    ;
;                                                                       ;
;     - UTC: hours between local time and UTC                           ;
;            unit (hours)                                               ; 
;     - valid_spectra: Percentage of the spectra that is valid          ;
;                      unit (%)                                         ;
;     - height: Array of the differing height values for each bin       ;
;               unit (m)                                                ;
;     - Transfer_function: array of values that corresponds to a        ;
;                          transfer value for each height bin           ;
;                          raw data is divided by this value            ;
;                          unitless                                     ;
;     - Frequency: Matrix of FFT spectra with 64 spectral bins and 31   ;
;                 Height bins. it is spectral Reflectivities            ;
;                 unit (dB)                                             ;
;     - Diameter: Matrix of the center of the diameter of the drop sizes;
;                 of an same volume sphere drop. for each height bin    ;
;                 unit (mm)                                             ;
;     - Number: Matrix of Spectral Drop Densities, N(D), number of drops;
;               corresponding to the size bin (Diameter) used for       ;
;               calculations of the fall velocity for each height       ;
;               unit (m^-3 mm^-1)                                       ;
;     - Path_attenuation: two way Path integrated attenuation,          ;
;                         unit (dB)                                     ;
;     - Reflectivity_attenuated: The radar Reflectivity without         ;
;                                attenuation corrections of each height ;
;                                unit (dBZ)                             ;
;     - Reflectivity: Radar reflectivity of each height                 ;
;                     unit (dBZ)                                        ;
;     - Rain_rate: Rain rate for each height bin                        ;
;                  unit (mm/h)                                          ;
;     - Liquid_water_content: Liquid water content for each height bin  ;
;                             can be negative, for no statistical bias  ;
;                             unit (g/m^3)                              ;
;     - Fall_velocity: Characteristic fall velocity of the height bin   ;
;                      From the first moment of the doppler velocity    ;
;                      each velocity bin is 0.1905 m/s wide             ;
;                      unit (m/s)                                       ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                            Sunphotometer                              ;
;                                                                       ;
;     - spectrum: the spectrum values for each wavelength and timestamp ;
;		              unit (counts)						                              ;
;     - wavelength: values of wavelength bins			                    	;
;		                unit (nm)				                                 		;
;     - exposure: array of exposure times			                        	;
;		              unit (ms)					                                   	;
;     - avg: number of points averaged	                        				;
;	           unitless					                                       		;
;     - temperature: Instrument temperature at given time	            	;
;		                 unit (°C)				                               		;
;     - chn_str: string of channel names of the instrument	          	;
;		             unitless				                                     		;
;						                                                      			;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                        Mixing Layer Height                            ;
;                                                                       ;
;     - mlh_1: The first mixing layer height                            ;
;              unit (m)                                                 ;
;     - mlh_2: The second mixing layer height                           ;
;              unit(m)                                                  ;
;     - mlh_3: The third mixing layer height                            ;
;              unit(m)                                                  ;
;                                                                       ;                 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                                Ozone                                  ;
;                                                                       ;
;     - ozone: The measured ozone concentration                         ;
;              unit (ppb)                                               ;
;     - voltage: The measured ozone concentration raw voltage           ;
;                unit(V)                                                ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                                Tau                                    ;
;                                                                       ;
;     - tau: Matrix of retrieved aerosol optical depth,                 ;
;            not cloud screened                                         ;
;            unitless                                                   ;
;     - wavelength: wavelength at which each optical depth is reported  ;
;                   unit(nm)                                            ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function read_skywatch, instrument=instrument, name=name

case instrument of
'pyrg': begin ;for the pyrgeometer
; get the date for the file
  openr, lun, name, /get_lun
  on_ioerror, io
  
  line= " "
  readf, lun, line
  readf, lun, line
  year=0
  month=0
  day=0
  reads, line, year, month, day, format='(6x,I4,1x,I2,1x,I2)'
  close, lun
  Free_lun, lun
  
;read the whole data
  data=read_ascii(name,comment_symbol=';')
  irr=data.field1[1,*] ;irradiance
  irrvolt=data.field1[2,*] ;voltage
  temp=data.field1[3,*] ;temp
  tempvolt=data.field1[4,*] ;temp voltage
  
;read in timestamp
  time=read_ascii(name,comment_symbol=';',delimiter=':') ;time in UTC
  hour = time.field1[0,*]
  minutes = time.field1[1,*]
  seconds = time.field1[2,*]

;get seconds of the day value
  u = size(hour, /dimensions)
  minsec = minutes * 60.0
  hoursec = reform(hour)
  for i=1,u[1]-1 do begin
    if (hoursec[i-1] gt hoursec[i]) then hoursec[i] = hour[i] + 24.0
  endfor
  hoursec = hoursec *3600.0

  allseconds = seconds + minsec + hoursec
  
  results={irradiance:irr,voltage:irrvolt,temperature:temp,temp_voltage:tempvolt,hour:hour,minutes:minutes,seconds:seconds,sec_of_day:allseconds,day:day,month:month,year:year}
end

'pyra': begin ;for the pyranometer
; get the date for the file
  openr, lun, name, /get_lun
  on_ioerror, io
  
  line= " "
  readf, lun, line
  readf, lun, line
  year=0
  month=0
  day=0
  reads, line, year, month, day, format='(6x,I4,1x,I2,1x,I2)'
  close, lun
  Free_lun, lun
;read the whole data
  data=read_ascii(name,comment_symbol=';')
  irr=data.field1[1,*] ;irradiance
  irrvolt=data.field1[2,*] ;voltage

;read in timestamp
  time=read_ascii(name,comment_symbol=';',delimiter=':') ;time in UTC
  hour = time.field1[0,*]
  minutes = time.field1[1,*]
  seconds = time.field1[2,*]
  
;get seconds of the day value
  u = size(hour, /dimensions)
  minsec = minutes * 60.0
  hoursec = reform(hour)
  for i=1,u[1]-1 do begin
    if (hoursec[i-1] gt hoursec[i]) then hoursec[i] = hour[i] + 24.0
  endfor
  hoursec = hoursec *3600.0

  allseconds = seconds + minsec + hoursec
  
  results={irradiance:irr,voltage:irrvolt,hour:hour,minutes:minutes,seconds:seconds,sec_of_day:allseconds,day:day,month:month,year:year}
end

'ceil': begin ;for the ceilometer
; get the date for the file
  openr, lun, name, /get_lun
  on_ioerror, io
  
  line= " "
  readf, lun, line
  readf, lun, line
  year=0
  month=0
  day=0
  reads, line, year, month, day, format='(6x,I4,1x,I2,1x,I2)'
  close, lun
  Free_lun, lun
  
;read the whole data
  data=read_ascii(name,comment_symbol=';')
  base1=data.field001[1,*] ;cloud base one
  base2=data.field001[2,*] ;cloud base two
  base3=data.field001[3,*] ;cloud base three
  reflectivity=data.field001[4:773,*] ;reflectivity at all the different heights
  
;read in timestamp
  time=read_ascii(name,comment_symbol=';',delimiter=':') ;time in UTC
  hour = time.field1[0,*]
  minutes = time.field1[1,*]
  seconds = time.field1[2,*]
  
; get teh seconds of the day value
  u = size(hour, /dimensions)
  minsec = minutes * 60.0
  hoursec = reform(hour)
  for i=1,u[1]-1 do begin
    if (hoursec[i-1] gt hoursec[i]) then hoursec[i] = hour[i] + 24.0
  endfor
  hoursec = hoursec *3600.0

  allseconds = seconds + minsec + hoursec
  
  results={cloud_base1:base1,cloud_base2:base2,cloud_base3:base3,reflectivity:reflectivity,hour:hour,minutes:minutes,seconds:seconds,sec_of_day:allseconds,day:day,month:month,year:year}
end

'disd': begin ;for the disdrometer
; get the date for the file
  openr, lun, name, /get_lun
  on_ioerror, io
  
  line= " "
  readf, lun, line
  readf, lun, line
  year=0
  month=0
  day=0
  reads, line, year, month, day, format='(6x,I4,1x,I2,1x,I2)'
  close, lun
  Free_lun, lun
  
;setup for reading in all the values 
  stemplate={COMMENTSYMBOL:';',DATASTART:0L,DELIMITER:9b,FIELDCOUNT:[1034L],FIELDGROUPS:replicate(0,1034),FIELDLOCATIONS:replicate(0L,1034),FIELDNAMES:replicate('FIELD1',1034),FIELDTYPES:replicate(4L,1034),MISSINGVALUE:!VALUES.F_NAN,VERSION:1.0}
  data=read_ascii(name,comment_symbol=';',template=stemplate)
  amount=data.field1[1,*] ;particle number
  a = size(amount, /dimensions)
  err = replicate(!values.f_nan, a[1])
  intensity = replicate(!values.f_nan, a[1])
  reflectivity = replicate(!values.f_nan, a[1])
  accumulated_rain = replicate(!values.f_nan, a[1])
  visibility = replicate(!values.f_nan, a[1])
  SYNOP_WW = replicate(0, a[1])
  sensor_temp = replicate(!values.f_nan, a[1])
  Error_code = replicate(0, a[1])
  Diam_vel = replicate(!values.f_nan, [1024,a[1]])
  temp = replicate(!values.f_nan, [32,32])
  diameter = replicate(!values.f_nan,[a[1],32,32])

;get the time stamp for this line
  time=read_ascii(name,comment_symbol=';',delimiter=':') ;time in UTC
  hour = time.field1[0,*]
  minutes = time.field1[1,*]
  seconds = time.field1[2,*]
  
  for i=0,a[1]-1 do begin
    if (amount[i] eq -1.0) then begin 
      err[i]=1.0
    endif else begin 
      if(amount[i] gt 0.0) then begin
        intensity[i]=data.field1[2,i]
        reflectivity[i]=data.field1[3,i]
        accumulated_rain[i]=data.field1[4,i]
        visibility[i]=data.field1[5,i]
        SYNOP_WW[i]=data.field1[6,i]
        if (julday(month,day,year,hour[i],minutes[i],seconds[i]) gt julday(10,26,2009,17,38,03)) then begin
          sensor_temp[i]=data.field1[7,i]
          Error_code[i]=data.field1[8,i]
          Diam_vel[*,i]=data.field1[9:1032,i]
        endif else begin
          sensor_temp[i]=!values.f_nan
          Error_code[i]=data.field1[7,i]
          Diam_vel[*,i]=data.field1[8:1031,i]
        endelse
      endif
    endelse
    t=0
    if (julday(month, day, year, hour[i],minutes[i],seconds[i]) gt julday(04,22,2010,20,09,18)) then begin
      for u=0,31 do begin
        temp[*,u]=Diam_vel[32*u:32*(u+1)-1,i]
      endfor
    endif else begin
      for u=0,31 do begin
        temp[u,*]=Diam_vel[32*u:32*(u+1)-1,i]
      endfor
    endelse
    diameter[i,*,*]=temp
    ;stop
  endfor
; get the correct seconds of the day values   
  u = size(hour, /dimensions)
  minsec = minutes * 60.0
  hoursec = reform(hour)
  for i=1,u[1]-1 do begin
    if (hoursec[i-1] gt hoursec[i]) then hoursec[i] = hour[i] + 24.0
  endfor
  hoursec = hoursec *3600.0

  allseconds = seconds + minsec + hoursec
  
  results={count:amount,error:err,intensity:intensity,reflectivity:reflectivity,total_rain:accumulated_rain,$
          vis:visibility,weather_code:SYNOP_ww,sensor_temperature:Sensor_temp,error_code:error_code,$
          distribution:diameter,day:day,month:month,year:year,hour:hour,minutes:minutes,seconds:seconds,$
          sec_of_day:allseconds}
end

'mrr_avg': begin ;for the averaged Micro Rain Radar
  openr, lun, name, /get_lun
  on_ioerror, io
  
  year=00
  date=00
  month=00
  hour=00
  minute=00
  seconds=00
  UTC=00
  avg=00
  resolution=00
  asl=00
  sample_rate=00e1
  Noise_0=0.0
  Noise_1=0.0
  Version_service=0.0
  Version_firmware=0.0
  serial=0
  calibration=0
  valid_spectra=0 ;percentage of valid spectra
  height=replicate(0,31) ;meters above radar
  Transfer_function=replicate(!values.f_nan,31)
  Path_attenuation=replicate(!values.f_nan,31)
  Reflectivity_attenuated=replicate(!values.f_nan,31)
  Reflectivity=replicate(!values.f_nan,31)
  Rain_rate=replicate(!values.f_nan, 31)
  Liquid_water_content=replicate(!values.f_nan,31)
  Fall_velocity=replicate(!values.f_nan,31)
  Frequency=replicate(!values.f_nan,[64,31])
  Diameter=replicate(!values.f_nan,[64,31])
  Number=replicate(!values.f_nan,[64,31])
  nans=replicate(!values.f_nan,31)
  counter=0
  first=0
  dummy=replicate(!values.F_NAN,[64,31])
  dummyd=replicate(!values.F_NAN,[64,31])
  dummyn=replicate(!values.F_NAN,[64,31])
  while not EOF(lun) do begin
    line= " "
    readf, lun, line
    indicator = ""
    reads, line, indicator, format='(A1)'
    ;;start a case sequence to get the proper formatting of the line
    case indicator of
    'M': begin
         reads, line, y,m,d,h,mm,s,U,ave,stp,seal,smp,nf0,nf1,svs,dvs,dsn,cc,mdq, format='(4x,I2,I2,I2,I2,I2,I2,4x,I3,4x,I6,4x,I6,4x,I6,4x,E6,4x,F6,4x,F6,4x,F5,4x,F5,4x,I11,3x,I7,4x,I3)'
         if (first eq 0) then begin
          year=y & month=m & date=d & hour=h & minute=mm & seconds=s & UTC=U
          avg=ave & resolution=stp & asl=seal & sample_rate=smp
          Noise_0=nf0 & Noise_1=nf1
          Version_service=svs & Version_firmware=dvs & serial=dsn
          calibration = cc & valid_spectra=mdq          
         endif else begin
          year=[year,y] & month=[month,m] & date=[date,d] & hour=[hour,h] & minute=[minute,mm] & seconds=[seconds,s] & UTC=[UTC,U]
          avg=[avg,ave] & resolution=[resolution,stp] & asl=[asl,seal] & sample_rate=[sample_rate,smp]
          Noise_0=[Noise_0,nf0] & Noise_1=[Noise_1,nf1]
          Version_service=[Version_service,svs] & Version_firmware=[Version_firmware,dvs] & serial=[serial,dsn]
          calibration=[calibration,cc] & valid_spectra=[valid_spectra,mdq] 
         endelse
         end
    'H': begin
         he=fltarr(31)
         reads, line, he, format='(3x,I7,I7,I7,I7,I7,I7,I7,I7,I7,I7,I7,I7,I7,I7,I7,I7,I7,I7,I7,I7,I7,I7,I7,I7,I7,I7,I7,I7,I7,I7,I7)'
         if (first eq 0) then begin
          height = he
         endif else begin
          height = [[height],[he]]
         endelse       
         end
    'T': begin
         trans=fltarr(31)
         reads, line, trans, format='(3x,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7)'
         if (first eq 0) then begin
          Transfer_function = trans
         endif else begin
          Transfer_function = [[Transfer_function],[trans]]       
         endelse         
         end
    'F': begin
         u=0
         reads, line, u, format='(1x,I2)'
         num=strlen(line)
         num=fix((num-3.)/7.)
         if (num gt 0) then freq=replicate(!values.f_nan,num) else freq=!values.f_nan
         if (num gt 0) then reads, line, freq, format='(3x,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7)'
         nan_replace=where(freq eq 0.0,count)
         if (count gt 0) then freq[nan_replace]=!values.F_NAN
         if (num le 30) then freq=[freq,nans[num:30]]
         if (num eq 0) then freq=nans
         if (first eq 0) then begin
          Frequency[u,*] = transpose(freq)
         endif else begin
          dummy[u,*]=freq       
         endelse 
         end
    'D': begin
         reads, line, u, format='(1x,I2)'
         num=strlen(line)
         num=fix((num-3.)/7.)
         if (num gt 0) then diam=replicate(!values.f_nan,num) else diam=!values.f_nan
         if (num gt 0) then reads, line, diam, format='(3x,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7)'
         nan_replace=where(diam eq 0.0,count)
         if (count gt 0) then diam[nan_replace]=!values.F_NAN
         if (num le 30) then diam=[diam,nans[num:30]]
         if (num eq 0) then diam=nans
         if (first eq 0) then begin
          Diameter[u,*] = diam
         endif else begin
          dummyd[u,*] = diam       
         endelse 
         end
    'N': begin
         reads, line, u, format='(1x,I2)'
         num=strlen(line)
         num=fix((num-3.)/7.)
         if (num gt 0) then nums=replicate(!values.f_nan,num) else nums=!values.f_nan
         if (num gt 0) then reads, line, nums, format='(3x,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7)'
         nan_replace=where(nums eq 0.0,count)
         if (count gt 0) then nums[nan_replace]=!values.F_NAN
         if (num le 30) then nums=[nums,nans[num:30]]
         if (num eq 0) then nums=nans
         if (first eq 0) then begin
          Number[u,*] = nums
         endif else begin
          dummyn[u,*] = nums       
         endelse 
         end
    'P': begin
         num=strlen(line)
         num=fix((num-3.)/7.)
         if (num gt 0) then path=replicate(!values.f_nan,num) else path=!values.f_nan
         if (num gt 0) then reads, line, path, format='(3x,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7)'
         if (num le 30) then path=[path,nans[num:30]]
         if (num eq 0) then path=nans
         if (first eq 0) then begin
          Path_attenuation = path
         endif else begin
          Path_attenuation = [[Path_attenuation],[path]]       
         endelse         
         end
    'z': begin
         num=strlen(line)
         num=fix((num-3.)/7.)
         if (num gt 0) then a_reflect=replicate(!values.f_nan,num) else a_reflect=!values.f_nan
         if (num gt 0) then reads, line, a_reflect, format='(3x,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7)'
         if (num le 30) then a_reflect=[a_reflect,nans[num:30]]
         if (num eq 0) then a_reflect=nans
         if (first eq 0) then begin
          Reflectivity_attenuated = a_reflect
         endif else begin
          Reflectivity_attenuated = [[Reflectivity_attenuated],[a_reflect]]       
         endelse         
         end
    'Z': begin
         num=strlen(line)
         num=fix((num-3.)/7.)
         if (num gt 0) then reflect=replicate(!values.f_nan,num) else reflect=!values.f_nan
         if (num gt 0) then reads, line, reflect, format='(3x,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7)'
         if (num le 30) then reflect=[reflect,nans[num:30]]
         if (num eq 0) then reflect=nans
         if (first eq 0) then begin
          Reflectivity = reflect
         endif else begin
          Reflectivity = [[Reflectivity],[reflect]]       
         endelse         
         end
    'R': begin
         num=strlen(line)
         num=fix((num-3.)/7.)
         if (num gt 0) then rate=replicate(!values.f_nan,num) else rate=!values.f_nan
         if (num gt 0) then reads, line, rate, format='(3x,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7)'
         if (num le 30) then rate=[rate,nans[num:30]]
         if (num eq 0) then rate=nans
         if (first eq 0) then begin
          Rain_rate = rate
         endif else begin
          Rain_rate = [[Rain_rate],[rate]]       
         endelse         
         end
    'L': begin
         num=strlen(line)
         num=fix((num-3.)/7.)
         if (num gt 0) then lwc=replicate(!values.f_nan,num) else lwc=!values.f_nan
         if (num gt 0) then reads, line, lwc, format='(3x,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7)'
         if (num le 30) then lwc=[lwc,nans[num:30]]
         if (num eq 0) then lwc=nans
         if (first eq 0) then begin
          Liquid_water_content = lwc
         endif else begin
          Liquid_water_content = [[Liquid_water_content],[lwc]]       
         endelse         
         end
    'W': begin
         num=strlen(line)
         num=fix((num-3.)/7.)
         if (num gt 0) then velo=replicate(!values.f_nan,num) else velo=!values.f_nan
         if (num gt 0) then reads, line, velo, format='(3x,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7)'
         if (num le 30) then velo=[velo,nans[num:30]]
         if (num eq 0) then velo=nans
         if (first eq 0) then begin
          Fall_velocity = velo
         endif else begin
          Fall_velocity = [[Fall_velocity],[velo]]       
         endelse     
         ;readf, lun,scrap   
         if (first ne 0) then Frequency = [[[Frequency]],[[dummy]]]
         if (first ne 0) then Diameter=[[[Diameter]],[[dummyd]]]
         if (first ne 0) then Number=[[[Number]],[[dummyn]]]
         first=first+1 
         dummy=replicate(!values.F_NAN,[64,31])
         dummyd=replicate(!values.F_NAN,[64,31])
         dummyn=replicate(!values.F_NAN,[64,31])         
         end
    else: begin
          ;print,'error in reading next line'
          ;readf, lun,scrap
          end
    endcase                             
                             
  counter=counter+1
  endwhile
  close, lun
  Free_lun, lun
  
  u = size(hour, /dimensions)
  minsec = minute * 60.0
  hoursec = reform(hour)
  for i=1,(u[0]-1) do begin
    if (hoursec[i-1] gt hoursec[i]) then hoursec[i] = hour[i] + 24.0
  endfor
  hoursec = hoursec *3600.0
  allseconds = seconds + minsec + hoursec
  
  results={year:year,month:month,day:date,hour:hour,minutes:minute,seconds:seconds,sec_of_day:allseconds,$
           UTC:UTC,avg:avg,resolution:resolution,asl:asl,sample_rate:sample_rate,noise_0:Noise_0,noise_1:Noise_1,$
           version_service:Version_service,version_firmware:Version_firmware,serial:serial,calibration:calibration,$
           valid_spectra:valid_spectra,height:height,Transfer_function:Transfer_function,Frequency:Frequency,Diameter:Diameter,$
           Number:Number,Path_attenuation:Path_attenuation,Reflectivity_attenuated:Reflectivity_attenuated,Reflectivity:Reflectivity,$
           Rain_rate:Rain_rate,Liquid_water_content:Liquid_water_content,Fall_velocity:Fall_velocity}
end 

'mrr_inst': begin ;for the instant Micro Rain Radar    
  openr, lun, name, /get_lun
  on_ioerror, io
  
  year=00
  date=00
  month=00
  hour=00
  minute=00
  seconds=00
  UTC=00
  valid_spectra=0 ;percentage of valid spectra
  height=replicate(0,31) ;meters above radar
  Transfer_function=replicate(!values.f_nan,31)
  Path_attenuation=replicate(!values.f_nan,31)
  Reflectivity_attenuated=replicate(!values.f_nan,31)
  Reflectivity=replicate(!values.f_nan,31)
  Rain_rate=replicate(!values.f_nan, 31)
  Liquid_water_content=replicate(!values.f_nan,31)
  Fall_velocity=replicate(!values.f_nan,31)
  Frequency=replicate(!values.f_nan,[64,31])
  Diameter=replicate(!values.f_nan,[64,31])
  Number=replicate(!values.f_nan,[64,31])
  nans=replicate(!values.f_nan,31)
  counter=0
  first=0
  dummy=replicate(!values.F_NAN,[64,31])
  dummyd=replicate(!values.F_NAN,[64,31])
  dummyn=replicate(!values.F_NAN,[64,31])
  while not EOF(lun) do begin
    line= " "
    readf, lun, line
    indicator = ""
    reads, line, indicator, format='(A1)'
    ;;start a case sequence to get the proper formatting of the line
    case indicator of
    'M': begin ;MRR 091103000007 UTC-07 MDQ 100
         reads, line, y,m,d,h,mm,s,U,mdq, format='(4x,I2,I2,I2,I2,I2,I2,4x,I3,4x,I6,5x,I4)'
         if (first eq 0) then begin
          year=y & month=m & date=d & hour=h & minute=mm & seconds=s & UTC=U
          valid_spectra=mdq          
         endif else begin
          year=[year,y] & month=[month,m] & date=[date,d] & hour=[hour,h] & minute=[minute,mm] & seconds=[seconds,s] & UTC=[UTC,U]
          valid_spectra=[valid_spectra,mdq] 
         endelse
         end
    'H': begin
         he=fltarr(31)
         reads, line, he, format='(3x,I7,I7,I7,I7,I7,I7,I7,I7,I7,I7,I7,I7,I7,I7,I7,I7,I7,I7,I7,I7,I7,I7,I7,I7,I7,I7,I7,I7,I7,I7,I7)'
         if (first eq 0) then begin
          height = he
         endif else begin
          height = [[height],[he]]
         endelse       
         end
    'T': begin
         trans=fltarr(31)
         reads, line, trans, format='(3x,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7)'
         if (first eq 0) then begin
          Transfer_function = trans
         endif else begin
          Transfer_function = [[Transfer_function],[trans]]       
         endelse         
         end
    'F': begin
         u=0
         reads, line, u, format='(1x,I2)'
         num=strlen(line)
         num=fix((num-3.)/7.)
         if (num gt 0) then freq=replicate(!values.f_nan,num) else freq=!values.f_nan
         if (num gt 0) then reads, line, freq, format='(3x,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7)'
         nan_replace=where(freq eq 0.0,count)
         if (count gt 0) then freq[nan_replace]=!values.F_NAN
         if (num le 30) then freq=[freq,nans[num:30]]
         if (num eq 0) then freq=nans
         if (first eq 0) then begin
          Frequency[u,*] = transpose(freq)
         endif else begin
          dummy[u,*]=freq       
         endelse 
         end
    'D': begin
         reads, line, u, format='(1x,I2)'
         num=strlen(line)
         num=fix((num-3.)/7.)
         if (num gt 0) then diam=replicate(!values.f_nan,num) else diam=!values.f_nan
         if (num gt 0) then reads, line, diam, format='(3x,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7)'
         nan_replace=where(diam eq 0.0,count)
         if (count gt 0) then diam[nan_replace]=!values.F_NAN
         if (num le 30) then diam=[diam,nans[num:30]]
         if (num eq 0) then diam=nans
         if (first eq 0) then begin
          Diameter[u,*] = diam
         endif else begin
          dummyd[u,*] = diam       
         endelse 
         end
    'N': begin
         reads, line, u, format='(1x,I2)'
         num=strlen(line)
         num=fix((num-3.)/7.)
         if (num gt 0) then nums=replicate(!values.f_nan,num) else nums=!values.f_nan
         if (num gt 0) then reads, line, nums, format='(3x,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7)'
         nan_replace=where(nums eq 0.0,count)
         if (count gt 0) then nums[nan_replace]=!values.F_NAN
         if (num le 30) then nums=[nums,nans[num:30]]
         if (num eq 0) then nums=nans
         if (first eq 0) then begin
          Number[u,*] = nums
         endif else begin
          dummyn[u,*] = nums       
         endelse 
         end
    'P': begin
         num=strlen(line)
         num=fix((num-3.)/7.)
         if (num gt 0) then path=replicate(!values.f_nan,num) else path=!values.f_nan
         if (num gt 0) then reads, line, path, format='(3x,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7)'
         if (num le 30) then path=[path,nans[num:30]]
         if (num eq 0) then path=nans
         if (first eq 0) then begin
          Path_attenuation = path
         endif else begin
          Path_attenuation = [[Path_attenuation],[path]]       
         endelse         
         end
    'z': begin
         num=strlen(line)
         num=fix((num-3.)/7.)
         if (num gt 0) then a_reflect=replicate(!values.f_nan,num) else a_reflect=!values.f_nan
         if (num gt 0) then reads, line, a_reflect, format='(3x,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7)'
         if (num le 30) then a_reflect=[a_reflect,nans[num:30]]
         if (num eq 0) then a_reflect=nans
         if (first eq 0) then begin
          Reflectivity_attenuated = a_reflect
         endif else begin
          Reflectivity_attenuated = [[Reflectivity_attenuated],[a_reflect]]       
         endelse         
         end
    'Z': begin
         num=strlen(line)
         num=fix((num-3.)/7.)
         if (num gt 0) then reflect=replicate(!values.f_nan,num) else reflect=!values.f_nan
         if (num gt 0) then reads, line, reflect, format='(3x,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7)'
         if (num le 30) then reflect=[reflect,nans[num:30]]
         if (num eq 0) then reflect=nans
         if (first eq 0) then begin
          Reflectivity = reflect
         endif else begin
          Reflectivity = [[Reflectivity],[reflect]]       
         endelse         
         end
    'R': begin
         num=strlen(line)
         num=fix((num-3.)/7.)
         if (num gt 0) then rate=replicate(!values.f_nan,num) else rate=!values.f_nan
         if (num gt 0) then reads, line, rate, format='(3x,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7)'
         if (num le 30) then rate=[rate,nans[num:30]]
         if (num eq 0) then rate=nans
         if (first eq 0) then begin
          Rain_rate = rate
         endif else begin
          Rain_rate = [[Rain_rate],[rate]]       
         endelse         
         end
    'L': begin
         num=strlen(line)
         num=fix((num-3.)/7.)
         if (num gt 0) then lwc=replicate(!values.f_nan,num) else lwc=!values.f_nan
         if (num gt 0) then reads, line, lwc, format='(3x,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7)'
         if (num le 30) then lwc=[lwc,nans[num:30]]
         if (num eq 0) then lwc=nans
         if (first eq 0) then begin
          Liquid_water_content = lwc
         endif else begin
          Liquid_water_content = [[Liquid_water_content],[lwc]]       
         endelse         
         end
    'W': begin
         num=strlen(line)
         num=fix((num-3.)/7.)
         if (num gt 0) then velo=replicate(!values.f_nan,num) else velo=!values.f_nan
         if (num gt 0) then reads, line, velo, format='(3x,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7,F7)'
         if (num le 30) then velo=[velo,nans[num:30]]
         if (num eq 0) then velo=nans
         if (first eq 0) then begin
          Fall_velocity = velo
         endif else begin
          Fall_velocity = [[Fall_velocity],[velo]]       
         endelse     
         ;readf, lun,scrap   
         if (first ne 0) then Frequency = [[[Frequency]],[[dummy]]]
         if (first ne 0) then Diameter=[[[Diameter]],[[dummyd]]]
         if (first ne 0) then Number=[[[Number]],[[dummyn]]]
         first=first+1 
         dummy=replicate(!values.F_NAN,[64,31])
         dummyd=replicate(!values.F_NAN,[64,31])
         dummyn=replicate(!values.F_NAN,[64,31])         
         end
    else: begin
          ;print,'error in reading next line'
          ;readf, lun,scrap
          end
    endcase                             
                             
  counter=counter+1
  endwhile
  close, lun
  Free_lun, lun
  
  u = size(hour, /dimensions)
  minsec = minute * 60.0
  hoursec = reform(hour)
  for i=1,(u[0]-1) do begin
    if (hoursec[i-1] gt hoursec[i]) then hoursec[i] = hour[i] + 24.0
  endfor
  hoursec = hoursec *3600.0
  allseconds = seconds + minsec + hoursec
  
  results={year:year,month:month,day:date,hour:hour,minutes:minute,seconds:seconds,sec_of_day:allseconds,$
           UTC:UTC,valid_spectra:valid_spectra,$
           height:height,Transfer_function:Transfer_function,Frequency:Frequency,Diameter:Diameter,$
           Number:Number,Path_attenuation:Path_attenuation,Reflectivity_attenuated:Reflectivity_attenuated,Reflectivity:Reflectivity,$
           Rain_rate:Rain_rate,Liquid_water_content:Liquid_water_content,Fall_velocity:Fall_velocity}
end

'sun': begin ; for the sunphotometer
  print, '** WARNING **'
  print, 'Times are NOT in UTC'
  openr, lun, name, /get_lun
  on_ioerror, io
  line=" "

  ; get number of data points
  readf, lun, line
  length=strlen(line)

  ; read in date data
  readf, lun, line
  date_str=strsplit(line, ',',/extract)
  year=intarr(length)
  month=intarr(length)
  day=intarr(length)
  if (date_str[0] ne 'date') then begin 
    print, 'Error reading date string'
    goto, crap
  endif
  for i=1, length do begin
    ymd=strsplit(date_str[i], '/',/extract)
	  year[i-1]=fix(ymd[0])
    month[i-1]=fix(ymd[1])
    day[i-1]=fix(ymd[2])
  endfor

  ; read in time data
  readf, lun, line
  time_str=strsplit(line, ',',/extract)
  hour=intarr(length)
  minutes=intarr(length)
  seconds=intarr(length)
  if (time_str[0] ne 'time') then begin
	  print, 'Error reading time string' & goto, crap
  endif
  for i=1, length do begin
	  hour[i-1]=fix(strmid(time_str[i],0,strpos(time_str[i],':')))
    minutes[i-1]=fix(strmid(time_str[i],strpos(time_str[i],':')+1,2))
    seconds[i-1]=fix(strmid(time_str[i],strpos(time_str[i],':')+4,2))
  endfor

  ; read in Exposure time data
  readf, lun, line
  expo_str=strsplit(line, ',',/extract)
  exposure=intarr(length)
  if (expo_str[0] ne 'Exposure time') then begin
	  print, 'Error reading Exposure Time' & goto, crap
  endif
  for i=1, length do begin
	  exposure[i-1]=fix(expo_str[i])
  endfor

  ; read in averaging number
  readf, lun, line
  avg_str=strsplit(line, ',',/extract)
  avg=intarr(length)
  if (avg_str[0] ne 'Number of times of an average') then begin
	  print, 'Error reading Averaging time' & goto, crap
  endif
  for i=1, length do begin
	  avg[i-1]=fix(avg_str[i])
  endfor

  ; read in Temperature
  readf, lun, line
  temp_str=strsplit(line, ',',/extract)
  temperature=fltarr(length)
  if (temp_str[0] ne 'Temperature') then begin
	  print, ' Error reading Temperature data' & goto, crap
  endif
  for i=1, length do begin
	  temperature[i-1]=float(temp_str[i])
  endfor

  ; verify length of dataset
  readf, lun, line
  length_ver=strlen(line)
  if (length_ver ne length) then begin
	  print, 'Error in File Length' & goto, crap
  endif

  ; read in channel data
  readf, lun, line
  chn_str=strsplit(line, ',',/extract)
  if (chn_str[0] ne 'Wavelength') then begin
	  print, 'Error in Channel read' & goto, crap
  endif

  ; read in spectrum data
  wavelength=fltarr(1024)
  spectrum=fltarr(1024,length)
  for j=0, 1023 do begin
	  readf, lun, line
    spec_str=strsplit(line, ',',/extract)
    wavelength[j]=spec_str[0]
    for i=1, length do begin
      spectrum[j,i-1]=float(spec_str[i])
    endfor
  endfor

  ; verify length of dataset
  readf, lun, line
  length_ver2=strlen(line)
  if (length_ver2 ne length) then begin
	  print, 'Error in ending File Length' & goto, crap
  endif

  close, lun
  free_lun, lun

  ; get seconds of the day value
  u = size(hour, /dimensions)
  minsec = minutes * 60.0
  hoursec = reform(hour)
  for i=1,u[0]-1 do begin
    if (hoursec[i-1] gt hoursec[i]) then hoursec[i] = hour[i] + 24.0
  endfor
  hoursec = hoursec *3600.0
  allseconds = seconds + minsec + hoursec
  
  results={spectrum:spectrum,wavelength:wavelength,exposure:exposure,avg:avg,temperature:temperature,chn_str:chn_str[1:*],$
  hour:hour,minutes:minutes,seconds:seconds,sec_of_day:allseconds,day:day,month:month,year:year}
  goto, ending
  
  crap:
  close, lun
  free_lun, lun
  results=!values.f_nan
  ending:
end

'mix':begin

  ;read in the date data
  time=read_ascii(name,delimiter='/',comment_symbol=';')
  month=time.field1[1,*]
  day=time.field1[0,*]
  year=time.field1[2,*]

  ;read in the time data
  openr, lun, name, /get_lun
  line=" "
  hour=intarr(n_elements(month))
  minute=intarr(n_elements(month))
  seconds=fltarr(n_elements(month))
  for i=0, n_elements(month)-1 do begin
    readf, lun, line
    reads, line, h,m,format='(11X,I2,1X,I2)'
    hour[i]=h
    minute[i]=m
  endfor
  close, lun
  free_lun, lun
  
  ;read in the actual data
  data=read_ascii(name, comment_symbol=';')
  mlh_1=data.field1[2,*]
  mlh_2=data.field1[3,*]
  mlh_3=data.field1[4,*]
  
  ;take out bad data
  q=where(mlh_1 eq 0.0, count)
  if (count gt 0) then mlh_1[q]=!values.f_nan
  q=where(mlh_2 eq 0.0, count)
  if (count gt 0) then mlh_2[q]=!values.f_nan
  q=where(mlh_3 eq 0.0, count)
  if (count gt 0) then mlh_3[q]=!values.f_nan

  ;for seconds of day data
  u = size(hour, /dimensions)
  minsec = minute * 60.0
  hoursec = reform(hour)
  for i=1,(u[0]-1) do begin
    if (hoursec[i-1] gt hoursec[i]) then hoursec[i] = hour[i] + 24.0
  endfor
  hoursec = hoursec *3600.0
  allseconds = seconds + minsec + hoursec

  ;return good data
  results={year:year,month:month,day:day,hour:hour,minutes:minute,seconds:seconds,sec_of_day:allseconds,$
          mlh_1:mlh_1,mlh_2:mlh_2,mlh_3:mlh_3}
end

'ozone':begin
; get the date for the file
  openr, lun, name, /get_lun
  on_ioerror, io
  
  line= " "
  readf, lun, line
  readf, lun, line
  year=0
  month=0
  day=0
  reads, line, year, month, day, format='(6x,I4,1x,I2,1x,I2)'
  close, lun
  Free_lun, lun
;read the whole data
  data=read_ascii(name,comment_symbol=';')
  irr=data.field1[1,*] ;irradiance
  irrvolt=data.field1[2,*] ;voltage

;read in timestamp
  time=read_ascii(name,comment_symbol=';',delimiter=':') ;time in UTC
  hour = time.field1[0,*]
  minutes = time.field1[1,*]
  seconds = time.field1[2,*]
  
;get seconds of the day value
  u = size(hour, /dimensions)
  minsec = minutes * 60.0
  hoursec = reform(hour)
  for i=1,u[1]-1 do begin
    if (hoursec[i-1] gt hoursec[i]) then hoursec[i] = hour[i] + 24.0
  endfor
  hoursec = hoursec *3600.0

  allseconds = seconds + minsec + hoursec
  
  results={ozone:irr,voltage:irrvolt,hour:hour,minutes:minutes,seconds:seconds,sec_of_day:allseconds,day:day,month:month,year:year}
end

'tau':begin
; get the date for the file
  openr, lun, name, /get_lun
  ;on_ioerror, io
  
  line= " "
  readf, lun, line
  readf, lun, line
  year=0
  month=0
  day=0
  arrr=fltarr(1025)
  reads, line, year, month, day, format='(7x,I4,1x,I2,1x,I2)'
  readf, lun, line
  readf, lun, line
  readf, lun, line
  readf, lun, line
  reads, strmid(line,1), arrr
  wavelength=arrr[1:1024] 
  close, lun
  Free_lun, lun

;read the whole data
  data=read_ascii(name,comment_symbol=';')
  tau=data.field0001[1:*,*] ;irradiance

;read in timestamp
  time=read_ascii(name,comment_symbol=';',delimiter=':') ;time in UTC
  hour = time.field1[0,*]
  minutes = time.field1[1,*]
  seconds = time.field1[2,*]
  
;get seconds of the day value
  u = size(hour, /dimensions)
  minsec = minutes * 60.0
  hoursec = reform(hour)
  for i=1,u[1]-1 do begin
    if (hoursec[i-1] gt hoursec[i]) then hoursec[i] = hour[i] + 24.0
  endfor
  hoursec = hoursec *3600.0

  allseconds = seconds + minsec + hoursec
  
  results={tau:tau,wavelength:wavelength,hour:hour,minutes:minutes,seconds:seconds,sec_of_day:allseconds,day:day,month:month,year:year}
end




else: begin 
  print,'Instrument not available'
  results = !values.f_nan
end
endcase
;stop
goto, ends
io:
print, '*************I/O error, check file name****************'
stop
results = !values.f_nan
goto, ends
ends:
return, results
end
