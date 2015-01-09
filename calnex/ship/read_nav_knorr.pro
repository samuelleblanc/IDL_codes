
pro read_nav_knorr,pattern,utc,lat,lon

files=file_search(pattern,count=nf,/FOLD_CASE)


str=''

for f=0,nf-1 do begin
  openu,uu,files[f],/get_lun
  print,'Open:',files[f]
  gnarr=file_lines(files[f])
 for yy=0, gnarr-1 do begin
  start:
  readf, uu, str
  num=floor(strlen(str)/54)
   if num eq 0 then begin
	if yy eq gnarr-1 then begin
	  break
	endif else begin	
	yy=yy+1	
	 goto, start
	endelse
   endif
  lon=replicate(!values.f_nan,num)
  lat=replicate(!values.f_nan,num)
  heading=replicate(!values.f_nan,num)
  pitch=replicate(!values.f_nan,num)
  roll=replicate(!values.f_nan,num)
  hour1=replicate(!values.f_nan,num)
  minute1=replicate(!values.f_nan,num)
  sec1=replicate(!values.f_nan,num)

  str_arr=strarr(num)
  index=0
  for ii=0, num-1 do begin
	first=strpos(str,':',index)
	last=strpos(str,':',strpos(str,':',strpos(str,':',index)+1)+1)
	str_arr[ii]=strmid(str,index,last-first)
	dat=strsplit(str_arr[ii],',',/extract)
	if n_elements(dat) ne 6 then goto, crap
	lon[ii]=float(dat[2]) ;longitude
        lat[ii]=float(dat[1]) ;lattitude
        heading[ii]=float(dat[3]) ;heading information
        pitch[ii]=float(dat[4]) ;pitch
        roll[ii]=float(dat[5]) ;roll

	reads, dat[0], h,m,s, format='(I2,x,I2,x,I2)'
        hour1[ii]=h
	minute1[ii]=m
	sec1[ii]=s
	crap:
	index=last-2
	
  endfor

  if (f eq 0 and yy eq 0) then begin
	lonl=lon
	latl=lat
	headingl=heading
	pitchl=pitch
	rolll=roll
	hour=hour1
	minute=minute1
	sec=sec1
  endif else begin
	lonl=[lonl,lon]
	latl=[latl,lat]
	headingl=[headingl,heading]
	pitchl=[pitchl,pitch]
	rolll=[rolll,roll]
	hour=[hour,hour1]
	minute=[minute,minute1]
	sec=[sec,sec1]
  endelse

endfor
free_lun, uu
endfor
utc1=hour+minute/60.+sec/3600.
lat=latl
lon=lonl
utc=utc1
stop
end
