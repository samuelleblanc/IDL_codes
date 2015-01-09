;gcc -c LaRC_visst_unpacker.c
;gcc -fPIC -c LaRC_visst_unpacker.c -o visst_unpacker_so.o -I.
;ld -shared -o visst_unpacker.so visst_unpacker_so.o
;
;  testing front-end for read_packed. replace with your own code.
;
PRO read_larc

  jul0=136

  restore,'/data/seven/schmidt/calnex/goes/ship_nav.out'
  utc_ship=utc
  lat_ship=lat
  lon_ship=lon
  ns      =n_elements(lon_ship)
  tau_larc=fltarr(ns)
  ref_larc=fltarr(ns)

  files=file_search('/data/seven/schmidt/calnex/goes/larc/136/G11V3*PX.08K',count=cf)
  ctr=0
  utc=fltarr(cf)
  flt=strarr(cf)
  for i=0,cf-1 do begin
    pos=strpos(files[i],'WUSA.2010')
    dat=strmid(files[i],pos+9)
    jul=strmid(dat,0,3)
    hh =strmid(dat,4,2)
    mm =strmid(dat,6,2)
    ;print,jul,hh,':',mm
    if fix(jul) eq jul0 then begin
      utc[ctr]=float(hh)+float(mm)/60.
      flt[ctr]=files[i]
      ctr=ctr+1
    endif
  endfor
  utc=utc[0:ctr-1]
  flt=flt[0:ctr-1]
  ss=sort(utc)
  utc=utc[ss]
  flt=flt[ss]


  ;fn = '/data/seven/schmidt/graham/vintrt.2003222.1900.dat.p'
  ;fn = '/data/seven/schmidt/graham/G12V03.0.TC4.2007198.1515.PX.04K'
  ;fn = '/data/seven/schmidt/calnex/goes/G11V3.0.WUSA.2010134.1345.PX.08K'
  ;fn = '/data/seven/schmidt/calnex/goes/G11V3.0.WUSA.2010136.1945.PX.08K'
  ;fs = '/data/seven/schmidt/calnex/rt/16.out'

  ; START HERE

  for cc=0,ctr-1 do begin
  fn=flt[cc]
  print,'Process '+fn

  selection = LONARR(256)
  FOR i=0, 21 DO BEGIN
    selection[i] = 1
  ENDFOR
  FOR i=22, 255 DO BEGIN
    selection[i] = 0
  ENDFOR
  navarr = LONARR(640)
  headarr = LONARR(64)
  arr_size = get_arr_size(fn)
  IF (arr_size[0] EQ -1) THEN BEGIN
    RETURN
  END
  data = FLTARR(arr_size[2], arr_size[1], arr_size[0])
  val_size = LONARR(3)
  lat_lon = FLTARR(6)
  comment = BYTARR(400)
  bfn = BYTE([BYTE(fn), 0])

  ret = read_packed(bfn, arr_size, selection, navarr, headarr,val_size, data, lat_lon, comment)
  data = TRANSPOSE(data, [2, 1, 0])


  lat =90.-reform(data[ 0,*,*])
  lon =reform(data[ 1,*,*])-360.
  alb =reform(data[ 7,*,*])
  tau =reform(data[12,*,*])
  ref =reform(data[13,*,*])

  ;ship=where(utc_ship gt utc[cc]-0.1 and utc_ship lt utc[cc]+0.1,nf)
  mm=min(abs(utc_ship-utc[cc]),ship)
  for iship=0,0 do begin ;nf-1 do begin
    print,'Match UTC=',utc_ship[ship[iship]]
    mm=min((lat_ship[ship[iship]]-lat)^2+(lon_ship[ship[iship]]-lon)^2,ind)
    ref_larc[ship[iship]]=ref[ind]
    tau_larc[ship[iship]]=tau[ind]
    print,utc_ship[ship[iship]],utc[cc],lat_ship[ship[iship]],lat[ind],tau_larc[ship[iship]]
  endfor

endfor
save,file='/data/seven/schmidt/calnex/goes/ship_larc.out',utc_ship,lat_ship,lon_ship,tau_larc,ref_larc
  STOP
END





;
;  function to open, read, and extract the array sizes of packed data
;
FUNCTION get_arr_size, fn
  b = BYTARR(400)
  OPENR, lun, fn, /GET_LUN, ERROR=err
  IF (err NE 0) THEN BEGIN
    MESSAGE,'Call to OPENR in get_arr_size failed:', err
    return, [-1, -1, -1]
  ENDIF
  READU, lun, b
  s = STRING(b)
  r = STRSPLIT(s, ' ', /EXTRACT)
  x = LONG(r[3])
  y = LONG(r[4])
  z = LONG(r[5])
  RETURN, [x, y, z]
END

;
;  calls the C routine, unpackcp_idl
;  returns the data to the caller
;
FUNCTION read_packed, fn, arr_size, selection, navarr, headarr, $
                      val_size, data, lat_lon, comment
  IF (CALL_EXTERNAL('/data/seven/schmidt/graham/visst_unpacker.so', 'unpackcp_idl', $
                    fn, arr_size, selection, navarr, headarr, $
                    val_size, data, lat_lon, comment) EQ 0) THEN BEGIN
    RETURN, 0
  ENDIF ELSE BEGIN
    MESSAGE,'External call to read_packed failed!'
    RETURN, -1
  ENDELSE

END

