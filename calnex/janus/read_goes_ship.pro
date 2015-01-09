;gcc -c LaRC_visst_unpacker.c
;gcc -fPIC -c LaRC_visst_unpacker.c -o visst_unpacker_so.o -I.
;ld -shared -o visst_unpacker.so visst_unpacker_so.o
;
;  testing front-end for read_packed. replace with your own code.
;
PRO read_goes

  ;fn = '/data/seven/schmidt/graham/vintrt.2003222.1900.dat.p'
  ;fn = '/data/seven/schmidt/graham/G12V03.0.TC4.2007198.1515.PX.04K'
  ;fn = '/data/seven/schmidt/calnex/goes/G11V3.0.WUSA.2010134.1345.PX.08K'
  fn = '/data/seven/schmidt/calnex/goes/larc/136/G11V3.0.WUSA.2010136.1945.PX.08K'
  ;fs = '/data/seven/schmidt/calnex/rt/16.out'
  fs = '/data/seven/schmidt/calnex/goes/ship_nav.out'
  ;restore,fs
  ;utcf=utc
  ;latf=lat
  ;lonf=lon
  ;nf      =n_elements(lonf)

  utc0=19.75

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

  ret = read_packed(fn, arr_size, selection, navarr, headarr,val_size, data, lat_lon, comment)
  data = TRANSPOSE(data, [2, 1, 0])


  lat =reform(data[ 0,*,*])
  lon =reform(data[ 1,*,*])-360.
  alb =reform(data[ 7,*,*])
  tau =reform(data[12,*,*])
  ref =reform(data[13,*,*])

  taug=fltarr(nf)
  refg=fltarr(nf)
  for i=0,nf-1 do begin
    if abs(utcf[i] - utc0) lt 0.3 then begin
      mm=min((latf[i]-lat)^2+(lonf[i]-lon)^2,ind)
      if tau[ind] ge 0 and ref[ind] ge 0 then begin
        taug[i]=tau[ind]*10
        refg[i]=ref[ind]*0.1
        print,latf[i],lonf[i],lat[ind],lon[ind],taug[i],refg[i]
      endif
    endif
  endfor
  save,utcf,lonf,latf,taug,refg,file=fs
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

