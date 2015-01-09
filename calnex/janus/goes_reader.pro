;gcc -c LaRC_visst_unpacker.c
;gcc -fPIC -c LaRC_visst_unpacker.c -o visst_unpacker_so.o -I.
;ld -shared -o visst_unpacker.so visst_unpacker_so.o
;
;  testing front-end for read_packed. replace with your own code.
;
;
; GOES - LARC reader function 
;

pro goes_reader, filein, lat,lon,alb,tau,ref,utc

  hr = strmid(filein,10,2, /reverse_offset)
  mn = strmid(filein,8,2, /reverse_offset)
  utc= float(hr)+ float(mn)/60.
  
  fn=filein

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
  fn = BYTE([BYTE(fn), 0])

  ret = read_packed(fn, arr_size, selection, navarr, headarr,val_size, data, lat_lon, comment)
  data = TRANSPOSE(data, [2, 1, 0])

  lat =90.-reform(data[ 0,*,*])
  lon =reform(data[ 1,*,*])-360.
  alb =reform(data[ 7,*,*])
  tau =reform(data[12,*,*])
  ref =reform(data[13,*,*])

END


;
;  function to open, read, and extract the array sizes of packed data
;
FUNCTION get_arr_size, fn
  b = BYTARR(400)
  OPENR, lun, fn, /GET_LUN, ERROR=err
  IF (err NE 0) THEN BEGIN
    MESSAGE,'Call to OPENR in get_arr_size failed:'+string(err)
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

