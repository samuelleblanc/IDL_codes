; Program to find the MODIS L3 pixel that is the closest to the variables given
; looks at one file, builds the lon lat grid then finds the closest

pro find_MODIS_pixel, file, latin, lonin
if n_elements(file) lt 1 then file='/argus/sat/modis/Boulder/MCD43B3.A2012145.h09v04.005.2012166145441.hdf'
if n_elements(latin) lt 1 then latin=40.00791666667
if n_elements(lonin) lt 1 then lonin=-105.26825


gridCount = EOS_GD_InqGrid(file, list)
grid=strsplit(list, ',',/extract)
; Open the file for reading.
    fileID = EOS_GD_Open(file)

thisgrid=grid[0]
gridID = EOS_GD_Attach(fileID, thisGrid)


void = EOS_GD_ProjInfo(gridID, projcode, zonecode, ellipsecode, projparams)
projcodesIndex = [ 0, 1, 4, 6, 7, 9, 11, 20, 22, 24, 99]
        projcodesNames = ['Geographic', 'UTM', 'Lambert Conformal Conic', 'Polar Stereographic', $
            'Polyconic', 'Transverse Mercator', 'Lambert Equal Area', 'Hotine Oblique Mercator', $
            'Space Oblique Mercator', 'Goode Homolosine', 'Intergerized Sinusoidal']
        index = Where(projcodesindex EQ projcode)
        thisProjection = projcodesNames[index]
        IF thisProjection EQ 'Intergerized Sinusoidal' THEN projcode = 31
        IF thisProjection EQ 'Geographic' THEN projcode = 17
        projcode = projcode + 100 ; To conform with Map_Proj_Init codes.
        Print, ''
        Print, '   Projection Information: '
        Print, '       GCTP Projection: ', thisProjection
        Print, '       CGTP Proj Code:  ', projcode
        Print, '       GCTP Zone:       ', zonecode
        Print, '       GCTP Ellipsoid:  ', ellipsecode

if projcode eq 131 then print, 'you have the right sinusoidial grid' else message, 'wrong grid'
	
;now get the dimension of the grid
void = EOS_GD_GridInfo(gridID, xsize, ysize, upleft, lowright)
        Print, ''
        Print, '   Dimensions of Grid: ', xsize, ysize
        Print, '   Upper-left Corner:  ', upleft
        Print, '   Lower-right Corner: ', lowright

fieldCount = EOS_GD_InqFields(gridID, fieldlist, rank, type)
IF fieldCount LE 0 THEN Message, 'No fields found in grid ' + thisGrid
        IF fieldCount GT 1 THEN BEGIN
            field = StrSplit(fieldlist, ',', /Extract)
        ENDIF ELSE field = fieldlist
        Print, ''
        Print, 'The following fields are in grid "' + thisGrid + '": '
        FOR j=0,N_Elements(field)-1 DO Print, j,'   "' + field[j] + '"'

;read, j, 'put in the field to selecct'
j=10
thisField = field[j]
    void = EOS_GD_ReadField(gridID, thisField, fieldValue)
;    fieldValue = Reverse(fieldValue, 2)


    ; Create vectors of latitudes and longitudes.
    xs = Scale_Vector(Findgen(xsize), upleft[0]-500., lowright[0]+500.) ;scale the vectors to represent the center of the pixels
    ys = Scale_Vector(Findgen(ysize), upleft[1]-500., lowright[1]+500.) ;scale the vectors to represente the center of the pixels
; now change the latitudes and longitutdes into proper degrees values
R=projparams[0]
lat=ys/r *180./!PI
lon=(xs/(r*cos(ys/r)))*180./!PI

dist=fltarr(ysize,xsize)
for i=0,xsize-1 do begin
  for j=0, ysize-1 do begin
    dist[j,i]=map_2points(lonin,latin,lon[i],lat[j],/meters)
  endfor
endfor
dx=min(dist,in)
print, 'minimum distance at:',dx,' m'
n=array_indices(dist,in)
print, n
stop
end
