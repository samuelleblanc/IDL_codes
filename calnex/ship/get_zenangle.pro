function get_zenangle,date,tmhrs,lat,lon

year = fix(strmid(date,0,4))
month = fix(strmid(date,4,2))
day = fix(strmid(date,6,2))

doy = dmtodayno(day,month,year)

zensun,doy,tmhrs,lat,lon,zenangles
return,zenangles

end
