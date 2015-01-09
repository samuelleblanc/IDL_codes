;program to get all the desired weather data from the rooftop


pro get_wx

url='http://foehn.colorado.edu/weather/atoc1/' ;wxobs20121001.txt

doy=findgen(153)+2456048.5
caldat,doy,month,day,year

for i=0, n_elements(doy)-1 do begin

  path='/home/leblanc/SSFR3/wx/'+string(year[i],format='(I04)')+string(month[i],format='(I02)')+'/'
  file='wxobs'+string(year[i],format='(I04)')+string(month[i],format='(I02)')+string(day[i],format='(I02)')+'.txt'
  spawn, 'wget '+url+file+' '+path
endfor

stop
end
