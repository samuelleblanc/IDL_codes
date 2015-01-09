; program to get the precipitable water vapor content 
; from the GPS sounding, interpolated to the specific times and date required

pro get_precip_water, tmhrs, date, water

pw=read_ascii('/home/leblanc/SSFR3/wx/DSRCnrt_2012.plt')

doys=julday(float(strmid(date,4,2)),float(strmid(date,6,2)),float(strmid(date,0,4)))-julday(01,00,2012)+tmhrs/24.0

water=fltarr(n_elements(tmhrs))

water=interpol(pw.field01[1,*],pw.field01[0,*],doys)

end
