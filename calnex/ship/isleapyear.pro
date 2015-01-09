;+
; Determine whether a year is a leap year in the gregorian calendar.
; Leap years are those years 
;  divisible by 4 and (!(divisible by 100) or (divisible by 400)).
; eg. (1900 is not a leap year, 2000 is).
;
; This code came from 
; <a href="http://www.naic.edu/~phil/">Phil Perillat</a> at Arecibo.
; Local changes:
; <UL>
; <LI> modify this documentation for use by idldoc.
; </UL>
; 
; @param  year {in}{required}{type=long integer} 4 digit year
; @returns istat: int  0 if not a leap year, 1 if  a leap year.
;
; @version $Id: isleapyear.pro,v 1.1 2004/11/30 15:42:58 bgarwood Exp $
;-
function isleapyear,year
; 
    if (year mod 4  ) ne 0 then return,0
    if (year mod 100) ne 0 then return,1
    if (year mod 400) eq 0 then return,1
    return,0
end
