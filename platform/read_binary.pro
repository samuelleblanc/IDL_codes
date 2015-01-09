;+
; NAME:
;   read_binary
;
; PURPOSE:
;   Program to read in the binary data from the limit_detect.vi
;
; CATEGORY:
;   Levelling Platform, binary data, limit switches
;
; CALLING SEQUENCE:
;   data=read_binary(file)
;   - file: filepath of the file to read in
;   - data: variable to hold the structure information
;
; OUTPUT:
;   structure of all the following variables
;   - time: timestamp of the measurement (in idl julian day format)
;   - year: year integer
;   - month: month integer
;   - day: day integer
;   - hour: hour timestamp integer
;   - minute: minute timestamp integer
;   - second: second timestamp
;   - hrs: hours since midnight
;   - pos: position of motor
;   - vel: velocity of motor
;   - req_pos: requested position of the motor (can be either relative or absolute)
;   - mini_ins_pitch: pitch measured by the mini_ins
;   - mini_ins_roll: roll measured by the mini_ins (these may not be inline with the aircraft pitch and roll)
;   - temp_stage: temperature of the stage
;   - temp_lid: temperature of the lid
;   - temp_motor1: temperature of motor 1
;   - temp_motor2: temperature of motor 2
;   - current_motor1: current that is sent to the motor 1 controller
;   - current_motor2: current that is sent to the motor 2 controller
;   - RH: Relative humidity inside the enclosure
;   - supply_volt: the supply voltage to the entire system  
;
; KEYWORDS:
;   none
;
; DEPENDENCIES:
;   none
;   
; NEEDED FILES:
;   none
;
; EXAMPLE:
;   data=read_binary('/home/leblanc/Platform/file/binary_test_5')
;   print, data.hrs
;   > 1.456667
;   > 1.456668
;   ...
;
; MODIFICATION HISTORY:
; Written:  Samuel LeBlanc, LASP CU Boulder, March 13th, 2012, Ottawa, Canada
; Modified: 
;---------------------------------------------------------------------------

function read_binary, file
f=file_search(file, count=m)
if m lt 1 then message, 'No file Found'

openu, lun, f[0],/get_lun
data={v:dblarr(14)}

bignum=600000L
time=dblarr(bignum)
pos=dblarr(bignum)
vel=dblarr(bignum)
req_pos=dblarr(bignum)
mini_ins_pitch=dblarr(bignum)
mini_ins_roll=dblarr(bignum)
temp_stage=dblarr(bignum)
temp_lid=dblarr(bignum)
temp_motor1=dblarr(bignum)
temp_motor2=dblarr(bignum)
current_motor1=dblarr(bignum)
current_motor2=dblarr(bignum)
RH=dblarr(bignum)
supply_volt=dblarr(bignum)

i=0L
while not eof(lun) do begin
  readu, lun, data
  time[i]=data.v[0] & pos[i]=data.v[1] & vel[i]=data.v[2] & req_pos[i]=data.v[3]
  mini_ins_pitch[i]=data.v[4] & mini_ins_roll[i]=data.v[5] & temp_stage[i]=data.v[6]
  temp_lid[i]=data.v[7] & temp_motor1[i]=data.v[8] & temp_motor2[i]=data.v[9]
  current_motor1[i]=data.v[10] & current_motor2[i]=data.v[11] & RH[i]=data.v[12]
  supply_volt[i]=data.v[13]
  i=i+1L
endwhile
close, lun

time=time[0:i-1]/3600./24.+julday(1,1,1904,0,0,0) & pos=pos[0:i-1] & vel=vel[0:i-1] & req_pos=req_pos[0:i-1]
mini_ins_pitch=mini_ins_pitch[0:i-1] & mini_ins_roll=mini_ins_roll[0:i-1]
temp_stage=temp_stage[0:i-1] & temp_lid=temp_lid[0:i-1]
temp_motor1=temp_motor1[0:i-1] & temp_motor2=temp_motor2[0:i-1]
current_motor1=current_motor1[0:i-1] & current_motor2=current_motor2[0:i-1]
RH=RH[0:i-1] & supply_volt=supply_volt[0:i-1]

caldat,time,month,day,year,hour,minute,second
hrs=hour+minute/60.+second/3600.

dat={time:time,month:month,day:day,year:year,hour:hour,minute:minute,second:second,hrs:hrs,$
     pos:pos,vel:vel,req_pos:req_pos,mini_ins_pitch:mini_ins_pitch,mini_ins_roll:mini_ins_roll,$
     temp_stage:temp_stage,temp_lid:temp_lid,temp_motor1:temp_motor1,temp_motor2:temp_motor2,$
     current_motor1:current_motor1,current_motor2:current_motor2,RH:RH,supply_volt:supply_volt}

return, dat
end



