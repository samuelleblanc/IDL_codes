pro rph2za,pitch,roll,heading,zenith,azimuth

; IDL subroutine by Paul Ricchiazzi, 25 Feb 97 paul@icess.ucsb.edu
; all angles in degrees
; pitch  : positive values indicate nose up
; roll   : positive values indicate right wing down
; azimuth: positive values clockwise, w.r.t. NORTH
;roll=roll*0.
dtor=!dtor

; assume aircraft heading north: rotate around roll
uz=cos(roll*dtor)*cos(pitch*dtor)
ux=sin(roll*dtor)
uy=-cos(roll*dtor)*sin(pitch*dtor)

; now rotate to correct heading
vz=uz
vx=ux*cos(heading*dtor)+uy*sin(heading*dtor)
vy=uy*cos(heading*dtor)-ux*sin(heading*dtor)

zenith=acos(vz)/dtor
azimuth=atan(vx,vy)/dtor

return
end
