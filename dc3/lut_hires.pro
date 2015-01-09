; program to create hires lut by interpolating between values of tau and ref
; keep the same sza values

pro lut_hires

restore, '/home/leblanc/DC3_SEAC4RS/library/CLD_LUT.out'

refs_hi=findgen(30)
taus_hi=[findgen(20),findgen(20)*2.+20.,findgen(10)*10.+60.]
nrhi=30
nthi=50






stop
end
