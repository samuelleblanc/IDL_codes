      PROGRAM MM
C
C      INTRISIC GETARG
      IMPLICIT   NONE
      INTEGER I
      REAL*8      WAVELENGTH, RAD1(0:1000), RAD2
      CHARACTER*64 wvl,rr,h
      call getarg(1, wvl)
      call getarg(2, rr)
      
      DO I=3, IARGC()
        call getarg(i, h)
        write (*,*) h
        read (h,*) rad1(i-2)
      end do
	  
C           Input the parameters
      WRITE (*,'(1X,A)') 'Wavelength'
      write (*,'(A)') wvl
      write (*,*) rr
      write (*,*) 'number of arguments'
      write (*,*) IARGC()
      write (*,*) rad1(0:IARGC()-2)
	  end
