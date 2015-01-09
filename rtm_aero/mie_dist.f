      PROGRAM MIE_DIST
C
C        MIE_DIST calculates the extinction and scattering coefficients
C      and the single scattering albedo for a specific distribution of 
C      spherical particles using Lorenz-Mie scattering.  The Legendre 
C      coefficients for the four unique phase matrix elements and the 
C      phase function values at gaussian quadrature angles are also output.
C       Particle size distribution comes from CalNex AEROSDtotal. This is to 
C      to be used with IDL, as inputs of the wavelength, index of refraction, 
C      and size distribution.
C        Outputs asymmetry parameter, single scattering albedo, and
C      extinction coefficient.
C        If the wavelength, effective radius, and radius range are
C      input in microns and the number concentration of particles in cm^-3,
C      then the volume extinction coefficient is output in km^-1.
C        The complex index of refraction is specified with a negative 
C      imaginary part.
C
      IMPLICIT   NONE
      INTEGER     MAXN, SD_size
      PARAMETER   (MAXN=10000)
      PARAMETER   (SD_size=138)
      INTEGER     NLEGEN, NANGLE, NUMRAD, I
      REAL*8      WAVELENGTH, RAD1, RAD2, input, total
      REAL*8      EXTINCTION, SSALBEDO, ASY, tm1, tm2
      REAL*8      PHASECOEF11(0:MAXN), PHASECOEF12(0:MAXN)
      REAL*8      PHASECOEF33(0:MAXN), PHASECOEF34(0:MAXN)
      REAL*8   distribution(0:SD_size),Diam(0:SD_size),deldia(0:SD_size)
      REAL*8      COSANGLE(MAXN)
      REAL*8      PHASEFUNC11(MAXN), PHASEFUNC12(MAXN)
      REAL*8      PHASEFUNC33(MAXN), PHASEFUNC34(MAXN)
      COMPLEX*16  MSPHERE
      CHARACTER*64 SCATFILE, PHASEFILE,temp

C     Get all the input: scatfile, phasefile, wavelength, msphere, distribution
      input = IARGC()
      IF (input.lt.143) WRITE (*,*) '** NOT ENOUGH INPUT ARGUMENTS **'
	  
      call getarg(1,SCATFILE)
      call getarg(2,PHASEFILE)
      call getarg(3,temp)
      read (temp,*) wavelength
      call getarg(4,temp)
      read (temp,*) tm1
      call getarg(5,temp)
      read (temp,*) tm2
      MSPHERE=COMPLEX(tm1,tm2)
	  
      do I=6, input
        call getarg(I,temp)
        read (temp,*) distribution(I-5)		
      end do
C      WRITE (*,'(139(3X,F8.5))') distribution
C     total=sum(distribution)
C      distribution=distribution / total
C      WRITE (*,*) 'total:', total
C      WRITE (*,'(139(2X,F7.5))') distribution

      RAD1=0.005012  /2.
      RAD2=6.71151   /2.
      NUMRAD=SD_size-1
      Diam=(/0.005012,0.00562356,0.00631,0.00707994,0.007943,0.00891219,
     . 0.01,0.0112202,0.01259,0.0141262,0.01585,0.017784,0.01996,
     . 0.0223955,0.02512,0.0281851,0.03162,0.0354782,0.0398100,
     . 0.0446676,0.05012,0.0562356,0.0631,0.0707994,0.07943,0.0825853,
     . 0.0849251,0.0873379,0.089822,0.0923762,0.0949987,0.0976881,
     . 0.100443,0.103261,0.106142,0.109084,0.112085,0.115145,0.118261,
     . 0.121434,0.124662,0.127945,0.131281,0.134672,0.138115,0.141613,
     . 0.145165,0.148772,0.152434,0.156154,0.159932,0.163771,0.167673,
     . 0.171640,0.175676,0.179783,0.183968,0.188232,0.192582,0.197022,
     . 0.201559,0.206198,0.210948,0.215814,0.220806,0.225932,0.231201,
     . 0.236622,0.242207,0.247967,0.253914,0.260060,0.266418,0.273003,
     . 0.279828,0.286911,0.294267,0.301914,0.309869,0.318151,0.326780,
     . 0.335776,0.345162,0.354959,0.365190,0.375881,0.387056,0.398741,
     . 0.410964,0.423754,0.437138,0.451149,0.465816,0.481173,0.497254,
     . 0.514093,0.531726,0.541645,0.575115,0.613325,0.657009,0.707138,
     . 0.761274,0.814933,0.865964,0.912346,0.975327,1.00874,1.07029, 
     . 1.13559,1.20487,1.27838,1.35638,1.43913,1.52693,1.62009,1.71894,
     . 1.82381,1.93509,2.05315,2.17841,2.31132,2.45234,2.60196,2.76070,
     . 2.92914,3.10785,3.29746,3.49865,3.71210,3.93858,4.17888,4.43384,
     . 4.70435,4.99137,5.29590,5.61901,5.96184,6.32557 /)
	 
       deldia=(/0.000611557,0.000686178,0.000769936,0.000863883,
     . 0.000969193,0.00108745,0.00122018,0.00136907,0.00153621,
     .0.00172366,0.00193399,0.00216998,0.00243549,0.00273266,0.00306510,
     .0.00343910,0.00385822,0.00432900,0.00485756,0.00545027,0.00611557,
     .0.00686178,0.00769936,0.00863883,0.00969193,0.00233465,0.00240944,
     . 0.00248274,0.0025545,0.00262466,0.0026932,0.00276013,0.00282545,
     . 0.0028892,0.00295143,0.00301223,0.00307165,0.00312983,0.00318689,
     .0.00324296,0.00329821,0.00335281,0.00340698,0.00346093,0.00351489,
     . 0.0035691,0.00362383,0.00367941,0.00373612,0.00379424,0.00385418,
     .0.00391628,0.00398089,0.00404844,0.00411937,0.00419402,0.00427292,
     .0.00435651,0.00444529,0.00453978,0.00464044,0.00474788,0.00486264,
     .0.00498529,0.00511642,0.00525668,0.00540666,0.00556704,0.00573850,
     . 0.00592173,0.00611741,0.0063263,0.00654915,0.00678669,0.00703974,
     . 0.00730907,0.00759557,0.00789997,0.00822322,0.0085662,0.00892973,
     . 0.00931483,0.00972234,0.0101533,0.0106086,0.0110892,0.0115963,
     . 0.0121307,0.0126936,0.0132859,0.0139089,0.0145635,0.0152509,
     . 0.0159722,0.0167285,0.0175212,0.0183512,0.0347428,0.0352115,
     . 0.0375508,0.0473204,0.0466293,0.0559668,0.0542301,0.0502958,
     . 0.0558981,0.0502315,0.0479658,0.0652996,0.0692836,0.0735106,
     . 0.0779955,0.0827543,0.0878032,0.0931602,0.0988440,0.104875,
     . 0.111273,0.118062,0.125265,0.132908,0.141016,0.149620,0.158749,
     . 0.168434,0.178711,0.189614,0.201183,0.213457,0.226480,0.240298,
     . 0.254959,0.270514,0.287019,0.30453,0.32311,0.342823,0.363739,
     . 0.385932 /)
	  
	  
      CALL MIEDIST (WAVELENGTH, MSPHERE, RAD1, RAD2, NUMRAD,
     .          Diam, deldia, distribution,
     .          EXTINCTION, SSALBEDO, NLEGEN, 
     .          PHASECOEF11, PHASECOEF12, PHASECOEF33, PHASECOEF34,
     .          NANGLE, COSANGLE, ASY,
     .          PHASEFUNC11, PHASEFUNC12, PHASEFUNC33, PHASEFUNC34)


      CALL WRITE_SCAT_FILE (SCATFILE, WAVELENGTH, MSPHERE, 
     .         RAD1, RAD2, NUMRAD,
     .         EXTINCTION, SSALBEDO, NLEGEN,
     .         PHASECOEF11, PHASECOEF12, PHASECOEF33, PHASECOEF34)


C         Output the phase function at the gaussian quadrature angles
C           Format: cos(Theta)  Theta(deg)  Phase function elements
      OPEN (UNIT=1, FILE=PHASEFILE, STATUS='UNKNOWN')
      WRITE (1,'(A,A)') '!   mu    Theta      ',
     .         'P11          P12          P33          P34'
      DO I = NANGLE, 1, -1
        WRITE (1,'(1X,F7.4,1X,F6.2,4(1X,F12.5))') 
     .    COSANGLE(I), (180.0/3.1415927)*ACOS(COSANGLE(I)), 
     .    PHASEFUNC11(I),PHASEFUNC12(I),PHASEFUNC33(I),PHASEFUNC34(I)
      ENDDO

      CLOSE (1)    
      
      WRITE (*,'(1X,F10.6,1X,F10.6,1X,F10.6)') ASY, SSALBEDO, EXTINCTION
      
      END






      SUBROUTINE MIEDIST (WAVELENGTH, MSPHERE, RAD1, RAD2, NUMRAD,
     .          Diam, deldia,distribution, EXTINCTION, SSALBEDO, NLEGEN,
     .          PHASECOEF11, PHASECOEF12, PHASECOEF33, PHASECOEF34,
     .          NANGLE, COSANGLE, ASY,
     .          PHASEFUNC11, PHASEFUNC12, PHASEFUNC33, PHASEFUNC34)
C
C        Uses Mie theory to compute extinction, single scattering albedo,
C      and phase matrix for a distribution of spheres.
C        The distribution of particle sizes is a gamma distribution,
C      which is specified by three parameters Ntot (the total number
C      concentation), r_e (the effective radius), and alpha:
C           n(r) = (Ntot/r_e) * (alpha+3)^(alpha+1)/Gamma(alpha+1)
C                        * (r/r_e)^alpha * exp[-(alpha+3)* r/r_e ] 
C
C      The Mie results are integrated across the radius range using the
C      trapezoidal rule (endpoints have half weight). If the number of 
C      radius steps is set to zero, then only one radius is done, and
C      the number concentration (Ntot) is the only distribution parameter.
C        If the wavelength, effective radius, and radius range are
C      input in microns and the number concentration of particles in cm^-3,
C      then the volume extinction coefficient is output in km^-1.
C        The complex index of refraction is specified with a negative 
C      imaginary part. This code is based on the algorithm discussed in 
C      Bohren and Huffman (1983).
C
C         Parameter             Description
C        WAVELENGTH  input   Wavelength
C        MSPHERE     input   Complex index of refraction of the sphere
C        RAD1        input   Minimum sphere radius
C        RAD2        input   Maximum sphere radius
C        NUMRAD      input   Number of radius steps
C        NTOT        input   Total number concentration of particles (cm^-3)
C        REFF        input   Effective radius (microns)
C        ALPHA       input   Gamma distribution parameter alpha
C        EXTINCTION  output  Volume extinction (km^-1)
C        SSALBEDO    output  Single scattering albedo 
C        NLEGEN      output  Degree of Legendre series (L=0 to NLEGEN output)
C        PHASECOEF*  output  Arrays of Legendre phase function coefficients 
C        NANGLE      output  Number of angles output for phase function
C        COSANGLE    output  Array of cosine of the scattering angle
C        PHASEFUNC*  output  Arrays of phase function at the scattering angles
C        
      IMPLICIT   NONE
      INTEGER     NUMRAD, NLEGEN, NANGLE
      REAL*8      WAVELENGTH, RAD1, RAD2
      COMPLEX*16  MSPHERE
      REAL*8      EXTINCTION, SSALBEDO
      REAL*8      Diam(0:*), deldia(0:*)
      REAL*8      PHASECOEF11(0:*), PHASECOEF12(0:*)
      REAL*8      PHASECOEF33(0:*), PHASECOEF34(0:*)
      REAL*8      COSANGLE(*)
      REAL*8      PHASEFUNC11(*), PHASEFUNC12(*)
      REAL*8      PHASEFUNC33(*), PHASEFUNC34(*)
      INTEGER     MAXN
      PARAMETER   (MAXN=10000)
      REAL*8      PI
      PARAMETER   (PI = 3.14159265358979D0)
      INTEGER     NTERMS, NQUAD
      INTEGER     I, L, IR
      REAL*8      X, DELRAD, RADIUS, NDENS, TMP
      REAL*8      QEXT, QSCAT, QBACK, SCATTER, ASY
      REAL*8      DISTRIBUTION(0:*)
      REAL*8      MU(MAXN), WTS(MAXN)
      REAL*8      PL, PL1, PL2,  P11, P12, P33, P34
      REAL*8      SUMQE, SUMQS
      REAL*8      SUMP11(MAXN), SUMP12(MAXN), SUMP33(MAXN), SUMP34(MAXN)
      COMPLEX*16  A(MAXN), B(MAXN)


C         Find the maximum number of terms required in the Mie series,
C           from the effective size parameter X
      X = 2.0D0*PI*RAD2/WAVELENGTH
      NTERMS = 0
      CALL MIECALC (NTERMS, X, MSPHERE, A, B)
      NLEGEN = 2*NTERMS
      NQUAD  = (NLEGEN + 2*NTERMS + 2)/2
      IF (NLEGEN .GT. MAXN .OR. NQUAD .GT. MAXN) STOP 'MAXN exceeded'


C           Get the Gauss-Legendre quadrature abscissas and weights
      CALL GAUSQUAD (NQUAD, MU, WTS)

      SUMQE = 0.0
      SUMQS = 0.0
      DO I = 1, NQUAD
        SUMP11(I) = 0.0
        SUMP12(I) = 0.0
        SUMP33(I) = 0.0
        SUMP34(I) = 0.0
      ENDDO

C               Integration loop over radius of spheres
      DO IR = 1, NUMRAD+1
          RADIUS = Diam(IR)/2.
          NDENS = DISTRIBUTION(IR)
          NDENS = NDENS*deldia(IR)/2.
          IF (IR.EQ.1 .OR. IR.EQ.NUMRAD+1) THEN
            NDENS = 0.5*NDENS
          ENDIF
          X = 2.0D0*PI*RADIUS/WAVELENGTH
          NTERMS = 0
          CALL MIECALC (NTERMS, X, MSPHERE, A, B)
          CALL MIECROSS (NTERMS, X, A, B, QEXT, QSCAT, QBACK)
          SUMQE = SUMQE + QEXT*NDENS*RADIUS**2
          SUMQS = SUMQS + QSCAT*NDENS*RADIUS**2
          DO I = 1, NQUAD
            CALL MIEANGLE (NTERMS, A, B, MU(I), P11, P12, P33, P34)
            SUMP11(I) = SUMP11(I) + NDENS*P11
            SUMP12(I) = SUMP12(I) + NDENS*P12
            SUMP33(I) = SUMP33(I) + NDENS*P33
            SUMP34(I) = SUMP34(I) + NDENS*P34
          ENDDO
      ENDDO


C           Multiply the sums the constants
      EXTINCTION = PI*SUMQE
      SCATTER = PI*SUMQS
      SSALBEDO = SCATTER/EXTINCTION

C         Normalize the phase function by dividing by the scattering, etc.
      TMP = (WAVELENGTH**2/(PI*SCATTER))
      DO I = 1, NQUAD
        SUMP11(I) = TMP*SUMP11(I)
        SUMP12(I) = TMP*SUMP12(I)
        SUMP33(I) = TMP*SUMP33(I)
        SUMP34(I) = TMP*SUMP34(I)
      ENDDO

C         Put the scattering angle and phase functions in the output arrays
      NANGLE = NQUAD
      DO I = 1, NANGLE
        COSANGLE(I) = MU(I)
        PHASEFUNC11(I) = SUMP11(I)
        PHASEFUNC12(I) = SUMP12(I)
        PHASEFUNC33(I) = SUMP33(I)
        PHASEFUNC34(I) = SUMP34(I)
        IF (I.eq.1) then
         ASY=acos(cosangle(NANGLE))/(6.*NANGLE) *
     .   (cosangle(I)*PHASEFUNC11(I)* sin(acos(cosangle(I))))
        ELSE IF (I.eq.NANGLE) then
          ASY=ASY+acos(cosangle(NANGLE))/(6.*NANGLE) *
     .   (cosangle(I)*PHASEFUNC11(I)* sin(acos(cosangle(I))))
        ELSE IF (MOD(I,2).eq.0) then
          ASY=ASY+acos(cosangle(NANGLE))/(6.*NANGLE) *
     .   (2.*cosangle(I)*PHASEFUNC11(I)* sin(acos(cosangle(I))))
        ELSE IF (MOD(I,2).eq.1) then
          ASY=ASY+acos(cosangle(NANGLE))/(6.*NANGLE) *
     .   (4.*cosangle(I)*PHASEFUNC11(I)* sin(acos(cosangle(I))))
        ENDIF
      ENDDO
      

C           Integrate the angular scattering functions times Legendre
C             polynomials to find the Legendre coefficients
      DO L = 0, NLEGEN
        PHASECOEF11(L) = 0.0
        PHASECOEF12(L) = 0.0
        PHASECOEF33(L) = 0.0
        PHASECOEF34(L) = 0.0
      ENDDO
C           Use upward recurrence to find Legendre polynomials
      DO I = 1, NQUAD
          PL1 = 1.0
          PL = 1.0
          DO L = 0, NLEGEN
              IF (L .GT. 0)  PL = (2*L-1)*MU(I)*PL1/L - (L-1)*PL2/L
              PHASECOEF11(L) = PHASECOEF11(L) + SUMP11(I)*PL*WTS(I)
              PHASECOEF12(L) = PHASECOEF12(L) + SUMP12(I)*PL*WTS(I)
              PHASECOEF33(L) = PHASECOEF33(L) + SUMP33(I)*PL*WTS(I)
              PHASECOEF34(L) = PHASECOEF34(L) + SUMP34(I)*PL*WTS(I)
              PL2 = PL1
              PL1 = PL
          ENDDO
      ENDDO
      NTERMS = 0
      DO L = 0, NLEGEN
        PHASECOEF11(L) = (2*L+1)/2.0 *PHASECOEF11(L)
        PHASECOEF12(L) = (2*L+1)/2.0 *PHASECOEF12(L)
        PHASECOEF33(L) = (2*L+1)/2.0 *PHASECOEF33(L)
        PHASECOEF34(L) = (2*L+1)/2.0 *PHASECOEF34(L)
        IF (ABS(PHASECOEF11(L)) .GT. 1.0E-6 .OR. 
     .      ABS(PHASECOEF12(L)) .GT. 1.0E-6 .OR. 
     .      ABS(PHASECOEF33(L)) .GT. 1.0E-6 .OR. 
     .      ABS(PHASECOEF34(L)) .GT. 1.0E-6) THEN
          NTERMS=L
        ENDIF
      ENDDO
      NLEGEN = NTERMS

      RETURN
      END





      SUBROUTINE MIECALC (NTERMS, X, MN, A, B)
C        MIECALC calculates the complex Mie coefficients An and Bn
C      given the dimensionless size parameter X and the complex
C      index of refraction (Mre,Mim).  The number of terms calculated
C      is given by NTERMS unless NTERMS <= 0 or in which case the
C      appropriate number is calculated and returned in NTERMS.
      IMPLICIT   NONE
      INTEGER   NTERMS
      REAL*8    X
      COMPLEX*16  MN, A(*), B(*)
      INTEGER     MAXTERMS,  NSTOP, N, NN
      PARAMETER   (MAXTERMS=5000)
      REAL*8      PSIN, PSIM, CHIN, CHIM, TMP
      REAL*8      DCOS, DSIN
      COMPLEX*16  M, Y, D(MAXTERMS+15), XIN, XIM, CTMP
      COMPLEX*16  DCMPLX


C           If NTERMS is not specified calculate it
      NSTOP = X + 4.0*X**0.3334 + 2
      IF (NTERMS .LE. 0)  NTERMS = NSTOP
      IF (NTERMS .GT. MAXTERMS) THEN
          WRITE (*,*)
     .       'Mie calculation requires more terms than available.'
          STOP
      ENDIF

C           Generate the Dn's by down recurrence  D = d(log(PSI(y)))/dy
      M = DCONJG(MN)
      Y = M*X
      NN = NTERMS + 15
      D(NN) = DCMPLX (0.0D0, 0.0D0)
      DO 100 N = NN, 2, -1
          D(N-1) = N/Y - 1.0/ (D(N) + N/Y)
100   CONTINUE

C           Generate the PSIn's and XIn'S by upward recurrence
C             and calculate the An's and Bn's from them.
C             (PSIN = PSI(n), PSIM = PSI(n-1), same for CHI)
      PSIM = DCOS(X)
      PSIN = DSIN(X)
      CHIM = -DSIN(X)
      CHIN = DCOS(X)
      DO N = 1, NTERMS
          TMP = PSIN
          PSIN = (2*N-1)/X *PSIN - PSIM
          PSIM = TMP
          TMP = CHIN
          CHIN = (2*N-1)/X *CHIN - CHIM
          CHIM = TMP
          XIN = DCMPLX (PSIN, -CHIN)
          XIM = DCMPLX (PSIM, -CHIM)
          CTMP = D(N)/M + N/X
          A(N) = (CTMP*PSIN - PSIM) / (CTMP*XIN - XIM)
          CTMP = M*D(N) + N/X
          B(N) = (CTMP*PSIN - PSIM) / (CTMP*XIN - XIM)
      ENDDO

      RETURN
      END







      SUBROUTINE MIECROSS (NTERMS, X, A, B, QEXT, QSCAT, QBACK)
C        MIECROSS calculates the extinction, scattering, and
C      backscatter efficiencies given the Mie coefficients An and Bn
C      and the size parameter X.
      IMPLICIT   NONE
      INTEGER    NTERMS
      REAL*8     X, QEXT, QSCAT, QBACK
      COMPLEX*16 A(*), B(*)
      INTEGER    N
      REAL*8     SUM1, SUM2, DREAL
      COMPLEX*16 SUM3, DCONJG

      SUM1 = 0.0D0
      SUM2 = 0.0D0
      SUM3 = DCMPLX(0.0,0.0)
      DO N = 1, NTERMS
          SUM1 = SUM1 + (2*N+1)*( DREAL(A(N)) + DREAL(B(N)) )
          SUM2 = SUM2 + (2*N+1)*( DREAL(A(N)*DCONJG(A(N)))
     .                          + DREAL(B(N)*DCONJG(B(N))) )
          SUM3 = SUM3 + (2*N+1)*(-1)**N * (A(N) - B(N))
      ENDDO
      QEXT = 2.0D0/X**2 * SUM1
      QSCAT = 2.0D0/X**2 * SUM2
      QBACK = CDABS(SUM3)**2 / X**2

      RETURN
      END





      SUBROUTINE MIEANGLE (NTERMS, A, B, MU, P11, P12, P33, P34)
C        MIEANGLE calculates the phase function matrix elements
C      (P11,P12,P33,P34) for a particular value of MU (cos(Theta)) from
C      the Mie coefficients An's and Bn's.  The phase function matrix
C      elements are calculated from the complex scattering amplitudes 
C      S1 and S2.
C      
      IMPLICIT   NONE
      INTEGER    NTERMS
      REAL*8     MU, P11, P12, P33, P34
      COMPLEX*16 A(NTERMS), B(NTERMS)
      INTEGER    N
      REAL*8     TMP, PIN, PIM, TAUN, C
      COMPLEX*16 S1, S2


      S1 = DCMPLX(0.0,0.0)
      S2 = DCMPLX(0.0,0.0)
C               Sum up the series using the An's and Bn's
      PIN = 1.0
      PIM = 0.0
      DO N = 1, NTERMS
          TAUN = N*MU*PIN - (N+1)*PIM
C               Calculate the scattering functions using the PIn and the TAUn.
          C = (2*N+1) / DFLOAT(N*(N+1))
          S1 = S1 + C*( A(N)*PIN + B(N)*TAUN)
          S2 = S2 + C*( B(N)*PIN + A(N)*TAUN)
C               Calculate the angular function PIn by up recurrence
          TMP = PIN
          PIN = ( (2*N+1)*MU*PIN - (N+1)*PIM ) / N
          PIM = TMP
      ENDDO
C           Calculate the Stokes parameter scattering matrix elements
      P11 = 0.5*( CDABS(S2)**2 + CDABS(S1)**2 )
      P12 = 0.5*( CDABS(S2)**2 - CDABS(S1)**2 )
      P33 = DREAL( DCONJG(S1)*S2 )
      P34 = -DIMAG( DCONJG(S1)*S2 )
C      P11 = CDABS(S1)**2     Il Ir  system for Stokes parameters
C      P22 = CDABS(S2)**2

      RETURN
      END




C      REAL*8 FUNCTION DISTRIBUTION (NTOT, REFF, ALPHA, R)
C        DISTRIBUTION returns the particle density for a given radius R
C      for a gamma distribution specified by NTOT (the total number
C      concentation), REFF (the effective radius), and ALPHA:
C           n(r) = (Ntot/r_e) * (alpha+3)^(alpha+1)/Gamma(alpha+1)
C                        * (r/r_e)^alpha * exp[-(alpha+3)* r/r_e ] 
C      IMPLICIT   NONE
C      REAL*8   NTOT, REFF, ALPHA, R
C      REAL     GAMMLN
C      REAL*8   A, B

C        Convert to raw gamma distribution parameters.
C          The 0.001 converts from cm^-3 um^-1  to  um^-2 km^-1.
C      B = (ALPHA+3)/REFF
C      A = 0.001*NTOT * B**(ALPHA+1) /EXP(GAMMLN(ALPHA+1))
C      DISTRIBUTION = A* R**ALPHA * DEXP(-B*R)

C      RETURN
C      END



C      REAL FUNCTION GAMMLN(XX)
C      REAL*8  XX
C      INTEGER J
C      REAL*8  COF(6),STP,HALF,ONE,FPF,X,TMP,SER
C      DATA COF,STP/76.18009173D0,-86.50532033D0,24.01409822D0,
C     *    -1.231739516D0,.120858003D-2,-.536382D-5,2.50662827465D0/
C      DATA HALF,ONE,FPF/0.5D0,1.0D0,5.5D0/
C      X=XX-ONE
C      TMP=X+FPF
C      TMP=(X+HALF)*LOG(TMP)-TMP
C      SER=ONE
C      DO 11 J=1,6
C        X=X+ONE
C        SER=SER+COF(J)/X
C11    CONTINUE
C      GAMMLN=TMP+LOG(STP*SER)
C      RETURN
C      END





      SUBROUTINE GAUSQUAD (N, XA, WT)
C        Generates the abscissas (X) and weights (W) for an N point
C      Gauss-Legendre quadrature.  
      IMPLICIT   NONE
      INTEGER  N
      REAL*8   XA(*), WT(*)
      INTEGER  K, I, J, L
      REAL*8   X, XP, PL, PL1, PL2, DPL, TINY
      PARAMETER (TINY=3.0D-13)

      K = (N+1)/2
      DO 130 J = 1, K
        X = COS(3.141592654*(J-.25)/(N+.5))
        I = 0
100     CONTINUE
          PL1 = 1
          PL = X
          DO 120 L = 2, N
            PL2 = PL1
            PL1 = PL
            PL = ( (2*L-1)*X*PL1 - (L-1)*PL2 )/L
120       CONTINUE
          DPL = N*(X*PL-PL1)/(X*X-1)
          XP = X
          X = XP - PL/DPL
          I = I+1
        IF (ABS(X-XP).GT.TINY .AND. I.LT.10) GO TO 100
        XA(J)     = -X
        XA(N-J+1) = X
        WT(J  )   = 2.0D0/((1.0D0-X*X)*DPL*DPL)
        WT(N-J+1) = WT(J)
130   CONTINUE

      RETURN
      END





      SUBROUTINE WRITE_SCAT_FILE (SCATFILE, WAVELEN, MSPHERE, 
     .               RAD1, RAD2, NUMRAD,
     .               EXTINCTION, SSALBEDO, NLEGEN, 
     .               PHASECOEF11,PHASECOEF12,PHASECOEF33,PHASECOEF34)
C       Outputs the scattering file with the extinction, single scattering
C     albedo, and phase matrix Legendre series coefficients.  
C     A header containing the input parameters of the Mie calculation
C     is also output.
      IMPLICIT   NONE
      INTEGER     NLEGEN, NUMRAD
      REAL*8      WAVELEN, RAD1, RAD2
      REAL*8      EXTINCTION, SSALBEDO
      REAL*8      PHASECOEF11(0:*), PHASECOEF12(0:*)
      REAL*8      PHASECOEF33(0:*), PHASECOEF34(0:*)
      COMPLEX*16  MSPHERE
      CHARACTER*(*)  SCATFILE
      INTEGER  L

      OPEN (UNIT=1, FILE=SCATFILE, STATUS='UNKNOWN')

C           Output the comment header lines
      WRITE (1,'(A,E14.6)')       'C  WAVELENGTH=', WAVELEN
      WRITE (1,'(A,2E14.6))')     'C  REFRACT_INDEX=',MSPHERE
      WRITE (1,'(A,2(1X,E14.6))') 'C  RADIUS_RANGE=', RAD1, RAD2
      WRITE (1,'(A,I4)')          'C  NUMBER_INTEGRATION_STEPS=',NUMRAD
C      WRITE (1,'(A,E14.6)')       'C  GAMMA_DIST_NTOT=',NTOT
C      WRITE (1,'(A,F10.4)')       'C  GAMMA_DIST_REFF=',REFF
C      WRITE (1,'(A,F7.3)')        'C  GAMMA_DIST_ALPHA=',ALPHA


C           Output the volume extinction and single scattering albedo
      WRITE (1,'(1X,E12.5,A)') EXTINCTION, '    Extinction'
      WRITE (1,'(1X,E12.5,A)') SSALBEDO*EXTINCTION, '    Scattering'
      WRITE (1,'(1X,F8.6,A)') SSALBEDO, '    Single scattering albedo'
      WRITE (1,'(1X,I4,A)')  NLEGEN, '    Degree of legendre series'
C           The Legendre coefficients are output on one line for
C             each L value.  Each line contains six values.
C             Even though Mie scattering only produces four unique
C             values in the scattering matrix, the full 6 values
C             for the scattering matrix are output for
C             compatibility with non-spherical scatterers.
      DO L = 0, NLEGEN
        WRITE (1,'(1X,I4,6(1X,E11.5)))') L, 
     .    PHASECOEF11(L),PHASECOEF12(L),PHASECOEF33(L),PHASECOEF34(L),
     .    PHASECOEF11(L),PHASECOEF33(L)
      ENDDO

      CLOSE (1)

      RETURN
      END



