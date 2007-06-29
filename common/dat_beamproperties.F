c get beam properties

#ifndef DEBUG_LOC
#ifdef DEBUG
#define DEBUG_LOC print 100,__FILE__,__LINE__
#else
#define DEBUG_LOC
#endif !DEBUG_LOC
#endif

      SUBROUTINE dat_beamproperties(bp_data, rl_data)

      USE ansys_par
      USE ansys_fun
      USE glans
      USE LOCMOD

      IMPLICIT NONE
C     Purpose:
C
C     Parameter:
C     in/out Name          Task
C ----------------------------------------------------------------------
C     Created: 2007-06-01  hoel
C ======================================================================
      DOUBLE PRECISION bp_data(*)
      INTEGER rl_data(*)

      INTEGER n,rnum,rmax,m,oi,oj
      LOGICAL identical

      DOUBLE PRECISION t_min, t_max
      INTEGER :: iErr, i

      DOUBLE PRECISION, DIMENSION(400) :: rTable

! dataspace for feeding erhandler subroutine
      DOUBLE PRECISION ::  derrinfo(10)
      CHARACTER(LEN=PARMSIZE) :: cerrinfo(10)

      CHARACTER(LEN=40), PARAMETER :: fname=__FILE__

#ifdef DEBUG
 100  FORMAT (A,':',I3,':dat_beamproperties')
#endif

      CALL TrackBegin('dat_beamproperties')

c     select all real constants of BEAM44 elements
      CALL ans2bmf_rlnosel()
      CALL esel('S', 'ENAME', VMIN='BEAM44')

c     loop through all selected elements and select associated real
c     constant sets
      CALL ans2bmf_rlsle()

      rnum = rlinqr(0,DB_NUMSELECTED)
      rmax = rlinqr(0,DB_MAXDEFINED)

      derrinfo(1) = rnum
      CALL erhandler(fname, __LINE__, ERH_NOTE,
     $     'ans2bmf:   no. of BEAM44 rconst.: %i',
     $     derrinfo, cerrinfo)
      derrinfo(1) = rmax
      CALL erhandler(fname, __LINE__, ERH_NOTE,
     $     'ans2bmf:   no.of rconsts to test: %i',
     $     derrinfo, cerrinfo)

c     read real constants into array
      bp_num_i = 0
      bp_num_j = 0
      rl_num = 0

      DO n = 1, rmax

c     test if real constant set is selected (i.e. if it belongs to a
c     BEAM44 element)
         IF (rlinqr(n,DB_SELECTED).EQ.1) THEN

c     check for overflow
            IF (bp_num_i.GT.rnum) THEN
               derrinfo(1) = rnum
               derrinfo(2) = bp_num_i
               CALL erhandler(fname, __LINE__, ERH_ERROR,
     $              'ans2bmf: ERROR: %i  beam properties allocated. '//
     $              'Pos. %i accessed. This is greater or too close '//
     $              'to maximum.',
     $              derrinfo, cerrinfo)
               CALL anserr(4,'Beam Prop. Overflow',0.0,' ')
            END IF
            IF (rl_num.ge.rnum) THEN
               derrinfo(1) = rnum
               derrinfo(2) = rl_num
               CALL erhandler(fname, __LINE__, ERH_ERROR,
     $              'ans2bmf: ERROR: %i conversion table places av.. '//
     $              'Pos. %i accessed. This is greater or too close '//
     $              'to maximum.',
     $              derrinfo, cerrinfo)
               CALL anserr(4,'Conv.table Overflow',0.0,' ')
            END IF

c     read real constant set from ANSYS and convert BEAM44 properties in
c     beam properties
            CALL vzero (rtable, 40)
            i  = rlget (n, rtable)

            oi = 1 + bp_rlen * bp_num_i
            oj = 1 + bp_rlen * (rnum+bp_num_j)

            bp_data(oi + bp_R)      =  n                       !        r.const no.
            bp_data(oi + bp_A  + 0) =  rtable( 1)              !        AREA1
            bp_data(oi + bp_I  + 1) =  rtable( 2)              !        IZ1
            bp_data(oi + bp_I  + 2) =  rtable( 3)              !        IY1
            bp_data(oi + bp_e  + 4) = -rtable( 4)              ! - TKZB1 = ey2,i
            bp_data(oi + bp_e  + 2) = -rtable( 5)              ! - TKYB1 = ez1,i
            IF (rtable( 6).EQ.0d0) THEN
               bp_data(oi + bp_I  + 0) = rtable(2) + rtable(3) !        IX1
            ELSE
               bp_data(oi + bp_I  + 0) = rtable( 6)            !        IX1
            END IF

            bp_data(oj + bp_R)      =  rmax+bp_num_j+1         !        r.const no.
            ! Values on j side default to i side values
            IF (rtable( 7).EQ.0d0) THEN
               bp_data(oj + bp_A  + 0) =  rtable( 1)           !        AREA2
            ELSE
               bp_data(oj + bp_A  + 0) =  rtable( 7)           !        AREA2
            END IF
            IF (rtable( 8).EQ.0d0) THEN
               bp_data(oj + bp_I  + 1) =  rtable( 2)           !        IZ2
            ELSE
               bp_data(oj + bp_I  + 1) =  rtable( 8)           !        IZ2
            END IF
            IF (rtable( 9).EQ.0d0) THEN
               bp_data(oj + bp_I  + 2) =  rtable( 3)           !        IY2
            ELSE
               bp_data(oj + bp_I  + 2) =  rtable( 9)           !        IY2
            END IF
            IF (rtable(10).EQ.0d0) THEN
               bp_data(oj + bp_e  + 4) = -rtable( 4)           !      - TKZB2
            ELSE
               bp_data(oj + bp_e  + 4) = -rtable(10)           !      - TKZB2
            END IF
            IF (rtable(11).EQ.0d0) THEN
               bp_data(oj + bp_e  + 2) = -rtable( 5)           !      - TKYB2
            ELSE
               bp_data(oj + bp_e  + 2) = -rtable(11)           !      - TKYB2
            END IF
            IF (rtable(12).EQ.0d0) THEN
               bp_data(oj + bp_I  + 0) = bp_data(oi + bp_I  + 0) !       IX2
            ELSE
               bp_data(oj + bp_I  + 0) =  rtable(12)           !        IX2
            END IF

            bp_data(oi + bp_d + 0) =  rtable(13)               !        DX1
            bp_data(oi + bp_d + 1) =  rtable(14)               !        DY1
            bp_data(oi + bp_d + 2) =  rtable(15)               !        DZ1

            bp_data(oj + bp_d + 0) =  rtable(16)               !        DX2
            bp_data(oj + bp_d + 1) =  rtable(17)               !        DY2
            bp_data(oj + bp_d + 2) =  rtable(18)               !        DZ2

            IF (rtable(20) .NE. 0d0) THEN
               bp_data(oi+bp_sf+0) = bp_data(oi+bp_A)/rtable(20)!        A1/SHEARY
               bp_data(oj+bp_sf+0) = bp_data(oj+bp_A)/rtable(20)!        A2/SHEARY
            ELSE
               bp_data(oi+bp_sf+0) = 0d0
               bp_data(oj+bp_sf+0) = 0d0
            END IF
            IF (rtable(19) .NE. 0d0) THEN
               bp_data(oi+bp_sf+1) = bp_data(oi+bp_A)/rtable(19)!        A1/SHEARZ
               bp_data(oj+bp_sf+1) = bp_data(oj+bp_A)/rtable(19)!        A2/SHEARZ
            ELSE
               bp_data(oi+bp_sf+1) = 0d0
               bp_data(oj+bp_sf+1) = 0d0
            END IF

            bp_data(oi + bp_e  + 5) =  rtable(21)              !        TKZT1
            bp_data(oi + bp_e  + 3) =  rtable(22)              !        TKYT1

            bp_data(oj + bp_e  + 5) =  rtable(23)              !        TKZT2
            bp_data(oj + bp_e  + 3) =  rtable(24)              !        TKYT2

            bp_data(oi + bp_A  + 1) =  rtable(25)              !        ARESZ1
            bp_data(oi + bp_A  + 2) =  rtable(26)              !        ARESY1

            bp_data(oj + bp_A  + 1) =  rtable(27)              !        ARESZ2
            bp_data(oj + bp_A  + 2) =  rtable(28)              !        ARESY2

c            --- not used ---         rtable(29)              !        TSF1
c            --- not used ---         rtable(30)              !        TSF2

            bp_data(oi + bp_sc + 0) =  0.0
            bp_data(oi + bp_sc + 1) =  0.0
            bp_data(oj + bp_sc + 0) =  0.0
            bp_data(oj + bp_sc + 1) =  0.0

            t_min = MIN(
     $           bp_data(oi+bp_e+3)-bp_data(oi+bp_e+2),
     $           bp_data(oi+bp_e+5)-bp_data(oi+bp_e+4))
            t_max = MAX(
     $           bp_data(oi+bp_e+3)-bp_data(oi+bp_e+2),
     $           bp_data(oi+bp_e+5)-bp_data(oi+bp_e+4))
            IF (t_min.NE.0d0) THEN
               bp_data(oi + bp_e  + 0) = bp_data(oi + bp_I  + 0) / t_min
            ELSE
               bp_data(oi + bp_e  + 0) = 0d0
            END IF
            IF (t_max.NE.0d0) THEN
               bp_data(oi + bp_e  + 1) = bp_data(oi + bp_I  + 0) / t_max
            ELSE
               bp_data(oi + bp_e  + 1) = 0d0
            END IF

            t_min = MIN(
     $           bp_data(oj+bp_e+3)-bp_data(oj+bp_e+2),
     $           bp_data(oj+bp_e+5)-bp_data(oj+bp_e+4))
            t_max = MAX(
     $           bp_data(oj+bp_e+3)-bp_data(oj+bp_e+2),
     $           bp_data(oj+bp_e+5)-bp_data(oj+bp_e+4))
            IF (t_min.NE.0d0) THEN
               bp_data(oj + bp_e  + 0) = bp_data(oj + bp_I  + 0) / t_min
            ELSE
               bp_data(oj + bp_e  + 0) = 0d0
            END IF
            IF (t_max.NE.0d0) THEN
               bp_data(oj + bp_e  + 1) = bp_data(oj + bp_I  + 0) / t_max
            ELSE
               bp_data(oj + bp_e  + 1) = 0d0
            END IF

c     compare the sets for node i and j and see if they are equal

            identical = .TRUE.
            DO m = 1, bp_rlen-1
               IF (ABS(bp_data(oi+m)-bp_data(oj+m)).GT.NZERO) THEN
                  identical = .FALSE.
                  GOTO 1000
               END IF
            END DO
 1000       CONTINUE
            IF (identical) THEN
c     sets are equal: keep only one set of beam props
               rl_data(rl_num*3 + 1) = n
               rl_data(rl_num*3 + 2) = bp_num_i
               rl_data(rl_num*3 + 3) = bp_num_i
               bp_num_i = bp_num_i + 1
            ELSE
c     sets are different: keep both sets of beam props
               rl_data(rl_num*3 + 1) = n
               rl_data(rl_num*3 + 2) = bp_num_i
               rl_data(rl_num*3 + 3) = bp_num_j+rnum
               bp_num_i = bp_num_i + 1
               bp_num_j = bp_num_j + 1
            END IF
            rl_num = rl_num + 1

         END IF
      END DO

      derrinfo(1) = bp_num_i+bp_num_j
      derrinfo(2) = bp_num_i
      derrinfo(3) = bp_num_j
      CALL erhandler(fname, __LINE__, ERH_NOTE,
     $     'ans2bmf:   no.beam prop. generated: %i %i %i',
     $     derrinfo, cerrinfo)

      CALL ans2bmf_rlallsel()

      CALL TrackEnd('dat_beamproperties')

      END

c###########################################################

      SUBROUTINE ans2bmf_rlallsel()

      USE ansys_par
      USE ansys_fun

      IMPLICIT NONE
C     Purpose:
C
C     Parameter:
C     in/out Name          Task
C ----------------------------------------------------------------------
C     Created: 2007-06-01  hoel
C ======================================================================
      INTEGER r,rmax

      CALL TrackBegin('ans2bmf_rlallsel')

      rmax = rlinqr(0,DB_MAXDEFINED)
      DO r = 1, rmax
         CALL rlsel(r,1)
      END DO

      CALL TrackEnd('ans2bmf_rlallsel')

      END

c-----------------------------------------------------------

      SUBROUTINE ans2bmf_rlnosel()

      USE ansys_par
      USE ansys_fun

      IMPLICIT NONE
C     Purpose:
C
C     Parameter:
C     in/out Name          Task
C ----------------------------------------------------------------------
C     Created: 2007-06-01  hoel
C ======================================================================
      INTEGER r,rmax

      CALL TrackBegin('ans2bmf_rlnosel')

      rmax = rlinqr(0,DB_MAXDEFINED)
      do r = 1, rmax
         CALL rlsel(r,-1)
      END DO

      CALL TrackEnd('ans2bmf_rlnosel')

      END

c-----------------------------------------------------------

      SUBROUTINE ans2bmf_rlsle()

      USE ansys_par
      USE ansys_fun
      USE LOCMOD

      IMPLICIT NONE
C     Purpose:
C     loop through all selected elements and select associated real
C     constant sets
C     Parameter:
C     in/out Name          Task
C ----------------------------------------------------------------------
C     Created: 2007-06-01  hoel
C ======================================================================
      INTEGER :: el, i
      INTEGER, DIMENSION(EL_DIM) :: elmdat
      INTEGER, DIMENSION(NNMAX) :: nodes

      CALL TrackBegin('ans2bmf_rlsle')

      CALL ans2bmf_rlnosel()

      el = elnext(0)
      DO WHILE (el.GT.0)
         i = elmget(el, elmdat, nodes)
         CALL rlsel(elmdat(EL_REAL),1)
         el = elnext(el)
      END DO

      CALL TrackEnd('ans2bmf_rlsle')

      END

c Local Variables:
c compile-command:"make -C .. test"
c End:
