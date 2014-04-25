! get beam properties

#ifndef DEBUG_LOC
#ifdef DEBUG
#define DEBUG_LOC print 100,__FILE__,__LINE__
#else
#define DEBUG_LOC
#endif !DEBUG_LOC
#endif

MODULE mod_dat_beamproperties

  USE ansys_par, ONLY : PARMSIZE, ERH_FNAME_LEN

  PRIVATE

  ! dataspace for feeding erhandler subroutine
  REAL(KIND=8), DIMENSION(10) ::  derrinfo
  CHARACTER(LEN=PARMSIZE), DIMENSION(10) :: cerrinfo
  CHARACTER(LEN=ERH_FNAME_LEN), PARAMETER :: fname=__FILE__

  PUBLIC :: dat_beamproperties, ans2bmf_rlnosel, ans2bmf_rlsle

CONTAINS

  SUBROUTINE dat_beamproperties(bp)

    USE ans_common

    IMPLICIT NONE

    TYPE(bp_type) :: bp

    CALL TrackBegin("dat_beamp")

    CALL dat_beamproperties_rc(bp)
    CALL dat_beamproperties_sec(bp)

    CALL TrackEnd("dat_beamp")

  END SUBROUTINE dat_beamproperties

  SUBROUTINE dat_beamproperties_sec(bp)

    USE ansys_upf, ONLY : TrackBegin, TrackEnd, erhandler
    USE ansys_par, ONLY : ERH_FATAL, STRING_MAX_LENG
    USE ans_common
    USE glans
    USE LOCMOD, ONLY : libname

    IMPLICIT NONE

    TYPE(bp_type), INTENT(INOUT) :: bp

    INTEGER :: n_sect

    CHARACTER(LEN=STRING_MAX_LENG) :: sec_type
    CHARACTER(LEN=STRING_MAX_LENG) :: sec_subtype

    INTEGER :: n

    CHARACTER(LEN=STRING_MAX_LENG) :: name
    REAL(KIND=8) :: area             ! Area value
    REAL(KIND=8) :: iyy, iyz, izz    ! Moments of inertia
    REAL(KIND=8) :: warp_val         ! Warping constant
    REAL(KIND=8) :: tors             ! Torsion constant
    REAL(KIND=8) :: cgy, cgz         ! Y or Z coordinate center of gravity
    REAL(KIND=8) :: shcy, shcz       ! Y or Z coordinate shear center
    REAL(KIND=8) :: scyy, scyz, sczz ! Shear correction factors
    INTEGER :: offset                ! Offset location
    REAL(KIND=8) :: offy             ! Section offset in the Y-direction.
    REAL(KIND=8) :: offz             ! Section offset in the Z-direction.

    ! outer dimensions for cross section
    REAL(KIND=8) :: t_y_1, t_y_2, t_z_1, t_z_2

    REAL(KIND=8) :: yI, zI, yJ, zJ, yK, zK, yL, zL

    CALL TrackBegin("dat_beamp_sec")

    ! Loop over all sections
    IF (get(n_sect, 'SECP', 0, 'NUM     ', 'MAX     ')) THEN
       CALL erhandler(fname, __LINE__, ERH_FATAL, &
            TRIM(libname)//':   Determining number of sections failed.', &
            derrinfo, cerrinfo)
    END IF
    derrinfo(1) = n_sect
    CALL erhandler(fname, __LINE__, ERH_NOTE, &
         TRIM(libname)//':   no. of section definitions: %i', &
         derrinfo, cerrinfo)
    DO n = 1, n_sect

       derrinfo(1) = n
       IF (get(sec_type, 'SECP', n, 'TYPE    ')) THEN
          CALL erhandler(fname, __LINE__, ERH_FATAL, &
               TRIM(libname)//': Determining section TYPE for section %i failed.', &
               derrinfo, cerrinfo)
       END IF

       IF (sec_type.eq.'BEAM') THEN

          bp%num = bp%num + 1
          bp%snum = bp%snum + 1
          bp%se_bp_map(bp%snum)%r = n
          bp%se_bp_map(bp%snum)%i = bp%num
          bp%se_bp_map(bp%snum)%j = bp%num

          bp%data(bp%num)%A(:) = 0d0
          bp%data(bp%num)%I(:) = 0d0
          bp%data(bp%num)%e(:) = 0d0
          bp%data(bp%num)%sc(:) = 0d0
          bp%data(bp%num)%d(:) = 0d0
          bp%data(bp%num)%Iyz = 0d0

          t_y_1 = 0d0
          t_y_2 = 0d0
          t_z_1 = 0d0
          t_z_2 = 0d0

          CALL get_t_beam_common(n, name, area, iyy, iyz, izz, warp_val, tors, &
               cgy, cgz, shcy, shcz, scyy, scyz, sczz, offset, offy, offz)

          bp%data(bp%num)%name(:) = name(1:16)
          bp%data(bp%num)%A(:) = (/ area, area * scyy, area * sczz /)

          bp%data(bp%num)%i(:) = (/ tors, izz, iyy /)

          bp%data(bp%num)%iyz = iyz

          IF (get(sec_subtype, 'SECP', n, 'SUBTYPE ')) THEN
             CALL erhandler(fname, __LINE__, ERH_FATAL, &
                  TRIM(libname)//': Determining section SUBTYPE for section %i failed.', &
                  derrinfo, cerrinfo)
          END IF

          IF (sec_subtype == 'RECT') THEN
             CALL get_t_beam_rect(t_y_1, t_y_2, t_z_1, t_z_2, cgy, cgz, n)
          ELSE IF (sec_subtype == 'QUAD') THEN
             CALL get_t_beam_quad(t_y_1, t_y_2, t_z_1, t_z_2, cgy, cgz, n)
          ELSE IF (sec_subtype == 'CSOL') THEN
             CALL get_t_beam_csolid(t_y_1, t_y_2, t_z_1, t_z_2, cgy, cgz, n)
          ELSE IF (sec_subtype == 'CTUB') THEN
             CALL get_t_beam_ctube(t_y_1, t_y_2, t_z_1, t_z_2, cgy, cgz, n)
          ELSE IF (sec_subtype == 'CHAN') THEN
             CALL get_t_beam_chan(t_y_1, t_y_2, t_z_1, t_z_2, cgy, cgz, n)
          ELSE IF (sec_subtype == 'I') THEN
             CALL get_t_beam_i(t_y_1, t_y_2, t_z_1, t_z_2, cgy, cgz, n)
          ELSE IF (sec_subtype == 'Z') THEN
             CALL get_t_beam_z(t_y_1, t_y_2, t_z_1, t_z_2, cgy, cgz, n)
          ELSE IF (sec_subtype == 'L') THEN
             CALL get_t_beam_l(t_y_1, t_y_2, t_z_1, t_z_2, cgy, cgz, n)
          ELSE IF (sec_subtype == 'T') THEN
             CALL get_t_beam_t(t_y_1, t_y_2, t_z_1, t_z_2, cgy, cgz, n)
          ELSE IF (sec_subtype == 'HATS') THEN
             CALL get_t_beam_hats(t_y_1, t_y_2, t_z_1, t_z_2, cgy, cgz, n)
          ELSE IF (sec_subtype == 'HREC') THEN
             CALL get_t_beam_hrec(t_y_1, t_y_2, t_z_1, t_z_2, cgy, cgz, n)
          ELSE IF (sec_subtype == 'ASEC') THEN
             CALL get_t_beam_asec(t_y_1, t_y_2, t_z_1, t_z_2, cgy, cgz, n)
          ELSE IF (sec_subtype == 'MESH') THEN
             CALL get_t_beam_mesh(t_y_1, t_y_2, t_z_1, t_z_2, cgy, cgz, n)
          ELSE
             cerrinfo(1) = sec_subtype
             CALL erhandler(fname, __LINE__, ERH_FATAL, &
                  TRIM(libname)//': Section SUBTYPE "%s" for section %i not supported.', &
                  derrinfo, cerrinfo)
          END IF

          ! t_y_1 = ABS(t_y_1 - cgy)
          ! t_y_2 = ABS(t_y_2 - cgy)
          ! t_z_1 = ABS(t_z_1 - cgz)
          ! t_z_2 = ABS(t_z_2 - cgz)

          IF (MIN(ABS(t_y_2) + ABS(t_y_2), ABS(t_z_1) + ABS(t_z_2)) .NE. 0d0) THEN
             bp%data(bp%num)%e(1) = tors / MIN(ABS(t_y_2) + ABS(t_y_2), ABS(t_z_1) + ABS(t_z_2))
          END IF
         IF (MAX(ABS(t_y_1) + ABS(t_y_2), ABS(t_z_1) + ABS(t_z_2)) .NE. 0d0) THEN
             bp%data(bp%num)%e(2) = tors / MAX(ABS(t_y_1) + ABS(t_y_2), ABS(t_z_1) + ABS(t_z_2))
          END IF
          bp%data(bp%num)%e(3:6) = (/ t_y_1, t_y_2, t_z_1, t_z_2 /)

          bp%data(bp%num)%d(:) = (/ 0d0, offy - cgy, -offz - cgz /)

       END IF

    END DO

    CALL TrackEnd("dat_beamp_sec")

  END SUBROUTINE dat_beamproperties_sec

  SUBROUTINE dat_beamproperties_rc(bp)

    USE ansys_upf, ONLY : TrackBegin, TrackEnd, erhandler, rlget, &
         rlinqr, vzero
    USE ansys_par, ONLY : DB_NUMSELECTED, DB_SELECTED, DB_MAXDEFINED, &
         ERH_ERROR, ERH_NOTE
    USE glans
    USE ans_common
    USE gl_math
    USE LOCMOD, ONLY : libname

    ! Purpose:
    !
    ! Parameter:
    ! in/out Name          Task
    ! ----------------------------------------------------------------------
    ! Created: 2007-06-01  hoel
    ! ======================================================================

    IMPLICIT NONE

    TYPE(bp_type), INTENT(INOUT) :: bp

    INTEGER n, rnum, rmax, m, oi, oj

    REAL(KIND=8) t_min, t_max
    INTEGER :: iErr, i

    REAL(KIND=8), DIMENSION(400) :: rTable

    TYPE(bp_values) :: i_vals, j_vals

    REAL(KIND=8) :: Iyy, Izz, Iyz
    REAL(KIND=8) :: theta

#ifdef DEBUG
100 FORMAT (A, ':', I3, ':dat_beamproperties_rc')
#endif

    CALL TrackBegin('dat_beamproperties_rc')

    ! select all real constants of BEAM44 elements
    CALL ans2bmf_rlnosel()
    CALL esel('S', 'ENAME', VMIN='BEAM44')

    ! loop through all selected elements and select associated real
    ! constant sets
    CALL ans2bmf_rlsle()

    rnum = rlinqr(0, DB_NUMSELECTED)
    rmax = rlinqr(0, DB_MAXDEFINED)

    derrinfo(1) = rnum
    CALL erhandler(fname, __LINE__, ERH_NOTE, &
         TRIM(libname)//':   no. of BEAM44 rconst.: %i', &
         derrinfo, cerrinfo)
    derrinfo(1) = rmax
    CALL erhandler(fname, __LINE__, ERH_NOTE, &
         TRIM(libname)//':   no.of rconsts to test: %i', &
         derrinfo, cerrinfo)

    DO n = 1, rmax

       ! test if real constant set is selected (i.e. if it belongs to a
       ! BEAM44 element)
       IF (rlinqr(n, DB_SELECTED).EQ.1) THEN
          ! read real constant set from ANSYS and convert BEAM44 properties in
          ! beam properties
          CALL vzero(rtable, 40)
          i  = rlget(n, rtable)

          i_vals%e(:) =  0.0
          j_vals%e(:) =  0.0


          i_vals%A(1) = rtable( 1)               !       AREA1
          i_vals%I(2) = rtable( 2)               !       IZ1
          i_vals%I(3) = rtable( 3)               !       IY1
          i_vals%e(5) = rtable( 4)               !  TKZB1 = ey2, i
          i_vals%e(3) = rtable( 5)               !  TKYB1 = ez1, i
          IF (rtable( 6).EQ.0d0) THEN
             i_vals%I(1) = rtable(2) + rtable(3) !       IX1
          ELSE
             i_vals%I(1) = rtable( 6)            !       IX1
          END IF

          !j_vals%R      =  rmax+bp_num_j+1         !       r.const no.
          ! Values on j side default to i side values
          IF (rtable( 7).EQ.0d0) THEN
             j_vals%A(1) =  rtable( 1)           !       AREA2
          ELSE
             j_vals%A(1) =  rtable( 7)           !       AREA2
          END IF
          IF (rtable( 8).EQ.0d0) THEN
             j_vals%I(2) =  rtable( 2)           !       IZ2
          ELSE
             j_vals%I(2) =  rtable( 8)           !       IZ2
          END IF
          IF (rtable( 9).EQ.0d0) THEN
             j_vals%I(3) =  rtable( 3)           !       IY2
          ELSE
             j_vals%I(3) =  rtable( 9)           !       IY2
          END IF

          IF (rtable(10).EQ.0d0) THEN
             j_vals%e(5) = rtable( 4)            !       TKZB2
          ELSE
             j_vals%e(5) = rtable(10)            !       TKZB2
          END IF
          IF (rtable(11).EQ.0d0) THEN
             j_vals%e(3) = rtable( 5)            !       TKYB2
          ELSE
             j_vals%e(3) = rtable(11)            !       TKYB2
          END IF
          IF (rtable(12).EQ.0d0) THEN
             j_vals%I(1) = i_vals%I(1)           !       IX2
          ELSE
             j_vals%I(1) =  rtable(12)           !       IX2
          END IF

          i_vals%d(1) = rtable(13)               !       DX1
          i_vals%d(2) = -rtable(14)               !       DY1
          i_vals%d(3) = rtable(15)               !       DZ1

          j_vals%d(1) = rtable(16)               !       DX2
          j_vals%d(2) = -rtable(17)               !       DY2
          j_vals%d(3) = rtable(18)               !       DZ2

          IF (rtable(20) .NE. 0d0) THEN
             i_vals%A(2) = i_vals%A(1)/rtable(20)!      A1/SHEARY
             j_vals%A(2) = j_vals%A(1)/rtable(20)!      A2/SHEARY
          ELSE
             i_vals%A(2) = 0d0
             j_vals%A(2) = 0d0
          END IF
          IF (rtable(19) .NE. 0d0) THEN
             i_vals%A(3) = i_vals%A(1)/rtable(19)!      A1/SHEARZ
             j_vals%A(3) = j_vals%A(1)/rtable(19)!      A2/SHEARZ
          ELSE
             i_vals%A(3) = 0d0
             j_vals%A(3) = 0d0
          END IF

          i_vals%e(6) =  rtable(21)              !       TKZT1
          i_vals%e(4) =  rtable(22)              !       TKYT1

          j_vals%e(6) =  rtable(23)              !       TKZT2
          j_vals%e(4) =  rtable(24)              !       TKYT2

          ! i_vals%A(2) =  rtable(25)              !       ARESZ1
          ! i_vals%A(3) =  rtable(26)              !       ARESY1

          ! j_vals%A(2) =  rtable(27)              !       ARESZ2
          ! j_vals%A(3) =  rtable(28)              !       ARESY2

          ! --- not used ---         rtable(29)              !       TSF1
          ! --- not used ---         rtable(30)              !       TSF2

          theta = deg2rad(rtable(53))

          Iyy = 5d-1 * (i_vals%I(2) + i_vals%I(3)) + 5d-1 * ( i_vals%I(2) - i_vals%I(3)) * COS(2d0 * theta)
          Izz = 5d-1 * (i_vals%I(2) + i_vals%I(3)) - 5d-1 * ( i_vals%I(2) - i_vals%I(3)) * COS(2d0 * theta)
          Iyz = 5d-1 * (i_vals%I(2) - i_vals%I(3)) * SIN(2d0 * theta)

          i_vals%I(2) = Iyy
          i_vals%I(3) = Izz
          i_vals%Iyz = Iyz

          Iyy = 5d-1 * (j_vals%I(2) + j_vals%I(3)) + 5d-1 * (j_vals%I(2) - j_vals%I(3)) * COS(2d0 * theta)
          Izz = 5d-1 * (j_vals%I(2) + j_vals%I(3)) - 5d-1 * (j_vals%I(2) - j_vals%I(3)) * COS(2d0 * theta)
          Iyz = 5d-1 * (j_vals%I(2) - j_vals%I(3)) * SIN(2d0 * theta)

          j_vals%I(2) = Iyy
          j_vals%I(3) = Izz
          j_vals%Iyz = Iyz

          i_vals%theta = 0d0
          j_vals%theta = 0d0

          i_vals%sc(:) =  0d0
          j_vals%sc(:) =  0d0

          t_min = MIN( &
               i_vals%e(4) + i_vals%e(3), &
               i_vals%e(6) + i_vals%e(5))
          t_max = MAX( &
               i_vals%e(4) + i_vals%e(3), &
               i_vals%e(6) + i_vals%e(5))
          IF (t_min.NE.0d0) THEN
             i_vals%e(1) = i_vals%I(1) / t_min
          END IF
          IF (t_max.NE.0d0) THEN
             i_vals%e(2) = i_vals%I(1) / t_max
          END IF

          t_min = MIN( &
               j_vals%e(4) + j_vals%e(3), &
               j_vals%e(6) + j_vals%e(5))
          t_max = MAX( &
               j_vals%e(4) + j_vals%e(3), &
               j_vals%e(6) + j_vals%e(5))
          IF (t_min.NE.0d0) THEN
             j_vals%e(1) = j_vals%I(1) / t_min
          ELSE
             j_vals%e(1) = 0d0
          END IF
          IF (t_max.NE.0d0) THEN
             j_vals%e(2) = j_vals%I(1) / t_max
          ELSE
             j_vals%e(2) = 0d0
          END IF

          bp%num = bp%num + 1
          bp%rnum = bp%rnum + 1
          bp%data(bp%num) = i_vals
          bp%r_bp_map(bp%rnum)%r = n
          bp%r_bp_map(bp%rnum)%i = bp%num
          IF (i_vals .NE. j_vals) THEN
             bp%num = bp%num + 1
             bp%data(bp%num) = j_vals
          END IF
          bp%r_bp_map(bp%rnum)%j = bp%num

       END IF
    END DO

    derrinfo(1) = bp%num
    CALL erhandler(fname, __LINE__, ERH_NOTE, &
         TRIM(libname)//':   no.beam prop. generated: %i', &
         derrinfo, cerrinfo)

    CALL ans2bmf_rlallsel()

    CALL TrackEnd('dat_beamproperties_rc')

  END SUBROUTINE dat_beamproperties_rc

!###########################################################

  SUBROUTINE ans2bmf_rlallsel()

    USE ansys_upf, ONLY : TrackBegin, TrackEnd, rlinqr, rlsel
    USE ansys_par, ONLY : DB_MAXDEFINED

    IMPLICIT NONE
    ! Purpose:
    !
    ! Parameter:
    ! in/out Name          Task
    ! ----------------------------------------------------------------------
    ! Created: 2007-06-01  hoel
    ! ======================================================================
    INTEGER r, rmax

    CALL TrackBegin('ans2bmf_rlallsel')

    rmax = rlinqr(0, DB_MAXDEFINED)
    DO r = 1, rmax
       CALL rlsel(r, 1)
    END DO

    CALL TrackEnd('ans2bmf_rlallsel')

  END SUBROUTINE ans2bmf_rlallsel

!-----------------------------------------------------------

  SUBROUTINE ans2bmf_rlnosel()

    USE ansys_upf, ONLY : TrackBegin, TrackEnd, rlinqr, rlsel
    USE ansys_par, ONLY : DB_MAXDEFINED

    IMPLICIT NONE
    ! Purpose:
    !
    ! Parameter:
    ! in/out Name          Task
    ! ----------------------------------------------------------------------
    ! Created: 2007-06-01  hoel
    ! ======================================================================
    INTEGER r, rmax

    CALL TrackBegin('ans2bmf_rlnosel')

    rmax = rlinqr(0, DB_MAXDEFINED)
    DO r = 1, rmax
       CALL rlsel(r, -1)
    END DO

    CALL TrackEnd('ans2bmf_rlnosel')

  END SUBROUTINE ans2bmf_rlnosel

!-----------------------------------------------------------

  SUBROUTINE ans2bmf_rlsle()

    USE ansys_upf, ONLY : TrackBegin, TrackEnd, elmget, elnext
    USE ansys_par, ONLY : EL_DIM, EL_REAL, NNMAX

    IMPLICIT NONE
    ! Purpose:
    ! loop through all selected elements and select associated real
    ! constant sets
    ! Parameter:
    ! in/out Name          Task
    ! ----------------------------------------------------------------------
    ! Created: 2007-06-01  hoel
    ! ======================================================================
    INTEGER :: el, i
    INTEGER, DIMENSION(EL_DIM) :: elmdat
    INTEGER, DIMENSION(NNMAX) :: nodes

    CALL TrackBegin('ans2bmf_rlsle')

    CALL ans2bmf_rlnosel()

    el = elnext(0)
    DO WHILE (el.GT.0)
       i = elmget(el, elmdat, nodes)
       CALL rlsel(elmdat(EL_REAL), 1)
       el = elnext(el)
    END DO

    CALL TrackEnd('ans2bmf_rlsle')

  END SUBROUTINE ans2bmf_rlsle

  SUBROUTINE get_t_beam_common(n, name, area, iyy, iyz, izz, warp_val, tors, cgy, cgz, &
       shcy, shcz, scyy, scyz, sczz, offset, offy, offz)

    USE ansys_upf, ONLY : TrackBegin, TrackEnd, erhandler
    USE ansys_par, ONLY : ERH_FATAL
    USE ans_common
    USE glans
    USE LOCMOD, ONLY : libname

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: n
    CHARACTER(LEN=*), INTENT(OUT) :: name
    ! cross area
    REAL(KIND=8), INTENT(OUT) :: area
    ! moments of interna
    REAL(KIND=8), INTENT(OUT) :: iyy, iyz, izz, tors
    ! warping factor
    REAL(KIND=8), INTENT(OUT) :: warp_val
    ! rel. loc. of centre of gravity
    REAL(KIND=8), INTENT(OUT) :: cgy, cgz
    ! rel. loc. of shear center
    REAL(KIND=8), INTENT(OUT) :: shcy, shcz
    ! Shear correction factors
    REAL(KIND=8), INTENT(OUT) :: scyy, scyz, sczz
    ! offset location: 1 = Centroid; 2 = Shear Center; 3 = Origin; 0 = User Defined
    INTEGER, INTENT(OUT) :: offset
    ! Section offset in the Y- and Z-direction.
    REAL(KIND=8), INTENT(OUT) :: offy, offz

    CALL TrackBegin("get_t_beam_common")

    IF (get(name, 'SECP', n, 'NAME     ')) THEN
       CALL erhandler(fname, __LINE__, ERH_FATAL, &
            TRIM(libname)//': Determining section NAME for section %i failed.', &
            derrinfo, cerrinfo)
    END IF
    IF (get(area, 'SECP', n, 'PROP     ', 'AREA    ')) THEN
       CALL erhandler(fname, __LINE__, ERH_FATAL, &
            TRIM(libname)//': Determining section AREA for section %i failed.', &
            derrinfo, cerrinfo)
    END IF
    IF (get(iyy, 'SECP', n, 'PROP     ', 'IYY     ')) THEN
       CALL erhandler(fname, __LINE__, ERH_FATAL, &
            TRIM(libname)//': Determining section IYY for section %i failed.', &
            derrinfo, cerrinfo)
    END IF
    IF (get(iyz, 'SECP', n, 'PROP     ', 'IYZ     ')) THEN
       CALL erhandler(fname, __LINE__, ERH_FATAL, &
            TRIM(libname)//': Determining section IYZ for section %i failed.', &
            derrinfo, cerrinfo)
    END IF
    IF (get(izz, 'SECP', n, 'PROP     ', 'IZZ     ')) THEN
       CALL erhandler(fname, __LINE__, ERH_FATAL, &
            TRIM(libname)//': Determining section IZZ for section %i failed.', &
            derrinfo, cerrinfo)
    END IF
    IF (get(warp_val, 'SECP', n, 'PROP     ', 'WARP    ')) THEN
       CALL erhandler(fname, __LINE__, ERH_FATAL, &
            TRIM(libname)//': Determining section WARP for section %i failed.', &
            derrinfo, cerrinfo)
    END IF
    IF (get(tors, 'SECP', n, 'PROP     ', 'TORS    ')) THEN
       CALL erhandler(fname, __LINE__, ERH_FATAL, &
            TRIM(libname)//': Determining section TORS for section %i failed.', &
            derrinfo, cerrinfo)
    END IF
    IF (get(cgy, 'SECP', n, 'PROP     ', 'CGY     ')) THEN
       CALL erhandler(fname, __LINE__, ERH_FATAL, &
            TRIM(libname)//': Determining section CGY for section %i failed.', &
            derrinfo, cerrinfo)
    END IF
    IF (get(cgz, 'SECP', n, 'PROP     ', 'CGZ     ')) THEN
       CALL erhandler(fname, __LINE__, ERH_FATAL, &
            TRIM(libname)//': Determining section CGZ for section %i failed.', &
            derrinfo, cerrinfo)
    END IF
    IF (get(shcy, 'SECP', n, 'PROP     ', 'SHCY    ')) THEN
       CALL erhandler(fname, __LINE__, ERH_FATAL, &
            TRIM(libname)//': Determining section SHCY for section %i failed.', &
            derrinfo, cerrinfo)
    END IF
    IF (get(shcz, 'SECP', n, 'PROP     ', 'SHCZ    ')) THEN
       CALL erhandler(fname, __LINE__, ERH_FATAL, &
            TRIM(libname)//': Determining section SHCZ for section %i failed.', &
            derrinfo, cerrinfo)
    END IF
    IF (get(scyy, 'SECP', n, 'PROP     ', 'SCYY    ')) THEN
       CALL erhandler(fname, __LINE__, ERH_FATAL, &
            TRIM(libname)//': Determining section SCYY for section %i failed.', &
            derrinfo, cerrinfo)
    END IF
    IF (get(scyz, 'SECP', n, 'PROP     ', 'SCYZ    ')) THEN
       CALL erhandler(fname, __LINE__, ERH_FATAL, &
            TRIM(libname)//': Determining section SCYZ for section %i failed.', &
            derrinfo, cerrinfo)
    END IF
    IF (get(sczz, 'SECP', n, 'PROP     ', 'SCZZ    ')) THEN
       CALL erhandler(fname, __LINE__, ERH_FATAL, &
            TRIM(libname)//': Determining section SCZZ for section %i failed.', &
            derrinfo, cerrinfo)
    END IF
    IF (get(offset, 'SECP', n, 'PROP     ', 'OFFSET  ')) THEN
       CALL erhandler(fname, __LINE__, ERH_FATAL, &
            TRIM(libname)//': Determining section OFFSET for section %i failed.', &
            derrinfo, cerrinfo)
    END IF
    IF (get(offy, 'SECP', n, 'PROP     ', 'OFFY    ')) THEN
       CALL erhandler(fname, __LINE__, ERH_FATAL, &
            TRIM(libname)//': Determining section OFFY for section %i failed.', &
            derrinfo, cerrinfo)
    END IF
    IF (get(offz, 'SECP', n, 'PROP     ', 'OFFZ    ')) THEN
       CALL erhandler(fname, __LINE__, ERH_FATAL, &
            TRIM(libname)//': Determining section OFFZ for section %i failed.', &
            derrinfo, cerrinfo)
    END IF

    CALL TrackEnd("get_t_beam_common")

  END SUBROUTINE get_t_beam_common

  SUBROUTINE get_t_beam_rect(t_y_1, t_y_2, t_z_1, t_z_2, cgy, cgz, n)

    USE ansys_upf, ONLY : TrackBegin, TrackEnd, erhandler
    USE ansys_par, ONLY : ERH_FATAL
    USE glans
    USE LOCMOD, ONLY : libname

    IMPLICIT NONE

    REAL(KIND=8), INTENT(OUT) :: t_y_1, t_y_2, t_z_1, t_z_2
    REAL(KIND=8), INTENT(IN) :: cgy, cgz
    INTEGER, INTENT(IN) :: n

    CALL TrackBegin("get_t_beam_rect")

    IF (get(t_y_2, 'SECP', n, 'DATA    ', 1)) THEN
       CALL erhandler(fname, __LINE__, ERH_FATAL, &
            TRIM(libname)//': Determining section "DATA,1" for section %i failed.', &
            derrinfo, cerrinfo)
    END IF
    IF (get(t_z_2, 'SECP', n, 'DATA    ', 2)) THEN
       CALL erhandler(fname, __LINE__, ERH_FATAL, &
            TRIM(libname)//': Determining section "DATA,2" for section %i failed.', &
            derrinfo, cerrinfo)
    END IF
    t_y_2 = t_y_2 / 2d0 - cgy
    t_y_1 = t_y_2
    t_z_2 = t_z_2 / 2d0 - cgz
    t_z_1 = t_z_2

    CALL TrackEnd("get_t_beam_rect")

  END SUBROUTINE get_t_beam_rect

  SUBROUTINE get_t_beam_quad(t_y_1, t_y_2, t_z_1, t_z_2, cgy, cgz, n)

    USE ansys_upf, ONLY : TrackBegin, TrackEnd, erhandler
    USE ansys_par, ONLY : ERH_FATAL
    USE glans
    USE LOCMOD, ONLY : libname

    IMPLICIT NONE

    REAL(KIND=8), INTENT(OUT) :: t_y_1, t_y_2, t_z_1, t_z_2
    REAL(KIND=8), INTENT(IN) :: cgy, cgz
    INTEGER, INTENT(IN) :: n

    REAL(KIND=8) :: yI, yJ, yK, yL
    REAL(KIND=8) :: zI, zJ, zK, zL

    CALL TrackBegin("get_t_beam_quad")

    IF (get(yI, 'SECP', n, 'DATA    ', 1)) THEN
       CALL erhandler(fname, __LINE__, ERH_FATAL, &
            TRIM(libname)//': Determining section "DATA,1" for section %i failed.', &
            derrinfo, cerrinfo)
    END IF

    IF (get(zI, 'SECP', n, 'DATA    ', 2)) THEN
       CALL erhandler(fname, __LINE__, ERH_FATAL, &
            TRIM(libname)//': Determining section "DATA,2" for section %i failed.', &
            derrinfo, cerrinfo)
    END IF

    IF (get(yJ, 'SECP', n, 'DATA    ', 3)) THEN
       CALL erhandler(fname, __LINE__, ERH_FATAL, &
            TRIM(libname)//': Determining section "DATA,3" for section %i failed.', &
            derrinfo, cerrinfo)
    END IF

    IF (get(zJ, 'SECP', n, 'DATA    ', 4)) THEN
       CALL erhandler(fname, __LINE__, ERH_FATAL, &
            TRIM(libname)//': Determining section "DATA,4" for section %i failed.', &
            derrinfo, cerrinfo)
    END IF

    IF (get(yK, 'SECP', n, 'DATA    ', 5)) THEN
       CALL erhandler(fname, __LINE__, ERH_FATAL, &
            TRIM(libname)//': Determining section "DATA,5" for section %i failed.', &
            derrinfo, cerrinfo)
    END IF

    IF (get(zK, 'SECP', n, 'DATA    ', 6)) THEN
       CALL erhandler(fname, __LINE__, ERH_FATAL, &
            TRIM(libname)//': Determining section "DATA,6" for section %i failed.', &
            derrinfo, cerrinfo)
    END IF

    IF (get(yL, 'SECP', n, 'DATA    ', 7)) THEN
       CALL erhandler(fname, __LINE__, ERH_FATAL, &
            TRIM(libname)//': Determining section "DATA,7" for section %i failed.', &
            derrinfo, cerrinfo)
    END IF

    IF (get(zL, 'SECP', n, 'DATA    ', 8)) THEN
       CALL erhandler(fname, __LINE__, ERH_FATAL, &
            TRIM(libname)//': Determining section "DATA,8" for section %i failed.', &
            derrinfo, cerrinfo)
    END IF

    t_y_1 = ABS(MIN(yI, yL) - cgy)
    t_y_2 = ABS(MAX(yJ, yK) - cgy)
    t_z_1 = ABS(MIN(zI, zJ) - cgz)
    t_z_2 = ABS(MAX(zK, zL) - cgz)

    CALL TrackEnd("get_t_beam_quad")

  END SUBROUTINE get_t_beam_quad

  SUBROUTINE get_t_beam_csolid(t_y_1, t_y_2, t_z_1, t_z_2, cgy, cgz, n)
    USE ansys_upf, ONLY : TrackBegin, TrackEnd, erhandler
    USE ansys_par, ONLY : ERH_FATAL
    USE glans
    USE LOCMOD, ONLY : libname

    IMPLICIT NONE

    REAL(KIND=8), INTENT(OUT) :: t_y_1, t_y_2, t_z_1, t_z_2
    REAL(KIND=8), INTENT(IN) :: cgy, cgz
    INTEGER, INTENT(IN) :: n

    CALL TrackBegin("get_t_beam_csolid")

    IF (get(t_y_2, 'SECP', n, 'DATA    ', 1)) THEN
       CALL erhandler(fname, __LINE__, ERH_FATAL, &
            TRIM(libname)//': Determining section "DATA,8" for section %i failed.', &
            derrinfo, cerrinfo)
    END IF

    t_y_1 = t_y_2
    t_z_1 = t_y_2
    t_z_2 = t_y_2

    CALL TrackEnd("get_t_beam_csolid")

  END SUBROUTINE get_t_beam_csolid

  SUBROUTINE get_t_beam_ctube(t_y_1, t_y_2, t_z_1, t_z_2, cgy, cgz, n)
    USE ansys_upf, ONLY : TrackBegin, TrackEnd, erhandler
    USE ansys_par, ONLY : ERH_FATAL
    USE glans
    USE LOCMOD, ONLY : libname

    IMPLICIT NONE

    REAL(KIND=8), INTENT(OUT) :: t_y_1, t_y_2, t_z_1, t_z_2
    REAL(KIND=8), INTENT(IN) :: cgy, cgz
    INTEGER, INTENT(IN) :: n

    CALL TrackBegin("get_t_beam_ctube")

    IF (get(t_y_2, 'SECP', n, 'DATA    ', 2)) THEN
       CALL erhandler(fname, __LINE__, ERH_FATAL, &
            TRIM(libname)//': Determining section "DATA,2" for section %i failed.', &
            derrinfo, cerrinfo)
    END IF

    t_y_1 = t_y_2
    t_z_1 = t_y_2
    t_z_2 = t_y_2

    CALL TrackEnd("get_t_beam_ctube")

  END SUBROUTINE get_t_beam_ctube

  SUBROUTINE get_t_beam_chan(t_y_1, t_y_2, t_z_1, t_z_2, cgy, cgz, n)
    USE ansys_upf, ONLY : TrackBegin, TrackEnd, erhandler
    USE ansys_par, ONLY : ERH_FATAL
    USE glans
    USE LOCMOD, ONLY : libname

    IMPLICIT NONE

    REAL(KIND=8), INTENT(OUT) :: t_y_1, t_y_2, t_z_1, t_z_2
    REAL(KIND=8), INTENT(IN) :: cgy, cgz
    INTEGER, INTENT(IN) :: n

    REAL(KIND=8) :: W1, W2, W3

    CALL TrackBegin("get_t_beam_chan")

    t_y_1 = 0d0
    t_y_2 = 0d0
    t_z_1 = 0d0
    t_z_2 = 0d0

!     IF (get(W1, 'SECP', n, 'DATA    ', 1)) THEN
!        CALL erhandler(fname, __LINE__, ERH_FATAL, &
!             TRIM(libname)//': Determining section "DATA,1" for section %i failed.', &
!             derrinfo, cerrinfo)
!     END IF

!     IF (get(W2, 'SECP', n, 'DATA    ', 2)) THEN
!        CALL erhandler(fname, __LINE__, ERH_FATAL, &
!             TRIM(libname)//': Determining section "DATA,2" for section %i failed.', &
!             derrinfo, cerrinfo)
!     END IF

!     IF (get(W3, 'SECP', n, 'DATA    ', 3)) THEN
!        CALL erhandler(fname, __LINE__, ERH_FATAL, &
!             TRIM(libname)//': Determining section "DATA,3" for section %i failed.', &
!             derrinfo, cerrinfo)
!     END IF

! ! structure|id|name|A              | A             | A             | I             | I             | I             | e             | e             | e             | e             | e             | e             | shear_center  | shear_center  | description|geometry|Iyz
! ! 1        | 5|    | 1.09500000E-04| 4.13425961E-05| 3.90054501E-05| 9.15190000E-12| 4.31420000E-08| 1.89860000E-07| 1.32044900E-10| 8.37894255E-11|-2.35220000E-02| 4.57870000E-02|-5.64540000E-02| 5.27710000E-02| 0.00000000E+00| 0.00000000E+00| 0          | 0      | 0.00000000E+00
! ! 1        | 5|    | 1.09500000E-04| 4.07510757E-05| 3.95980963E-05| 9.15191962E-12| 4.63288680E-08| 1.86673794E-07| 1.83038392E-10| 9.15191962E-11| 0.00000000E+00| 5.00000000E-02| 0.00000000E+00| 1.00000000E-01| 0.00000000E+00| 0.00000000E+00| 0          | 0      | 0.00000000E+00

!     t_y_1 = MAX(W1, W2)
!     t_y_2 = 0d0
!     t_z_1 = W3
!     t_z_2 = 0d0

    CALL TrackEnd("get_t_beam_chan")

  END SUBROUTINE get_t_beam_chan

  SUBROUTINE get_t_beam_i(t_y_1, t_y_2, t_z_1, t_z_2, cgy, cgz, n)
    USE ansys_upf, ONLY : TrackBegin, TrackEnd, erhandler
    USE ansys_par, ONLY : ERH_FATAL
    USE glans
    USE LOCMOD, ONLY : libname

    IMPLICIT NONE

    REAL(KIND=8), INTENT(OUT) :: t_y_1, t_y_2, t_z_1, t_z_2
    REAL(KIND=8), INTENT(IN) :: cgy, cgz
    INTEGER, INTENT(IN) :: n

    CALL TrackBegin("get_t_beam_i")

    t_y_1 = 0d0
    t_y_2 = 0d0
    t_z_1 = 0d0
    t_z_2 = 0d0

    CALL TrackEnd("get_t_beam_i")

  END SUBROUTINE get_t_beam_i

  SUBROUTINE get_t_beam_z(t_y_1, t_y_2, t_z_1, t_z_2, cgy, cgz, n)
    USE ansys_upf, ONLY : TrackBegin, TrackEnd, erhandler
    USE ansys_par, ONLY : ERH_FATAL
    USE glans
    USE LOCMOD, ONLY : libname

    IMPLICIT NONE

    REAL(KIND=8), INTENT(OUT) :: t_y_1, t_y_2, t_z_1, t_z_2
    REAL(KIND=8), INTENT(IN) :: cgy, cgz
    INTEGER, INTENT(IN) :: n

    CALL TrackBegin("get_t_beam_z")

    t_y_1 = 0d0
    t_y_2 = 0d0
    t_z_1 = 0d0
    t_z_2 = 0d0

    CALL TrackEnd("get_t_beam_z")

  END SUBROUTINE get_t_beam_z

  SUBROUTINE get_t_beam_l(t_y_1, t_y_2, t_z_1, t_z_2, cgy, cgz, n)
    USE ansys_upf, ONLY : TrackBegin, TrackEnd, erhandler
    USE ansys_par, ONLY : ERH_FATAL
    USE glans
    USE LOCMOD, ONLY : libname

    IMPLICIT NONE

    REAL(KIND=8), INTENT(OUT) :: t_y_1, t_y_2, t_z_1, t_z_2
    REAL(KIND=8), INTENT(IN) :: cgy, cgz
    INTEGER, INTENT(IN) :: n

    CALL TrackBegin("get_t_beam_l")

    t_y_1 = 0d0
    t_y_2 = 0d0
    t_z_1 = 0d0
    t_z_2 = 0d0

    CALL TrackEnd("get_t_beam_l")

  END SUBROUTINE get_t_beam_l

  SUBROUTINE get_t_beam_t(t_y_1, t_y_2, t_z_1, t_z_2, cgy, cgz, n)
    USE ansys_upf, ONLY : TrackBegin, TrackEnd, erhandler
    USE ansys_par, ONLY : ERH_FATAL
    USE glans
    USE LOCMOD, ONLY : libname

    IMPLICIT NONE

    REAL(KIND=8), INTENT(OUT) :: t_y_1, t_y_2, t_z_1, t_z_2
    REAL(KIND=8), INTENT(IN) :: cgy, cgz
    INTEGER, INTENT(IN) :: n

    CALL TrackBegin("get_t_beam_t")

    t_y_1 = 0d0
    t_y_2 = 0d0
    t_z_1 = 0d0
    t_z_2 = 0d0

    CALL TrackEnd("get_t_beam_t")

  END SUBROUTINE get_t_beam_t

  SUBROUTINE get_t_beam_hats(t_y_1, t_y_2, t_z_1, t_z_2, cgy, cgz, n)
    USE ansys_upf, ONLY : TrackBegin, TrackEnd, erhandler
    USE ansys_par, ONLY : ERH_FATAL
    USE glans
    USE LOCMOD, ONLY : libname

    IMPLICIT NONE

    REAL(KIND=8), INTENT(OUT) :: t_y_1, t_y_2, t_z_1, t_z_2
    REAL(KIND=8), INTENT(IN) :: cgy, cgz
    INTEGER, INTENT(IN) :: n

    CALL TrackBegin("get_t_beam_hats")

    t_y_1 = 0d0
    t_y_2 = 0d0
    t_z_1 = 0d0
    t_z_2 = 0d0

    CALL TrackEnd("get_t_beam_hats")

  END SUBROUTINE get_t_beam_hats

  SUBROUTINE get_t_beam_hrec(t_y_1, t_y_2, t_z_1, t_z_2, cgy, cgz, n)
    USE ansys_upf, ONLY : TrackBegin, TrackEnd, erhandler
    USE ansys_par, ONLY : ERH_FATAL
    USE glans
    USE LOCMOD, ONLY : libname

    IMPLICIT NONE

    REAL(KIND=8), INTENT(OUT) :: t_y_1, t_y_2, t_z_1, t_z_2
    REAL(KIND=8), INTENT(IN) :: cgy, cgz
    INTEGER, INTENT(IN) :: n

    CALL TrackBegin("get_t_beam_hrec")

    t_y_1 = 0d0
    t_y_2 = 0d0
    t_z_1 = 0d0
    t_z_2 = 0d0

    CALL TrackEnd("get_t_beam_hrec")

  END SUBROUTINE get_t_beam_hrec

  SUBROUTINE get_t_beam_asec(t_y_1, t_y_2, t_z_1, t_z_2, cgy, cgz, n)
    USE ansys_upf, ONLY : TrackBegin, TrackEnd, erhandler
    USE ansys_par, ONLY : ERH_FATAL
    USE glans
    USE LOCMOD, ONLY : libname

    IMPLICIT NONE

    REAL(KIND=8), INTENT(OUT) :: t_y_1, t_y_2, t_z_1, t_z_2
    REAL(KIND=8), INTENT(IN) :: cgy, cgz
    INTEGER, INTENT(IN) :: n

    CALL TrackBegin("get_t_beam_asec")

    t_y_1 = 0d0
    t_y_2 = 0d0
    t_z_1 = 0d0
    t_z_2 = 0d0

    CALL TrackEnd("get_t_beam_asec")

  END SUBROUTINE get_t_beam_asec

  SUBROUTINE get_t_beam_mesh(t_y_1, t_y_2, t_z_1, t_z_2, cgy, cgz, n)
    USE ansys_upf, ONLY : TrackBegin, TrackEnd, erhandler
    USE ansys_par, ONLY : ERH_FATAL
    USE glans
    USE LOCMOD, ONLY : libname

    IMPLICIT NONE

    REAL(KIND=8), INTENT(OUT) :: t_y_1, t_y_2, t_z_1, t_z_2
    REAL(KIND=8), INTENT(IN) :: cgy, cgz
    INTEGER, INTENT(IN) :: n

    CALL TrackBegin("get_t_beam_mesh")

    t_y_1 = 0d0
    t_y_2 = 0d0
    t_z_1 = 0d0
    t_z_2 = 0d0

    CALL TrackEnd("get_t_beam_mesh")

  END SUBROUTINE get_t_beam_mesh

END MODULE mod_dat_beamproperties

! Local Variables:
! compile-command:"make -C .. test"
! End:
