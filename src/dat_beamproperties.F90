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

    USE ansys_upf, ONLY : TrackBegin, TrackEnd

    USE ansys_par, ONLY : STRING_MAX_LENG, ERH_FATAL
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
    TYPE(cross_section_info) :: section_info
    ! REAL(KIND=8) :: t_max, t_min, t_y_1, t_y_2, t_z_1, t_z_2

    REAL(KIND=8) :: yI, zI, yJ, zJ, yK, zK, yL, zL

    INTEGER :: iErr
    LOGICAL :: err

    CALL TrackBegin("dat_beamp_sec")

    ! Loop over all sections
    err = s_get(fname, libname, __LINE__, ERH_FATAL, &
         & n_sect, 'SECP', 0, 'NUM     ', 'MAX     ')
    derrinfo(1) = n_sect
    CALL ans_note(fname, __LINE__, libname, &
         & '   no. of section definitions: %d')

    DO n = 1, n_sect
       err = s_get(fname, libname, __LINE__, ERH_NOTE, &
            & sec_type, 'SECP', n, 'TYPE    ')

       IF (.NOT. err .AND. sec_type.eq.'BEAM') THEN

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

          section_info%t_y_1 = 0d0
          section_info%t_y_2 = 0d0
          section_info%t_z_1 = 0d0
          section_info%t_z_2 = 0d0

          CALL get_t_beam_common(n, name, area, iyy, iyz, izz, warp_val, tors, &
               cgy, cgz, shcy, shcz, scyy, scyz, sczz, offset, offy, offz)

          bp%data(bp%num)%name(:) = name(1:16)
          bp%data(bp%num)%A(:) = (/ area, area * scyy, area * sczz /)

          bp%data(bp%num)%i(:) = (/ tors, izz, iyy /)

          bp%data(bp%num)%iyz = iyz

          bp%cs_info(bp%snum) = section_info

          err = s_get(fname, libname, __LINE__, ERH_FATAL, &
               & sec_subtype, 'SECP', n, 'SUBTYPE ')

          SELECT CASE(sec_subtype)
          CASE('RECT')
             CALL get_t_beam_rect(section_info, cgy, cgz, n)
          CASE('QUAD')
             CALL get_t_beam_quad(section_info, cgy, cgz, n)
          CASE('CSOL')
             CALL get_t_beam_csolid(section_info, cgy, cgz, n)
          CASE('CTUB')
             CALL get_t_beam_ctube(section_info, cgy, cgz, n)
          CASE('CHAN')
             CALL get_t_beam_chan(section_info, cgy, cgz, n)
          CASE('I')
             CALL get_t_beam_i(section_info, cgy, cgz, n)
          CASE('Z')
             CALL get_t_beam_z(section_info, cgy, cgz, n)
          CASE('L')
             CALL get_t_beam_l(section_info, cgy, cgz, n)
          CASE('T')
             CALL get_t_beam_t(section_info, cgy, cgz, n)
          CASE('HATS')
             CALL get_t_beam_hats(section_info, cgy, cgz, n)
          CASE('HREC')
             CALL get_t_beam_hrec(section_info, cgy, cgz, n)
          CASE('ASEC')
             CALL get_t_beam_asec(section_info, cgy, cgz, n)
          CASE('MESH')
             CALL get_t_beam_mesh(section_info, cgy, cgz, n)
          CASE DEFAULT
             cerrinfo(1) = sec_subtype
             CALL ans_fatal(fname, __LINE__, libname, &
                  & ' Section SUBTYPE "%s" for section %d not supported.')
          END SELECT

          IF (section_info%t_max .NE. 0d0) THEN
             bp%data(bp%num)%e(1) = tors / section_info%t_max
          END IF
          IF (section_info%t_min .NE. 0d0) THEN
             bp%data(bp%num)%e(2) = tors / section_info%t_min
          END IF

          bp%data(bp%num)%e(3:6) = (/ &
             section_info%t_y_1, section_info%t_y_2, &
             section_info%t_z_1, section_info%t_z_2 /)

          bp%data(bp%num)%d(:) = (/ 0d0, cgy - offy, cgz - offz /)

          bp%data(bp%num)%sc(:) = (/ shcz, shcy /)

       END IF

    END DO

    CALL TrackEnd("dat_beamp_sec")

  END SUBROUTINE dat_beamproperties_sec

  SUBROUTINE dat_beamproperties_rc(bp)

    USE ansys_upf, ONLY : TrackBegin, TrackEnd, rlget, &
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
    CALL ans_note(fname, __LINE__, libname, '   no. of BEAM44 rconst.: %d')
    derrinfo(1) = rmax
    CALL ans_note(fname, __LINE__, libname, '   no.of rconsts to test: %d')

    DO n = 1, rmax

       ! test if real constant set is selected (i.e. if it belongs to a
       ! BEAM44 element)
       IF (rlinqr(n, DB_SELECTED).EQ.1) THEN
          ! read real constant set from ANSYS and convert BEAM44 properties in
          ! beam properties
          CALL vzero(rtable, 40)
          i  = rlget(n, rtable)

          i_vals%e(:) =  0d0
          j_vals%e(:) =  0d0


          i_vals%A(1) = rtable( 1)               !       AREA1
          i_vals%I(2) = rtable( 2)               !       IZ1
          i_vals%I(3) = rtable( 3)               !       IY1
          i_vals%e(5) = -rtable( 4)              !  TKZB1 = ey2, i
          i_vals%e(3) = -rtable( 5)              !  TKYB1 = ez1, i
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
             j_vals%e(5) = i_vals%e(5)           !       TKZB2
          ELSE
             j_vals%e(5) = -rtable(10)           !       TKZB2
          END IF
          IF (rtable(11).EQ.0d0) THEN
             j_vals%e(3) = i_vals%e(3)           !       TKYB2
          ELSE
             j_vals%e(3) = -rtable(11)           !       TKYB2
          END IF
          IF (rtable(12).EQ.0d0) THEN
             j_vals%I(1) = i_vals%I(1)           !       IX2
          ELSE
             j_vals%I(1) = rtable(12)            !       IX2
          END IF

          i_vals%d(1) = rtable(13)               !       DX1
          i_vals%d(2) = -rtable(14)              !       DY1
          i_vals%d(3) = rtable(15)               !       DZ1

          j_vals%d(1) = rtable(16)               !       DX2
          j_vals%d(2) = -rtable(17)              !       DY2
          j_vals%d(3) = rtable(18)               !       DZ2

          IF (rtable(26).NE.0d0) THEN
             i_vals%A(2) =  rtable(26)           !       ARESY1
          ELSE
             i_vals%A(2) = i_vals%A(1)*rtable(20)!      A1/SHEARY
          END IF
          IF (rtable(25).NE.0d0) THEN
             i_vals%A(3) =  rtable(25)           !       ARESZ1
          ELSE
             i_vals%A(3) = i_vals%A(1)*rtable(19)!      A1/SHEARZ
          END IF

          IF (rtable(28).NE.0d0) THEN
             j_vals%A(2) =  rtable(28)           !       ARESY2
          ELSE IF (rtable(26).NE.0d0) THEN
             j_vals%A(2) =  rtable(26)           !       ARESY1
          ELSE
             j_vals%A(2) = j_vals%A(1)*rtable(20)!      A2/SHEARY
          END IF
          IF (rtable(27).NE.0d0) THEN
             j_vals%A(3) =  rtable(27)           !       ARESZ2
          ELSE IF (rtable(25).NE.0d0) THEN
             j_vals%A(3) =  rtable(25)           !       ARESZ1
          ELSE
             j_vals%A(3) = j_vals%A(1)*rtable(19)!      A2/SHEARZ
          END IF

          i_vals%e(6) =  rtable(21)              !       TKZT1
          i_vals%e(4) =  rtable(22)              !       TKYT1

          IF (rtable(23) .EQ. 0d0) THEN
             j_vals%e(6) = i_vals%e(6)           !       TKZT2
          ELSE
             j_vals%e(6) =  rtable(23)           !       TKZT2
          END IF
          IF (rtable(24) .EQ. 0d0) THEN
             j_vals%e(4) = i_vals%e(4)           !       TKYT2
          ELSE
             j_vals%e(4) = rtable(24)            !       TKYT2
          END IF

          ! --- not used ---         rtable(29)              !       TSF1
          ! --- not used ---         rtable(30)              !       TSF2

          theta = -deg2rad(rtable(53))

          Iyy = 5d-1 * (i_vals%I(2) + i_vals%I(3)) + 5d-1 * ( i_vals%I(2) - i_vals%I(3)) * COS(2d0 * theta)
          Izz = 5d-1 * (i_vals%I(2) + i_vals%I(3)) - 5d-1 * ( i_vals%I(2) - i_vals%I(3)) * COS(2d0 * theta)
          Iyz = 5d-1 * (i_vals%I(2) - i_vals%I(3)) * SIN(2d0 * theta)

          Iyy = i_vals%I(2)
          Izz = i_vals%I(3)
          Iyz = 0d0

          i_vals%I(2) = Iyy
          i_vals%I(3) = Izz
          i_vals%Iyz = Iyz

          ! Iyy = 5d-1 * (j_vals%I(2) + j_vals%I(3)) + 5d-1 * (j_vals%I(2) - j_vals%I(3)) * COS(2d0 * theta)
          ! Izz = 5d-1 * (j_vals%I(2) + j_vals%I(3)) - 5d-1 * (j_vals%I(2) - j_vals%I(3)) * COS(2d0 * theta)
          ! !Iyz = 5d-1 * (j_vals%I(2) - j_vals%I(3)) * SIN(2d0 * theta)

          j_vals%I(2) = Iyy
          j_vals%I(3) = Izz
          j_vals%Iyz = Iyz

          i_vals%theta = theta
          j_vals%theta = theta

          i_vals%sc(:) = (/ rtable(31), rtable(32) /)
          IF (rtable(34) .NE. 0d0) THEN
             j_vals%sc(1) = rtable(33)
          else
             j_vals%sc(1) = rtable(31)
          END IF
          IF (rtable(33) .NE. 0d0) THEN
             j_vals%sc(2) = rtable(34)
          ELSE
             j_vals%sc(2) = rtable(32)
          END IF

          t_min = MIN( &
               i_vals%e(4) - i_vals%e(3), i_vals%e(6) - i_vals%e(5))
          t_max = MAX( &
               i_vals%e(4) - i_vals%e(3), i_vals%e(6) - i_vals%e(5))
          IF (t_min.NE.0d0) THEN
             i_vals%e(1) = i_vals%I(1) / t_min
          END IF
          IF (t_max.NE.0d0) THEN
             i_vals%e(2) = i_vals%I(1) / t_max
          END IF

          t_min = MIN( &
               j_vals%e(4) - j_vals%e(3), j_vals%e(6) - j_vals%e(5))
          t_max = MAX( &
               j_vals%e(4) - j_vals%e(3), j_vals%e(6) - j_vals%e(5))
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
    CALL ans_note(fname, __LINE__, libname, '   no.beam prop. generated: %i')

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

    USE ansys_upf, ONLY : TrackBegin, TrackEnd
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

    LOGICAL :: err

    CALL TrackBegin("get_t_beam_common")

    derrinfo(1) = n

    err = s_get(fname, libname, __LINE__, ERH_FATAL, name, 'SECP', n, 'NAME     ')
    err = s_get(fname, libname, __LINE__, ERH_FATAL, area, 'SECP', n, 'PROP     ', 'AREA    ')
    err = s_get(fname, libname, __LINE__, ERH_FATAL, iyy, 'SECP', n, 'PROP     ', 'IYY     ')
    err = s_get(fname, libname, __LINE__, ERH_FATAL, iyz, 'SECP', n, 'PROP     ', 'IYZ     ')
    err = s_get(fname, libname, __LINE__, ERH_FATAL, izz, 'SECP', n, 'PROP     ', 'IZZ     ')
    err = s_get(fname, libname, __LINE__, ERH_FATAL, warp_val, 'SECP', n, 'PROP     ', 'WARP    ')
    err = s_get(fname, libname, __LINE__, ERH_FATAL, tors, 'SECP', n, 'PROP     ', 'TORS    ')
    err = s_get(fname, libname, __LINE__, ERH_FATAL, cgy, 'SECP', n, 'PROP     ', 'CGY     ')
    err = s_get(fname, libname, __LINE__, ERH_FATAL, cgz, 'SECP', n, 'PROP     ', 'CGZ     ')
    err = s_get(fname, libname, __LINE__, ERH_FATAL, shcy, 'SECP', n, 'PROP     ', 'SHCY    ')
    err = s_get(fname, libname, __LINE__, ERH_FATAL, shcz, 'SECP', n, 'PROP     ', 'SHCZ    ')
    err = s_get(fname, libname, __LINE__, ERH_FATAL, scyy, 'SECP', n, 'PROP     ', 'SCYY    ')
    err = s_get(fname, libname, __LINE__, ERH_FATAL, scyz, 'SECP', n, 'PROP     ', 'SCYZ    ')
    err = s_get(fname, libname, __LINE__, ERH_FATAL, sczz, 'SECP', n, 'PROP     ', 'SCZZ    ')
    err = s_get(fname, libname, __LINE__, ERH_FATAL, offset, 'SECP', n, 'PROP     ', 'OFFSET  ')
    err = s_get(fname, libname, __LINE__, ERH_FATAL, offy, 'SECP', n, 'PROP     ', 'OFFY    ')
    err = s_get(fname, libname, __LINE__, ERH_FATAL, offz, 'SECP', n, 'PROP     ', 'OFFZ    ')

    CALL TrackEnd("get_t_beam_common")

  END SUBROUTINE get_t_beam_common

  SUBROUTINE get_t_beam_rect(section_info, cgy, cgz, n)

    USE ansys_upf, ONLY : TrackBegin, TrackEnd
    USE ansys_par, ONLY : ERH_FATAL
    USE glans
    USE LOCMOD, ONLY : libname
    USE ans_common, ONLY : cross_section_info, CS_TYPE_RECT

    IMPLICIT NONE

    TYPE(cross_section_info), INTENT(OUT) :: section_info
    REAL(KIND=8), INTENT(IN) :: cgy, cgz
    INTEGER, INTENT(IN) :: n

    INTEGER :: i

    LOGICAL :: err

    CALL TrackBegin("get_t_beam_rect")

    derrinfo(1) = n

    ALLOCATE(section_info%data(4))
    DO i = 1, 4
       err = s_get(fname, libname, __LINE__, ERH_FATAL, &
              & section_info%data(i), 'SECP', n, 'DATA    ', i)
    END DO

    section_info%t_y_2 = section_info%data(1)
    section_info%t_z_2 = section_info%data(2)

    section_info%t_max = max(section_info%t_y_1, section_info%t_z_1)
    section_info%t_min = min(section_info%t_y_1, section_info%t_z_1)
    section_info%t_y_2 = section_info%t_y_2 / 2d0
    section_info%t_y_1 = section_info%t_y_2
    section_info%t_z_2 = section_info%t_z_2 / 2d0
    section_info%t_z_1 = section_info%t_z_2

    section_info%web_height = section_info%t_z_2
    section_info%web_thickness = section_info%t_y_2

    section_info%type_code = CS_TYPE_RECT

    CALL TrackEnd("get_t_beam_rect")

  END SUBROUTINE get_t_beam_rect

  SUBROUTINE get_t_beam_quad(section_info, cgy, cgz, n)

    USE ansys_upf, ONLY : TrackBegin, TrackEnd
    USE ansys_par, ONLY : ERH_FATAL
    USE glans
    USE LOCMOD, ONLY : libname
    USE ans_common, ONLY : cross_section_info, CS_TYPE_QUAD

    IMPLICIT NONE

    TYPE(cross_section_info), INTENT(OUT) :: section_info
    REAL(KIND=8), INTENT(IN) :: cgy, cgz
    INTEGER, INTENT(IN) :: n

    REAL(KIND=8), DIMENSION(4) :: y
    REAL(KIND=8), DIMENSION(4) :: z
    INTEGER :: i
    LOGICAL :: err

    CALL TrackBegin("get_t_beam_quad")

    derrinfo(2) = n

    ALLOCATE(section_info%data(10))
    DO i = 1, 10
       err = s_get(fname, libname, __LINE__, ERH_FATAL, &
            & section_info%data(i), 'SECP', n, 'DATA    ', i)
    END DO

    DO i = 1, 4
       y(i) = section_info%data(2*i-1)
       z(i) = section_info%data(2*i)
    END DO

    section_info%t_min = MIN(-y(1) + y(2), -y(4) + y(3), -z(1) + z(4), -z(2) + z(3))
    section_info%t_max = MAX(-y(1) + y(2), -y(4) + y(3), -z(1) + z(4), -z(2) + z(3))

    section_info%t_y_1 = ABS(MIN(y(1), y(4)) - cgy)
    section_info%t_y_2 = ABS(MAX(y(2), y(3)) - cgy)
    section_info%t_z_1 = ABS(MIN(z(1), z(2)) - cgz)
    section_info%t_z_2 = ABS(MAX(z(3), z(4)) - cgz)

    section_info%type_code = CS_TYPE_QUAD

    CALL TrackEnd("get_t_beam_quad")

  END SUBROUTINE get_t_beam_quad

  SUBROUTINE get_t_beam_csolid(section_info, cgy, cgz, n)
    USE ansys_upf, ONLY : TrackBegin, TrackEnd
    USE ansys_par, ONLY : ERH_FATAL
    USE glans
    USE LOCMOD, ONLY : libname
    USE ans_common, ONLY : cross_section_info, CS_TYPE_CSOLID

    IMPLICIT NONE

    TYPE(cross_section_info), INTENT(OUT) :: section_info
    REAL(KIND=8), INTENT(IN) :: cgy, cgz
    INTEGER, INTENT(IN) :: n

    LOGICAL :: err
    INTEGER :: i

    CALL TrackBegin("get_t_beam_csolid")

    derrinfo(1) = n

    ALLOCATE(section_info%data(3))
    DO i = 1, 3
       err = s_get(fname, libname, __LINE__, ERH_FATAL, &
              & section_info%data(i), 'SECP', n, 'DATA    ', i)
    END DO

    section_info%t_y_2 = section_info%data(1)

    section_info%t_max = section_info%t_y_2 * 2.
    section_info%t_min = section_info%t_max
    section_info%t_y_1 = section_info%t_y_2
    section_info%t_z_1 = section_info%t_y_2
    section_info%t_z_2 = section_info%t_y_2

    section_info%type_code = CS_TYPE_CSOLID

    CALL TrackEnd("get_t_beam_csolid")

  END SUBROUTINE get_t_beam_csolid

  SUBROUTINE get_t_beam_ctube(section_info, cgy, cgz, n)
    USE ansys_upf, ONLY : TrackBegin, TrackEnd
    USE ansys_par, ONLY : ERH_FATAL
    USE glans
    USE LOCMOD, ONLY : libname
    USE ans_common, ONLY : cross_section_info, CS_TYPE_CTUBE

    IMPLICIT NONE

    TYPE(cross_section_info), INTENT(OUT) :: section_info
    REAL(KIND=8), INTENT(IN) :: cgy, cgz
    INTEGER, INTENT(IN) :: n

    REAL(KIND=8) :: r_i
    LOGICAL :: err
    INTEGER :: i

    CALL TrackBegin("get_t_beam_ctube")

    derrinfo(1) = n

    ALLOCATE(section_info%data(3))
    DO i = 1, 3
       err = s_get(fname, libname, __LINE__, ERH_FATAL, &
              & section_info%data(i), 'SECP', n, 'DATA    ', i)
    END DO

    r_i = section_info%data(1)
    section_info%t_y_2 = section_info%data(2)

    section_info%t_max = section_info%t_y_2 - r_i
    section_info%t_min = section_info%t_max
    section_info%t_y_1 = section_info%t_y_2
    section_info%t_z_1 = section_info%t_y_2
    section_info%t_z_2 = section_info%t_y_2

    section_info%type_code = CS_TYPE_CTUBE

    CALL TrackEnd("get_t_beam_ctube")

  END SUBROUTINE get_t_beam_ctube

  SUBROUTINE get_t_beam_chan(section_info, cgy, cgz, n)
    USE ansys_upf, ONLY : TrackBegin, TrackEnd
    USE ansys_par, ONLY : ERH_FATAL
    USE glans
    USE LOCMOD, ONLY : libname
    USE ans_common, ONLY : cross_section_info, CS_TYPE_CHAN

    IMPLICIT NONE

    TYPE(cross_section_info), INTENT(OUT) :: section_info
    REAL(KIND=8), INTENT(IN) :: cgy, cgz
    INTEGER, INTENT(IN) :: n

    REAL(KIND=8), dimension(3) :: w
    REAL(KIND=8), dimension(3) :: t
    INTEGER :: i
    LOGICAL :: err

    CALL TrackBegin("get_t_beam_chan")

    derrinfo(2) = n

    ALLOCATE(section_info%data(6))
    DO i = 1, 6
       err = s_get(fname, libname, __LINE__, ERH_FATAL, &
              & section_info%data(i), 'SECP', n, 'DATA    ', i)
    END DO

    DO i = 1, 3
       w(i) = section_info%data(i)
       t(i) = section_info%data(i+3)
    END DO

    section_info%t_min = MIN(t(1), t(2), t(3))
    section_info%t_max = MAX(t(1), t(2), t(3))
    section_info%t_y_1 = - cgy
    section_info%t_y_2 = MAX(W(1), W(2)) - cgy
    section_info%t_z_1 = - cgz
    section_info%t_z_2 = W(3) - cgz

    section_info%type_code = CS_TYPE_CHAN

    CALL TrackEnd("get_t_beam_chan")

  END SUBROUTINE get_t_beam_chan

  SUBROUTINE get_t_beam_i(section_info, cgy, cgz, n)
    USE ansys_upf, ONLY : TrackBegin, TrackEnd
    USE ansys_par, ONLY : ERH_FATAL
    USE glans
    USE LOCMOD, ONLY : libname
    USE ans_common, ONLY : cross_section_info, CS_TYPE_I

    IMPLICIT NONE

    TYPE(cross_section_info), INTENT(OUT) :: section_info
    REAL(KIND=8), INTENT(IN) :: cgy, cgz
    INTEGER, INTENT(IN) :: n

    REAL(KIND=8), DIMENSION(3) :: w
    REAL(KIND=8), DIMENSION(3) :: t
    INTEGER :: i
    LOGICAL :: err

    CALL TrackBegin("get_t_beam_i")

    derrinfo(2) = n

    ALLOCATE(section_info%data(6))
    DO i = 1, 6
       err = s_get(fname, libname, __LINE__, ERH_FATAL, &
              & section_info%data(i), 'SECP', n, 'DATA    ', i)
    END DO

    DO i = 1, 3
       w(i) = section_info%data(i)
       t(i) = section_info%data(i+3)
    END DO

    section_info%t_min = MIN(t(1), t(2), t(3))
    section_info%t_max = MAX(t(1), t(2), t(3))
    section_info%t_y_2 = MAX(w(1), w(2)) / 2d0 - cgy
    section_info%t_y_1 = -section_info%t_y_2
    section_info%t_z_1 = -cgz
    section_info%t_z_2 = W(3) - cgz

    section_info%type_code = CS_TYPE_I

    CALL TrackEnd("get_t_beam_i")

  END SUBROUTINE get_t_beam_i

  SUBROUTINE get_t_beam_z(section_info, cgy, cgz, n)
    USE ansys_upf, ONLY : TrackBegin, TrackEnd
    USE ansys_par, ONLY : ERH_FATAL
    USE glans
    USE LOCMOD, ONLY : libname
    USE ans_common, ONLY : cross_section_info, CS_TYPE_Z

    IMPLICIT NONE

    TYPE(cross_section_info), INTENT(OUT) :: section_info
    REAL(KIND=8), INTENT(IN) :: cgy, cgz
    INTEGER, INTENT(IN) :: n

    REAL(KIND=8), DIMENSION(3) :: w
    REAL(KIND=8), DIMENSION(3) :: t
    INTEGER :: i
    LOGICAL :: err

    CALL TrackBegin("get_t_beam_z")

    derrinfo(2) = n

    ALLOCATE(section_info%data(6))
    DO i = 1, 6
       err = s_get(fname, libname, __LINE__, ERH_FATAL, &
              & section_info%data(i), 'SECP', n, 'DATA    ', i)
    END DO

    DO i = 1, 3
       w(i) = section_info%data(i)
       t(i) = section_info%data(i+3)
    END DO

    section_info%t_min = MIN(t(1), t(2), t(3))
    section_info%t_max = MAX(t(1), t(2), t(3))
    section_info%t_y_2 = w(2) - cgy
    section_info%t_y_1 = w(1) - cgy
    section_info%t_z_1 = - cgz
    section_info%t_z_2 = W(3) - cgz

    section_info%type_code = CS_TYPE_Z

    CALL TrackEnd("get_t_beam_z")

  END SUBROUTINE get_t_beam_z

  SUBROUTINE get_t_beam_l(section_info, cgy, cgz, n)
    USE ansys_upf, ONLY : TrackBegin, TrackEnd
    USE ansys_par, ONLY : ERH_FATAL
    USE glans
    USE LOCMOD, ONLY : libname
    USE ans_common, ONLY : cross_section_info, CS_TYPE_RECT

    IMPLICIT NONE

    TYPE(cross_section_info), INTENT(OUT) :: section_info
    REAL(KIND=8), INTENT(IN) :: cgy, cgz
    INTEGER, INTENT(IN) :: n

    REAL(KIND=8), DIMENSION(2) :: w
    REAL(KIND=8), DIMENSION(2) :: t
    INTEGER :: i
    LOGICAL :: err

    CALL TrackBegin("get_t_beam_l")

    derrinfo(2) = n

    ALLOCATE(section_info%data(4))
    DO i = 1, 4
       err = s_get(fname, libname, __LINE__, ERH_FATAL, &
              & section_info%data(i), 'SECP', n, 'DATA    ', i)
    END DO

    DO i = 1, 2
       w(i) = section_info%data(i)
       t(i) = section_info%data(i+2)
    END DO

    section_info%t_min = MIN(t(1), t(2))
    section_info%t_max = MAX(t(1), t(2))
    section_info%t_y_1 = - cgy
    section_info%t_y_2 = w(1) - cgy
    section_info%t_z_1 = - cgz
    section_info%t_z_2 = W(2) - cgz

    section_info%web_height = w(2)
    section_info%web_thickness = t(2)
    section_info%flange_width = w(1)
    section_info%flange_thickness = t(1)

    section_info%type_code = CS_TYPE_RECT

    CALL TrackEnd("get_t_beam_l")

  END SUBROUTINE get_t_beam_l

  SUBROUTINE get_t_beam_t(section_info, cgy, cgz, n)
    USE ansys_upf, ONLY : TrackBegin, TrackEnd
    USE ansys_par, ONLY : ERH_FATAL
    USE glans
    USE LOCMOD, ONLY : libname
    USE ans_common, ONLY : cross_section_info, CS_TYPE_T

    IMPLICIT NONE

    TYPE(cross_section_info), INTENT(OUT) :: section_info
    REAL(KIND=8), INTENT(IN) :: cgy, cgz
    INTEGER, INTENT(IN) :: n

    REAL(KIND=8), DIMENSION(2) :: w
    REAL(KIND=8), DIMENSION(2) :: t
    INTEGER :: i
    LOGICAL :: err

    CALL TrackBegin("get_t_beam_t")

    derrinfo(2) = n

    ALLOCATE(section_info%data(4))
    DO i = 1, 4
       err = s_get(fname, libname, __LINE__, ERH_FATAL, &
              & section_info%data(i), 'SECP', n, 'DATA    ', i)
    END DO

    DO i = 1, 2
       w(i) = section_info%data(i)
       t(i) = section_info%data(i+2)
    END DO

    section_info%t_min = MIN(t(1), t(2))
    section_info%t_max = MAX(t(1), t(2))
    section_info%t_y_1 = -w(1) / 2d0 - cgy
    section_info%t_y_2 = w(1) / 2d0 - cgy
    section_info%t_z_1 = - cgz
    section_info%t_z_2 = w(2) - cgz

    section_info%web_height = w(2) - t(1)
    section_info%web_thickness = t(2)
    section_info%flange_width = w(1)
    section_info%flange_thickness = t(1)

    section_info%type_code = CS_TYPE_T

    CALL TrackEnd("get_t_beam_t")

  END SUBROUTINE get_t_beam_t

  SUBROUTINE get_t_beam_hats(section_info, cgy, cgz, n)
    USE ansys_upf, ONLY : TrackBegin, TrackEnd
    USE ansys_par, ONLY : ERH_FATAL
    USE glans
    USE LOCMOD, ONLY : libname
    USE ans_common, ONLY : cross_section_info, CS_TYPE_HATS

    IMPLICIT NONE

    TYPE(cross_section_info), INTENT(OUT) :: section_info
    REAL(KIND=8), INTENT(IN) :: cgy, cgz
    INTEGER, INTENT(IN) :: n

    REAL(KIND=8), DIMENSION(4) :: w
    REAL(KIND=8), DIMENSION(5) :: t
    INTEGER :: i
    LOGICAL :: err

    CALL TrackBegin("get_t_beam_hats")

    derrinfo(2) = n

    ALLOCATE(section_info%data(9))
    DO i = 1, 9
       err = s_get(fname, libname, __LINE__, ERH_FATAL, &
              & section_info%data(i), 'SECP', n, 'DATA    ', i)
    END DO

    DO i = 1, 4
       w(i) = section_info%data(i)
    END DO
    DO i = 1, 5
       t(i) = section_info%data(i+2)
    END DO

    section_info%t_min = MIN(t(1), t(2), t(3), t(4), t(5))
    section_info%t_max = MAX(t(1), t(2), t(3), t(4), t(5))
    section_info%t_y_1 = - cgy
    section_info%t_y_2 = w(1) + w(2) + w(3) - cgy
    section_info%t_z_1 = - cgz
    section_info%t_z_2 = W(4) - cgz

    section_info%type_code = CS_TYPE_HATS

    CALL TrackEnd("get_t_beam_hats")

  END SUBROUTINE get_t_beam_hats

  SUBROUTINE get_t_beam_hrec(section_info, cgy, cgz, n)
    USE ansys_upf, ONLY : TrackBegin, TrackEnd
    USE ansys_par, ONLY : ERH_FATAL
    USE glans
    USE LOCMOD, ONLY : libname
    USE ans_common, ONLY : cross_section_info, CS_TYPE_HREC

    IMPLICIT NONE

    TYPE(cross_section_info), INTENT(OUT) :: section_info
    REAL(KIND=8), INTENT(IN) :: cgy, cgz
    INTEGER, INTENT(IN) :: n

    REAL(KIND=8), DIMENSION(2) :: w
    REAL(KIND=8), DIMENSION(4) :: t
    INTEGER :: i
    LOGICAL :: err

    CALL TrackBegin("get_t_beam_hrec")

    derrinfo(2) = n

    ALLOCATE(section_info%data(6))
    DO i = 1, 6
       err = s_get(fname, libname, __LINE__, ERH_FATAL, &
              & section_info%data(i), 'SECP', n, 'DATA    ', i)
    END DO

    DO i = 1, 2
       w(i) = section_info%data(i)
    END DO
    DO i = 1, 4
       t(i) = section_info%data(i+2)
    END DO

    section_info%t_min = MIN(t(1), t(2), t(3), t(4))
    section_info%t_max = MAX(t(1), t(2), t(3), t(4))
    section_info%t_y_1 =  - cgy
    section_info%t_y_2 = w(1) - cgy
    section_info%t_z_1 = - cgz
    section_info%t_z_2 = W(2) - cgz

    section_info%type_code = CS_TYPE_HREC

    CALL TrackEnd("get_t_beam_hrec")

  END SUBROUTINE get_t_beam_hrec

  SUBROUTINE get_t_beam_asec(section_info, cgy, cgz, n)
    USE ansys_upf, ONLY : TrackBegin, TrackEnd
    USE ansys_par, ONLY : ERH_FATAL
    USE glans
    USE LOCMOD, ONLY : libname
    USE ans_common, ONLY : cross_section_info, CS_TYPE_ASEC

    IMPLICIT NONE

    TYPE(cross_section_info), INTENT(OUT) :: section_info
    REAL(KIND=8), INTENT(IN) :: cgy, cgz
    INTEGER, INTENT(IN) :: n

    LOGICAL :: err
    INTEGER :: i

    CALL TrackBegin("get_t_beam_asec")

    ALLOCATE(section_info%data(12))
    DO i = 1, 12
       err = s_get(fname, libname, __LINE__, ERH_FATAL, &
              & section_info%data(i), 'SECP', n, 'DATA    ', i)
    END DO

    section_info%t_min = 0d0
    section_info%t_max = 0d0

#if ANSVER >= 150
    section_info%t_y_1 = section_info%data(12)
    section_info%t_z_1 = section_info%data(11)
    section_info%t_y_2 = section_info%t_y_1 / 2d0
    section_info%t_z_2 = section_info%t_z_1 / 2d0
    section_info%t_y_1 = - section_info%t_y_2
    section_info%t_z_1 = - section_info%t_z_2
#else
    section_info%t_y_1 = 0d0
    section_info%t_z_1 = 0d0
    section_info%t_y_2 = 0d0
    section_info%t_z_2 = 0d0
#endif

    section_info%type_code = CS_TYPE_ASEC

    CALL TrackEnd("get_t_beam_asec")

  END SUBROUTINE get_t_beam_asec

  SUBROUTINE get_t_beam_mesh(section_info, cgy, cgz, n)
    USE ansys_upf, ONLY : TrackBegin, TrackEnd
    USE ansys_par, ONLY : ERH_FATAL
    USE glans
    USE LOCMOD, ONLY : libname
    USE ans_common, ONLY : cross_section_info, CS_TYPE_MESH

    IMPLICIT NONE

    TYPE(cross_section_info), INTENT(OUT) :: section_info
    REAL(KIND=8), INTENT(IN) :: cgy, cgz
    INTEGER, INTENT(IN) :: n

    CALL TrackBegin("get_t_beam_mesh")

    section_info%t_max = 0d0
    section_info%t_min = 0d0
    section_info%t_y_1 = 0d0
    section_info%t_y_2 = 0d0
    section_info%t_z_1 = 0d0
    section_info%t_z_2 = 0d0

    section_info%type_code = CS_TYPE_MESH

    CALL TrackEnd("get_t_beam_mesh")

  END SUBROUTINE get_t_beam_mesh

END MODULE mod_dat_beamproperties

! Local Variables:
! compile-command:"make -C .. test"
! End:
