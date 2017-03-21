MODULE ans_common

  USE csubdata
  USE ansys_par
  USE dnvglans, ONLY : ans_error

  ! variables to interface with ANSYS
  INTEGER :: ityp
  INTEGER :: ielc(IelCSZ)

  ! global information about ANSYS model
  INTEGER :: n_nodes
  INTEGER :: n_nloads
  INTEGER :: n_elem
  INTEGER :: n_masses
  INTEGER :: n_mass_groups

  ! variables and parameters

  ! NZERO is used to compare double numbers
  ! NDIGITS is the number of significant digits
  REAL(KIND=8), PARAMETER :: NZERO=1E-30
  REAL(KIND=8), PARAMETER :: NDIGITS=8

  ! at this point, just one structure is admitted
  INTEGER, PARAMETER :: mainstructure=1

  ! [jobname].ascii is the file name opened as outfile
  CHARACTER(LEN=32) :: jobname
  INTEGER :: outfile
  INTEGER :: sfiles

  ! this block holds the component information
  INTEGER :: el_offset
  INTEGER :: el_length
  INTEGER :: el_count

  INTEGER, PARAMETER :: max_components=1024
  CHARACTER(LEN=16), DIMENSION(max_components) :: l_comp_name
  CHARACTER(LEN=STRING_MAX_LENG), DIMENSION(max_components) :: comp_name = " "
  INTEGER, DIMENSION(max_components) :: l_comp_pos
  INTEGER, DIMENSION(max_components) :: l_comp_len
  CHARACTER(LEN=STRING_MAX_LENG), DIMENSION(max_components) :: l_comp_ansys
  INTEGER, DIMENSION(max_components) :: l_comp_csub
  INTEGER, DIMENSION(max_components) :: l_csub_gid
  INTEGER :: comp_num

  INTEGER :: n_sfiles
  INTEGER :: max_loadcases

  INTEGER :: bp_length

  TYPE :: bp_values

     INTEGER :: id
     CHARACTER(LEN=16) :: name = ""
     REAL(KIND=8), DIMENSION(3) :: A = 0d0
     REAL(KIND=8), DIMENSION(3) :: I = 0d0
     REAL(KIND=8), DIMENSION(6) :: e = 0d0
     REAL(KIND=8), DIMENSION(2) :: sc = 0d0
     REAL(KIND=8), DIMENSION(3) :: d = 0d0
     REAL(KIND=8) :: Iyz = 0d0
     REAL(KIND=8) :: theta = 0d0

  END TYPE bp_values

  INTERFACE OPERATOR(.EQ.)
     MODULE PROCEDURE bp_values_equal
  END INTERFACE

  INTERFACE OPERATOR(.NE.)
     MODULE PROCEDURE bp_values_not_equal
  END INTERFACE

  INTERFACE ASSIGNMENT(=)
     MODULE PROCEDURE bp_values_assign
  END INTERFACE

  TYPE :: bp_loc
     INTEGER :: r = 999, i = 999, j = 999
  END TYPE bp_loc

  INTERFACE ASSIGNMENT(=)
     MODULE PROCEDURE bp_loc_assign
  END INTERFACE

  INTEGER, PARAMETER :: CS_TYPE_RECT = 1
  INTEGER, PARAMETER :: CS_TYPE_QUAD = 2
  INTEGER, PARAMETER :: CS_TYPE_CSOLID = 3
  INTEGER, PARAMETER :: CS_TYPE_CTUBE = 4
  INTEGER, PARAMETER :: CS_TYPE_CHAN = 5
  INTEGER, PARAMETER :: CS_TYPE_I = 6
  INTEGER, PARAMETER :: CS_TYPE_Z = 7
  INTEGER, PARAMETER :: CS_TYPE_L = 8
  INTEGER, PARAMETER :: CS_TYPE_T = 9
  INTEGER, PARAMETER :: CS_TYPE_HATS = 10
  INTEGER, PARAMETER :: CS_TYPE_HREC = 11
  INTEGER, PARAMETER :: CS_TYPE_ASEC = 12
  INTEGER, PARAMETER :: CS_TYPE_MESH = 13
  INTEGER, PARAMETER :: CS_TYPE_NOT_VALID = 666

  TYPE :: cs_mat_entry
     INTEGER :: mat_id = 0
     INTEGER :: cs_id = 0
     TYPE(cs_mat_entry), POINTER :: next => null()
  END TYPE cs_mat_entry

  TYPE :: cross_section_info
     INTEGER :: id = 0
     ! outer dimensions for cross section
     REAL(KIND=8) :: t_max, t_min, t_y_1, t_y_2, t_z_1, t_z_2
     REAL(KIND=8) :: web_height = 0.
     REAL(KIND=8) :: web_thickness = 0.
     REAL(KIND=8) :: flange_width = 0.
     REAL(KIND=8) :: flange_thickness = 0.
     ! raw cross section data acc. to ANSYS
     REAL(KIND=8), DIMENSION(:), ALLOCATABLE :: data
     ! cross section type
     INTEGER :: type_code = CS_TYPE_NOT_VALID

     TYPE(cs_mat_entry), POINTER :: gp_entries => null()
  END TYPE cross_section_info

  INTERFACE OPERATOR(.EQ.)
     MODULE PROCEDURE cross_section_equal
  END INTERFACE OPERATOR(.EQ.)

  INTERFACE OPERATOR(.NE.)
     MODULE PROCEDURE cross_section_not_equal
  END INTERFACE OPERATOR(.NE.)

  INTERFACE ASSIGNMENT(=)
     MODULE PROCEDURE cross_section_assign
  END INTERFACE ASSIGNMENT(=)

  INTERFACE OPERATOR(.EQ.)
     MODULE PROCEDURE cs_mat_equal
  END INTERFACE OPERATOR(.EQ.)

  INTERFACE OPERATOR(.NE.)
     MODULE PROCEDURE cs_mat_not_equal
  END INTERFACE OPERATOR(.NE.)

  INTERFACE ASSIGNMENT(=)
     MODULE PROCEDURE cs_mat_assign
  END INTERFACE ASSIGNMENT(=)

  INTERFACE
     SUBROUTINE collect_sc_info()
     END SUBROUTINE collect_sc_info
     FUNCTION find_gp_for_cs(i_id) RESULT(o_id)
       INTEGER, INTENT(in):: i_id
       INTEGER :: o_id
     END FUNCTION find_gp_for_cs
  END INTERFACE

  TYPE :: bp_type
     INTEGER :: num = 0 ! Total number of ANSYS CS geometry entries
     INTEGER :: rnum = 0 ! Number of Real CS entries
     INTEGER :: snum = 0 ! Number of Section CS entries
     INTEGER :: cs_id_max = 0 ! CS ids for BMF export
     ! beam property entries
     TYPE(bp_values), DIMENSION(:), ALLOCATABLE :: data
     ! Mapping real const. number to beam property entry
     TYPE(bp_loc), DIMENSION(:), ALLOCATABLE :: r_bp_map
     ! Mapping section number to beam property entry
     TYPE(bp_loc), DIMENSION(:), ALLOCATABLE :: se_bp_map
     ! Information on supported cross section descriptions
     TYPE(cross_section_info), DIMENSION(:), ALLOCATABLE :: cs_info
  END TYPE bp_type

  LOGICAL :: batch

  CHARACTER(LEN=ERH_FNAME_LEN), PARAMETER :: fname=__FILE__
  PRIVATE :: fname

CONTAINS

  FUNCTION bp_values_equal(a, b)
    IMPLICIT NONE
    TYPE(bp_values), INTENT(IN) :: a, b
    LOGICAL :: bp_values_equal

    bp_values_equal = ( (a%id == b%id) .AND. &
         (a%name == b%name) .AND. ALL(a%A == b%A) .AND. &
         ALL(a%I == b%I) .AND. ALL(a%e == b%e) .AND. &
         ALL(a%sc == b%sc) .AND. ALL(a%d == b%d) .AND. &
         (a%Iyz == b%Iyz) .AND. (a%theta == b%theta))
    RETURN
  END FUNCTION bp_values_equal

  FUNCTION bp_values_not_equal(a, b)
    IMPLICIT NONE
    TYPE(bp_values), INTENT(IN) :: a, b
    LOGICAL :: bp_values_not_equal

    bp_values_not_equal = .NOT. (a .EQ. b)
    RETURN
  END FUNCTION bp_values_not_equal

  SUBROUTINE bp_values_assign(a, b)
    IMPLICIT NONE
    TYPE(bp_values), INTENT(OUT) :: a
    TYPE(bp_values), INTENT(IN) :: b

    a%id = b%id
    a%name(:) = b%name(:)
    a%A(:) = b%A(:)
    a%I(:) = b%I(:)
    a%e(:) = b%e(:)
    a%sc(:) = b%sc(:)
    a%d(:) = b%d(:)
    a%Iyz = b%Iyz
    a%theta = b%theta
  END SUBROUTINE bp_values_assign

  SUBROUTINE bp_loc_assign(a, b)
    IMPLICIT NONE
    TYPE(bp_loc), INTENT(OUT) :: a
    TYPE(bp_loc), INTENT(IN) :: b

    a%r = b%r
    a%i = b%i
    a%j = b%j
  END SUBROUTINE bp_loc_assign

  FUNCTION allocate_bp_space(bp, rnum, snum)
    IMPLICIT NONE
    LOGICAL :: allocate_bp_space
    TYPE(bp_type), INTENT(INOUT) :: bp
    INTEGER, INTENT(IN) :: rnum, snum

    allocate_bp_space = .True.
    ALLOCATE(bp%data((2*rnum) + snum + 2))
    ALLOCATE(bp%r_bp_map(rnum))
    ALLOCATE(bp%se_bp_map(snum))
    ALLOCATE(bp%cs_info(snum))
    allocate_bp_space = .False.
    RETURN
  END FUNCTION allocate_bp_space

  SUBROUTINE deallocate_bp_space(bp)
    IMPLICIT NONE
    TYPE(bp_type), INTENT(INOUT) :: bp

    INTEGER :: i

    bp%num = 0
    DEALLOCATE(bp%data)
    DEALLOCATE(bp%r_bp_map)
    DEALLOCATE(bp%se_bp_map)
    DO i = 1, SIZE(bp%cs_info)
       IF (ALLOCATED(bp%cs_info(i)%data)) THEN
          DEALLOCATE(bp%cs_info(i)%data)
       END IF
    END DO
    DEALLOCATE(bp%cs_info)
  END SUBROUTINE deallocate_bp_space

  FUNCTION cross_section_equal(a, b)
    USE LOCMOD, ONLY : libname
    USE dnvglans, ONLY : ans_error
    IMPLICIT NONE
    TYPE(cross_section_info), INTENT(IN) :: a
    TYPE(cross_section_info), INTENT(IN) :: b
    LOGICAL :: cross_section_equal
    CALL ans_error(fname, __LINE__, libname, "not implemented")
    cross_section_equal = .FALSE.
  END FUNCTION cross_section_equal

  FUNCTION cross_section_not_equal(a, b)
    USE LOCMOD, ONLY : libname
    USE dnvglans, ONLY : ans_error
    IMPLICIT NONE
    TYPE(cross_section_info), INTENT(IN) :: a
    TYPE(cross_section_info), INTENT(IN) :: b
    LOGICAL :: cross_section_not_equal
    CALL ans_error(fname, __LINE__, libname, "not implemented")
    cross_section_not_equal = .FALSE.
  END FUNCTION cross_section_not_equal

  SUBROUTINE cross_section_assign(a, b)
    USE LOCMOD, ONLY : libname
    IMPLICIT NONE
    TYPE(cross_section_info), INTENT(OUT) :: a
    TYPE(cross_section_info), INTENT(IN) :: b
    a%id = b%id
    a%t_max = b%t_max
    a%t_min = b%t_min
    a%t_y_1 = b%t_y_1
    a%t_y_2 = b%t_y_2
    a%t_z_1 = b%t_z_1
    a%t_z_2 = b%t_z_2
    a%web_height = b%web_height
    a%web_thickness = b%web_thickness
    a%flange_width = b%flange_width
    a%flange_thickness = b%flange_thickness
    IF (ALLOCATED(a%data)) THEN
       ALLOCATE(a%data(SIZE(b%data)))
       a%data(:) = b%data(:)
    END IF
    a%type_code = b%type_code
  END SUBROUTINE cross_section_assign

  FUNCTION cs_mat_equal(a, b)
    USE LOCMOD, ONLY : libname
    USE dnvglans, ONLY : ans_error
    IMPLICIT NONE
    TYPE(cs_mat_entry), INTENT(IN) :: a
    TYPE(cs_mat_entry), INTENT(IN) :: b
    LOGICAL :: cs_mat_equal
    CALL ans_error(fname, __LINE__, libname, "not implemented")
    cs_mat_equal = .FALSE.
  END FUNCTION cs_mat_equal

  FUNCTION cs_mat_not_equal(a, b)
    USE LOCMOD, ONLY : libname
    USE dnvglans, ONLY : ans_error
    IMPLICIT NONE
    TYPE(cs_mat_entry), INTENT(IN) :: a
    TYPE(cs_mat_entry), INTENT(IN) :: b
    LOGICAL :: cs_mat_not_equal
    CALL ans_error(fname, __LINE__, libname, "not implemented")
    cs_mat_not_equal = .FALSE.
  END FUNCTION cs_mat_not_equal

  SUBROUTINE cs_mat_assign(a, b)
    USE LOCMOD, ONLY : libname
    USE dnvglans, ONLY : ans_error
    IMPLICIT NONE
    TYPE(cs_mat_entry), INTENT(OUT) :: a
    TYPE(cs_mat_entry), INTENT(IN) :: b
    CALL ans_error(fname, __LINE__, libname, "not implemented")
  END SUBROUTINE cs_mat_assign

END MODULE ans_common

! Local Variables:
! compile-command: "make -C .. test"
! mode: f90
! End:
