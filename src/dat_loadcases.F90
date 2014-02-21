#ifndef DEBUG_LOC
#ifdef DEBUG
#define DEBUG_LOC(func) call doDebugLoc(func,__FILE__,__LINE__)
#else
#define DEBUG_LOC
#endif !DEBUG_LOC
#endif

MODULE mod_dat_loadcases

CONTAINS

  SUBROUTINE dat_loadcases()

    USE ansys_upf, ONLY : TrackBegin, TrackEnd, erhandler, foriqr
    USE ansys_par, ONLY : DB_NUMDEFINED, ERH_NOTE, PARMSIZE
    USE ans_common
    USE LOCMOD, ONLY : libname

    ! Purpose:
    !
    ! Parameter:
    ! in/out Name          Task
    ! ----------------------------------------------------------------------
    ! Created: 2007-06-01  hoel
    ! ======================================================================

    IMPLICIT NONE

    CHARACTER(LEN=2) :: ext
    CHARACTER(LEN=1) :: cl
    CHARACTER(LEN=2) :: c2
    LOGICAL ex, eof
    INTEGER*2 l
    INTEGER s_file

    ! dataspace for feeding erhandler subroutine
    DOUBLE PRECISION, DIMENSION(10) ::  derrinfo
    CHARACTER(LEN=PARMSIZE), DIMENSION(10) :: cerrinfo

    CHARACTER(LEN=40), PARAMETER :: fname=__FILE__

    CALL TrackBegin('dat_loadcases')

    max_loadcases = 0

    ! inquiry on nodal loads in model and define global parameter
    ! n_nloads

    n_nloads = foriqr(0, DB_NUMDEFINED)
    IF (n_nloads.GT.0) THEN
       max_loadcases = max_loadcases + 1
    END IF

    ! check on files for loadcases written with lswrite and define
    ! global parameter n_sfiles
    n_sfiles = 0
    ex = .TRUE.
    l = 0
    s_file = 12

    ! check if file.s** with # l exists
    DO WHILE (ex)
       l = l + 1
       ! create filename
       IF (l.LE.9) THEN
          WRITE(cl,'(i1)') l
          ext = '0'//cl
       ELSE
          WRITE(ext,'(i2)') l
       ENDIF

       ! check
       INQUIRE(FILE=jobname(1:len_trim(jobname)) &
            //'.s'//ext, EXIST=ex)
       IF (ex) THEN
          ! and count all load lines on file
          eof = .FALSE.
          OPEN(UNIT=s_file, FILE=jobname(1:len_trim(jobname)) &
               //'.s'//ext)
          DO WHILE (.NOT.eof)
             READ(s_file,'(a)', END = 500) c2
             IF (c2 .EQ. 'F,') THEN
                n_nloads = n_nloads + 1
             END IF
          END DO
500       CONTINUE
       END IF
    END DO

    n_sfiles = l - 1
    max_loadcases = max_loadcases + n_sfiles
    derrinfo(1) = max_loadcases
    CALL erhandler(fname, __LINE__, ERH_NOTE, &
         'ans2bmf:   loadcases defined:  %i', derrinfo, cerrinfo)

    CALL TrackEnd('dat_loadcases')

  END SUBROUTINE dat_loadcases

END MODULE mod_dat_loadcases

! Local Variables:
! compile-command:"make -C .. test"
! End:
