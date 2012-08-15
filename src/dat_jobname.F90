!     get jobname

MODULE mod_dat_jobname

CONTAINS

  SUBROUTINE dat_jobname()

    USE ansys_upf, ONLY : TrackBegin, TrackEnd, erhandler
    USE ansys_par, ONLY : ERH_NOTE, PARMSIZE, STRING_MAX_LENG
    USE glans, ONLY : ans2bmf_get_s
    USE ans_common, ONLY : jobname
    USE LOCMOD, ONLY : libname

    IMPLICIT NONE
    ! Purpose:
    !
    ! Parameter:
    ! in/out Name          Task
    ! ----------------------------------------------------------------------
    ! Created: 2007-06-01  hoel
    ! ======================================================================

    CHARACTER(LEN=STRING_MAX_LENG) :: dummy

    ! dataspace for feeding erhandler subroutine
    DOUBLE PRECISION, DIMENSION(10) ::  derrinfo
    CHARACTER(LEN=PARMSIZE), DIMENSION(10) :: cerrinfo

    CHARACTER(LEN=40), PARAMETER :: fname=__FILE__

    CALL TrackBegin('dat_jobname')

    ! reads jobname from database. See description of *GET

    CALL ans2bmf_get_s('ACTIVE,0,JOBNAM,,START,1', dummy)
    jobname(1:8) = dummy(1:8)
    CALL ans2bmf_get_s('ACTIVE,0,JOBNAM,,START,9', dummy)
    jobname(9:16) = dummy(1:8)
    CALL ans2bmf_get_s('ACTIVE,0,JOBNAM,,START,17', dummy)
    jobname(17:24) = dummy(1:8)
    CALL ans2bmf_get_s('ACTIVE,0,JOBNAM,,START,25', dummy)
    jobname(25:32) = dummy(1:8)

    cerrinfo(1) = jobname
    CALL erhandler(fname, __LINE__, ERH_NOTE, &
         trim(libname)//':   jobname: %s', &
         derrinfo, cerrinfo)

    CALL TrackEnd('dat_jobname')

  END SUBROUTINE dat_jobname

END MODULE mod_dat_jobname

! Local Variables:
! compile-command:"make -C .. test"
! End:
