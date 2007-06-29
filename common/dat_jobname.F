c     get jobname
      SUBROUTINE dat_jobname()

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

      CHARACTER(LEN=STRING_MAX_LENG) :: dummy

!     dataspace for feeding erhandler subroutine
      DOUBLE PRECISION ::  derrinfo(10)
      CHARACTER(LEN=PARMSIZE) :: cerrinfo(10)

      CHARACTER(LEN=40), PARAMETER :: fname=__FILE__

      CALL TrackBegin('dat_jobname')

c     reads jobname from database. See description of *GET

      CALL ans2bmf_get_s('ACTIVE,0,JOBNAM,,START,1', dummy)
      jobname(1:8) = dummy(1:8)
      CALL ans2bmf_get_s('ACTIVE,0,JOBNAM,,START,9', dummy)
      jobname(9:16) = dummy(1:8)
      CALL ans2bmf_get_s('ACTIVE,0,JOBNAM,,START,17', dummy)
      jobname(17:24) = dummy(1:8)
      CALL ans2bmf_get_s('ACTIVE,0,JOBNAM,,START,25', dummy)
      jobname(25:32) = dummy(1:8)

      cerrinfo(1) = jobname
      CALL erhandler(fname, __LINE__, ERH_NOTE,
     $     trim(libname)//':   jobname: %s',
     $     derrinfo, cerrinfo)

      CALL TrackEnd('dat_jobname')

      END

c Local Variables:
c compile-command:"make -C .. test"
c End:
