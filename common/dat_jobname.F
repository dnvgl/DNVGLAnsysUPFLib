c     get jobname
      SUBROUTINE dat_jobname()

#include "common.F"

      CHARACTER(LEN=STRING_MAX_LENG) :: dummy

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
      CALL erhandler(
     $     __FILE__,
     $     __LINE__,
     $     ANS_MSGLVL_NOTE,
     $     'ans2bmf:   jobname: %s',
     $     derrinfo, cerrinfo)

      CALL TrackEnd('dat_jobname')

      END

c Local Variables:
c compile-command:"make -C .. test"
c End:
