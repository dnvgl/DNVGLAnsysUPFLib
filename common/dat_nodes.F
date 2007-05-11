c     find out and store # of nodes
      SUBROUTINE dat_nodes()

#include "common.F"

      CALL TrackBegin('dat_nodes')

      n_nodes = ndinqr(0,DB_NUMDEFINED)
      derrinfo(1) = n_nodes
      CALL erhandler(
     $     __FILE__,
     $     __LINE__,
     $     ANS_MSGLVL_NOTE,
     $     'ans2bmf:   nodes defined: %i', derrinfo, cerrinfo)

      CALL TrackEnd('dat_nodes')

      END

c Local Variables:
c compile-command:"make -C .. test"
c End:
