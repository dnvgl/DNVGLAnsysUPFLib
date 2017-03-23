!     find out and store # of nodes
MODULE mod_dat_nodes

  USE dnvglans, ONLY : ezTrackBegin, ezTrackEnd

CONTAINS

  SUBROUTINE dat_nodes()

    USE ansys_upf, ONLY : ndinqr
    USE ansys_par, ONLY : DB_NUMDEFINED, ERH_FNAME_LEN, PARMSIZE
    USE ans_common, ONLY : n_nodes
    USE dnvglans
    USE LOCMOD, ONLY : libname

    ! Purpose:
    !
    ! Parameter:
    ! in/out Name          Task
    ! ----------------------------------------------------------------------
    ! Created: 2007-06-01  hoel
    ! ======================================================================

    IMPLICIT NONE

    CHARACTER(LEN=ERH_FNAME_LEN), PARAMETER :: fname=__FILE__

    CALL ezTrackBegin('dat_nodes')

    n_nodes = ndinqr(0, DB_NUMDEFINED)
    derrinfo(1) = n_nodes
    CALL ans_note(fname, __LINE__, libname, '  nodes defined: %i')

    CALL ezTrackEnd()

  END SUBROUTINE dat_nodes

END MODULE mod_dat_nodes

! Local Variables:
! compile-command:"make -C .. test"
! End:
