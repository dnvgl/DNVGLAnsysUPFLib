!     find out and store # of nodes
MODULE mod_dat_nodes

CONTAINS

  SUBROUTINE dat_nodes()

    USE ansys_upf, ONLY : TrackBegin, TrackEnd, ndinqr, erhandler
    USE ansys_par, ONLY : DB_NUMDEFINED, ERH_FNAME_LEN, ERH_NOTE, PARMSIZE
    USE ans_common, ONLY : n_nodes
    USE LOCMOD, ONLY : libname

    ! Purpose:
    !
    ! Parameter:
    ! in/out Name          Task
    ! ----------------------------------------------------------------------
    ! Created: 2007-06-01  hoel
    ! ======================================================================

    ! dataspace for feeding erhandler subroutine

    IMPLICIT NONE

    REAL(KIND=8), DIMENSION(10) ::  derrinfo
    CHARACTER(LEN=PARMSIZE), DIMENSION(10) :: cerrinfo

    CHARACTER(LEN=ERH_FNAME_LEN), PARAMETER :: fname=__FILE__

    CALL TrackBegin('dat_nodes')

    n_nodes = ndinqr(0, DB_NUMDEFINED)
    derrinfo(1) = n_nodes
    CALL erhandler(fname, __LINE__, ERH_NOTE, &
         trim(libname)//':   nodes defined: %i', derrinfo, cerrinfo)

    CALL TrackEnd('dat_nodes')

  END SUBROUTINE dat_nodes

END MODULE mod_dat_nodes

! Local Variables:
! compile-command:"make -C .. test"
! End:
