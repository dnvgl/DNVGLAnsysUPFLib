c     find out and store # of nodes
      SUBROUTINE dat_nodes()
      USE ansys_upf, ONLY : TrackBegin, TrackEnd, ndinqr, erhandler
      USE ansys_par, ONLY : DB_NUMDEFINED, ERH_NOTE, PARMSIZE
      USE LOCMOD, ONLY : libname, n_nodes
      IMPLICIT NONE
C     Purpose:
C
C     Parameter:
C     in/out Name          Task
C ----------------------------------------------------------------------
C     Created: 2007-06-01  hoel
C ======================================================================

!     dataspace for feeding erhandler subroutine
      DOUBLE PRECISION, DIMENSION(10) ::  derrinfo
      CHARACTER(LEN=PARMSIZE), DIMENSION(10) :: cerrinfo

      CHARACTER(LEN=40), PARAMETER :: fname=__FILE__

      CALL TrackBegin('dat_nodes')

      n_nodes = ndinqr(0, DB_NUMDEFINED)
      derrinfo(1) = n_nodes
      CALL erhandler(fname, __LINE__, ERH_NOTE,
     $     trim(libname)//':   nodes defined: %i', derrinfo, cerrinfo)

      CALL TrackEnd('dat_nodes')

      END

c Local Variables:
c compile-command:"make -C .. test"
c End:
