!     Copyright (C) 2005 by DNV GL SE

!     ======================================================================
!     Task      collect mass information from ANSYS model
!     ----------------------------------------------------------------------
!     Author    Berthold Höllmann <berthold.hoellmann@dnvgl.com>
!     Project   ans2bmf
!     ======================================================================


! CVSID: $Id$

MODULE mod_dat_masses

  USE dnvglans, ONLY : ezTrackBegin, ezTrackEnd, ezRunCommand

CONTAINS

  SUBROUTINE dat_masses()
    USE ansys_upf, ONLY : elmiqr
    USE ansys_par, ONLY : CMD_MAX_LENG, DB_NUMSELECTED
    USE ans_common, ONLY : n_masses
    ! Purpose:
    !
    ! Parameter:
    ! in/out Name          Task
    ! ----------------------------------------------------------------------
    ! Created: 2007-06-01  hoel
    ! ======================================================================

    IMPLICIT NONE

    INTEGER :: iErr

    CHARACTER(LEN=CMD_MAX_LENG) :: cmd

    CALL ezTrackBegin('dat_masses')

    ! select all nodal mass elements
    cmd = 'ESEL,S,ENAME,,MASS21'
    iErr = ezRunCommand(cmd)

    n_masses = elmiqr(0, DB_NUMSELECTED)

    CALL ezTrackEnd()

  END SUBROUTINE dat_masses

END MODULE mod_dat_masses

! Local Variables:
! compile-command: "make -C .. test"
! End:
