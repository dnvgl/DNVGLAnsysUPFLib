!     Copyright (C) 2005 by Germanischer Lloyd AG

!     ======================================================================
!     Task      collect mass information from ANSYS model
!     ----------------------------------------------------------------------
!     Author    Berthold Höllmann <hoel@GL-Group.com>
!     Project   ans2bmf
!     ======================================================================


! CVSID: $Id$

MODULE mod_dat_masses

CONTAINS

  SUBROUTINE dat_masses()
    USE ansys_upf, ONLY : TrackBegin, TrackEnd, RunCommand, elmiqr
#if ANSVER >= 70
    USE ansys_par, ONLY : CMD_MAX_LENG, DB_NUMSELECTED
#else
    USE ansys_par, ONLY : SYS_LNG_CMDLN, DB_NUMSELECTED
#endif
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

#if ANSVER >= 70
    CHARACTER(LEN=CMD_MAX_LENG) :: cmd
#else
    CHARACTER(LEN=SYS_LNG_CMDLN) :: cmd
#endif

    CALL TrackBegin('dat_masses')

    ! select all nodal mass elements
    cmd = 'ESEL,S,ENAME,,MASS21'
    iErr = RunCommand(LEN_TRIM(cmd), cmd)

    n_masses = elmiqr(0, DB_NUMSELECTED)

    CALL TrackEnd('dat_masses')

  END SUBROUTINE dat_masses

END MODULE mod_dat_masses

! Local Variables:
! compile-command: "make -C .. test"
! End:
