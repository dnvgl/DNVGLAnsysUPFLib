! Copyright (C) 2007 by Germanischer Lloyd AG

! ======================================================================
! Task      interface definitions for ANSYS routines
! ----------------------------------------------------------------------
! Author    Berthold Höllmann <berthold.hoellmann@gl-group.com>
! Project   ans2bmf
! ======================================================================

! CVSID: $Id$

#include "computer.h"

MODULE ansys_par

!DEC$ NOFREEFORM
#include "impcom.inc"
#include "ansysdef.inc"
#include "echprm.inc"
#include "syspar.inc"
#include "elempr.inc"
#include "tblecm.inc"
!DEC$ FREEFORM

  INTEGER, PARAMETER :: RL_DIM  = 40

END MODULE ansys_par

! Local Variables:
! compile-command:"make -C .. test"
! End:
