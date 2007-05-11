C     Copyright (C) 2005 by Germanischer Lloyd AG
      
C     ======================================================================
C     Task      collect mass information from ANSYS model
C     ----------------------------------------------------------------------
C     Author    Berthold Höllmann <hoel@GL-Group.com>
C     Project   ans2bmf
C     ======================================================================


C CVSID: $Id$

      SUBROUTINE dat_masses()

#include "common.F"

      CALL TrackBegin('dat_masses')

c     select all nodal mass elements
      cmd = 'ESEL,S,ENAME,,MASS21'
      i=runcmd(cmd)

      n_masses = elmiqr(0,DB_NUMSELECTED)

      CALL TrackEnd('dat_masses')

      END


C Local Variables:
C compile-command: "make -C .. test"
C End:
