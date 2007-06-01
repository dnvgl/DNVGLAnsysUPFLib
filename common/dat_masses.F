C     Copyright (C) 2005 by Germanischer Lloyd AG
      
C     ======================================================================
C     Task      collect mass information from ANSYS model
C     ----------------------------------------------------------------------
C     Author    Berthold Höllmann <hoel@GL-Group.com>
C     Project   ans2bmf
C     ======================================================================


C CVSID: $Id$

      SUBROUTINE dat_masses()

      USE ansys
      USE LOCMOD

      IMPLICIT NONE
C     Purpose: 
C     
C     Parameter:
C     in/out Name          Task
C ----------------------------------------------------------------------
C     Created: 2007-06-01  hoel
C ======================================================================

      INTEGER :: iErr

#if   ANSVER >= 70
      CHARACTER(LEN=CMD_MAX_LENG) :: cmd
#else
      CHARACTER(LEN=SYS_LNG_CMDLN) :: cmd
#endif

      CALL TrackBegin('dat_masses')

c     select all nodal mass elements
      cmd = 'ESEL,S,ENAME,,MASS21'
      iErr = RunCommand(LEN_TRIM(cmd), cmd)

      n_masses = elmiqr(0,DB_NUMSELECTED)

      CALL TrackEnd('dat_masses')

      END


C Local Variables:
C compile-command: "make -C .. test"
C End:
