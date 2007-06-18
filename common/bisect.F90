! Copyright (C) 2007 by Germanischer Lloyd AG

! ======================================================================
! Task      Bisection search on arrays
! ----------------------------------------------------------------------
! Author    Berthold Höllmann <berthold.hoellmann@gl-group.com>
! Project   ans2bmf
! ======================================================================

! CVSID: $Id$

MODULE bisect

  CONTAINS

  FUNCTION idToInd(id, array, lena, dima) RESULT(ind)

    USE LOCMOD
    USE ansys

    IMPLICIT NONE
    !     Purpose:
    !     returns index to line whose first entry equals ind. Assumes first
    !     column of array to be sorted.
    !
    !     Parameter:
    !     in/out Name          Task
    !     in     id            id to look up index in array up
    !     in     array         array to search in
    !     in     lena          first dimensin of array
    !     in     dima          second dimension of array
    ! ----------------------------------------------------------------------
    !     Created: 2007-05-31  hoel
    ! ======================================================================
    INTEGER :: ind
    INTEGER, INTENT(IN) :: id
    INTEGER, INTENT(IN) :: lena
    INTEGER, INTENT(IN) :: dima
    INTEGER, INTENT(IN), DIMENSION(lena, dima) :: array

    INTEGER :: steps, maxsteps, laststep

    CHARACTER(LEN=40), PARAMETER :: fname=__FILE__

    CALL TrackBegin('bisect:idToInd')

    steps=0
    laststep = dima/2
    ind = laststep
    maxsteps = laststep+1

    DO WHILE (array(1, ind).NE.id)
       laststep = laststep / 2
       IF (laststep.LT.1) laststep = 1
       IF (array(1, ind).LT.id) THEN
          ind = ind + laststep
       ELSE
          ind = ind - laststep
       END IF
       IF (ind.LT.1) ind = 1
       IF (steps.GT.maxsteps) THEN
          derrinfo(1) = id
          derrinfo(2) = steps
          derrinfo(3) = dima
          CALL erhandler(fname, __LINE__, ERH_FATAL,&
               & trim(libname)// &
          &           ': line for id %d not found in %d steps, but array '// &
          &           'contains only %d lines.', &
          &           derrinfo, cerrinfo)
       END IF
       steps = steps + 1
    END DO

    CALL TrackEnd('bisect:idToInd')

  END FUNCTION idToInd

END MODULE bisect

! Local Variables:
! mode: f90
! compile-command:"make -C .. test"
! End:


