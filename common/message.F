c     issue warnings and update message count
      SUBROUTINE a2b_message(wcode,n1,n2)

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

      CHARACTER(LEN=4) :: wcode
      INTEGER n1,n2

      CHARACTER(LEN=40), PARAMETER :: fname=__FILE__

      CALL TrackBegin('a2b_message')

      wcount=wcount+1

      derrinfo(1) = n1
      derrinfo(2) = n2
      IF (wcode.EQ.'comp') THEN
         derrinfo(1) = n1
         CALL erhandler(fname, __LINE__, ERH_WARNING,
     $        'ans2bmf: Not all elements of component %i '//
     $        'converted !%/'//
     $        '(Check if element types are allowed in present version)',
     $        derrinfo, cerrinfo)
      ELSE IF (wcode.EQ.'matE') THEN
         CALL erhandler(fname, __LINE__, ERH_WARNING,
     $        'ans2bmf: no E-Module found in MP %i',
     $        derrinfo, cerrinfo)
      ELSE IF (wcode.EQ.'matP') THEN
         CALL erhandler(fname, __LINE__, ERH_WARNING,
     $        'ans2bmf: no Poisson ratio found in MP %i',
     $        derrinfo, cerrinfo)
      ELSE IF (wcode.EQ.'matD') THEN
         CALL erhandler(fname, __LINE__, ERH_WARNING,
     $        'ans2bmf: no density value found in MP %i',
     $        derrinfo, cerrinfo)
      ELSE IF (wcode.EQ.'beam') THEN
         CALL erhandler(fname, __LINE__, ERH_WARNING,
     $        'ans2bmf: beam at nodes %i %i '//
     $        'does not have orientation node',
     $        derrinfo, cerrinfo)
      ELSE IF (wcode.EQ.'pses') THEN
         CALL erhandler(fname, __LINE__, ERH_WARNING,
     $        'ans2bmf: shells with different thicknesses '//
     $        'not supported (el. %i). Average value taken.',
     $        derrinfo, cerrinfo)
      ELSE IF (wcode.EQ.'shel') THEN
         CALL erhandler(fname, __LINE__, ERH_WARNING,
     $        'ans2bmf: shells with different thicknesses '//
     $        'not supported (el. %i). Average value taken.',
     $        derrinfo, cerrinfo)
      END IF

      CALL TrackEnd('a2b_message')

      END

c Local Variables:
c compile-command:"make -C .. test"
c End:
