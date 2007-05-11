c     issue warnings and update message count
      SUBROUTINE a2b_message(wcode,n1,n2)

#if     (defined(SOLARIS_SYS) || defined(LINUXIA32_SYS) || defined(LINUXIA64_SYS)) && defined(DYN_MOD)
#include "common.F"
#else
#include "ans2bmf/common.F"
#endif

      CHARACTER(LEN=4) :: wcode
      INTEGER n1,n2

      CALL TrackBegin('a2b_message')

      wcount=wcount+1

      derrinfo(1) = n1
      derrinfo(2) = n2
      IF (wcode.EQ.'comp') THEN
         derrinfo(1) = n1
         CALL erhandler(
     $        __FILE__,
     $        __LINE__,
     $        ANS_MSGLVL_WARN,
     $        'WARNING (ans2bmf): Not all elements of component %i '//
     $        'converted !\n'//
     $        '(Check if element types are allowed in present version)',
     $        derrinfo, cerrinfo)
      ELSE IF (wcode.EQ.'matE') THEN
         CALL erhandler(
     $        __FILE__,
     $        __LINE__,
     $        ANS_MSGLVL_WARN,
     $        'WARNING (ans2bmf): no E-Module found in MP %i',
     $        derrinfo, cerrinfo)
      ELSE IF (wcode.EQ.'matP') THEN
         CALL erhandler(
     $        __FILE__,
     $        __LINE__,
     $        ANS_MSGLVL_WARN,
     $        'WARNING (ans2bmf): no Poisson ratio found in MP %i',
     $        derrinfo, cerrinfo)
      ELSE IF (wcode.EQ.'matD') THEN
         CALL erhandler(
     $        __FILE__,
     $        __LINE__,
     $        ANS_MSGLVL_WARN,
     $        'WARNING (ans2bmf): no density value found in MP %i',
     $        derrinfo, cerrinfo)
      ELSE IF (wcode.EQ.'beam') THEN
         CALL erhandler(
     $        __FILE__,
     $        __LINE__,
     $        ANS_MSGLVL_WARN,
     $        'WARNING (ans2bmf): beam at nodes %i %i '//
     $        'does not have orientation node',
     $        derrinfo, cerrinfo)
      ELSE IF (wcode.EQ.'pses') THEN
         CALL erhandler(
     $        __FILE__,
     $        __LINE__,
     $        ANS_MSGLVL_WARN,
     $        'WARNING (ans2bmf): shells with different thicknesses '//
     $        'not supported (el. %i). Average value taken.',
     $        derrinfo, cerrinfo)
      ELSE IF (wcode.EQ.'shel') THEN
         CALL erhandler(
     $        __FILE__,
     $        __LINE__,
     $        ANS_MSGLVL_WARN,
     $        'WARNING (ans2bmf): shells with different thicknesses '//
     $        'not supported (el. %i). Average value taken.',
     $        derrinfo, cerrinfo)
      END IF

      CALL TrackEnd('a2b_message')

      END

c Local Variables:
c compile-command:"make -C .. test"
c End:
