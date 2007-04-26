C     Copyright (C) 2007 by Germanischer Lloyd AG
      
C     ======================================================================
C     Task      Helper routines for ansys converters
C     ----------------------------------------------------------------------
C     Author    Berthold Höllmann <hoel@GL-Group.com>
C     Project   ans2bmf
C     ======================================================================


C CVSID: $Id$

      SUBROUTINE ans2bmf_get_d(cmd_str, dout)
c     reads ANSYS command string 'cmd_str'
c     puts out double precision parameter 'dout'

#if     (defined(SOLARIS_SYS) || defined(LINUXIA32_SYS) || defined(LINUXIA64_SYS)) && defined(DYN_MOD)
#include "common.F"
#else
#include "ans2bmf/common.F"
#endif

      CHARACTER(LEN=*) :: cmd_str
      DOUBLE PRECISION dout

#if   ANSVER >= 70
      CHARACTER(LEN=128) dummy
#endif

      CALL TrackBegin("ans2bmf_get_d")
      cmd = '*GET,PARA,'//cmd_str
      i = runcmd(cmd)
      para ='PARA'

#if   ANSVER >= 70
      CALL parevl(para, 0, subc, 2, dout, dummy, kerr)
#else
      dout = parevl(para,0,subc,2,kerr)
#endif
      CALL TrackEnd("ans2bmf_get_d")
      END

c-----------------------------------------------------------

      SUBROUTINE ans2bmf_get_s(cmd_str, sout)
c     reads ANSYS command string 'cmd_str'
c     writes a string of length 8 into 'sout'

#if     (defined(SOLARIS_SYS) || defined(LINUXIA32_SYS) || defined(LINUXIA64_SYS)) && defined(DYN_MOD)
#include "common.F"
#else
#include "ans2bmf/common.F"
#endif

      CHARACTER(LEN=*) :: cmd_str
#if   ANSVER < 70
      CHARACTER(LEN=8) :: sout
#else
      CHARACTER(LEN=PARMSIZE) :: sout
#endif
      DOUBLE PRECISION res_double
#if   ANSVER < 70
      CHARACTER(LEN=8)      res_char8
      EQUIVALENCE      (res_double,res_char8)
#endif

#if   ANSVER >= 70
      CHARACTER(LEN=STRING_MAX_LENG) :: dummy
#endif
      CALL TrackBegin("ans2bmf_get_s")
      cmd = '*GET,PARA,'//cmd_str
      i = runcmd(cmd)
      para ='PARA'
#if   ANSVER >= 70
      CALL parevl(para, 0, subc, 2, res_double, dummy, kerr)
      sout = dummy(1:len_trim(dummy))
#else
      res_double = parevl(para,0,subc,2,kerr)
      sout = res_char8
#endif

      CALL TrackEnd("ans2bmf_get_s")
      END


C Local Variables:
C End:
