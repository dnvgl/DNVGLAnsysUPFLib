C -*- fortran -*-
C Copyright (C) 2003 Germanischer Lloyd

C ======================================================================
C Task      common block initialization for ans2bmf
C ----------------------------------------------------------------------
C Author    Berthold Höllmann
C Project   ans2bmf
C ======================================================================

C CVSID: $Id$

      INTEGER, PARAMETER :: csub_num=10

      CHARACTER(LEN=9), PARAMETER :: l_csub_ename(csub_num) = (/
     x     'SHELL41   ', 'SHELL63   ', 'BEAM44    ',
     x     'LINK8     ', 'COMBIN14  ', 'MASS21    ',
     x     'SHELL181  ', 'SHELL63_M ', 'SHELL181_M',
     $     'QUAD4     ' /)

      CHARACTER(LEN=3), PARAMETER :: l_csub_eid(csub_num) = (/
     $     'PSE', 'SH ', 'BE ', 'TE ', 'SPR', '   ', 'SH ',
     $     'PSE', 'PSE', 'VOL' /)
      CHARACTER(LEN=7), PARAMETER :: l_csub_postfix(csub_num) = (/
     x     'PSE    ', 'SHELLS ', 'BEAMS  ',
     x     'TRUSSES', 'BOUNDAR', 'MASS   ',
     x     'SHELLS ', 'PSE    ', 'PSE    ',
     $     'VOLUME '/)


      INTEGER, PARAMETER :: ANS_SHELL41=1
      INTEGER, PARAMETER :: ANS_SHELL63=2
      INTEGER, PARAMETER :: ANS_BEAM44=3
      INTEGER, PARAMETER :: ANS_LINK8=4
      INTEGER, PARAMETER :: ANS_COMBIN14=5
      INTEGER, PARAMETER :: ANS_MASS21=6
      INTEGER, PARAMETER :: ANS_SHELL181=7
      ! shell elements with membrane option set
      INTEGER, PARAMETER :: ANS_SHELL63_M=8
      INTEGER, PARAMETER :: ANS_SHELL181_M=9

!      COMMON / ans2bmf_csub /
!     x     l_csub_ename, l_csub_eid, l_csub_postfix

C     name of actual library, to be used in common error messages
#define _LIBNAME 'LIBNAME'
      CHARACTER(LEN=*), PARAMETER :: libname=_LIBNAME
!      COMMON / ans2conv_msg /
!     $     libname

C Local Variables:
C compile-command:"make -C .. test"
C End:
