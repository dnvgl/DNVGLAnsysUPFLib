C -*- fortran -*-
C Copyright (C) 2003 Germanischer Lloyd

C ======================================================================
C Task      common block initialization for ans2bmf
C ----------------------------------------------------------------------
C Author    Berthold Höllmann
C Project   ans2bmf
C ======================================================================

C CVSID: $Id$

      INTEGER csub_num
      PARAMETER (csub_num=9)

      CHARACTER(LEN=9) :: l_csub_ename(csub_num)
      CHARACTER(LEN=3) :: l_csub_eid(csub_num)
      CHARACTER(LEN=7) :: l_csub_postfix(csub_num)

      INTEGER ANS_SHELL41
      PARAMETER (ANS_SHELL41=1)
      INTEGER ANS_SHELL63
      PARAMETER (ANS_SHELL63=2)
      INTEGER ANS_BEAM44
      PARAMETER (ANS_BEAM44=3)
      INTEGER ANS_LINK8
      PARAMETER (ANS_LINK8=4)
      INTEGER ANS_COMBIN14
      PARAMETER (ANS_COMBIN14=5)
      INTEGER ANS_MASS21
      PARAMETER (ANS_MASS21=6)
      INTEGER ANS_SHELL181
      PARAMETER (ANS_SHELL181=7)
      ! shell elements with membrane option set
      INTEGER ANS_SHELL63_M
      PARAMETER (ANS_SHELL63_M=8)
      INTEGER ANS_SHELL181_M
      PARAMETER (ANS_SHELL181_M=9)

      COMMON / ans2bmf_csub /
     x     l_csub_ename, l_csub_eid, l_csub_postfix

c     values for error reporting
      INTEGER ANS_MSGLVL_TIME, ANS_MSGLVL_NO, ANS_MSGLVL_NOTE,
     $     ANS_MSGLVL_WARN, ANS_MSGLVL_ERR, ANS_MSGLVL_FTL

      COMMON / ans2bmf_errors /
     $     ANS_MSGLVL_TIME, ANS_MSGLVL_NO, ANS_MSGLVL_NOTE,
     $     ANS_MSGLVL_WARN, ANS_MSGLVL_ERR, ANS_MSGLVL_FTL

C Local Variables:
C compile-command:"make test"
C End:
