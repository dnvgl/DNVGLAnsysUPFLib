! Copyright (C) 2003 Germanischer Lloyd

! ======================================================================
! Task      common block initialization for ans2bmf
! ----------------------------------------------------------------------
! Author    Berthold Höllmann
! Project   ans2bmf
! ======================================================================

! CVSID: $Id$

MODULE csubdata

  INTEGER, PARAMETER :: csub_num=12

  CHARACTER(LEN=9), PARAMETER :: l_csub_ename(csub_num) = (/        &
       'SHELL41   ', 'SHELL63   ', 'BEAM44    ',                    &
       'LINK8     ', 'COMBIN14  ', 'MASS21    ',                    &
       'SHELL181  ', 'SHELL63_M ', 'SHELL181_M',                    &
       'SOLID45   ', 'SOLID185  ', 'BEAM188   ' /)

  CHARACTER(LEN=3), PARAMETER :: l_csub_eid(csub_num) = (/          &
       'PSE', 'SH ', 'BE ', 'TE ', 'SPR', '   ', 'SH ',             &
       'PSE', 'PSE', 'VOL', 'VOL', 'BE ' /)
  CHARACTER(LEN=7), PARAMETER :: l_csub_postfix(csub_num) = (/      &
       'PSE    ', 'SHELLS ', 'BEAMS  ',                             &
       'TRUSSES', 'BOUNDAR', 'MASS   ',                             &
       'SHELLS ', 'PSE    ', 'PSE    ',                             &
       'VOLUME ', 'VOLUME ', 'BEAMS  ' /)

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
  ! volume elements
  INTEGER, PARAMETER :: ANS_SOLID45=10
  INTEGER, PARAMETER :: ANS_SOLID185=11
  ! replacement for beam44
  INTEGER, PARAMETER :: ANS_BEAM188=12

END MODULE csubdata

! Local Variables:
! mode: f90
! ispell-local-dictionary: "english"
! compile-command: "make -C .. test"
! End:
