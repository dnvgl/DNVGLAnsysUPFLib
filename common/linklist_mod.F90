! Copyright (C) 2007 by Germanischer Lloyd AG

! ======================================================================
! Task      Provide types for linked lists.
! ----------------------------------------------------------------------
! Author    Berthold Höllmann <berthold.hoellmann@gl-group.com>
! Project   ans2bmf
! ======================================================================


! CVSID: $Id$

MODULE linklist_mod

  TYPE :: i_list
     INTEGER :: value = 0
     TYPE(i_list), POINTER :: next => NULL()
  END TYPE i_list

  TYPE :: ii_list
     INTEGER :: value1 = 0
     INTEGER :: value2 = 0
     TYPE(ii_list), POINTER :: next => NULL()
  END TYPE ii_list

  TYPE :: il_list
     INTEGER :: value1 = 0
     LOGICAL :: value2 = .FALSE.
     TYPE(il_list), POINTER :: next => NULL()
  END TYPE il_list

  TYPE :: dd_list
     DOUBLE PRECISION :: value1 = 0d0
     DOUBLE PRECISION :: value2 = 0d0
     TYPE(dd_list), POINTER :: next => NULL()
  END TYPE dd_list

  TYPE :: ddddd_list
     DOUBLE PRECISION :: value1 = 0d0
     DOUBLE PRECISION :: value2 = 0d0
     DOUBLE PRECISION :: value3 = 0d0
     DOUBLE PRECISION :: value4 = 0d0
     DOUBLE PRECISION :: value5 = 0d0
     TYPE(ddddd_list), POINTER :: next => NULL()
  END TYPE ddddd_list

END MODULE linklist_mod

! Local Variables:
! compile-command: "make -C .. test"
! End:


