! Copyright (C) 2007 by Germanischer Lloyd AG

! ======================================================================
! Task      Provide types for linked lists.
! ----------------------------------------------------------------------
! Author    Berthold Höllmann <berthold.hoellmann@gl-group.com>
! Project   ans2bmf
! ======================================================================


! CVSID: $Id$

MODULE linklist_mod

  TYPE :: intlist
     INTEGER :: value = 0
     TYPE(intlist), POINTER :: next => NULL()
  END TYPE intlist

  TYPE :: intintlist
     INTEGER :: value1 = 0
     INTEGER :: value2 = 0
     TYPE(intintlist), POINTER :: next => NULL()
  END TYPE intintlist

  TYPE :: intloglist
     INTEGER :: value1 = 0
     LOGICAL :: value2 = .FALSE.
     TYPE(intloglist), POINTER :: next => NULL()
  END TYPE intloglist

END MODULE linklist_mod

! Local Variables:
! compile-command: "make -C .. test"
! End:


