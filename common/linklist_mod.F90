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
     INTEGER :: value
     TYPE(intlist), POINTER :: next => NULL()
  END TYPE intlist

  TYPE :: intintlist
     INTEGER :: value1
     INTEGER :: value2
     TYPE(intintlist), POINTER :: next => NULL()
  END TYPE intintlist

END MODULE linklist_mod

! Local Variables:
! compile-command: "make -C .. test"
! End:


