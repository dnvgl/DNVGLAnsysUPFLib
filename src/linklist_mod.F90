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
     REAL(KIND=8) :: value1 = 0d0
     REAL(KIND=8) :: value2 = 0d0
     TYPE(dd_list), POINTER :: next => NULL()
  END TYPE dd_list

CONTAINS

  SUBROUTINE append_i_list(base, item)
    USE dnvglans, ONLY : ezTrackBegin, ezTrackEnd
    IMPLICIT NONE
    ! Purpose:
    !
    ! Parameter:
    ! in/out Name          Task
    ! ----------------------------------------------------------------------
    ! Created: 2007-06-29  hoel
    ! ======================================================================
    TYPE(i_list), POINTER :: base
    TYPE(i_list), POINTER :: item

    TYPE(i_list), POINTER :: next
    TYPE(i_list), POINTER :: cur

    CALL ezTrackBegin('linklist:append_i_list')
    cur => NULL()
    next => base
    DO WHILE (ASSOCIATED(next))
       cur => next
       next => next % next
    END DO
    IF (ASSOCIATED(cur)) THEN
       cur % next => item
    ELSE
       base => item
    END IF
    CALL ezTrackEnd()
  END SUBROUTINE append_i_list

  SUBROUTINE clear_i_list(base)
    USE dnvglans, ONLY : ezTrackBegin, ezTrackEnd
    IMPLICIT NONE
    ! Purpose:
    !
    ! Parameter:
    ! in/out Name          Task
    ! ----------------------------------------------------------------------
    ! Created: 2007-06-29  hoel
    ! ======================================================================
    TYPE(i_list), POINTER :: base

    TYPE(i_list), POINTER :: next
    TYPE(i_list), POINTER :: cur
    CALL ezTrackBegin('linklist:clear_ii_list')
    cur => base
    DO WHILE (ASSOCIATED(cur))
       next => cur % next
       DEALLOCATE(cur)
       cur => next
    END DO
    base => NULL()
    CALL ezTrackEnd()
  END SUBROUTINE clear_i_list

  SUBROUTINE append_ii_list(base, item)
    USE dnvglans, ONLY : ezTrackBegin, ezTrackEnd
    IMPLICIT NONE
    ! Purpose:
    !
    ! Parameter:
    ! in/out Name          Task
    ! ----------------------------------------------------------------------
    ! Created: 2007-06-29  hoel
    ! ======================================================================
    TYPE(ii_list), POINTER :: base
    TYPE(ii_list), POINTER :: item

    TYPE(ii_list), POINTER :: next
    TYPE(ii_list), POINTER :: cur

    CALL ezTrackBegin('linklist:append_ii_list')
    cur => NULL()
    next => base
    DO WHILE (ASSOCIATED(next))
       cur => next
       next => next % next
    END DO
    IF (ASSOCIATED(cur)) THEN
       cur % next => item
    ELSE
       base => item
    END IF
    CALL ezTrackEnd()
  END SUBROUTINE append_ii_list

  FUNCTION insert_ii_list(base, item, id) RESULT(inserted)
    USE dnvglans, ONLY : ezTrackBegin, ezTrackEnd
    IMPLICIT NONE
    ! Purpose:
    !
    ! Parameter:
    ! in/out Name          Task
    ! ----------------------------------------------------------------------
    ! Created: 2007-06-29  hoel
    ! ======================================================================
    LOGICAL :: inserted
    TYPE(ii_list), POINTER :: base
    TYPE(ii_list), POINTER :: item
    INTEGER, INTENT(OUT) :: id

    TYPE(ii_list), POINTER :: prev
    TYPE(ii_list), POINTER :: cur

    CALL ezTrackBegin('linklist:insert_ii_list')
    inserted = .TRUE.
    prev => NULL()
    cur => base
    id = item % value2
    DO WHILE (ASSOCIATED(cur))
       IF (.NOT. (item % value1 .LT. cur % value1)) EXIT
       prev => cur
       cur => cur % next
    END DO

    IF (ASSOCIATED(cur)) THEN
       IF  (cur % value1 .EQ. item % value1) THEN
          ! Alread found section, return ID
          id = cur % value2
          inserted = .FALSE.
          CALL ezTrackEnd()
          RETURN
       END IF
    END IF
    IF (ASSOCIATED(prev)) THEN
       prev % next => item
    ELSE
       base => item
    END IF
    IF (ASSOCIATED(cur)) THEN
       item % next => cur
    END IF

    CALL ezTrackEnd()
  END FUNCTION insert_ii_list

  SUBROUTINE clear_ii_list(base)
    USE dnvglans, ONLY : ezTrackBegin, ezTrackEnd
    IMPLICIT NONE
    ! Purpose:
    !
    ! Parameter:
    ! in/out Name          Task
    ! ----------------------------------------------------------------------
    ! Created: 2007-06-29  hoel
    ! ======================================================================
    TYPE(ii_list), POINTER :: base

    TYPE(ii_list), POINTER :: next
    TYPE(ii_list), POINTER :: cur
    CALL ezTrackBegin('linklist:clear_iii_list')
    cur => base
    DO WHILE (ASSOCIATED(cur))
       next => cur % next
       DEALLOCATE(cur)
       cur => next
    END DO
    base => NULL()
    CALL ezTrackEnd()
  END SUBROUTINE clear_ii_list

  SUBROUTINE append_il_list(base, item)
    USE dnvglans, ONLY : ezTrackBegin, ezTrackEnd
    IMPLICIT NONE
    ! Purpose:
    !
    ! Parameter:
    ! in/out Name          Task
    ! ----------------------------------------------------------------------
    ! Created: 2007-06-29  hoel
    ! ======================================================================
    TYPE(il_list), POINTER :: base
    TYPE(il_list), POINTER :: item

    TYPE(il_list), POINTER :: next
    TYPE(il_list), POINTER :: cur

    CALL ezTrackBegin('linklist:append_il_list')
    cur => NULL()
    next => base
    DO WHILE (ASSOCIATED(next))
       cur => next
       next => next % next
    END DO
    IF (ASSOCIATED(cur)) THEN
       cur % next => item
    ELSE
       base => item
    END IF
    CALL ezTrackEnd()
  END SUBROUTINE append_il_list

  FUNCTION insert_il_list(base, item, val) RESULT(inserted)
    USE dnvglans, ONLY : ezTrackBegin, ezTrackEnd
    IMPLICIT NONE
    ! Purpose:
    !
    ! Parameter:
    ! in/out Name          Task
    ! ----------------------------------------------------------------------
    ! Created: 2007-06-29  hoel
    ! ======================================================================
    LOGICAL :: inserted
    TYPE(il_list), POINTER :: base
    TYPE(il_list), POINTER :: item
    LOGICAL, INTENT(OUT) :: val

    TYPE(il_list), POINTER :: prev
    TYPE(il_list), POINTER :: cur

    CALL ezTrackBegin('linklist:insert_il_list')
    inserted = .TRUE.
    prev => NULL()
    cur => base
    DO WHILE (ASSOCIATED(cur))
       IF (.NOT. (item % value1 .LT. cur % value1)) EXIT
       prev => cur
       cur => cur % next
    END DO

    IF (ASSOCIATED(cur)) THEN
       IF (cur % value1 .EQ. item % value1) THEN
          ! Alread found section, return ID
          val = cur % value2
          inserted = .FALSE.
          CALL ezTrackEnd()
          RETURN
       END IF
    END IF
    val = item % value2
    IF (ASSOCIATED(prev)) THEN
       prev % next => item
    ELSE
       base => item
    END IF
    IF (ASSOCIATED(cur)) THEN
       item % next => cur
    END IF

    CALL ezTrackEnd()
  END FUNCTION insert_il_list

  SUBROUTINE clear_il_list(base)
    USE dnvglans, ONLY : ezTrackBegin, ezTrackEnd
    IMPLICIT NONE
    ! Purpose:
    !
    ! Parameter:
    ! in/out Name          Task
    ! ----------------------------------------------------------------------
    ! Created: 2007-06-29  hoel
    ! ======================================================================
    TYPE(il_list), POINTER :: base

    TYPE(il_list), POINTER :: next
    TYPE(il_list), POINTER :: cur
    CALL ezTrackBegin('linklist:clear_iil_list')
    cur => base
    DO WHILE (ASSOCIATED(cur))
       next => cur % next
       DEALLOCATE(cur)
       cur => next
    END DO
    base => NULL()
    CALL ezTrackEnd()
  END SUBROUTINE clear_il_list

  SUBROUTINE append_dd_list(base, item)
    USE dnvglans, ONLY : ezTrackBegin, ezTrackEnd
    IMPLICIT NONE
    ! Purpose:
    !
    ! Parameter:
    ! in/out Name          Task
    ! ----------------------------------------------------------------------
    ! Created: 2007-06-29  hoel
    ! ======================================================================
    TYPE(dd_list), POINTER :: base
    TYPE(dd_list), POINTER :: item

    TYPE(dd_list), POINTER :: next
    TYPE(dd_list), POINTER :: cur

    CALL ezTrackBegin('linklist:append_dd_list')
    cur => NULL()
    next => base
    DO WHILE (ASSOCIATED(next))
       cur => next
       next => next % next
    END DO
    IF (ASSOCIATED(cur)) THEN
       cur % next => item
    ELSE
       base => item
    END IF
    CALL ezTrackEnd()
  END SUBROUTINE append_dd_list

  SUBROUTINE clear_dd_list(base)
    USE dnvglans, ONLY : ezTrackBegin, ezTrackEnd
    IMPLICIT NONE
    ! Purpose:
    !
    ! Parameter:
    ! in/out Name          Task
    ! ----------------------------------------------------------------------
    ! Created: 2007-06-29  hoel
    ! ======================================================================
    TYPE(dd_list), POINTER :: base

    TYPE(dd_list), POINTER :: next
    TYPE(dd_list), POINTER :: cur
    CALL ezTrackBegin('linklist:clear_idd_list')
    cur => base
    DO WHILE (ASSOCIATED(cur))
       next => cur % next
       DEALLOCATE(cur)
       cur => next
    END DO
    base => NULL()
    CALL ezTrackEnd()
  END SUBROUTINE clear_dd_list

END MODULE linklist_mod

! Local Variables:
! compile-command: "make -C .. test"
! End:
