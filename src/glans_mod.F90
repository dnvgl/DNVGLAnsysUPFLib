! Copyright (C) 2007 by Germanischer Lloyd AG

! ======================================================================
! Task      Helper routines for ansys converters
! ----------------------------------------------------------------------
! Author    Berthold Höllmann <hoel@GL-Group.com>
! Project   ans2bmf
! ======================================================================

! CVSID: $Id$

#ifndef DEBUG_LOC
#ifdef DEBUG
#define DEBUG_LOC(func) call doDebugLoc(func,__FILE__,__LINE__)
#else
#define DEBUG_LOC
#endif !DEBUG_LOC
#endif

MODULE glans

  USE ansys_par, ONLY : PARMSIZE, ERH_FNAME_LEN, STRING_MAX_LENG, &
       & ERH_NOTE, ERH_WARNING, ERH_ERROR, ERH_FATAL, ERH_CAUTION

  PUBLIC :: ERH_FNAME_LEN

  PRIVATE

  INTERFACE get
     MODULE PROCEDURE getf, getfs, getfi, geti, getis, getii, gets, getsi
  END INTERFACE

  INTERFACE s_get
     MODULE PROCEDURE s_getf, s_getfs, s_getfi, s_geti, s_getis, s_getii, &
          & s_gets, s_getsi
  END INTERFACE

  INTERFACE esel
     MODULE PROCEDURE esels, eseli
  END INTERFACE

  PUBLIC :: get
  PUBLIC :: s_get
  PUBLIC :: esel
  PUBLIC :: nsel
  PUBLIC :: cmselect
  PUBLIC :: message
  PUBLIC :: doDebugLoc
  PUBLIC :: ans2bmf_get_s
  PUBLIC :: ans2bmf_get_d

  PUBLIC :: wcount

  ! number of warnings issued
  INTEGER :: wcount = 0

  ! dataspace for feeding erhandler subroutine
  REAL(KIND=8), DIMENSION(10) ::  derrinfo
  CHARACTER(LEN=PARMSIZE), DIMENSION(10) :: cerrinfo

  CHARACTER(LEN=ERH_FNAME_LEN), PARAMETER :: fname=__FILE__

  PUBLIC :: derrinfo, cerrinfo
  PUBLIC :: ans_note, ans_warn, ans_error, ans_fatal, ans_caution

CONTAINS

  SUBROUTINE ans_note(fname, line, libname, msg)
    IMPLICIT NONE
    CHARACTER(LEN=ERH_FNAME_LEN), INTENT(IN) :: fname
    INTEGER, INTENT(IN) :: line
    CHARACTER(LEN=*), INTENT(IN) :: libname
    CHARACTER(LEN=*), INTENT(IN) :: msg
    CALL erhandler(fname, line, ERH_NOTE, libname // ':' // msg, &
         derrinfo, cerrinfo)
  END SUBROUTINE ans_note

  SUBROUTINE ans_warn(fname, line, libname, msg)
    IMPLICIT NONE
    CHARACTER(LEN=ERH_FNAME_LEN), INTENT(IN) :: fname
    INTEGER, INTENT(IN) :: line
    CHARACTER(LEN=*), INTENT(IN) :: libname
    CHARACTER(LEN=*), INTENT(IN) :: msg
    CALL erhandler(fname, line, ERH_WARNING, msg, &
         derrinfo, cerrinfo)
  END SUBROUTINE ans_warn

  SUBROUTINE ans_error(fname, line, libname, msg)
    IMPLICIT NONE
    CHARACTER(LEN=ERH_FNAME_LEN), INTENT(IN) :: fname
    INTEGER, INTENT(IN) :: line
    CHARACTER(LEN=*), INTENT(IN) :: libname
    CHARACTER(LEN=*), INTENT(IN) :: msg
    CALL erhandler(fname, line, ERH_ERROR, msg, &
         derrinfo, cerrinfo)
  END SUBROUTINE ans_error

  SUBROUTINE ans_fatal(fname, line, libname, msg)
    IMPLICIT NONE
    CHARACTER(LEN=ERH_FNAME_LEN), INTENT(IN) :: fname
    INTEGER, INTENT(IN) :: line
    CHARACTER(LEN=*), INTENT(IN) :: libname
    CHARACTER(LEN=*), INTENT(IN) :: msg
    CALL erhandler(fname, line, ERH_FATAL, msg, &
         derrinfo, cerrinfo)
  END SUBROUTINE ans_fatal

  SUBROUTINE ans_caution(fname, line, libname, msg)
    IMPLICIT NONE
    CHARACTER(LEN=ERH_FNAME_LEN), INTENT(IN) :: fname
    INTEGER, INTENT(IN) :: line
    CHARACTER(LEN=*), INTENT(IN) :: libname
    CHARACTER(LEN=*), INTENT(IN) :: msg
    CALL erhandler(fname, line, ERH_CAUTION, msg, &
         derrinfo, cerrinfo)
  END SUBROUTINE ans_caution


  SUBROUTINE ans2bmf_get_d(cmd_str, dout)
    ! reads ANSYS command string 'cmd_str'
    ! puts out REAL(KIND=8) parameter 'dout'

    USE ansys_upf, ONLY : TrackBegin, TrackEnd, RunCommand, parevl
    USE ansys_par, ONLY : CMD_MAX_LENG, STRING_MAX_LENG, PARMSIZE
    IMPLICIT NONE
    ! Purpose:
    !
    ! Parameter:
    ! in/out Name          Task
    ! ----------------------------------------------------------------------
    ! Created: 2007-06-01  hoel
    ! ======================================================================
    CHARACTER(LEN=*) :: cmd_str
    REAL(KIND=8) :: dout
    REAL(KIND=8), DIMENSION(3) :: subc

    CHARACTER(LEN=CMD_MAX_LENG) :: cmd
    CHARACTER(LEN=STRING_MAX_LENG) :: dummy
    CHARACTER(LEN=PARMSIZE) :: para

    INTEGER :: iErr

    CALL TrackBegin("glans:ans2bmf_get_d")
    cmd = '*GET,PAR_,'//cmd_str
    iErr = RunCommand(LEN_TRIM(cmd), cmd)
    para ='PAR_'

    CALL parevl(para, 0, subc, 2, dout, dummy, iErr)

    CALL TrackEnd("glans:ans2bmf_get_d")
  END SUBROUTINE ans2bmf_get_d

  SUBROUTINE ans2bmf_get_s(cmd_str, sout)
    ! reads ANSYS command string 'cmd_str'
    ! writes a string of length 8 into 'sout'
    USE ansys_upf, ONLY : TrackBegin, TrackEnd, RunCommand, parevl
    USE ansys_par, ONLY : CMD_MAX_LENG, STRING_MAX_LENG, PARMSIZE
    IMPLICIT NONE
    ! Purpose:
    !
    ! Parameter:
    ! in/out Name          Task
    ! ----------------------------------------------------------------------
    ! Created: 2007-06-01  hoel
    ! ======================================================================
    CHARACTER(LEN=*) :: cmd_str
    CHARACTER(LEN=PARMSIZE) :: sout
    REAL(KIND=8) res_double
    REAL(KIND=8), DIMENSION(3) :: subc
    CHARACTER(LEN=CMD_MAX_LENG) :: cmd
    CHARACTER(LEN=STRING_MAX_LENG) :: dummy
    CHARACTER(LEN=PARMSIZE) :: para

    INTEGER :: iErr

    CALL TrackBegin("glans:ans2bmf_get_s")

    cmd = '*GET,PAR_,'//cmd_str
    iErr = RunCommand(LEN_TRIM(cmd), cmd)
    para ='PAR_'
    CALL parevl(para, 0, subc, 2, res_double, dummy, iErr)
    sout = TRIM(dummy)

    CALL TrackEnd("glans:ans2bmf_get_s")
  END SUBROUTINE ans2bmf_get_s

  FUNCTION upcase(string) RESULT(upper)
    ! Convert input string to uppercase
    USE ansys_upf, ONLY : TrackBegin, TrackEnd
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: string
    CHARACTER(LEN=LEN(string)) :: upper
    INTEGER :: j

    CALL TrackBegin('glans:upcase')

    DO j = 1, LEN(string)
       IF(string(j:j) >= "a" .AND. string(j:j) <= "z") THEN
          upper(j:j) = ACHAR(IACHAR(string(j:j)) - 32)
       ELSE
          upper(j:j) = string(j:j)
       END IF
    END DO

    CALL TrackEnd('glans:upcase')

  END FUNCTION upcase

  SUBROUTINE cmselect(mode, name)
    ! select component
    USE ansys_upf, ONLY : TrackBegin, TrackEnd, RunCommand
    USE ansys_par, ONLY : CMD_MAX_LENG, ERH_FATAL, ERH_FNAME_LEN, PARMSIZE
    IMPLICIT NONE
    ! Purpose:
    !
    !  Select given ANSYS component
    !
    ! Parameter:
    ! in/out Name          Task
    ! in     mode          selection mode, one of:
    !                         S    -- Select a new set (default).
    !                         R    -- Reselect a set from the current set.
    !                         A    -- Additionally select a set and extend
    !                                 the current set.
    !                         U    -- Unselect a set from the current set.
    !                         ALL  -- Also select all components.
    !                         NONE -- Unselect all components.
    ! in     name          name of component to be selected. Value meaningless
    !                      for mode=='ALL' and mode=='NONE'
    ! ----------------------------------------------------------------------
    ! Created: 2007-05-21  hoel
    ! ======================================================================
    CHARACTER(LEN=*) :: mode
    CHARACTER(LEN=*), OPTIONAL :: name

    CHARACTER(LEN=LEN(mode)) :: umode
    INTEGER :: iErr

    CHARACTER(LEN=CMD_MAX_LENG) :: cmd

!    CHARACTER(LEN=ERH_FNAME_LEN), PARAMETER :: fname=__FILE__

    CALL TrackBegin('glans:cmselect')

    umode = upcase(mode)

    IF ((umode.NE.'S') .AND. &
         & (umode.NE.'R') .AND. &
         & (umode.NE.'A') .AND. &
         & (umode.NE.'U') .AND. &
         & (umode.NE.'ALL') .AND. &
         & (umode.NE.'NONE')) THEN
       CALL ans_fatal(fname, __LINE__, 'glansys', &
            & 'invalid mode: '//TRIM(umode)//'.')
    END IF

    IF (PRESENT(name)) THEN
       WRITE(cmd,200) TRIM(upcase(mode)), TRIM(upcase(name))
    ELSE
       WRITE(cmd,200) TRIM(upcase(mode)), ''
    END IF
200 FORMAT('CMSEL,'A,','A)

    iErr = RunCommand(LEN_TRIM(cmd), cmd)

    CALL TrackEnd('glans:cmselect')

    RETURN
  END SUBROUTINE cmselect

  SUBROUTINE eseli(Type, Item, Comp, VMIN, VMAX, VINC, KABS)
    USE ansys_upf, ONLY : TrackBegin, TrackEnd, RunCommand
    USE ansys_par, ONLY : CMD_MAX_LENG, ERH_FATAL, ERH_FNAME_LEN, PARMSIZE

    IMPLICIT NONE
    ! Purpose: select elements
    !
    ! Parameter:
    ! in/out Name          Task
    ! in     Type          Label identifying the type of select:
    !                        S -- Select a new set (default).
    !                        R -- Reselect a set from the current set.
    !                        A -- Additionally select a set and extend
    !                             the current set.
    !                        U -- Unselect a set from the current set.
    !                        ALL -- Restore the full set.
    !                        NONE -- Unselect the full set.
    !                        INVE -- Invert the current set (selected
    !                                becomes unselected and vice versa).
    ! The following fields are used only with Type = S, R, A, or U:
    ! in     Item          Label identifying data, see ESEL
    !                      command. Some items also require a component
    !                      label. Defaults to ELEM. If Item = STRA
    !                      (straightened), elements are selected whose
    !                      midside nodes do not conform to the curved
    !                      line or non-flat area on which they should
    !                      lie. (Such elements are sometimes formed
    !                      during volume meshing [VMESH] in an attempt
    !                      to avoid excessive element distortion.) You
    !                      should graphically examine any such elements
    !                      to evaluate their possible effect on solution
    !                      accuracy.
    ! in     Comp          Component of the item (if required). For valid
    !                      component labels see ESEL command.
    ! in     VMIN          Minimum value of item range. Ranges are
    !                      element numbers, attribute numbers, load
    !                      values, or result values as appropriate for
    !                      the item.
    ! in     VMAX          Maximum value of item range. VMAX defaults to
    !                      VMIN for input values.
    !
    !                      For result values, VMAX defaults to infinity
    !                      if VMIN is positive, or to zero if VMIN is
    !                      negative.
    ! in     VINC          Value increment within range. Used only with
    !                      integer ranges (such as for element and
    !                      attribute numbers). Defaults to 1. VINC
    !                      cannot be negative.
    ! in     KABS          Absolute value key:
    !                        0 -- Check sign of value during selection.
    !                        1 -- Use absolute value during selection
    !                             (sign ignored).
    ! ----------------------------------------------------------------------
    ! Created: 2007-05-29 hoel
    ! ======================================================================
    CHARACTER(LEN=*), INTENT(IN) :: Type
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: Item
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: Comp
    INTEGER, INTENT(IN), OPTIONAL :: VMIN
    INTEGER, INTENT(IN), OPTIONAL :: VMAX
    INTEGER, INTENT(IN), OPTIONAL :: VINC
    INTEGER, INTENT(IN), OPTIONAL :: KABS

    CHARACTER(LEN=LEN(type)) :: utype
    CHARACTER(LEN=128) :: uitem
    INTEGER :: iErr

    CHARACTER(LEN=CMD_MAX_LENG) :: cmd

    CHARACTER(LEN=ERH_FNAME_LEN), PARAMETER :: fname=__FILE__

    CALL TrackBegin('glans:eseli')

    utype = upcase(type)

    IF ((utype.NE.'S').AND. &
         & (utype.NE.'R').AND. &
         & (utype.NE.'A').AND. &
         & (utype.NE.'U').AND. &
         & (utype.NE.'ALL').AND. &
         & (utype.NE.'NONE').AND. &
         & (utype.NE.'INVE')) THEN
       CALL ans_fatal(fname, __LINE__, 'glansys', &
            & 'invalid type: '//TRIM(utype)//'.')
    END IF

    IF (PRESENT(Item)) THEN
       IF (LEN(item).GT.128) THEN
          CALL ans_fatal(fname, __LINE__, 'glansys', &
               & 'length of value item greater then 129.')
       END IF
       uitem = upcase(item)
       IF ((uitem.NE.'ELEM').AND. &
            & (uitem.NE.'ADJ').AND. &
            & (uitem.NE.'TYPE').AND. &
            & (uitem.NE.'ENAME').AND. &
            & (uitem.NE.'MAT').AND. &
            & (uitem.NE.'REAL').AND. &
            & (uitem.NE.'ESYS').AND. &
            & (uitem.NE.'PART').AND. &
            & (uitem.NE.'LIVE').AND. &
            & (uitem.NE.'LAYER').AND. &
            & (uitem.NE.'SEC').AND. &
            & (uitem.NE.'PINC').AND. &
            & (uitem.NE.'PEXC').AND. &
            & (uitem.NE.'STRA').AND. &
            & (uitem.NE.'SFE').AND. &
            & (uitem.NE.'BFE').AND. &
            & (uitem.NE.'PATH')) THEN
          CALL ans_fatal(fname, __LINE__, 'glansys', &
               & 'invalid item: '//TRIM(uitem)//'.')
       END IF
       IF (PRESENT(comp)) THEN
          IF ((uitem.EQ.'ELEM').AND. &
               & (uitem.EQ.'ADJ').AND. &
               & (uitem.EQ.'TYPE').AND. &
               & (uitem.EQ.'ENAME').AND. &
               & (uitem.EQ.'MAT').AND. &
               & (uitem.EQ.'REAL').AND. &
               & (uitem.EQ.'ESYS').AND. &
               & (uitem.EQ.'PART').AND. &
               & (uitem.EQ.'LIVE').AND. &
               & (uitem.EQ.'LAYER').AND. &
               & (uitem.EQ.'SEC').AND. &
               & (uitem.EQ.'PINC').AND. &
               & (uitem.EQ.'PEXC').AND. &
               & (uitem.EQ.'STRA')) THEN
             CALL ans_fatal(fname, __LINE__, 'glansys', &
                  & 'invalid input: component not allowed '// &
                  & 'with item '//uitem//'.')
          ELSE IF (uitem.EQ.'SFE') THEN
             CALL ans_fatal(fname, __LINE__, 'glansys', &
                  & 'item '//uitem//' not yet supported.')
          ELSE IF (uitem.EQ.'BFE') THEN
             CALL ans_fatal(fname, __LINE__, 'glansys', &
                  & 'item '//uitem//' not yet supported.')
          ELSE IF (uitem.EQ.'PATH') THEN
             CALL ans_fatal(fname, __LINE__, 'glansys', &
                  & 'item '//uitem//' not yet supported.')
          END IF
       END IF
    ELSE
       IF (PRESENT(comp)) THEN
          CALL ans_fatal(fname, __LINE__, 'glansys', &
               & 'invaid call: comp given, but no item.')
       END IF
    END IF

    cmd = 'ESEL'
    WRITE(cmd,200) TRIM(cmd), TRIM(Type)
    IF (PRESENT(Item)) THEN
       WRITE(cmd,200) TRIM(cmd), TRIM(Item)
    ELSE
       WRITE(cmd,200) TRIM(cmd), ''
    END IF
    IF (PRESENT(Comp)) THEN
       WRITE(cmd,200) TRIM(cmd), TRIM(Comp)
    ELSE
       WRITE(cmd,200) TRIM(cmd), ''
    END IF
    IF (PRESENT(VMIN)) THEN
       WRITE(cmd,201) TRIM(cmd), VMIN
    ELSE
       WRITE(cmd,200) TRIM(cmd), ''
    END IF
    IF (PRESENT(VMAX)) THEN
       WRITE(cmd,201) TRIM(cmd), VMAX
    ELSE
       WRITE(cmd,200) TRIM(cmd), ''
    END IF
    IF (PRESENT(VINC)) THEN
       WRITE(cmd,201) TRIM(cmd), VINC
    ELSE
       WRITE(cmd,200) TRIM(cmd), ''
    END IF
    IF (PRESENT(KABS)) THEN
       WRITE(cmd,201) TRIM(cmd), KABS
    ELSE
       WRITE(cmd,200) TRIM(cmd), ''
    END IF

200 FORMAT(A,','A)
201 FORMAT(A,','I)

    iErr = RunCommand(LEN_TRIM(cmd), cmd)

    CALL TrackEnd('glans:eseli')

    RETURN
  END SUBROUTINE eseli

  SUBROUTINE esels(Type, Item, Comp, VMIN)
    USE ansys_upf, ONLY : TrackBegin, TrackEnd, RunCommand
    USE ansys_par, ONLY : CMD_MAX_LENG, ERH_FNAME_LEN, PARMSIZE, STRING_MAX_LENG

    IMPLICIT NONE
    ! Purpose: select elements
    !
    ! Parameter:
    ! in/out Name          Task
    ! in     Type          Label identifying the type of select:
    !                        S -- Select a new set (default).
    !                        R -- Reselect a set from the current set.
    !                        A -- Additionally select a set and extend
    !                             the current set.
    !                        U -- Unselect a set from the current set.
    !                        ALL -- Restore the full set.
    !                        NONE -- Unselect the full set.
    !                        INVE -- Invert the current set (selected
    !                                becomes unselected and vice versa).
    ! The following fields are used only with Type = S, R, A, or U:
    ! in     Item          Label identifying data, see ESEL
    !                      command. Some items also require a component
    !                      label. Defaults to ELEM. If Item = STRA
    !                      (straightened), elements are selected whose
    !                      midside nodes do not conform to the curved
    !                      line or non-flat area on which they should
    !                      lie. (Such elements are sometimes formed
    !                      during volume meshing [VMESH] in an attempt
    !                      to avoid excessive element distortion.) You
    !                      should graphically examine any such elements
    !                      to evaluate their possible effect on solution
    !                      accuracy.
    ! in     Comp          Component of the item (if required). For valid
    !                      component labels see ESEL command.
    ! in     VMIN          Minimum value of item range. Ranges are
    !                      element numbers, attribute numbers, load
    !                      values, or result values as appropriate for
    !                      the item.
    ! ----------------------------------------------------------------------
    ! Created: 2007-05-29 hoel
    ! ======================================================================
    CHARACTER(LEN=*), INTENT(IN) :: Type
    CHARACTER(LEN=*), INTENT(IN) :: Item
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: Comp
    CHARACTER(LEN=*), INTENT(IN) :: VMIN

    CHARACTER(LEN=LEN(type)) :: utype
    CHARACTER(LEN=STRING_MAX_LENG) :: uitem
    CHARACTER(LEN=STRING_MAX_LENG) :: uvmin
    INTEGER :: iErr

    CHARACTER(LEN=CMD_MAX_LENG) :: cmd

    CALL TrackBegin('glans:esels')

    utype = upcase(type)

    IF ((utype.NE.'S').AND. &
         & (utype.NE.'R').AND. &
         & (utype.NE.'A').AND. &
         & (utype.NE.'U').AND. &
         & (utype.NE.'ALL').AND. &
         & (utype.NE.'NONE').AND. &
         & (utype.NE.'INVE')) THEN
       CALL ans_fatal(fname, __LINE__, 'glansys', &
            & 'invalid type: '//TRIM(utype)//'.')
    END IF

    IF (LEN(item).GT.STRING_MAX_LENG) THEN
       CALL ans_fatal(fname, __LINE__, 'glansys', &
            & 'length of value item greater then STRING_MAX_LENG.')
    END IF
    uitem = upcase(item)
    IF ((uitem.NE.'ELEM').AND. &
         & (uitem.NE.'ADJ').AND. &
         & (uitem.NE.'TYPE').AND. &
         & (uitem.NE.'ENAME').AND. &
         & (uitem.NE.'MAT').AND. &
         & (uitem.NE.'REAL').AND. &
         & (uitem.NE.'ESYS').AND. &
         & (uitem.NE.'PART').AND. &
         & (uitem.NE.'LIVE').AND. &
         & (uitem.NE.'LAYER').AND. &
         & (uitem.NE.'SEC').AND. &
         & (uitem.NE.'PINC').AND. &
         & (uitem.NE.'PEXC').AND. &
         & (uitem.NE.'STRA').AND. &
         & (uitem.NE.'SFE').AND. &
         & (uitem.NE.'BFE').AND. &
         & (uitem.NE.'PATH')) THEN
       CALL ans_fatal(fname, __LINE__, 'glansys', &
            & 'invalid item: '//TRIM(uitem)//'.')
    END IF
    IF (PRESENT(comp)) THEN
       IF ((uitem.EQ.'ELEM').AND. &
            & (uitem.EQ.'ADJ').AND. &
            & (uitem.EQ.'TYPE').AND. &
            & (uitem.EQ.'ENAME').AND. &
            & (uitem.EQ.'MAT').AND. &
            & (uitem.EQ.'REAL').AND. &
            & (uitem.EQ.'ESYS').AND. &
            & (uitem.EQ.'PART').AND. &
            & (uitem.EQ.'LIVE').AND. &
            & (uitem.EQ.'LAYER').AND. &
            & (uitem.EQ.'SEC').AND. &
            & (uitem.EQ.'PINC').AND. &
            & (uitem.EQ.'PEXC').AND. &
            & (uitem.EQ.'STRA')) THEN
          CALL ans_fatal(fname, __LINE__, 'glansys', &
               & 'invalid input: component not allowed '// &
               & 'with item '//uitem//'.')
       ELSE IF (uitem.EQ.'SFE') THEN
          CALL ans_fatal(fname, __LINE__, 'glansys', &
               & 'item '//uitem//' not yet supported.')
       ELSE IF (uitem.EQ.'BFE') THEN
          CALL ans_fatal(fname, __LINE__, 'glansys', &
               & 'item '//uitem//' not yet supported.')
       ELSE IF (uitem.EQ.'PATH') THEN
          CALL ans_fatal(fname, __LINE__, 'glansys', &
               & 'item '//uitem//' not yet supported.')
       END IF
    END IF

    cmd = 'ESEL'
    WRITE(cmd,200) TRIM(cmd), TRIM(Type)
    WRITE(cmd,200) TRIM(cmd), TRIM(Item)
    IF (PRESENT(Comp)) THEN
       WRITE(cmd,200) TRIM(cmd), TRIM(Comp)
    ELSE
       WRITE(cmd,200) TRIM(cmd), ''
    END IF
    WRITE(cmd,200) TRIM(cmd), VMIN

200 FORMAT(A,','A)

    iErr = RunCommand(LEN_TRIM(cmd), cmd)

    CALL TrackEnd('glans:esels')

    RETURN
  END SUBROUTINE esels

  SUBROUTINE nsel(Type, Item, Comp, VMIN, VMAX, VINC, KABS)
    USE ansys_upf, ONLY : TrackBegin, TrackEnd, RunCommand
    USE ansys_par, ONLY : CMD_MAX_LENG, ERH_FATAL, ERH_FNAME_LEN, PARMSIZE

    IMPLICIT NONE
    ! Purpose: select nodes
    !
    ! Parameter:
    ! in/out Name          Task
    ! in     Type          Label identifying the type of select:
    !                        S -- Select a new set (default).
    !                        R -- Reselect a set from the current set.
    !                        A -- Additionally select a set and extend
    !                             the current set.
    !                        U -- Unselect a set from the current set.
    !                        ALL -- Restore the full set.
    !                        NONE -- Unselect the full set.
    !                        INVE -- Invert the current set (selected
    !                                becomes unselected and vice versa).
    !                        STAT -- Display the current select status.
    ! The following fields are used only with Type = S, R, A, or U:
    ! in     Item          Label identifying data. Valid item labels
    !                      described in the NSEL documentation Defaults
    !                      to NODE.
    ! in     Comp          Component of the item (if required). Valid
    !                      component labels are desribed in the NSEL
    !                      docu.
    ! in     VMIN          Minimum value of item range. Ranges are node
    !                      numbers, set numbers, coordinate values, load
    !                      values, or result values as appropriate for
    !                      the item.
    ! in     VMAX          Maximum value of item range. VMAX defaults to
    !                      VMIN for input values. For result values,
    !                      VMAX defaults to infinity if VMIN is
    !                      positive, or to zero if VMIN is negative.
    ! in     VINC          Value increment within range. Used only with
    !                      integer ranges (such as for node and set
    !                      numbers). Defaults to 1. VINC cannot be
    !                      negative.
    ! in     KABS          Absolute value key:
    !                        0 -- Check sign of value during selection.
    !                        1 -- Use absolute value during selection
    !                             (sign ignored).
    ! ----------------------------------------------------------------------
    ! Created: 2007-05-29  hoel
    ! ======================================================================

    CHARACTER(LEN=*) :: Type
    CHARACTER(LEN=*), OPTIONAL :: Item
    CHARACTER(LEN=*), OPTIONAL :: Comp
    INTEGER, OPTIONAL :: VMIN
    INTEGER, OPTIONAL :: VMAX
    INTEGER, OPTIONAL :: VINC
    INTEGER, OPTIONAL :: KABS

    CHARACTER(LEN=LEN(type)) :: utype
    CHARACTER(LEN=128) :: uitem
    INTEGER :: iErr

    CHARACTER(LEN=CMD_MAX_LENG) :: cmd

    CHARACTER(LEN=ERH_FNAME_LEN), PARAMETER :: fname=__FILE__

    CALL TrackBegin('glans:nsel')

    utype = upcase(type)

    IF ((utype.NE.'S').AND. &
         & (utype.NE.'R').AND. &
         & (utype.NE.'A').AND. &
         & (utype.NE.'U').AND. &
         & (utype.NE.'ALL').AND. &
         & (utype.NE.'NONE').AND. &
         & (utype.NE.'INVE')) THEN
       CALL ans_fatal(fname, __LINE__, 'glansys', &
            & 'invalid type: '//TRIM(utype)//'.')
    END IF

    IF (PRESENT(Item)) THEN
       IF (LEN(item).GT.128) THEN
          CALL ans_fatal(fname, __LINE__, 'glansys', &
               & 'length of value item greater then 129.')
       END IF
       uitem = upcase(item)
       IF ( (uitem.NE.'NODE').AND. &
            & (uitem.NE.'EXT').AND. &
            & (uitem.NE.'LOC').AND. &
            & (uitem.NE.'ANG').AND. &
            & (uitem.NE.'M').AND. &
            & (uitem.NE.'CP').AND. &
            & (uitem.NE.'CE').AND. &
            & (uitem.NE.'D').AND. &
            & (uitem.NE.'F').AND. &
            & (uitem.NE.'BF')) THEN
          CALL ans_fatal(fname, __LINE__, 'glansys', &
               & 'invalid item: '//TRIM(uitem)//'.')
       END IF
       IF (PRESENT(comp)) THEN
          CALL ans_fatal(fname, __LINE__, 'glansys', &
               & 'component not yet allowed.')
       END IF
    END IF

    cmd = 'NSEL'
    WRITE(cmd,200) TRIM(cmd), TRIM(Type)
    IF (PRESENT(Item)) THEN
       WRITE(cmd,200) TRIM(cmd), TRIM(Item)
    ELSE
       WRITE(cmd,200) TRIM(cmd), ''
    END IF
    IF (PRESENT(Comp)) THEN
       WRITE(cmd,200) TRIM(cmd), TRIM(Comp)
    ELSE
       WRITE(cmd,200) TRIM(cmd), ''
    END IF
    IF (PRESENT(VMIN)) THEN
       WRITE(cmd,201) TRIM(cmd), VMIN
    ELSE
       WRITE(cmd,200) TRIM(cmd), ''
    END IF
    IF (PRESENT(VMAX)) THEN
       WRITE(cmd,201) TRIM(cmd), VMAX
    ELSE
       WRITE(cmd,200) TRIM(cmd), ''
    END IF
    IF (PRESENT(VINC)) THEN
       WRITE(cmd,201) TRIM(cmd), VINC
    ELSE
       WRITE(cmd,200) TRIM(cmd), ''
    END IF
    IF (PRESENT(KABS)) THEN
       WRITE(cmd,201) TRIM(cmd), KABS
    ELSE
       WRITE(cmd,200) TRIM(cmd), ''
    END IF

200 FORMAT(A,','A)
201 FORMAT(A,','I)

    iErr = RunCommand(LEN_TRIM(cmd), cmd)

    CALL TrackEnd('glans:nsel')

    RETURN
  END SUBROUTINE nsel

  SUBROUTINE doDebugLoc(func, file, line)

#ifdef __INTEL_COMPILER
    USE ifcore, ONLY : COMMITQQ
#endif
    USE ansys_upf, ONLY : wrinqr
    USE ansys_par, ONLY : WR_OUTPUT

    IMPLICIT NONE
    ! Purpose:
    ! Generate debugging output with sourcefilename, current line number
    ! and name of current function.
    ! Parameter:
    ! in/out Name          Task
    ! in     func          name of function to log
    ! in     file          source file where function is defined (__FILE__)
    ! in     line          current line number in source file (__LINE__)
    ! ----------------------------------------------------------------------
    ! Created: 2007-05-31  hoel
    ! ======================================================================
    CHARACTER(LEN=*) :: func
    CHARACTER(LEN=*) :: file
    INTEGER :: line
#ifdef __INTEL_COMPILER
    INTEGER :: res
#endif
    INTEGER :: stderr

    stderr = wrinqr(WR_OUTPUT)
    WRITE(stderr,100) file, line, func
100 FORMAT (X,A,':',I4,':',A)
#ifdef __INTEL_COMPILER
    res = COMMITQQ(stderr)
#else
    CALL flush(stderr)
#endif

  END SUBROUTINE doDebugLoc

  FUNCTION s_gets(fname, libname, line, errlvl, &
       & value, Entity, ENTNUM, Item1, IT1NUM, Item2, IT2NUM) &
       & RESULT(flag)
    IMPLICIT NONE
    ! Purpose:
    !
    ! Parameter:
    ! in/out Name          Task
    ! in     fname         fname at calling for error reporting
    ! in     libname       library name for error reporting
    ! in     line          line number at calling for error reporting
    ! in     errlvl        error level for error reporting
    ! in     Entity
    ! in     ENTNUM
    ! in     Item1
    ! in     IT1NUM
    ! in     Item2
    ! in     IT2NUM
    ! ----------------------------------------------------------------------
    ! Created: 2017-02-22  hoel
    ! ======================================================================
    LOGICAL :: flag
    CHARACTER(LEN=ERH_FNAME_LEN), INTENT(IN) :: fname
    CHARACTER(LEN=*), INTENT(IN) :: libname
    INTEGER, INTENT(IN) :: line
    INTEGER, INTENT(IN) :: errlvl
    CHARACTER(LEN=STRING_MAX_LENG), INTENT(OUT) :: value
    CHARACTER(LEN=4), INTENT(IN) :: Entity
    INTEGER, INTENT(IN) :: ENTNUM
    CHARACTER(LEN=8), INTENT(IN) :: Item1
    CHARACTER(LEN=8), INTENT(IN), OPTIONAL :: IT1NUM
    CHARACTER(LEN=8), INTENT(IN), OPTIONAL :: Item2
    CHARACTER(LEN=8), INTENT(IN), OPTIONAL :: IT2NUM

    CALL TrackBegin('glans:s_gets')

    derrinfo(1) = ENTNUM

    IF (PRESENT(IT2NUM)) THEN
       flag = gets(value, Entity, ENTNUM, Item1, IT1NUM, Item2, IT2NUM)
    ELSE IF (PRESENT(Item2)) THEN
       flag = gets(value, Entity, ENTNUM, Item1, IT1NUM, Item2)
    ELSE IF (PRESENT(IT1NUM)) THEN
       flag = gets(value, Entity, ENTNUM, Item1, IT1NUM)
    ELSE
       flag = gets(value, Entity, ENTNUM, Item1)
    END IF
    IF (flag) THEN
       CALL erhandler(fname, __LINE__, errlvl, &
               & TRIM(libname)//': Determining ' // trim(Entity) // &
               & ', ' // Item1 // ' for ENTNUM=%d failed.', derrinfo, cerrinfo)
    END IF

    CALL TrackEnd('glans:s_gets')
  END FUNCTION s_gets

  FUNCTION gets(value, Entity, ENTNUM, Item1, IT1NUM, Item2, IT2NUM) &
       & RESULT(flag)
    USE ansys_upf, ONLY : TrackBegin, TrackEnd, RunCommand, erinqr
    USE ansys_par, ONLY : CMD_MAX_LENG, ER_NUMWARNING, ER_NUMERROR, PARMSIZE, &
         & STRING_MAX_LENG
    IMPLICIT NONE
    ! Purpose:
    !
    ! Parameter:
    ! in/out Name          Task
    !        Entity
    !        ENTNUM
    !        Item1
    !        IT1NUM
    !        Item2
    !        IT2NUM
    ! ----------------------------------------------------------------------
    ! Created: 2007-05-29  hoel
    ! ======================================================================
    LOGICAL :: flag
    CHARACTER(LEN=STRING_MAX_LENG), INTENT(OUT) :: value
    CHARACTER(LEN=4), INTENT(IN) :: Entity
    INTEGER, INTENT(IN) :: ENTNUM
    CHARACTER(LEN=8), INTENT(IN) :: Item1
    CHARACTER(LEN=8), INTENT(IN), OPTIONAL :: IT1NUM
    CHARACTER(LEN=8), INTENT(IN), OPTIONAL :: Item2
    CHARACTER(LEN=8), INTENT(IN), OPTIONAL :: IT2NUM

    CHARACTER(LEN=CMD_MAX_LENG) :: cmd
    REAL(KIND=8) :: Dummy
    CHARACTER(LEN=PARMSIZE) :: para
    REAL(KIND=8), DIMENSION(3) :: subc

    INTEGER :: iErr

    INTEGER :: numwrn
    INTEGER :: numerr

    CALL TrackBegin('glans:gets')

    numwrn = erinqr(ER_NUMWARNING)
    numerr = erinqr(ER_NUMERROR)

    value = ''

    WRITE(cmd,100) '*GET,PAR_', TRIM(Entity)
    WRITE(cmd,101) TRIM(cmd), ENTNUM
    WRITE(cmd,100) TRIM(cmd), TRIM(Item1)
    IF (PRESENT(IT1NUM)) THEN
       WRITE(cmd,100) TRIM(cmd), TRIM(IT1NUM)
    ELSE
       WRITE(cmd,102) TRIM(cmd)
    END IF
    IF (PRESENT(Item2)) THEN
       WRITE(cmd,100) TRIM(cmd), TRIM(Item2)
    ELSE
       WRITE(cmd,102) TRIM(cmd)
    END IF
    IF (PRESENT(IT2NUM)) THEN
       WRITE(cmd,100) TRIM(cmd), TRIM(IT2NUM)
    ELSE
       WRITE(cmd,102) TRIM(cmd)
    END IF
    ! WRITE(wrinqr(WR_OUTPUT),*) 'cmd: ', TRIM(cmd)
    iErr = RunCommand(LEN_TRIM(cmd), TRIM(cmd))

    flag = ((erinqr(ER_NUMWARNING) - numwrn) + (erinqr(ER_NUMERROR) - numerr)).NE.0

    IF (flag) THEN
       CALL TrackEnd('glans:gets')
       RETURN
    END IF

    para ='PAR_'
    CALL parevl(para, 0, subc, 2, dummy, value, iErr)

    numwrn = erinqr(ER_NUMWARNING) - numwrn
    numerr = erinqr(ER_NUMERROR) - numerr
    flag = (numwrn + numerr).NE.0

100 FORMAT(A,',',A)
101 FORMAT(A,',',I)
102 FORMAT(A,',')

    CALL TrackEnd('glans:gets')

  END FUNCTION gets

  FUNCTION s_getsi(fname, libname, line, errlvl, &
       & value, Entity, ENTNUM, Item1, IT1NUM, Item2, IT2NUM) &
       & RESULT(flag)
    IMPLICIT NONE
    ! Purpose:
    !
    ! Parameter:
    ! in/out Name          Task
    ! in     fname         fname at calling for error reporting
    ! in     libname       library name for error reporting
    ! in     line          line number at calling for error reporting
    ! in     errlvl        error level for error reporting
    ! in     Entity
    ! in     ENTNUM
    ! in     Item1
    ! in     IT1NUM
    ! in     Item2
    ! in     IT2NUM
    ! ----------------------------------------------------------------------
    ! Created: 2017-02-22  hoel
    ! ======================================================================
    LOGICAL :: flag
    CHARACTER(LEN=ERH_FNAME_LEN), INTENT(IN) :: fname
    CHARACTER(LEN=*), INTENT(IN) :: libname
    INTEGER, INTENT(IN) :: line
    INTEGER, INTENT(IN) :: errlvl
    CHARACTER(LEN=STRING_MAX_LENG) :: value
    CHARACTER(LEN=4), INTENT(IN) :: Entity
    INTEGER, INTENT(IN) :: ENTNUM
    CHARACTER(LEN=8), INTENT(IN) :: Item1
    INTEGER, INTENT(IN) :: IT1NUM
    CHARACTER(LEN=8), INTENT(IN), OPTIONAL :: Item2
    INTEGER, INTENT(IN), OPTIONAL :: IT2NUM

    CALL TrackBegin('glans:s_getsi')

    derrinfo(1) = ENTNUM

    IF (PRESENT(IT2NUM)) THEN
       flag = getsi(value, Entity, ENTNUM, Item1, IT1NUM, Item2, IT2NUM)
    ELSE IF (PRESENT(Item2)) THEN
       flag = getsi(value, Entity, ENTNUM, Item1, IT1NUM, Item2)
    ELSE
       flag = getsi(value, Entity, ENTNUM, Item1, IT1NUM)
    END IF
    IF (flag) THEN
       CALL erhandler(fname, __LINE__, errlvl, &
               & TRIM(libname)//': Determining ' // trim(Entity) // &
               & ', ' // Item1 // ' for ENTNUM=%d failed.', derrinfo, cerrinfo)
    END IF

    CALL TrackEnd('glans:s_getsi')
  END FUNCTION s_getsi

  FUNCTION getsi(value, Entity, ENTNUM, Item1, IT1NUM, Item2, IT2NUM) &
       & RESULT(flag)
    USE ansys_upf, ONLY : TrackBegin, TrackEnd, RunCommand, erinqr
    USE ansys_par, ONLY : CMD_MAX_LENG, ER_NUMWARNING, ER_NUMERROR, &
         & ERH_FATAL, ERH_FNAME_LEN, PARMSIZE, STRING_MAX_LENG
    IMPLICIT NONE
    ! Purpose:
    !
    ! Parameter:
    ! in/out Name          Task
    !        Entity
    !        ENTNUM
    !        Item1
    !        IT1NUM
    !        Item2
    !        IT2NUM
    ! ----------------------------------------------------------------------
    ! Created: 2007-05-29  hoel
    ! ======================================================================
    LOGICAL :: flag
    CHARACTER(LEN=STRING_MAX_LENG) :: value
    CHARACTER(LEN=4), INTENT(IN) :: Entity
    INTEGER, INTENT(IN) :: ENTNUM
    CHARACTER(LEN=8), INTENT(IN) :: Item1
    INTEGER, INTENT(IN) :: IT1NUM
    CHARACTER(LEN=8), INTENT(IN), OPTIONAL :: Item2
    INTEGER, INTENT(IN), OPTIONAL :: IT2NUM

    INTEGER :: numwrn
    INTEGER :: numerr

    CHARACTER(LEN=ERH_FNAME_LEN), PARAMETER :: fname=__FILE__

    CALL TrackBegin('glans:getsi')

    numwrn = erinqr(ER_NUMWARNING)
    numerr = erinqr(ER_NUMERROR)

    CALL ans_fatal(fname, __LINE__, 'glansys', &
         & 'glans:getsi not implemented')

    value = 'empty'

    numwrn = erinqr(ER_NUMWARNING) - numwrn
    numerr = erinqr(ER_NUMERROR) - numerr
    flag = (numwrn + numerr).NE.0

    flag = .TRUE.

    CALL TrackEnd('glans:getsi')

  END FUNCTION getsi

  FUNCTION s_getf(fname, libname, line, errlvl, &
       & value, Entity, ENTNUM, Item1, IT1NUM, Item2, IT2NUM) &
       & RESULT(flag)
    IMPLICIT NONE
    ! Purpose:
    !
    ! Parameter:
    ! in/out Name          Task
    ! in     fname         fname at calling for error reporting
    ! in     libname       library name for error reporting
    ! in     line          line number at calling for error reporting
    ! in     errlvl        error level for error reporting
    ! in     Entity
    ! in     ENTNUM
    ! in     Item1
    ! in     IT1NUM
    ! in     Item2
    ! in     IT2NUM
    ! ----------------------------------------------------------------------
    ! Created: 2017-02-22  hoel
    ! ======================================================================
    LOGICAL :: flag
    CHARACTER(LEN=ERH_FNAME_LEN), INTENT(IN) :: fname
    CHARACTER(LEN=*), INTENT(IN) :: libname
    INTEGER, INTENT(IN) :: line
    INTEGER, INTENT(IN) :: errlvl
    REAL(KIND=8), INTENT(OUT) :: value
    CHARACTER(LEN=4), INTENT(IN) :: Entity
    INTEGER, INTENT(IN) :: ENTNUM
    CHARACTER(LEN=8), INTENT(IN) :: Item1
    CHARACTER(LEN=8), INTENT(IN), OPTIONAL :: IT1NUM
    CHARACTER(LEN=8), INTENT(IN), OPTIONAL :: Item2
    CHARACTER(LEN=8), INTENT(IN), OPTIONAL :: IT2NUM

    CALL TrackBegin('glans:s_getf')

    derrinfo(1) = ENTNUM

    IF (PRESENT(IT2NUM)) THEN
       flag = getf(value, Entity, ENTNUM, Item1, IT1NUM, Item2, IT2NUM)
    ELSE IF (PRESENT(Item2)) THEN
       flag = getf(value, Entity, ENTNUM, Item1, IT1NUM, Item2)
    ELSE IF (PRESENT(IT1NUM)) THEN
       flag = getf(value, Entity, ENTNUM, Item1, IT1NUM)
    ELSE
       flag = getf(value, Entity, ENTNUM, Item1)
    END IF
    IF (flag) THEN
       CALL erhandler(fname, __LINE__, errlvl, &
               & TRIM(libname)//': Determining ' // trim(Entity) // &
               & ', ' // Item1 // ' for ENTNUM=%d failed.', derrinfo, cerrinfo)
    END IF

    CALL TrackEnd('glans:s_getf')
  END FUNCTION s_getf

  FUNCTION getf(value, Entity, ENTNUM, Item1, IT1NUM, Item2, IT2NUM) &
       & RESULT(flag)
    USE ansys_upf, ONLY : TrackBegin, TrackEnd, RunCommand, erinqr
    USE ansys_par, ONLY : CMD_MAX_LENG, ER_NUMWARNING, ER_NUMERROR, PARMSIZE, &
         & STRING_MAX_LENG
    IMPLICIT NONE
    ! Purpose:
    !
    ! Parameter:
    ! in/out Name          Task
    !        Entity
    !        ENTNUM
    !        Item1
    !        IT1NUM
    !        Item2
    !        IT2NUM
    ! ----------------------------------------------------------------------
    ! Created: 2007-05-29  hoel
    ! ======================================================================
    LOGICAL :: flag
    REAL(KIND=8), INTENT(OUT) :: value
    CHARACTER(LEN=4), INTENT(IN) :: Entity
    INTEGER, INTENT(IN) :: ENTNUM
    CHARACTER(LEN=8), INTENT(IN) :: Item1
    CHARACTER(LEN=8), INTENT(IN), OPTIONAL :: IT1NUM
    CHARACTER(LEN=8), INTENT(IN), OPTIONAL :: Item2
    CHARACTER(LEN=8), INTENT(IN), OPTIONAL :: IT2NUM

    CHARACTER(LEN=CMD_MAX_LENG) :: cmd
    CHARACTER(LEN=STRING_MAX_LENG) :: dummy
    CHARACTER(LEN=PARMSIZE) :: para
    REAL(KIND=8), DIMENSION(3) :: subc

    INTEGER :: iErr

    INTEGER :: numwrn
    INTEGER :: numerr

    CALL TrackBegin('glans:getf')

    value = 0d0

    numwrn = erinqr(ER_NUMWARNING)
    numerr = erinqr(ER_NUMERROR)

    WRITE(cmd,100) '*GET,PAR_', TRIM(Entity)
    WRITE(cmd,101) TRIM(cmd), ENTNUM
    WRITE(cmd,100) TRIM(cmd), TRIM(Item1)
    IF (PRESENT(IT1NUM)) THEN
       WRITE(cmd,100) TRIM(cmd), TRIM(IT1NUM)
    ELSE
       WRITE(cmd,100) TRIM(cmd), ''
    END IF
    IF (PRESENT(Item2)) THEN
       WRITE(cmd,100) TRIM(cmd), TRIM(Item2)
    ELSE
       WRITE(cmd,100) TRIM(cmd), ''
    END IF
    IF (PRESENT(IT2NUM)) THEN
       WRITE(cmd,100) TRIM(cmd), TRIM(IT2NUM)
    ELSE
       WRITE(cmd,100) TRIM(cmd), ''
    END IF
    ! WRITE(wrinqr(WR_OUTPUT),*) 'cmd: ', TRIM(cmd)
    iErr = RunCommand(LEN_TRIM(cmd), TRIM(cmd))

    flag = ((erinqr(ER_NUMWARNING) - numwrn) + (erinqr(ER_NUMERROR) - numerr)).NE.0

    IF (flag) THEN
       CALL TrackEnd('glans:getf')
       RETURN
    END IF

    para ='PAR_'
    CALL parevl(para, 0, subc, 2, value, dummy, iErr)

    numwrn = erinqr(ER_NUMWARNING) - numwrn
    numerr = erinqr(ER_NUMERROR) - numerr
    flag = (numwrn + numerr).NE.0

100 FORMAT(A,',',A)
101 FORMAT(A,',',I)

    CALL TrackEnd('glans:getf')

  END FUNCTION getf

  FUNCTION s_getfs(fname, libname, line, errlvl, &
       & value, Entity, ENTNUM, Item1, IT1NUM, Item2, IT2NUM) &
       & RESULT(flag)
    IMPLICIT NONE
    ! Purpose:
    !
    ! Parameter:
    ! in/out Name          Task
    ! in     fname         fname at calling for error reporting
    ! in     libname       library name for error reporting
    ! in     line          line number at calling for error reporting
    ! in     errlvl        error level for error reporting
    ! in     Entity
    ! in     ENTNUM
    ! in     Item1
    ! in     IT1NUM
    ! in     Item2
    ! in     IT2NUM
    ! ----------------------------------------------------------------------
    ! Created: 2017-02-22  hoel
    ! ======================================================================
    LOGICAL :: flag
    CHARACTER(LEN=ERH_FNAME_LEN), INTENT(IN) :: fname
    CHARACTER(LEN=*), INTENT(IN) :: libname
    INTEGER, INTENT(IN) :: line
    INTEGER, INTENT(IN) :: errlvl
    REAL(KIND=8), INTENT(OUT) :: value
    CHARACTER(LEN=4), INTENT(IN) :: Entity
    CHARACTER(LEN=PARMSIZE), INTENT(IN) :: ENTNUM
    CHARACTER(LEN=8), INTENT(IN) :: Item1
    CHARACTER(LEN=8), INTENT(IN), OPTIONAL :: IT1NUM
    CHARACTER(LEN=8), INTENT(IN), OPTIONAL :: Item2
    CHARACTER(LEN=8), INTENT(IN), OPTIONAL :: IT2NUM

    CALL TrackBegin('glans:s_getfs')

    cerrinfo(1) = ENTNUM

    IF (PRESENT(IT2NUM)) THEN
       flag = getfs(value, Entity, ENTNUM, Item1, IT1NUM, Item2, IT2NUM)
    ELSE IF (PRESENT(Item2)) THEN
       flag = getfs(value, Entity, ENTNUM, Item1, IT1NUM, Item2)
    ELSE IF (PRESENT(IT1NUM)) THEN
       flag = getfs(value, Entity, ENTNUM, Item1, IT1NUM)
    ELSE
       flag = getfs(value, Entity, ENTNUM, Item1)
    END IF
    IF (flag) THEN
       CALL erhandler(fname, __LINE__, errlvl, &
               & TRIM(libname)//': Determining ' // trim(Entity) // &
               & ', ' // Item1 // ' for ENTNUM=%s failed.', derrinfo, cerrinfo)
    END IF

    CALL TrackEnd('glans:s_getfs')
  END FUNCTION s_getfs

  FUNCTION getfs(value, Entity, ENTNUM, Item1, IT1NUM, Item2, IT2NUM) &
       & RESULT(flag)
    USE ansys_upf, ONLY : TrackBegin, TrackEnd, RunCommand, erinqr
    USE ansys_par, ONLY : CMD_MAX_LENG, ER_NUMWARNING, ER_NUMERROR, &
         & PARMSIZE, STRING_MAX_LENG
    IMPLICIT NONE
    ! Purpose:
    !
    ! Parameter:
    ! in/out Name          Task
    !        Entity
    !        ENTNUM
    !        Item1
    !        IT1NUM
    !        Item2
    !        IT2NUM
    ! ----------------------------------------------------------------------
    ! Created: 2007-05-29  hoel
    ! ======================================================================
    LOGICAL :: flag
    REAL(KIND=8), INTENT(OUT) :: value
    CHARACTER(LEN=4), INTENT(IN) :: Entity
    CHARACTER(LEN=PARMSIZE), INTENT(IN) :: ENTNUM
    CHARACTER(LEN=8), INTENT(IN) :: Item1
    CHARACTER(LEN=8), INTENT(IN), OPTIONAL :: IT1NUM
    CHARACTER(LEN=8), INTENT(IN), OPTIONAL :: Item2
    CHARACTER(LEN=8), INTENT(IN), OPTIONAL :: IT2NUM

    CHARACTER(LEN=CMD_MAX_LENG) :: cmd
    CHARACTER(LEN=STRING_MAX_LENG) :: dummy
    CHARACTER(LEN=PARMSIZE) :: para
    REAL(KIND=8), DIMENSION(3) :: subc

    INTEGER :: numwrn
    INTEGER :: numerr

    INTEGER :: iErr

    CALL TrackBegin('glans:getfs')

    value = 0d0

    numwrn = erinqr(ER_NUMWARNING)
    numerr = erinqr(ER_NUMERROR)

    WRITE(cmd,100) '*GET,PAR_', TRIM(Entity)
    WRITE(cmd,100) TRIM(cmd), ENTNUM
    WRITE(cmd,100) TRIM(cmd), TRIM(Item1)
    IF (PRESENT(IT1NUM)) THEN
       WRITE(cmd,100) TRIM(cmd), TRIM(IT1NUM)
    ELSE
       WRITE(cmd,102) TRIM(cmd)
    END IF
    IF (PRESENT(Item2)) THEN
       WRITE(cmd,100) TRIM(cmd), TRIM(Item2)
    ELSE
       WRITE(cmd,102) TRIM(cmd)
    END IF
    IF (PRESENT(IT2NUM)) THEN
       WRITE(cmd,100) TRIM(cmd), TRIM(IT2NUM)
    ELSE
       WRITE(cmd,102) TRIM(cmd)
    END IF
    ! WRITE(wrinqr(WR_OUTPUT),*) 'cmd: ', TRIM(cmd)
    iErr = RunCommand(LEN_TRIM(cmd), TRIM(cmd))

    flag = ((erinqr(ER_NUMWARNING) - numwrn) + (erinqr(ER_NUMERROR) - numerr)).NE.0

    IF (flag) THEN
       CALL TrackEnd('glans:getfs')
       RETURN
    END IF

    para ='PAR_'
    CALL parevl(para, 0, subc, 2, value, dummy, iErr)

    numwrn = erinqr(ER_NUMWARNING) - numwrn
    numerr = erinqr(ER_NUMERROR) - numerr
    flag = (numwrn + numerr).NE.0

100 FORMAT(A,',',A)
102 FORMAT(A,',')

    CALL TrackEnd('glans:getfs')

  END FUNCTION getfs

  FUNCTION s_getfi(fname, libname, line, errlvl, &
       & value, Entity, ENTNUM, Item1, IT1NUM, Item2, IT2NUM) &
       & RESULT(flag)
    IMPLICIT NONE
    ! Purpose:
    !
    ! Parameter:
    ! in/out Name          Task
    ! in     fname         fname at calling for error reporting
    ! in     libname       library name for error reporting
    ! in     line          line number at calling for error reporting
    ! in     errlvl        error level for error reporting
    ! in     Entity
    ! in     ENTNUM
    ! in     Item1
    ! in     IT1NUM
    ! in     Item2
    ! in     IT2NUM
    ! ----------------------------------------------------------------------
    ! Created: 2017-02-22  hoel
    ! ======================================================================
    LOGICAL :: flag
    CHARACTER(LEN=ERH_FNAME_LEN), INTENT(IN) :: fname
    CHARACTER(LEN=*), INTENT(IN) :: libname
    INTEGER, INTENT(IN) :: line
    INTEGER, INTENT(IN) :: errlvl
    REAL(KIND=8) :: value
    CHARACTER(LEN=4), INTENT(IN) :: Entity
    INTEGER, INTENT(IN) :: ENTNUM
    CHARACTER(LEN=8), INTENT(IN) :: Item1
    INTEGER, INTENT(IN) :: IT1NUM
    CHARACTER(LEN=8), INTENT(IN), OPTIONAL :: Item2
    INTEGER, INTENT(IN), OPTIONAL :: IT2NUM

    CALL TrackBegin('glans:s_getfi')

    derrinfo(1) = ENTNUM

    IF (PRESENT(IT2NUM)) THEN
       flag = getfi(value, Entity, ENTNUM, Item1, IT1NUM, Item2, IT2NUM)
    ELSE IF (PRESENT(Item2)) THEN
       flag = getfi(value, Entity, ENTNUM, Item1, IT1NUM, Item2)
    ELSE
       flag = getfi(value, Entity, ENTNUM, Item1, IT1NUM)
    END IF
    IF (flag) THEN
       CALL erhandler(fname, __LINE__, errlvl, &
               & TRIM(libname)//': Determining ' // trim(Entity) // &
               & ', ' // Item1 // ' for ENTNUM=%d failed.', derrinfo, cerrinfo)
    END IF

    CALL TrackEnd('glans:s_getfi')
  END FUNCTION s_getfi

  FUNCTION getfi(value, Entity, ENTNUM, Item1, IT1NUM, Item2, IT2NUM) &
       & RESULT(flag)
    USE ansys_upf, ONLY : TrackBegin, TrackEnd, RunCommand, erinqr, wrinqr
    USE ansys_par, ONLY : CMD_MAX_LENG, ER_NUMWARNING, ER_NUMERROR, ERH_FATAL, &
         & ERH_FNAME_LEN, PARMSIZE, STRING_MAX_LENG, WR_OUTPUT
    IMPLICIT NONE
    ! Purpose:
    !
    ! Parameter:
    ! in/out Name          Task
    !        Entity
    !        ENTNUM
    !        Item1
    !        IT1NUM
    !        Item2
    !        IT2NUM
    ! ----------------------------------------------------------------------
    ! Created: 2007-05-29  hoel
    ! ======================================================================
    LOGICAL :: flag
    REAL(KIND=8) :: value
    CHARACTER(LEN=4), INTENT(IN) :: Entity
    INTEGER, INTENT(IN) :: ENTNUM
    CHARACTER(LEN=8), INTENT(IN) :: Item1
    INTEGER, INTENT(IN) :: IT1NUM
    CHARACTER(LEN=8), INTENT(IN), OPTIONAL :: Item2
    INTEGER, INTENT(IN), OPTIONAL :: IT2NUM

    CHARACTER(LEN=CMD_MAX_LENG) :: cmd
    CHARACTER(LEN=STRING_MAX_LENG) :: dummy
    CHARACTER(LEN=PARMSIZE) :: para
    REAL(KIND=8), DIMENSION(3) :: subc

    CHARACTER(LEN=ERH_FNAME_LEN), PARAMETER :: fname=__FILE__

    INTEGER :: numwrn
    INTEGER :: numerr

    INTEGER :: iErr

    CALL TrackBegin('glans:getfi')

    value = 0d0

    numwrn = erinqr(ER_NUMWARNING)
    numerr = erinqr(ER_NUMERROR)

    WRITE(cmd,100) '*GET,PAR_', TRIM(Entity)
    WRITE(cmd,101) TRIM(cmd), ENTNUM
    WRITE(cmd,100) TRIM(cmd), TRIM(Item1)
    WRITE(cmd,101) TRIM(cmd), IT1NUM
    IF (PRESENT(Item2)) THEN
       WRITE(cmd,100) TRIM(cmd), TRIM(Item2)
    ELSE
       WRITE(cmd,102) TRIM(cmd)
    END IF
    IF (PRESENT(IT2NUM)) THEN
       WRITE(cmd,101) TRIM(cmd), IT2NUM
    ELSE
       WRITE(cmd,102) TRIM(cmd)
    END IF
    ! WRITE(wrinqr(WR_OUTPUT),*) 'cmd: ', TRIM(cmd)
    iErr = RunCommand(LEN_TRIM(cmd), TRIM(cmd))

    flag = ((erinqr(ER_NUMWARNING) - numwrn) + (erinqr(ER_NUMERROR) - numerr)).NE.0

    IF (flag) THEN
       CALL TrackEnd('glans:getfi')
       RETURN
    END IF

    para ='PAR_'
    CALL parevl(para, 0, subc, 2, value, dummy, iErr)

    numwrn = erinqr(ER_NUMWARNING) - numwrn
    numerr = erinqr(ER_NUMERROR) - numerr
    flag = (numwrn + numerr).NE.0

100 FORMAT(A,',',A)
101 FORMAT(A,',',I)
102 FORMAT(A,',')

    CALL TrackEnd('glans:getfi')

  END FUNCTION getfi

  FUNCTION s_geti(fname, libname, line, errlvl, &
       & value, Entity, ENTNUM, Item1, IT1NUM, Item2, IT2NUM) &
       & RESULT(flag)
    IMPLICIT NONE
    ! Purpose:
    !
    ! Parameter:
    ! in/out Name          Task
    ! in     fname         fname at calling for error reporting
    ! in     libname       library name for error reporting
    ! in     line          line number at calling for error reporting
    ! in     errlvl        error level for error reporting
    ! in     Entity
    ! in     ENTNUM
    ! in     Item1
    ! in     IT1NUM
    ! in     Item2
    ! in     IT2NUM
    ! ----------------------------------------------------------------------
    ! Created: 2017-02-22  hoel
    ! ======================================================================
    LOGICAL :: flag
    CHARACTER(LEN=ERH_FNAME_LEN), INTENT(IN) :: fname
    CHARACTER(LEN=*), INTENT(IN) :: libname
    INTEGER, INTENT(IN) :: line
    INTEGER, INTENT(IN) :: errlvl
    INTEGER, INTENT(OUT) :: value
    CHARACTER(LEN=4), INTENT(IN) :: Entity
    INTEGER, INTENT(IN) :: ENTNUM
    CHARACTER(LEN=8), INTENT(IN) :: Item1
    CHARACTER(LEN=8), INTENT(IN), OPTIONAL :: IT1NUM
    CHARACTER(LEN=8), INTENT(IN), OPTIONAL :: Item2
    CHARACTER(LEN=8), INTENT(IN), OPTIONAL :: IT2NUM

    CALL TrackBegin('glans:s_geti')

    derrinfo(1) = ENTNUM

    IF (PRESENT(IT2NUM)) THEN
       flag = geti(value, Entity, ENTNUM, Item1, IT1NUM, Item2, IT2NUM)
    ELSE IF (PRESENT(Item2)) THEN
       flag = geti(value, Entity, ENTNUM, Item1, IT1NUM, Item2)
    ELSE IF (PRESENT(IT1NUM)) THEN
       flag = geti(value, Entity, ENTNUM, Item1, IT1NUM)
    ELSE
       flag = geti(value, Entity, ENTNUM, Item1)
    END IF
    IF (flag) THEN
       CALL erhandler(fname, __LINE__, errlvl, &
               & TRIM(libname)//': Determining ' // trim(Entity) // &
               & ', ' // Item1 // ' for ENTNUM=%d failed.', derrinfo, cerrinfo)
    END IF

    CALL TrackEnd('glans:s_geti')
  END FUNCTION s_geti

  FUNCTION geti(value, Entity, ENTNUM, Item1, IT1NUM, Item2, IT2NUM) &
       & RESULT(flag)
    USE ansys_upf, ONLY : TrackBegin, TrackEnd, RunCommand, erinqr
    USE ansys_par, ONLY : CMD_MAX_LENG, ER_NUMWARNING, ER_NUMERROR, &
         & PARMSIZE, STRING_MAX_LENG
    IMPLICIT NONE
    ! Purpose:
    !
    ! Parameter:
    ! in/out Name          Task
    !        Entity
    !        ENTNUM
    !        Item1
    !        IT1NUM
    !        Item2
    !        IT2NUM
    ! ----------------------------------------------------------------------
    ! Created: 2007-05-29  hoel
    ! ======================================================================
    LOGICAL :: flag
    INTEGER, INTENT(OUT) :: value
    CHARACTER(LEN=4), INTENT(IN) :: Entity
    INTEGER, INTENT(IN) :: ENTNUM
    CHARACTER(LEN=8), INTENT(IN) :: Item1
    CHARACTER(LEN=8), INTENT(IN), OPTIONAL :: IT1NUM
    CHARACTER(LEN=8), INTENT(IN), OPTIONAL :: Item2
    CHARACTER(LEN=8), INTENT(IN), OPTIONAL :: IT2NUM

    REAL(KIND=8) :: fvalue

    CALL TrackBegin('glans:geti')

    flag = getf(fvalue, Entity, ENTNUM, Item1, IT1NUM, Item2, IT2NUM)
    value = INT(fvalue)

    CALL TrackEnd('glans:geti')

  END FUNCTION geti

  FUNCTION s_getis(fname, libname, line, errlvl, &
       & value, Entity, ENTNUM, Item1, IT1NUM, Item2, IT2NUM) &
       & RESULT(flag)
    IMPLICIT NONE
    ! Purpose:
    !
    ! Parameter:
    ! in/out Name          Task
    ! in     fname         fname at calling for error reporting
    ! in     libname       library name for error reporting
    ! in     line          line number at calling for error reporting
    ! in     errlvl        error level for error reporting
    ! in     Entity
    ! in     ENTNUM
    ! in     Item1
    ! in     IT1NUM
    ! in     Item2
    ! in     IT2NUM
    ! ----------------------------------------------------------------------
    ! Created: 2017-02-22  hoel
    ! ======================================================================
    LOGICAL :: flag
    CHARACTER(LEN=ERH_FNAME_LEN), INTENT(IN) :: fname
    CHARACTER(LEN=*), INTENT(IN) :: libname
    INTEGER, INTENT(IN) :: line
    INTEGER, INTENT(IN) :: errlvl
    INTEGER, INTENT(OUT) :: value
    CHARACTER(LEN=4), INTENT(IN) :: Entity
    CHARACTER(LEN=PARMSIZE), INTENT(IN) :: ENTNUM
    CHARACTER(LEN=8), INTENT(IN) :: Item1
    CHARACTER(LEN=8), INTENT(IN), OPTIONAL :: IT1NUM
    CHARACTER(LEN=8), INTENT(IN), OPTIONAL :: Item2
    CHARACTER(LEN=8), INTENT(IN), OPTIONAL :: IT2NUM

    CALL TrackBegin('glans:s_getis')

    cerrinfo(1) = ENTNUM

    IF (PRESENT(IT2NUM)) THEN
       flag = getis(value, Entity, ENTNUM, Item1, IT1NUM, Item2, IT2NUM)
    ELSE IF (PRESENT(Item2)) THEN
       flag = getis(value, Entity, ENTNUM, Item1, IT1NUM, Item2)
    ELSE IF (PRESENT(IT2NUM)) THEN
       flag = getis(value, Entity, ENTNUM, Item1, IT1NUM)
    ELSE
       flag = getis(value, Entity, ENTNUM, Item1)
    END IF
    IF (flag) THEN
       CALL erhandler(fname, __LINE__, errlvl, &
               & TRIM(libname)//': Determining ' // trim(Entity) // &
               & ', ' // Item1 // ' for ENTNUM=%s failed.', derrinfo, cerrinfo)
    END IF

    CALL TrackEnd('glans:s_getis')
  END FUNCTION s_getis

  FUNCTION getis(value, Entity, ENTNUM, Item1, IT1NUM, Item2, IT2NUM) &
       & RESULT(flag)
    USE ansys_upf, ONLY : TrackBegin, TrackEnd, RunCommand, erinqr
    USE ansys_par, ONLY : CMD_MAX_LENG, ER_NUMWARNING, ER_NUMERROR, &
         & PARMSIZE, STRING_MAX_LENG
    IMPLICIT NONE
    ! Purpose:
    !
    ! Parameter:
    ! in/out Name          Task
    !        Entity
    !        ENTNUM
    !        Item1
    !        IT1NUM
    !        Item2
    !        IT2NUM
    ! ----------------------------------------------------------------------
    ! Created: 2007-05-29  hoel
    ! ======================================================================
    LOGICAL :: flag
    INTEGER, INTENT(OUT) :: value
    CHARACTER(LEN=4), INTENT(IN) :: Entity
    CHARACTER(LEN=PARMSIZE), INTENT(IN) :: ENTNUM
    CHARACTER(LEN=8), INTENT(IN) :: Item1
    CHARACTER(LEN=8), INTENT(IN), OPTIONAL :: IT1NUM
    CHARACTER(LEN=8), INTENT(IN), OPTIONAL :: Item2
    CHARACTER(LEN=8), INTENT(IN), OPTIONAL :: IT2NUM

    REAL(KIND=8) :: fvalue

    CALL TrackBegin('glans:getis')

    flag = getfs(fvalue, Entity, ENTNUM, Item1, IT1NUM, Item2, IT2NUM)
    value = INT(fvalue)

    CALL TrackEnd('glans:getis')

  END FUNCTION getis

  FUNCTION s_getii(fname, libname, line, errlvl, &
       & value, Entity, ENTNUM, Item1, IT1NUM, Item2, IT2NUM) &
       & RESULT(flag)
    IMPLICIT NONE
    ! Purpose:
    !
    ! Parameter:
    ! in/out Name          Task
    ! in     fname         fname at calling for error reporting
    ! in     libname       library name for error reporting
    ! in     line          line number at calling for error reporting
    ! in     errlvl        error level for error reporting
    ! in     Entity
    ! in     ENTNUM
    ! in     Item1
    ! in     IT1NUM
    ! in     Item2
    ! in     IT2NUM
    ! ----------------------------------------------------------------------
    ! Created: 2017-02-22  hoel
    ! ======================================================================
    LOGICAL :: flag
    CHARACTER(LEN=ERH_FNAME_LEN), INTENT(IN) :: fname
    CHARACTER(LEN=*), INTENT(IN) :: libname
    INTEGER, INTENT(IN) :: line
    INTEGER, INTENT(IN) :: errlvl
    INTEGER, INTENT(OUT) :: value
    CHARACTER(LEN=4), INTENT(IN) :: Entity
    INTEGER, INTENT(IN) :: ENTNUM
    CHARACTER(LEN=8), INTENT(IN) :: Item1
    INTEGER, INTENT(IN) :: IT1NUM
    CHARACTER(LEN=8), INTENT(IN), OPTIONAL :: Item2
    INTEGER, INTENT(IN), OPTIONAL :: IT2NUM

    CALL TrackBegin('glans:s_getii')

    derrinfo(1) = ENTNUM

    IF (PRESENT(IT2NUM)) THEN
       flag = getii(value, Entity, ENTNUM, Item1, IT1NUM, Item2, IT2NUM)
    ELSE IF (PRESENT(Item2)) THEN
       flag = getii(value, Entity, ENTNUM, Item1, IT1NUM, Item2)
    ELSE
       flag = getii(value, Entity, ENTNUM, Item1, IT1NUM)
    END IF
    IF (flag) THEN
       CALL erhandler(fname, __LINE__, errlvl, &
               & TRIM(libname)//': Determining ' // trim(Entity) // &
               & ', ' // Item1 // ' for ENTNUM=%d failed.', derrinfo, cerrinfo)
    END IF

    CALL TrackEnd('glans:s_getii')
  END FUNCTION s_getii

  FUNCTION getii(value, Entity, ENTNUM, Item1, IT1NUM, Item2, IT2NUM) &
       & RESULT(flag)
    USE ansys_upf, ONLY : TrackBegin, TrackEnd, RunCommand, erinqr
    USE ansys_par, ONLY : CMD_MAX_LENG, ER_NUMWARNING, ER_NUMERROR, &
         & PARMSIZE, STRING_MAX_LENG
    IMPLICIT NONE
    ! Purpose:
    !
    ! Parameter:
    ! in/out Name          Task
    !        Entity
    !        ENTNUM
    !        Item1
    !        IT1NUM
    !        Item2
    !        IT2NUM
    ! ----------------------------------------------------------------------
    ! Created: 2007-05-29  hoel
    ! ======================================================================
    LOGICAL :: flag
    INTEGER, INTENT(OUT) :: value
    CHARACTER(LEN=4), INTENT(IN) :: Entity
    INTEGER, INTENT(IN) :: ENTNUM
    CHARACTER(LEN=8), INTENT(IN) :: Item1
    INTEGER, INTENT(IN) :: IT1NUM
    CHARACTER(LEN=8), INTENT(IN), OPTIONAL :: Item2
    INTEGER, INTENT(IN), OPTIONAL :: IT2NUM

    REAL(KIND=8) :: fvalue

    CALL TrackBegin('glans:getii')

    flag = getfi(fvalue, Entity, ENTNUM, Item1, IT1NUM, Item2, IT2NUM)
    value = INT(fvalue)

    CALL TrackEnd('glans:getii')

  END FUNCTION getii

  ! issue warnings and update message count
  SUBROUTINE message(wcode,n1,n2)
    USE ansys_upf, ONLY : TrackBegin, TrackEnd, erhandler
    USE ansys_par, ONLY : ERH_WARNING, ERH_FNAME_LEN, PARMSIZE, MP_EX, MP_NUXY, MP_DENS
    USE LOCMOD, ONLY : libname
    IMPLICIT NONE
    ! Purpose:
    !
    ! Parameter:
    ! in/out Name          Task
    ! ----------------------------------------------------------------------
    ! Created: 2007-06-01  hoel
    ! ======================================================================
    CHARACTER(LEN=4) :: wcode
    INTEGER n1,n2

    CHARACTER(LEN=ERH_FNAME_LEN), PARAMETER :: fname=__FILE__
    CHARACTER(LEN=1024) :: msg

    CALL TrackBegin('glans:message')
    wcount=wcount+1
    derrinfo(1) = n1
    derrinfo(2) = n2
    IF (wcode.EQ.'comp') THEN
       CALL ans_warn(fname, __LINE__, libname, &
            & ': Not all elements of component %i '// &
            & 'converted !%/'// &
            & '(Check if element types are allowed in present version)')
    ELSE IF (wcode.EQ.'mat ') THEN
       IF (n2.EQ.MP_EX) THEN
          msg = 'no E-Module found in MP %i'
       ELSE IF (n2.EQ.MP_NUXY) THEN
          msg = 'no Poisson ratio found in MP %i'
       ELSE IF (n2.EQ.MP_DENS) THEN
          msg = 'no density found in MP %i'
       ELSE
          msg = 'unspecified material property not found in MP %i'
       END IF
       CALL ans_warn(fname, __LINE__, libname, TRIM(msg))
    ELSE IF (wcode.EQ.'matE') THEN
       CALL ans_warn(fname, __LINE__, libname, 'no E-Module found in MP %i')
    ELSE IF (wcode.EQ.'matP') THEN
       CALL ans_warn(fname, __LINE__, libname, &
            & 'no Poisson ratio found in MP %i')
    ELSE IF (wcode.EQ.'matD') THEN
       CALL ans_warn(fname, __LINE__, libname, &
            & 'no density value found in MP %i')
    ELSE IF (wcode.EQ.'beam') THEN
       CALL ans_warn(fname, __LINE__, libname, &
            & 'beam at nodes %i %i does not have orientation node')
    ELSE IF (wcode.EQ.'pses') THEN
       CALL ans_warn(fname, __LINE__, libname, &
            & 'shells with different thicknesses '// &
            & 'not supported (el. %i). Average value taken.')
    ELSE IF (wcode.EQ.'shel') THEN
       CALL ans_warn(fname, __LINE__, libname, &
            & 'shells with different thicknesses '// &
            & 'not supported (el. %i). Average value taken.')
    END IF

    CALL TrackEnd('glans:message')

  END SUBROUTINE message

END MODULE glans

! Local Variables:
! compile-command:"make -C .. test"
! End:
