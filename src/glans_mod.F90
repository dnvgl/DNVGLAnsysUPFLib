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

  PRIVATE

  INTERFACE get
     MODULE PROCEDURE getf, getfs, getfi, geti, getis, getii, gets, getsi
  END INTERFACE

  INTERFACE esel
     MODULE PROCEDURE esels, eseli
  END INTERFACE

  PUBLIC :: get
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

CONTAINS

  SUBROUTINE ans2bmf_get_d(cmd_str, dout)
    ! reads ANSYS command string 'cmd_str'
    ! puts out double precision parameter 'dout'

    USE ansys_upf, ONLY : TrackBegin, TrackEnd, RunCommand, parevl
#if ANSVER >= 70
    USE ansys_par, ONLY : CMD_MAX_LENG, STRING_MAX_LENG, &
#else
    USE ansys_par, ONLY : SYS_LNG_CMDLN, &
#endif
    & PARMSIZE
    IMPLICIT NONE
    ! Purpose:
    !
    ! Parameter:
    ! in/out Name          Task
    ! ----------------------------------------------------------------------
    ! Created: 2007-06-01  hoel
    ! ======================================================================
    CHARACTER(LEN=*) :: cmd_str
    DOUBLE PRECISION :: dout
    DOUBLE PRECISION, DIMENSION(3) :: subc

#if ANSVER >= 70
    CHARACTER(LEN=CMD_MAX_LENG) :: cmd
    CHARACTER(LEN=STRING_MAX_LENG) :: dummy
#else
    CHARACTER(LEN=SYS_LNG_CMDLN) :: cmd
#endif
    CHARACTER(LEN=PARMSIZE) :: para

    INTEGER :: iErr

    CALL TrackBegin("glans:ans2bmf_get_d")
    cmd = '*GET,PAR_,'//cmd_str
    iErr = RunCommand(LEN_TRIM(cmd), cmd)
    para ='PAR_'

#if   ANSVER >= 70
    CALL parevl(para, 0, subc, 2, dout, dummy, iErr)
#else
    dout = parevl(para, 0, subc, 2, iErr)
#endif
    CALL TrackEnd("glans:ans2bmf_get_d")
  END SUBROUTINE ans2bmf_get_d

  SUBROUTINE ans2bmf_get_s(cmd_str, sout)
    ! reads ANSYS command string 'cmd_str'
    ! writes a string of length 8 into 'sout'
    USE ansys_upf, ONLY : TrackBegin, TrackEnd, RunCommand, parevl
#if ANSVER >= 70
    USE ansys_par, ONLY : CMD_MAX_LENG, STRING_MAX_LENG, &
#else
    USE ansys_par, ONLY : SYS_LNG_CMDLN, &
#endif
    & PARMSIZE
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
    DOUBLE PRECISION res_double
    DOUBLE PRECISION, DIMENSION(3) :: subc
#if ANSVER >= 70
    CHARACTER(LEN=CMD_MAX_LENG) :: cmd
    CHARACTER(LEN=STRING_MAX_LENG) :: dummy
#else
    CHARACTER(LEN=SYS_LNG_CMDLN) :: cmd
    CHARACTER(LEN=8)      res_char8
    EQUIVALENCE      (res_double,res_char8)
#endif
    CHARACTER(LEN=PARMSIZE) :: para

    INTEGER :: iErr

    CALL TrackBegin("glans:ans2bmf_get_s")

    cmd = '*GET,PAR_,'//cmd_str
    iErr = RunCommand(LEN_TRIM(cmd), cmd)
    para ='PAR_'
#if ANSVER < 70
    res_double = parevl(para, 0, subc, 2, iErr)
    sout = res_char8
#else
    CALL parevl(para, 0, subc, 2, res_double, dummy, iErr)
    sout = TRIM(dummy)
#endif

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
#if ANSVER >= 70
   USE ansys_par, ONLY : CMD_MAX_LENG, &
#else
   USE ansys_par, ONLY : SYS_LNG_CMDLN, &
#endif
   & ERH_FATAL, PARMSIZE
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

#if ANSVER >= 70
    CHARACTER(LEN=CMD_MAX_LENG) :: cmd
#else
    CHARACTER(LEN=SYS_LNG_CMDLN) :: cmd
#endif

    ! dataspace for feeding erhandler subroutine
    DOUBLE PRECISION, DIMENSION(10) ::  derrinfo
    CHARACTER(LEN=PARMSIZE), DIMENSION(10) :: cerrinfo

    CHARACTER(LEN=40), PARAMETER :: fname=__FILE__

    CALL TrackBegin('glans:cmselect')

    umode = upcase(mode)

    IF ((umode.NE.'S').AND. &
         & (umode.NE.'R').AND. &
         & (umode.NE.'A').AND. &
         & (umode.NE.'U').AND. &
         & (umode.NE.'ALL').AND. &
         & (umode.NE.'NONE')) THEN
       CALL erhandler(fname, __LINE__, ERH_FATAL, &
            & 'glansys: invalid mode: '//TRIM(umode)//'.', &
            & derrinfo, cerrinfo)
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
#if ANSVER >= 70
    USE ansys_par, ONLY : CMD_MAX_LENG, &
#else
    USE ansys_par, ONLY : SYS_LNG_CMDLN, &
#endif
    & ERH_FATAL, PARMSIZE
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

#if ANSVER >= 70
    CHARACTER(LEN=CMD_MAX_LENG) :: cmd
#else
    CHARACTER(LEN=SYS_LNG_CMDLN) :: cmd
#endif

    ! dataspace for feeding erhandler subroutine
    DOUBLE PRECISION, DIMENSION(10) ::  derrinfo
    CHARACTER(LEN=PARMSIZE), DIMENSION(10) :: cerrinfo

    CHARACTER(LEN=40), PARAMETER :: fname=__FILE__

    CALL TrackBegin('glans:eseli')

    utype = upcase(type)

    IF ((utype.NE.'S').AND. &
         & (utype.NE.'R').AND. &
         & (utype.NE.'A').AND. &
         & (utype.NE.'U').AND. &
         & (utype.NE.'ALL').AND. &
         & (utype.NE.'NONE').AND. &
         & (utype.NE.'INVE')) THEN
       CALL erhandler(fname, __LINE__, ERH_FATAL, &
            & 'glansys: invalid type: '//TRIM(utype)//'.', &
            & derrinfo, cerrinfo)
    END IF

    IF (PRESENT(Item)) THEN
       IF (LEN(item).GT.128) THEN
          CALL erhandler(fname, __LINE__, ERH_FATAL, &
               & 'glansys: length of value item greater then 129.', &
               & derrinfo, cerrinfo)
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
          CALL erhandler(fname, __LINE__, ERH_FATAL, &
               & 'glansys: invalid item: '//TRIM(uitem)//'.', &
               & derrinfo, cerrinfo)
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
             CALL erhandler(fname, __LINE__, ERH_FATAL, &
                  & 'glansys: invalid input: component not allowed '// &
                  & 'with item '//uitem//'.', &
                  & derrinfo, cerrinfo)
          ELSE IF (uitem.EQ.'SFE') THEN
             CALL erhandler(fname, __LINE__, ERH_FATAL, &
                  & 'glansys: item '//uitem//' not yet supported.', &
                  & derrinfo, cerrinfo)
          ELSE IF (uitem.EQ.'BFE') THEN
             CALL erhandler(fname, __LINE__, ERH_FATAL, &
                  & 'glansys: item '//uitem//' not yet supported.', &
                  & derrinfo, cerrinfo)
          ELSE IF (uitem.EQ.'PATH') THEN
             CALL erhandler(fname, __LINE__, ERH_FATAL, &
                  & 'glansys: item '//uitem//' not yet supported.', &
                  & derrinfo, cerrinfo)
          END IF
       END IF
    ELSE
       IF (PRESENT(comp)) THEN
          CALL erhandler(fname, __LINE__, ERH_FATAL, &
               & 'glansys: invaid call: comp given, but no item.', &
               & derrinfo, cerrinfo)
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
    USE ansys_upf, ONLY : TrackBegin, TrackEnd, RunCommand, erhandler
#if ANSVER >= 70
    USE ansys_par, ONLY : CMD_MAX_LENG, &
#else
    USE ansys_par, ONLY : SYS_LNG_CMDLN, &
#endif
    & ERH_FATAL, PARMSIZE, STRING_MAX_LENG
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

#if ANSVER >= 70
    CHARACTER(LEN=CMD_MAX_LENG) :: cmd
#else
    CHARACTER(LEN=SYS_LNG_CMDLN) :: cmd
#endif

    ! dataspace for feeding erhandler subroutine
    DOUBLE PRECISION, DIMENSION(10) ::  derrinfo
    CHARACTER(LEN=PARMSIZE), DIMENSION(10) :: cerrinfo

    CHARACTER(LEN=40), PARAMETER :: fname=__FILE__

    CALL TrackBegin('glans:esels')

    utype = upcase(type)

    IF ((utype.NE.'S').AND. &
         & (utype.NE.'R').AND. &
         & (utype.NE.'A').AND. &
         & (utype.NE.'U').AND. &
         & (utype.NE.'ALL').AND. &
         & (utype.NE.'NONE').AND. &
         & (utype.NE.'INVE')) THEN
       CALL erhandler(fname, __LINE__, ERH_FATAL, &
            & 'glansys: invalid type: '//TRIM(utype)//'.', &
            & derrinfo, cerrinfo)
    END IF

    IF (LEN(item).GT.STRING_MAX_LENG) THEN
       CALL erhandler(fname, __LINE__, ERH_FATAL, &
            & 'glansys: length of value item greater then STRING_MAX_LENG.', &
            & derrinfo, cerrinfo)
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
       CALL erhandler(fname, __LINE__, ERH_FATAL, &
            & 'glansys: invalid item: '//TRIM(uitem)//'.', &
            & derrinfo, cerrinfo)
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
          CALL erhandler(fname, __LINE__, ERH_FATAL, &
               & 'glansys: invalid input: component not allowed '// &
               & 'with item '//uitem//'.', &
               & derrinfo, cerrinfo)
       ELSE IF (uitem.EQ.'SFE') THEN
          CALL erhandler(fname, __LINE__, ERH_FATAL, &
               & 'glansys: item '//uitem//' not yet supported.', &
               & derrinfo, cerrinfo)
       ELSE IF (uitem.EQ.'BFE') THEN
          CALL erhandler(fname, __LINE__, ERH_FATAL, &
               & 'glansys: item '//uitem//' not yet supported.', &
               & derrinfo, cerrinfo)
       ELSE IF (uitem.EQ.'PATH') THEN
          CALL erhandler(fname, __LINE__, ERH_FATAL, &
               & 'glansys: item '//uitem//' not yet supported.', &
               & derrinfo, cerrinfo)
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
    USE ansys_upf, ONLY : TrackBegin, TrackEnd, RunCommand, erhandler
#if ANSVER >= 70
    USE ansys_par, ONLY : CMD_MAX_LENG, &
#else
    USE ansys_par, ONLY : SYS_LNG_CMDLN, &
#endif
    & ERH_FATAL, PARMSIZE
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

#if ANSVER >= 70
    CHARACTER(LEN=CMD_MAX_LENG) :: cmd
#else
    CHARACTER(LEN=SYS_LNG_CMDLN) :: cmd
#endif

    ! dataspace for feeding erhandler subroutine
    DOUBLE PRECISION, DIMENSION(10) ::  derrinfo
    CHARACTER(LEN=PARMSIZE), DIMENSION(10) :: cerrinfo

    CHARACTER(LEN=40), PARAMETER :: fname=__FILE__

    CALL TrackBegin('glans:nsel')

    utype = upcase(type)

    IF ((utype.NE.'S').AND. &
         & (utype.NE.'R').AND. &
         & (utype.NE.'A').AND. &
         & (utype.NE.'U').AND. &
         & (utype.NE.'ALL').AND. &
         & (utype.NE.'NONE').AND. &
         & (utype.NE.'INVE')) THEN
       CALL erhandler(fname, __LINE__, ERH_FATAL, &
            & 'glansys: invalid type: '//TRIM(utype)//'.', &
            & derrinfo, cerrinfo)
    END IF

    IF (PRESENT(Item)) THEN
       IF (LEN(item).GT.128) THEN
          CALL erhandler(fname, __LINE__, ERH_FATAL, &
               & 'glansys: length of value item greater then 129.', &
               & derrinfo, cerrinfo)
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
          CALL erhandler(fname, __LINE__, ERH_FATAL, &
               & 'glansys: invalid item: '//TRIM(uitem)//'.', &
               & derrinfo, cerrinfo)
       END IF
       IF (PRESENT(comp)) THEN
          CALL erhandler(fname, __LINE__, ERH_FATAL, &
               & 'glansys: component not yet allowed.', &
               & derrinfo, cerrinfo)
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

  FUNCTION gets(value, Entity, ENTNUM, Item1, IT1NUM, Item2, IT2NUM) &
       & RESULT(flag)
    USE ansys_upf, ONLY : TrackBegin, TrackEnd, RunCommand, erinqr
#if ANSVER >= 70
    USE ansys_par, ONLY : CMD_MAX_LENG, &
#else
    USE ansys_par, ONLY : SYS_LNG_CMDLN, &
#endif
    & ER_NUMWARNING, ER_NUMERROR, PARMSIZE, STRING_MAX_LENG
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

#if ANSVER >= 70
    CHARACTER(LEN=CMD_MAX_LENG) :: cmd
    DOUBLE PRECISION :: Dummy
#else
    CHARACTER(LEN=SYS_LNG_CMDLN) :: cmd
    CHARACTER(LEN=8) :: res_char8
    DOUBLE PRECISION :: res_double
    EQUIVALENCE      (res_double, res_char8)
#endif
    CHARACTER(LEN=PARMSIZE) :: para
    DOUBLE PRECISION, DIMENSION(3) :: subc

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
    !WRITE(wrinqr(WR_OUTPUT),*) 'cmd: ', TRIM(cmd)
    iErr = RunCommand(LEN_TRIM(cmd), TRIM(cmd))

    flag = ((erinqr(ER_NUMWARNING) - numwrn) + (erinqr(ER_NUMERROR) - numerr)).NE.0

    IF (flag) THEN
       CALL TrackEnd('glans:gets')
       RETURN
    END IF

    para ='PAR_'
#if ANSVER >= 70
    CALL parevl(para, 0, subc, 2, dummy, value, iErr)
#else
    res_double = parevl(para, 0, subc, 2, iErr)
    value = TRIM(res_char8)
#endif

    numwrn = erinqr(ER_NUMWARNING) - numwrn
    numerr = erinqr(ER_NUMERROR) - numerr
    flag = (numwrn + numerr).NE.0

100 FORMAT(A,',',A)
101 FORMAT(A,',',I)

    CALL TrackEnd('glans:gets')

  END FUNCTION gets

  FUNCTION getsi(value, Entity, ENTNUM, Item1, IT1NUM, Item2, IT2NUM) &
       & RESULT(flag)
    USE ansys_upf, ONLY : TrackBegin, TrackEnd, RunCommand, erinqr
#if ANSVER >= 70
    USE ansys_par, ONLY : CMD_MAX_LENG, &
#else
    USE ansys_par, ONLY : SYS_LNG_CMDLN, &
#endif
    & ER_NUMWARNING, ER_NUMERROR, ERH_FATAL, PARMSIZE, STRING_MAX_LENG
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

    ! dataspace for feeding erhandler subroutine
    DOUBLE PRECISION, DIMENSION(10) ::  derrinfo
    CHARACTER(LEN=PARMSIZE), DIMENSION(10) :: cerrinfo

    CHARACTER(LEN=40), PARAMETER :: fname=__FILE__

    CALL TrackBegin('glans:getsi')

    numwrn = erinqr(ER_NUMWARNING)
    numerr = erinqr(ER_NUMERROR)

    CALL erhandler(fname, __LINE__, ERH_FATAL, &
         & 'glansys: glans:getsi not implemented', &
         & derrinfo, cerrinfo)

    value = 'empty'

    numwrn = erinqr(ER_NUMWARNING) - numwrn
    numerr = erinqr(ER_NUMERROR) - numerr
    flag = (numwrn + numerr).NE.0

    flag = .TRUE.

    CALL TrackEnd('glans:getsi')

  END FUNCTION getsi

  FUNCTION getf(value, Entity, ENTNUM, Item1, IT1NUM, Item2, IT2NUM) &
       & RESULT(flag)
    USE ansys_upf, ONLY : TrackBegin, TrackEnd, RunCommand, erinqr
#if ANSVER >= 70
    USE ansys_par, ONLY : CMD_MAX_LENG, &
#else
    USE ansys_par, ONLY : SYS_LNG_CMDLN, &
#endif
    & ER_NUMWARNING, ER_NUMERROR, PARMSIZE, STRING_MAX_LENG
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
    DOUBLE PRECISION, INTENT(OUT) :: value
    CHARACTER(LEN=4), INTENT(IN) :: Entity
    INTEGER, INTENT(IN) :: ENTNUM
    CHARACTER(LEN=8), INTENT(IN) :: Item1
    CHARACTER(LEN=8), INTENT(IN), OPTIONAL :: IT1NUM
    CHARACTER(LEN=8), INTENT(IN), OPTIONAL :: Item2
    CHARACTER(LEN=8), INTENT(IN), OPTIONAL :: IT2NUM

#if ANSVER >= 70
    CHARACTER(LEN=CMD_MAX_LENG) :: cmd
    CHARACTER(LEN=STRING_MAX_LENG) :: dummy
#else
    CHARACTER(LEN=SYS_LNG_CMDLN) :: cmd
#endif
    CHARACTER(LEN=PARMSIZE) :: para
    DOUBLE PRECISION, DIMENSION(3) :: subc

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
    ! write(wrinqr(WR_OUTPUT),*) 'cmd: ', trim(cmd)
    iErr = RunCommand(LEN_TRIM(cmd), TRIM(cmd))

    flag = ((erinqr(ER_NUMWARNING) - numwrn) + (erinqr(ER_NUMERROR) - numerr)).NE.0

    IF (flag) THEN
       CALL TrackEnd('glans:getf')
       RETURN
    END IF

    para ='PAR_'
#if ANSVER >= 70
    CALL parevl(para, 0, subc, 2, value, dummy, iErr)
#else
    value = parevl(para, 0, subc, 2, iErr)
#endif

    numwrn = erinqr(ER_NUMWARNING) - numwrn
    numerr = erinqr(ER_NUMERROR) - numerr
    flag = (numwrn + numerr).NE.0

100 FORMAT(A,',',A)
101 FORMAT(A,',',I)

    CALL TrackEnd('glans:getf')

  END FUNCTION getf

  FUNCTION getfs(value, Entity, ENTNUM, Item1, IT1NUM, Item2, IT2NUM) &
       & RESULT(flag)
    USE ansys_upf, ONLY : TrackBegin, TrackEnd, RunCommand, erinqr
#if ANSVER >= 70
    USE ansys_par, ONLY : CMD_MAX_LENG, &
#else
    USE ansys_par, ONLY : SYS_LNG_CMDLN, &
#endif
    & ER_NUMWARNING, ER_NUMERROR, PARMSIZE, STRING_MAX_LENG
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
    DOUBLE PRECISION, INTENT(OUT) :: value
    CHARACTER(LEN=4), INTENT(IN) :: Entity
    CHARACTER(LEN=PARMSIZE), INTENT(IN) :: ENTNUM
    CHARACTER(LEN=8), INTENT(IN) :: Item1
    CHARACTER(LEN=8), INTENT(IN), OPTIONAL :: IT1NUM
    CHARACTER(LEN=8), INTENT(IN), OPTIONAL :: Item2
    CHARACTER(LEN=8), INTENT(IN), OPTIONAL :: IT2NUM

#if ANSVER >= 70
    CHARACTER(LEN=CMD_MAX_LENG) :: cmd
    CHARACTER(LEN=STRING_MAX_LENG) :: dummy
#else
    CHARACTER(LEN=SYS_LNG_CMDLN) :: cmd
#endif
    CHARACTER(LEN=PARMSIZE) :: para
    DOUBLE PRECISION, DIMENSION(3) :: subc

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
       CALL TrackEnd('glans:getfs')
       RETURN
    END IF

    para ='PAR_'
#if ANSVER >= 70
    CALL parevl(para, 0, subc, 2, value, dummy, iErr)
#else
    value = parevl(para, 0, subc, 2, iErr)
#endif

    numwrn = erinqr(ER_NUMWARNING) - numwrn
    numerr = erinqr(ER_NUMERROR) - numerr
    flag = (numwrn + numerr).NE.0

100 FORMAT(A,',',A)

    CALL TrackEnd('glans:getfs')

  END FUNCTION getfs

  FUNCTION getfi(value, Entity, ENTNUM, Item1, IT1NUM, Item2, IT2NUM) &
       & RESULT(flag)
    USE ansys_upf, ONLY : TrackBegin, TrackEnd, RunCommand, erinqr
#if ANSVER >= 70
    USE ansys_par, ONLY : CMD_MAX_LENG, &
#else
    USE ansys_par, ONLY : SYS_LNG_CMDLN, &
#endif
    & ER_NUMWARNING, ER_NUMERROR, ERH_FATAL, PARMSIZE, STRING_MAX_LENG
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
    DOUBLE PRECISION :: value
    CHARACTER(LEN=4), INTENT(IN) :: Entity
    INTEGER, INTENT(IN) :: ENTNUM
    CHARACTER(LEN=8), INTENT(IN) :: Item1
    INTEGER, INTENT(IN) :: IT1NUM
    CHARACTER(LEN=8), INTENT(IN), OPTIONAL :: Item2
    INTEGER, INTENT(IN), OPTIONAL :: IT2NUM

    INTEGER :: numwrn
    INTEGER :: numerr

    ! dataspace for feeding erhandler subroutine
    DOUBLE PRECISION, DIMENSION(10) ::  derrinfo
    CHARACTER(LEN=PARMSIZE), DIMENSION(10) :: cerrinfo
    CHARACTER(LEN=40), PARAMETER :: fname=__FILE__

    CALL TrackBegin('glans:getfi')

    numwrn = erinqr(ER_NUMWARNING)
    numerr = erinqr(ER_NUMERROR)

    CALL erhandler(fname, __LINE__, ERH_FATAL, &
         & 'glansys: glans:getfi not implemented', &
         & derrinfo, cerrinfo)

    value = 0d0

    numwrn = erinqr(ER_NUMWARNING) - numwrn
    numerr = erinqr(ER_NUMERROR) - numerr
    flag = (numwrn + numerr).NE.0

    flag = .TRUE.

    CALL TrackEnd('glans:getfi')

  END FUNCTION getfi

  FUNCTION geti(value, Entity, ENTNUM, Item1, IT1NUM, Item2, IT2NUM) &
       & RESULT(flag)
    USE ansys_upf, ONLY : TrackBegin, TrackEnd, RunCommand, erinqr
#if ANSVER >= 70
    USE ansys_par, ONLY : CMD_MAX_LENG, &
#else
    USE ansys_par, ONLY : SYS_LNG_CMDLN, &
#endif
    & ER_NUMWARNING, ER_NUMERROR, PARMSIZE, STRING_MAX_LENG
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

    DOUBLE PRECISION :: fvalue

    CALL TrackBegin('glans:geti')

    flag = getf(fvalue, Entity, ENTNUM, Item1, IT1NUM, Item2, IT2NUM)
    value = INT(fvalue)

    CALL TrackEnd('glans:geti')

  END FUNCTION geti

  FUNCTION getis(value, Entity, ENTNUM, Item1, IT1NUM, Item2, IT2NUM) &
       & RESULT(flag)
    USE ansys_upf, ONLY : TrackBegin, TrackEnd, RunCommand, erinqr
#if ANSVER >= 70
    USE ansys_par, ONLY : CMD_MAX_LENG, &
#else
    USE ansys_par, ONLY : SYS_LNG_CMDLN, &
#endif
    & ER_NUMWARNING, ER_NUMERROR, PARMSIZE, STRING_MAX_LENG
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

    DOUBLE PRECISION :: fvalue

    CALL TrackBegin('glans:getis')

    flag = getfs(fvalue, Entity, ENTNUM, Item1, IT1NUM, Item2, IT2NUM)
    value = INT(fvalue)

    CALL TrackEnd('glans:getis')

  END FUNCTION getis

  FUNCTION getii(value, Entity, ENTNUM, Item1, IT1NUM, Item2, IT2NUM) &
       & RESULT(flag)
    USE ansys_upf, ONLY : TrackBegin, TrackEnd, RunCommand, erinqr
#if ANSVER >= 70
    USE ansys_par, ONLY : CMD_MAX_LENG, &
#else
    USE ansys_par, ONLY : SYS_LNG_CMDLN, &
#endif
    & ER_NUMWARNING, ER_NUMERROR, PARMSIZE, STRING_MAX_LENG
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

    DOUBLE PRECISION :: fvalue

    CALL TrackBegin('glans:getii')

    flag = getfi(fvalue, Entity, ENTNUM, Item1, IT1NUM, Item2, IT2NUM)
    value = INT(fvalue)

    CALL TrackEnd('glans:getii')

  END FUNCTION getii

  ! issue warnings and update message count
  SUBROUTINE message(wcode,n1,n2)
    USE ansys_upf, ONLY : TrackBegin, TrackEnd, erhandler
    USE ansys_par, ONLY : ERH_WARNING, PARMSIZE, MP_EX, MP_NUXY, MP_DENS
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

    ! dataspace for feeding erhandler subroutine
    DOUBLE PRECISION, DIMENSION(10) ::  derrinfo
    CHARACTER(LEN=PARMSIZE), DIMENSION(10) :: cerrinfo
    CHARACTER(LEN=40), PARAMETER :: fname=__FILE__
    CHARACTER(LEN=1024) :: msg

    CALL TrackBegin('glans:message')
    wcount=wcount+1
    derrinfo(1) = n1
    derrinfo(2) = n2
    IF (wcode.EQ.'comp') THEN
       CALL erhandler(fname, __LINE__, ERH_WARNING, &
            & TRIM(libname)//': Not all elements of component %i '// &
            & 'converted !%/'// &
            & '(Check if element types are allowed in present version)', &
            & derrinfo, cerrinfo)
    ELSE IF (wcode.EQ.'mat ') THEN
       IF (n2.EQ.MP_EX) THEN
          msg = TRIM(libname)//': no E-Module found in MP %i'
       ELSE IF (n2.EQ.MP_NUXY) THEN
          msg = TRIM(libname)//': no Poisson ratio found in MP %i'
       ELSE IF (n2.EQ.MP_DENS) THEN
          msg = TRIM(libname)//': no density found in MP %i'
       ELSE
          msg = TRIM(libname)//': unspecified material property not found in MP %i'
       END IF
       CALL erhandler(fname, __LINE__, ERH_WARNING, TRIM(msg), &
            & derrinfo, cerrinfo)
    ELSE IF (wcode.EQ.'matE') THEN
       CALL erhandler(fname, __LINE__, ERH_WARNING, &
            & TRIM(libname)//': no E-Module found in MP %i', &
            & derrinfo, cerrinfo)
    ELSE IF (wcode.EQ.'matP') THEN
       CALL erhandler(fname, __LINE__, ERH_WARNING, &
            & TRIM(libname)//': no Poisson ratio found in MP %i', &
            & derrinfo, cerrinfo)
    ELSE IF (wcode.EQ.'matD') THEN
       CALL erhandler(fname, __LINE__, ERH_WARNING, &
            & TRIM(libname)//': no density value found in MP %i', &
            & derrinfo, cerrinfo)
    ELSE IF (wcode.EQ.'beam') THEN
       CALL erhandler(fname, __LINE__, ERH_WARNING, &
            & TRIM(libname)//': beam at nodes %i %i '// &
            & 'does not have orientation node', &
            derrinfo, cerrinfo)
    ELSE IF (wcode.EQ.'pses') THEN
       CALL erhandler(fname, __LINE__, ERH_WARNING, &
            & TRIM(libname)//': shells with different thicknesses '// &
            & 'not supported (el. %i). Average value taken.', &
            & derrinfo, cerrinfo)
    ELSE IF (wcode.EQ.'shel') THEN
       CALL erhandler(fname, __LINE__, ERH_WARNING, &
            & TRIM(libname)//': shells with different thicknesses '// &
            & 'not supported (el. %i). Average value taken.', &
            & derrinfo, cerrinfo)
    END IF

    CALL TrackEnd('glans:message')

  END SUBROUTINE message

END MODULE glans

! Local Variables:
! compile-command:"make -C .. test"
! End:
