C     Copyright (C) 2007 by Germanischer Lloyd AG

C     ======================================================================
C     Task      Helper routines for ansys converters
C     ----------------------------------------------------------------------
C     Author    Berthold Höllmann <hoel@GL-Group.com>
C     Project   ans2bmf
C     ======================================================================

C CVSID: $Id$

      MODULE glans

      INTERFACE getf
        MODULE PROCEDURE getf, getfs, getfi
      END INTERFACE getf

      INTERFACE gets
        MODULE PROCEDURE gets, getsi
      END INTERFACE gets

      INTERFACE esel
        MODULE PROCEDURE esels, eseli
      END INTERFACE esel

      CONTAINS

      SUBROUTINE ans2bmf_get_d(cmd_str, dout)
c     reads ANSYS command string 'cmd_str'
c     puts out double precision parameter 'dout'

      USE ansys_par
      USE ansys_fun
      USE LOCMOD

      IMPLICIT NONE
C     Purpose: 
C     
C     Parameter:
C     in/out Name          Task
C ----------------------------------------------------------------------
C     Created: 2007-06-01  hoel
C ======================================================================
      CHARACTER(LEN=*) :: cmd_str
      DOUBLE PRECISION dout

#if   ANSVER >= 70
      CHARACTER(LEN=CMD_MAX_LENG) :: cmd
      CHARACTER(LEN=STRING_MAX_LENG) :: dummy
#else
      CHARACTER(LEN=SYS_LNG_CMDLN) :: cmd
#endif
      CHARACTER(LEN=PARMSIZE) :: para

      INTEGER :: iErr

      CALL TrackBegin("glans:ans2bmf_get_d")
      cmd = '*GET,PARA,'//cmd_str
      iErr = RunCommand(LEN_TRIM(cmd), cmd)
      para ='PARA'

#if   ANSVER >= 70
      CALL parevl(para, 0, subc, 2, dout, dummy, kerr)
#else
      dout = parevl(para,0,subc,2,kerr)
#endif
      CALL TrackEnd("glans:ans2bmf_get_d")
      END SUBROUTINE ans2bmf_get_d

      SUBROUTINE ans2bmf_get_s(cmd_str, sout)
c     reads ANSYS command string 'cmd_str'
c     writes a string of length 8 into 'sout'

      USE ansys_par
      USE ansys_fun
      USE LOCMOD

      IMPLICIT NONE
C     Purpose: 
C     
C     Parameter:
C     in/out Name          Task
C ----------------------------------------------------------------------
C     Created: 2007-06-01  hoel
C ======================================================================
      CHARACTER(LEN=*) :: cmd_str
      CHARACTER(LEN=PARMSIZE) :: sout
      DOUBLE PRECISION res_double
#if   ANSVER >= 70
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

      cmd = '*GET,PARA,'//cmd_str
      iErr = RunCommand(LEN_TRIM(cmd), cmd)
      para ='PARA'
#if   ANSVER < 70
      res_double = parevl(para,0,subc,2,kerr)
      sout = res_char8
#else
      CALL parevl(para, 0, subc, 2, res_double, dummy, kerr)
      sout = TRIM(dummy)
#endif

      CALL TrackEnd("glans:ans2bmf_get_s")
      END SUBROUTINE ans2bmf_get_s

      FUNCTION upcase(string) RESULT(upper)
!     Convert input string to uppercase

      USE ansys_fun

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
!     select component
      USE ansys_par
      USE ansys_fun
      USE LOCMOD

      IMPLICIT NONE
!     Purpose:
!
!       Select given ANSYS component
!
!     Parameter:
!     in/out Name          Task
!     in     mode          selection mode, one of:
!                             S    -- Select a new set (default).
!                             R    -- Reselect a set from the current set.
!                             A    -- Additionally select a set and extend
!                                     the current set.
!                             U    -- Unselect a set from the current set.
!                             ALL  -- Also select all components.
!                             NONE -- Unselect all components.
!     in     name          name of component to be selected. Value meaningless
!                          for mode=='ALL' and mode=='NONE'
! ----------------------------------------------------------------------
!     Created: 2007-05-21  hoel
! ======================================================================
      CHARACTER(LEN=*) :: mode
      CHARACTER(LEN=*), OPTIONAL :: name

      CHARACTER(LEN=LEN(mode)) :: umode
      INTEGER :: iErr

      CHARACTER(LEN=40), PARAMETER :: fname=__FILE__

#if   ANSVER >= 70
      CHARACTER(LEN=CMD_MAX_LENG) :: cmd
#else
      CHARACTER(LEN=SYS_LNG_CMDLN) :: cmd
#endif

      CALL TrackBegin('glans:cmselect')

      umode = upcase(mode)

      IF ((umode.NE.'S').AND.
     $     (umode.NE.'R').AND.
     $     (umode.NE.'A').AND.
     $     (umode.NE.'U').AND.
     $     (umode.NE.'ALL').AND.
     $     (umode.NE.'NONE')) THEN
         CALL erhandler(fname, __LINE__, ERH_FATAL,
     $        TRIM(libname)// ': invalid mode: '//TRIM(umode)//'.',
     $        derrinfo, cerrinfo)
      END IF

      IF (PRESENT(name)) THEN
         WRITE(cmd,200) TRIM(upcase(mode)), TRIM(upcase(name))
      ELSE
         WRITE(cmd,200) TRIM(upcase(mode)), ''
      END IF
 200  FORMAT('CMSEL,'A,','A)

      iErr = RunCommand(LEN_TRIM(cmd), cmd)

      CALL TrackEnd('glans:cmselect')

      RETURN
      END SUBROUTINE cmselect

      SUBROUTINE eseli(Type, Item, Comp, VMIN, VMAX, VINC, KABS)

      USE ansys_par
      USE ansys_fun
      USE LOCMOD

      IMPLICIT NONE
!     Purpose: select elements
!
!     Parameter:
!     in/out Name          Task
!     in     Type          Label identifying the type of select:
!                            S -- Select a new set (default).
!                            R -- Reselect a set from the current set.
!                            A -- Additionally select a set and extend
!                                 the current set.
!                            U -- Unselect a set from the current set.
!                            ALL -- Restore the full set.
!                            NONE -- Unselect the full set.
!                            INVE -- Invert the current set (selected
!                                    becomes unselected and vice versa).
!     The following fields are used only with Type = S, R, A, or U:
!     in     Item          Label identifying data, see ESEL
!                          command. Some items also require a component
!                          label. Defaults to ELEM. If Item = STRA
!                          (straightened), elements are selected whose
!                          midside nodes do not conform to the curved
!                          line or non-flat area on which they should
!                          lie. (Such elements are sometimes formed
!                          during volume meshing [VMESH] in an attempt
!                          to avoid excessive element distortion.) You
!                          should graphically examine any such elements
!                          to evaluate their possible effect on solution
!                          accuracy.
!     in     Comp          Component of the item (if required). For valid
!                          component labels see ESEL command.
!     in     VMIN          Minimum value of item range. Ranges are
!                          element numbers, attribute numbers, load
!                          values, or result values as appropriate for
!                          the item.
!     in     VMAX          Maximum value of item range. VMAX defaults to
!                          VMIN for input values.
!
!                          For result values, VMAX defaults to infinity
!                          if VMIN is positive, or to zero if VMIN is
!                          negative.
!     in     VINC          Value increment within range. Used only with
!                          integer ranges (such as for element and
!                          attribute numbers). Defaults to 1. VINC
!                          cannot be negative.
!     in     KABS          Absolute value key:
!                            0 -- Check sign of value during selection.
!                            1 -- Use absolute value during selection
!                                 (sign ignored).
! ----------------------------------------------------------------------
!     Created: 2007-05-29 hoel
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

#if   ANSVER >= 70
      CHARACTER(LEN=CMD_MAX_LENG) :: cmd
#else
      CHARACTER(LEN=SYS_LNG_CMDLN) :: cmd
#endif

      CHARACTER(LEN=40), PARAMETER :: fname=__FILE__

      CALL TrackBegin('glans:eseli')

      utype = upcase(type)

      IF ((utype.NE.'S').AND.
     $     (utype.NE.'R').AND.
     $     (utype.NE.'A').AND.
     $     (utype.NE.'U').AND.
     $     (utype.NE.'ALL').AND.
     $     (utype.NE.'NONE').AND.
     $     (utype.NE.'INVE')) THEN
         CALL erhandler(fname, __LINE__, ERH_FATAL,
     $        TRIM(libname)//': invalid type: '//TRIM(utype)//'.',
     $        derrinfo, cerrinfo)
      END IF

      IF (PRESENT(Item)) THEN
         IF (LEN(item).GT.128) THEN
            CALL erhandler(fname, __LINE__, ERH_FATAL,
     $           TRIM(libname)//
     $           ': length of value item greater then 129.',
     $           derrinfo, cerrinfo)
         END IF
         uitem = upcase(item)
         IF ((uitem.NE.'ELEM').AND.
     $        (uitem.NE.'ADJ').AND.
     $        (uitem.NE.'TYPE').AND.
     $        (uitem.NE.'ENAME').AND.
     $        (uitem.NE.'MAT').AND.
     $        (uitem.NE.'REAL').AND.
     $        (uitem.NE.'ESYS').AND.
     $        (uitem.NE.'PART').AND.
     $        (uitem.NE.'LIVE').AND.
     $        (uitem.NE.'LAYER').AND.
     $        (uitem.NE.'SEC').AND.
     $        (uitem.NE.'PINC').AND.
     $        (uitem.NE.'PEXC').AND.
     $        (uitem.NE.'STRA').AND.
     $        (uitem.NE.'SFE').AND.
     $        (uitem.NE.'BFE').AND.
     $        (uitem.NE.'PATH')) THEN
            CALL erhandler(fname, __LINE__, ERH_FATAL,
     $           TRIM(libname)//': invalid item: '//TRIM(uitem)//'.',
     $           derrinfo, cerrinfo)
         END IF
         IF (PRESENT(comp)) THEN
            IF ((uitem.EQ.'ELEM').AND.
     $           (uitem.EQ.'ADJ').AND.
     $           (uitem.EQ.'TYPE').AND.
     $           (uitem.EQ.'ENAME').AND.
     $           (uitem.EQ.'MAT').AND.
     $           (uitem.EQ.'REAL').AND.
     $           (uitem.EQ.'ESYS').AND.
     $           (uitem.EQ.'PART').AND.
     $           (uitem.EQ.'LIVE').AND.
     $           (uitem.EQ.'LAYER').AND.
     $           (uitem.EQ.'SEC').AND.
     $           (uitem.EQ.'PINC').AND.
     $           (uitem.EQ.'PEXC').AND.
     $           (uitem.EQ.'STRA')) THEN
               CALL erhandler(fname, __LINE__, ERH_FATAL,
     $              TRIM(libname)
     $              //': invalid input: component not allowed '//
     $              'with item '//uitem//'.',
     $              derrinfo, cerrinfo)
            ELSE IF (uitem.EQ.'SFE') THEN
               CALL erhandler(fname, __LINE__, ERH_FATAL,
     $              TRIM(libname)
     $              //': item '//uitem//' not yet supported.',
     $              derrinfo, cerrinfo)
            ELSE IF (uitem.EQ.'BFE') THEN
               CALL erhandler(fname, __LINE__, ERH_FATAL,
     $              TRIM(libname)
     $              //': item '//uitem//' not yet supported.',
     $              derrinfo, cerrinfo)
            ELSE IF (uitem.EQ.'PATH') THEN
               CALL erhandler(fname, __LINE__, ERH_FATAL,
     $              TRIM(libname)
     $              //': item '//uitem//' not yet supported.',
     $              derrinfo, cerrinfo)
            END IF
         END IF
      ELSE
         IF (PRESENT(comp)) THEN
            CALL erhandler(fname, __LINE__, ERH_FATAL,
     $           TRIM(libname)//
     $           ': invaid call: comp given, but no item.',
     $           derrinfo, cerrinfo)
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

 200  FORMAT(A,','A)
 201  FORMAT(A,','I)

      iErr = RunCommand(LEN_TRIM(cmd), cmd)

      CALL TrackEnd('glans:eseli')

      RETURN
      END SUBROUTINE eseli

      SUBROUTINE esels(Type, Item, Comp, VMIN)

      USE ansys_par
      USE ansys_fun
      USE LOCMOD

      IMPLICIT NONE
!     Purpose: select elements
!
!     Parameter:
!     in/out Name          Task
!     in     Type          Label identifying the type of select:
!                            S -- Select a new set (default).
!                            R -- Reselect a set from the current set.
!                            A -- Additionally select a set and extend
!                                 the current set.
!                            U -- Unselect a set from the current set.
!                            ALL -- Restore the full set.
!                            NONE -- Unselect the full set.
!                            INVE -- Invert the current set (selected
!                                    becomes unselected and vice versa).
!     The following fields are used only with Type = S, R, A, or U:
!     in     Item          Label identifying data, see ESEL
!                          command. Some items also require a component
!                          label. Defaults to ELEM. If Item = STRA
!                          (straightened), elements are selected whose
!                          midside nodes do not conform to the curved
!                          line or non-flat area on which they should
!                          lie. (Such elements are sometimes formed
!                          during volume meshing [VMESH] in an attempt
!                          to avoid excessive element distortion.) You
!                          should graphically examine any such elements
!                          to evaluate their possible effect on solution
!                          accuracy.
!     in     Comp          Component of the item (if required). For valid
!                          component labels see ESEL command.
!     in     VMIN          Minimum value of item range. Ranges are
!                          element numbers, attribute numbers, load
!                          values, or result values as appropriate for
!                          the item.
! ----------------------------------------------------------------------
!     Created: 2007-05-29 hoel
! ======================================================================
      CHARACTER(LEN=*), INTENT(IN) :: Type
      CHARACTER(LEN=*), INTENT(IN) :: Item
      CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: Comp
      CHARACTER(LEN=*), INTENT(IN) :: VMIN

      CHARACTER(LEN=LEN(type)) :: utype
      CHARACTER(LEN=STRING_MAX_LENG) :: uitem
      CHARACTER(LEN=STRING_MAX_LENG) :: uvmin
      INTEGER :: iErr

#if   ANSVER >= 70
      CHARACTER(LEN=CMD_MAX_LENG) :: cmd
#else
      CHARACTER(LEN=SYS_LNG_CMDLN) :: cmd
#endif

      CHARACTER(LEN=40), PARAMETER :: fname=__FILE__

      CALL TrackBegin('glans:esels')

      utype = upcase(type)

      IF ((utype.NE.'S').AND.
     $     (utype.NE.'R').AND.
     $     (utype.NE.'A').AND.
     $     (utype.NE.'U').AND.
     $     (utype.NE.'ALL').AND.
     $     (utype.NE.'NONE').AND.
     $     (utype.NE.'INVE')) THEN
         CALL erhandler(fname, __LINE__, ERH_FATAL,
     $        TRIM(libname)//': invalid type: '//TRIM(utype)//'.',
     $        derrinfo, cerrinfo)
      END IF

      IF (LEN(item).GT.STRING_MAX_LENG) THEN
         CALL erhandler(fname, __LINE__, ERH_FATAL,
     $        TRIM(libname)//
     $        ': length of value item greater then STRING_MAX_LENG.',
     $        derrinfo, cerrinfo)
      END IF
      uitem = upcase(item)
      IF ((uitem.NE.'ELEM').AND.
     $     (uitem.NE.'ADJ').AND.
     $     (uitem.NE.'TYPE').AND.
     $     (uitem.NE.'ENAME').AND.
     $     (uitem.NE.'MAT').AND.
     $     (uitem.NE.'REAL').AND.
     $     (uitem.NE.'ESYS').AND.
     $     (uitem.NE.'PART').AND.
     $     (uitem.NE.'LIVE').AND.
     $     (uitem.NE.'LAYER').AND.
     $     (uitem.NE.'SEC').AND.
     $     (uitem.NE.'PINC').AND.
     $     (uitem.NE.'PEXC').AND.
     $     (uitem.NE.'STRA').AND.
     $     (uitem.NE.'SFE').AND.
     $     (uitem.NE.'BFE').AND.
     $     (uitem.NE.'PATH')) THEN
         CALL erhandler(fname, __LINE__, ERH_FATAL,
     $        TRIM(libname)//': invalid item: '//TRIM(uitem)//'.',
     $        derrinfo, cerrinfo)
      END IF
      IF (PRESENT(comp)) THEN
         IF ((uitem.EQ.'ELEM').AND.
     $        (uitem.EQ.'ADJ').AND.
     $        (uitem.EQ.'TYPE').AND.
     $        (uitem.EQ.'ENAME').AND.
     $        (uitem.EQ.'MAT').AND.
     $        (uitem.EQ.'REAL').AND.
     $        (uitem.EQ.'ESYS').AND.
     $        (uitem.EQ.'PART').AND.
     $        (uitem.EQ.'LIVE').AND.
     $        (uitem.EQ.'LAYER').AND.
     $        (uitem.EQ.'SEC').AND.
     $        (uitem.EQ.'PINC').AND.
     $        (uitem.EQ.'PEXC').AND.
     $        (uitem.EQ.'STRA')) THEN
            CALL erhandler(fname, __LINE__, ERH_FATAL,
     $           TRIM(libname)
     $           //': invalid input: component not allowed '//
     $           'with item '//uitem//'.',
     $           derrinfo, cerrinfo)
         ELSE IF (uitem.EQ.'SFE') THEN
            CALL erhandler(fname, __LINE__, ERH_FATAL,
     $           TRIM(libname)
     $           //': item '//uitem//' not yet supported.',
     $           derrinfo, cerrinfo)
         ELSE IF (uitem.EQ.'BFE') THEN
            CALL erhandler(fname, __LINE__, ERH_FATAL,
     $           TRIM(libname)
     $           //': item '//uitem//' not yet supported.',
     $           derrinfo, cerrinfo)
         ELSE IF (uitem.EQ.'PATH') THEN
            CALL erhandler(fname, __LINE__, ERH_FATAL,
     $           TRIM(libname)
     $           //': item '//uitem//' not yet supported.',
     $           derrinfo, cerrinfo)
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

 200  FORMAT(A,','A)

      iErr = RunCommand(LEN_TRIM(cmd), cmd)

      CALL TrackEnd('glans:esels')

      RETURN
      END SUBROUTINE esels

      SUBROUTINE nsel(Type, Item, Comp, VMIN, VMAX, VINC, KABS)

      USE ansys_par
      USE ansys_fun
      USE LOCMOD

      IMPLICIT NONE
!     Purpose: select nodes
!
!     Parameter:
!     in/out Name          Task

!     in     Type          Label identifying the type of select:
!                            S -- Select a new set (default).
!                            R -- Reselect a set from the current set.
!                            A -- Additionally select a set and extend
!                                 the current set.
!                            U -- Unselect a set from the current set.
!                            ALL -- Restore the full set.
!                            NONE -- Unselect the full set.
!                            INVE -- Invert the current set (selected
!                                    becomes unselected and vice versa).
!                            STAT -- Display the current select status.
!     The following fields are used only with Type = S, R, A, or U:
!     in     Item          Label identifying data. Valid item labels
!                          described in the NSEL documentation Defaults
!                          to NODE.
!     in     Comp          Component of the item (if required). Valid
!                          component labels are desribed in the NSEL
!                          docu.
!     in     VMIN          Minimum value of item range. Ranges are node
!                          numbers, set numbers, coordinate values, load
!                          values, or result values as appropriate for
!                          the item.
!     in     VMAX          Maximum value of item range. VMAX defaults to
!                          VMIN for input values. For result values,
!                          VMAX defaults to infinity if VMIN is
!                          positive, or to zero if VMIN is negative.
!     in     VINC          Value increment within range. Used only with
!                          integer ranges (such as for node and set
!                          numbers). Defaults to 1. VINC cannot be
!                          negative.
!     in     KABS          Absolute value key:
!                            0 -- Check sign of value during selection.
!                            1 -- Use absolute value during selection
!                                 (sign ignored).
! ----------------------------------------------------------------------
!     Created: 2007-05-29  hoel
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

#if   ANSVER >= 70
      CHARACTER(LEN=CMD_MAX_LENG) :: cmd
#else
      CHARACTER(LEN=SYS_LNG_CMDLN) :: cmd
#endif

      CHARACTER(LEN=40), PARAMETER :: fname=__FILE__

      CALL TrackBegin('glans:nsel')

      utype = upcase(type)

      IF ((utype.NE.'S').AND.
     $     (utype.NE.'R').AND.
     $     (utype.NE.'A').AND.
     $     (utype.NE.'U').AND.
     $     (utype.NE.'ALL').AND.
     $     (utype.NE.'NONE').AND.
     $     (utype.NE.'INVE')) THEN
         CALL erhandler(fname, __LINE__, ERH_FATAL,
     $        TRIM(libname)//': invalid type: '//TRIM(utype)//'.',
     $        derrinfo, cerrinfo)
      END IF

      IF (PRESENT(Item)) THEN
         IF (LEN(item).GT.128) THEN
            CALL erhandler(fname, __LINE__, ERH_FATAL,
     $           TRIM(libname)//
     $           ': length of value item greater then 129.',
     $           derrinfo, cerrinfo)
         END IF
         uitem = upcase(item)
         IF ( (uitem.NE.'NODE').AND.
     $        (uitem.NE.'EXT').AND.
     $        (uitem.NE.'LOC').AND.
     $        (uitem.NE.'ANG').AND.
     $        (uitem.NE.'M').AND.
     $        (uitem.NE.'CP').AND.
     $        (uitem.NE.'CE').AND.
     $        (uitem.NE.'D').AND.
     $        (uitem.NE.'F').AND.
     $        (uitem.NE.'BF')) THEN
            CALL erhandler(fname, __LINE__, ERH_FATAL,
     $           TRIM(libname)//': invalid item: '//TRIM(uitem)//'.',
     $           derrinfo, cerrinfo)
         END IF
         IF (PRESENT(comp)) THEN
            CALL erhandler(fname, __LINE__, ERH_FATAL,
     $           TRIM(libname)
     $           //': component not yet allowed.',
     $           derrinfo, cerrinfo)
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

 200  FORMAT(A,','A)
 201  FORMAT(A,','I)

      iErr = RunCommand(LEN_TRIM(cmd), cmd)

      CALL TrackEnd('glans:nsel')

      RETURN
      END SUBROUTINE nsel

      SUBROUTINE doDebugLoc(func, file, line)

      USE ifcore
      USE ansys_par
      USE ansys_fun

      IMPLICIT NONE
C     Purpose: 
C     Generate debugging output with sourcefilename, current line number
C     and name of current function.
C     Parameter:
C     in/out Name          Task
C     in     func          name of function to log
C     in     file          source file where function is defined (__FILE__)
C     in     line          current line number in source file (__LINE__)
C ----------------------------------------------------------------------
C     Created: 2007-05-31  hoel
C ======================================================================
      CHARACTER(LEN=*) :: func
      CHARACTER(LEN=*) :: file
      INTEGER :: line
      INTEGER :: res
      INTEGER :: stderr

      CALL TrackBegin('glans:doDebugLoc')

      stderr = wrinqr(WR_OUTPUT)
      WRITE(stderr,100) file, line, func
 100  FORMAT (X,A,':',I3,':',A)
      res = COMMITQQ(stderr)

      CALL TrackEnd('glans:doDebugLoc')

      END SUBROUTINE doDebugLoc

      FUNCTION gets(Entity, ENTNUM, Item1, IT1NUM, Item2, IT2NUM)
     $     RESULT(value)

      USE ansys_par
      USE ansys_fun
      USE LOCMOD

      IMPLICIT NONE
C     Purpose:
C
C     Parameter:
C     in/out Name          Task
C            Entity
C            ENTNUM
C            Item1
C            IT1NUM
C            Item2
C            IT2NUM
C ----------------------------------------------------------------------
C     Created: 2007-05-29  hoel
C ======================================================================
      CHARACTER(LEN=STRING_MAX_LENG) :: value
      CHARACTER(LEN=4), INTENT(IN) :: Entity
      INTEGER, INTENT(IN) :: ENTNUM
      CHARACTER(LEN=8), INTENT(IN) :: Item1
      CHARACTER(LEN=8), INTENT(IN), OPTIONAL :: IT1NUM
      CHARACTER(LEN=8), INTENT(IN), OPTIONAL :: Item2
      CHARACTER(LEN=8), INTENT(IN), OPTIONAL :: IT2NUM

#if   ANSVER >= 70
      CHARACTER(LEN=CMD_MAX_LENG) :: cmd
      DOUBLE PRECISION :: Dummy
#else
      CHARACTER(LEN=SYS_LNG_CMDLN) :: cmd
      CHARACTER(LEN=8) :: res_char8
      DOUBLE PRECISION :: res_double
      EQUIVALENCE      (res_double, res_char8)
#endif
      CHARACTER(LEN=PARMSIZE) :: para

      INTEGER :: iErr

      CALL TrackBegin('glans:gets')

      WRITE(cmd,100) '*GET,PARA', TRIM(Entity)
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
      write(wrinqr(WR_OUTPUT),*) 'cmd: ', trim(cmd)
      iErr = RunCommand(LEN_TRIM(cmd), TRIM(cmd))
      para ='PARA'

#if   ANSVER >= 70
      CALL parevl(para, 0, subc, 2, dummy, value, kerr)
#else
      res_double = parevl(para, 0, subc, 2, kerr)
      value = trim(res_char8)
#endif

 100  FORMAT(A,',',A)
 101  FORMAT(A,',',I)

      CALL TrackEnd('glans:gets')

      END FUNCTION gets

      FUNCTION getsi(Entity, ENTNUM, Item1, IT1NUM, Item2, IT2NUM)
     $     RESULT(value)

      USE ansys_par
      USE ansys_fun
      USE LOCMOD

      IMPLICIT NONE
C     Purpose:
C
C     Parameter:
C     in/out Name          Task
C            Entity
C            ENTNUM
C            Item1
C            IT1NUM
C            Item2
C            IT2NUM
C ----------------------------------------------------------------------
C     Created: 2007-05-29  hoel
C ======================================================================
      CHARACTER(LEN=STRING_MAX_LENG) :: value
      CHARACTER(LEN=4), INTENT(IN) :: Entity
      INTEGER, INTENT(IN) :: ENTNUM
      CHARACTER(LEN=8), INTENT(IN) :: Item1
      INTEGER, INTENT(IN) :: IT1NUM
      CHARACTER(LEN=8), INTENT(IN), OPTIONAL :: Item2
      INTEGER, INTENT(IN), OPTIONAL :: IT2NUM

      CHARACTER(LEN=40), PARAMETER :: fname=__FILE__

      CALL TrackBegin('glans:getsi')

      CALL erhandler(fname, __LINE__, ERH_FATAL,
     $        trim(libname)//': glans:getsi not implemented',
     $        derrinfo, cerrinfo)

      value = 'empty'

      CALL TrackEnd('glans:getsi')

      END FUNCTION getsi

      FUNCTION getf(Entity, ENTNUM, Item1, IT1NUM, Item2, IT2NUM)
     $     RESULT(value)

      USE ansys_par
      USE ansys_fun
      USE LOCMOD

      IMPLICIT NONE
C     Purpose:
C
C     Parameter:
C     in/out Name          Task
C            Entity
C            ENTNUM
C            Item1
C            IT1NUM
C            Item2
C            IT2NUM
C ----------------------------------------------------------------------
C     Created: 2007-05-29  hoel
C ======================================================================
      DOUBLE PRECISION :: value
      CHARACTER(LEN=4), INTENT(IN) :: Entity
      INTEGER, INTENT(IN) :: ENTNUM
      CHARACTER(LEN=8), INTENT(IN) :: Item1
      CHARACTER(LEN=8), INTENT(IN), OPTIONAL :: IT1NUM
      CHARACTER(LEN=8), INTENT(IN), OPTIONAL :: Item2
      CHARACTER(LEN=8), INTENT(IN), OPTIONAL :: IT2NUM

#if   ANSVER >= 70
      CHARACTER(LEN=CMD_MAX_LENG) :: cmd
      CHARACTER(LEN=STRING_MAX_LENG) :: dummy
#else
      CHARACTER(LEN=SYS_LNG_CMDLN) :: cmd
#endif
      CHARACTER(LEN=PARMSIZE) :: para

      INTEGER :: iErr

      CALL TrackBegin('glans:getf')

      WRITE(cmd,100) '*GET,PARA', TRIM(Entity)
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
      write(wrinqr(WR_OUTPUT),*) 'cmd: ', trim(cmd)
      iErr = RunCommand(LEN_TRIM(cmd), TRIM(cmd))
      para ='PARA'

#if   ANSVER >= 70
      CALL parevl(para, 0, subc, 2, value, dummy, kerr)
#else
      value = parevl(para,0,subc,2,kerr)
#endif

 100  FORMAT(A,',',A)
 101  FORMAT(A,',',I)

      CALL TrackEnd('glans:getf')

      END FUNCTION getf

      FUNCTION getfs(Entity, ENTNUM, Item1, IT1NUM, Item2, IT2NUM)
     $     RESULT(value)

      USE ansys_par
      USE ansys_fun
      USE LOCMOD

      IMPLICIT NONE
C     Purpose:
C
C     Parameter:
C     in/out Name          Task
C            Entity
C            ENTNUM
C            Item1
C            IT1NUM
C            Item2
C            IT2NUM
C ----------------------------------------------------------------------
C     Created: 2007-05-29  hoel
C ======================================================================
      DOUBLE PRECISION :: value
      CHARACTER(LEN=4), INTENT(IN) :: Entity
      CHARACTER(LEN=8), INTENT(IN) :: ENTNUM
      CHARACTER(LEN=8), INTENT(IN) :: Item1
      CHARACTER(LEN=8), INTENT(IN), OPTIONAL :: IT1NUM
      CHARACTER(LEN=8), INTENT(IN), OPTIONAL :: Item2
      CHARACTER(LEN=8), INTENT(IN), OPTIONAL :: IT2NUM

#if   ANSVER >= 70
      CHARACTER(LEN=CMD_MAX_LENG) :: cmd
      CHARACTER(LEN=STRING_MAX_LENG) :: dummy
#else
      CHARACTER(LEN=SYS_LNG_CMDLN) :: cmd
#endif
      CHARACTER(LEN=PARMSIZE) :: para

      INTEGER :: iErr

      CALL TrackBegin('glans:getfs')

      WRITE(cmd,100) '*GET,PARA', TRIM(Entity)
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
      write(wrinqr(WR_OUTPUT),*) 'cmd: ', trim(cmd)
      iErr = RunCommand(LEN_TRIM(cmd), TRIM(cmd))
      para ='PARA'

#if   ANSVER >= 70
      CALL parevl(para, 0, subc, 2, value, dummy, kerr)
#else
      value = parevl(para,0,subc,2,kerr)
#endif

 100  FORMAT(A,',',A)

      CALL TrackEnd('glans:getfs')

      END FUNCTION getfs

      FUNCTION getfi(Entity, ENTNUM, Item1, IT1NUM, Item2, IT2NUM)
     $     RESULT(value)

      USE ansys_par
      USE ansys_fun
      USE LOCMOD

      IMPLICIT NONE
C     Purpose:
C
C     Parameter:
C     in/out Name          Task
C            Entity
C            ENTNUM
C            Item1
C            IT1NUM
C            Item2
C            IT2NUM
C ----------------------------------------------------------------------
C     Created: 2007-05-29  hoel
C ======================================================================
      DOUBLE PRECISION :: value
      CHARACTER(LEN=4), INTENT(IN) :: Entity
      INTEGER, INTENT(IN) :: ENTNUM
      CHARACTER(LEN=8), INTENT(IN) :: Item1
      INTEGER, INTENT(IN) :: IT1NUM
      CHARACTER(LEN=8), INTENT(IN), OPTIONAL :: Item2
      INTEGER, INTENT(IN), OPTIONAL :: IT2NUM

      CHARACTER(LEN=40), PARAMETER :: fname=__FILE__

      CALL TrackBegin('glans:getfi')

      CALL erhandler(fname, __LINE__, ERH_FATAL,
     $        trim(libname)//': glans:getfi not implemented',
     $        derrinfo, cerrinfo)

      value = 0d0

      CALL TrackEnd('glans:getfi')

      END FUNCTION getfi

      END MODULE glans

! Local Variables:
! compile-command:"make -C .. test"
! End:




