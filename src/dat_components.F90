!     collect components

#ifndef DEBUG_LOC
#ifdef DEBUG
#define DEBUG_LOC(fmt) call doDebugLoc(fmt,__FILE__,__LINE__)
#else
#define DEBUG_LOC(fmt)
#endif !DEBUG_LOC
#endif

MODULE mod_dat_components

CONTAINS

  SUBROUTINE dat_components(el_data)

    USE ansys_upf, ONLY : TrackBegin, TrackEnd, RunCommand, erhandler, &
         elmiqr
#if ANSVER >= 70
    USE ansys_par, ONLY : CMD_MAX_LENG, &
#else
    USE ansys_par, ONLY : SYS_LNG_CMDLN, &
#endif
    ERH_ERROR, ERH_FATAL, ERH_NOTE, ERH_WARNING, DB_NUMSELECTED, &
    PARMSIZE

    USE glans, ONLY : cmselect, get, esel, ans2bmf_get_s, message
    USE LOCMOD, ONLY : libname
    USE ans_common

    ! Purpose:
    !
    ! Parameter:
    ! in/out Name          Task
    ! ----------------------------------------------------------------------
    ! Created: 2007-06-01  hoel
    ! ======================================================================

    IMPLICIT NONE

    INTEGER :: el_data(*)

    INTEGER :: ansys_comp, an_cnum, n, m, el_b4, msglvl
    INTEGER :: res

   INTEGER :: iErr

#if ANSVER >= 70
    CHARACTER(LEN=CMD_MAX_LENG) :: cmd
#else
    CHARACTER(LEN=SYS_LNG_CMDLN) :: cmd
#endif
    CHARACTER(LEN=PARMSIZE) :: para

    LOGICAL :: flag

    ! dataspace for feeding erhandler subroutine
    DOUBLE PRECISION, DIMENSION(10) ::  derrinfo
    CHARACTER(LEN=PARMSIZE), DIMENSION(10) :: cerrinfo

    CHARACTER(LEN=40), PARAMETER :: fname=__FILE__

    CALL TrackBegin('dat_components')

    ! find number of components: -> ansys_comp
    flag = get(ansys_comp, Entity='COMP', ENTNUM=0, Item1='NCOMP   ')
    if (flag) THEN
       CALL erhandler(fname, __LINE__, ERH_FATAL, &
            trim(libname)// &
            ': Could not determine number of components.', &
            derrinfo, cerrinfo)
    END IF

    derrinfo(1) = ansys_comp
    CALL erhandler(fname, __LINE__, ERH_NOTE, &
         trim(libname)//': # of ANSYS components: %i', &
         derrinfo, cerrinfo)

    ! loop over all components
    !  running # for sxf components created
    comp_num=0
    !  running # of ANSYS components treated
    an_cnum=0
    DO n = 1, ansys_comp+1

       IF (comp_num.GE.max_components-10) THEN
          derrinfo(1) = comp_num
          derrinfo(2) = max_components
          CALL erhandler(fname, __LINE__, ERH_ERROR, &
               trim(libname)//': '// &
               'ERROR: %i components defined. Greater or '// &
               'too close to maximal number %i', &
               derrinfo, cerrinfo)
          CALL anserr(4,'Too many components',0.0,' ')
       END IF

       IF (n.GT.ansys_comp) THEN

          ! generate components for all elements not in any component
          comp_name(n) = 'NotNamed'
          cerrinfo(1) = trim(comp_name(n))
          CALL erhandler(fname, __LINE__, ERH_NOTE, &
               trim(libname)//': Component %s', derrinfo, cerrinfo)
          CALL esel('ALL')
          DO m = 1, n-1
             CALL cmselect('U', trim(comp_name(m)))
          END DO
          cmd = 'CM,NotNamed,ELEM'
          iErr = RunCommand(LEN_TRIM(cmd), trim(cmd))
          an_cnum = an_cnum+1

       ELSE

          ! get name of component: -> comp_name(an_cnum)
          WRITE(para,'(i8)') n
          CALL ans2bmf_get_s('COMP,'//para//',NAME', &
               comp_name(an_cnum+1))

          ! get type of component
          flag = get(res, 'COMP', comp_name(an_cnum+1), 'TYPE    ')
          if (flag .OR. res.EQ.0) THEN
             cerrinfo(1) = comp_name(an_cnum+1)
             CALL erhandler(fname, __LINE__, ERH_FATAL, &
                  trim(libname)// &
                  ': Could not determine type for component '// &
                  'named %s.', &
                  derrinfo, cerrinfo)
          END IF

!     check if this is a component of elements
          IF (res.GT.10) THEN
             cerrinfo(1) = comp_name(an_cnum+1)
             IF (batch) THEN
                msglvl = ERH_FATAL
             ELSE
                msglvl = ERH_ERROR
             END IF
             CALL erhandler(fname, __LINE__, msglvl, &
                  trim(libname)//': '// &
                  'Component %s is not of type ELEM'// &
                  '\nElements in assemblies are not supported.', &
                  derrinfo, cerrinfo)
             an_cnum = an_cnum + 1
             GOTO 1000
          ENDIF
          IF (res.NE.2) THEN
             cerrinfo(1) = comp_name(an_cnum+1)
             CALL erhandler(fname, __LINE__, ERH_WARNING, &
                  trim(libname)//': Component %s is not of type ELEM', &
                  derrinfo, cerrinfo)
             an_cnum = an_cnum + 1
             GOTO 1000
          ENDIF

          ! select component
          an_cnum = an_cnum+1
          CALL cmselect('S', comp_name(an_cnum))
          cerrinfo(1) = comp_name(an_cnum)
          CALL erhandler(fname, __LINE__, ERH_NOTE, &
               trim(libname)//': Treating ANSYS component %s', &
               derrinfo, cerrinfo)
          el_length = elmiqr(0, DB_NUMSELECTED)
          derrinfo(1) =  el_length
          CALL erhandler(fname, __LINE__, ERH_NOTE, &
               trim(libname)//':     # of elements:     %i', &
               derrinfo, cerrinfo)

       END IF

       ! unselect all previous components
       DO m = 1, an_cnum-1
          CALL cmselect('U', comp_name(m))
       END DO

       ! find # of elements in component
       el_length = elmiqr(0, DB_NUMSELECTED)
       derrinfo(1) = el_length
       CALL erhandler(fname, __LINE__, ERH_NOTE, &
            trim(libname)//':     # of elements not in any previous '// &
            'component: %i', &
            derrinfo, cerrinfo)

       IF (el_length.GT.0) THEN

          ! define temporary component
          cmd = 'CM,XXXCOMP,ELEM'
          iErr = RunCommand(LEN_TRIM(cmd), cmd)

          ! subselect all different types in the component an_cnum and
          ! store on stack
          el_count = 0
          DO m = 1, csub_num
             CALL ans2bmf_cstore(comp_name(an_cnum), m, el_data)
          END DO
       END IF

       IF (el_count.LT.el_length) THEN
          CALL message('comp', n, n)

          ! check specifically for BEAM4 elements
          CALL cmselect('S', 'XXXCOMP')
          CALL esel('R', 'ENAME', VMIN='BEAM4')
          el_b4 = elmiqr(0, DB_NUMSELECTED)
          derrinfo(1) = el_b4
          cerrinfo(1) = comp_name(an_cnum)
          CALL erhandler(fname, __LINE__, ERH_NOTE, &
               trim(libname)//': '// &
               'Component %s contains %i BEAM4 type elements', &
               derrinfo, cerrinfo)
       END IF

1000   CONTINUE
    END DO

    ! delete temporary component
    cmd = 'CMDELE,XXXCOMP'
    iErr = RunCommand(LEN_TRIM(cmd), cmd)

    CALL TrackEnd('dat_components')

  END SUBROUTINE dat_components

!###########################################################

  SUBROUTINE ans2bmf_cstore(comp, csub, el_data)

    USE ansys_upf, ONLY : TrackBegin, TrackEnd, elmget, elmiqr, &
         elnext, etyget, erhandler
    USE ansys_par, ONLY : DB_NUMSELECTED, EL_DIM, EL_TYPE, ERH_NOTE, &
         NNMAX, PARMSIZE, KYOP1
    USE glans, ONLY : cmselect, esel
    USE LOCMOD, ONLY : libname
    USE ans_common, ONLY : comp_num, el_count, el_offset, ielc, &
         l_comp_ansys, l_comp_csub, l_csub_ename, l_comp_len, &
         l_comp_name, l_comp_pos, l_csub_postfix, &
         ANS_SHELL63, ANS_SHELL181, ANS_SHELL63_M, ANS_SHELL181_M

    ! Purpose:
    !
    ! Parameter:
    ! in/out Name          Task
    ! ----------------------------------------------------------------------
    ! Created: 2007-06-01  hoel
    ! ======================================================================

    IMPLICIT NONE

    CHARACTER(LEN=*) :: comp
    INTEGER :: csub, el_data(*)

    INTEGER :: sub_length

    INTEGER :: elid, stat, type
    INTEGER :: iErr
    INTEGER, DIMENSION(EL_DIM) :: elmdat
    INTEGER, DIMENSION(NNMAX) :: nodes

    ! dataspace for feeding erhandler subroutine
    DOUBLE PRECISION, DIMENSION(10) ::  derrinfo
    CHARACTER(LEN=PARMSIZE), DIMENSION(10) :: cerrinfo

    CHARACTER(LEN=40), PARAMETER :: fname=__FILE__

    CALL TrackBegin('ans2bmf_cstore')

    CALL cmselect('s', 'XXXCOMP')

    IF (csub.EQ.ANS_SHELL63_M) THEN
       CALL esel('R', 'ENAME', VMIN=l_csub_ename(ANS_SHELL63))
    ELSE IF (csub.EQ.ANS_SHELL181_M) THEN
       CALL esel('R', 'ENAME', VMIN=l_csub_ename(ANS_SHELL181))
    ELSE
       CALL esel('R', 'ENAME', VMIN=l_csub_ename(csub))
    END IF

    IF ( csub.EQ.ANS_SHELL63_M .OR. &
         csub.EQ.ANS_SHELL181_M ) THEN
       ! deselect elements without membrane option set.
       elid = 0
10     elid = elnext(elid)
       IF (ELID.GT.0) THEN
          stat = elmget(elid, elmdat, nodes) ! element info
          if (stat.LE.0) THEN
             print *, 'Error should not occur 1', stat
          END IF
          type = elmdat(EL_TYPE)
          stat = etyget(type, ielc)
          if (stat.LE.0) THEN
             print *, 'Error should not occur 2', stat
          END IF
          IF (ielc(KYOP1).NE.1) THEN
             ! deselect element without membrane property
             CALL elsel(elid, -1)
          END IF
          GOTO 10
       END IF
    ELSE IF ( csub.EQ.ANS_SHELL63 .OR. &
         csub.EQ.ANS_SHELL181 ) THEN
       ! deselect elements with membrane option set.
       elid = 0
20     elid = elnext(elid)
       IF (ELID.GT.0) THEN
          stat = elmget(elid, elmdat, nodes) ! element info
          if (stat.LE.0) THEN
             print *, 'Error should not occur 3', stat
          END IF
          type = elmdat(EL_TYPE)
          stat = etyget(type, ielc)
          if (stat.LE.0) THEN
             print *, 'Error should not occur 4', stat
          END IF
          IF (ielc(KYOP1).EQ.1) THEN
             ! deselect element with membrane property
             CALL elsel(elid, -1)
          END IF
          GOTO 20
       END IF
    END IF
    sub_length=elmiqr(0, DB_NUMSELECTED)

    el_count = el_count + sub_length
    IF (sub_length.GT.0) THEN
       comp_num = comp_num+1
       l_comp_name(comp_num) = &
            comp(1:min(8,len_trim(comp))) // '_' // &
            l_csub_postfix(csub)
       l_comp_pos(comp_num)   = el_offset
       l_comp_len(comp_num)   = sub_length
       l_comp_ansys(comp_num) = comp
       l_comp_csub(comp_num)  = csub

       CALL ans2bmf_elread(el_data)

       derrinfo(1) = comp_num
       derrinfo(2) = l_comp_len(comp_num)
       cerrinfo(1) = l_comp_name(comp_num)
       CALL erhandler(fname, __LINE__, ERH_NOTE, &
            trim(libname)//':   comp - num  %i%/'// &
            trim(libname)//':        - name %s%/'// &
            trim(libname)//':        - len  %i', &
            derrinfo, cerrinfo)
    END IF

    CALL TrackEnd('ans2bmf_cstore')

  END SUBROUTINE ans2bmf_cstore

!-----------------------------------------------------------

  SUBROUTINE ans2bmf_elread(el_data)

    USE ansys_upf, ONLY : TrackBegin, TrackEnd, elmiqr, elnext, &
         erhandler
    USE ansys_par, ONLY : DB_NUMSELECTED, ERH_ERROR, PARMSIZE
    USE ans_common, ONLY : el_offset, n_elem
    USE LOCMOD, ONLY : libname

    ! Purpose:
    !
    ! Parameter:
    ! in/out Name          Task
    ! ----------------------------------------------------------------------
    ! Created: 2007-06-01  hoel
    ! ======================================================================

    IMPLICIT NONE

    INTEGER el_data(*)
    INTEGER n, nmax, el

    ! dataspace for feeding erhandler subroutine
    DOUBLE PRECISION, DIMENSION(10) ::  derrinfo
    CHARACTER(LEN=PARMSIZE), DIMENSION(10) :: cerrinfo

    CHARACTER(LEN=40), PARAMETER :: fname=__FILE__

    CALL TrackBegin('ans2bmf_elread')

    nmax = elmiqr(0, DB_NUMSELECTED)
    n_elem=n_elem-nmax
    IF (n_elem.LT.0) THEN
       derrinfo(1) = n_elem
       derrinfo(2) = nmax
       CALL erhandler(fname, __LINE__, ERH_ERROR, &
            trim(libname)//': '// &
            'ERROR: n_elem= %i , not enough heap space '// &
            'allocated. Still needed: %i integers.', &
            derrinfo, cerrinfo)
       CALL anserr(4,'Heap overflow',0.0,' ')
    END IF

    el = elnext(0)
    DO n = 1, nmax
       el_data(el_offset)=el
       el=elnext(el)
       el_offset=el_offset+1
    END DO

    CALL TrackEnd('ans2bmf_elread')

  END SUBROUTINE ans2bmf_elread

END MODULE mod_dat_components

! Local Variables:
! compile-command:"make -C .. test"
! End: