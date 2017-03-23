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

    USE ansys_upf, ONLY : elmiqr, erhandler
    USE ansys_par, ONLY : CMD_MAX_LENG, ERH_FNAME_LEN, DB_NUMSELECTED, PARMSIZE

    USE dnvglans
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

    INTEGER, DIMENSION(*) :: el_data

    INTEGER :: ansys_comp, an_cnum, n, m, el_b4
    INTEGER :: res

    INTEGER :: iErr

    CHARACTER(LEN=CMD_MAX_LENG) :: cmd
    CHARACTER(LEN=PARMSIZE) :: para

    LOGICAL :: flag

    CHARACTER(LEN=ERH_FNAME_LEN), PARAMETER :: fname=__FILE__

    CALL ezTrackBegin('dat_components')

    ! find number of components: -> ansys_comp
    flag = get(ansys_comp, Entity='COMP', ENTNUM=0, Item1='NCOMP   ')
    if (flag) THEN
       CALL ans_fatal(fname, __LINE__, libname, &
            'Could not determine number of components.')
    END IF

    derrinfo(1) = ansys_comp
    CALL ans_note(fname, __LINE__, libname, ' # of ANSYS components: %i')

    ! loop over all components
    !  running # for sxf components created
    comp_num = 0
    !  running # of ANSYS components treated
    an_cnum = 0
    DO n = 1, ansys_comp+1

       IF (comp_num.GE.max_components-csub_num) THEN
          derrinfo(1) = comp_num
          derrinfo(2) = max_components
          CALL ans_error(fname, __LINE__, libname, &
               & 'ERROR: %i components defined. Greater or '// &
               & 'too close to maximal number %i')
          CALL anserr(4,'Too many components',0.0,' ')
       END IF

       IF (n.GT.ansys_comp) THEN

          ! generate components for all elements not in any component
          comp_name(n) = 'NOTNAMED'
          cerrinfo(1) = trim(comp_name(n))
          CALL ans_note(fname, __LINE__, libname, ': Component %s')
          CALL esel('ALL')
          DO m = 1, n-1
             CALL cmselect('U', trim(comp_name(m)))
          END DO
          cmd = 'CM,NOTNAMED,ELEM'
          iErr = ezRunCommand(cmd)
          an_cnum = an_cnum+1

       ELSE

          ! get name of component: -> comp_name(an_cnum)
          WRITE(para,'(i8)') n
          CALL ans2bmf_get_s('COMP,'//para//',NAME', &
               comp_name(an_cnum+1))

          ! get type of component
          cerrinfo(1) = comp_name(an_cnum+1)
          flag = get(res, 'COMP', comp_name(an_cnum+1), 'TYPE    ')
          if (flag .OR. res.EQ.0) THEN
             cerrinfo(1) = comp_name(an_cnum+1)
             CALL ans_fatal(fname, __LINE__, libname,  &
                  & 'Could not determine type for component named %s.')
          END IF

!     check if this is a component of elements
          IF (res.GT.10) THEN
             cerrinfo(1) = comp_name(an_cnum+1)
             IF (batch) THEN
                CALL ans_fatal(fname, __LINE__, libname, &
                     & 'Component %s is not of type ELEM'// &
                     & '\nElements in assemblies are not supported.')
             ELSE
                CALL ans_error(fname, __LINE__, libname, &
                     & 'Component %s is not of type ELEM'// &
                     & '\nElements in assemblies are not supported.')
             END IF
             an_cnum = an_cnum + 1
             GOTO 1000
          ENDIF
          IF (res.NE.2) THEN
             cerrinfo(1) = comp_name(an_cnum+1)
             CALL ans_warn(fname, __LINE__, libname, &
                  & 'Component %s is not of type ELEM')
             an_cnum = an_cnum + 1
             GOTO 1000
          ENDIF

          ! select component
          an_cnum = an_cnum+1
          CALL cmselect('S', comp_name(an_cnum))
          cerrinfo(1) = comp_name(an_cnum)
          CALL ans_note(fname, __LINE__, libname, 'Treating ANSYS component %s')
          el_length = elmiqr(0, DB_NUMSELECTED)
          derrinfo(1) =  el_length
          CALL ans_note(fname, __LINE__, libname, '    # of elements:     %i')

       END IF

       ! unselect all previous components
       DO m = 1, an_cnum-1
          CALL cmselect('U', comp_name(m))
       END DO

       ! find # of elements in component
       el_length = elmiqr(0, DB_NUMSELECTED)
       derrinfo(1) = el_length
       CALL ans_note(fname, __LINE__, libname, &
            & '    # of elements not in any previous component: %i')

       IF (el_length.GT.0) THEN

          ! define temporary component
          cmd = 'CM,XXXCOMP,ELEM'
          iErr = ezRunCommand(cmd)

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
          CALL ans_note(fname, __LINE__, libname, &
               & 'Component %s contains %i BEAM4 type elements')
       END IF

1000   CONTINUE
    END DO

    ! delete temporary component
    cmd = 'CMDELE,XXXCOMP'
    iErr = ezRunCommand(cmd)

    CALL ezTrackEnd()

  END SUBROUTINE dat_components

!###########################################################

  SUBROUTINE ans2bmf_cstore(comp, csub, el_data)

    USE ansys_upf, ONLY : elmget, elmiqr, elnext, etyget
    USE ansys_par, ONLY : DB_NUMSELECTED, EL_DIM, EL_TYPE, ERH_FNAME_LEN, &
         & NNMAX, PARMSIZE, KYOP1
    USE dnvglans
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

    CHARACTER(LEN=*), INTENT(IN) :: comp
    INTEGER, INTENT(IN) :: csub
    INTEGER, INTENT(INOUT), DIMENSION(*) :: el_data

    INTEGER :: sub_length

    INTEGER :: elid, stat, type
    INTEGER :: iErr
    INTEGER, DIMENSION(EL_DIM) :: elmdat
    INTEGER, DIMENSION(NNMAX) :: nodes

    CHARACTER(LEN=ERH_FNAME_LEN), PARAMETER :: fname=__FILE__

    CALL ezTrackBegin('ans2bmf_cstore')

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
             derrinfo(1) = stat
             CALL ans_fatal(fname, __LINE__, libname, &
                  & ':Error should not occur: "elmget" returns %d')
          END IF
          type = elmdat(EL_TYPE)
          stat = etyget(type, ielc)
          if (stat.LE.0) THEN
             derrinfo(1) = stat
             CALL ans_fatal(fname, __LINE__, libname, &
                  & 'Error should not occur: "etyget" returns %d')
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
             derrinfo(1) = stat
             CALL ans_fatal(fname, __LINE__, libname, &
                  'Error should not occur: "elmget" returns %d')
          END IF
          type = elmdat(EL_TYPE)
          stat = etyget(type, ielc)
          if (stat.LE.0) THEN
             derrinfo(1) = stat
             CALL ans_fatal(fname, __LINE__, libname, &
                  & 'Error should not occur: "etyget" returns %d')
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
       call ans_note(fname, __LINE__, libname, '  comp - num  %i')
       derrinfo(1) = l_comp_len(comp_num)
       call ans_note(fname, __LINE__, libname, '       - name %s')
       cerrinfo(1) = l_comp_name(comp_num)
       call ans_note(fname, __LINE__, libname, '       - len  %i')
    END IF

    CALL ezTrackEnd()

  END SUBROUTINE ans2bmf_cstore

!-----------------------------------------------------------

  SUBROUTINE ans2bmf_elread(el_data)

    USE ansys_upf, ONLY : elmiqr, elnext
    USE ansys_par, ONLY : DB_NUMSELECTED, ERH_FNAME_LEN, PARMSIZE
    USE ans_common, ONLY : el_offset, n_elem
    USE dnvglans, ONLY : ezTrackBegin, ezTrackEnd, derrinfo, cerrinfo, ans_error
    USE LOCMOD, ONLY : libname

    ! Purpose:
    !
    ! Parameter:
    ! in/out Name          Task
    ! ----------------------------------------------------------------------
    ! Created: 2007-06-01  hoel
    ! ======================================================================

    IMPLICIT NONE

    INTEGER, INTENT(INOUT), DIMENSION(*) :: el_data
    INTEGER :: n, nmax, el

    CHARACTER(LEN=ERH_FNAME_LEN), PARAMETER :: fname=__FILE__

    CALL ezTrackBegin('ans2bmf_elread')

    nmax = elmiqr(0, DB_NUMSELECTED)
    n_elem=n_elem-nmax
    IF (n_elem.LT.0) THEN
       derrinfo(1) = n_elem
       derrinfo(2) = nmax
       CALL ans_error(fname, __LINE__, libname, &
            & 'ERROR: n_elem= %i , not enough heap space '// &
            & 'allocated. Still needed: %i integers.')
       CALL anserr(4,'Heap overflow',0.0,' ')
    END IF

    el = elnext(0)
    DO n = 1, nmax
       el_data(el_offset)=el
       el=elnext(el)
       el_offset=el_offset+1
    END DO

    CALL ezTrackEnd()

  END SUBROUTINE ans2bmf_elread

END MODULE mod_dat_components

! Local Variables:
! compile-command:"make -C .. test"
! End:
