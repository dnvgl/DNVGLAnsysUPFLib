c     collect components

#ifndef DEBUG_LOC
#ifdef DEBUG
#define DEBUG_LOC print 100,__FILE__,__LINE__
#else
#define DEBUG_LOC
#endif !DEBUG_LOC
#endif

      SUBROUTINE dat_components(el_data)

#include "common.F"

      INTEGER el_data(*)

      INTEGER ansys_comp,an_cnum,n,m,el_b4, msglvl
      DOUBLE PRECISION res
      CHARACTER*(PARMSIZE) comp(max_components)

      LOGICAL batch
      COMMON /arguments/ batch

      CALL TrackBegin('dat_components')

c     find number of components: -> ansys_comp
      CALL ans2bmf_get_d('COMP,,NCOMP',res)
      ansys_comp = nint( res )

      derrinfo(1) = ansys_comp
      CALL erhandler(
     $     __FILE__,
     $     __LINE__,
     $     ANS_MSGLVL_NOTE,
     $     'ans2bmf: # of ANSYS components: %i',
     $     derrinfo, cerrinfo)

c     loop over all components
c       running # for sxf components created
      comp_num=0
c       running # of ANSYS components treated
      an_cnum=0
      DO n = 1, ansys_comp+1

         IF (comp_num.GE.max_components-10) THEN
            derrinfo(1) = comp_num
            derrinfo(2) = max_components
            CALL erhandler(
     $           __FILE__,
     $           __LINE__,
     $           ANS_MSGLVL_ERR,
     $           'ans2bmf: ERROR: %i components defined. Greater or '//
     $           'too close to maximal number %i',
     $           derrinfo, cerrinfo)
            CALL anserr(4,'Too many components',0.0,' ')
         END IF

         IF (n.GT.ansys_comp) THEN

c     generate components for all elements not in any component
            comp(n) = 'NotNamed'
            cerrinfo(1) = comp(n)
            CALL erhandler(
     $           __FILE__,
     $           __LINE__,
     $           ANS_MSGLVL_NOTE,
     $           'ans2bmf: Component %s', derrinfo, cerrinfo)
            cmd = 'ESEL,ALL'
            i = runcmd(cmd)
            an_cnum = an_cnum+1

         ELSE

c     get name of component: -> comp(an_cnum)
            WRITE(para,'(i8)') n
            CALL ans2bmf_get_s('COMP,'//para//',NAME',comp(an_cnum+1))

c     get type of component
            CALL ans2bmf_get_d('COMP,'//comp(an_cnum+1)
     x           //',TYPE',res)

c     check if this is a component of elements
            IF (nint(res).GT.10) THEN
               cerrinfo(1) = comp(an_cnum+1)
               IF (batch) THEN
                  msglvl = ANS_MSGLVL_FTL
               ELSE
                  msglvl = ANS_MSGLVL_ERR
               END IF
               CALL erhandler(
     $ __FILE__,
     $              __LINE__,
     $              msglvl,
     $              'ans2bmf: Component %s is not of type ELEM'//
     $              '\nElements in assemblies are not supported.',
     $              derrinfo, cerrinfo)
               an_cnum = an_cnum + 1
               GOTO 1000
            ENDIF
            IF (nint(res).NE.2) THEN
               cerrinfo(1) = comp(an_cnum+1)
               CALL erhandler(
     $ __FILE__,
     $              __LINE__,
     $              ANS_MSGLVL_WARN,
     $              'ans2bmf: Component %s is not of type ELEM',
     $              derrinfo, cerrinfo)
               an_cnum = an_cnum + 1
               GOTO 1000
            ENDIF

c     select component
           an_cnum = an_cnum+1
           cmd = 'CMSEL,S,'//comp(an_cnum)
           i=runcmd(cmd)
           cerrinfo(1) = comp(an_cnum)
           CALL erhandler(
     $          __FILE__,
     $          __LINE__,
     $          ANS_MSGLVL_NOTE,
     $          'ans2bmf: Treating ANSYS component %s',
     $          derrinfo, cerrinfo)
           el_length = elmiqr(0, DB_NUMSELECTED)
           derrinfo(1) =  el_length
           CALL erhandler(
     $          __FILE__,
     $          __LINE__,
     $          ANS_MSGLVL_NOTE,
     $          'ans2bmf:     # of elements:     %i',
     $     derrinfo, cerrinfo)

        END IF

c     unselect all previous components
        DO m = 1, an_cnum-1
           cmd = 'CMSEL,U,'//comp(m)
           i=runcmd(cmd)
        END DO

c     find # of elements in component
        el_length = elmiqr(0, DB_NUMSELECTED)
        derrinfo(1) = el_length
        CALL erhandler(
     $       __FILE__,
     $       __LINE__,
     $       ANS_MSGLVL_NOTE,
     $       'ans2bmf:     # of elements not in any previous '//
     $       'component: %i',
     $       derrinfo, cerrinfo)

        IF (el_length.GT.0) THEN

c     define temporary component
           cmd = 'CM,XXXCOMP,ELEM'
           i=runcmd(cmd)

c     subselect all different types in the component an_cnum and store on stack
           el_count = 0
           DO m = 1, csub_num
              CALL ans2bmf_cstore(comp(an_cnum), m, el_data)
           END DO
        END IF

        IF (el_count.LT.el_length) THEN
           CALL a2b_message('comp',n,n)

c     check specifically for BEAM4 elements
           cmd = 'CMSEL,S,XXXCOMP'
           i=runcmd(cmd)
           cmd='ESEL,R,ENAME,,BEAM4'
           i=runcmd(cmd)
           el_b4 = elmiqr(0, DB_NUMSELECTED)
           derrinfo(1) = el_b4
           cerrinfo(1) = comp(an_cnum)
           CALL erhandler(
     $          __FILE__,
     $          __LINE__,
     $          ANS_MSGLVL_NOTE,
     $          'ans2bmf: Component %s contains %i BEAM4 type elements',
     $     derrinfo, cerrinfo)
        END IF

 1000   CONTINUE
      END DO

c     delete temporary component
      cmd = 'CMDELE,XXXCOMP'
      i=runcmd(cmd)

      CALL TrackEnd('dat_components')

      END

c###########################################################

      SUBROUTINE ans2bmf_cstore(comp, csub, el_data)

#include "common.F"

      CHARACTER(LEN=*) :: comp
      INTEGER csub, el_data(*)

      INTEGER sub_length

      INTEGER elid, stat, type

      CALL TrackBegin('ans2bmf_cstore')

      cmd = 'CMSEL,S,XXXCOMP'
      i=runcmd(cmd)

      IF (csub.EQ.ANS_SHELL63_M) THEN
         cmd = 'ESEL,R,ENAME,,' // l_csub_ename(ANS_SHELL63)
      ELSE IF (csub.EQ.ANS_SHELL181_M) THEN
         cmd = 'ESEL,R,ENAME,,' // l_csub_ename(ANS_SHELL181)
      ELSE
         cmd = 'ESEL,R,ENAME,,' // l_csub_ename(csub)
      END IF
      i=runcmd(cmd)

      IF ( csub.EQ.ANS_SHELL63_M .OR.
     $     csub.EQ.ANS_SHELL181_M ) THEN
        ! deselect elements without membrane option set.
         elid = 0
 10      elid = elnext(elid)
         IF (ELID.GT.0) THEN
            stat = elmget(elid, elmdat(1), nodes(1)) ! element info
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
      ELSE IF ( csub.EQ.ANS_SHELL63 .OR.
     $          csub.EQ.ANS_SHELL181 ) THEN
         ! deselect elements with membrane option set.
         elid = 0
 20      elid = elnext(elid)
         IF (ELID.GT.0) THEN
            stat = elmget(elid, elmdat(1), nodes(1)) ! element info
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
      sub_length=elmiqr(0,DB_NUMSELECTED)


      el_count = el_count + sub_length
      IF (sub_length.GT.0) THEN
         comp_num = comp_num+1
         l_comp_name(comp_num) =
     x        comp(1:min(8,len_trim(comp))) // '_' //
     $        l_csub_postfix(csub)
         l_comp_pos(comp_num)   = el_offset
         l_comp_len(comp_num)   = sub_length
         l_comp_ansys(comp_num) = comp
         l_comp_csub(comp_num)  = csub

         CALL ans2bmf_elread(el_data)

         derrinfo(1) = comp_num
         derrinfo(2) = l_comp_len(comp_num)
         cerrinfo(1) = l_comp_name(comp_num)
         CALL erhandler(
     $        __FILE__,
     $        __LINE__,
     $        ANS_MSGLVL_NOTE,
     $        'ans2bmf:   comp - num  %i\n'//
     $        'ans2bmf:        - name %s\n'//
     $        'ans2bmf:        - len  %i',
     $        derrinfo, cerrinfo)
      END IF

      CALL TrackEnd('ans2bmf_cstore')

      END

c-----------------------------------------------------------

      SUBROUTINE ans2bmf_elread(el_data)

#include "common.F"

      INTEGER el_data(*)
      INTEGER n, nmax, el

#ifdef DEBUG
 100  FORMAT (A,':',I3,':ans2bmf_elread')
#endif
      CALL TrackBegin('ans2bmf_elread')

      nmax = elmiqr(0,DB_NUMSELECTED)
      n_elem=n_elem-nmax
      IF (n_elem.LT.0) THEN
         derrinfo(1) = n_elem
         derrinfo(2) = nmax
         CALL erhandler(
     $ __FILE__,
     $        __LINE__,
     $        ANS_MSGLVL_ERR,
     $        'ans2bmf: ERROR: n_elem= %i , not enough heap space '//
     $        'allocated. Still needed: %i integers.',
     $     derrinfo, cerrinfo)
         CALL anserr(4,'Heap overflow',0.0,' ')
      END IF

      el = elnext(0)
      DO n = 1, nmax
         el_data(el_offset)=el
         el=elnext(el)
         el_offset=el_offset+1
      END DO

      CALL TrackEnd('ans2bmf_elread')

      END

c Local Variables:
c compile-command:"make -C .. test"
c End:
