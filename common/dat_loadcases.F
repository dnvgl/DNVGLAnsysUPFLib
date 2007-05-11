      SUBROUTINE dat_loadcases()

#include "common.F"

      CHARACTER(LEN=2) :: ext
      CHARACTER(LEN=1) :: cl
      CHARACTER(LEN=5) :: c5
      LOGICAL ex, eof
      INTEGER*2 l
      INTEGER s_file

      CALL TrackBegin('dat_loadcases')

      max_loadcases = 0

c     inquiry on nodal loads in model and define global parameter
c     n_nloads

      n_nloads = foriqr(0,DB_NUMDEFINED)
      IF (n_nloads.GT.0) THEN
         max_loadcases = max_loadcases + 1
      END IF

c     check on files for loadcases written with lswrite and define
c     global parameter n_sfiles
      n_sfiles = 0
      ex = .TRUE.
      l = 1
      s_file = 12

c     check if file.s** with # l exists
      DO WHILE (ex)

c     create filename
         IF (l.LE.9) THEN
            WRITE(cl,'(i1)') l
            ext = '0'//cl
         ELSE
            WRITE(ext,'(i2)') l
         ENDIF

c     check
         INQUIRE(FILE=jobname(1:len_trim(jobname))
     x        //'.s'//ext, EXIST=ex)

         IF (ex) THEN
            l = l + 1
c     and count all load lines on file
            eof = .FALSE.
            OPEN(UNIT=s_file, FILE=jobname(1:len_trim(jobname))
     x           //'.s'//ext)
            DO WHILE (.NOT.eof)
               READ(s_file,'(a)', END = 500) c5
               IF (c5 .EQ. 'F,  ') THEN
                  n_nloads = n_nloads + 1
               END IF
            END DO
 500        CONTINUE
         ELSE
c     we have found the last file
            l = l - 1
            GOTO 100
         END IF
      END DO

 100  n_sfiles = l

      max_loadcases = max_loadcases + n_sfiles
      derrinfo(1) = max_loadcases
      CALL erhandler(
     $     __FILE__,
     $     __LINE__,
     $     ANS_MSGLVL_NOTE,
     $     'ans2bmf:   loadcases defined:  %i', derrinfo, cerrinfo)

      CALL TrackEnd('dat_loadcases')

      END

c Local Variables:
c compile-command:"make -C .. test"
c End:
