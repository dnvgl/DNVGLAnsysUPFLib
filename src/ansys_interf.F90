! Copyright (C) 2012 by Germanischer Lloyd SE

! ======================================================================
! Task      Provide module for routines for interfacing with ansys files.
! ----------------------------------------------------------------------
! Author    Berthold Hoellmann <hoel@GL-group.com>
! Project   ans_lib
! ======================================================================


! ID: $Id$

!DEC$ NOFREEFORM
#include "computer.h"
!DEC$ FREEFORM

MODULE ansys_interf

! Guide to Interfacing with ANSYS

! Chapter 2: Accessing Binary Data Files

! 2.1.5. binini (Initializing Buffered Binary I/O Systems)

! *deck,binini
  INTERFACE
     SUBROUTINE binini (iott)
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: iott
     END SUBROUTINE binini
  END INTERFACE
! *** primary function: initialize buffered binary i/o system
! --- This routine is intended to be used in standalone programs.
! --- This routine should not be linked into the ANSYS program.

! *** Notice - This file contains ANSYS Confidential information ***

! input arguments:
!     iott (int,sc,in) - output unit number for error output

! output arguments: none


! 2.1.6. Function sysiqr (Retrieving the Status of a File)

! *deck,sysiqr
  INTERFACE
     FUNCTION sysiqr (nunit,fname,lname_in,inqr_in)
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: nunit
       CHARACTER(LEN=50), INTENT(IN) :: fname
       INTEGER, INTENT(IN) :: lname_in
       CHARACTER, INTENT(IN) :: inqr_in
       INTEGER :: sysiqr
     END FUNCTION sysiqr
  END INTERFACE

! *** primary function: do a file system inquire (system dependent)

! *** Notice - This file contains ANSYS Confidential information ***

! input arguments:
!    variable (typ,siz,intent)     description
!    nunit (int,sc,in)           - fortran unit number (used only for inqr='O')
!    fname (chr,sc,in)            - name of file
!    lname_in (int,sc,in)         - length of file name (characters, max=50)
!    inqr_in (chr,sc,in)          - character key for information requested
!                                   = 'E' - return whether file exists
!                                       sysiqr = 1 - file exists
!                                              = 0 - file does not exist
!                                              < 0 - error occured
!                                   = 'O' - return whether file is open
!                                       sysiqr = 1 - file is open
!                                              = 0 - file is closed
!                                              < 0 - error occured
!                                   = 'N' - return unit number of file
!                                       sysiqr > 0 - unit number for file
!                                              = 0 - file not assigned to a unit
!                                              < 0 - error occured

! output arguments:
!   sysiqr (int,func,out)        - the returned value of sysiqr is based on
!                                       setting of inqr

! 2.1.7. Function biniqr8 (Retrieving System-Dependent Parameters)

! deck,biniqr8
  INTERFACE
     FUNCTION biniqr8 (nblk,key)
       IMPLICIT NONE
       LONGINT, INTENT(IN) :: nblk
       INTEGER, INTENT(IN) :: key
       LONGINT :: biniqr8
     END FUNCTION biniqr8
  END INTERFACE
! *** primary function: get data about a block i/o buffer
! --- This routine is intended to be used in standalone programs.
! --- This routine should not be linked into the ANSYS program.

! *** Notice - This file contains ANSYS Confidential information ***

!  input arguments:
!      nblk     (int,sc,in)       - the block number for the inquiry
!                                   or zero (see below)
!      key      (int,sc,in) - key for information requested
!                      nblk = 0 - return information about system/file
!                               key = 1 - return system block size
!                                   = 2 - return number of integers per dp
!                                   = 3 - return filename length
!                                     5 = return integers per LONG
!                      nblk > 0 - return information about this block
!                               key = 1 - return fortran unit number
!                                   = 2 - return number of blocks in file
!                                   = 3 - return length of page (32 bit words)
!                                   = 4 - return open status
!                                         0 - file close
!                                         1 - file open
!                                   = 5 - return file format
!                                         0 - internal format
!                                         1 - external format
!                                   = 6 - return read/write status
!                                         0 - both read & write
!                                         1 - read
!                                         2 - write
!                                   = 7 - return current position on file
!                                   = 8 - return maximum length of file
!                                         (in words)
!                                   = 9 - return starting word for this page
!                                         in buffer
!                                   =10 - return base location
!                                   =11 - return debug key
!                                   =12 - return absolute (non-base) key
!                                   =15 - return max record written
!                                   =16 - return swap and record header key
!                                   =17 - return precision key
!  output arguments:
!      biniqr (int,func,out)    - the returned value of biniqr is based on
!                                     setting of nblk and key

  INTERFACE
     FUNCTION biniqr (nblk,key)
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: nblk
       INTEGER, INTENT(IN) :: key
       INTEGER :: biniqr
     END FUNCTION biniqr
  END INTERFACE

! 2.1.8. Function binset (Opening a Blocked Binary File or Initializing Paging
!        Space)

! *deck,binset
  INTERFACE
     FUNCTION binset (nblk,nunit,ikeyrw,istart,paglen,npage,pname,  &
          nchar,kext,Buffer4)
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: nblk
       INTEGER, INTENT(IN) :: nunit
       INTEGER, INTENT(IN) :: ikeyrw
       INTEGER, INTENT(IN) :: istart
       INTEGER, INTENT(IN) :: paglen
       INTEGER, INTENT(IN) :: npage
       CHARACTER(LEN=*), INTENT(IN) :: pname
       INTEGER, INTENT(IN) :: nchar
       INTEGER, INTENT(IN) :: kext
       INTEGER, INTENT(INOUT), DIMENSION(paglen*npage*nblk) :: Buffer4
       INTEGER :: binset
     END FUNCTION binset
  END INTERFACE

! *** primary function: initialize paging space for a blocked binary file.
! binset should be used to open a blocked file
! before binrd8 or binwrt8 are used. binclo should
! be used to close the file.
! --- This routine is intended to be used in standalone programs.
! --- This routine should not be linked into the ANSYS program.

! *** Notice - This file contains ANSYS Confidential information ***

!  input arguments:
!     nblk      (int,sc,in)       - block number (1 to BIO_MAXFILES max)
!     nunit     (int,sc,in)       - fortran unit number for the file
!                                   (if 0, bit bucket)
!     ikeyrw    (int,sc,in)       - read/write flag
!                                   = 0 - both read & write
!                                   = 1 - read
!                                   = 2 - write
!                                   = 9 - read only
!       NOTE: 0 may write, but the file length may not be extended and
!               the file may or may not exist
!             1 reads only, but the file protection must set set to "rw"
!             2 may extend the file length and the file is a new file
!             9 reads only, but the file protection may be "r" only
!     istart (int,sc,in)          - starting location in buffer array
!                                   usually 1 for nblk=1, paglen*npage+1
!                                   for nblk=2,etc.
!     paglen (int,sc,in)          - page length in integer*4 words for external
!                                   files
!                                   paglen should always be a multiple of
!                                   512 words for efficiency
!     npage (int,sc,in)           - number of pages (1 to BIO_MAXBLOCKS max)
!     pname (chr,ar(*),in)        - name of the file
!     nchar (int,sc,in)           - number of characters in the file name (not
!                                   used)
!     kext (int,sc,in)            - no longer used, always external format
!     Buffer4 (i4, ar(*),inout)   - work array for paging, should be
!                                   dimensioned to paglen*npage*nblk (max)
!  output arguments:
!     binset (int,func,out)       - error status
!                                   = 0 - no error
!                                   <>0 - error occurred
!     Buffer4 (i4, ar(*),inout)   - work array for paging

! 2.1.9. Subroutine bintfo (Defining Data for a Standard ANSYS File Header)

!deck,bintfo
  INTERFACE
     SUBROUTINE bintfo (title,jobnam,units,code)
       IMPLICIT NONE
       CHARACTER(LEN=80), INTENT(IN), DIMENSION(2) :: title
       CHARACTER(LEN=8), INTENT(IN) :: jobnam
       INTEGER, INTENT(IN) :: units
       INTEGER, INTENT(IN) :: code
     END SUBROUTINE bintfo
  END INTERFACE
! *** primary function:    set information necessary for binhed
! --- This routine is intended to be used in standalone programs.
! --- This routine should not be linked into the ANSYS program.
!

! *** Notice - This file contains ANSYS Confidential information ***
!
!     typ=int,dp,log,chr,dcp   siz=sc,ar(n)   intent=in,out,inout
!
!  input arguments:
!     variable (typ,siz,intent)     description
!     title    (chr*80,ar(2),in)  - main title and 1st subtitle
!     jobnam   (chr*8,sc,in)      - jobname
!     units    (int,sc,in)        - units
!                                   = 0 - user defined units
!                                   = 1 - SI (MKS)
!                                   = 2 - CSG
!                                   = 3 - U.S. Customary, using feet
!                                   = 4 - U.S. Customary, using inches
!                                   = 6 - MPA
!                                   = 7 - uMKS
!     code     (int,sc,in)        - code defining 3rd party vendor
!                                   (contact ANSYS, Inc. for code assignment)
!
!  output arguments:
!     none

! 2.1.10. Subroutine binhed (Writing the Standard ANSYS File Header)
!deck,binhed
  INTERFACE
     SUBROUTINE binhed8 (nblk,nunit,filpos,buffer)
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: nblk
       INTEGER, INTENT(IN) :: nunit
       LONGINT, INTENT(OUT) :: filpos
       INTEGER, DIMENSION(*), INTENT(INOUT) :: buffer
     END SUBROUTINE binhed8
  END INTERFACE
! *** primary function: put standard header on a binary file, all
!     permanent binary files should have this header
! *** secondary functions: return the first data position
! --- This routine is intended to be used in standalone programs.
! --- This routine should not be linked into the ANSYS program.

! *** Notice - This file contains ANSYS Confidential information ***

!  input arguments:
!     nblk (int,sc,in)         - block number of open binary file
!                                (as defined with subroutine binset)
!     nunit (int,sc,in)        - the unit number for this file
!     buffer (int,ar(*),inout) - work array for paging, should be the
!                                same array as used in binset
!  output arguments:
!     filpos (int,sc,out) - the position after the header
!     buffer (int,ar(*),inout) - work array for paging
!   **********  ANSYS standard header data description (100 words) **********
!  loc   no. words   contents
!   1       1        fortran unit number
!   2       2        file format
!                    = 0 - internal format
!                    = 1 - external format
!   3       1        time in compact form (ie 130619 is 13:06:19)
!   4       1        date in compact form (ie 19981023 is 10/23/1998)
!   5       1        units
!                    = 0 - user defined units
!                    = 1 - SI (MKS)
!                    = 2 - CSG
!                    = 3 - U.S. Customary, using feet
!                    = 4 - U.S. Customary, using inches
!                    = 6 - MPA
!                    = 7 - uMKS
!   6       1        User_Linked
!  10       1        revision in text format ' 5.0' (inexc4)
!  11       1        date of revision release for this version
!  12       3        machine identifier - 3 4-character strings
!  15       2        jobname - 2 4-character strings
!  17       2        product name - 2 4-character strings
!  19       1        special version label - 1 4-character string
!  20       3        user name - 3 4-character strings
!  23       3        machine identifier - 3 4-character strings
!  26       1        system record size at file write
!  27       1        maximum file length
!  28       1        maximum record number
!  31       8        jobname - 8 4-character strings
!  41      20        main title - 20 4-character strings
!  61      20        first subtitle - 20 4-character strings
!  95       1        split point of file
!                     NOTE: Split files are not support by binlib!
! 97-98     2        LONGINT of file size at write
  INTERFACE
     SUBROUTINE binhed (nblk,nunit,filpos,buffer)
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: nblk
       INTEGER, INTENT(IN) :: nunit
       INTEGER, INTENT(OUT) :: filpos
       INTEGER, DIMENSION(*), INTENT(INOUT) :: buffer
     END SUBROUTINE binhed
  END INTERFACE

!  2.1.11. Subroutine binrd8 (Reading Data from a Buffered File)

! *deck,binrd8
  INTERFACE 
     SUBROUTINE binrd8 (nblk,LongLocL,leng,ivect,kbfint,Buffer4)
       !DEC$ ATTRIBUTES NO_ARG_CHECK :: ivect
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: nblk
       LONGINT, INTENT(INOUT) :: LongLocL
       INTEGER, INTENT(IN) :: leng
       INTEGER, INTENT(OUT), DIMENSION(*) :: ivect
       INTEGER, INTENT(OUT) ::kbfint
       INTEGER, INTENT(INOUT), DIMENSION(*) :: Buffer4
     END SUBROUTINE binrd8
  END INTERFACE

! ********** buffer read routine **********

! *** Notice - This file contains ANSYS Confidential information ***

!  input arguments:
!   nblk   (int,sc,in)     - block number. see fd___(i.e. fdtri for tri
!
!   LongLocL(LONG,sc,inout)- location in integer*4 words of the startin
!                            position on the file.
!   leng (int,sc,inout)    - number of words to read into ivect. (must be
!                            less or equal to dimension given to ivect in
!                            the calling routine). if ivect is to be used
!                            as integers, use as is. if ivect is to be
!                            used for double precision numbers, it must be
!                            increased by multiplying it by INTPDP.
!                            if negative, skip record and do not return
!                            data(results).
!   Buffer4 (i4,ar(*),inout) - work array for paging, should be the
!                              same array as used in binset

!  output arguments:
!   LongLocL(LONG,sc,inout)- location in integer*4 words of the current
!                            position on the file. It is updated after
!                            each read operation
!   leng (int,sc,inout)    - tells you how many items it actually read(in
!                            integer words).
!                            if zero, end of file(error case)
!   ivect (int,ar(*),out)  - results (can be either integer or double
!                            precision in the calling routine)
!   kbfint (int,sc,out)    - key for type(used only for AUX2 dump)
!                           = 0 double precision data
!                           > 0 integer data(usually the same as leng)
!   Buffer4 (i4,ar(*),inout) - work array for paging

! Versions of binrd8/binwrt8 exist without the "8" suffix (binrd/binwrt)
! that take a regular integer for the second argument. These
! subroutines, therefore, cannot address large files where the file
! position exceeds 2**31. Use the binrd8/binwrt8 versions for any new
! programs.

  INTERFACE 
     SUBROUTINE binrd (nblk,LocL,leng,ivect,kbfint,Buffer4)
       !DEC$ ATTRIBUTES NO_ARG_CHECK :: ivect
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: nblk
       INTEGER, INTENT(INOUT) :: LocL
       INTEGER, INTENT(IN) :: leng
       INTEGER, INTENT(OUT), DIMENSION(*) :: ivect
       INTEGER, INTENT(OUT) ::kbfint
       INTEGER, INTENT(INOUT), DIMENSION(*) :: Buffer4
     END SUBROUTINE binrd
  END INTERFACE

! 2.1.12. Subroutine binwrt8 (Writing Data to a Buffered File)

!deck,binwrt8
  INTERFACE
     SUBROUTINE binwrt8 (nblk,LongLocL,leng,ivect,kbfint,Buffer4)
       !DEC$ ATTRIBUTES NO_ARG_CHECK :: ivect
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: nblk
       LONGINT, INTENT(INOUT) :: LongLocL
       INTEGER, INTENT(IN) :: leng
       INTEGER, DIMENSION(leng), INTENT(IN) :: ivect
       INTEGER, INTENT(IN) :: kbfint
       INTEGER, DIMENSION(*), INTENT(INOUT) :: Buffer4
     END SUBROUTINE binwrt8
  END INTERFACE
! *** primary function: buffer write routine
! --- This routine is intended to be used in standalone programs.
! --- This routine should not be linked into the ANSYS program.

! *** Notice - This file contains ANSYS Confidential information ***

!  input arguments:
!   nblk (int,sc,in)       - block number. see fd___(i.e. fdtri for tri
!
!   LongLocL(LONG,sc,inout)- location in integer words of the starting
!                            position on the file.
!   leng (int,sc,in)       - number of words to read from ivect. (must be
!                            less or equal to dimension given to ivect in
!                            the calling routine). if ivect is to be used
!                            as integers, use as is. if ivect is to be
!                            used for double precision numbers, it must be
!                            increased by multiplying it by INTPDP.
!   ivect (int,ar(*),in)   - data to be written onto the file(can be either
!                            integer or double precision in the calling
!                            routine)
!   kbfint (int,sc,in)     - key for type(used only for AUX2 dump)
!                           = 0 double precision data
!                           > 0 integer data(usually the same as leng)
!   Buffer4 (int,ar(*),inout) - work array for paging, should be the
!                                   same array as used in binset on this
!                                   block
!  output arguments:
!   LongLocL(LONG,sc,inout)- location in integer words of the current
!                            position on the file. It is updated after
!                            each write operation
!   ivect (int,ar(*),out)  - vector containing record to be written
!   Buffer4 (int,ar(*),inout) - work array for paging

! Versions of binrd8/binwrt8 exist without the "8" suffix
! (binrd/binwrt) that take a regular integer for the second
! argument. These subroutines, therefore, cannot address large files
! where the file position exceeds 2**31. Use the binrd8/binwrt8
! versions for any new programs.

  INTERFACE
     SUBROUTINE binwrt (nblk, LocL, leng, ivect, kbfint, Buffer4)
       !DEC$ ATTRIBUTES NO_ARG_CHECK :: ivect
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: nblk
       INTEGER, INTENT(INOUT) :: LocL
       INTEGER, INTENT(IN) :: leng
       INTEGER, DIMENSION(*), INTENT(IN) :: ivect
       INTEGER, INTENT(IN) :: kbfint
       INTEGER, DIMENSION(*), INTENT(INOUT) :: Buffer4
     END SUBROUTINE binwrt
  END INTERFACE

! 2.1.13. Subroutine exinc4 (Decoding an Integer String into a Character String)

! 2.1.14. Subroutine inexc4 (Coding a Character String into an Integer String)

!deck,inexc4
  INTERFACE
     SUBROUTINE inexc4 (chin,ichext,n)
       IMPLICIT NONE
       CHARACTER, DIMENSION(n), INTENT(IN) :: chin
       INTEGER, DIMENSION(n), INTENT(OUT) :: ichext
       INTEGER, INTENT(IN) :: n
     END SUBROUTINE inexc4
  END INTERFACE
! primary function: encode plain 4-character strings into externally formatted
!                   integer versions of 4-character strings (used to convert
!                   data from internally formatted files to data for
!                   externally formatted files)
!
! *** Notice - This file contains ANSYS Confidential information ***
!
!  input arguments:
!     chin      (char,ar(n),in)  - strings in character form
!     n         (int,sc,in)      - number of strings to convert
!
!  output arguments:
!     ichext    (int,ar(n),out)  - externally formatted integer form of
!                                  4-character strings

! 2.1.15. Subroutine binclo (Closing or Deleting a Blocked Binary File)

!deck,binclo
  INTERFACE
     SUBROUTINE binclo (nblk,pstat,Buffer4)
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: nblk
       CHARACTER, INTENT(IN)  :: pstat
       INTEGER, INTENT(INOUT), DIMENSION(*) :: Buffer4
     END SUBROUTINE binclo
  END INTERFACE
! *** primary function: close a blocked file, every block/file opened with
!                       binset should be closed with binclo
! *** secondary function: the file can be deleted by specifying 'D' in pstat
! --- This routine is intended to be used in standalone programs.
! --- This routine should not be linked into the ANSYS program.

! *** Notice - This file contains ANSYS Confidential information ***

!  input arguments:
!      nblk     (int,sc,in)         - the block number to close
!                                     (as defined with subroutine binset)
!      pstat    (chr,sc,in)         - keep or delete flag
!                                     = 'K' - keep file
!                                     = 'D' - delete file
!      Buffer4  (int,ar(*),inout)   - work array for paging, should be the
!                                     same array as used in binset
!  output arguments:
!      Buffer4  (int,ar(*),inout)   - work array for paging

! 2.1.16. Subroutine largeIntGet (Converting Two Integers into a Pointer)

! 2.3. Results File Access Routines

! 2.3.2. ResRdBegin (Opening the File and Retrieving Global Information)
! 2.3.3. ResRdGeomBegin (Retrieving Global Geometry Information)
! 2.3.4. ResRdType (Retrieving Element Types)
! 2.3.5. ResRdReal (Retrieving Real Constants)
! 2.3.6. ResRdCsys (Retrieving Coordinate Systems)
! 2.3.7. ResRdNode (Retrieving Nodal Coordinates)
! 2.3.8. ResRdElem (Retrieving Elements)
! 2.3.9. ResRdSectMatBegin (Retrieving Global Section and Material Information)
! 2.3.10. ResRdSect (Retrieving Section Data)
! 2.3.11. ResRdMat (Retrieving Material Data)
! 2.3.12. ResRdSolBegin (Retrieving Result Set Location)
! 2.3.13. ResRdDisp (Retrieving Nodal Solution)
! 2.3.14. ResRdRfor (Retrieving Reaction Solution)
! 2.3.15. ResRdFix (Retrieving Applied Nodal Constraints)
! 2.3.16. ResRdForc (Retrieving Applied Nodal Loads Solution)
! 2.3.17. ResRdEstr (Retrieving Element Solutions)

END MODULE ansys_interf

! Local Variables:
! mode:f90
! mode:flyspell
! ispell-local-dictionary:"en"
! compile-command:"make -C .."
! End:
