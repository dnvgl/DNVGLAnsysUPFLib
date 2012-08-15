! Copyright (C) 2007 by Germanischer Lloyd AG

! ======================================================================
! Task      interface definitions for ANSYS routines
! ----------------------------------------------------------------------
! Author    Berthold Höllmann <berthold.hoellmann@gl-group.com>
! Project   ans2bmf
! ======================================================================


! CVSID: $Id$

#include "computer.h"

MODULE ansys_upf

  ! **************************************************************
  ! Functions for accessing data on the command line
  ! integer function  intinfun(iField) - gets an integer from field iField
  INTERFACE
     FUNCTION intinfun(iField)
       IMPLICIT NONE
       INTEGER :: intinfun
       INTEGER, INTENT(IN) :: iField
     END FUNCTION intinfun
  END INTERFACE
  ! double precision function dpinfun(iField) - gets double precision
  INTERFACE
     FUNCTION dpinfun(iField)
       IMPLICIT NONE
       DOUBLE PRECISION :: dpinfun
       INTEGER, INTENT(IN) :: iField
     END FUNCTION dpinfun
  END INTERFACE
  ! character*4 ch4infun(iField) - gets (upper case) 4 characters
  INTERFACE
     FUNCTION ch4infun(iField)
       IMPLICIT NONE
       CHARACTER(LEN=4) :: ch4infun
       INTEGER, INTENT(IN) :: iField
     END FUNCTION ch4infun
  END INTERFACE
  ! character*8 ch8infun(iField) - gets (mixed case) 8 characters
  INTERFACE
     FUNCTION ch8infun(iField)
       IMPLICIT NONE
       CHARACTER(LEN=8) :: ch8infun
       INTEGER, INTENT(IN) :: iField
     END FUNCTION ch8infun
  END INTERFACE
  ! character*32  ch32infun(iField) - gets (mixed case) 32 characters
  INTERFACE
     FUNCTION ch32infun(iField)
       IMPLICIT NONE
       CHARACTER(LEN=32) :: ch32infun
       INTEGER, INTENT(IN) :: iField
     END FUNCTION ch32infun
  END INTERFACE
  ! **************************************************************


  ! 6.2. Supporting Subroutines for Element Creation
  ! 6.2.1. Subroutine nminfo (Returning Element Reference Names)
  ! 6.2.2. Subroutine svgidx (Fetching the Index for Saved Variables)
  ! 6.2.3. Subroutine svrget (Fetching Saved Variable Data for an Element)
  ! 6.2.4. Subroutine svrput (Writing an Element's Saved Variable Set)
  ! 6.2.5. Subroutine svpidx (Writing the Saved Variable Element Index to a File)
  ! 6.2.6. Subroutine mreuse (Determining Which Element Matrices Can Be Reused)
  ! 6.2.7. Subroutine subrd (Reading Element Load Data for a Substructure Generation Run)
  ! 6.2.8. Subroutine subwrt (Writing an Element Load Vector to a File for a Substructure Generation Run)
  ! 6.2.9. Subroutine rvrget (Fetching Real Constants for an Element)
  ! 6.2.10. Subroutine propev (Evaluating a Group of Material Properties)
  ! 6.2.11. Subroutine prope1 (Evaluating One Material Property)
  ! 6.2.12. Subroutine pstev1 (Evaluating Material Properties for 1-D Elements)

  ! 6.2.13. Subroutine tbuser (Retrieving User Table Data)
  ! *** primary function:    return the tb data for the user table
  ! *** Notice - This file contains ANSYS Confidential information ***
  !  input arguments:
  !     mat      (int,sc,in)         - material property number
  !     numitm   (int,sc,in)         - the number of data items requested
  !  output arguments:
  !     tbprop   (dp,ar(numitm),out) - array of tb data
  INTERFACE
     SUBROUTINE tbuser(mat, numitm, tbprop)
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: mat
       INTEGER, INTENT(IN) :: numitm
       DOUBLE PRECISION, INTENT(OUT), DIMENSION(numitm) :: tbprop
     END SUBROUTINE tbuser
  END INTERFACE

  ! 6.2.14. Subroutine plast1 (Updating an Element's Plastic History)
  ! 6.2.15. Subroutine plast3 (Updating an Element's Plastic History, 4 or 6 components)
  ! 6.2.16. Subroutine creep1 (Updating an Element's Creep History)
  ! 6.2.17. Subroutine creep3 (Updating an Element's Creep History, 3-D Elements)
  ! 6.2.18. Subroutine swell1 (Updating an Element's Swelling History)
  ! 6.2.19. Subroutine swell3 (Updating an Element's Swelling History, 3-D Elements)
  ! 6.2.20. Function elLenPsvrBuf (Determining additional ESAV Record for Plasticity)

  ! 6.2.21. Function nlget (Retrieving Material Nonlinear Property Information)
  ! *** primary function:    get a material non-linear property (TB) table.

  ! *** Notice - This file contains ANSYS Confidential information ***
  !     input arguments:
  !        variable (typ,siz,intent)    description
  !        mat      (int,sc,in)       - material number
  !        iprop    (int,sc,in)       - property number (tbpnum in tblecm)
  !                                        use 13 for tb,user
  !                                        use 14 for tb,nl
  !     output arguments:
  !        variable (typ,siz,intent)    description
  !        nlget   (int,sc,out)       - number of property values
  !        prop    (dp,ar(nlget),out) - vector of the property values
  !                                      (the first 15(tbhdsz) items are a header,
  !                                      given below.  The terms are defined in
  !                                      tblecm.inc)
  !        --- terms of the descriptor record:
  !        header(1) = tbtyp
  !        header(2) = tbtems
  !        header(3) = temloc
  !        header(4) = dprtem
  !        header(5) = tbrow
  !        header(6) = tbcol
  !        header(7) = rowkey
  !        header(8) = nxtloc
  !        header(9) = nxttem
  !        header(10) = temptr
  !        header(11) = tbpt
  !        header(12) = tbsiz
  !        header(13) = tbopt
  !        header(14) = hypopt
  !        header(15) = tbnpts
  INTERFACE
     FUNCTION nlget(mat, iprop, prop)
       IMPLICIT NONE
       INTEGER :: nlget
       INTEGER, INTENT(IN) :: mat
       INTEGER, INTENT(IN) :: iprop
       DOUBLE PRECISION, INTENT(OUT), DIMENSION(*) :: prop
     END FUNCTION nlget
  END INTERFACE

  ! 6.2.22. Subroutine usereo (Storing Data in the nmisc Record)
  ! 6.2.23. Subroutine eldwrtL (Writing Element Data to a File)
  ! 6.2.24. Subroutine eldwrnL (Writing Element Nonsummable Miscellaneous Data to the Results File)
  ! 6.2.25. Subroutine trrot (Computing the Rotation Vector)
  ! 6.2.26. Subroutine rottr (Computing the Transformation Matrix)
  ! 6.2.27. Subroutine xyzup3 (Updating an Element's 3-D Nodal Coordinates)
  ! 6.2.28. Subroutine updrot (Updating the Rotation Pseudovector)
  ! 6.2.29. Subroutine tmpget (Defining Current Temperature Loads)
  ! 6.2.30. Subroutine prsget (Defining Current Pressure Loads)
  ! 6.2.31. Subroutine cnvget (Defining Current Convection Loads)
  ! 6.2.32. Subroutine hgnget (Defining Current Heat Generation Loads)
  ! 6.2.33. Subroutine prinst (Computing principal stress and stress intensity)
  ! 6.3. Routines for Modifying and Monitoring Existing Elements
  ! 6.3.1. Subroutine userfd (Computing the Complex Load Vector for Frequency Domain Logic)
  ! 6.3.2. Subroutine userou (Storing User-Supplied Element Output)
  ! 6.3.3. Subroutine useran (Modifying Orientation of Material Properties)
  ! 6.3.4. Subroutine usanly (Modifying Orientation of Material Properties and Stresses of Layers)
  ! 6.3.5. Subroutine userrc (Performing User Operations on COMBIN7 and COMBIN37 Parameters)
  ! 6.3.6. Function userpe (Calculating Rotation Caused by Internal Pressure)
  ! 6.3.7. Subroutine UElMatx (Accessing Element Matrices and Load Vectors)
  ! 6.3.8. Subroutine UTHICK (Getting User-defined Initial Thickness)
  ! 6.3.9. Subroutine USTRESS (Getting User-defined Initial Stress)
  ! 6.3.10. Subroutine UsrFictive (Providing User-defined Fictive Temperature Relationship)
  ! 6.3.11. Subroutine UsrViscEl (Performs Viscoelastic Computation)
  ! 6.3.12. Subroutine usrsurf116 (Modifying SURF151 and SURF152 Film Coefficients and Bulk Temperatures)
  ! 6.3.13. Subroutine User116Cond (Computes the conductance coefficient for FLUID116)
  ! 6.3.14. Subroutine User116Hf (Computes the film coefficient for FLUID116)
  ! 6.3.15. Subroutine Us_Surf_Str (Captures surface stresses)
  ! 6.3.16. Subroutine usflex (Computes the flexibility factor for PIPE16, PIPE17, PIPE18, and PIPE60)
  ! 6.3.17. Subroutine UsrShift (Calculates pseudotime time increment)
  ! 6.4. Routines for Customizing Material Behavior
  ! 6.4.1. Subroutine usermat (Writing Your Own Material Models)
  ! 6.4.2. Subroutine userpl (Writing Your Own Plasticity Laws)
  ! 6.4.3. Subroutines usercreep and usercr (Defining Viscoplastic/Creep Material Behavior)
  ! 6.4.3.1. Creep Subroutine usercreep
  ! 6.4.3.2. Creep Subroutine usercr
  ! 6.4.4. Subroutine usersw (Writing Your Own Swelling Laws)
  ! 6.4.5. Subroutine UserHyper (Writing Your Own Hyperelasticity Laws)
  ! 6.4.6. Subroutine uservp (Updating Nonlinear Strain History for Materials)
  ! 6.4.7. Subroutine userck (Checking User-Defined Material Data)
  ! 6.4.8. Subroutine usermc (Controlling Hygrothermal Growth)
  ! 6.4.9. Subroutine usrfc6 (Defining Custom Failure Criteria)
  ! 6.4.10. Subroutines usrfc1 through usrfc5
  ! 6.4.11. Subroutine UserVisLaw (Defining Viscosity Laws)
  ! 6.4.12. Supporting Function egen
  ! 6.5. Routines for Customizing Loads
  ! 6.5.1. Subroutine usrefl (Changing Scalar Fields to User-Defined Values)
  ! 6.5.2. Subroutine userpr (Changing Element Pressure Information)
  ! 6.5.3. Subroutine usercv (Changing Element Face Convection Surface Information)
  ! 6.5.4. Subroutine userfx (Changing Element Face Heat Flux Surface Information)
  ! 6.5.5. Subroutine userch (Changing Element Face Charge Density Surface Information)
  ! 6.6. Running ANSYS as a Subroutine
  ! 6.7. Defining Your Own Commands
  ! 6.7.1. Function user01
  ! 6.7.2. Function user02 (Demonstrates Offsetting Selected Nodes)
  ! 6.7.3. Function user03 (Demonstrates Using ANSYS Memory)
  ! 6.7.4. Function user04
  ! 6.7.5. Functions user05 through user10
  ! 6.8. Supporting Subroutines
  ! 6.8.1. Function GetRForce (Getting Nodal Reaction Force values)
  ! 6.8.2. Function GetStackDisp (Getting Current Displacement Values)
  ! 6.8.3. Subroutine ElResultStrt (Getting Load Data from Analysis Results)
  ! 6.8.4. Subroutine ElResultGet (Getting Results Values at Selected Points)
  ! 6.8.5. Subroutine ElInterp (Finding Element Coordinates)
  ! 6.9. Access at the Beginning and End of Various Operations
  ! 6.10. Creating Your Own Optimization Routine
  ! 6.10.1. Linking a Custom Optimization Program to ANSYS
  ! 6.10.2. Subroutine userop (Defining a Custom Optimization Routine)
  ! 6.10.3. Structuring Your Input
  ! 6.11. Memory Management Routines
  ! 6.11.1. Using the Memory Management Routines

  ! 6.11.2. Function fAnsMemAlloc (Allocating Space and Returning a Pointer)
  !     fAnsMemAlloc (Allocating Space and Returning a Pointer)
  !     function fAnsMemAlloc (iLen, key, c16Label)
  !     primary function:
  !        Get A Block of Space from mem manager and Return Pointer
  ! keywords:  integer function for mem allocate
  ! object/library:  mem
  ! *** Notice - This file contains ANSYS Confidential information ***
  !  input arguments:
  !     iLen (int,sc,in)           - length of the block (in data elements)
  !     c16Label (chr*16,sc,in)    - 16 character name for the Block
  !     key (int,sc,in)            - type of data for this block (see ansysdef)
  !
  !  output arguments:
  !     fAnsMemAlloc (PTRFTN,sc,out)  - Pointer to this data block -- needs to be
  !                                     tied to a local variable in the calling
  !                                     routine
  INTERFACE
     FUNCTION fAnsMemAlloc(iLen, key, c16Label)
       USE ansys_par
       IMPLICIT NONE
       PTRFTN :: fAnsMemAlloc
       INTEGER, INTENT(IN) :: iLen
       INTEGER, INTENT(IN) :: key
       CHARACTER(LEN=16), INTENT(IN) :: c16Label
     END FUNCTION fAnsMemAlloc
  END INTERFACE

  ! 6.11.3. Subroutine fAnsMemFree (Deallocating Space)
  !     fAnsMemFree (Deallocating Space)
  !     subroutine fAnsMemFree (memPtr)
  !     primary function:   Free a Data Block, given a pointe
  !
  !  input arguments:
  !     ptr   (PTRFTN,sc,inout)  - pointer for this block
  !  output arguments:
  !     ptr   (PTRFTN,sc,inout)  - pointer will be set to zero
  INTERFACE
     SUBROUTINE fAnsMemFree(ptr)
       USE ansys_par
       IMPLICIT NONE
       PTRFTN, INTENT(INOUT) :: ptr
     END SUBROUTINE fAnsMemFree
  END INTERFACE

  ! 6.12. Parameter Processing Routines
  ! 6.12.1. Subroutine pardim (Creating a Dimensioned Parameter)
  ! 6.12.2. Function parevl (Finding and Evaluating a Parameter)

  ! *** primary function:    find and evaluate a parameter
  !
  ! *** Notice - This file contains ANSYS Confidential information ***
  !
  !  input arguments:
  !     ParName  (chr*(PARMSIZE),sc,in) - the name of the parameter
  !                                       (must be upper case, left justified)
  !     nDim     (int,sc,in)            - the number of subscripts (0,scaler)
  !     subc     (dp,ar(*),in)          - values for the subscripts (if any)
  !     lvl      (int,sc,in)            - 0,1  no error output  2, report error
  !                                       -1, set kerr flag with no anserr call
  !
  !  output arguments:
  !     dpValue (dp,sc,out)           - the value of the parameter (may be a
  !                                    packed character*8
  !     chValue (ch*STRING_MAX_LENG,sc,out)       - character output
  !     kerr    (int,sc,out)          - error flag  (0,ok  -1,output is packed
  !                                     0=ok,  1=error,  2=error but TINY is used
  !                                      -2, output is string in chValue
  INTERFACE
#if   ANSVER < 70
     FUNCTION parevl(ParName, nDim, subc, lvl, kerr)
#else
     SUBROUTINE parevl(ParName, nDim, subc, lvl, dpValue, chValue, &
          kerr)
#endif
       USE ansys_par, ONLY : STRING_MAX_LENG, PARMSIZE
       IMPLICIT NONE
#if ANSVER < 70
       DOUBLE PRECISION :: parevl
#endif
       CHARACTER(LEN=PARMSIZE), INTENT(IN) :: ParName
       INTEGER, INTENT(IN) :: nDim
       DOUBLE PRECISION, DIMENSION(*), INTENT(IN) :: subc
       INTEGER, INTENT(IN) :: lvl
#if ANSVER < 70
     END FUNCTION parevl
#else
       DOUBLE PRECISION, INTENT(OUT) :: dpValue
       CHARACTER(LEN=STRING_MAX_LENG), INTENT(OUT) :: chValue
       INTEGER, INTENT(OUT) :: kerr
     END SUBROUTINE parevl
#endif
  END INTERFACE

  ! 6.12.3. Subroutine pardef (Adding a Parameter)
  ! 6.13. Miscellaneous Useful Functions

  ! 6.13.1. Using Function RunCommand
  !     RunCommand
  !     This function enables you to execute an ANSYS command from within
  !     a user routine. Inputs and outputs for RunCommand are as follows:

  !     function RunCommand (nChar,command)
  !     primary function:
  !       Execute an ansys command
  !
  !     input arguments:
  !       nChar    (int,sc,in)           - Length of the command string (8 min)
  !       command  (ch*(nChar),sc,in)    - A character string containing a
  !                                        valid ANSYS command
  !     output arguments:
  !       RunCommand   (int,sc,out)      - An internally defined value, ignore
  INTERFACE
     FUNCTION RunCommand(nChar, command)
       IMPLICIT NONE
       INTEGER :: RunCommand
       INTEGER, intent(in) :: nChar
       CHARACTER(LEN=nChar), intent(in) :: command
     END FUNCTION RunCommand
  END INTERFACE

  ! 6.13.2. Using the /UNDO Command
  ! 6.13.3. Using the /HOLD command
  ! 7. Accessing the ANSYS Database
  ! 7.1. Inputs and Outputs for Database Access Routines
  ! 7.2. Types of Database Access Routines
  ! 7.3. Routines for Selecting and Retrieving Nodes and Elements

  ! 7.3.1. ndnext Function (Getting the Next Node Number)
  ! *** primary function:    get the number of the next selected node
  ! *** Notice - This file contains ANSYS Confidential information ***
  !     input arguments:
  !        next     (int,sc,in)       - the last node number used
  !                                     = 0 - use for initial value
  !     output arguments:
  !        ndnext   (int,func,out)    - the next selected node number
  !                                     = 0 - no more nodes
  INTERFACE
     FUNCTION ndnext(next)
       IMPLICIT NONE
       INTEGER :: ndnext
       INTEGER, INTENT(IN) :: next
     END FUNCTION ndnext
  END INTERFACE

  ! 7.3.2. ndprev Function (Getting the Number of the Previous Selected Node)
  ! 7.3.3. ndnxdf Function (Getting the Number of the Next Defined Node)
  ! 7.3.4. ndsel Function (Selecting, Unselecting, Deleting, or Inverting a Node)

  ! 7.3.5. elnext Function (Getting the Number of the Next Element)
  ! primary function:    get the number of the next selected element
  !     input arguments:
  !        next     (int,sc,in)       - the last element number used
  !                                     = 0 - use for initial value
  !     output arguments:
  !        elnext   (int,func,out)    - the next selected element
  !                                     = 0 - no more elements
  INTERFACE
     FUNCTION elnext(next)
       IMPLICIT NONE
       INTEGER :: elnext
       INTEGER, INTENT(IN) :: next
     END FUNCTION elnext
  END INTERFACE

  INTERFACE
     FUNCTION etnext(next)
       IMPLICIT NONE
       INTEGER :: etnext
       INTEGER, INTENT(IN) :: next
     END FUNCTION etnext
  END INTERFACE

  INTERFACE
     FUNCTION rlnext (rlId)
       INTEGER :: rlnext
       INTEGER, INTENT(IN) :: rlId
     END FUNCTION rlnext
  END INTERFACE

  ! 7.3.6. elprev Function (Getting the Number of the Previous Selected Element)
  ! 7.3.7. elnxdf Function (Getting the Number of the Next Defined Element)
  ! 7.3.8. elsel Subroutine (Selecting, Unselecting, Deleting, or Inverting an Element)
  ! 7.4. Node Information Routines

  ! 7.4.1. ndinqr Function (Getting Information About a Node)
  ! The primary function of ndinqr is getting information about a
  ! node. You can also use this function to set the current node pointer
  ! to this node.
  !     input arguments:
  !        node     (int,sc,in)       - node number
  !                                      Should be 0 for key=11, DB_NUMDEFINED,
  !                                      DB_NUMSELECTED, DB_MAXDEFINED, and
  !                                      DB_MAXRECLENG
  !        key      (dp,sc,in)        - key as to information needed about
  !                                     the node.
  !                 = DB_SELECTED    - return select status:
  !                     ndinqr  = 0 - node is undefined.
  !                             =-1 - node is unselected.
  !                             = 1 - node is selected.
  !                 = DB_NUMDEFINED  - return number of defined nodes
  !                 = DB_NUMSELECTED - return number of selected nodes
  !                 = DB_MAXDEFINED  - return highest node number defined
  !                 = DB_MAXRECLENG  - return maximum record length (dp words)
  !                 =  2, return length (dp words)
  !                 =  3,
  !                 =  4, pointer to first data word
  !                 = 11, return void percent (integer)
  !                 = 17, pointer to start of index
  !                 = -1,
  !                 = -2, superelement flag
  !                 = -3, master dof bit pattern
  !                 = -4, active dof bit pattern
  !                 = -5, solid model attachment
  !                 = -6, pack nodal line parametric value
  !                 = -7, constraint bit pattern
  !                 = -8, force bit pattern
  !                 = -9, body force bit pattern
  !                 = -10, internal node flag
  !                 = -11, orientation node flag =1 is =0 isnot
  !                 = -11, contact node flag <0
  !                 = -12, constraint bit pattern (for DSYM)
  !                 = -13, if dof constraint written to file.k (for LSDYNA only)
  !                 = -14, nodal coordinate system number (set by NROTATE)
  !                 =-101, pointer to node data record
  !                 =-102, pointer to angle record
  !                 =-103,
  !                 =-104, pointer to attached couplings
  !                 =-105, pointer to attacted constraint equations
  !                 =-106, pointer to nodal stresses
  !                 =-107, pointer to specified disp'S
  !                 =-108, pointer to specified forces
  !                 =-109, pointer to x/y/z record
  !                 =-110,
  !                 =-111,
  !                 =-112, pointer to nodal temperatures
  !                 =-113, pointer to nodal heat generations
  !                 =-114,
  !                 =-115, pointer to calculated displacements
  !                 =-116,
  !     output arguments:
  !        ndinqr   (int,func,out)   - the returned value of ndinqr is based on
  !                                        setting of key.
  INTERFACE
     FUNCTION ndinqr (node, key)
       IMPLICIT NONE
       INTEGER :: ndinqr
       INTEGER, INTENT(IN) :: node
       INTEGER, INTENT(IN) :: key
     END FUNCTION ndinqr
  END INTERFACE

  ! 7.4.2. getnod Function (Getting a Nodal Point)
  ! *** primary function:    get a nodal point

  ! *** Notice - This file contains ANSYS Confidential information ***
  !  input arguments:
  !     node     (int,sc,in)       - node number
  !     kerr     (int,sc,inout)    - message flag
  !                                  = 0 - print no message if node is unselected
  !                                         or undefined
  !                                  = 1 - print message if node is undefined
  !                                  = 2 - print message if node is undefined
  !                                         or unselected
  !     kcrot    (int,sc,in)       - output coordinates in this coordinate system.
  !                                    if kcrot is negative, output theta and
  !                                    phi coordinates in radians

  !  output arguments:
  !     v        (dp,ar(6),out)    - Coordinates (first 3 values) and rotation
  !                                    angles (last 3 values)
  !     kerr     (int,sc,inout)    - select status
  !                                  = 0 - node is selected
  !                                  = 1 - node is not defined
  !                                  =-1 - node is unselected
  INTERFACE
     SUBROUTINE getnod(node, v, kerr, kcrot)
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: node
       DOUBLE PRECISION, INTENT(OUT), DIMENSION(6) :: v
       INTEGER, INTENT(INOUT) :: kerr
       INTEGER, INTENT(IN) :: kcrot
     END SUBROUTINE getnod
  END INTERFACE

  ! 7.4.3. putnod Function (Storing a Node)
  ! *** primary function:    store a node
  ! *** secondary functions: display node if in immediate mode.

  ! *** Notice - This file contains ANSYS Confidential information ***
  !     input arguments:
  !        node     (int,sc,in)       - node number
  !        vctn     (dp,ar(6),in)     - array of 3 nodal coordinates and
  !                                              3 nodal rotation angles.
  !        kcrot    (int,sc,in)       - local coordinate system in which the nodal
  !                                      coordinates and angles are defined
  !     output arguments:  none.
  INTERFACE
     SUBROUTINE putnod(node, vctn, kcrot)
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: node
       DOUBLE PRECISION, INTENT(IN), DIMENSION(6) :: vctn
       INTEGER, INTENT(IN) :: kcrot
     END SUBROUTINE putnod
  END INTERFACE

  ! 7.4.4. ndgall Function (Getting the XYZ/Rotation Coordinates Vector for a Node)
  ! primary function:    get x,y,z,rotx,roty,rotz vector for a node.
  !     input arguments:
  !        node     (int,sc,in)       - node number for operation.

  !     output arguments:
  !        ndgall   (int,sc,out)      - status of node.
  !                                       0=node is undefined.
  !                                      -1=node is unselected.
  !                                       1=node is selected.
  !        xyz      (dp,ar(6),out)    - vector containing x,y,z,rotx,roty,rotz

  INTERFACE
     FUNCTION ndgall (node,xyz)
       IMPLICIT NONE
       INTEGER :: ndgall
       INTEGER, INTENT(IN) :: node
       DOUBLE PRECISION, INTENT(OUT), DIMENSION(6) :: xyz
     END FUNCTION ndgall
  END INTERFACE

  ! 7.4.5. ndspgt Subroutine (Getting the Nodal Solution for a Node of an Element)
  ! 7.5. Element Attribute Routines

  ! 7.5.1. elmiqr Function (Getting Information About an Element)
  ! primary function:    get information about an element.
  ! secondary functions: set current element pointer to this element.
  !     input arguments:
  !        ielem    (int,sc,in)       - element number
  !                                     should be zero for key=11, DB_NUMDEFINED,
  !                                       DB_NUMSELECTED, or DB_MAXDEFINED
  !        key      (int,sc,in)       - information flag.
  !                 = DB_SELECTED    - return select status:                (1)
  !                      elmiqr = 0 - element is undefined.
  !                              =-1 - element is unselected.
  !                              = 1 - element is selected.
  !                 = DB_NUMDEFINED  - return number of defined elements    (12)
  !                 = DB_NUMSELECTED - return number of selected elements   (13)
  !                 = DB_MAXDEFINED  - return maximum element number used   (14)
  !                 = DB_MAXRECLENG  - return maximum record length         (15)
  !                                     (int words)
  !                 = 2 - return length (int words)
  !                 = 3 - return layer number
  !                       (for cross reference files return number of entities)
  !                 = 4 - return address of first data word
  !                 = 5 - return length (in record type units)
  !                 = 6 - return compressed record number.
  !                 = 11 - return void percent (integer)
  !                 = 16 - return location of next record
  !                        (this increments the next record count)
  !                 = 17 - pointer to start of index
  !                 = 18 - return type of file.
  !                     elmiqr = 0 - integer
  !                             = 1 - double precision
  !                             = 2 - real
  !                             = 3 - complex
  !                             = 4 - character*8
  !                             = 7 - index
  !                 = 19 - return virtual type of file.
  !                     elmiqr = 0 - fixed length (4.4 form)
  !                             = 1 - indexed variable length (layer data)
  !                             = 2 - xref data tables
  !                             = 3 - bitmap data (for 32 data item packed records)
  !                             = 4 - data tables (three dimensional arrays)
  !                 = -1  - material number etc. (see elmcmx)
  !                 =-101 - pointer to element integers etc.
  !                            (see elmcmx with elmilg and 1 instead of -101)
  !
  !     output arguments:
  !        elmiqr  (int,sc,out)  - the returned value of elmiqr is based on
  !                                      setting of key.
  !
  INTERFACE
     FUNCTION elmiqr (ielem,key)
       IMPLICIT NONE
       INTEGER :: elmiqr
       INTEGER, INTENT(IN) :: ielem
       INTEGER, INTENT(IN) :: key
     END FUNCTION elmiqr
  END INTERFACE

  ! 7.5.2. elmget Function (Getting an Element's Attributes and Nodes)
  ! *** primary function:    get element attributes and nodes.

  ! *** Notice - This file contains ANSYS Confidential information ***
  !     input arguments:
  !        ielem    (int,sc,in)       - element number
  !     output arguments:
  !        elmget   (int,func,out)    - status of element.
  !                                     = 0 - element undefined
  !                                     < 0 - number of nodes on unselected
  !                                            element
  !                                     > 0 - number of nodes on selected element
  !        elmdat   (int,ar(EL_DIM),in) - element attributes.
  !                             elmdat(EL_MAT)  - material number
  !                                   (EL_TYPE)  - element type
  !                                   (EL_REAL)  - real constant number
  !                                   (EL_SECT)  - section number
  !                                   (EL_CSYS)  - coordinate system number
  !                                   (EL_DEAD)  - death flag (bit 0)
  !                                           if clear - alive
  !                                           if set   - dead
  !                                   (EL_SOLID) - solid model reference
  !                                   (EL_SHAPE) - 100*shape + specific shape
  !                                   (EL_OBJOPTIONS)  - reserved
  !                                   (EL_PEXCLUDE) - p element include flag
  !                                           (bit 0)
  !                                           if clear - include
  !                          (element may need to have its p-level increased)
  !                                           if set   - exclude
  !                          (element does not need to have its p-level increased)
  !                 EL_PEXCLUDE is also used for the LSDYNA part number (trh 9/05)
  !        nodes    (int,ar(*),out)   - node numbers for element.
  INTERFACE
     FUNCTION elmget(ielem, elmdat, nodes)
       USE ansys_par, ONLY : EL_DIM, NNMAX
       IMPLICIT NONE
       INTEGER :: elmget
       INTEGER, INTENT(IN) :: ielem
       INTEGER, INTENT(OUT), DIMENSION(EL_DIM) :: elmdat
       INTEGER, INTENT(OUT), DIMENSION(NNMAX) :: nodes
     END FUNCTION elmget
  END INTERFACE

  ! 7.5.3. elmput Subroutine (Storing an Element)
  ! primary function:    store element attributes and node numbers.
  ! secondary functions: set current element pointer to this element.
  ! *** NOTICE - The user is also responsible for defining the centroid for the
  !              element using the elmpct subroutine.  Calling the elmput
  !              subroutine will NULL the element centroid previously defined.
  !     input arguments:
  !        ielem    (int,sc,in)       - element number
  !        elmdat   (int,ar(EL_DIM),in) - element attributes.
  !                             elmdat(EL_MAT)  - material number
  !                                   (EL_TYPE)  - element type
  !                                   (EL_REAL)  - real constant number
  !                                   (EL_SECT)  - section number
  !                                   (EL_CSYS)  - coordinate system number
  !                                   (EL_DEAD)  - death flag (bit 0)
  !                                           if clear - alive
  !                                           if set   - dead
  !                                   (EL_SOLID) - solid model reference
  !                                   (EL_SHAPE) - 100*shape + specific shape
  !                                   (EL_OBJOPTIONS)  - reserved
  !                                   (EL_PEXCLUDE) - p element include flag
  !                                           (bit 0)
  !                                           if clear - include
  !                                           if set   - exclude
  !                                           For LSDYNA, it means part ID
  !                                           in reqular ANSYS, it is never part ID
  !        nnod     (int,sc,in)       - number of nodes for this element.
  !        nodes    (int,ar(*),in)    - node numbers for this element.

  !     output arguments:  none.
  INTERFACE
     SUBROUTINE elmput (ielem, elmdat, nnod, nodes)
       USE ansys_par, ONLY : EL_DIM
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: ielem
       INTEGER, INTENT(IN), DIMENSION(EL_DIM) :: elmdat
       INTEGER, INTENT(IN) :: nnod
       INTEGER, INTENT(IN), DIMENSION(nnod) :: nodes
     END SUBROUTINE elmput
  END INTERFACE

  ! 7.5.4. etyiqr Function (Getting a Data Item About an Element Type)
  ! *** primary function:    get information about an element type.
  !     input arguments:
  !        itype    (int,sc,in)       - element type number
  !                                      Should be 0 for key=11, DB_NUMDEFINED,
  !                                      DB_NUMSELECTED, DB_MAXDEFINED, and
  !                                      DB_MAXRECLENG
  !        key      (int,sc,in)       - information flag.
  !                 = DB_SELECTED    - return select status:
  !                      etyiqr = 0 - element type is undefined.
  !                             =-1 - element type is unselected.
  !                             = 1 - element type is selected.
  !                 = DB_NUMDEFINED  - return number of defined element types
  !                 = DB_NUMSELECTED - return number of selected element types
  !                 = DB_MAXDEFINED  - return highest element type number defined
  !                 = DB_MAXRECLENG  - return maximum record length (int words)
  !                = -n, return element characteristic n from etycom for element
  !                       type itype.
  !                      n is correlated to the parameter names in echprm.
  !                      see elccmt for definitions of element characteristics.
  !                      note- this will not overwrite the current setting of
  !                       etycom.
  !     output arguments:
  !        etyiqr   (int,func,out)    - the returned value of etyiqr is based on
  !                                      setting of key.
  INTERFACE
     FUNCTION etyiqr(itype, key)
       IMPLICIT NONE
       INTEGER :: etyiqr
       INTEGER, INTENT(IN) :: itype
       INTEGER, INTENT(IN) :: key
     END FUNCTION etyiqr
  END INTERFACE

  ! 7.5.5. etyget Function (Getting Information About an Element Type)
  ! primary function:    get element type data.
  !     input arguments:
  !        itype    (int,sc,in)       - element type number
  !     output arguments:
  !        etyget   (int,func,out)    - status of element type.
  !                                     = 0 - element type is undefined.
  !                                     < 0 - number of data items on unselected
  !                                            element type.
  !                                     > 0 - number of data items on selected
  !                                            element type.
  !        ielx     (int,ar(*),out)   - element type data. see elccmt for
  !                                     description of data.
  INTERFACE
     FUNCTION etyget (itype,ielx)
       USE ansys_par, ONLY : IELCSZ
       IMPLICIT NONE
       INTEGER :: etyget
       INTEGER, INTENT(IN) :: itype
       INTEGER, INTENT(OUT), DIMENSION(IELCSZ) :: ielx
     END FUNCTION etyget
  END INTERFACE

  ! 7.5.6. etyput Subroutine (Storing Element Type Data)
  ! 7.5.7. echrtr Subroutine (Getting Information About Element Characteristics)
  ! 7.5.8. etysel Subroutine (Selecting, Unselecting, Deleting, or Inverting an Element Type)

  ! 7.5.9. mpinqr Function (Getting Information About a Material Property)
  ! primary function:    get information about a material property.
  !     input arguments:
  !        mat      (int,sc,in)       - material number
  !                                      should be 0 for key=11,
  !                                      DB_NUMDEFINED(12),
  !                                      DB_MAXDEFINED(14), and
  !                                      DB_MAXRECLENG(15)
  !        iprop    (int,sc,in)       - property reference number:
  !         if iprop = 0, test for existence of any material property with this
  !                  material number (with key = DB_SELECTED(1))
  !        ---- MP command labels --------
  !        EX  = 1, EY  = 2, EZ  = 3, NUXY= 4, NUYZ= 5, NUXZ= 6, GXY = 7, GYZ = 8
  !        GXZ = 9, ALPX=10, ALPY=11, ALPZ=12, DENS=13, MU  =14, DAMP=15, KXX =16
  !        KYY =17, KZZ =18, RSVX=19, RSVY=20, RSVZ=21, C   =22, HF  =23, VISC=24
  !        EMIS=25, ENTH=26, LSST=27, PRXY=28, PRYZ=29, PRXZ=30, MURX=31, MURY=32
  !        MURZ=33, PERX=34, PERY=35, PERZ=36, MGXX=37, MGYY=38, MGZZ=39, EGXX=40
  !        EGYY=41, EGZZ=42, SBKX=43, SBKY=44, SBKZ=45, SONC=46, SLIM=47, ELIM=48
  !        USR1=49, USR2=50, USR3=51, USR4=51, FLUI=53, ORTH=54, CABL=55, RIGI=56
  !        HGLS=57, BM  =58, QRAT=59, REFT=60, CTEX=61, CTEY=62, CTEZ=63, THSX=64,
  !        THSY=65, THSZ=66, DMPR=67, LSSM=68,     =69,     =79,     =71,     =72,
  !            =73,     =74,     =75,     =76,     =77,     =78,     =79,     =80
  !               (see mpinit for uncommented code and for TB command information)
  !
  !        key      (int,sc,in)       - key as to the information needed
  !                                     about material property.
  !             = DB_SELECTED(1)- return select status:
  !                        mpinqr = 0 - material prop is undefined.
  !                               = 1 - material prop is selected.
  !             = DB_NUMDEFINED(12) - number of defined material properties
  !             = DB_MAXDEFINED(14) - highest material property number defined
  !             = DB_MAXRECLENG(15) - maximum record length (dp words)
  !             =  2 - return length (dp words)
  !             =  3 - return number of temp. values
  !             = 11 - return void percent (integer)
  !     output arguments:
  !        mpinqr   (int,func,out)    - returned value of mpinqr is based on
  !                                      setting of key.
  INTERFACE
     FUNCTION mpinqr(mat, iprop, key)
       IMPLICIT NONE
       INTEGER :: mpinqr
       INTEGER, INTENT(IN) :: mat
       INTEGER, INTENT(IN) :: iprop
       INTEGER, INTENT(IN) :: key
     END FUNCTION mpinqr
  END INTERFACE

  INTERFACE
     FUNCTION mpnext(next)
       IMPLICIT NONE
       INTEGER :: mpnext
       INTEGER, INTENT(IN) :: next
     END FUNCTION mpnext
  END INTERFACE

  ! 7.5.10. mpget Function (Getting a Material Property Table)
  ! primary function:    get a material property table.
  !     input arguments:
  !        variable (typ,siz,intent)    description
  !        mat      (int,sc,in)       - material number
  !        iprop    (int,sc,in)       - property reference number:
  !        ---- MP command labels --------
  !        EX  = 1, EY  = 2, EZ  = 3, NUXY= 4, NUYZ= 5, NUXZ= 6, GXY = 7, GYZ = 8
  !        GXZ = 9, ALPX=10, ALPY=11, ALPZ=12, DENS=13, MU  =14, DAMP=15, KXX =16
  !        KYY =17, KZZ =18, RSVX=19, RSVY=20, RSVZ=21, C   =22, HF  =23, VISC=24
  !        EMIS=25, ENTH=26, LSST=27, PRXY=28, PRYZ=29, PRXZ=30, MURX=31, MURY=32
  !        MURZ=33, PERX=34, PERY=35, PERZ=36, MGXX=37, MGYY=38, MGZZ=39, EGXX=40
  !        EGYY=41, EGZZ=42, SBKX=43, SBKY=44, SBKZ=45, SONC=46, SLIM=47, ELIM=48
  !        USR1=49, USR2=50, USR3=51, USR4=51, FLUI=53, ORTH=54, CABL=55, RIGI=56
  !        HGLS=57, BM  =58, QRAT=59, REFT=60, CTEX=61, CTEY=62, CTEZ=63, THSX=64,
  !        THSY=65, THSZ=66, DMPR=67, LSSM=68,     =69,     =79,     =71,     =72,
  !            =73,     =74,     =75,     =76,     =77,     =78,     =79,     =80
  !                  (see mpinit for uncommented code and TB command information)
  !     output arguments:
  !        mpget   (int,func,out)     - number of temperature values
  !        temp    (dp,ar(mpget),out) - vector of the temperature values
  !        prop    (dp,ar(mpget),out) - vector of the property values
  INTERFACE
     FUNCTION mpget (mat, iprop, temp, prop)
       INTEGER :: mpget
       INTEGER, INTENT(IN) :: mat
       INTEGER, INTENT(IN) :: iprop
       DOUBLE PRECISION, INTENT(OUT), DIMENSION(*) :: temp
       DOUBLE PRECISION, INTENT(OUT), DIMENSION(*) :: prop
     END FUNCTION mpget
  END INTERFACE

  ! 7.5.11. mpput Subroutine (Storing a Material Property Table)
  ! 7.5.12. mpdel Subroutine (Deleting a Material Property Table)

  ! 7.5.13. rlinqr Function (Getting Information About a Real Constant Set)
  ! primary function:    get information about a real constant set
  ! secondary functions: none
  !  input arguments:
  !        variable (typ,siz,intent)    description
  !        nreal   (int,sc,in)       - real constant table number
  !                                      should be 0 for key=11, DB_NUMDEFINED,
  !                                      DB_NUMSELECTED, DB_MAXDEFINED, and
  !                                      DB_MAXRECLENG
  !        key      (int,sc,in)       - information flag.
  !             = 5              - return number of values stored for nreal
  !             = DB_SELECTED    - return select status
  !                      rlinqr = 0 - real constant table is undefined.
  !                             =-1 - real constant table is unselected.
  !                             = 1 - real constant table is selected
  !             = DB_NUMDEFINED  - return number of defined real constant tables
  !             = DB_NUMSELECTED - return number of selected real constant tables
  !             = DB_MAXDEFINED  - return highest real constant table defined
  !             = DB_MAXRECLENG  - return maximum record length (dp words)
  !  output arguments:
  !        rlinqr   (int,func,out)    - the returned value of rlinqr is based on
  !                                      setting of key.
  !
  INTERFACE
     FUNCTION rlinqr (nreal,key)
       IMPLICIT NONE
       INTEGER :: rlinqr
       INTEGER, INTENT(IN) :: nreal
       INTEGER, INTENT(IN) :: key
     END FUNCTION rlinqr
  END INTERFACE

  ! 7.5.14. rlget Function (Getting Real Constant Data)
  ! primary function:    get real constant data
  !     input arguments:
  !        nreal    (int,sc,in)       - real constant table number
  !     output arguments:
  !        rlget    (int,func,out)    - number of real constant data obtained
  !        rtable   (dp,ar(*),out)    - real constant data obtained
  INTERFACE
     FUNCTION rlget (nreal,rtable)
       USE ansys_par, ONLY : RL_DIM
       IMPLICIT NONE
       INTEGER :: rlget
       INTEGER, INTENT(IN) :: nreal
       DOUBLE PRECISION, INTENT(OUT), DIMENSION(RL_DIM) :: rtable
     END FUNCTION rlget
  END INTERFACE

  INTERFACE
     SUBROUTINE rlput (i, nreal, rtable)
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: i
       INTEGER, INTENT(IN) :: nreal
       DOUBLE PRECISION, INTENT(IN), DIMENSION(nreal) :: rtable
     END SUBROUTINE rlput
  END INTERFACE

  ! 7.5.15. rlsel Subroutine (Selecting or Deleting a Real Constant Set)
  ! *** primary function:    select or delete a real constant set
  ! *** secondary functions: none
  !
  ! *** Notice - This file contains ANSYS Confidential information ***
  !
  !     typ=int,dp,log,chr,dcp   siz=sc,ar(n),func    intent=in,out,inout
  !
  !  input arguments:
  !     variable (typ,siz,intent)    description
  !     nreai    (int,sc,in)       - real constant table
  !                                  = 0 - all real constant tables
  !     ksel     (int,sc,in)       - type of operation to be performed.
  !                                  = 0 - delete real constant table.
  !                                  = 1 - select real constant table.
  !                                  =-1 - unselect real constant table.
  !                                  = 2 - invert real constant table.
  !
  !  output arguments:
  !     none c
  INTERFACE
     SUBROUTINE rlsel(nreai, ksel)
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: nreai
       INTEGER, INTENT(IN) :: ksel
     END SUBROUTINE rlsel
  END INTERFACE

  ! 7.5.16. csyiqr Function (Getting Information About a Coordinate System)
  ! *** primary function:    get information about a coordinate system

  ! *** Notice - This file contains ANSYS Confidential information ***
  !  input arguments:
  !     ncsy     (int,sc,in)       - coordinate system reference number
  !                                   should be zero for key= DB_NUMDEFINED
  !                                   or DB_MAXDEFINED
  !     key      (int,sc,in)       - information flag.
  !              = DB_SELECTED    - return status:
  !                            csyiqr = 0 - coordinate system is not defined
  !                                    -1 - coordinate system is not selected
  !                                     1 - coordinate system is selected
  !              = DB_NUMDEFINED  - number of defined coordinate systems
  !              = DB_MAXDEFINED  - maximum coordinate system reference
  !                                 number used.

  !  output arguments:
  !     csyiqr   (int,func,out)    - the returned value of csyiqr is based on
  !                                   setting of key.
  INTERFACE
     FUNCTION csyiqr (ncsy, key)
       IMPLICIT NONE
       INTEGER :: csyiqr
       INTEGER, INTENT(OUT) :: ncsy
       INTEGER, INTENT(OUT) :: key
     END FUNCTION csyiqr
  END INTERFACE

  ! 7.5.17. csyget Function (Getting a Coordinate System)
  ! *** primary function:    get a coordinate system
  ! *** secondary functions: none

  ! *** Notice - This file contains ANSYS Confidential information ***
  !  NOTE:  As a time-saving device, this routine will not fetch the coordinate
  !         system data from the database (an expensive operation)
  !         if ncsy = csyinx(4), as this would indicate that the data is current.
  !         If you wish to force this routine to fetch coordinate system data (in
  !         the case of loading a local array, for example), you MUST set
  !         ncsy != csyinx(4) before function call.

  !     typ=int,dp,log,chr,dcp   siz=sc,ar(n),func    intent=in,out,inout
  !  input arguments:
  !     variable (typ,siz,intent)    description                  csycom name
  !     ncsy     (int,sc,in)       - coordinate system number
  !     csyinx(4) (int,sc,inout)   - coordinate system number     csyact

  !  output arguments:
  !     csydpx   (dp,ar(18),out)
  !                         csydpx(1-9)   - transformation matrix
  !                               (10-12) - origin (XC, YC, ZC)
  !                               (13-14) - coordinate system parameters  cparm
  !                                                                       cparm2
  !                               (15)    - spare
  !                               (16-18) - defining angles
  !     csyinx   (int,ar(6),out)
  !                         csyinx(1-2)   - theta, phi singularity keys
  !                               (3)     - coordinate system type        icdsys
  !         (csyinx(4) is inout)  (4)     - coordinate system number      csyact
  !                               (5)     - spare
  !                               (6)     - spare
  !     csyget   (int,func,out)      - status of coordinate system
  !                                   = 0 - coordinate system exists
  !                                   = 1 - coordinate system doesn't exist
  INTERFACE
     FUNCTION csyget (ncsy, csydpx, csyinx)
       IMPLICIT NONE
       INTEGER :: csyget
       INTEGER, INTENT(IN) :: ncsy
       DOUBLE PRECISION, INTENT(OUT), DIMENSION(18) :: csydpx
       INTEGER, INTENT(INOUT), DIMENSION(6) :: csyinx
     END FUNCTION csyget
  END INTERFACE

  ! 7.5.18. csyput Subroutine (Storing a Coordinate System)
  ! 7.5.19. csydel Subroutine (Deleting a Coordinate System)
  ! 7.5.20. userac Subroutine (Demonstrates Use of Element Attribute Routines)
  ! 7.6. Coupling and Constraint Routines
  ! 7.6.1. cpinqr Function (Getting Information About a Coupled Set)
  ! 7.6.2. cpget Function (Getting a Coupled Set)
  ! 7.6.3. cpput Subroutine (Storing a Coupled Set)
  ! 7.6.4. cpsel Subroutine (Selecting or Deleting a Coupled Set)
  ! 7.6.5. ceinqr Function (Getting Information About a Constraint Equation Set)
  ! 7.6.6. ceget Function (Getting an Constraint Equation)
  ! 7.6.7. ceput Subroutine (Storing a Constraint Equation)
  ! 7.6.8. cesel Subroutine (Deleting or Selecting a Constraint Equation)
  ! 7.7. Nodal Loading Routines

  ! 7.7.1. disiqr Function (Getting a Information About Constraints)
  ! primary function: get information about constraints
  !     input arguments:
  !        node    (int,sc,in)       - node number for inquire.
  !        key     (int,sc,in)       - key as to the information needed
  !                          = 1              - return constraint mask
  !                          = DB_MAXDEFINED,
  !                            DB_NUMDEFINED  - return number of nodal constraints
  !                                              NOTE: both DB_MAXDEFINED and
  !                                              DB_NUMDEFINED produce the same
  !                                              functionality
  !     output arguments:
  !        disiqr   (int,func,out)   - the returned value of disiqr is based on
  !                                    setting of key.
  INTERFACE
     FUNCTION disiqr(node, key)
       IMPLICIT NONE
       INTEGER :: disiqr
       INTEGER, INTENT(IN) :: node
       INTEGER, INTENT(IN) :: key
     END FUNCTION disiqr
  END INTERFACE

  ! 7.7.2. disget Function (Getting a Constraint from the Database)
  ! *** primary function:    get a constraint from the data base (in raw form)
  ! *** Notice - This file contains ANSYS Confidential information ***
  !     input arguments:
  !     variable (typ,siz,intent)     description
  !        inode    (int,sc,in)       - node number (negative value for no
  !                                                  partabeval)
  !        idf      (int,sc,in)       - reference number for the DOF: (1-32)
  !    UX  = 1, UY  = 2, UZ  = 3, ROTX= 4, ROTY= 5, ROTZ= 6, AX  = 7, AY  = 8
  !    AZ  = 9, VX  =10, VY  =11, VZ  =12
  !    PRES=19, TEMP=20, VOLT=21, MAG =22, ENKE=23, ENDS=24
  !    EMF =25, CURR=26  SP01=27, SP02=28, SP03=29, SP04=30, SP05=31, SP06=32
  !                                 (missing entries are spares)
  !     output arguments:
  !        disget   (int,func,out)    - status of constraint.
  !                                     = 0 - no constraint on this node
  !                                            for this DOF
  !                                     = 4 - this node has a constraint
  !                                            defined for this DOF
  !                                     = -4 - this node has a pseudo-support
  !                                            defined for this DOF
  !        value    (dp,ar(4),out)    - constraint values
  !                         value(1-2) - (real,imag) values of present settings
  !                         value(3-4) - (real,imag) values of previous settings
  INTERFACE
     FUNCTION disget(inode, idf, value)
       IMPLICIT NONE
       INTEGER :: disget
       INTEGER, INTENT(IN) :: inode
       INTEGER, INTENT(IN) :: idf
       DOUBLE PRECISION, DIMENSION(4), INTENT(OUT) :: value
     END FUNCTION disget
  END INTERFACE

  ! 7.7.3. disput Subroutine (Storing a Constraint at a Node)
  ! 7.7.4. disdel Subroutine (Deleting a Constraint at a Node)

  ! 7.7.5. foriqr Function (Getting Information About Nodal Loads)
  ! primary function: get information about nodal loads.
  !     input arguments:
  !        node     (int,sc,in)       - number of node being inquired about.
  !                                      should be 0 for key=DB_MAXDEFINED or
  !                                      DB_NUMDEFINED
  !        key      (dp,sc,in)        - key as to information needed
  !                             = 1              - return force mask for node
  !                             = DB_MAXDEFINED,
  !                               DB_NUMDEFINED  - return number of nodal loadings
  !                                                 in model
  !                                    NOTE: both DB_MAXDEFINED and DB_NUMDEFINED
  !                                    produce the same functionality

  !     output arguments:
  !        foriqr   (int,func,out)    - the returned value of foriqr is based on
  !                                     setting of key.

  INTERFACE
     FUNCTION foriqr(node, key)
       IMPLICIT NONE
       INTEGER :: foriqr
       INTEGER, INTENT(IN) :: node
       INTEGER, INTENT(IN) :: key
     END FUNCTION foriqr
  END INTERFACE

  ! 7.7.6. forget Function (Getting a Constraint from the Database)
  ! primary function:    get a force from the data base (in raw form)
  !     input arguments:
  !        inode    (int,sc,in)       - node number (negative value for no
  !                                                  partabeval)
  !        idf      (int,sc,in)       - reference number for the DOF: (1-32)
  !                                     (see echprm.inc)
  !     output arguments:
  !        forget   (int,func,out)  - status of constraint.
  !                                   = 0 - no loading on this node for this DOF
  !                                   = 4 - this node has a loading for this DOF
  !        value    (dp,ar(4),out)
  !                         value(1-2) - (real,imag) values of present settings
  !                         value(3-4) - (real,imag) values of previous settings
  INTERFACE
     FUNCTION forget(inode, idf, value)
       IMPLICIT NONE
       INTEGER :: forget
       INTEGER, INTENT(IN) :: inode
       INTEGER, INTENT(IN) :: idf
       DOUBLE PRECISION, INTENT(OUT), DIMENSION(4) :: value
     END FUNCTION forget
  END INTERFACE

  ! 7.7.7. forput Subroutine (Storing a Nodal Load at a Node)
  ! 7.7.8. fordel Subroutine (Deleting a Nodal Load at a Node)
  ! 7.7.9. ntpiqr Function (Getting Information About a Nodal Temperature)
  ! 7.7.10. ntpget Function (Getting a Specified Nodal Temperature)
  ! 7.7.11. ntpput Subroutine (Storing a Nodal Temperature)
  ! 7.7.12. ntpdel Subroutine (Deleting a Nodal Temperature)
  ! 7.7.13. nhgiqr Function (Getting Information About Nodal Heat Generations)
  ! 7.7.14. nhgget Function (Getting a Nodal Heat Generation)
  ! 7.7.15. nhgput Subroutine (Storing Nodal Heat Generation)
  ! 7.7.16. nhgdel Subroutine (Deleting a Nodal Heat Generation)
  ! 7.7.17. nfuiqr Function (Getting Information About Nodal Fluences)
  ! 7.7.18. nfuget Function (Getting a Nodal Fluence)
  ! 7.7.19. nfuput Subroutine (Storing a Nodal Fluence)
  ! 7.7.20. nfudel Subroutine (Deleting a Nodal Fluence)
  ! 7.7.21. ndciqr Function (Getting Information About Nodal Current Densities)
  ! 7.7.22. ndcget Function (Getting a Nodal Current Density)
  ! 7.7.23. ndcput Subroutine (Storing a Nodal Current Density)
  ! 7.7.24. ndcdel Subroutine (Deleting a Nodal Current Density)
  ! 7.7.25. nvdiqr Function (Getting Information About Nodal Magnetic Virtual Displacements)
  ! 7.7.26. nvdget Function (Getting a Nodal Magnetic Virtual Displacement)
  ! 7.7.27. nvdput Subroutine (Storing a Nodal Virtual Displacement)
  ! 7.7.28. nvddel Subroutine (Deleting a Nodal Virtual Displacement)
  ! 7.8. Element Loading Routines

  ! 7.8.1. epriqr Function (Getting Information About Element Pressure/Convection)
  ! *** primary function: get information about element pressure/convection
  ! *** Notice - This file contains ANSYS Confidential information ***
  !     input arguments:
  !        ielem    (int,sc,in)       - element number
  !                                      should be zero for key=DB_NUMDEFINED or
  !                                      DB_MAXRECLENG
  !        iface    (int,sc,in)       - face number for inquire (0-6)
  !                                      face number is needed for key=5. for
  !                                      other values of key, iface has different
  !                                      meaning (see below)
  !        key      (int,sc,in)       - key as to the information needed
  !                 = 1              - return pressure mask for element
  !                 = 5              - return number of pressures for this
  !                                      element face
  !                 = DB_NUMDEFINED,
  !                 = DB_MAXDEFINED  - return value is based on setting of iface
  !                                     NOTE: both DB_NUMDEFINED and
  !                                     DB_MAXDEFINED produce the same
  !                                     functionality
  !                             iface = 0 - return number of surface loads defined for model
  !                                   = 1-6 - return number of pressure loads
  !                                      defined for this element.
  !                                      NOTE:  only 1-6 is valid, but this
  !                                      routine simply checks that iface is in
  !                                      the range.  The actual value of iface
  !                                      does not matter in this case.
  !                 = DB_MAXRECLENG  - return the maximum number of element
  !                                     pressures on any element (max record
  !                                     length)
  !     output arguments:
  !        epriqr   (int,func,out)    - the returned value of epriqr is based on
  !                                      setting of key.
  INTERFACE
     FUNCTION epriqr(ielem, iface, key)
       IMPLICIT NONE
       INTEGER :: epriqr
       INTEGER, INTENT(IN) :: ielem
       INTEGER, INTENT(IN) :: iface
       INTEGER, INTENT(IN) :: key
     END FUNCTION epriqr
  END INTERFACE

  ! 7.8.2. eprget Function (Getting an Element Face Pressure)
  ! *** primary function:    get an element face pressure
  ! *** Notice - This file contains ANSYS Confidential information ***
  !     input arguments:
  !        elem     (int,sc,in)       - element number (negative value for
  !                                       no partabeval)
  !        iface    (int,sc,in)       - face number (1-6)
  !     output arguments:
  !        eprget   (int,func,out)    - status of element.
  !                                     =-1 - element has no pressures
  !                                     = 0 - this element face has no pressures
  !                                     > 0 - number of values defined
  !        value    (dp ,ar(*),out)   - the element pressures (real,imag) at each
  !                                      face
  INTERFACE
     FUNCTION eprget(elem, iface, value)
       IMPLICIT NONE
       INTEGER :: eprget
       INTEGER, INTENT(IN) :: elem
       INTEGER, INTENT(IN) :: iface
       DOUBLE PRECISION, INTENT(OUT), DIMENSION(*) :: value
     END FUNCTION eprget
  END INTERFACE

  ! 7.8.3. eprput Subroutine (Storing an Element Face Pressure)
  ! 7.8.4. eprdel Subroutine (Deleting an Element Pressure/Convection)
  ! 7.8.5. ecviqr Function (Getting Information About Element Convections)
  ! 7.8.6. ecvget Function (Getting an Element Face Convection)
  ! 7.8.7. ecvput Subroutine (Storing an Element Face Convection)
  ! 7.8.8. ecvdel Subroutine (Deleting a Convection on an Element)
  ! 7.8.9. etpiqr Function (Getting Information About Element Temperatures)
  ! 7.8.10. etpget Function (Getting an Element Temperature)
  ! 7.8.11. etpput Subroutine (Storing an Element Temperature)
  ! 7.8.12. etpdel Subroutine (Deleting an Element Temperature)
  ! 7.8.13. ehgiqr Function (Getting Information About Element Heat Generation)
  ! 7.8.14. ehgget Function (Getting an Element Heat Generation)
  ! 7.8.15. ehgput Subroutine (Storing an Element Heat Generation)
  ! 7.8.16. ehgdel Subroutine (Deleting an Element Heat Generation)
  ! 7.8.17. efuiqr Function (Getting Information About Element Fluences)
  ! 7.8.18. efuget Function (Getting an Element Fluence)
  ! 7.8.19. efuput Subroutine (Storing an Element Fluence)
  ! 7.8.20. efudel Subroutine (Deleting an Element Fluence)
  ! 7.8.21. edciqr Function (Getting Information About Element Current Densities)
  ! 7.8.22. edcget Function (Getting Element Current Densities)
  ! 7.8.23. edcput Subroutine (Storing an Element Current Density)
  ! 7.8.24. edcdel Subroutine (Deleting an Element Current Density)
  ! 7.8.25. evdiqr Function (Getting Information About Element Virtual Displacements)
  ! 7.8.26. evdget Function (Getting an Element Virtual Displacement)
  ! 7.8.27. evdput Subroutine (Storing an Element Virtual Displacement)
  ! 7.8.28. eimiqr Function (Getting Information About Element Impedances)
  ! 7.8.29. eimget Function (Getting an Element Face Impedance)
  ! 7.8.30. eimput Subroutine (Storing an Element Impedance)
  ! 7.8.31. eimdel Subroutine (Deleting an Element Impedance)
  ! 7.8.32. esfiqr Function (Getting Information About Element Surface Stress Data)
  ! 7.8.33. esfget Function (Getting Element Surface Stress Data)
  ! 7.8.34. esfput Subroutine (Storing Element Surface Stress Data)
  ! 7.8.35. esfdel Subroutine (Deleting an Element's Surface Stress Data)
  ! 7.8.36. efsdel Subroutine (Deleting a Flagged Surface on an Element)
  ! 7.8.37. efsget function (Getting Element Face Flagged Surfaces)
  ! 7.8.38. efsiqr function (Getting Information About Flagged Surfaces)
  ! 7.8.39. efsput Subroutine (Storing an Element Face Flagged Surface)
  ! 7.9. Results Information Routines
  ! 7.9.1. dspiqr Function (Getting Information About Nodal Results)
  ! 7.9.2. dspget Function (Getting a Nodal Result from the Database)
  ! 7.9.3. dspput Subroutine (Storing a Constraint at a Node)
  ! 7.9.4. dspdel Subroutine (Deleting a Result at a Node)
  ! 7.9.5. emsiqr Function (Getting Information About an Element's Miscellaneous Summable Data)
  ! 7.9.6. emsget Function (Getting an Element's Miscellaneous Summable Data)
  ! 7.9.7. emsput Subroutine (Storing an Element's Miscellaneous Summable Data)
  ! 7.9.8. emsdel Subroutine (Deleting an Element's Miscellaneous Summable Data)
  ! 7.9.9. enfiqr Function (Getting Information About Element Nodal Forces)
  ! 7.9.10. enfget Function (Getting an Element's Nodal Forces)
  ! 7.9.11. enfput Subroutine (Storing an Element's Nodal Forces)
  ! 7.9.12. enfdel Subroutine (Deleting an Element's Nodal Forces)
  ! 7.9.13. ensiqr Function (Getting Information About an Element's Nodal Stresses)
  ! 7.9.14. ensget Function (Getting an Element's Nodal Stresses)
  ! 7.9.15. ensput Subroutine (Storing Nodal Stresses at an Element)
  ! 7.9.16. ensdel Subroutine (Deleting an Element's Nodal Stresses)
  ! 7.9.17. engiqr Function (Getting Information About an Element's Energies)
  ! 7.9.18. engget Function (Getting an Element's Energies)
  ! 7.9.19. engput Subroutine (Storing an Element's Energies and Volume)
  ! 7.9.20. engdel Subroutine (Deleting an Element's Energies)
  ! 7.9.21. egriqr Function (Getting Information About an Element's Nodal Gradients)
  ! 7.9.22. egrget Function (Getting an Element's Nodal Gradients)
  ! 7.9.23. egrput Subroutine (Storing an Element's Nodal Gradients)
  ! 7.9.24. egrdel Subroutine (Deleting an Element's Nodal Gradients)
  ! 7.9.25. eeliqr Function (Getting Information About an Element's Nodal Elastic Strains)
  ! 7.9.26. eelget Function (Getting an Element's Nodal Elastic Strains)
  ! 7.9.27. eelput Subroutine (Storing an Element's Nodal Elastic Strains)
  ! 7.9.28. eeldel Subroutine (Deleting an Element's Nodal Elastic Strains)
  ! 7.9.29. epliqr Function (Getting Information About an Element's Nodal Plastic Strains)
  ! 7.9.30. eplget Function (Getting an Element's Nodal Plastic Strains)
  ! 7.9.31. eplput Subroutine (Storing an Element's Nodal Plastic Strains)
  ! 7.9.32. epldel Subroutine (Deleting an Element's Nodal Plastic Strains)
  ! 7.9.33. ecriqr Function (Getting Information About an Element's Nodal Creep Strains)
  ! 7.9.34. ecrget Function (Getting an Element's Nodal Creep Strains)
  ! 7.9.35. ecrput Subroutine (Storing an Element's Nodal Creep Strains)
! 7.9.36. ecrdel Subroutine (Deleting an Element's Nodal Creep Strains)
  ! 7.9.37. ethiqr Function (Getting Information About an Element's Nodal Thermal Strains)
  ! 7.9.38. ethget Function (Getting an Element's Nodal Thermal Stresses)
  ! 7.9.39. ethput Subroutine (Storing an Element's Nodal Thermal Stresses)
  ! 7.9.40. ethdel Subroutine (Deleting an Element's Thermal, Initial, and Swelling Strains)
  ! 7.9.41. euliqr Function (Getting Information About an Element's Euler Angles)
  ! 7.9.42. eulget Function (Getting an Element's Nodal Euler Angles)
  ! 7.9.43. eulput Subroutine (Storing an Element's Euler Angles)
  ! 7.9.44. euldel Subroutine (Deleting an Element's Euler Angles)
  ! 7.9.45. efxiqr Function (Getting Information About Element Fluxes)
  ! 7.9.46. efxget Function (Getting an Element Flux)
  ! 7.9.47. efxput Subroutine (Storing an Element's Fluxes)
  ! 7.9.48. efxdel Subroutine (Deleting Element Fluxes)
  ! 7.9.49. elfiqr Function (Getting Information About Element Local Forces)
  ! 7.9.50. elfget Function (Getting an Element Local Force)
  ! 7.9.51. elfput Subroutine (Storing an Element's Local Forces)
  ! 7.9.52. elfdel Subroutine (Deleting Element Local Forces)
  ! 7.9.53. emniqr Function (Getting Information About Element Miscellaneous Non-summable Data)
  ! 7.9.54. emnget Function (Getting an Element's Miscellaneous Non-summable Data)
  ! 7.9.55. emnput Subroutine (Storing an Element's Miscellaneous Non-summable Data)
  ! 7.9.56. emndel Subroutine (Deleting an Element's Miscellaneous Non-summable Data)
  ! 7.9.57. ecdiqr Function (Getting Information About Element Current Densities)
  ! 7.9.58. ecdget Function (Getting an Element Current Density)
  ! 7.9.59. ecdput Subroutine (Storing an Element's Current Densities)
  ! 7.9.60. ecddel Subroutine (Deleting Element Current Densities)
  ! 7.9.61. enliqr Function (Getting Information About Element Nonlinear Tables)
  ! 7.9.62. enlget Function (Getting Element Nonlinear Tables)
  ! 7.9.63. enlput Subroutine (Storing an Element's Nonlinear Tables)
  ! 7.9.64. enldel Subroutine (Deleting Element Nonlinear Tables)
  ! 7.9.65. ehciqr Function (Getting Information About Calculated Element Heat Generations)
  ! 7.9.66. ehcget Function (Getting a Calculated Element Heat Generation)
  ! 7.9.67. ehcput Subroutine (Storing an Element's Calculated Heat Generations)
  ! 7.9.68. ehcdel Subroutine (Deleting Element Calculated Heat Generations)
  ! 8. Subroutines for Users' Convenience
  ! 8.1. Input and Output Abbreviations
  ! 8.2. General Subroutines
  ! 8.2.1. dptoch Subroutine (Retrieve Eight Characters From a Double Precision Variable)

  ! 8.2.2. wrinqr Function (Obtain Information About Output)
  !     wrinqr Function (Obtain Information About Output)
  !     function wrinqr (key)
  !     primary function:
  !       obtain information about output

  ! --- caution: the following variables are "saved/resumed".
  ! ---          key=WR_COLINTER thru WR_SUPCOLMAX in "wrinqr/wrinfo"
  ! ---           (data for "/fmt,/page,/header" commands).
  ! ---           note that the whole common cannot be "saved/resumed".  cwa
  !
  !  input arguments:
  !     variable (typ,siz,intent)    description                        wrcom name
  !     key      (int,sc,in)
  !                  = WR_PRINT       - print flag (kprint)                 prtkey
  !                       wrinqr = 0 - no output
  !                              = 1 - print
  !                  = WR_OUTPUT      - current output unit number (iott)  outfil
  !                  = WR_MASTEROUT   - master output file                 frstot
  !                  = WR_COLINTER    - interactive columns per page       intcol
  !                  = WR_COLBATCH    - batch columns per page             batcol
  !                  = WR_LINEINTER   - interactive lines per page         intlin
  !                  = WR_LINEBATCH   - batch lines per page               batlin
  !                  = WR_CHARITEM    - characters per output item         chrper
  !                  = WR_CHARDECIMAL - characters past decimal            chrdec
  !                  = WR_CHARINTEGER - characters in leading integer      chrint
  !                  = WR_CHARTYPE    -                                    chrtyp
  !                        wrinqr = 1 - using E format in output
  !                               = 2 - using F format in output
  !                               = 3 - using G format in output
  !                  = WR_SUPTITLE    - tlabel supress key                 keyhed
  !                  = WR_SUPSUBTITLE - subtitle supress key               keytit
  !                  = WR_SUPLSITER   - ls,iter id supress key             keyid
  !                  = WR_NOTELINE    - note line supress key              keynot
  !                  = WR_SUPCOLHEADER - column header supress key         keylab
  !                  = WR_SUPCOLMAX   - column maximum supress key         keysum
  !                  = WR_LISTOPT     - ListOpt from /output command      ListOpt
  !
  !  output arguments:
  !     wrinqr   (int,func,out)      - the value corresponding to key
  INTERFACE
     FUNCTION wrinqr(key)
       IMPLICIT NONE
       INTEGER :: wrinqr
       INTEGER, INTENT(IN) :: key
     END FUNCTION wrinqr
  END INTERFACE

  ! 8.2.3. erinqr Subroutine (Obtaining Information from the Errors Common)
  ! *** primary function:    obtain information from errors common
  !
  ! *** Notice - This file contains ANSYS Confidential information ***
  !
  !  input arguments:
  !     key      (int,sc,in)       - item to be returned
  !                                  1=keyerr, 2=errfil,    3=numnot, 4=numwrn,
  !                                  5=numerr, 6=numfat,    7=maxmsg, 8=lvlerr,
  !                                  9=mxpcmd, 10=nercmd,  11=nertim,12=nomore,
  !                                  13=eropen,14=ikserr,  15=kystat,16=mxr4r5,
  !                                  17=mshkey,            19=opterr,20=flowrn,
  !                                  21=errhpi,22=noreport,23=pdserr,24=mxpcmdw
  !                                  25=kystop,26=icloads, 27=ifkey
  !
  ! ---- below definitions copied from errcom 7/92 for user information
  !
  !                            *** key number= ..........................
  !                    (see ansysdef for parameter definitions)          |
  !                                                                      \/
  !
  !o keyerr - master error flag                                    (ER_ERRORFLAG)
  !o errfil - errors file unit number                              (ER_ERRORFILE)
  !o numnot - total number of notes displayed                      (ER_NUMNOTE)
  !o numwrn - total number of warnings displayed                   (ER_NUMWARNING)
  !o numerr - total number of errors displayed                     (ER_NUMERROR)
  !o numfat - total number of fatals displayed                     (ER_NUMFATAL)
  !o maxmsg - max allowed number of displayed messages before abort(ER_MAXMESSAGE)
  !o lvlerr - used basicly in solution (from cnvr command.)        (ER_ERRORLEVEL)
  !o           -1=do not set keyerr for notes/errors/warnings.
  !o           -2=same as -1 but do not display message either.
  !o mxpcmd - maximum number of messages allowed per command       (ER_MAXCOMMAND)
  !o nercmd - number of messages displayed for any one command     (ER_NUMCOMMAND)
  !o nertim - key as to how message cleared from u/i pop-up        (ER_UICLEAR)
  !o          (as per rsg/pft 5/1/92 - only for "info" calls
  !o           -1=message is timed before removal
  !o            0=message needs pick or keyboard before removal
  !o            1=message stays up untill replaced by another message
  !o nomore   display any more messages                            (ER_NOMOREMSG)
  !o           0=display messages
  !o           1=display discontinue message and stop displaying
  !o eropen - 0=errors file is closed                              (ER_FILEOPEN)
  !o          1=errors file is opened
  !o ikserr - 0=if interactive do not set keyerr                   (ER_INTERERROR)
  !         - 1=if interactive set keyerr (used by mesher and tessalation)
  !o kystat - flag to bypass keyopt tests in the elcxx routines    (ER_KEYOPTTEST)
  !           associated with status/panel info  inquiries.
  !            0=do not bypass keyopt tests
  !            1=perform all keyopt tests
  !           also flag to bypass setting of _STATUS upon resume
  !o mxr4r5 - mixed rev4-rev5 input logic (*do,*if,*go,*if-go)     (ER_MIXEDREV)
  !            (used in chkmix called from rdmac)
  !            1=rev5 found (*do,*fi-then-*endif)
  !            2=rev4 found (*go,:xxx,*if,....,:xxx)
  !            3=warning printed. do not issue any more.
  !o mshkey - cpu intensive meshing etc. this will cause           (ER_MESHING)
  !           "nertim (11)" to be set to -1 for "notes", 1 for "warnings",
  !           and 0 for "errors". checking of this key is done in "anserr".
  !            0=not meshing or cpu intensive
  !            1=yes, meshing or cpu intensive
  !o syerro - systop error code. read by anserr if set.            (18)
  !o opterr - 0=no error in main ansys during opt looping          (ER_OPTLOOPING)
  !           1=an error has happened in main ansys during opt looping
  !o flowrn - flag used by "floqa" as to list floqa.ans            (20)
  !           0=list "floqa.ans"
  !           1="floqa.ans" has been listed. do not list again.
  !o noreport- used in GUI for turning off errors due to strsub calls (22)
  !           0=process errors as usual
  !           1=do NOT report errors
  !o pdserr - 0=no error in main ansys during pds looping          (ER_PDSLOOPING)
  !           1=an error has happened in main ansys during pds looping
  !o mxpcmdw- number of messages written to file.err for any one   (24)
  !o          command
  !           0=write all errors to file.err
  !           1=only write displayed errors to file.err
  !o icloads - key to forbid the iclist command from listing solution    (26)
  !            data instead of the input data.
  !           0=iclist is OK
  !           1=do not permit iclist
  !o ifkey   - key on whether or not to abort during /input on error     (27)
  !           0=do not abort
  !           1=abort
  !
  !o espare - spare integer variables
  !
  !  --- end of information from errcom
  !
  !  output arguments:
  !     erinqr   (int,sc,out)      - value corresponding to key
  INTERFACE
     FUNCTION erinqr(key)
       IMPLICIT NONE
       INTEGER :: erinqr
       INTEGER, INTENT(IN) :: key
     END FUNCTION erinqr
  END INTERFACE

  ! 8.2.4. TrackBegin Subroutine (Beginning Tracking for a Subroutine Call)
  ! function: mark beginning of track ansys call
  !  input arguments:
  !     sub32   (char*(*),sc,in)    - name of subroutine being entered and left
  !                                    (32 characters max)
  !  output arguments:  none
  INTERFACE
     SUBROUTINE TrackBegin(sub32)
       IMPLICIT NONE
       CHARACTER(LEN=*), intent(in) :: sub32
     END SUBROUTINE TrackBegin
  END INTERFACE

  ! 8.2.5. TrackEnd Subroutine (Ending Tracking for a Subroutine Call)
  ! function: mark end of track ansys call
  !  input arguments:
  !     sub32     (char*(*),sc,in)  - name of subroutine being left
  !                                   (32 characters max)
  !  output arguments:  none
  INTERFACE
     SUBROUTINE TrackEnd (sub32)
       IMPLICIT NONE
       CHARACTER(LEN=*), intent(in) :: sub32
     END SUBROUTINE TrackEnd
  END INTERFACE

  ! 8.2.6. erhandler Subroutine (Displaying ANSYS Errors)
  ! *** primary function:    Display ANSYS error messages
  ! *** Notice - This file contains ANSYS Confidential information ***
  !  input arguments:
  !     filein   (ch*40,sc,in)     - Filename used for character portion of
  !                                  message ID (this is the file name of the
  !                                  file which contains the source for this
  !                                  routine)
  !
  !                                  if 'ErrorMessageProbe', then error was
  !                                    generated on another processor (distributed
  !                                    ANSYS). In that case, dperr contains the
  !                                    message already made ASCII and expanded
  !
  !     msgid    (int,sc,in)       - Numeric portion of the message ID
  !                                  1 - 9999, unique for each erhandler
  !                                  call in the FILE.  Recommend using
  !                                  a sequence, similar to format conventions,
  !                                  i.e., 5000, 5010, 5020
  !                                  if filein='ErrorMessageProbe', this is the
  !                                    CPU # that originally generated the error
  !     msglvl   (int,sc,in)       - level of error (same as lngerr)
  !                                  0=no label (used for u/i pop-ups)
  !                                 -1=no label (used for u/i pop-ups) timed
  !                                    as a note message
  !                                  1=note, 2=warning, 3=error, 4=fatal
  !                                                    -3=error w/tech supp note
  !                                                    -4=fatal w/tech supp note
  !                                     (see lngerr.F for text of tech supp note)
  !     lngstrng (ch*(*),sc,in)    - error message to display. use keywords
  !                                  of %i %g %c %/ for formating (same as
  !                                  lngerr)
  !     dperr    (dp,ar(*),in)     - vector of data to display. contains both
  !                                  integer and double precision data.
  !                                  (same as lngerr)
  !                                    if filein='ErrorMessageProbe', dperr
  !                                    contains the unpacked message and lngstrng
  !                                    and cherr are ignored
  !     cherr    (ch*(*),ar(*),in) - vector of character data to display
  !                                  max length of character data is 32
  !                                  characters
  INTERFACE
     SUBROUTINE erhandler(filein, msgid, msglvl, lngstrng, &
          dperr, cherr)
       CHARACTER(LEN=40), INTENT(IN) :: filein
       INTEGER, INTENT(IN) :: msgid
       INTEGER, INTENT(IN) :: msglvl
       CHARACTER(LEN=*), INTENT(IN) :: lngstrng
       DOUBLE PRECISION, INTENT(IN), DIMENSION(*) :: dperr
       CHARACTER(LEN=32), INTENT(IN), DIMENSION(*) :: cherr
     END SUBROUTINE erhandler
  END INTERFACE

  ! 8.2.7. intrp Subroutine (Doing Single Interpolation)
  ! 8.2.8. tranx3 Subroutine (Processing Geometry for 3-D Line Elements)
  ! 8.2.9. systop Subroutine (Stopping an ANSYS Program Run)
  ! 8.3. Vector Functions
  ! 8.3.1. vdot Function (Computing the Dot Product of Two Vectors)
  ! 8.3.2. vsum Function (Summing Vector Components)
  ! 8.3.3. vmax Function (Retrieving the Maximum Vector Value at a Given Location)
  ! 8.3.4. lastv Function (Retrieving the Position of the Last Nonzero Term in a Double Precision Vector)

  ! 8.3.5. izero Function (Setting an Integer Vector to Zero)

  ! **********  set an integer vector to zero  **********
  INTERFACE
     SUBROUTINE izero (ivect, n)
       IMPLICIT NONE
       INTEGER, INTENT(OUT), DIMENSION(n) :: ivect
       INTEGER, INTENT(IN) :: n
     END SUBROUTINE izero
  END INTERFACE

  ! 8.3.6. imove Function (Assigning Equal Values to Two Integer Vectors)

  ! 8.3.7. vzero Subroutine (Initializing a Vector to Zero)
  ! primary function:    initialize a vector to zero
  !  input arguments:
  !     v        (dp,ar(n),inout)  - vector to be zeroed out
  !     n        (int,sc,in)       - number of words to zero out
  !  output arguments:
  !     v        (dp,ar(n),inout)  - zeroed vector c
  INTERFACE
     SUBROUTINE vzero (v,n)
       IMPLICIT NONE
       DOUBLE PRECISION, INTENT(OUT), DIMENSION(n) :: v
       INTEGER, INTENT(IN) :: n
     END SUBROUTINE vzero
  END INTERFACE

  ! 8.3.8. vmove Subroutine (Moving One Vector into Another)
  ! 8.3.9. vimove Subroutine (Moving One Vector into Another Incrementally)
  ! 8.3.10. vinit Subroutine (Assigning a Scalar Constant to a Vector)
  ! 8.3.11. viinit Subroutine (Assigning a Scalar Constant to a Vector Incrementally)
  ! 8.3.12. vapb Subroutine (Setting a Vector to Sum of Two Vectors)
  ! 8.3.13. vapb1 Subroutine (Combining Two Vectors in One)
  ! 8.3.14. vapcb1 Subroutine (Multiplying a Vector to a Constant)
  ! 8.3.15. vamb Subroutine (Gets a Third Vector by Subtracting One Vector from Another)
  ! 8.3.16. vamb1 Subroutine (Subtracting One Vector from Another)
  ! 8.3.17. vmult Subroutine (Multiplying a Vector by a Constant)
  ! 8.3.18. vmult1 Subroutine (Multiplying a Vector by a Constant)
  ! 8.3.19. vcross Subroutine (Defining a Vector via a Cross Product)
  ! 8.3.20. vnorme Subroutine (Normalizing a Three-Component Vector)
  ! 8.3.21. vnorm Subroutine (Normalizing a Vector to Unit Length)

  ! 8.3.22. ndgxyz Function (Getting the X,Y,Z Vector for a Node)

  ! *** primary function:    get x,y,z vector for a node.
  !     input arguments:
  !        node     (int,sc,in)       - node number for operation.
  !     output arguments:
  !        ndgxyz   (int,sc,out)      - status of node.
  !                                       0=node is undefined.
  !                                      -1=node is unselected.
  !                                       1=node is selected.
  !        xyz      (dp,ar(3),out)    - vector containing x,y,z
  INTERFACE
     FUNCTION ndgxyz(node, xyz)
       INTEGER :: ndgxyz
       INTEGER, INTENT(IN) :: node
       DOUBLE PRECISION, INTENT(OUT), DIMENSION(3) :: xyz
     END FUNCTION ndgxyz
  END INTERFACE

  ! 8.3.23. ndpxyz Subroutine (Storing X,Y,Z for a Node)
  ! 8.4. Matrix Subroutines
  ! 8.4.1. maxv Subroutine (Multiplying a Vector by a Matrix)
  ! 8.4.2. maxv1 Subroutine (Multiplying a Vector by a Matrix)
  ! 8.4.3. matxv Subroutine (Multiplying a Vector by a Full Transposed Matrix)
  ! 8.4.4. matxv1 Subroutine (Multiplying a Vector by a Full Transposed Matrix)
  ! 8.4.5. matxb Subroutine (Transposing a matrix)
  ! 8.4.6. maat Subroutine (Changing a Matrix Value via Addition, Multiplication, and Transposition)
  ! 8.4.7. matsym Subroutine (Filling the Upper Triangle from the Lower Triangle)
  ! 8.4.8. mctac Subroutine (Transposing a symmetric matrix)
  ! 8.4.9. tran Subroutine (Transposing a matrix)
  ! 8.4.10. symeqn Subroutine (Solving Simultaneous Linear Equations)

END MODULE ansys_upf

! Local Variables:
! compile-command:"make -C .. test"
! End:
