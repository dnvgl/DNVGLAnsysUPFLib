MODULE ans_common

  USE csubdata
  USE ansys_par

  ! variables to interface with ANSYS
  INTEGER :: ityp
  INTEGER :: ielc(IelCSZ)

  ! global information about ANSYS model
  INTEGER :: n_nodes
  INTEGER :: n_nloads
  INTEGER :: n_elem
  INTEGER :: n_masses
  INTEGER :: n_mass_groups

  ! variables and parameters

  ! NZERO is used to compare double numbers
  ! NDIGITS is the number of significant digits
  DOUBLE PRECISION, PARAMETER :: NZERO=1E-30
  DOUBLE PRECISION, PARAMETER :: NDIGITS=8

  ! at this point, just one structure is admitted
  INTEGER, PARAMETER :: structure=1

  ! [jobname].ascii is the file name opened as outfile
  CHARACTER(LEN=32) :: jobname
  INTEGER :: outfile
  INTEGER :: sfiles

  ! this block holds the component information
  INTEGER :: el_offset
  INTEGER :: el_length
  INTEGER :: el_count

  INTEGER, PARAMETER :: max_components=1024
  CHARACTER(LEN=16), DIMENSION(max_components) :: l_comp_name
  CHARACTER(LEN=PARMSIZE), DIMENSION(max_components) :: comp_name
  INTEGER, DIMENSION(max_components) :: l_comp_pos
  INTEGER, DIMENSION(max_components) :: l_comp_len
  CHARACTER(LEN=PARMSIZE), DIMENSION(max_components) :: l_comp_ansys
  INTEGER, DIMENSION(max_components) :: l_comp_csub
  INTEGER, DIMENSION(max_components) :: l_csub_gid
  INTEGER :: comp_num

  INTEGER :: n_sfiles
  INTEGER :: max_loadcases

  INTEGER :: bp_length
  INTEGER :: bp_num_i
  INTEGER :: bp_num_j
  INTEGER, PARAMETER :: bp_R=0
  INTEGER, PARAMETER :: bp_A=1
  INTEGER, PARAMETER :: bp_I=4
  INTEGER, PARAMETER :: bp_e=7
  INTEGER, PARAMETER :: bp_sc=13
  INTEGER, PARAMETER :: bp_sf=15
  INTEGER, PARAMETER :: bp_d=17
  INTEGER, PARAMETER :: bp_rlen=20

  INTEGER :: rl_length
  INTEGER :: rl_num

  LOGICAL :: batch

END MODULE ans_common

! Local Variables:
! compile-command:"make -C .. test"
! End:
