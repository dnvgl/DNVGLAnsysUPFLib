
!     variables to interface with ANSYS
      INTEGER ityp, ielc(IelCSZ)

!     global information about ANSYS model
      INTEGER n_nodes, n_nloads, n_elem, n_masses, n_mass_groups

!     variables and parameters

!     NZERO is used to compare double numbers
!     NDIGITS is the number of significant digits
      DOUBLE PRECISION NZERO, NDIGITS
      PARAMETER (NZERO = 1E-30, NDIGITS=8)

!     at this point, just one structure is admitted
      INTEGER structure
      PARAMETER (structure = 1)
!     [jobname].ascii is the file name opened as outfile
      CHARACTER(LEN=32) :: jobname
      INTEGER outfile, sfiles

!     this block holds the component information
      INTEGER el_offset, el_length, el_count

      INTEGER max_components
      PARAMETER (max_components = 1024)
      CHARACTER(LEN=16) :: l_comp_name (max_components)
      CHARACTER(LEN=PARMSIZE) :: comp_name(max_components)
      INTEGER        l_comp_pos  (max_components)
      INTEGER        l_comp_len  (max_components)
      CHARACTER(LEN=PARMSIZE) :: l_comp_ansys(max_components)
      INTEGER :: l_comp_csub(max_components)
      INTEGER :: l_csub_gid(max_components)
      INTEGER comp_num

      INTEGER n_sfiles, max_loadcases

      INTEGER bp_length, bp_num_i, bp_num_j
      INTEGER bp_R, bp_A, bp_I, bp_e, bp_sc, bp_sf, bp_d, bp_rlen
      PARAMETER (bp_R = 0, bp_A=1, bp_I=4, bp_e=7, bp_sc=13, bp_sf=15,  &
     &     bp_d=17, bp_rlen=20)

      INTEGER rl_length, rl_num

      COMMON / ans2bmf_ans2bmf /                                        &
     &     ityp, ielc, n_nodes, n_nloads, n_sfiles, max_loadcases,      &
     &   jobname, outfile, sfiles, n_elem, n_masses, n_mass_groups,     &
     &   el_offset, el_length, el_count,                                &
     &     l_comp_name, comp_name, l_comp_pos, l_comp_len, comp_num,    &
     &     l_comp_ansys, l_comp_csub,                                   &
     &   bp_length, bp_num_i, bp_num_j,                                 &
     &   rl_length, rl_num,                                             &
     &   l_csub_gid

#include "csubdata.inc"

! Local Variables:
! compile-command:"make -C .. test"
! End:
