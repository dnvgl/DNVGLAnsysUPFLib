
#include "impcom.inc"

c     external functions from ANSYS library

c     external procedures
!      EXTERNAL anserr, rlsel

c     variables to interface with ANSYS
      INTEGER nodes(NNMAX), elmdat(EL_DIM), kerr
      INTEGER ityp, ielc(IelCSZ)
      DOUBLE PRECISION rtable(400), mptemp(100), mpval(100), subc(3)


c     global information about ANSYS model
      INTEGER n_nodes, n_nloads, n_elem, n_masses, n_mass_groups

c     variables and parameters

c     NZERO is used to compare double numbers
c     NDIGITS is the number of significant digits
      DOUBLE PRECISION NZERO, NDIGITS
      PARAMETER (NZERO = 1E-30, NDIGITS=8)

c     at this point, just one structure is admitted
      INTEGER structure
      PARAMETER (structure = 1)
c     [jobname].ascii is the file name opened as outfile
      CHARACTER(LEN=32) :: jobname
      INTEGER outfile, sfiles

c     this block holds the component information
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
      PARAMETER (bp_R = 0, bp_A=1, bp_I=4, bp_e=7, bp_sc=13, bp_sf=15,
     $     bp_d=17, bp_rlen=20)

      INTEGER rl_length, rl_num

      COMMON / ans2bmf_ans2bmf /
     x   nodes, elmdat, subc, kerr,
     x     ityp, ielc, n_nodes, n_nloads, n_sfiles, max_loadcases,
     x   jobname, outfile, sfiles, n_elem, n_masses, n_mass_groups,
     x   el_offset, el_length, el_count,
     x     l_comp_name, comp_name, l_comp_pos, l_comp_len, comp_num,
     x     l_comp_ansys, l_comp_csub,
     x   bp_length, bp_num_i, bp_num_j,
     x   rl_length, rl_num,
     x   rtable, mptemp, mpval,
     x   l_csub_gid

#include "csubdata.inc"

c     number of warnings issued
      COMMON /ans2bmf_msg/ wcount
      INTEGER wcount

c     dataspace for feeding erhandler subroutine
      REAL     *        8  derrinfo(10)
      CHARACTER(LEN=PARMSIZE) :: cerrinfo(10)

c Local Variables:
c compile-command:"make -C .. test"
c End:
