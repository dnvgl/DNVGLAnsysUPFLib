! Copyright (C) 2014 by DNV GL SE

! Task      Misc math tasks for FORTRAN

! Author    Berthold Höllmann <berthold.hoellmann@dnvgl.com>



! ID: $Id$

MODULE dnvgl_math

  IMPLICIT NONE

  REAL(KIND=8), PARAMETER :: pi = atan(1d0) * 4d0
  REAL(KIND=8), PARAMETER :: deg2rad_val = pi / 1.8d2

  PRIVATE :: deg2rad_val

CONTAINS

  FUNCTION cross(a, b)

    IMPLICIT NONE

    REAL(KIND=8), DIMENSION(3) :: cross
    REAL(KIND=8), DIMENSION(3), INTENT(IN) :: a, b

    cross(1) = a(2) * b(3) - a(3) * b(2)
    cross(2) = a(3) * b(1) - a(1) * b(3)
    cross(3) = a(1) * b(2) - a(2) * b(1)
  END FUNCTION cross

  FUNCTION dot(mat, a)

    IMPLICIT NONE

    REAL(KIND=8), DIMENSION(3) :: dot
    REAL(KIND=8), DIMENSION(3, 3), INTENT(IN) :: mat
    REAL(KIND=8), DIMENSION(3), INTENT(IN) :: a

    dot(:) = mat(:, 1) * a(1) + mat(:, 2) * a(2) + mat(:, 3) * a(3)
  END FUNCTION dot

  FUNCTION deg2rad(a)

    IMPLICIT NONE

    REAL(KIND=8) :: deg2rad
    REAL(KIND=8), INTENT(IN) :: a

    deg2rad = a * deg2rad_val

  END FUNCTION deg2rad

END MODULE dnvgl_math

! Local Variables:
! mode: f90
! compile-command: "make"
! End:
