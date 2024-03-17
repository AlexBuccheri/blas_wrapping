! To compile on linux: gfortran blasing.f90 -lblas -llapack -o bl
! To compile on mac:   gfortran -framework Accelerate blasing.f90 -o bl
!
! Other compiler errors (not bugs) to note:
!  1. Assumed-rank variable x at (1) shall not have a subobject reference
!     call dcopy(size(x), x(1), 1, y(1), 1)
module blas_copy
  use, intrinsic :: iso_fortran_env
  implicit none
  private
  public :: copy

  interface copy
    module procedure copy_real64
  end interface copy

  ! Interface for BLAS side routine
  ! On linux with gfortran 10.2.1 dx(*) and dy(*) worked.
  ! Note, this means I cannot use the contiguous flag in the dcopy interface
  ! On Mac with gfortran 13.2.0, the compiler complains
  interface
     subroutine dcopy(n, dx, incx, dy, incy)
       use, intrinsic :: iso_fortran_env
       integer, intent(in) :: n, incx, incy
       real(real64), intent(in)  :: dx(*)
       real(real64), intent(out) :: dy(*)
     end subroutine dcopy
  end interface

contains

  subroutine copy_consistency_check_real64(x, y)
    real(real64), intent(in), contiguous :: x(..)
    real(real64), intent(in), contiguous :: y(..)
    integer, allocatable :: shape_x(:), shape_y(:)
    integer :: i

    shape_x = shape(x)
    shape_y = shape(y)

    ! Arrays must have the same number of dimensions
    if (size(shape_x) /= size(shape_y)) then
       write(*, *) 'Target array dims differs to input array dims'
       error stop 101
    endif

    ! Each dimension should be consistent
    do i = 1, size(shape_x)
       if (shape_x(i) /= shape_y(i)) then
          write(*, *) 'Size of dimension ', i , 'differs between input and outputs'
          error stop 101
       endif
    end do

  end subroutine copy_consistency_check_real64

   subroutine copy_real64(x, y)
    real(real64), intent(in),  contiguous :: x(..)
    real(real64), intent(out), contiguous :: y(..)
    integer, allocatable :: shape_x(:), shape_y(:)
    integer :: i

    shape_x = shape(x)
    shape_y = shape(y)

    ! Arrays must have the same number of dimensions
    if (size(shape_x) /= size(shape_y)) then
       write(*, *) 'Target array dims differs to input array dims'
       error stop 101
    endif

    ! Each dimension should be consistent
    do i = 1, size(shape_x)
       if (shape_x(i) /= shape_y(i)) then
          write(*, *) 'Size of dimension ', i , 'differs between input and outputs'
          error stop 101
       endif
    end do

    ! Abstraction of these checks leads to a compiler bug for GCC 10 and 13
    !call copy_consistency_check_real64(x, y)
    call dcopy(size(x), x, 1, y, 1)

  end subroutine copy_real64

end module blas_copy


program blasing
  use, intrinsic :: iso_fortran_env
  use blas_copy, only: copy
  implicit none
  real(real64) :: x_1d(5), y_1d(5)
  real(real64) :: x_2d(3, 3), y_2d(3, 3)

  x_2d = 2._real64
  call copy(x_2d, y_2d)
  write(*, *) y_2d

end program blasing
