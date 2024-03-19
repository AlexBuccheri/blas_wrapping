# Blas Interface Testing

* Produce F2008 interfaces for BLAS 1 and 2 routines, with consistency-checking
* Use assumed-rank arrays to reduce the number of overloads required

What I would like to do is something basic like:

```fortran
   subroutine copy_real64(x, y)
    real(real64), intent(in),  contiguous :: x(..)
    real(real64), intent(out), contiguous :: y(..)
    integer, allocatable :: shape_x(:), shape_y(:)
    integer :: i
    
    ! Call to blas routine with assumed-rank array
    call dcopy(size(x), x, 1, y, 1)

  end subroutine copy_real64
```

however the compiler complains that I require an explicit interface. Tests show that while GCC 10 and 11 allow:

```fortran
  interface
     subroutine dcopy(n, dx, incx, dy, incy)
       use, intrinsic :: iso_fortran_env
       integer, intent(in) :: n, incx, incy
       real(real64), intent(in)  :: dx(*)
       real(real64), intent(out) :: dy(*)
     end subroutine dcopy
  end interface
```

GCC 12 and above, aswell as ifx, require the interface to be consistent with the declarations of what I'm passing
to the actual call:

```fortran
  interface
     subroutine dcopy(n, dx, incx, dy, incy)
       use, intrinsic :: iso_fortran_env
       integer, intent(in) :: n, incx, incy
       real(real64), intent(in)  :: dx(..)
       real(real64), intent(out) :: dy(..)
     end subroutine dcopy
  end interface
```


