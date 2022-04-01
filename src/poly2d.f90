module poly2d
  implicit none

  type :: poly2d_type
     real :: a
     real :: b
     real :: c
     real :: d
     real :: e
     real :: f
   contains
     procedure :: compute => polynomial2d
  end type poly2d_type

  interface poly2d_type
     procedure :: load_polynomial2d
  end interface poly2d_type

  private :: polynomial2d

contains
  elemental function polynomial2d(this, x, y)
    real :: polynomial2d
    class(poly2d_type), intent(in) :: this
    real, intent(in) :: x
    real, intent(in) :: y

    polynomial2d = this%a*x**2 + this%b*y**2 + this%c*x*y + &
         this%d*x + this%e*y + this%f
  end function polynomial2d

  function load_polynomial2d(path)
    use, intrinsic :: iso_fortran_env, only: stderr => error_unit
    type(poly2d_type) :: load_polynomial2d
    character(len=*),  intent(in), optional  :: path
    real :: a, b, c, d, e, f
    integer :: error, funit

    namelist /POLYNOMIAL/ a, b, c, d, e, f

    ! defaults
    a = 0.
    b = 0.
    c = 0.
    d = 1.
    e = 1.
    f = 0.

    if (present(path)) then
       ! Check whether file exists.
       inquire (file=path, iostat=error)

       if (error /= 0) then
          write (stderr, '("Error: input file ", a, " does not exist")') path
          stop 1
       end if

       open (action='read', file=path, iostat=error, newunit=funit)
       read (nml=POLYNOMIAL, iostat=error, unit=funit)
    end if

    load_polynomial2d = poly2d_type(a, b, c, d, e, f)
  end function load_polynomial2d

end module poly2d
