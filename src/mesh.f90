module mesh_mod

  implicit none

  type :: mesh_type
     integer :: nx, ny
     real, dimension(:), allocatable :: x, y
  end type mesh_type

  interface mesh_type
     procedure :: create_mesh
     procedure :: load_mesh
  end interface mesh_type

contains
  function create_mesh(x0, y0, dx, dy, nx, ny)
    type(mesh_type) :: create_mesh
    real, intent(in) :: x0, y0
    real, intent(in) :: dx, dy
    integer, intent(in) :: nx, ny

    integer :: i

    create_mesh%nx = nx
    create_mesh%ny = ny

    allocate(create_mesh%x(nx))
    allocate(create_mesh%y(ny))

    do i=1, nx
       create_mesh%x(i) = x0 + (i-1)*dx
    end do
    do i=1, ny
       create_mesh%y(i) = y0 + (i-1)*dy
    end do
  end function create_mesh

  function load_mesh(path)
    use, intrinsic :: iso_fortran_env, only: stderr => error_unit
    type(mesh_type) :: load_mesh
    character(len=*),  intent(in) :: path

    real :: x0, y0
    real :: dx, dy
    integer :: nx, ny
    integer :: error, funit
    logical :: exists

    namelist /MESH/ x0, y0, dx, dy, nx, ny

    ! Check whether file exists.
    inquire (file=path, exist=exists)

    if (.not.exists) then
       write(stderr, '("Error: input file ", a, " does not exist")') path
       stop 1
    end if

    open (action='read', file=path, iostat=error, newunit=funit)
    read (nml=MESH, iostat=error, unit=funit)

    if (error /= 0) then
       write(stderr, '("Error: cannot read input file ", a)') path
       stop 1
    end if

    load_mesh = create_mesh(x0, y0, dx, dy, nx, ny)
  end function load_mesh

end module mesh_mod
