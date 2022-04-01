program dummy
  use, intrinsic :: iso_fortran_env, only: stderr => error_unit
  use poly2d
  use mesh_mod
  use netcdf
  implicit none
  type(poly2d_type) :: polynomial
  type(mesh_type) :: mesh
  character(:), allocatable :: config, outname
  integer arglen, stat
  integer i,j
  real, dimension(:,:), allocatable :: z
  integer ncid
  integer dimid_x, dimid_y
  integer varid_x, varid_y, varid_z

  call get_command_argument(number=1, length=arglen, status=stat)
  if (stat /= 0) then
     write (stderr, *) 'Error, no configuration file specified'
     stop 1
  end if
  allocate (character(arglen) :: config)
  call get_command_argument(number=1, value=config)

  polynomial = poly2d_type()

  write(*,*) polynomial%compute(0., 0.)

  mesh = mesh_type(config)

  allocate(z(mesh%nx,mesh%ny))

  do j=1,mesh%ny
     do i=1,mesh%nx
        z(i,j) = polynomial%compute(mesh%x(i), mesh%y(j))
     end do
  end do

  call get_command_argument(number=2, length=arglen, status=stat)
  if (stat /= 0) then
     write(*,*) z
  else
     allocate (character(arglen) :: outname)
     call get_command_argument(number=2, value=outname)

     call checknc(nf90_create(outname, NF90_CLOBBER, ncid))

     call checknc(nf90_def_dim(ncid, 'x', mesh%nx, dimid_x))
     call checknc(nf90_def_dim(ncid, 'y', mesh%ny, dimid_y))

     call checknc(nf90_def_var(ncid, 'x', NF90_REAL, [ dimid_x ], varid_x))
     call checknc(nf90_def_var(ncid, 'y', NF90_REAL, [ dimid_y ], varid_y))
     call checknc(nf90_def_var(ncid, 'z', NF90_REAL, [ dimid_x, dimid_y ], varid_z))

     call checknc(nf90_enddef(ncid))

     call checknc(nf90_put_var(ncid, varid_x, mesh%x))
     call checknc(nf90_put_var(ncid, varid_y, mesh%y))
     call checknc(nf90_put_var(ncid, varid_z, z))

     call checknc(nf90_close(ncid))
  end if


contains
  subroutine checknc(stat)
    integer, intent(in) :: stat

    if (stat /= NF90_NOERR) then
       write (stderr, *) trim(nf90_strerror(stat))
       stop
    end if
  end subroutine checknc

end program dummy
