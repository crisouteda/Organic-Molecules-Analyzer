program fortran
    use mass
    interface
    end interface
    integer :: p
    character (len=100) :: filename
    double precision, dimension (:,:), allocatable :: coord
    character,dimension(:), allocatable :: atoms
    print*, "Write the name of the file haing the molecular coordinates in xyz format"
    read (*,*) filename
    open (unit = 11, file = filename, status = "old", action = "read")
    read (unit=11,fmt=*) p
    read (unit=11,fmt=*)
    allocate(atoms(p))
    allocate(coord(p,3))
    call counting(p,coord,atoms)
    close(unit=11)
    call  maxmin(coord,p,atoms)
    call  get_bonds(coord,p,atoms,1.20d0,1.30d0,"C","O","=")
    call  get_bonds(coord,p,atoms,0.94d0,1.05d0,"H","O","-")
end program fortran
