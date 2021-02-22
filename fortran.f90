module mass
    implicit none
        integer, parameter :: C = 12 , H = 1 , O = 16 , S = 32, N = 15
    end module mass

subroutine counting(p,coord,atoms)
    implicit none
    integer :: p, er, i, nc, nn, nh, no, mmass, molarmass
    double precision :: coord(p,3)
    character :: atoms(p)
    nc = 0
    nn = 0
    nh = 0
    no = 0
    er = 0
    do i = 1,p 
        read(unit=11,fmt=*,iostat = er) atoms(i), coord(i,1), coord(i,2), coord(i,3)
        if (er.eq.0) then
        if (atoms(i) == "C") then
           nc = nc + 1
        else if (atoms(i) == 'N') then
           nn = nn + 1
        else if (atoms(i) == 'H') then
           nh = nh + 1
        else if (atoms(i) == 'O') then
           no = no + 1
        else
            print*, "Sorry, I'm pretty useless like my programmer and don't know any atom out of {H,C,N,O}"
        stop
        endif
    endif
    end do
    mmass=molarmass(nh,nc,nn,no)
    print*,"Pretty heavy bb: ", mmass
end subroutine

function molarmass(nh,nc,nn,no)
    use mass
    implicit none
    integer :: molarmass, no, nh, nn, nc
    molarmass = no * O + nh * H + nn * N + nc * C
end function molarmass

subroutine maxmin(coord,p,atoms)
    implicit none
    integer :: e, d, min_e, min_d, max_e, max_d, p,minh_e,minh_d, maxh_e, maxh_d
    double precision :: dist, mindist, maxdist, mindisth, maxdisth, disth
    double precision :: coord(p,3)
    character :: atoms(p)
    mindist=100
    mindisth=100
    maxdist=0
    maxdisth=0
    do e = 1, p-1
        do d = e +1, p
            dist=(((coord(e,1)-coord(d,1))**2.d0)+((coord(e,2)-coord(d,2))**2.d0)+((coord(e,3)-coord(d,3))**2.d0))**0.5d0
            if (dist.lt.mindist) then 
                mindist=dist
                min_e = e
                min_d = d
            else if (dist.gt.maxdist) then
                maxdist=dist
                max_e = e
                max_d = d
            endif
            if (atoms(e).ne."H".and.atoms(d).ne."H") then
                disth=(((coord(e,1)-coord(d,1))**2.d0)+((coord(e,2)-coord(d,2))**2.d0)+((coord(e,3)-coord(d,3))**2.d0))**0.5d0
                if (disth.lt.mindisth) then 
                    mindisth=disth
                    minh_e = e
                    minh_d = d
                else if (disth.gt.maxdisth) then
                    maxdisth=disth
                    maxh_e = e
                    maxh_d = d
                endif
            end if
        enddo
    enddo
    print*, 'The minimum distance is ', mindist, 'between atoms ',  atoms(min_e),' and ', atoms(min_d)
    print*, 'Correspondin with the distances ', min_e,' and ', min_d
    print*, 'The maximum distance is ', maxdist, 'between atoms ', atoms(max_e),  ' and ', atoms(max_d)
    print*, 'Correspondin with the distances ', max_e,' and ', max_d
    print*, 'The minimum heavy distance is ', mindisth, 'between atoms ', atoms(minh_e), ' and ', atoms(minh_d)
    print*, 'Correspondin with the distances ', minh_e,' and ', minh_d
    print*, 'The maximum heavy distance is ', maxdisth, 'between atoms ', atoms(maxh_e), ' and ', atoms(maxh_d)
    print*, 'Correspondin with the distances ', maxh_e,' and ', maxh_d
end subroutine maxmin

subroutine get_bonds(coord,p,atoms,low,high,atom1,atom2,bondtype)
    implicit none
    integer :: e , d , bonds , p
    double precision :: dist, mindist, maxdist,low,high
    double precision :: coord(p,3)
    character :: atoms(p)
    character(len=1) :: atom1,atom2,bondtype
    mindist=100
    maxdist=0
    bonds=0
    do e = 1, p -1
        do d = e + 1, p
            if (atoms(e).eq.atom1.and.atoms(d).eq.atom2.or.atoms(e).eq.atom2.and.atoms(d).eq.atom1) then
            dist=(((coord(e,1)-coord(d,1))**2.d0)+((coord(e,2)-coord(d,2))**2.d0)+((coord(e,3)-coord(d,3))**2.d0))**0.5d0 
            if (dist.le.high.and.dist.ge.low) then
                bonds=bonds+1
            endif
        end if
        enddo
    enddo
    print*, 'The number of ',atom1,bondtype,atom2, ' bonds is ', bonds
end subroutine get_bonds

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