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
