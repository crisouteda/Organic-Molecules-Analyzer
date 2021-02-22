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
    print*, 'Corresponding with the index ', min_e,' and ', min_d
    print*, 'The maximum distance is ', maxdist, 'between atoms ', atoms(max_e),  ' and ', atoms(max_d)
    print*, 'Corresponding with the index ', max_e,' and ', max_d
    print*, 'The minimum heavy distance is ', mindisth, 'between atoms ', atoms(minh_e), ' and ', atoms(minh_d)
    print*, 'Corresponding with the index ', minh_e,' and ', minh_d
    print*, 'The maximum heavy distance is ', maxdisth, 'between atoms ', atoms(maxh_e), ' and ', atoms(maxh_d)
    print*, 'Corresponding with the index ', maxh_e,' and ', maxh_d
end subroutine maxmin
