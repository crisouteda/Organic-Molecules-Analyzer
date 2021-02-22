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
