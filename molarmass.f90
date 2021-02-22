function molarmass(nh,nc,nn,no)
    use mass
    implicit none
    integer :: molarmass, no, nh, nn, nc
    molarmass = no * O + nh * H + nn * N + nc * C
end function molarmass
