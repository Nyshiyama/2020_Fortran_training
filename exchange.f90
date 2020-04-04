program main
    implicit none

    double precision left, right, tmp

    left=1d0
    right=-1d0

    print*, left, right

    tmp=left
    left=right
    right=tmp

    print*, left, right
end program main
