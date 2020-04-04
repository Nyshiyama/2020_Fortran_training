program main
    implicit none
    double precision, parameter :: delta_x=0.2d0, delta_y=0.3d0 ! 格子幅
    double precision, parameter :: delta_t=1d-4  ! 時間刻み
    double precision rx, ry
    double precision u, u_new
    double precision uxp, uxm, uyp, uym

    ! 初期値
    u=1d0
    print*, 'uxp, uxm, uyp, uym'
    read*, uxp, uxm, uyp, uym

    rx=delta_t/delta_x**2 ! 陽解法式参照
    ry=delta_t/delta_y**2 ! 陽解法式参照

    open(10,file='open_ex.txt',form='formatted',status='unknown')
    write(10,*) 'rx, ry', rx, ry ! rx, ry   2.4999999999999996E-003   1.1111111111111111E-003
    close(10)

    u_new=rx*(uxp+uxm)+ry*(uyp+uym)+(1d0-2d0*(rx+ry))*u ! 陽解法式参照

    open(10,file='open_ex.txt',form='formatted',status='unknown',position='append')
    write(10,*) 'u_new', u_new ! u_new   1.0072222222222222
    close(10)

end program main
