program main
    implicit none
    double precision delta_x, delta_y ! 格子幅
    double precision delta_t  ! 時間刻み
    double precision rx, ry
    double precision u, u_new
    double precision uxp, uxm, uyp, uym

    ! パラメータ
    delta_x=0.2d0
    delta_y=0.3d0
    delta_t=1d-4

    ! 初期値
    u  =1d0
    uxp=2d0
    uxm=2d0
    uyp=2d0
    uym=2d0

    rx=delta_t/delta_x**2 ! 陽解法式参照
    ry=delta_t/delta_y**2 ! 陽解法式参照

    print*, 'rx, ry', rx, ry ! rx, ry   2.4999999999999996E-003   1.1111111111111111E-003

    u_new=rx*(uxp+uxm)+ry*(uyp+uym)+(1d0-2d0*(rx+ry))*u ! 陽解法式参照

    print*, 'u_new', u_new ! u_new   1.0072222222222222

end program main
