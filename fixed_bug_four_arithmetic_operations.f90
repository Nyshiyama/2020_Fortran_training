program main
    implicit none ! ←追加
    double precision delta_x, delta_y ! 格子幅
    double precision delta_t  ! 時間刻み
    double precision kx, ky ! ←追加
    double precision u, u_new
    double precision uxp, uxm, uyp, uym

  ! パラメータ
  delta_x=0.2d0
  delta_y=0.3d0
  delta_t=1d-4

    ! 初期値
    u  =1d0 ! ←追加
    uxp=2d0
    uxm=2d0
    uyp=2d0
    uym=2d0

    kx=delta_t/delta_x**2 ! 陽解法式参照
    ky=delta_t/delta_y**2 ! 陽解法式参照

    print*, 'kx, ky', kx, ky

    u_new=kx*(uxp+uxm)+ky*(uyp+uym)+(1d0-2d0*(kx+ky))*u ! 陽解法式参照

print*, 'u_new', u_new ! 本来は u_new   1.0072222222222222

end program main
