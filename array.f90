program main
    implicit none
    double precision, parameter :: xmax=1d0, ymax=1d0   ! 解析領域の大きさ
    double precision, parameter :: delta_t=1d-4  ! 時間刻み
    integer         , parameter :: nx=31, ny=21  ! 格子点数
    double precision delta_x, delta_y ! 格子幅
    double precision rx, ry
    double precision, dimension(nx,ny) :: u, u_new
    integer i, j
    integer xp, xm, yp, ym

    ! 初期値
    u(:,:)=1d0

    delta_x=xmax/dble(nx-1)
    delta_y=ymax/dble(ny-1)

    rx=delta_t/delta_x**2 ! 陽解法式参照
    ry=delta_t/delta_y**2 ! 陽解法式参照
    print*, 'rx=',rx,'ry=',ry ! rx=   9.0000000000000011E-002 ry=   3.9999999999999994E-002

    i=2
    j=2
    xp=i+1
    xm=i-1
    yp=j+1
    ym=j-1
    u_new(i,j)=rx*(u(xp,j)+u(xm,j))+ry*(u(i,yp)+u(i,ym))+(1d0-2d0*(rx+ry))*u(i,j) ! 陽解法式参照

    open(10,file='open_ex.txt',form='formatted',status='unknown')
    write(10,*) 'u_new', u_new(:,:) !(2,2)成分のみの計算なのでそれ以外にゼロが入っている保証はない→初期化問題
    close(10)

end program main
