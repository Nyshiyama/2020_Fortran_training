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
    logical, dimension(nx,ny) :: obs

    delta_x=xmax/dble(nx-1)
    delta_y=ymax/dble(ny-1)

    rx=delta_t/delta_x**2 ! 陽解法式参照
    ry=delta_t/delta_y**2 ! 陽解法式参照
    print*, 'rx=',rx,'ry=',ry ! rx=   9.0000000000000011E-002 ry=   3.9999999999999994E-002

    ! 障害物の設定
    obs(:,:)=.true.
    do j=2,ny-1
        do i=2,nx-1
            obs(i,j)=.false.
        enddo
    enddo

    ! uの初期条件
    do j=1,ny
        do i=1,nx
            if(obs(i,j))then
                u(i,j)=0d0
            else
                u(i,j)=1d0
            endif
        enddo
    enddo

    do j=1,ny
        do i=1,nx
            if(.not. obs(i,j))then
                xp=mod(i,nx)+1
                xm=nx-mod(nx+1-i,nx)
                yp=mod(j,ny)+1
                ym=ny-mod(ny+1-j,ny)

                u_new(i,j)=rx*(u(xp,j)+u(xm,j))+ry*(u(i,yp)+u(i,ym))+(1d0-2d0*(rx+ry))*u(i,j) ! 陽解法式参照
            endif
        enddo
    enddo

    open(10,file='open_ex.txt',form='formatted',status='unknown')
    write(10,*) nx, ny, 'u_new' ! ヘッダー：ファイル冒頭に格子点数等を記録しておくと参照するときに便利
    write(10,*) u_new(:,:) ! 先頭の文字列は参照するときに邪魔なので削除（printのときは出力値を明確化するために付けていた）
    close(10)

end program main
