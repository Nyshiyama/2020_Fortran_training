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

    do j=1,ny
        do i=1,nx
            xp=i+1
            if(i==nx) xp=1
            xm=i-1
            if(i==1) xm=nx
            yp=j+1
            if(j==ny) yp=1
            ym=j-1
            if(j==1) ym=ny
            ! if文を使わずにxp等は記述できる（ヒント：mod関数，答えは最初のサンプルコード）

            u_new(i,j)=rx*(u(xp,j)+u(xm,j))+ry*(u(i,yp)+u(i,ym))+(1d0-2d0*(rx+ry))*u(i,j) ! 陽解法式参照
        enddo
    enddo

    open(10,file='open_ex.txt',form='formatted',status='unknown')
    write(10,*) nx, ny, 'u_new' ! ヘッダー：ファイル冒頭に格子点数等を記録しておくと参照するときに便利
    write(10,*) u_new(:,:) ! 先頭の文字列は参照するときに邪魔なので削除（printのときは出力値を明確化するために付けていた）
    close(10)

end program main
