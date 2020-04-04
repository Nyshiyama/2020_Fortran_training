program main
    implicit none
    integer, parameter :: nx=30, ny=20  ! 格子点数
    integer, parameter :: tmax=1000 ! タイムステップ数
    integer, parameter :: nmax=100  ! ヤコビ反復法による収束を諦める反復回数
    integer, parameter :: period=10 ! 解析結果の出力周期
    double precision, parameter :: delta_x=0.2d0, delta_y=0.3d0 ! 格子幅
    double precision, parameter :: delta_t=0.001d0  ! 時間刻み
    integer i, j
    integer xp, xm, yp, ym
    integer t
    integer n
    double precision rx, ry
    double precision, dimension(nx,ny) :: u, u_new
    double precision, dimension(nx,ny) :: c
    logical, dimension(nx,ny) :: obs

    rx=delta_t/delta_x**2
    ry=delta_t/delta_y**2

    ! 障害物の設定
    obs(:,:)=.true.
    do j=2,ny-1
    do i=2,nx-1
        obs(i,j)=.false.
    enddo
    enddo

    ! 障害物の出力
    call Output_obs(obs,nx,ny)

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

    ! 初期場の出力
    call Output_u(0,u,nx,ny)

    ! 反復計算開始
    do t=1,tmax
        ! クランク・ニコルソン法(ヤコビ反復法による解法)
        do j=1,ny
        do i=1,nx
            xp=mod(i,nx)+1
            xm=nx-mod(nx+1-i,nx)
            yp=mod(j,ny)+1
            ym=ny-mod(ny+1-j,ny)

            c(i,j)=(1d0-rx-ry)*u(i ,j ) &
                   +0.5d0*rx*(u(xp,j )+u(xm,j )) &
                   +0.5d0*ry*(u(i ,yp)+u(i ,ym))
        enddo
        enddo

        do n=1,nmax
            do j=1,ny
            do i=1,nx
                if(.not. obs(i,j))then
                    xp=mod(i,nx)+1
                    xm=nx-mod(nx+1-i,nx)
                    yp=mod(j,ny)+1
                    ym=ny-mod(ny+1-j,ny)

                    u_new(i,j)=-(rx+ry)*u(i ,j ) &
                               +0.5d0*rx*(u(xp,j )+u(xm,j )) &
                               +0.5d0*ry*(u(i ,yp)+u(i ,ym)) &
                               +c(i,j)
                endif
            enddo
            enddo

            if(maxval(abs(u_new(:,:)-u(:,:)))<1d-8)then
                write(*,*) t, n
                u(:,:)=u_new(:,:)
                exit
            else
                u(:,:)=u_new(:,:)
            endif
        enddo

        ! 解析結果の出力
        if(mod(t,period)==0) call Output_u(t,u,nx,ny)
    enddo
end program main


! 障害物を出力するサブルーチン
subroutine Output_obs(obs,nx,ny)
    implicit none
    integer nx, ny
    integer i, j
    logical, dimension(nx,ny) :: obs
    integer, dimension(nx,ny) :: Iobs
    character cnx*8, cny*8, cnz*8, cntot*16, buffer*80

    ! 障害物データを整数型に変換
    do j=1,ny
    do i=1,nx
        if(obs(i,j))then
            Iobs(i,j)=1
        else
            Iobs(i,j)=0
        endif
    enddo
    enddo

    write(cnx,'(I8)') nx
    write(cny,'(I8)') ny
    write(cnz,'(I8)') 1
    write(cntot,'(I16)') nx*ny

    ! Paraviewフォーマット
    open(20, file='obs.vtk', form='unformatted', access='stream', status='unknown', position='append', convert='BIG_ENDIAN')
    buffer = '# vtk DataFile Version 3.0'//char(10) ; write(20) trim(buffer)
    buffer = 'contour.vtk'//char(10)                ; write(20) trim(buffer)
    buffer = 'BINARY'//char(10)                     ; write(20) trim(buffer)
    buffer = 'DATASET STRUCTURED_POINTS'//char(10)  ; write(20) trim(buffer)
    buffer = 'DIMENSIONS '//cnx//cny//cnz//char(10) ; write(20) trim(buffer)
    buffer = 'ORIGIN 1.0 1.0 1.0'//char(10)         ; write(20) trim(buffer)
    buffer = 'SPACING 1.0 1.0 1.0'//char(10)        ; write(20) trim(buffer)
    buffer = 'POINT_DATA'//cntot//char(10)          ; write(20) trim(buffer)
    buffer = 'SCALARS obs int'//char(10)            ; write(20) trim(buffer)
    buffer = 'LOOKUP_TABLE default'//char(10)       ; write(20) trim(buffer)
    write(20) ((Iobs(i,j),i=1,nx),j=1,ny)
    close(20)
end subroutine Output_obs


! 解析結果を出力するサブルーチン
subroutine Output_u(t,u,nx,ny)
    implicit none
    integer nx, ny
    integer i, j
    integer t
    double precision, dimension(nx,ny) :: u
    character ct*8, cnx*8, cny*8, cnz*8, cntot*16, buffer*80

    write(ct,'(I8.8)') t
    write(cnx,'(I8)') nx
    write(cny,'(I8)') ny
    write(cnz,'(I8)') 1
    write(cntot,'(I16)') nx*ny

    ! Paraviewフォーマット
    open(20, file='u-'//ct//'.vtk', form='unformatted', access='stream', status='unknown', position='append', convert='BIG_ENDIAN')
    buffer = '# vtk DataFile Version 3.0'//char(10) ; write(20) trim(buffer)
    buffer = 'contour.vtk'//char(10)                ; write(20) trim(buffer)
    buffer = 'BINARY'//char(10)                     ; write(20) trim(buffer)
    buffer = 'DATASET STRUCTURED_POINTS'//char(10)  ; write(20) trim(buffer)
    buffer = 'DIMENSIONS '//cnx//cny//cnz//char(10) ; write(20) trim(buffer)
    buffer = 'ORIGIN 1.0 1.0 1.0'//char(10)         ; write(20) trim(buffer)
    buffer = 'SPACING 1.0 1.0 1.0'//char(10)        ; write(20) trim(buffer)
    buffer = 'POINT_DATA'//cntot//char(10)          ; write(20) trim(buffer)
    buffer = 'SCALARS u float'//char(10)            ; write(20) trim(buffer)
    buffer = 'LOOKUP_TABLE default'//char(10)       ; write(20) trim(buffer)
    write(20) ((real(u(i,j)),i=1,nx),j=1,ny)
    close(20)
end subroutine Output_u
