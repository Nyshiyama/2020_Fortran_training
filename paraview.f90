program main
    implicit none
    double precision, parameter :: xmax=1d0, ymax=1d0   ! 解析領域の大きさ
    double precision, parameter :: delta_t=1d-4  ! 時間刻み
    integer         , parameter :: nx=31, ny=21  ! 格子点数
    double precision delta_x, delta_y ! 格子幅
    double precision rx, ry
    double precision, dimension(nx,ny) :: u, u_new
    logical         , dimension(nx,ny) :: obs
    integer         , dimension(nx,ny) :: Iobs ! ParaView
    integer i, j
    integer xp, xm, yp, ym
    character cnx*8, cny*8, cnz*8, cntot*16, cdx*12, cdy*12, buffer*80 ! ParaView

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

    write(cnx,'(i8)') nx
    write(cny,'(i8)') ny
    write(cnz,'(i8)') 1
    write(cntot,'(i16)') nx*ny
    write(cdx,'(f12.10)') delta_x
    write(cdy,'(f12.10)') delta_y

    ! ParaViewフォーマット
    open(20, file='obs.vtk', form='unformatted', access='stream', status='unknown', convert='BIG_ENDIAN')
    buffer = '# vtk DataFile Version 3.0'//char(10) ; write(20) trim(buffer)
    buffer = 'contour.vtk'//char(10)                ; write(20) trim(buffer)
    buffer = 'BINARY'//char(10)                     ; write(20) trim(buffer)
    buffer = 'DATASET STRUCTURED_POINTS'//char(10)  ; write(20) trim(buffer)
    buffer = 'DIMENSIONS '//cnx//cny//cnz//char(10) ; write(20) trim(buffer)
    buffer = 'ORIGIN 0.0 0.0 0.0'//char(10)         ; write(20) trim(buffer)
    buffer = 'SPACING '//cdx//cdy//' 1.0'//char(10) ; write(20) trim(buffer)
    buffer = 'POINT_DATA'//cntot//char(10)          ; write(20) trim(buffer)
    buffer = 'SCALARS obs int'//char(10)            ; write(20) trim(buffer)
    buffer = 'LOOKUP_TABLE default'//char(10)       ; write(20) trim(buffer)
    write(20) ((Iobs(i,j),i=1,nx),j=1,ny)
    close(20)

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

    ! ParaViewフォーマット
    open(20, file='u.vtk', form='unformatted', access='stream', status='unknown', convert='BIG_ENDIAN')
    buffer = '# vtk DataFile Version 3.0'//char(10) ; write(20) trim(buffer)
    buffer = 'contour.vtk'//char(10)                ; write(20) trim(buffer)
    buffer = 'BINARY'//char(10)                     ; write(20) trim(buffer)
    buffer = 'DATASET STRUCTURED_POINTS'//char(10)  ; write(20) trim(buffer)
    buffer = 'DIMENSIONS '//cnx//cny//cnz//char(10) ; write(20) trim(buffer)
    buffer = 'ORIGIN 0.0 0.0 0.0'//char(10)         ; write(20) trim(buffer)
    buffer = 'SPACING '//cdx//cdy//' 1.0'//char(10) ; write(20) trim(buffer)
    buffer = 'POINT_DATA'//cntot//char(10)          ; write(20) trim(buffer)
    buffer = 'SCALARS u float'//char(10)            ; write(20) trim(buffer)
    buffer = 'LOOKUP_TABLE default'//char(10)       ; write(20) trim(buffer)
    write(20) ((real(u_new(i,j)),i=1,nx),j=1,ny)
    close(20)

end program main
