program main
    implicit none
    double precision, parameter :: xmax=1d0, ymax=1d0   ! 解析領域の大きさ
    double precision, parameter :: delta_t=1d-4  ! 時間刻み
    integer         , parameter :: nx=31, ny=21  ! 格子点数
    integer         , parameter :: tmax=1000 ! タイムステップ数
    integer         , parameter :: period=10 ! 解析結果の出力周期

    double precision, dimension(nx,ny) :: u, u_new
    logical         , dimension(nx,ny) :: obs
    integer         , dimension(nx,ny) :: Iobs ! ParaView
    double precision delta_x, delta_y ! 格子幅
    double precision rx, ry
    integer i, j
    integer xp, xm, yp, ym
    integer t

    delta_x=xmax/dble(nx-1)
    delta_y=ymax/dble(ny-1)

    rx=delta_t/delta_x**2 ! 陽解法式参照
    ry=delta_t/delta_y**2 ! 陽解法式参照
    print*, 'rx=',rx,'ry=',ry ! rx=   9.0000000000000011E-002 ry=   3.9999999999999994E-002

    ! 障害物の設定
    obs(:,:)=.true.
    Iobs(:,:)=1 ! 可視化用
    do j=2,ny-1
        do i=2,nx-1
            obs(i,j)=.false.
            Iobs(i,j)=0
        enddo
    enddo

    ! 障害物の出力（t=0）
    call Output_for_ParaView(0,'obs',real(Iobs),nx,ny)

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

    ! 初期場の出力（t=0）
    call Output_for_ParaView(0,'u',real(u_new),nx,ny)

    ! 反復計算開始
    do t=1,tmax
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

        ! 時刻の更新
        u(:,:)=u_new(:,:)

        ! 解析結果の出力
        if(mod(t,period)==0) call Output_for_ParaView(t,'u',real(u),nx,ny)
    enddo
end program main

subroutine Output_for_ParaView(t,filename,array,nx,ny)
    implicit none
    real, dimension(nx,ny), intent(in) :: array
    integer, intent(in) :: nx, ny
    integer, intent(in) :: t
    character(*), intent(in) :: filename ! 文字列長は引数依存
    integer i, j
    character ct*8, cnx*8, cny*8, cnz*8, cntot*16, cdx*12, cdy*12, buffer*80

    write(ct,'(i8.8)') t ! 00000000-99999999
    write(cnx,'(i8)') nx
    write(cny,'(i8)') ny
    write(cnz,'(i8)') 1
    write(cntot,'(i16)') nx*ny
    write(cdx,'(f12.10)') delta_x
    write(cdy,'(f12.10)') delta_y

    ! ParaViewフォーマット
    open(20, file=filename//ct//'.vtk', form='unformatted', access='stream', status='unknown', convert='BIG_ENDIAN')
    buffer = '# vtk DataFile Version 3.0'//char(10)   ; write(20) trim(buffer)
    buffer = 'contour.vtk'//char(10)                  ; write(20) trim(buffer)
    buffer = 'BINARY'//char(10)                       ; write(20) trim(buffer)
    buffer = 'DATASET STRUCTURED_POINTS'//char(10)    ; write(20) trim(buffer)
    buffer = 'DIMENSIONS '//cnx//cny//cnz//char(10)   ; write(20) trim(buffer)
    buffer = 'ORIGIN 0.0 0.0 0.0'//char(10)           ; write(20) trim(buffer)
    buffer = 'SPACING '//cdx//cdy//' 1.0'//char(10)   ; write(20) trim(buffer)
    buffer = 'POINT_DATA'//cntot//char(10)            ; write(20) trim(buffer)
    buffer = 'SCALARS '//filename//' float'//char(10) ; write(20) trim(buffer)
    buffer = 'LOOKUP_TABLE default'//char(10)         ; write(20) trim(buffer)
    write(20) ((array(i,j),i=1,nx),j=1,ny)
    close(20)
end subroutine Output_for_ParaView
