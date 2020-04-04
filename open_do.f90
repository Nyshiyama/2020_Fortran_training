program main
    implicit none

    integer a(3,3)
    integer i, j

    do j=1,3
        do i=1,3
            a(i,j)=10*i+j
        enddo
    enddo

    open(10,file='opendo_ex.txt',form='formatted',status='unknown')
    ! コロン型
    write(10,*) a(:,:)
    write(10,*) char(10) ! 文字列長1の改行文字コード（何もwriteしないとしてwrite(10,*)のみでもよい）

    ! 1行do文型
    write(10,*) ((a(i,j),i=1,3),j=1,3)
    write(10,*) char(10) ! 文字列長1の改行文字コード（何もwriteしないとしてwrite(10,*)のみでもよい）

    ! do文型
    do j=1,3
        do i=1,3
            write(10,*) a(i,j)
        enddo
    enddo
    close(10)
end program main
