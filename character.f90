program main
    implicit none
    character last_name*20, first_name*20, full_name*60
    character birth_dc*2, birth_mc*2, birth_yc*4, birthday*8
    integer birth_d, birth_m, birth_y

    last_name ='Accounto'
    first_name='Ryota'

    birth_d=31
    birth_m=3
    birth_y=2020

    ! これは間違い
    full_name=last_name//' '//first_name
    print*, full_name ! Accounto             Ryota

    ! こちらが正解例
    full_name=trim(last_name)//' '//trim(first_name)
    print*, trim(full_name) ! Accounto Ryota

    write(birth_dc,'(i2.2)') birth_d
    write(birth_mc,'(i2.2)') birth_m
    write(birth_yc,'(i4.4)') birth_y

    birthday=birth_yc//birth_mc//birth_dc
    print*, birthday ! 20200331
end program main
