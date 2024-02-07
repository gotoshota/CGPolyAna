module coord_convert
    use global_types
    implicit none

contains
    function center_of_mass(traj) result(com)
        implicit none

        type(trajectory), INTENT(IN) :: traj
        real, allocatable :: com(:,:,:)

        ! local variables
        INTEGER :: i, j, k
        INTEGER :: shift_id
        DOUBLE PRECISION :: sum_coords(3)

        if (traj%nbeads == 0)then
            print *, "Input Nbeads, the chain length."
            stop
        else
            ALLOCATE(com(3, traj%nchains, traj%nframes))

            do i = 1, traj%nframes
                do j = 1, traj%nchains
                    shift_id = (j-1)*traj%nbeads
                    sum_coords = 0.0d0
                    do k = 1, traj%nbeads
                        sum_coords(:) = sum_coords(:) + traj%coords(:, shift_id+k, i)
                    enddo
                    com(:, j, i) = sum_coords(:) / traj%nbeads
                enddo
            enddo
        endif
    end function center_of_mass

    function wrap_coords(traj) result(wrapped_coords)
        implicit none 

        type(trajectory), intent(in) :: traj
        real, allocatable :: wrapped_coords(:,:,:)

        integer :: i, j, frame
        real :: box_size(3)

        allocate(wrapped_coords, mold=traj%coords)

        do frame = 1, traj%nframes
            ! 立方格子の場合、全ての方向で箱のサイズは同じです。
            if (traj%is_cubic) then
                box_size = [traj%box_dim(1, frame), traj%box_dim(2, frame), traj%box_dim(3, frame)]
            else
                print *, "Error: This program support only a cubic simulation cell."
            end if

            do i = 1, traj%nparticles
                do j = 1, 3 ! x, y, z座標
                    ! 座標を箱のサイズでラップします。
                    wrapped_coords(j, i, frame) = mod(traj%coords(j, i, frame) - traj%box_dim(j, frame), box_size(j))
                    ! 負の座標の場合、箱のサイズを加算して正の範囲に戻します。
                    if (wrapped_coords(j, i, frame) < 0.0) then
                        wrapped_coords(j, i, frame) = wrapped_coords(j, i, frame) + box_size(j)
                    end if
                end do
            end do
        end do
    end function wrap_coords

end module 





