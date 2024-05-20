module coord_convert
    use global_types
    implicit none

contains
    function center_of_mass(traj) result(com)
        implicit none

        type(trajectory), intent(IN) :: traj
        real, allocatable :: com(:, :, :)

        ! local variables
        integer :: i, j, k
        integer :: shift_id
        double precision :: sum_coords(3)
        real :: wrapped_coords(3, traj%nbeads)

        if (traj%nbeads .eq. 0) then
            print *, "Input Nbeads, the chain length."
            stop
        else
            allocate (com(3, traj%nchains, traj%nframes), source=0.0e0)

            do i = 1, traj%nframes
                do j = 1, traj%nchains
                    shift_id = (j - 1)*traj%nbeads
                    sum_coords = 0.0d0
                    do k = 1, traj%nbeads
                        sum_coords(:) = sum_coords(:) + dble(traj%coords(:, shift_id + k, i))
                    end do
                    com(:, j, i) = real(sum_coords(:)/dble(traj%nbeads))
                end do
            end do
        end if
    end function center_of_mass

    function distance(coord1, coord2)
        implicit none

        real, intent(in) :: coord1(3)
        real, intent(in) :: coord2(3)
        real :: distance

        distance = sqrt(sum((coord1 - coord2)**2))
    end function distance

    subroutine wrap_polymer(coords, box_size, wrapped_coords)
        implicit none
    
        real, intent(in)  :: coords(:, :)
        real, intent(in)  :: box_size(3)
        real, intent(out) :: wrapped_coords(:, :)
    
        integer :: nbeads
        integer :: i
        real :: dist
        real :: box_inv(3)
    
        nbeads = size(coords, 2)
        box_inv = 1.0 / box_size
    
        ! 初期位置をそのままコピー
        wrapped_coords(:, 1) = coords(:, 1)
    
        do i = 2, nbeads
            dist = distance(coords(:, i-1), coords(:, i))
    
            ! 距離が閾値を超えた場合
            if (dist .gt. 1.1) then
                wrapped_coords(:, i) = coords(:, i) - box_size(:) * nint((coords(:, i) - coords(:, i-1)) * box_inv(:))
                ! 距離が閾値を超えない場合はそのままコピー
            end if
        enddo
    end subroutine wrap_polymer
    
    subroutine wrap_coords(traj, center, wrapped_coords)
        implicit none

        type(trajectory), intent(in) :: traj
        real, intent(in), optional :: center(3) = [0.0, 0.0, 0.0]
        real, intent(out) :: wrapped_coords(:, :, :)

        integer :: i, j, frame
        real :: box_size(3)

        allocate (wrapped_coords, mold=traj%coords)

        do frame = 1, traj%nframes
            if (traj%is_cubic) then
                box_size = [traj%box_dim(1, 2, frame) - traj%box_dim(1, 1, frame), &
                    traj%box_dim(2, 2, frame) - traj%box_dim(2, 1, frame), &
                    traj%box_dim(3, 2, frame) - traj%box_dim(3, 1, frame)]
            else
                print *, "Error: This program support only a cubic simulation cell."
            end if

            do i = 1, traj%nparticles
                do j = 1, 3 ! x, y, z座標
                    ! 座標を箱のサイズでラップします。
                    wrapped_coords(j, i, frame) = mod(traj%coords(j, i, frame) - center(j) - traj%box_dim(j, 2, frame) &
                    + traj%box_dim(j, 1, frame), box_size(j))
                    ! 負の座標の場合、箱のサイズを加算して正の範囲に戻します。
                    if (wrapped_coords(j, i, frame) .lt. 0.0) then
                        wrapped_coords(j, i, frame) = wrapped_coords(j, i, frame) + box_size(j)
                    end if
                end do
            end do
        end do
    end subroutine wrap_coords

    subroutine unwrap_coords(traj, center, unwrapped_coords)
        implicit none

        type(trajectory), intent(in) :: traj
        real, intent(in), optional :: center(3) = [0.0, 0.0, 0.0]
        real, intent(out) :: unwrapped_coords(:, :, :)

        integer :: i, j, frame
        real :: box_size(3)

        allocate (unwrapped_coords, mold=traj%coords)

        do frame = 1, traj%nframes
            if (traj%is_cubic) then
                box_size = [traj%box_dim(1, 2, frame) - traj%box_dim(1, 1, frame), &
                    traj%box_dim(2, 2, frame) - traj%box_dim(2, 1, frame), &
                    traj%box_dim(3, 2, frame) - traj%box_dim(3, 1, frame)]
            else
                print *, "Error: This program support only a cubic simulation cell."
            end if

            do i = 1, traj%nparticles
                do j = 1, 3 ! x, y, z座標
                    ! 座標を箱のサイズでラップします。
                    unwrapped_coords(j, i, frame) = traj%coords(j, i, frame) - center(j) - traj%box_dim(j, 2, frame) &
                    + traj%box_dim(j, 1, frame)
                end do
            end do
        end do
    end subroutine unwrap_coords

end module

