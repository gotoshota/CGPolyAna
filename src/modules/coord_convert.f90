module coord_convert
    use global_types
    implicit none

contains
    ! 重心座標を計算する関数
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

    ! 2つの座標間の距離を計算する関数
    function distance(coord1, coord2)
        implicit none

        real, intent(in) :: coord1(3)
        real, intent(in) :: coord2(3)
        real :: distance

        distance = sqrt(sum((coord1 - coord2)**2))
    end function distance

    ! 高分子の連結性を保証したまま座標をラップするサブルーチン
    subroutine wrap_polymer(coords, box_size, wrapped_coords)
        implicit none

        real, intent(in)  :: coords(:, :)
        real, intent(in)  :: box_size(3)
        real, intent(out) :: wrapped_coords(:, :)

        integer :: nbeads
        integer :: i, j
        real :: box_inv(3)
        real :: delta(3)

        nbeads = size(coords, 2)
        box_inv = 1.0 / box_size

        ! 初期位置をそのままコピー
        wrapped_coords(:, 1) = coords(:, 1)

        do i = 2, nbeads
            delta = coords(:, i) - coords(:, i-1)

            ! ラッピング処理
            do j = 1, 3
                ! deltaがボックスサイズの半分を超えているかチェック
                if (abs(delta(j)) .gt. box_size(j) / 2.0) then
                    delta(j) = delta(j) - box_size(j) * nint(delta(j) * box_inv(j))
                end if
            end do

            wrapped_coords(:, i) = wrapped_coords(:, i-1) + delta
        end do
    end subroutine wrap_polymer

    ! 座標をラップするサブルーチン
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

    ! 座標をアンラップするサブルーチン
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

