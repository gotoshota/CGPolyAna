module coord_convert
    implicit none

contains
    ! ==========================================================
    ! =============== 重心座標を計算する関数 ===================
    ! ==========================================================
    function center_of_mass(coords) result(com)
        implicit none

        real, intent(in) :: coords(:, :)
        real :: com(3)

        integer :: i

        com = 0.0e0
        do i = 1, size(coords, 2)
            com = com + coords(:, i)
        end do
        com = com / real(size(coords, 2))
    end function center_of_mass
    ! ==========================================================


    ! 2つの座標間の距離を計算する関数
    function distance(coord1, coord2)
        implicit none

        real, intent(in) :: coord1(3)
        real, intent(in) :: coord2(3)
        real :: distance

        distance = sqrt(sum((coord1 - coord2)**2.0))
    end function distance

    ! ==========================================================
    ! ============== 座標のラップとアンラップ ==================
    ! ==========================================================
    ! LAMMPSのbox_boundsから実際のボックスの境界を計算
    subroutine calc_box_size_and_center(box_bounds, box_size, center)
        implicit none

        double precision, intent(in) :: box_bounds(:, :) ! LAMMPSのbox_bounds
        ! box_bounds は (1:3, 1:3) の配列で、
        ! xlo, xhi, xy
        ! ylo, yhi, xz
        ! zlo, zhi, yz
        ! の順で格納されている
        double precision, intent(out) :: box_size(3)
        double precision, intent(out) :: center(3)
        double precision :: xlo, xhi, ylo, yhi, zlo, zhi ! 実際のボックスの境界

        ! LAMMPSのbox_boundsから実際のボックスの境界を計算
        xlo = box_bounds(1, 1) - min(0.0, box_bounds(3, 1), box_bounds(3, 2), box_bounds(3, 1) + box_bounds(3, 2))
        xhi = box_bounds(2, 1) - max(0.0, box_bounds(3, 1), box_bounds(3, 2), box_bounds(3, 1) + box_bounds(3, 2))
        ylo = box_bounds(1, 2) - min(0.0, box_bounds(3, 3))
        yhi = box_bounds(2, 2) - max(0.0, box_bounds(3, 3))
        zlo = box_bounds(1, 3) 
        zhi = box_bounds(2, 3) 
        ! Simulation cell のサイズと中心を計算
        box_size(1) = xhi - xlo
        box_size(2) = yhi - ylo
        box_size(3) = zhi - zlo
        center(1)   = (box_bounds(1, 1) + box_bounds(2, 1)) / 2.0
        center(2)   = (box_bounds(1, 2) + box_bounds(2, 2)) / 2.0
        center(3)   = (box_bounds(1, 3) + box_bounds(2, 3)) / 2.0
    end subroutine calc_box_size_and_center

    ! 座標をアンラップする関数
    function unwrap_coords(coords, box_bounds) result(unwrap_coords)
        implicit none
        
        real, intent(in) :: coords(:, :)
        double precision, intent(in) :: box_bounds(:, :) ! LAMMPSのbox_bounds
        real :: unwrap_coords(size(coords, 1), size(coords, 2))
        real :: coords_prime(size(coords, 1), size(coords, 2))
        real :: unwrap_coords_prime(size(coords, 1), size(coords, 2))
        real :: disp(3), disp_prime(3)

        integer :: i, j
        double precision :: box_size(3)
        double precision :: center(3)
        ! 歪み


    end function

    ! 座標をラップする関数
    function wrap_coords(coords, box_bounds) result(wrapped_coords)
        implicit none
        
        real, intent(in) :: coords(:, :)
        double precision, intent(in) :: box_bounds(:, :) ! LAMMPSのbox_bounds
        real :: wrapped_coords(size(coords, 1), size(coords, 2))
        real :: coords_prime(size(coords, 1), size(coords, 2))
        real :: wrapped_coords_prime(size(coords, 1), size(coords, 2))
        real :: disp(3), disp_prime(3)
        
        integer :: i, j
        double precision :: box_size(3)
        double precision :: box_size_prime(3)
        double precision :: center(3)
        double precision :: center_prime(3)
        ! 歪み
        double precision :: cos_theta ! xy平面
        double precision :: sin_theta ! xy平面
        
        call calc_box_size_and_center(box_bounds, box_size, center)
        ! xy からせん断による歪みcos\theta を計算
        ! cos_theta = xy / sqrt(y^2 + xy^2)
        cos_theta = box_bounds(3, 1) / sqrt(box_size(2)**2.0d0 + box_bounds(3, 1) **2.0d0)
        sin_theta = sqrt(1.0d0 - cos_theta**2.0d0)
        !!!! 現在、xy方向のせん断、あるいはcubicにのみ対応している !!!!
        if ( abs(box_bounds(3, 1)) > 0.0d0) then
            ! y' = y / sin\theta
            coords_prime(2, :) = coords(2, :) / sin_theta
            ! x' = x - y' * cos\theta = x - y / tan\theta
            coords_prime(1, :) = coords(1, :) - coords_prime(2, :) * cos_theta
            ! z' = z
            coords_prime(3, :) = coords(3, :)
            box_size_prime(2) = box_size(2) / sin_theta
            box_size_prime(1) = box_size(1) - box_size_prime(2) * cos_theta
            box_size_prime(3) = box_size(3)
            center_prime(2)   = center(2) / sin_theta
            center_prime(1)   = center(1) - center_prime(2) * cos_theta
            center_prime(3)   = center(3)
        else
            coords_prime = coords
            box_size_prime = box_size
            center_prime = center
        end if
        
        do i = 1, size(coords, 2)
            disp_prime(:) = coords_prime(:, i) - center_prime(:)
            do j = 1, 3
                if (abs(disp_prime(j)) > box_size_prime(j) / 2.0) then
                    wrapped_coords_prime(j, :) = wrapped_coords_prime(j, :) &
                        - box_size_prime(j) * nint(disp_prime(j) / box_size_prime(j))
                end if
            end do
        end do
        ! 逆変換
        if ( abs(box_bounds(3, 1)) > 0.0d0) then
            wrapped_coords(2, :) = wrapped_coords_prime(2, :) * sin_theta
            wrapped_coords(1, :) = wrapped_coords_prime(1, :) + wrapped_coords_prime(2, :) * cos_theta
            wrapped_coords(3, :) = wrapped_coords_prime(3, :)
        else
            wrapped_coords = wrapped_coords_prime
        end if
    end function wrap_coords

    ! ポリマーの連結性を保証してラップする
    function wrap_polymer(coords, box_bounds) result(wrapped_coords)
        implicit none

        real, intent(in) :: coords(:, :)
        double precision, intent(in) :: box_bounds(:, :) 
        real :: wrapped_coords(size(coords, 1), size(coords, 2))
        real :: coords_prime(size(coords, 1), size(coords, 2))
        real :: wrapped_coords_prime(size(coords, 1), size(coords, 2))

        real :: dist, dist2
        real :: disp(3), disp_prime(3)
        double precision :: box_size(3)
        double precision :: box_size_prime(3)
        double precision :: center(3), center_prime(3)
        real :: com(3), com_prime(3)

        ! 歪み
        double precision :: cos_theta ! xy平面
        double precision :: sin_theta ! xy平面

        integer :: i, j

        call calc_box_size_and_center(box_bounds, box_size, center)
        ! xy からせん断による歪みcos\theta を計算
        ! cos_theta = xy / sqrt(y^2 + xy^2)
        cos_theta = box_bounds(3, 1) / sqrt(box_size(2)**2.0d0 + box_bounds(3, 1) **2.0d0)
        sin_theta = sqrt(1.0d0 - cos_theta**2.0d0)
        ! 座標を全て変換
        ! prime は全て非直交座標系を意味する
        !!!! 現在、xy方向のせん断、あるいはcubicにのみ対応している !!!!
        if ( abs(box_bounds(3, 1)) > 0.0d0) then
            ! y' = y / sin\theta
            coords_prime(2, :) = coords(2, :) / sin_theta
            ! x' = x - y' * cos\theta = x - y / tan\theta
            coords_prime(1, :) = coords(1, :) - coords_prime(2, :) * cos_theta
            ! z' = z
            coords_prime(3, :) = coords(3, :)
            box_size_prime(2) = box_size(2) / sin_theta
            box_size_prime(1) = box_size(1) - box_size_prime(2) * cos_theta
            box_size_prime(3) = box_size(3)
            center_prime(2)   = center(2) / sin_theta
            center_prime(1)   = center(1) - center_prime(2) * cos_theta
            center_prime(3)   = center(3)
        else
            coords_prime = coords
            box_size_prime = box_size
            center_prime = center
        end if
        
        wrapped_coords_prime = coords_prime
        do i = 1, size(coords, 2) - 1
            disp_prime = coords_prime(:, i + 1) - coords_prime(:, i)
            do j = 1, 3
                if (abs(disp_prime(j)) > box_size_prime(j) / 2.0) then
                    wrapped_coords_prime(j, i + 1) = wrapped_coords_prime(j, i + 1) &
                        - box_size_prime(j) * nint(disp_prime(j) / box_size_prime(j))
                end if
            enddo
        end do

        com_prime = center_of_mass(wrapped_coords_prime)
        disp_prime = com_prime - center_prime
        do j = 1, 3
            if (abs(disp_prime(j)) > box_size_prime(j) / 2.0) then
                wrapped_coords_prime(j, :) = wrapped_coords_prime(j, :) &
                    - box_size_prime(j) * nint(disp_prime(j) / box_size_prime(j))
            end if
        end do

        ! 逆変換
        if ( abs(box_bounds(3, 1)) > 0.0d0) then
            wrapped_coords(2, :) = wrapped_coords_prime(2, :) * sin_theta
            wrapped_coords(1, :) = wrapped_coords_prime(1, :) + wrapped_coords_prime(2, :) * cos_theta
            wrapped_coords(3, :) = wrapped_coords_prime(3, :)
        else
            wrapped_coords = wrapped_coords_prime
        end if

    end function wrap_polymer

    ! ==========================================================
    ! ============== 距離行列を計算する関数 ====================
    ! ==========================================================
    subroutine distance_matrix(coords, matrix, box_bounds)
        real, dimension(:,:), intent(in)  :: coords !(3, n)
        real, dimension(:,:), intent(out) :: matrix
        double precision, intent(in) :: box_bounds(:, :)
        double precision :: box_size(3), center(3)
        integer :: i, j, k
        integer :: n
        double precision :: tiny = 1.0d-10
        n = size(coords, 2)

        call calc_box_size_and_center(box_bounds, box_size, center)
        ! 周期境界条件を考慮する距離行列を作成
        ! ここでは、ユークリッド距離を計算
        do i = 1, n
            do j = 1, n
                matrix(i, j) = 0.0
                do k = 1, 3
                    matrix(i, j) = matrix(i, j) + (coords(k, i) - coords(k, j) - box_size(k) * nint((coords(k, i) - coords(k, j)) / box_size(k)))**2
                end do
                if (matrix(i, j) < tiny) then
                    matrix(i, j) = tiny
                endif
                matrix(i, j) = sqrt(matrix(i, j))
            end do
        end do
    end subroutine distance_matrix

end module

