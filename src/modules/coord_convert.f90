!> @file coord_convert.f90
!> @brief 座標変換を行うためのモジュール
!> @details 重心計算、距離計算、座標のラップ・アンラップなどの機能を提供します
!> @author CGPolyAna開発チーム
!> @date 2025-03-16
!> @version 1.0
module coord_convert
    use error_handling
    implicit none

contains
    !> @brief 重心座標を計算する関数
    !> @param[in] coords 座標配列（3次元ベクトルの配列）
    !> @return 重心座標（3次元ベクトル）
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

    !> @brief 2つの座標間の距離を計算する関数
    !> @param[in] coord1 1つ目の座標（3次元ベクトル）
    !> @param[in] coord2 2つ目の座標（3次元ベクトル）
    !> @return 2点間の距離
    function distance(coord1, coord2) result(dist)
        implicit none
        real, intent(in) :: coord1(3)
        real, intent(in) :: coord2(3)
        real :: dist

        dist = sqrt(sum((coord1 - coord2)**2.0))
    end function distance

    !> @brief LAMMPSのbox_boundsから実際のボックスの境界を計算する
    !> @param[in] box_bounds LAMMPSのbox_bounds
    !> @param[out] box_size ボックスのサイズ
    !> @param[out] center ボックスの中心
    !> @return エラーコード（0: 成功, 非0: エラー）
    function calc_box_size_and_center(box_bounds, box_size, center) result(ierr)
        implicit none
        double precision, intent(in) :: box_bounds(:, :) !< LAMMPSのbox_bounds
        !< box_bounds は (1:3, 1:3) の配列で、
        !< xlo, xhi, xy
        !< ylo, yhi, xz
        !< zlo, zhi, yz
        !< の順で格納されている
        double precision, intent(out) :: box_size(3)
        double precision, intent(out) :: center(3)
        integer :: ierr
        
        double precision :: xlo, xhi, ylo, yhi, zlo, zhi !< 実際のボックスの境界

        ! エラーコード初期化
        ierr = 0
        
        ! 入力チェック
        if (size(box_bounds, 1) < 3 .or. size(box_bounds, 2) < 3) then
            ierr = ERR_INVALID_PARAMETER
            return
        end if

        ! LAMMPSのbox_boundsから実際のボックスの境界を計算
        xlo = box_bounds(1, 1) - min(0.0d0, box_bounds(3, 1), box_bounds(3, 2), box_bounds(3, 1) + box_bounds(3, 2))
        xhi = box_bounds(2, 1) - max(0.0d0, box_bounds(3, 1), box_bounds(3, 2), box_bounds(3, 1) + box_bounds(3, 2))
        ylo = box_bounds(1, 2) - min(0.0d0, box_bounds(3, 3))
        yhi = box_bounds(2, 2) - max(0.0d0, box_bounds(3, 3))
        zlo = box_bounds(1, 3) 
        zhi = box_bounds(2, 3) 
        
        ! Simulation cell のサイズと中心を計算
        box_size(1) = xhi - xlo
        box_size(2) = yhi - ylo
        box_size(3) = zhi - zlo
        center(1)   = (box_bounds(1, 1) + box_bounds(2, 1)) / 2.0d0
        center(2)   = (box_bounds(1, 2) + box_bounds(2, 2)) / 2.0d0
        center(3)   = (box_bounds(1, 3) + box_bounds(2, 3)) / 2.0d0
    end function calc_box_size_and_center

    !> @brief 座標をアンラップする関数
    !> @param[in] coords 座標配列
    !> @param[in] box_bounds LAMMPSのbox_bounds
    !> @param[in] image_flags イメージフラグ
    !> @param[out] error エラー情報（オプション）
    !> @return アンラップされた座標
    function unwrap_coords(coords, box_bounds, image_flags, error) result(unwrapped_coords)
        implicit none
        real, intent(in) :: coords(:, :)
        double precision, intent(in) :: box_bounds(:, :)
        integer, intent(in) :: image_flags(:,:)
        type(ErrorInfo), intent(out), optional :: error
        real :: unwrapped_coords(size(coords, 1), size(coords, 2))
        
        ! local variables
        integer :: i, j, ierr
        double precision :: box_size(3)
        double precision :: center(3)

        ! エラー情報の初期化
        if (present(error)) call error%clear()
        
        ! 入力チェック
        if (size(coords, 1) /= 3) then
            if (present(error)) then
                call error%set(ERR_INVALID_PARAMETER, "coords must be 3xN array", "unwrap_coords")
            end if
            unwrapped_coords = coords
            return
        end if
        
        if (size(image_flags, 1) /= 3 .or. size(image_flags, 2) /= size(coords, 2)) then
            if (present(error)) then
                call error%set(ERR_INVALID_PARAMETER, "image_flags must be 3xN array with same N as coords", "unwrap_coords")
            end if
            unwrapped_coords = coords
            return
        end if

        ! ボックスサイズと中心の計算
        ierr = calc_box_size_and_center(box_bounds, box_size, center)
        if (ierr /= 0) then
            if (present(error)) then
                call error%set(ERR_INVALID_PARAMETER, "Invalid box_bounds", "unwrap_coords")
            end if
            unwrapped_coords = coords
            return
        end if

        ! 座標のアンラップ
        do i = 1, size(coords, 2)
            unwrapped_coords(1, i) = coords(1, i) + box_size(1) * image_flags(1, i) + box_bounds(3, 1) * image_flags(2, i) &
            + box_bounds(3, 2) * image_flags(3, i)
            unwrapped_coords(2, i) = coords(2, i) + box_size(2) * image_flags(2, i) + box_bounds(3, 3) * image_flags(3, i)
            unwrapped_coords(3, i) = coords(3, i) + box_size(3) * image_flags(3, i)
        end do
    end function unwrap_coords

    !> @brief 座標をラップする関数
    !> @param[in] coords 座標配列
    !> @param[in] box_bounds LAMMPSのbox_bounds
    !> @param[out] error エラー情報（オプション）
    !> @return ラップされた座標
    function wrap_coords(coords, box_bounds, error) result(wrapped_coords)
        implicit none
        real, intent(in) :: coords(:, :)
        double precision, intent(in) :: box_bounds(:, :) !< LAMMPSのbox_bounds
        type(ErrorInfo), intent(out), optional :: error
        real :: wrapped_coords(size(coords, 1), size(coords, 2))
        
        ! local variables
        integer :: i, j, ierr
        double precision :: box_size(3)
        double precision :: center(3)
        
        ! 歪み
        double precision :: cos_theta !< xy平面
        double precision :: sin_theta !< xy平面
        
        ! 非直交座標系
        real :: coords_prime(size(coords, 1), size(coords, 2))
        real :: wrapped_coords_prime(size(coords, 1), size(coords, 2))
        real :: disp_prime(3)
        double precision :: box_size_prime(3)
        double precision :: center_prime(3)

        ! エラー情報の初期化
        if (present(error)) call error%clear()
        
        ! 入力チェック
        if (size(coords, 1) /= 3) then
            if (present(error)) then
                call error%set(ERR_INVALID_PARAMETER, "coords must be 3xN array", "wrap_coords")
            end if
            wrapped_coords = coords
            return
        end if

        ! ボックスサイズと中心の計算
        ierr = calc_box_size_and_center(box_bounds, box_size, center)
        if (ierr /= 0) then
            if (present(error)) then
                call error%set(ERR_INVALID_PARAMETER, "Invalid box_bounds", "wrap_coords")
            end if
            wrapped_coords = coords
            return
        end if
        
        ! xy からせん断による歪みcos\theta を計算
        ! cos_theta = xy / sqrt(y^2 + xy^2)
        if (abs(box_size(2)) < 1.0e-10) then
            if (present(error)) then
                call error%set(ERR_INVALID_PARAMETER, "Box size in y direction is too small", "wrap_coords")
            end if
            wrapped_coords = coords
            return
        end if
        
        cos_theta = box_bounds(3, 1) / sqrt(box_size(2)**2.0d0 + box_bounds(3, 1)**2.0d0)
        sin_theta = sqrt(1.0d0 - cos_theta**2.0d0)
        
        ! 座標を全て変換
        ! prime は全て非直交座標系を意味する
        ! 現在、xy方向のせん断、あるいはcubicにのみ対応している
        if (abs(box_bounds(3, 1)) > 0.0d0) then
            ! y' = y / sin\theta
            coords_prime(2, :) = coords(2, :) / sin_theta
            ! x' = x - y' * cos\theta = x - y / tan\theta
            coords_prime(1, :) = coords(1, :) - coords_prime(2, :) * cos_theta
            ! z' = z
            coords_prime(3, :) = coords(3, :)
            box_size_prime(2) = box_size(2) / sin_theta
            box_size_prime(1) = box_size(1) !- box_size_prime(2) * cos_theta
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
        do i = 1, size(coords, 2)
            disp_prime = coords_prime(:, i) - center_prime
            do j = 1, 3
                wrapped_coords_prime(j, i) = coords_prime(j, i) - box_size_prime(j) * nint(disp_prime(j) / box_size_prime(j))
            enddo
        end do

        ! 逆変換
        if (abs(box_bounds(3, 1)) > 0.0d0) then
            wrapped_coords(2, :) = wrapped_coords_prime(2, :) * sin_theta
            wrapped_coords(1, :) = wrapped_coords_prime(1, :) + wrapped_coords_prime(2, :) * cos_theta
            wrapped_coords(3, :) = wrapped_coords_prime(3, :)
        else
            wrapped_coords = wrapped_coords_prime
        end if
    end function wrap_coords

    !> @brief ポリマーの連結性を保証してラップする関数
    !> @param[in] coords 座標配列
    !> @param[in] box_bounds LAMMPSのbox_bounds
    !> @param[out] error エラー情報（オプション）
    !> @return ラップされた座標
    function wrap_polymer(coords, box_bounds, error) result(wrapped_coords)
        implicit none
        real, intent(in) :: coords(:, :)
        double precision, intent(in) :: box_bounds(:, :) 
        type(ErrorInfo), intent(out), optional :: error
        real :: wrapped_coords(size(coords, 1), size(coords, 2))
        
        ! local variables
        integer :: i, j, ierr
        real :: coords_prime(size(coords, 1), size(coords, 2))
        real :: wrapped_coords_prime(size(coords, 1), size(coords, 2))
        real :: disp_prime(3)
        double precision :: box_size(3)
        double precision :: box_size_prime(3)
        double precision :: center(3), center_prime(3)
        real :: com_prime(3)

        ! 歪み
        double precision :: cos_theta !< xy平面
        double precision :: sin_theta !< xy平面

        ! エラー情報の初期化
        if (present(error)) call error%clear()
        
        ! 入力チェック
        if (size(coords, 1) /= 3) then
            if (present(error)) then
                call error%set(ERR_INVALID_PARAMETER, "coords must be 3xN array", "wrap_polymer")
            end if
            wrapped_coords = coords
            return
        end if
        
        if (size(coords, 2) < 2) then
            if (present(error)) then
                call error%set(ERR_INVALID_PARAMETER, "coords must have at least 2 points for polymer", "wrap_polymer")
            end if
            wrapped_coords = coords
            return
        end if

        ! ボックスサイズと中心の計算
        ierr = calc_box_size_and_center(box_bounds, box_size, center)
        if (ierr /= 0) then
            if (present(error)) then
                call error%set(ERR_INVALID_PARAMETER, "Invalid box_bounds", "wrap_polymer")
            end if
            wrapped_coords = coords
            return
        end if
        
        ! xy からせん断による歪みcos\theta を計算
        ! cos_theta = xy / sqrt(y^2 + xy^2)
        if (abs(box_size(2)) < 1.0e-10) then
            if (present(error)) then
                call error%set(ERR_INVALID_PARAMETER, "Box size in y direction is too small", "wrap_polymer")
            end if
            wrapped_coords = coords
            return
        end if
        
        cos_theta = box_bounds(3, 1) / sqrt(box_size(2)**2.0d0 + box_bounds(3, 1)**2.0d0)
        sin_theta = sqrt(1.0d0 - cos_theta**2.0d0)
        
        ! 座標を全て変換
        ! prime は全て非直交座標系を意味する
        ! 現在、xy方向のせん断、あるいはcubicにのみ対応している
        if (abs(box_bounds(3, 1)) > 0.0d0) then
            ! y' = y / sin\theta
            coords_prime(2, :) = coords(2, :) / sin_theta
            ! x' = x - y' * cos\theta = x - y / tan\theta
            coords_prime(1, :) = coords(1, :) - coords_prime(2, :) * cos_theta
            ! z' = z
            coords_prime(3, :) = coords(3, :)
            box_size_prime(2) = box_size(2) / sin_theta
            box_size_prime(1) = box_size(1) 
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
            disp_prime = wrapped_coords_prime(:, i + 1) - wrapped_coords_prime(:, i)
            do j = 1, 3
                wrapped_coords_prime(j, i + 1) = wrapped_coords_prime(j, i + 1) &
                    - box_size_prime(j) * nint(disp_prime(j) / box_size_prime(j))
            enddo
        end do

        com_prime = center_of_mass(wrapped_coords_prime)
        disp_prime = com_prime - center_prime
        do j = 1, 3
            wrapped_coords_prime(j, :) = wrapped_coords_prime(j, :) &
                - box_size_prime(j) * nint(disp_prime(j) / box_size_prime(j))
        end do

        ! 逆変換
        if (abs(box_bounds(3, 1)) > 0.0d0) then
            wrapped_coords(2, :) = wrapped_coords_prime(2, :) * sin_theta
            wrapped_coords(1, :) = wrapped_coords_prime(1, :) + wrapped_coords_prime(2, :) * cos_theta
            wrapped_coords(3, :) = wrapped_coords_prime(3, :)
        else
            wrapped_coords = wrapped_coords_prime
        end if
    end function wrap_polymer

    !> @brief 距離行列を計算する関数
    !> @param[in] coords 座標配列
    !> @param[out] matrix 距離行列
    !> @param[in] box_bounds LAMMPSのbox_bounds
    !> @param[out] error エラー情報（オプション）
    !> @return エラーコード（0: 成功, 非0: エラー）
    function distance_matrix(coords, matrix, box_bounds, error) result(ierr)
        implicit none
        real, dimension(:,:), intent(in)  :: coords !(3, n)
        real, dimension(:,:), intent(out) :: matrix
        double precision, intent(in) :: box_bounds(:, :)
        type(ErrorInfo), intent(out), optional :: error
        integer :: ierr
        
        ! local variables
        double precision :: box_size(3), center(3)
        integer :: i, j, k
        integer :: n
        double precision :: tiny = 1.0d-10

        ! エラー情報の初期化
        if (present(error)) call error%clear()
        ierr = 0
        
        ! 入力チェック
        if (size(coords, 1) /= 3) then
            if (present(error)) then
                call error%set(ERR_INVALID_PARAMETER, "coords must be 3xN array", "distance_matrix")
            end if
            ierr = ERR_INVALID_PARAMETER
            return
        end if
        
        n = size(coords, 2)
        if (size(matrix, 1) /= n .or. size(matrix, 2) /= n) then
            if (present(error)) then
                call error%set(ERR_INVALID_PARAMETER, "matrix must be NxN array with same N as coords", "distance_matrix")
            end if
            ierr = ERR_INVALID_PARAMETER
            return
        end if

        ! ボックスサイズと中心の計算
        ierr = calc_box_size_and_center(box_bounds, box_size, center)
        if (ierr /= 0) then
            if (present(error)) then
                call error%set(ERR_INVALID_PARAMETER, "Invalid box_bounds", "distance_matrix")
            end if
            return
        end if
        
        ! 周期境界条件を考慮する距離行列を作成
        ! ここでは、ユークリッド距離を計算
        do i = 1, n
            do j = 1, n
                matrix(i, j) = 0.0
                do k = 1, 3
                    matrix(i, j) = matrix(i, j) + (coords(k, i) - coords(k, j) - box_size(k) &
                    * nint((coords(k, i) - coords(k, j)) / box_size(k)))**2
                end do
                if (matrix(i, j) < tiny) then
                    matrix(i, j) = tiny
                endif
                matrix(i, j) = sqrt(matrix(i, j))
            end do
        end do
    end function distance_matrix

end module coord_convert
