module coord_convert
    use global_types
    implicit none

    interface center_of_mass
        module procedure center_of_mass_traj_arg
        module procedure center_of_mass_coords_arg
    end interface center_of_mass
    
    interface triclinic_to_orthogonal
        module procedure triclinic_to_orthogonal_real
        module procedure triclinic_to_orthogonal_traj
    end interface triclinic_to_orthogonal

contains
    ! ==========================================================
    ! =============== 重心座標を計算する関数 ===================
    ! ==========================================================
    ! type(trajectory)型の変数を引数に取り、重心座標を計算する関数
    function center_of_mass_traj_arg(traj) result(com)
        implicit none

        type(trajectory), intent(IN) :: traj
        real, allocatable :: com(:, :, :)

        ! local variables
        integer :: i, j, k
        integer :: shift_id
        double precision :: sum_coords(3)

        if (traj%nbeads .eq. 0) then
            print *, "Input Nbeads, the chain length."
            stop
        else
            allocate (com(3, traj%nchains, traj%nframes))
            com = 0.0e0

            do i = 1, traj%nframes
                do j = 1, traj%nchains
                    shift_id = (j - 1) * traj%nbeads
                    sum_coords = 0.0d0
                    do k = 1, traj%nbeads
                        sum_coords(:) = sum_coords(:) + dble(traj%coords(:, shift_id + k, i))
                    end do
                    com(:, j, i) = real(sum_coords(:) / dble(traj%nbeads))
                end do
            end do
        end if
    end function center_of_mass_traj_arg
    ! 座標を引数に取り、重心座標を計算する関数
    function center_of_mass_coords_arg(coords) result(com)
        implicit none

        real, intent(in) :: coords(:, :)
        real :: com(3)

        integer :: i

        com = 0.0e0
        do i = 1, size(coords, 2)
            com = com + coords(:, i)
        end do
        com = com / real(size(coords, 2))
    end function center_of_mass_coords_arg
    ! ==========================================================


    ! 2つの座標間の距離を計算する関数
    function distance(coord1, coord2)
        implicit none

        real, intent(in) :: coord1(3)
        real, intent(in) :: coord2(3)
        real :: distance

        distance = sqrt(sum((coord1 - coord2)**2))
    end function distance

    ! ==========================================================
    ! ============== 座標のラップとアンラップ ==================
    ! ==========================================================
    ! 座標をラップする関数
    function wrap_coords(coords, box_dim) result(wrapped_coords)
        implicit none
        
        real, intent(in) :: coords(:, :)
        real, intent(in) :: box_dim(2, 3)
        real, allocatable :: wrapped_coords(:, :)
        
        integer :: i, j
        real :: box_size(3)
        
        ! Allocate wrapped_coords with the same shape as coords
        allocate(wrapped_coords(size(coords, 1), size(coords, 2)))
        
        ! Calculate the box size in each dimension
        box_size = [box_dim(2, 1) - box_dim(1, 1), &
                    box_dim(2, 2) - box_dim(1, 2), &
                    box_dim(2, 3) - box_dim(1, 3)]
        
        ! Loop over particles and coordinates
        do i = 1, size(coords, 2)
            do j = 1, 3 ! x, y, z coordinates
                ! Shift coordinates by the box minimum, wrap by the box size, and shift back
                wrapped_coords(j, i) = mod(coords(j, i) - box_dim(1, j), box_size(j))
                if (wrapped_coords(j, i) < 0.0) then
                    wrapped_coords(j, i) = wrapped_coords(j, i) + box_size(j)
                end if
                wrapped_coords(j, i) = wrapped_coords(j, i) + box_dim(1, j)
            end do
        end do
    end function wrap_coords

    ! ポリマーの連結性を保証して座標をラップする関数
    function wrap_polymer(coords, box_dim) result(wrapped_coords)
        implicit none

        real, intent(in) :: coords(:, :)
        real, intent(in) :: box_dim(2, 3)
        real :: wrapped_coords(size(coords, 1), size(coords, 2))

        real :: dist
        real :: disp(3)
        real :: box_size(3)
        real :: com(3), center(3)

        integer :: i, j

        ! Calculate the box size in each dimension
        box_size = [box_dim(2, 1) - box_dim(1, 1), &
                    box_dim(2, 2) - box_dim(1, 2), &
                    box_dim(2, 3) - box_dim(1, 3)]
        center   = [(box_dim(2, 1) + box_dim(1, 1)) / 2.0, &
                    (box_dim(2, 2) + box_dim(1, 2)) / 2.0, &
                    (box_dim(2, 3) + box_dim(1, 3)) / 2.0]

        wrapped_coords = coords
        do i = 1, size(coords, 2) - 1
            dist = distance(coords(:, i), coords(:, i+1))
            if (dist > 1.3) then
                do j = 1, 3
                    disp = coords(:, i+1) - coords(:, i)
                    wrapped_coords(:, i+1) = coords(:, i+1) - box_size(j) * nint(disp(j) / box_size(j))
                end do
            else
                wrapped_coords(:, i+1) = coords(:, i+1)
            end if
        enddo

        com = center_of_mass_coords_arg(wrapped_coords)
        disp = com - center
        do j = 1, 3
            if (abs(disp(j)) > box_size(j) / 2.0) then
                wrapped_coords(j, :) = wrapped_coords(j, :) - box_size(j) * nint(disp(j) / box_size(j))
            end if
        end do
    end function wrap_polymer

    ! ==========================================================
    ! triclinic => orthogonal
    ! ==========================================================
    ! x 方向へのせん断 tilte_xy を用いて直交座標系に変換する関数
    ! 将来的に、もっと一般化したい
    function triclinic_to_orthogonal_real(coords, box_dim) result(orthogonal_coords)
        implicit none

        real, intent(in) :: coords(:, :)
        real, intent(in) :: box_dim(:, :)
        real :: orthogonal_coords(size(coords, 1), size(coords, 2))
        real :: ly, xy
        integer :: i

        orthogonal_coords = coords
        ly = box_dim(2, 2) - box_dim(2, 1)
        xy = box_dim(1, 3)
        orthogonal_coords(1, :) = coords(1, :) + coords(2, :) * xy / sqrt(ly*ly + xy*xy)
        orthogonal_coords(2, :) = coords(2, :) * ly / sqrt(ly*ly + xy*xy)

    end function triclinic_to_orthogonal_real
    function triclinic_to_orthogonal_traj(traj) result(orthogonal_traj)
        implicit none

        type(trajectory), intent(in) :: traj
        type(trajectory) :: orthogonal_traj

        orthogonal_traj = traj
        do i = 1, traj%nframes
            orthogonal_traj%coords(:, :, i) = triclinic_to_orthogonal_real(traj%coords(:, :, i), traj%box_dim(:, :, i))
        end do
        orthogonal_traj%is_cubic = .true.
    end function triclinic_to_orthogonal_traj
    ! ==========================================================


end module

