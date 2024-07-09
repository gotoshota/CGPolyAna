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

        distance = sqrt(sum((coord1 - coord2)**2.0))
    end function distance

    ! ==========================================================
    ! ============== 座標のラップとアンラップ ==================
    ! ==========================================================
    ! 座標をラップする関数
    function wrap_coords(coords, box_dim) result(wrapped_coords)
        implicit none
        
        real, intent(in) :: coords(:, :)
        real, intent(in) :: box_dim(:, :)
        real, allocatable :: wrapped_coords(:, :)
        real :: disp(3)
        
        integer :: i, j
        real :: box_size(3)
        real :: center(3)
        
        ! Allocate wrapped_coords with the same shape as coords
        allocate(wrapped_coords(size(coords, 1), size(coords, 2)))
        
        ! Calculate the box size in each dimension
        box_size = [box_dim(2, 1) - box_dim(1, 1), &
                    box_dim(2, 2) - box_dim(1, 2), &
                    box_dim(2, 3) - box_dim(1, 3)]
        center   = [(box_dim(2, 1) + box_dim(1, 1)) / 2.0, &
                (box_dim(2, 2) + box_dim(1, 2)) / 2.0, &
                (box_dim(2, 3) + box_dim(1, 3)) / 2.0]
        
        ! Loop over particles and coordinates
        do i = 1, size(coords, 2)
            disp(:) = coords(:, i) - center(:)
            do j = 1, size(coords, 1) ! x, y, z coordinates
                ! Shift coordinates by the box minimum, wrap by the box size, and shift back
                 wrapped_coords(j, i) = coords(j, i) - box_size(j) * nint(disp(j) / box_size(j))
            end do
        end do
    end function wrap_coords

    ! ポリマーの連結性を保証してラップする
    function wrap_polymer(coords, box_dim) result(wrapped_coords)
        implicit none

        real, intent(in) :: coords(:, :)
        real, intent(in) :: box_dim(:, :)
        real :: wrapped_coords(size(coords, 1), size(coords, 2))

        real :: dist, dist2
        real :: disp(3)
        real :: box_size(3)
        real :: com(3), center(3)

        integer :: i, j

        ! Calculate the box size in each dimension
        box_size(1) = box_dim(2, 1) - box_dim(1, 1)
        box_size(2) = box_dim(2, 2) - box_dim(1, 2)
        box_size(3) = box_dim(2, 3) - box_dim(1, 3)
        center(1)   = (box_dim(2, 1) + box_dim(1, 1)) / 2.0
        center(2)   = (box_dim(2, 2) + box_dim(1, 2)) / 2.0
        center(3)   = (box_dim(2, 3) + box_dim(1, 3)) / 2.0

        wrapped_coords = coords
        do i = 1, size(coords, 2) - 1
            dist = distance(wrapped_coords(:, i), coords(:, i+1))
            if (dist > 1.5) then
                disp = coords(:, i+1) - wrapped_coords(:, i)
                do j = 1, 3
                    wrapped_coords(j, i+1) = coords(j, i+1) - box_size(j) * nint(disp(j) / box_size(j))
                end do
                dist2 = distance(wrapped_coords(:, i), wrapped_coords(:, i+1))
                if (dist2 > 1.5) then
                    print *, "Error: wrapping failed"
                    print*, "dist2 = ", dist2
                    print*, "index = ", i
                    !stop
                end if
            else
                wrapped_coords(:, i+1) = coords(:, i+1)
            end if
        end do

        com = center_of_mass_coords_arg(wrapped_coords)
        disp = com - center
        do j = 1, 3
            if (abs(disp(j)) > box_size(j) / 2.0) then
                do i = 1, size(coords, 2)
                    wrapped_coords(j, i) = wrapped_coords(j, i) - box_size(j) * nint(disp(j) / box_size(j))
                end do
            end if
        end do

    end function wrap_polymer

    ! ==========================================================
    ! triclinic => orthogonal
    ! ==========================================================
    function triclinic_to_orthogonal_real(coords, box_dim) result(orthogonal_coords)
        implicit none

        real, intent(in) :: coords(:, :)
        real, intent(in) :: box_dim(:, :)
        real :: orthogonal_coords(size(coords, 1), size(coords, 2))
        real :: ly, xy

        orthogonal_coords = coords
        ly = box_dim(2, 2) - box_dim(1, 2)
        xy = box_dim(3, 1)
        orthogonal_coords(1, :) = coords(1, :) + coords(2, :) * xy / sqrt(ly*ly + xy*xy)
        orthogonal_coords(2, :) = coords(2, :) !* ly / sqrt(ly*ly + xy*xy)
    end function triclinic_to_orthogonal_real
    !function triclinic_to_orthogonal_real(coords, box_dim) result(orthogonal_coords)
    !    implicit none
    !    real, intent(in) :: coords(:, :)
    !    real, intent(in) :: box_dim(:, :)
    !    real :: orthogonal_coords(size(coords, 1), size(coords, 2))

    !    real :: a, b(2), c(3) ! 変換行列 : LAMMPS How to triclinc 参照
    !    real :: scalar_b, scalar_c ! 変換行列のスカラー成分 : 長さ
    !    real :: cos_beta, cos_gamma ! 非直交座標系の角度
    !    real :: lx, ly, lz ! LAMMPS出力: lh - lo
    !    real :: xy, xz, yz ! LAMMPS出力: tilte


    !    ! 直交座標系でのボックスの大きさ: LAMMPSの出力
    !    lx = box_dim(2, 1) - box_dim(1, 1)
    !    ly = box_dim(2, 2) - box_dim(1, 2)
    !    lz = box_dim(2, 3) - box_dim(1, 3)
    !    ! ボックスの角度: LAMMPSの出力
    !    xy = box_dim(3, 1)
    !    xz = box_dim(3, 2)
    !    yz = box_dim(3, 3)
    !    ! スカラー長さの計算
    !    scalar_b = sqrt(ly**2 + xy**2)
    !    scalar_c = sqrt(lz**2 + xz**2 + yz**2)
    !    ! cos(beta), cos(gamma)の計算 : How to triclinic 参照
    !    cos_beta = xz / scalar_c
    !    cos_gamma = xy / scalar_b
    !    ! 変換行列の計算
    !    a = lx
    !    b(1) = scalar_b * cos_gamma
    !    b(2) = sqrt(scalar_b**2 - b(1)**2)
    !    c(1) = scalar_c * cos_beta
    !    c(2) = (ly * lz - xy * xz) / scalar_b
    !    c(3) = sqrt(scalar_c**2 - c(1)**2 - c(2)**2)
    !    ! 出力がx, y, zの場合
    !    a = 1.0
    !    b(2) = 1.0
    !    c(3) = 1.0
    !    ! 座標変換
    !    orthogonal_coords(1, :) = a * coords(1, :) + b(1) * coords(2, :) + c(1) * coords(3, :)
    !    orthogonal_coords(2, :) = b(2) * coords(2, :) + c(2) * coords(3, :)
    !    orthogonal_coords(3, :) = c(3) * coords(3, :)
    !    print *, orthogonal_coords(:, 1)
    !end function triclinic_to_orthogonal_real
    !function triclinic_to_orthogonal_real(coords, box_dim) result(orthogonal_coords)
    !    implicit none
    !    real, intent(in) :: coords(:, :)
    !    real, intent(in) :: box_dim(3, 3)
    !    real :: orthogonal_coords(size(coords, 1), size(coords, 2))

    !    real :: lx, ly, lz
    !    real :: xy, xz, yz
    !    real :: xlo, ylo, zlo
    !    real :: xhi, yhi, zhi
    !    integer :: i

    !    ! LAMMPSボックスの寸法を取得
    !    xlo = box_dim(1, 1)
    !    xhi = box_dim(2, 1)
    !    ylo = box_dim(1, 2)
    !    yhi = box_dim(2, 2)
    !    zlo = box_dim(1, 3)
    !    zhi = box_dim(2, 3)
    !    xy = box_dim(3, 1)
    !    xz = box_dim(3, 2)
    !    yz = box_dim(3, 3)

    !    ! ボックスのサイズを計算
    !    lx = xhi - xlo
    !    ly = yhi - ylo
    !    lz = zhi - zlo

    !    ! 座標変換
    !    do i = 1, size(coords, 2)
    !        orthogonal_coords(1, i) = coords(1, i) * lx + coords(2, i) * xy + coords(3, i) * xz
    !        orthogonal_coords(2, i) = coords(2, i) * ly + coords(3, i) * yz
    !        orthogonal_coords(3, i) = coords(3, i) * lz
    !    end do

    !    print *, orthogonal_coords(:, 1)
    !end function triclinic_to_orthogonal_real
    function triclinic_to_orthogonal_traj(traj) result(orthogonal_traj)
        implicit none

        type(trajectory), intent(in) :: traj
        type(trajectory) :: orthogonal_traj
        integer :: i

        orthogonal_traj = traj
        do i = 1, traj%nframes
            orthogonal_traj%coords(:, :, i) = triclinic_to_orthogonal_real(traj%coords(:, :, i), traj%box_dim(:, :, i))
            orthogonal_traj%box_dim(:, :, i) = traj%box_dim(:, :, 1)
        end do
        orthogonal_traj%is_cubic = .true.
    end function triclinic_to_orthogonal_traj
    ! ==========================================================


end module

