module global_types
    implicit none

    ! 粒子データを格納するための型定義
    type trajectory
        ! Read from Namelist
        character(LEN=100), dimension(100) :: dumpfilenames            ! dump file のリスト
        integer :: ndumpfiles = 1           ! dump file の数
        integer :: nparticles = 0           ! 粒子の数
        integer :: nchains = 0              ! 分子数
        integer :: nbeads = 0               ! 重合度
        integer :: nframes = 0              ! フレーム数
        integer :: dump_freq = 0            ! ダンプの書き出し頻度
        real :: dt = 0                   ! 時間の刻み幅
        logical :: is_cubic = .true.        ! 立方格子かどうかのフラグ
        logical :: is_wrap = .true.        ! wrap or unwrap

        ! Read from trajectory
        integer, allocatable :: timesteps(:)
        real, allocatable :: box_dim(:, :, :) ! (3, 2 or 3, nframes)
        integer, allocatable :: mol(:) ! mol_id
        integer, allocatable :: type(:) ! atom_type
        real, allocatable :: mass(:)
        real, allocatable :: coords(:, :, :)
        integer, ALLOCATABLE :: image_flag(:, :, :)
        ! trajectory(座標軸, 粒子インデックス, frame index)の形式でデータを格納
    end type
end module

