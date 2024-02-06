module global_types
    implicit none

    ! 粒子データを格納するための型定義
    type trajectory
        ! Read from Namelist
        CHARACTER(LEN=100), DIMENSION(100)  :: dumpfilenames            ! dump file のリスト 
        integer                             :: ndumpfiles = 1           ! dump file の数
        integer                             :: nparticles               ! 粒子の数
        integer                             :: nchains                  ! 分子数
        integer                             :: nbeads                   ! 重合度
        integer                             :: nframes = 0              ! フレーム数
        integer                             :: dump_freq                ! ダンプの書き出し頻度
        real                                :: dt                       ! 時間の刻み幅
        logical                             :: is_cubic = .true.        ! 立方格子かどうかのフラグ

        ! Read from trajectory
        integer, allocatable    :: timesteps(:)
        real, allocatable       :: box_dim(:,:) ! (3 or 6, nframes)
        integer, ALLOCATABLE    :: mol(:) ! mol_id
        integer, ALLOCATABLE    :: type(:) ! atom_tyep
        real, ALLOCATABLE       :: mass(:)
        real, allocatable       :: coord(:,:,:)
        ! trajectory(座標軸, 粒子インデックス, frame index)の形式でデータを格納
    end type 
end module

