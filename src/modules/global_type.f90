module simulation_data_types
    implicit none

    ! 粒子データを格納するための型定義
    type trajectory
        integer, ALLOCATABLE :: mol(:) ! mol_id
        integer, ALLOCATABLE :: type(:) ! atom_tyep
        real, ALLOCATABLE :: mass(:)
        real, allocatable :: coord(:,:,:)
        ! trajectory(座標軸, 粒子インデックス, ステップインデックス)の形式でデータを格納
    end type particle

    ! シミュレーションのメタデータを格納する型
    type simulation_data
        integer :: nparticles ! 粒子の数
        integer :: nmols ! 分子数
        integer :: natoms !  重合度
        integer :: nsteps     ! ステップ数
        real :: dt               ! 時間の刻み幅
        integer :: dump_frequency ! データダンプの頻度
        real, allocatable :: box_dim(:,:) ! (3 or 6, nframes)
    end type simulation_data
end module simulation_data_types

