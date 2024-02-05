module global_types
    implicit none

    ! 粒子データを格納するための型定義
    type trajectory
        ! Read from Namelist
        CHARACTER(LEN=256)    :: dumpfilename
        integer             :: nparticles       ! 粒子の数
        integer             :: nchains          ! 分子数
        integer             :: nbeads           ! 重合度
        integer             :: nsteps           ! ステップ数
        integer             :: nframes          ! フレーム数
        integer             :: dump_freq        ! ダンプの書き出し頻度
        real                :: dt               ! 時間の刻み幅
        logical             :: is_cubic         ! 立方格子かどうかのフラグ
        logical             :: is_dump_first    ! 初期構造を書き出しているかどうか

        ! Read from trajectory
        real, allocatable :: box_dim(:,:) ! (3 or 6, nframes)
        integer, ALLOCATABLE :: mol(:) ! mol_id
        integer, ALLOCATABLE :: type(:) ! atom_tyep
        real, ALLOCATABLE :: mass(:)
        real, allocatable :: coord(:,:,:)
        ! trajectory(座標軸, 粒子インデックス, frame index)の形式でデータを格納
    end type 

contains

    subroutine read_simulation_params(filename, traj)
        character(len=*), intent(in) :: filename
        type(trajectory), intent(out) :: traj

        ! local variables
        integer :: nparticles, nchains, nbeads, nsteps, nframes
        real :: dt
        integer :: dump_freq
        logical :: is_cubic, is_dump_first
        CHARACTER(len=256) :: dumpfilename

        namelist /simulation_params/ dumpfilename, nparticles, nchains, nbeads, nsteps, &
        dt, dump_freq, nframes, is_cubic, is_dump_first

        ! initial value (nframs, is_cubic, and is_dump_first are optional)
        nframes = 0
        is_cubic = .true.
        is_dump_first = .true.

        ! read namelist
        open(unit=10, file=filename, status='old')
            read(10, simulation_params)
        close(10)

        ! ユーザー定義型のインスタンスに読み込んだデータを代入
        traj%dumpfilename = dumpfilename
        traj%nparticles = nparticles
        traj%nchains = nchains
        traj%nbeads = nbeads
        traj%nsteps = nsteps
        traj%dt = dt
        traj%dump_freq = dump_freq
        traj%is_cubic = is_cubic
        traj%is_dump_first = is_dump_first

        if (nframes == 0 .eqv. is_dump_first) then ! if is_dump_first = .true.
            traj%nframes = traj%nsteps / traj%dump_freq + 1
        else if (nframes == 0 .neqv. is_dump_first) then ! if is_dump_first = .false.
            traj%nframes = traj%nsteps / traj%dump_freq
        else
            traj%nframes = nframes
        endif

        if (allocated(traj%box_dim)) deallocate(traj%box_dim)
        if (traj%is_cubic) then
            allocate(traj%box_dim(3, traj%nframes))
        else
            allocate(traj%box_dim(6, traj%nframes))
        endif

        ALLOCATE(traj%mass(nparticles), traj%type(nparticles), traj%mol(nparticles))
        ALLOCATE(traj%coord(3, nparticles, 0:nframes))
    end subroutine
end module

