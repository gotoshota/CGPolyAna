module global_types
    implicit none

    ! 粒子データを格納するための型定義
    type MDParams
        ! Read from Namelist
        character(LEN=100), dimension(100) :: dumpfilenames            ! dump file のリスト
        integer :: ndumpfiles = 1           ! dump file の数
        integer :: nchains = 0              ! 分子数
        integer :: nbeads = 0               ! 重合度
        integer :: nframes = 0              ! フレーム数
        integer :: dump_freq = 0            ! ダンプの書き出し頻度
        double precision :: dt = 0          ! 時間の刻み幅
        logical :: is_cubic = .true.        ! 立方格子かどうかのフラグ
        logical :: is_wrap = .true.         ! wrap or unwrap
    end type
contains
    subroutine read_MDParams(nmlfilename, params)
        implicit none
        character(len=*), intent(in) :: nmlfilename
        type(MDParams), intent(inout) :: params

        ! local variables
        integer :: nchains, nbeads, nframes
        double precision :: dt
        integer :: dump_freq, ndumpfiles
        logical :: is_cubic
        character(len=100), dimension(100) :: dumpfilenames

        namelist /MDParams_nml/ dumpfilenames, ndumpfiles, nchains, nbeads, &
        dt, dump_freq, nframes, is_cubic

        ! read namelist
        open (unit=10, file=nmlfilename, status='old')
            read (10, MDParams_nml)
        close (10)

        ! ユーザー定義型のインスタンスに読み込んだデータを代入
        params%dumpfilenames = dumpfilenames
        params%ndumpfiles = ndumpfiles
        params%nchains = nchains
        params%nbeads = nbeads
        params%dt = dt
        params%nframes = nframes
        params%dump_freq = dump_freq
        params%is_cubic = is_cubic
    end subroutine read_MDParams
end module

