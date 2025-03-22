!> @file global_types.f90
!> @brief グローバルな型定義を提供するモジュール
!> @details 分子動力学シミュレーションのパラメータを格納する型を定義します

!> @module global_types
!> @brief グローバルな型定義モジュール
module global_types
    implicit none

    !> @type MDParams
    !> @brief 分子動力学シミュレーションのパラメータを格納する型
    type MDParams
        ! Read from Namelist
        !> @var character(LEN=100), dimension(100) :: dumpfilenames
        !> @brief dump fileのリスト
        character(LEN=100), dimension(100) :: dumpfilenames            ! dump file のリスト
        
        !> @var integer :: ndumpfiles
        !> @brief dump fileの数
        integer :: ndumpfiles = 1           ! dump file の数
        
        !> @var integer :: nchains
        !> @brief 分子数
        integer :: nchains = 0              ! 分子数
        
        !> @var integer :: nbeads
        !> @brief 重合度
        integer :: nbeads = 0               ! 重合度
        
        !> @var integer :: nframes
        !> @brief フレーム数
        integer :: nframes = 0              ! フレーム数
        
        !> @var integer :: dump_freq
        !> @brief ダンプの書き出し頻度
        integer :: dump_freq = 0            ! ダンプの書き出し頻度
        
        !> @var double precision :: dt
        !> @brief 時間の刻み幅
        double precision :: dt = 0          ! 時間の刻み幅
        
        !> @var logical :: is_cubic
        !> @brief 立方格子かどうかのフラグ
        logical :: is_cubic = .true.        ! 立方格子かどうかのフラグ
        
        !> @var logical :: is_wrap
        !> @brief wrap or unwrap
        logical :: is_wrap = .true.         ! wrap or unwrap
        contains
            !> @brief 初期化メソッド
            procedure :: init
    end type
contains
    !> @brief MDParamsをNamelistファイルから読み込む
    !> @param[in] nmlfilename Namelistファイル名
    !> @param[inout] params MDParamsオブジェクト
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

    !> @brief MDParamsオブジェクトを初期化する
    !> @param[inout] this MDParamsオブジェクト
    !> @param[in] nmlfilename Namelistファイル名
    subroutine init(this, nmlfilename)
        implicit none
        class(MDParams), intent(inout) :: this
        character(len=*), intent(in) :: nmlfilename

        call read_MDParams(nmlfilename, this)
    end subroutine init
end module

