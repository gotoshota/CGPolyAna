!> @file global_types.f90
!> @brief 基本的なデータ型の定義を提供するモジュール
!> @details 分子動力学シミュレーションのパラメータを格納する型などを定義します
!> @author CGPolyAna開発チーム
!> @date 2025-03-16
!> @version 1.0
module global_types
    use error_handling
    implicit none

    !> @brief 分子動力学シミュレーションのパラメータを格納する型
    !> @details LAMMPSトラジェクトリファイルの解析に必要なパラメータを格納します
    type MDParams
        ! Read from Namelist
        character(LEN=100), dimension(100) :: dumpfilenames            !< dump file のリスト
        integer :: ndumpfiles = 1           !< dump file の数
        integer :: nchains = 0              !< 分子数
        integer :: nbeads = 0               !< 重合度
        integer :: nframes = 0              !< フレーム数
        integer :: dump_freq = 0            !< ダンプの書き出し頻度
        double precision :: dt = 0          !< 時間の刻み幅
        logical :: is_cubic = .true.        !< 立方格子かどうかのフラグ
        logical :: is_wrap = .true.         !< wrap or unwrap
        type(ErrorInfo) :: error            !< エラー情報
        contains
            procedure :: init               !< パラメータの初期化
            procedure :: validate           !< パラメータの検証
    end type

contains
    !> @brief Namelistファイルからパラメータを読み込む
    !> @param[in] nmlfilename Namelistファイル名
    !> @param[out] params 読み込んだパラメータを格納する変数
    !> @return エラーコード（0: 成功, 非0: エラー）
    subroutine read_MDParams(nmlfilename, params)
        implicit none
        character(len=*), intent(in) :: nmlfilename
        type(MDParams), intent(inout) :: params
        integer :: ierr

        ! local variables
        integer :: nchains, nbeads, nframes
        double precision :: dt
        integer :: dump_freq, ndumpfiles
        logical :: is_cubic
        character(len=100), dimension(100) :: dumpfilenames
        integer :: ios

        namelist /MDParams_nml/ dumpfilenames, ndumpfiles, nchains, nbeads, &
        dt, dump_freq, nframes, is_cubic

        ! エラー情報の初期化
        call params%error%clear()
        ierr = 0

        ! read namelist
        open (unit=10, file=nmlfilename, status='old', iostat=ios)
        if (ios /= 0) then
            call params%error%set(ERR_FILE_NOT_FOUND, &
                 "Failed to open namelist file: " // trim(nmlfilename), &
                 "read_MDParams")
            ierr = ERR_FILE_NOT_FOUND
            return
        end if

        read (10, MDParams_nml, iostat=ios)
        if (ios /= 0) then
            call params%error%set(ERR_INVALID_FORMAT, &
                 "Failed to read namelist: " // trim(nmlfilename), &
                 "read_MDParams")
            ierr = ERR_INVALID_FORMAT
            close(10)
            return
        end if
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

        ! パラメータの検証
        ierr = params%validate()
    end subroutine read_MDParams

    !> @brief パラメータの初期化
    !> @param[inout] this MDParamsオブジェクト
    !> @param[in] nmlfilename Namelistファイル名
    !> @return エラーコード（0: 成功, 非0: エラー）
    function init(this, nmlfilename) result(ierr)
        implicit none
        class(MDParams), intent(inout) :: this
        character(len=*), intent(in) :: nmlfilename
        integer :: ierr

        call read_MDParams(nmlfilename, this)
        ierr = 0
    end function init

    !> @brief パラメータの検証
    !> @param[inout] this MDParamsオブジェクト
    !> @return エラーコード（0: 成功, 非0: エラー）
    function validate(this) result(ierr)
        implicit none
        class(MDParams), intent(inout) :: this
        integer :: ierr
        character(len=32) :: err_str
        
        ierr = 0
        
        ! 基本的なパラメータの検証
        if (this%nchains <= 0) then
            write(err_str, '(I10)') this%nchains
            call this%error%set(ERR_INVALID_PARAMETER, &
                 "Invalid number of chains: " // trim(adjustl(err_str)), &
                 "MDParams%validate")
            ierr = ERR_INVALID_PARAMETER
            return
        end if
        
        if (this%nbeads <= 0) then
            write(err_str, '(I10)') this%nbeads
            call this%error%set(ERR_INVALID_PARAMETER, &
                 "Invalid number of beads: " // trim(adjustl(err_str)), &
                 "MDParams%validate")
            ierr = ERR_INVALID_PARAMETER
            return
        end if
        
        if (this%nframes <= 0) then
            write(err_str, '(I10)') this%nframes
            call this%error%set(ERR_INVALID_PARAMETER, &
                 "Invalid number of frames: " // trim(adjustl(err_str)), &
                 "MDParams%validate")
            ierr = ERR_INVALID_PARAMETER
            return
        end if
        
        if (this%dt <= 0.0) then
            write(err_str, '(F15.6)') this%dt
            call this%error%set(ERR_INVALID_PARAMETER, &
                 "Invalid time step: " // trim(adjustl(err_str)), &
                 "MDParams%validate")
            ierr = ERR_INVALID_PARAMETER
            return
        end if
        
        if (this%dump_freq <= 0) then
            write(err_str, '(I10)') this%dump_freq
            call this%error%set(ERR_INVALID_PARAMETER, &
                 "Invalid dump frequency: " // trim(adjustl(err_str)), &
                 "MDParams%validate")
            ierr = ERR_INVALID_PARAMETER
            return
        end if
        
        ! dumpfilenamesの検証
        if (this%ndumpfiles <= 0) then
            write(err_str, '(I10)') this%ndumpfiles
            call this%error%set(ERR_INVALID_PARAMETER, &
                 "Invalid number of dump files: " // trim(adjustl(err_str)), &
                 "MDParams%validate")
            ierr = ERR_INVALID_PARAMETER
            return
        end if
        
        ! 各dumpfileの存在確認は省略（実際の実装では必要に応じて追加）
    end function validate
end module
