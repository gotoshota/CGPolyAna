!> @file correlation_function.f90
!> @brief 相関関数計算を行うためのモジュール
!> @details 時系列データなどの相関関数を計算する機能を提供します
!> @author CGPolyAna開発チーム
!> @date 2025-03-16
!> @version 1.0
module correlation_function
    use error_handling
    implicit none
    
    !> @brief 1次元関数を格納する型
    !> @details 時系列データなどの1次元関数を格納し、相関関数を計算するために使用します
    type Function1D
        ! -- have to input -- !
        logical :: is_log = .true.                   !< 対数スケールかどうかのフラグ

        ! -- Input one of these -- !
        integer :: npoints = 0                       !< 点の数
        real :: base = 0                             !< 対数の底
        integer :: window_width                      !< 窓幅

        ! -- this is output -- !
        integer, allocatable :: frame_intervals(:)   !< フレーム間隔

        ! physical quantity
        double precision, allocatable :: y(:)        !< 関数の値
        double precision, allocatable :: x(:)        !< 時間
        
        type(ErrorInfo) :: error                     !< エラー情報
    contains
        procedure :: init => init_function1d         !< 初期化
    end type Function1D

    !> @brief 自己相関関数を計算するインターフェース
    interface calc_AutoFunction1D
        procedure calc_AutoCorrelationFunction_timeave, calc_AutoCorrelationFunction_particleave
    end interface calc_AutoFunction1D

contains
    !> @brief Function1D型の初期化
    !> @param[inout] self Function1Dオブジェクト
    !> @param[in] nmlfilename Namelistファイル名
    !> @return エラーコード（0: 成功, 非0: エラー）
    function init_function1d(self, nmlfilename) result(ierr)
        implicit none
        class(Function1D), intent(inout) :: self
        character(len=*), intent(in) :: nmlfilename
        integer :: ierr
        
        ! エラー情報の初期化
        call self%error%clear()
        ierr = 0
        
        ! Namelistファイルの読み込み
        ierr = read_Function1DInfo(nmlfilename, self)
    end function init_function1d

    !> @brief Namelistファイルから関数情報を読み込む
    !> @param[in] nmlfilename Namelistファイル名
    !> @param[out] func 読み込んだ情報を格納する変数
    !> @return エラーコード（0: 成功, 非0: エラー）
    function read_Function1DInfo(nmlfilename, func) result(ierr)
        implicit none
        character(len=*), intent(in) :: nmlfilename
        type(Function1D), intent(out) :: func
        integer :: ierr
        integer :: ios

        ! local variables
        logical :: is_log = .true.
        integer :: npoints = 0
        real :: base = 0
        integer :: window_width = 0

        namelist /Function1DInfo/ is_log, base, npoints, window_width

        ! エラー情報の初期化
        call func%error%clear()
        ierr = 0

        ! read namelist
        open (unit=10, file=nmlfilename, status='old', iostat=ios)
        if (ios /= 0) then
            call func%error%set(ERR_FILE_NOT_FOUND, "Failed to open namelist file: " // trim(nmlfilename), "read_Function1DInfo")
            ierr = ERR_FILE_NOT_FOUND
            return
        end if
        
        read (10, Function1DInfo, iostat=ios)
        if (ios /= 0) then
            call func%error%set(ERR_INVALID_FORMAT, "Failed to read Function1DInfo namelist", "read_Function1DInfo")
            ierr = ERR_INVALID_FORMAT
            close(10)
            return
        end if
        close (10)

        ! Substitute read data into func
        func%is_log = is_log

        if (npoints == 0 .and. base == 0 .and. window_width == 0) then
            call func%error%set(ERR_INVALID_PARAMETER, "Have to input at least 1 from following parameter: " // &
                "base (for log plot), window_width (for linear plot), npoints (for both plots).", "read_Function1DInfo")
            ierr = ERR_INVALID_PARAMETER
            return
        else if (npoints /= 0 .and. base == 0 .and. window_width == 0) then
            func%npoints = npoints
        else if (npoints == 0 .and. base /= 0 .and. window_width == 0 .and. func%is_log) then
            func%base = base
        else if (npoints == 0 .and. base == 0 .and. window_width /= 0 .and. .not. func%is_log) then
            func%window_width = window_width
        else if (npoints /= 0) then
            ! 警告を出力
            print *, "Warning: Two or more criteria have been input. Use npoints preferentially."
            func%npoints = npoints
        else
            call func%error%set(ERR_INVALID_PARAMETER, "Have to input criteria which matches to is_log.", "read_Function1DInfo")
            ierr = ERR_INVALID_PARAMETER
            return
        end if
    end function read_Function1DInfo

    !> @brief フレーム間隔を決定する
    !> @param[inout] func Function1Dオブジェクト
    !> @param[in] nframes フレーム数
    !> @param[in] dt 時間刻み（オプション）
    !> @param[in] dump_freq ダンプ頻度（オプション）
    !> @return エラーコード（0: 成功, 非0: エラー）
    function determine_frame_intervals(func, nframes, dt, dump_freq) result(ierr)
        implicit none
        type(Function1D), intent(inout) :: func
        integer, intent(in) :: nframes
        integer, intent(in), optional :: dump_freq
        double precision, intent(in), optional :: dt
        integer :: ierr
        integer :: i, alloc_stat

        ! エラー情報の初期化
        call func%error%clear()
        ierr = 0

        ! フレーム間隔の決定
        if (func%is_log) then
            if (func%npoints /= 0) then
                ierr = log_npoints(func, nframes)
                if (ierr /= 0) return
            else if (func%base /= 0) then
                ierr = log_base(func, nframes)
                if (ierr /= 0) return
            else
                call func%error%set(ERR_INVALID_PARAMETER, "Have to input criteria which matches to is_log.", "determine_frame_intervals")
                ierr = ERR_INVALID_PARAMETER
                return
            end if
        else
            if (func%npoints /= 0) then
                ierr = linear_npoints(func, nframes)
                if (ierr /= 0) return
            else if (func%window_width /= 0) then
                ierr = linear_window_width(func, nframes)
                if (ierr /= 0) return
            else
                call func%error%set(ERR_INVALID_PARAMETER, "Have to input criteria which matches to is_log.", "determine_frame_intervals")
                ierr = ERR_INVALID_PARAMETER
                return
            end if
        end if

        ! メモリ割り当て
        if (allocated(func%y)) deallocate(func%y)
        if (allocated(func%x)) deallocate(func%x)
        
        allocate(func%y(func%npoints), func%x(func%npoints), stat=alloc_stat)
        if (alloc_stat /= 0) then
            call func%error%set(ERR_MEMORY_ALLOCATION, "Failed to allocate memory for function values", "determine_frame_intervals")
            ierr = ERR_MEMORY_ALLOCATION
            return
        end if
        
        func%y = 0.0d0
        
        ! 時間軸の設定
        if (present(dt) .and. present(dump_freq)) then
            do i = 1, func%npoints
                func%x(i) = func%frame_intervals(i) * dt * dump_freq
            end do
        else
            do i = 1, func%npoints
                func%x(i) = dble(func%frame_intervals(i))
            end do
        end if
    end function determine_frame_intervals

    !> @brief 対数スケールでのフレーム間隔を決定する（点数指定）
    !> @param[inout] func Function1Dオブジェクト
    !> @param[in] nframes フレーム数
    !> @return エラーコード（0: 成功, 非0: エラー）
    function log_npoints(func, nframes) result(ierr)
        use, intrinsic :: iso_fortran_env, only: real64
        implicit none
        type(Function1D), intent(inout) :: func
        integer, intent(in) :: nframes
        integer :: ierr
        
        integer :: i, unique_count, alloc_stat
        real(real64) :: log_interval, current_value, previous_value
        real(real64), allocatable :: temp_intervals(:)

        ! エラー情報の初期化
        call func%error%clear()
        ierr = 0

        ! パラメータの検証
        if (func%npoints <= 0) then
            call func%error%set(ERR_INVALID_PARAMETER, "npoints must be positive", "log_npoints")
            ierr = ERR_INVALID_PARAMETER
            return
        end if
        
        if (nframes <= 0) then
            call func%error%set(ERR_INVALID_PARAMETER, "nframes must be positive", "log_npoints")
            ierr = ERR_INVALID_PARAMETER
            return
        end if

        print *, ""
        print *, "Determine the points of Time Correlation Function."
        print "(A, I0, A)", "The x-axis is equally devided into ", func%npoints, " fragments in log scale."
        
        ! nframes と npoints から log_interval を計算（real64で精度高く）
        log_interval = nframes**(1.0_real64/func%npoints)

        ! 一時配列の割り当て
        allocate(temp_intervals(func%npoints), stat=alloc_stat)
        if (alloc_stat /= 0) then
            call func%error%set(ERR_MEMORY_ALLOCATION, "Failed to allocate memory for temporary intervals", "log_npoints")
            ierr = ERR_MEMORY_ALLOCATION
            return
        end if
        
        unique_count = 0
        previous_value = 0.0_real64

        ! ログスケールでのサンプリング間隔の計算と重複のチェック
        current_value = 1.0_real64  ! 初期値を設定
        do i = 1, func%npoints
            if (i > 1) then
                current_value = current_value * log_interval
            end if
            ! 重複チェックとnframesを超えないように確認
            if (current_value > nframes) exit  ! nframes を超えたら終了
            if (int(current_value) > int(previous_value)) then
                unique_count = unique_count + 1
                temp_intervals(unique_count) = current_value
                previous_value = current_value
            end if
        end do

        ! 実際に使用するサンプリング間隔の数を更新
        func%npoints = unique_count

        ! frame_intervals の割り当てと更新
        if (allocated(func%frame_intervals)) deallocate(func%frame_intervals)
        
        allocate(func%frame_intervals(unique_count), stat=alloc_stat)
        if (alloc_stat /= 0) then
            call func%error%set(ERR_MEMORY_ALLOCATION, "Failed to allocate memory for frame intervals", "log_npoints")
            ierr = ERR_MEMORY_ALLOCATION
            deallocate(temp_intervals)
            return
        end if
        
        do i = 1, unique_count
            func%frame_intervals(i) = int(temp_intervals(i))
        end do

        ! 一時配列の解放
        deallocate(temp_intervals)
    end function log_npoints

    !> @brief 線形スケールでのフレーム間隔を決定する（窓幅指定）
    !> @param[inout] func Function1Dオブジェクト
    !> @param[in] nframes フレーム数
    !> @return エラーコード（0: 成功, 非0: エラー）
    function linear_window_width(func, nframes) result(ierr)
        use, intrinsic :: iso_fortran_env, only: real64
        implicit none
        type(Function1D), intent(inout) :: func
        integer, intent(in) :: nframes
        integer :: ierr
        
        integer :: i, interval_count, current_frame, alloc_stat

        ! エラー情報の初期化
        call func%error%clear()
        ierr = 0

        print *, ""
        print *, "Determine the points of Time Correlation Function."
        print "(A, I0, A)", "The x-axis is equally devided by ", func%window_width, " in linear space."

        ! パラメータの確認
        if (func%window_width <= 1) then
            call func%error%set(ERR_INVALID_PARAMETER, "window_width must be greater than 1", "linear_window_width")
            ierr = ERR_INVALID_PARAMETER
            return
        end if

        ! サンプリング間隔の数を計算
        interval_count = int(nframes/func%window_width) + 1
        if (interval_count < 1) then
            call func%error%set(ERR_INVALID_PARAMETER, "window_width is too large for the given nframes", "linear_window_width")
            ierr = ERR_INVALID_PARAMETER
            return
        end if

        ! frame_intervals の割り当て
        if (allocated(func%frame_intervals)) deallocate(func%frame_intervals)
        
        allocate(func%frame_intervals(interval_count), stat=alloc_stat)
        if (alloc_stat /= 0) then
            call func%error%set(ERR_MEMORY_ALLOCATION, "Failed to allocate memory for frame intervals", "linear_window_width")
            ierr = ERR_MEMORY_ALLOCATION
            return
        end if

        ! 等間隔でのサンプリング間隔の計算
        current_frame = 0
        do i = 1, interval_count
            func%frame_intervals(i) = current_frame
            current_frame = current_frame + func%window_width
        end do
        
        ! 点数の更新
        func%npoints = interval_count
    end function linear_window_width

    !> @brief 対数スケールでのフレーム間隔を決定する（底指定）
    !> @param[inout] func Function1Dオブジェクト
    !> @param[in] nframes フレーム数
    !> @return エラーコード（0: 成功, 非0: エラー）
    function log_base(func, nframes) result(ierr)
        use, intrinsic :: iso_fortran_env, only: real64
        implicit none
        integer, intent(in) :: nframes
        type(Function1D), intent(inout) :: func
        integer :: ierr
        
        integer :: i, unique_count, i_max, alloc_stat
        real(real64) :: current_value
        real(real64), allocatable :: temp_intervals(:)

        ! エラー情報の初期化
        call func%error%clear()
        ierr = 0

        print *, ""
        print *, "Determine the points of Time Correlation Function."
        print "(A, G0, A)", "The x-axis is equally devided with ", func%base, " as a base in log scale."

        ! パラメータの確認
        if (func%base <= 1.0_real64) then
            call func%error%set(ERR_INVALID_PARAMETER, "base must be greater than 1", "log_base")
            ierr = ERR_INVALID_PARAMETER
            return
        end if

        unique_count = 0
        i_max = int(log(real(nframes))/log(func%base)) + 1
        
        allocate(temp_intervals(i_max), stat=alloc_stat)
        if (alloc_stat /= 0) then
            call func%error%set(ERR_MEMORY_ALLOCATION, "Failed to allocate memory for temporary intervals", "log_base")
            ierr = ERR_MEMORY_ALLOCATION
            return
        end if

        ! ログスケールでのサンプリング間隔の計算と重複のチェック
        do i = 1, i_max
            current_value = func%base**i
            if (current_value > nframes) exit  ! 最大値を超えたら終了
            if (i == 1 .or. int(current_value) > int(temp_intervals(unique_count))) then
                unique_count = unique_count + 1
                temp_intervals(unique_count) = current_value
            end if
        end do

        ! 実際に使用するサンプリング間隔の数を更新
        func%npoints = unique_count

        ! frame_intervals の割り当てと更新
        if (allocated(func%frame_intervals)) deallocate(func%frame_intervals)
        
        allocate(func%frame_intervals(unique_count), stat=alloc_stat)
        if (alloc_stat /= 0) then
            call func%error%set(ERR_MEMORY_ALLOCATION, "Failed to allocate memory for frame intervals", "log_base")
            ierr = ERR_MEMORY_ALLOCATION
            deallocate(temp_intervals)
            return
        end if
        
        func%frame_intervals = int(temp_intervals(1:unique_count))

        ! 一時配列の解放
        deallocate(temp_intervals)
    end function log_base

    !> @brief 線形スケールでのフレーム間隔を決定する（点数指定）
    !> @param[inout] func Function1Dオブジェクト
    !> @param[in] nframes フレーム数
    !> @return エラーコード（0: 成功, 非0: エラー）
    function linear_npoints(func, nframes) result(ierr)
        use, intrinsic :: iso_fortran_env, only: real64
        implicit none
        type(Function1D), intent(inout) :: func
        integer, intent(in) :: nframes
        integer :: ierr
        
        integer :: i, interval_size, alloc_stat

        ! エラー情報の初期化
        call func%error%clear()
        ierr = 0

        print *, ""
        print *, "Determine the points of Time Correlation Function."
        print "(A, I0, A)", "The x-axis is equally devided into ", func%npoints, " fragments in linear space."

        ! パラメータの確認
        if (func%npoints < 2) then
            call func%error%set(ERR_INVALID_PARAMETER, "npoints must be at least 2", "linear_npoints")
            ierr = ERR_INVALID_PARAMETER
            return
        end if
        
        if (nframes < 2) then
            call func%error%set(ERR_INVALID_PARAMETER, "nframes must be at least 2", "linear_npoints")
            ierr = ERR_INVALID_PARAMETER
            return
        end if

        ! サンプリング間隔のサイズを計算（整数）
        interval_size = nframes/(func%npoints - 1)

        ! frame_intervals の割り当て
        if (allocated(func%frame_intervals)) deallocate(func%frame_intervals)
        
        allocate(func%frame_intervals(func%npoints), stat=alloc_stat)
        if (alloc_stat /= 0) then
            call func%error%set(ERR_MEMORY_ALLOCATION, "Failed to allocate memory for frame intervals", "linear_npoints")
            ierr = ERR_MEMORY_ALLOCATION
            return
        end if

        ! 等間隔でのサンプリング間隔の計算
        do i = 1, func%npoints
            func%frame_intervals(i) = (i - 1) * interval_size
        end do
    end function linear_npoints

    !> @brief 時間平均の自己相関関数を計算する
    !> @param[inout] func Function1Dオブジェクト
    !> @param[in] A 時系列データ
    !> @return エラーコード（0: 成功, 非0: エラー）
    function calc_AutoCorrelationFunction_timeave(func, A) result(ierr)
        implicit none
        type(Function1D), intent(inout) :: func
        double precision, intent(in) :: A(:)
        integer :: ierr
        
        ! local
        integer :: i, j
        integer :: nframes 

        ! エラー情報の初期化
        call func%error%clear()
        ierr = 0

        ! パラメータの検証
        if (.not. allocated(func%frame_intervals)) then
            call func%error%set(ERR_INVALID_PARAMETER, "frame_intervals not initialized", "calc_AutoCorrelationFunction_timeave")
            ierr = ERR_INVALID_PARAMETER
            return
        end if
        
        if (.not. allocated(func%y)) then
            call func%error%set(ERR_INVALID_PARAMETER, "function values not initialized", "calc_AutoCorrelationFunction_timeave")
            ierr = ERR_INVALID_PARAMETER
            return
        end if

        nframes = size(A)
        
        ! Calculate correlation function
        do i = 1, func%npoints
            func%y(i) = 0.0d0
            do j = 1, nframes - func%frame_intervals(i) 
                func%y(i) = func%y(i) + A(func%frame_intervals(i) - func%frame_intervals(j) + 1) * A(func%frame_intervals(j))
            end do
            func%y(i) = func%y(i) / func%frame_intervals(i)
        end do
    end function calc_AutoCorrelationFunction_timeave

    !> @brief 粒子平均の自己相関関数を計算する
    !> @param[inout] func Function1Dオブジェクト
    !> @param[in] A 時系列データ（粒子ごと）
    !> @return エラーコード（0: 成功, 非0: エラー）
    function calc_AutoCorrelationFunction_particleave(func, A) result(ierr)
        implicit none
        type(Function1D), intent(inout) :: func
        double precision, intent(in) :: A(:,:)
        integer :: ierr
        
        ! local
        integer :: i, j, k
        integer :: nframes 
        integer :: nparticles 
        double precision :: temp

        ! エラー情報の初期化
        call func%error%clear()
        ierr = 0

        ! パラメータの検証
        if (.not. allocated(func%frame_intervals)) then
            call func%error%set(ERR_INVALID_PARAMETER, "frame_intervals not initialized", "calc_AutoCorrelationFunction_particleave")
            ierr = ERR_INVALID_PARAMETER
            return
        end if
        
        if (.not. allocated(func%y)) then
            call func%error%set(ERR_INVALID_PARAMETER, "function values not initialized", "calc_AutoCorrelationFunction_particleave")
            ierr = ERR_INVALID_PARAMETER
            return
        end if

        nframes = size(A, 2)
        nparticles = size(A, 1)
        
        ! Calculate correlation function
        do i = 1, func%npoints
            func%y(i) = 0.0d0
            do j = 1, nframes - func%frame_intervals(i) 
                temp = 0.0d0
                do k = 1, nparticles
                    temp = temp + A(k, func%frame_intervals(i) - func%frame_intervals(j) + 1) * A(k, func%frame_intervals(j))
                end do
                func%y(i) = func%y(i) + temp / nparticles
            end do
            func%y(i) = func%y(i) / func%frame_intervals(i)
        end do
    end function calc_AutoCorrelationFunction_particleave

end module correlation_function
