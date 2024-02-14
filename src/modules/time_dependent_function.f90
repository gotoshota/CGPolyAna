module time_dependent_function
    use global_types
    implicit none
    
    type TimeDependentFunction
        ! -- have to input -- !
        logical                 :: is_log = .true.

        ! -- Input one of these -- !
        integer                 :: npoints = 0
        real                    :: base = 0
        integer                 :: window_width
        
        ! -- this is output -- !
        integer, allocatable    :: frame_intervals(:)

        ! physical quantity 
        DOUBLE PRECISION, ALLOCATABLE :: y(:) ! any quantity
        DOUBLE PRECISION, ALLOCATABLE :: t(:) ! Time

    end type

contains 
    subroutine read_TimeDependentFunctionInfo(nmlfilename, TDFunc)
        implicit none

        character(len=*), intent(in) :: nmlfilename
        type(TimeDependentFunction), INTENT(OUT) :: TDFunc

        ! local variables
        logical                 :: is_log = .true.
        integer                 :: npoints = 0
        real                    :: base = 0
        integer                 :: window_width = 0

        namelist /TimeDependentFunctionInfo/ is_log, base, npoints, window_width

        ! read namelist
        open(unit=10, file=nmlfilename, status='old')
            read(10, TimeDependentFunctionInfo)
        close(10)

        ! Substitute read data into TDFunc
        TDFunc%is_log = is_log

        if (npoints == 0 .and. base==0 .and. window_width == 0) then
            print *, "Error: Have to input at least 1 from following parameter:"
            print *, "base (for log plot), window_width (for linear plot), npoints (for both plots)."
            stop

        else if (npoints /= 0 .and. base==0 .and. window_width == 0) then
            TDFunc%npoints = npoints

        else if (npoints == 0 .and. base/=0 .and. window_width == 0 .and. TDFunc%is_log .eqv. .true.) then
            TDFunc%base = base

        else if (npoints == 0 .and. base==0 .and. window_width /= 0 .and. TDFunc%is_log .eqv. .false.) then
            TDFunc%window_width = window_width

        else if (npoints /= 0) then
            print *, "Warning: Two or more criteria have been input. Use npoints preferentially."
            TDFunc%npoints = npoints
        
        else 
            print *, "Error: Have to input criteria which matches to is_log."
            stop

        end if

    end subroutine 

    subroutine determine_frame_intervals(TDFunc, traj)
        implicit none

        TYPE(trajectory), INTENT(IN)               :: traj
        type(TimeDependentFunction), INTENT(INOUT) :: TDFunc

        integer :: i

        if (TDFunc%is_log) then
            if (TDFunc%npoints /= 0) then
                call log_npoints(TDFunc, traj%nframes)

            else if (TDFunc%base /= 0) then
                call log_base(TDFunc, traj%nframes)
           
            else 
                print *, "Error: Have to input criteria which matches to is_log."
                stop

            end if

        else 
            if (TDFunc%npoints /= 0) then
                call linear_npoints(TDFunc, traj%nframes)

            else if (TDFunc%window_width /= 0) then
                call linear_window_width(TDFunc, traj%nframes)

            else
                print *, "Error: Have to input criteria which matches to is_log."
                stop

            end if
        end if

        ALLOCATE(TDFunc%y(TDFunc%npoints), TDFunc%t(TDFunc%npoints))
        do i = 1, TDFunc%npoints
            TDFunc%t(i) = TDFunc%frame_intervals(i) * traj%dt * traj%dump_freq 
        enddo
    end subroutine

    subroutine log_npoints(TDFunc, nframes)
        use, intrinsic :: iso_fortran_env, only: real64
        type(TimeDependentFunction), intent(inout) :: TDFunc
        integer, intent(in) :: nframes
        integer :: i, unique_count
        real(real64) :: log_interval, current_value, previous_value
        real(real64), allocatable :: temp_intervals(:)

        print *, ""
        print *, "Determine the points of Time Correlation Function."
        print "(A, I0, A)", "The x-axis is equally devided into ", TDFunc%npoints, " fragments in log scale."
        ! nframes と npoints から log_interval を計算（real64で精度高く）
        log_interval = nframes**(1.0_real64 / TDFunc%npoints)

        ! 一時配列の割り当て
        allocate(temp_intervals(TDFunc%npoints))
        unique_count = 0
        previous_value = 0.0_real64

        ! ログスケールでのサンプリング間隔の計算と重複のチェック
        current_value = 1.0_real64  ! 初期値を設定
        do i = 1, TDFunc%npoints
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
        TDFunc%npoints = unique_count

        ! frame_intervals の割り当てと更新
        allocate(TDFunc%frame_intervals(unique_count))
        do i = 1, unique_count
            TDFunc%frame_intervals(i) = int(temp_intervals(i))
        end do

        ! 一時配列の解放
        deallocate(temp_intervals)
    end subroutine log_npoints

    subroutine linear_window_width(TDFunc, nframes)
        use, intrinsic :: iso_fortran_env, only: real64
        type(TimeDependentFunction), intent(inout) :: TDFunc
        integer, intent(in) :: nframes
        integer :: i, interval_count, current_frame

        print *, ""
        print *, "Determine the points of Time Correlation Function."
        print "(A, I0, A)", "The x-axis is equally devided by ", TDFunc%window_width, " in linear space."

        ! パラメータの確認
        if (TDFunc%window_width <= 1) then
            print *, "Error: window_width must be greater than 1."
            return
        end if

        ! サンプリング間隔の数を計算
        interval_count = int(nframes / TDFunc%window_width) + 1
        if (interval_count < 1) then
            print *, "Error: window_width is too large for the given nframes."
            return
        end if

        ! frame_intervals の割り当て
        allocate(TDFunc%frame_intervals(interval_count))

        ! 等間隔でのサンプリング間隔の計算
        current_frame = 0
        do i = 1, interval_count
            TDFunc%frame_intervals(i) = current_frame
            current_frame = current_frame + TDFunc%window_width
        end do
    end subroutine linear_window_width

    subroutine log_base(TDFunc, nframes)
        use, intrinsic :: iso_fortran_env, only: real64
        integer, intent(in) :: nframes
        type(TimeDependentFunction), intent(inout) :: TDFunc
        integer :: i, unique_count, i_max
        real(real64) :: current_value
        real(real64), allocatable :: temp_intervals(:) 

        print *, ""
        print *, "Determine the points of Time Correlation Function."
        print "(A, G0, A)", "The x-axis is equally devided with ", TDFunc%base, " as a base in log scale."

        ! パラメータの確認
        if (TDFunc%base <= 1.0_real64) then
            print *, "Error: base must be greater than 1."
            return
        end if

        unique_count = 0
        i_max = int(LOG(real(nframes)) / LOG(TDFunc%base)) + 1
        ALLOCATE(temp_intervals(i_max))

        ! ログスケールでのサンプリング間隔の計算と重複のチェック
        do i = 1, i_max
            current_value = TDFunc%base**i
            if (current_value > nframes) exit  ! 最大値を超えたら終了
            if (i == 1 .or. int(current_value) > int(temp_intervals(unique_count))) then
                unique_count = unique_count + 1
                temp_intervals(unique_count) = current_value
            end if
        end do

        ! 実際に使用するサンプリング間隔の数を更新
        TDFunc%npoints = unique_count

        ! frame_intervals の割り当てと更新
        allocate(TDFunc%frame_intervals(unique_count))
        TDFunc%frame_intervals = int(temp_intervals(1:unique_count))

        ! 一時配列の解放
        deallocate(temp_intervals)
    end subroutine log_base

    subroutine linear_npoints(TDFunc, nframes)
        use, intrinsic :: iso_fortran_env, only: real64
        type(TimeDependentFunction), intent(inout) :: TDFunc
        integer, intent(in) :: nframes
        integer :: i, interval_size

        print *, ""
        print *, "Determine the points of Time Correlation Function."
        print "(A, I0, A)", "The x-axis is equally devided into ", TDFunc%npoints, " fragments in linear space."

        ! パラメータの確認
        if (TDFunc%npoints < 2) then
            print *, "Error: npoints must be at least 2."
            return
        end if
        if (nframes < 2) then
            print *, "Error: nframes must be at least 2."
            return
        end if


        ! サンプリング間隔のサイズを計算（整数）
        interval_size = nframes / (TDFunc%npoints - 1)

        ! frame_intervals の割り当て
        allocate(TDFunc%frame_intervals(TDFunc%npoints))

        ! 等間隔でのサンプリング間隔の計算
        do i = 1, TDFunc%npoints
            TDFunc%frame_intervals(i) = (i - 1) * interval_size
        end do
    end subroutine linear_npoints



end module 
