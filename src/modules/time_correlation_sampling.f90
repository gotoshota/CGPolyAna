module time_correlation_sampling
    use global_types
    implicit none
    
    type TimeCorrelationInfo
        ! -- have to input -- !
        logical                 :: is_log = .true.

        ! -- Input one of these -- !
        integer                 :: npoints = 0
        real                    :: base = 0
        integer                 :: window_width
        
        ! -- this is output -- !
        integer, allocatable    :: frame_intervals(:)
    end type

contains 
    subroutine read_TimeCorrelationInfo(nmlfilename, TCinfo)
        implicit none

        character(len=*), intent(in) :: nmlfilename
        type(TimeCorrelationInfo), INTENT(OUT) :: TCinfo

        ! local variables
        logical                 :: is_log = .true.
        integer                 :: npoints = 0
        real                    :: base = 0
        integer                 :: window_width = 0

        namelist /TimeCorrelation_params/ is_log, base, npoints, window_width

        ! read namelist
        open(unit=10, file=nmlfilename, status='old')
            read(10, TimeCorrelation_params)
        close(10)

        ! Substitute read data into TCinfo
        TCinfo%is_log = is_log

        if (npoints == 0 .and. base==0 .and. window_width == 0) then
            print *, "Error: Have to input at least 1 from following parameter:"
            print *, "base (for log plot), window_width (for linear plot), npoints (for both plots)."
            stop

        else if (npoints /= 0 .and. base==0 .and. window_width == 0) then
            TCinfo%npoints = npoints

        else if (npoints == 0 .and. base/=0 .and. window_width == 0 .and. TCinfo%is_log .eqv. .true.) then
            TCinfo%base = base

        else if (npoints == 0 .and. base==0 .and. window_width /= 0 .and. TCinfo%is_log .eqv. .false.) then
            TCinfo%window_width = window_width

        else if (npoints /= 0) then
            print *, "Warning: Two or more criteria have been input. Use npoints preferentially."
            TCinfo%npoints = npoints
        
        else 
            print *, "Error: Have to input criteria which matches to is_log."
            stop

        end if
        
    end subroutine 

    subroutine determine_frame_intervals(TCinfo, nframes)
        implicit none

        type(TimeCorrelationInfo), INTENT(INOUT) :: TCinfo
        integer, intent(in) :: nframes

        if (TCinfo%is_log) then
            if (TCinfo%npoints /= 0) then
                call log_npoints(TCinfo, nframes)

            else if (TCinfo%base /= 0) then
                call log_base(TCinfo, nframes)
           
            else 
                print *, "Error: Have to input criteria which matches to is_log."
                stop

            end if

        else 
            if (TCinfo%npoints /= 0) then
                call linear_npoints(TCinfo, nframes)

            else if (TCinfo%window_width /= 0) then
                call linear_window_width(TCinfo, nframes)

            else
                print *, "Error: Have to input criteria which matches to is_log."
                stop

            end if
        end if
    end subroutine

    subroutine log_npoints(TCinfo, nframes)
        use, intrinsic :: iso_fortran_env, only: real64
        type(TimeCorrelationInfo), intent(inout) :: TCinfo
        integer, intent(in) :: nframes
        integer :: i, unique_count
        real(real64) :: log_interval, current_value, previous_value
        real(real64), allocatable :: temp_intervals(:)

        print *, ""
        print *, "Determine the points of Time Correlation Function."
        print "(A, I0, A)", "The x-axis is equally devided into ", TCinfo%npoints, " fragments in log scale."
        ! nframes と npoints から log_interval を計算（real64で精度高く）
        log_interval = nframes**(1.0_real64 / TCinfo%npoints)

        ! 一時配列の割り当て
        allocate(temp_intervals(TCinfo%npoints))
        unique_count = 0
        previous_value = 0.0_real64

        ! ログスケールでのサンプリング間隔の計算と重複のチェック
        current_value = 1.0_real64  ! 初期値を設定
        do i = 1, TCinfo%npoints
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
        TCinfo%npoints = unique_count

        ! frame_intervals の割り当てと更新
        allocate(TCinfo%frame_intervals(unique_count))
        do i = 1, unique_count
            TCinfo%frame_intervals(i) = int(temp_intervals(i))
        end do

        ! 一時配列の解放
        deallocate(temp_intervals)
    end subroutine log_npoints

    subroutine linear_window_width(TCinfo, nframes)
        use, intrinsic :: iso_fortran_env, only: real64
        type(TimeCorrelationInfo), intent(inout) :: TCinfo
        integer, intent(in) :: nframes
        integer :: i, interval_count, current_frame

        print *, ""
        print *, "Determine the points of Time Correlation Function."
        print "(A, I0, A)", "The x-axis is equally devided by ", TCinfo%window_width, " in linear space."

        ! パラメータの確認
        if (TCinfo%window_width <= 1) then
            print *, "Error: window_width must be greater than 1."
            return
        end if

        ! サンプリング間隔の数を計算
        interval_count = int(nframes / TCinfo%window_width) + 1
        if (interval_count < 1) then
            print *, "Error: window_width is too large for the given nframes."
            return
        end if

        ! frame_intervals の割り当て
        allocate(TCinfo%frame_intervals(interval_count))

        ! 等間隔でのサンプリング間隔の計算
        current_frame = 0
        do i = 1, interval_count
            TCinfo%frame_intervals(i) = current_frame
            current_frame = current_frame + TCinfo%window_width
        end do
    end subroutine linear_window_width

    subroutine log_base(TCinfo, nframes)
        use, intrinsic :: iso_fortran_env, only: real64
        integer, intent(in) :: nframes
        type(TimeCorrelationInfo), intent(inout) :: TCinfo
        integer :: i, unique_count, i_max
        real(real64) :: current_value
        real(real64), allocatable :: temp_intervals(:) 

        print *, ""
        print *, "Determine the points of Time Correlation Function."
        print "(A, G0, A)", "The x-axis is equally devided with ", TCinfo%base, " as a base in log scale."

        ! パラメータの確認
        if (TCinfo%base <= 1.0_real64) then
            print *, "Error: base must be greater than 1."
            return
        end if

        unique_count = 0
        i_max = int(LOG(real(nframes)) / LOG(TCinfo%base)) + 1
        ALLOCATE(temp_intervals(i_max))

        ! ログスケールでのサンプリング間隔の計算と重複のチェック
        do i = 1, i_max
            current_value = TCinfo%base**i
            if (current_value > nframes) exit  ! 最大値を超えたら終了
            if (i == 1 .or. int(current_value) > int(temp_intervals(unique_count))) then
                unique_count = unique_count + 1
                temp_intervals(unique_count) = current_value
            end if
        end do

        ! 実際に使用するサンプリング間隔の数を更新
        TCinfo%npoints = unique_count

        ! frame_intervals の割り当てと更新
        allocate(TCinfo%frame_intervals(unique_count))
        TCinfo%frame_intervals = int(temp_intervals(1:unique_count))

        ! 一時配列の解放
        deallocate(temp_intervals)
    end subroutine log_base

    subroutine linear_npoints(TCinfo, nframes)
        use, intrinsic :: iso_fortran_env, only: real64
        type(TimeCorrelationInfo), intent(inout) :: TCinfo
        integer, intent(in) :: nframes
        integer :: i, interval_size

        print *, ""
        print *, "Determine the points of Time Correlation Function."
        print "(A, I0, A)", "The x-axis is equally devided into ", TCinfo%npoints, " fragments in linear space."

        ! パラメータの確認
        if (TCinfo%npoints < 2) then
            print *, "Error: npoints must be at least 2."
            return
        end if
        if (nframes < 2) then
            print *, "Error: nframes must be at least 2."
            return
        end if


        ! サンプリング間隔のサイズを計算（整数）
        interval_size = nframes / (TCinfo%npoints - 1)

        ! frame_intervals の割り当て
        allocate(TCinfo%frame_intervals(TCinfo%npoints))

        ! 等間隔でのサンプリング間隔の計算
        do i = 1, TCinfo%npoints
            TCinfo%frame_intervals(i) = (i - 1) * interval_size
        end do
    end subroutine linear_npoints



end module 
