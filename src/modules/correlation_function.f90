module correlation_function
    implicit none
    
    type Function1D
        ! -- have to input -- !
        logical :: is_log = .true.

        ! -- Input one of these -- !
        integer :: npoints = 0
        real :: base = 0
        integer :: window_width
        
        ! -- this is output -- !
        integer, allocatable :: frame_intervals(:)

        ! physical quantity
        double precision, allocatable :: y(:) ! any quantity
        double precision, allocatable :: x(:) ! Time
    end type

    interface calc_AutoFunction1D
        procedure calc_AutoCorrelationFunction_timeave, calc_AutoCorrelationFunction_particleave
    end interface

contains
    subroutine read_Function1DInfo(nmlfilename, func)
        implicit none

        character(len=*), intent(in) :: nmlfilename
        type(Function1D), intent(OUT) :: func

        ! local variables
        logical :: is_log = .true.
        integer :: npoints = 0
        real :: base = 0
        integer :: window_width = 0

        namelist /Function1DInfo/ is_log, base, npoints, window_width

        ! read namelist
        open (unit=10, file=nmlfilename, status='old')
            read (10, Function1DInfo)
        close (10)

        ! Substitute read data into func
        func%is_log = is_log

        if (npoints .eq. 0 .and. base .eq. 0 .and. window_width .eq. 0) then
            print *, "Error: Have to input at least 1 from following parameter:"
            print *, "base (for log plot), window_width (for linear plot), npoints (for both plots)."
            stop

        else if (npoints .ne. 0 .and. base .eq. 0 .and. window_width .eq. 0) then
            func%npoints = npoints

        else if (npoints .eq. 0 .and. base .ne. 0 .and. window_width .eq. 0 .and. func%is_log .eqv. .true.) then
            func%base = base

        else if (npoints .eq. 0 .and. base .eq. 0 .and. window_width .ne. 0 .and. func%is_log .eqv. .false.) then
            func%window_width = window_width

        else if (npoints .ne. 0) then
            print *, "Warning: Two or more criteria have been input. Use npoints preferentially."
            func%npoints = npoints
        
        else
            print *, "Error: Have to input criteria which matches to is_log."
            stop

        end if

    end subroutine

    subroutine determine_frame_intervals(func, nframes, dt, dump_freq)
        implicit none

        type(Function1D), intent(INOUT) :: func
        integer, intent(in) :: nframes
        integer, intent(in), optional :: dump_freq
        double precision, intent(in), optional :: dt

        integer :: i

        if (func%is_log) then
            if (func%npoints .ne. 0) then
                call log_npoints(func, nframes)

            else if (func%base .ne. 0) then
                call log_base(func, nframes)
           
            else
                print *, "Error: Have to input criteria which matches to is_log."
                stop

            end if

        else
            if (func%npoints .ne. 0) then
                call linear_npoints(func, nframes)

            else if (func%window_width .ne. 0) then
                call linear_window_width(func, nframes)

            else
                print *, "Error: Have to input criteria which matches to is_log."
                stop

            end if
        end if

        allocate (func%y(func%npoints), func%x(func%npoints))
        if (present(dt) .and. present(dump_freq)) then
            do i = 1, func%npoints
                func%x(i) = func%frame_intervals(i)*dt*dump_freq
            end do
        end if
    end subroutine

    subroutine log_npoints(func, nframes)
        use, intrinsic :: iso_fortran_env, only: real64
        type(Function1D), intent(inout) :: func
        integer, intent(in) :: nframes
        integer :: i, unique_count
        real(real64) :: log_interval, current_value, previous_value
        real(real64), allocatable :: temp_intervals(:)

        print *, ""
        print *, "Determine the points of Time Correlation Function."
        print "(A, I0, A)", "The x-axis is equally devided into ", func%npoints, " fragments in log scale."
        ! nframes と npoints から log_interval を計算（real64で精度高く）
        log_interval = nframes**(1.0_real64/func%npoints)

        ! 一時配列の割り当て
        allocate (temp_intervals(func%npoints))
        unique_count = 0
        previous_value = 0.0_real64

        ! ログスケールでのサンプリング間隔の計算と重複のチェック
        current_value = 1.0_real64  ! 初期値を設定
        do i = 1, func%npoints
            if (i .gt. 1) then
                current_value = current_value*log_interval
            end if
            ! 重複チェックとnframesを超えないように確認
            if (current_value .gt. nframes) exit  ! nframes を超えたら終了
            if (int(current_value) .gt. int(previous_value)) then
                unique_count = unique_count + 1
                temp_intervals(unique_count) = current_value
                previous_value = current_value
            end if
        end do

        ! 実際に使用するサンプリング間隔の数を更新
        func%npoints = unique_count

        ! frame_intervals の割り当てと更新
        allocate (func%frame_intervals(unique_count))
        do i = 1, unique_count
            func%frame_intervals(i) = int(temp_intervals(i))
        end do

        ! 一時配列の解放
        deallocate (temp_intervals)
    end subroutine log_npoints

    subroutine linear_window_width(func, nframes)
        use, intrinsic :: iso_fortran_env, only: real64
        type(Function1D), intent(inout) :: func
        integer, intent(in) :: nframes
        integer :: i, interval_count, current_frame

        print *, ""
        print *, "Determine the points of Time Correlation Function."
        print "(A, I0, A)", "The x-axis is equally devided by ", func%window_width, " in linear space."

        ! パラメータの確認
        if (func%window_width .le. 1) then
            print *, "Error: window_width must be greater than 1."
            return
        end if

        ! サンプリング間隔の数を計算
        interval_count = int(nframes/func%window_width) + 1
        if (interval_count .lt. 1) then
            print *, "Error: window_width is too large for the given nframes."
            return
        end if

        ! frame_intervals の割り当て
        allocate (func%frame_intervals(interval_count))

        ! 等間隔でのサンプリング間隔の計算
        current_frame = 0
        do i = 1, interval_count
            func%frame_intervals(i) = current_frame
            current_frame = current_frame + func%window_width
        end do
    end subroutine linear_window_width

    subroutine log_base(func, nframes)
        use, intrinsic :: iso_fortran_env, only: real64
        integer, intent(in) :: nframes
        type(Function1D), intent(inout) :: func
        integer :: i, unique_count, i_max
        real(real64) :: current_value
        real(real64), allocatable :: temp_intervals(:)

        print *, ""
        print *, "Determine the points of Time Correlation Function."
        print "(A, G0, A)", "The x-axis is equally devided with ", func%base, " as a base in log scale."

        ! パラメータの確認
        if (func%base .le. 1.0_real64) then
            print *, "Error: base must be greater than 1."
            return
        end if

        unique_count = 0
        i_max = int(log(real(nframes))/log(func%base)) + 1
        allocate (temp_intervals(i_max))

        ! ログスケールでのサンプリング間隔の計算と重複のチェック
        do i = 1, i_max
            current_value = func%base**i
            if (current_value .gt. nframes) exit  ! 最大値を超えたら終了
            if (i .eq. 1 .or. int(current_value) .gt. int(temp_intervals(unique_count))) then
                unique_count = unique_count + 1
                temp_intervals(unique_count) = current_value
            end if
        end do

        ! 実際に使用するサンプリング間隔の数を更新
        func%npoints = unique_count

        ! frame_intervals の割り当てと更新
        allocate (func%frame_intervals(unique_count))
        func%frame_intervals = int(temp_intervals(1:unique_count))

        ! 一時配列の解放
        deallocate (temp_intervals)
    end subroutine log_base

    subroutine linear_npoints(func, nframes)
        use, intrinsic :: iso_fortran_env, only: real64
        type(Function1D), intent(inout) :: func
        integer, intent(in) :: nframes
        integer :: i, interval_size

        print *, ""
        print *, "Determine the points of Time Correlation Function."
        print "(A, I0, A)", "The x-axis is equally devided into ", func%npoints, " fragments in linear space."

        ! パラメータの確認
        if (func%npoints .lt. 2) then
            print *, "Error: npoints must be at least 2."
            return
        end if
        if (nframes .lt. 2) then
            print *, "Error: nframes must be at least 2."
            return
        end if

        ! サンプリング間隔のサイズを計算（整数）
        interval_size = nframes/(func%npoints - 1)

        ! frame_intervals の割り当て
        allocate (func%frame_intervals(func%npoints))

        ! 等間隔でのサンプリング間隔の計算
        do i = 1, func%npoints
            func%frame_intervals(i) = (i - 1)*interval_size
        end do
    end subroutine linear_npoints

    subroutine calc_AutoCorrelationFunction_timeave(func, A)
        implicit none
        
        type(Function1D), intent(INOUT) :: func
        double precision, intent(IN) :: A(:)
        ! local
        integer :: i, j
        integer :: nframes 

        nframes = size(A)
        ! Calculate correlation function
        do i = 1, func%npoints
            func%y(i) = 0.0d0
            do j = 1, nframes - func%frame_intervals(i) 
                func%y(i) = func%y(i) + A(func%frame_intervals(i) - func%frame_intervals(j) + 1)*A(func%frame_intervals(j))
            end do
            func%y(i) = func%y(i)/func%frame_intervals(i)
        end do
    end subroutine

    subroutine calc_AutoCorrelationFunction_particleave(func, A)
        implicit none
        
        type(Function1D), intent(INOUT) :: func
        double precision, intent(IN) :: A(:,:)
        ! local
        integer :: i, j, k
        integer :: nframes 
        integer :: nparticles 
        double precision :: temp

        nframes = size(A, 2)
        nparticles = size(A, 1)
        ! Calculate correlation function
        do i = 1, func%npoints
            func%y(i) = 0.0d0
            do j = 1, nframes - func%frame_intervals(i) 
                temp = 0.0d0
                do k = 1, nparticles
                    temp = temp + A(k, func%frame_intervals(i) - func%frame_intervals(j) + 1)*A(k, func%frame_intervals(j))
                end do
                func%y(i) = func%y(i) + temp/nparticles
            end do
            func%y(i) = func%y(i)/func%frame_intervals(i)
        end do
    end subroutine

end module
