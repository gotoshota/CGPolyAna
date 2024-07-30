program main
    !$ use omp_lib
    use global_types
    use lammpsIO
    use coord_convert
    use math
    use correlation_function

    implicit none

    type(MDParams) :: params
    type(lammpstrjReader) :: lmp
    type(Function1D) :: msd

    real, allocatable :: coords(:, :, :)
    double precision, allocatable :: msd_sq(:)

    character(len=256) :: arg
    integer :: num_args
    integer, parameter :: max_args = 3
    character(LEN=256) :: param_filename = "param.nml"
    character(LEN=256) :: msd_mon_filename = "mon.msd"
    character(LEN=256) :: msd_com_filename = "com.msd"

    integer :: i, j, k, idx_frame

    real, allocatable :: com(:, :, :)

    double precision :: displacement
    double precision :: summation, summation_sq, tmp

    double precision :: box_size(3)

    ! 引数を取得
    call get_command_argument(1, arg)
    if (trim(adjustl(arg)) .eq. "-h" .or. trim(adjustl(arg)) .eq. "--help") then
       call display_usage()
       stop
    end if
    num_args = command_argument_count()
    do i = 1, num_args
        call get_command_argument(i, arg)
        if (i .eq. 1) then
            param_filename = arg
        else if (i .eq. 2) then
            msd_mon_filename = arg
        else if (i .eq. 3) then
            msd_com_filename = arg
        else if (i .gt. max_args) then
            exit
        end if
    end do

    ! input ファイルの読み込み
    call read_MDParams(param_filename, params)
    call read_Function1Dinfo(param_filename, msd)
    allocate(msd_sq(msd%npoints))
    call determine_frame_intervals(msd, params%nframes, params%dt, params%dump_freq)

    call lmp%open(params%dumpfilenames(1))
    ! 全部のフレームを読んで格納しちゃう
    ! idx_frame = 1
    call lmp%read()
    ALLOCATE(coords(3, lmp%nparticles, params%nframes))
    box_size(:) = lmp%box_bounds(2, :) - lmp%box_bounds(1, :)
    do i = 1, lmp%nparticles
        coords(:, i, 1) = lmp%coords(:, i) + lmp%image_flags(:, i) * box_size(:)
    end do
    coords(:, :, 1) = lmp%coords(:,:)
    do idx_frame = 2, params%nframes
        call lmp%read()
        coords(:, :, idx_frame) = unwrap_coords(lmp%coords, lmp%box_bounds, lmp%image_flags)
    enddo

    do i = 1, msd%npoints
    print *, "Calculating MSD of monomer... ", i, "/", msd%npoints
        ! MSD Monoer

        !do j = 1, params%nframes - msd%frame_intervals(i)
        !    summation = 0.0
        !    summation_sq = 0.0
        !    do k = 1, lmp%nparticles
        !        tmp = sum((coords(:, k, j + msd%frame_intervals(i)) - coords(:, k, j)) ** 2)
        !        summation = summation + tmp
        !        summation_sq = summation_sq + tmp * tmp
        !    end do
        !    print *, "a"
        !    msd%y(i) = msd%y(i) + summation / lmp%nparticles
        !    print *, "b"
        !    msd_sq(i) = msd_sq(i) + summation_sq / lmp%nparticles
        !    print *, "c"
        !end do
        !msd % y(i) = msd % y(i) / (params % nframes - msd % frame_intervals(i))
        !msd_sq(i) = msd_sq(i) / (params % nframes - msd % frame_intervals(i))
        call calc_msd(params, lmp, msd, coords, msd_sq, i)
    end do

    call write_msd(msd_mon_filename, msd, msd_sq)



contains
    subroutine calc_msd(params, lmp, msd, coords, msd_sq, i)
        !$ use omp_lib
        implicit none
        type(MDParams), intent(IN) :: params
        type(lammpstrjReader), intent(IN) :: lmp
        type(Function1D), intent(INOUT) :: msd
        real, intent(IN) :: coords(:, :, :)
        double precision, intent(INOUT) :: msd_sq(:)
        integer, intent(IN) :: i
         

        print *, lmp%nparticles
        print *, params%nframes
        print *, msd%frame_intervals(i)
        do j = 1, params%nframes - msd%frame_intervals(i)
            summation = 0.0
            summation_sq = 0.0
            do k = 1, lmp%nparticles
                tmp = sum((coords(:, k, j + msd%frame_intervals(i)) - coords(:, k, j)) ** 2)
                summation = summation + tmp
                summation_sq = summation_sq + tmp * tmp
            end do
            msd%y(i) = msd%y(i) + summation / lmp%nparticles
            msd_sq(i) = msd_sq(i) + summation_sq / lmp%nparticles
        end do
        msd % y(i) = msd % y(i) / (params % nframes - msd % frame_intervals(i))
        msd_sq(i) = msd_sq(i) / (params % nframes - msd % frame_intervals(i))
    end subroutine

    subroutine write_msd(filename, msd, msd_sq)
        implicit none

        type(Function1D), intent(IN) :: msd
        character(LEN=*), intent(IN) :: filename
        double precision, intent(IN) :: msd_sq(:)

        ! local variables
        integer :: output = 17
        integer :: i

        open (output, file=filename, status="replace")
            write (output, "(A)") "# MSD data file. "
            write (output, "(A)") "# Time [tau_LJ], MSD [sigma^2], squared MSD <\delta r^4> [sigma^4]"
            do i = 1, msd%npoints
                write (output, "(G0, 1X, G0, 1X, G0)") msd%x(i), msd%y(i), msd_sq(i)
            end do
        close (output)
    end subroutine

    subroutine display_usage()
        print *, ""
        print *, "Usage: <program_name> [options] [args]"
        print *, "Options:"
        print *, "  -h, --help  : Display this usage information"
        print *, "Args:"
        print *, "1st arg : path to parameter file constains simulation_data and TimeCorrelationInfo as namelist."
        print *, "          [default] : param.nml "
        print *, "2nd arg : path to output file of MSD of monomer."
        print *, "          [default] : mon.msd"
        print *, "3rd arg : path to output file of MSD of center of mass."
        print *, "          [default] : com.msd"
        print *, ""
        ! 他のオプションに関する情報を追加する場合はここに追記する
    end subroutine display_usage
end program main
