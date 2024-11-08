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
    allocate(com(3, params%nchains, params%nframes))
    call determine_frame_intervals(msd, params%nframes, params%dt, params%dump_freq)

    ! lammpstrjReader の初期化
    call lmp%open(params%dumpfilenames(1))
    ! 全部のフレームを読んで格納しちゃう
    ! idx_frame = 1
    call lmp%read()
    ALLOCATE(coords(3, lmp%nparticles, params%nframes))
    if (ALLOCATED(lmp%image_flags)) then
        print *, "This trajectory may be wrapped."
        coords(:, :, 1) = unwrap_coords(lmp%coords, lmp%box_bounds, lmp%image_flags)
    else
        coords(:, :, 1) = lmp%coords
    end if
    do i = 1, params%nchains
        com(:, i, 1) = center_of_mass(coords(:, (i-1)*params%nbeads+1:i*params%nbeads, 1))
    end do
    ! idx_frame = 2 ~ nframes
    do idx_frame = 2, params%nframes
        call lmp%read()
        if (ALLOCATED(lmp%image_flags)) then
            coords(:, :, idx_frame) = unwrap_coords(lmp%coords, lmp%box_bounds, lmp%image_flags)
        else
            coords(:, :, idx_frame) = lmp%coords
        end if
        do i = 1, params%nchains
            com(:, i, idx_frame) = center_of_mass(coords(:, (i - 1) * params%nbeads + 1:i * params%nbeads, idx_frame))
        end do
    enddo
    ! MSD の計算 
    ! monomer
    msd%y = 0.0
    msd_sq = 0.0
    do i = 1, msd%npoints
        call calc_msd(lmp, msd, coords, msd_sq, i)
    end do
    call write_msd(msd_mon_filename, msd, msd_sq)
    ! center of mass
    msd%y = 0.0
    msd_sq = 0.0
    do i = 1, msd%npoints
        call calc_msd(lmp, msd, com, msd_sq, i)
    end do
    call write_msd(msd_com_filename, msd, msd_sq)
    !call write_lmptrj(com, lmp%box_bounds, lmp%image_flags)

contains
    subroutine calc_msd(lmp, msd, coords, msd_sq, i)
        !$ use omp_lib
        implicit none
        type(lammpstrjReader), intent(IN) :: lmp
        type(Function1D), intent(INOUT) :: msd
        real, intent(IN) :: coords(:, :, :)
        double precision, intent(INOUT) :: msd_sq(:)
        integer, intent(IN) :: i
        real :: dr(3)
         

        do j = 1, size(coords, 3) - msd%frame_intervals(i)
            summation = 0.0
            summation_sq = 0.0
            do k = 1, size(coords, 2)
                dr = coords(:, k, j + msd%frame_intervals(i)) - coords(:, k, j)
                tmp = dr(1) * dr(1) + dr(2) * dr(2) + dr(3) * dr(3)
                summation = summation + tmp
                summation_sq = summation_sq + tmp * tmp
            end do
            msd%y(i) = msd%y(i) + summation / size(coords, 2)
            msd_sq(i) = msd_sq(i) + summation_sq / size(coords, 2)
        end do
        msd % y(i) = msd % y(i) / (size(coords,3) - msd % frame_intervals(i) + 1)
        msd_sq(i) = msd_sq(i) / (size(coords, 3) - msd % frame_intervals(i) + 1)
    end subroutine

    subroutine write_lmptrj(coords, box_bounds, image_flags)
        implicit none
        real, intent(IN) :: coords(:, :, :)
        DOUBLE PRECISION, intent(IN) :: box_bounds(3, 2)
        integer, intent(IN) :: image_flags(3)

        integer :: output = 17
        integer :: i, j, k
        character(LEN=256) :: filename = "test.lammpstrj"

        open(output, file=filename, status="replace")
            do i = 1, size(coords, 3)
                write(output, "(A)") "ITEM: TIMESTEP"
                write(output, "(I0)") 0
                write(output, "(A)") "ITEM: NUMBER OF ATOMS"
                write(output, "(I0)") size(coords, 2)
                write(output, "(A)") "ITEM: BOX BOUNDS pp pp pp"
                write(output, "(E22.15, 1x, E22.15)") box_bounds(1, 1), box_bounds(2, 1)
                write(output, "(E22.15, 1x, E22.15)") box_bounds(1, 1), box_bounds(2, 1)
                write(output, "(E22.15, 1x, E22.15)") box_bounds(1, 1), box_bounds(2, 1)
                write(output, "(A)") "ITEM: ATOMS id type x y z"
                do j = 1, size(coords, 2)
                    write(output, "(I0, 1X, I1, 1X, 3(G0,1x))") j, 1, coords(1, j, i), coords(2, j, i), coords(3, j, i)
                end do
            end do
        close(output)
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
