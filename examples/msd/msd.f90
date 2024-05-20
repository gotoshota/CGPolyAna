program main
    use global_types
    use io
    use coord_convert
    use math
    use time_dependent_function

    implicit none

    type(trajectory) :: traj
    type(TimeDependentFunction) :: msd

    double precision, allocatable :: msd_sq(:)

    character(len=256) :: arg
    integer :: num_args
    integer, parameter :: max_args = 3
    character(LEN=256) :: param_filename = "param.nml"
    character(LEN=256) :: msd_mon_filename = "mon.msd"
    character(LEN=256) :: msd_com_filename = "com.msd"

    integer :: i, j, k

    real, allocatable :: com(:, :, :)

    double precision :: displacement
    double precision :: summation, summation_sq

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

    call read_simulation_params(param_filename, traj)
    call read_traj(traj)
    allocate (com(3, traj%nchains, traj%nframes))
    com = center_of_mass(traj)

    call read_TimeDependentFunctionInfo(param_filename, msd)
    call determine_frame_intervals(msd, traj)
    allocate (msd_sq(msd%npoints), source=0.0d0)

    print *, ""
    print *, "Start computing MSD of monomer."
    msd%y = 0.0d0
    do i = 1, msd%npoints
        do j = 1, traj%nframes - msd%frame_intervals(i)
            summation = 0.0d0
            summation_sq = 0.0d0
            do k = 1, traj%nparticles
                displacement = norm(traj%coords(:, k, j) - traj%coords(:, k, j + msd%frame_intervals(i)))
                summation = summation + displacement
                summation_sq = summation_sq + displacement*displacement
            end do
            msd%y(i) = msd%y(i) + summation/dble(traj%nparticles)
            msd_sq(i) = msd_sq(i) + summation_sq/dble(traj%nparticles)
        end do
        msd%y(i) = msd%y(i)/real(traj%nframes - msd%frame_intervals(i))
        msd_sq(i) = msd_sq(i)/real(traj%nframes - msd%frame_intervals(i))
    end do

    call write_msd(msd_mon_filename, msd, msd_sq)
    print *, "Fineshed computing MSD of monomer."
    
    print *, ""
    print *, "Start computing MSD of center of mass."
    msd%y = 0.0d0
    msd_sq = 0.0d0
    do i = 1, msd%npoints
        do j = 1, traj%nframes - msd%frame_intervals(i)
            summation = 0.0d0
            summation_sq = 0.0d0
            do k = 1, traj%nchains
                displacement = norm(com(:, k, j) - com(:, k, j + msd%frame_intervals(i)))
                summation = summation + displacement
                summation_sq = summation_sq + displacement*displacement
            end do
            msd%y(i) = msd%y(i) + summation/dble(traj%nchains)
            msd_sq(i) = msd_sq(i) + summation_sq/dble(traj%nchains)
        end do
        msd%y(i) = msd%y(i)/real(traj%nframes - msd%frame_intervals(i))
        msd_sq(i) = msd_sq(i)/real(traj%nframes - msd%frame_intervals(i))
    end do

    call write_msd(msd_com_filename, msd, msd_sq)
    print *, "Fineshed computing MSD of center of mass."

contains
    subroutine write_msd(filename, msd, msd_sq)
        implicit none

        type(TimeDependentFunction), intent(IN) :: msd
        character(LEN=*), intent(IN) :: filename
        double precision, intent(IN) :: msd_sq(:)

        ! local variables
        integer :: output = 17
        integer :: i

        open (output, file=filename, status="replace")
            write (output, "(A)") "# MSD data file. "
            write (output, "(A)") "# Time [tau_LJ], MSD [sigma^2], squared MSD <\delta r^4> [sigma^4]"
            do i = 1, msd%npoints
                write (output, "(G0, 1X, G0, 1X, G0)") msd%t(i), msd%y(i), msd_sq(i)
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
