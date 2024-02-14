program main
    use global_types
    use io
    use coord_convert
    use math
    use time_dependent_function

    implicit none

    type(trajectory)              :: traj
    type(TimeDependentFunction)   :: msd

    DOUBLE PRECISION, ALLOCATABLE :: msd_sq(:)

    character(len=256)  :: arg
    integer             :: num_args
    integer, PARAMETER  :: max_args = 3
    CHARACTER(LEN=256)  :: param_filename = "param.nml"
    CHARACTER(LEN=256)  :: msd_mon_filename = "mon.msd"
    CHARACTER(LEN=256)  :: msd_com_filename = "com.msd"

    integer :: i, j, k

    real, ALLOCATABLE   :: com(:,:,:)

    real                :: displacement 
    DOUBLE PRECISION    :: summation, summation_sq

    call get_command_argument(1, arg)
    if (trim(adjustl(arg)) == "-h" .or. trim(adjustl(arg)) == "--help") then
       call display_usage()
       stop
    end if

    num_args = COMMAND_ARGUMENT_COUNT()
    do i = 1, num_args
        call GET_COMMAND_ARGUMENT(1, arg)
        if ( i == 1 ) then
            param_filename = arg
        else if ( i == 2) then
            msd_mon_filename = arg
        else if ( i == 3) then
            msd_com_filename = arg
        else if (i > max_args) then
            exit
        end if
    enddo

    call read_simulation_params(param_filename, traj)
    call read_lammpstrj(traj)
    ALLOCATE(com(3, traj%nchains, traj%nframes))
    com = center_of_mass(traj)

    call read_TimeDependentFunctionInfo(param_filename, msd)
    call determine_frame_intervals(msd, traj)
    ALLOCATE(msd_sq(msd%npoints), source=0.0d0)

    print *, ""
    print *, "Start computing MSD of monomer."
    msd%y = 0.0d0
    do i = 1, msd%npoints
        do j = 1, traj%nframes - msd%frame_intervals(i)
            summation = 0.0d0
            summation_sq = 0.0d0
            do k = 1, traj%nparticles 
                displacement = norm(traj%coords(:, k, j) - traj%coords(:, k, j+msd%frame_intervals(i)))
                summation = summation + displacement
                summation_sq = summation_sq + displacement*displacement
            enddo
            msd%y(i) = msd%y(i) + summation / DBLE(traj%nparticles)
            msd_sq(i) = msd_sq(i) + summation_sq / DBLE(traj%nparticles)
        enddo
        msd%y(i) = msd%y(i) / real(traj%nframes - msd%frame_intervals(i))
        msd_sq(i) = msd_sq(i) / real(traj%nframes - msd%frame_intervals(i))
    enddo

    CALL write_msd(msd_mon_filename, msd, msd_sq)
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
                displacement = norm(com(:, k, j) - com(:, k, j+msd%frame_intervals(i)))
                summation = summation + displacement
                summation_sq = summation_sq + displacement*displacement
            enddo
            msd%y(i) = msd%y(i) + summation / DBLE(traj%nchains)
            msd_sq(i) = msd_sq(i) + summation_sq / DBLE(traj%nchains)
        enddo
        msd%y(i) = msd%y(i) / real(traj%nframes - msd%frame_intervals(i))
        msd_sq(i) = msd_sq(i) / real(traj%nframes - msd%frame_intervals(i))
    enddo

    CALL write_msd(msd_com_filename, msd, msd_sq)
    print *, "Fineshed computing MSD of center of mass."

contains
    subroutine write_msd(filename, msd, msd_sq)
        implicit none

        TYPE(TimeDependentFunction), INTENT(IN) :: msd
        CHARACTER(LEN=*), INTENT(IN)            :: filename
        DOUBLE PRECISION, INTENT(IN)            :: msd_sq(:)

        ! local variables
        integer :: output = 17
        integer :: i

        open (output, file=filename, status="replace")
            write(output, "(A)") "# MSD data file. "
            write(output, "(A)") "# Time [tau_LJ], MSD [sigma^2], squared MSD <\delta r^4> [sigma^4]"
            do i = 1, msd%npoints
                WRITE(output, "(G0, 1X, G0, 1X, G0)") msd%t(i), msd%y(i), msd_sq(i)
            enddo
        close(output)
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
