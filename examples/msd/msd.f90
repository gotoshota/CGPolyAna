program main
    use global_types
    use io
    use coord_convert
    use math
    use time_correlation_sampling 

    implicit none

    type(trajectory) :: traj
    type(TimeCorrelationInfo) :: tcinfo

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
    real, ALLOCATABLE   :: msd(:), msd_sq(:)


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

    call read_TimeCorrelationInfo(param_filename, TCinfo)
    call determine_frame_intervals(TCinfo, traj%nframes)
    ALLOCATE(msd(tcinfo%npoints), source=0.0e0)
    ALLOCATE(msd_sq(tcinfo%npoints), source=0.0e0)

    print *, ""
    print *, "Start computing MSD of monomer."
    do i = 1, tcinfo%npoints
        do j = 1, traj%nframes - tcinfo%frame_intervals(i)
            summation = 0.0d0
            summation_sq = 0.0d0
            do k = 1, traj%nparticles 
                displacement = norm(traj%coords(:, k, j) - traj%coords(:, k, j+tcinfo%frame_intervals(i)))
                summation = summation + displacement
                summation_sq = summation_sq + displacement*displacement
            enddo
            msd(i) = msd(i) + summation / DBLE(traj%nparticles)
            msd_sq(i) = msd_sq(i) + summation_sq / DBLE(traj%nparticles)
        enddo
        msd(i) = msd(i) / real(traj%nframes - tcinfo%frame_intervals(i))
        msd_sq(i) = msd_sq(i) / real(traj%nframes - tcinfo%frame_intervals(i))
    enddo

    CALL write_msd(msd_mon_filename, traj, tcinfo, msd, msd_sq)
    print *, "Fineshed computing MSD of monomer."
    
    print *, ""
    print *, "Start computing MSD of center of mass."
    do i = 1, tcinfo%npoints
        do j = 1, traj%nframes - tcinfo%frame_intervals(i)
            summation = 0.0d0
            summation_sq = 0.0d0
            do k = 1, traj%nchains
                displacement = norm(com(:, k, j) - com(:, k, j+tcinfo%frame_intervals(i)))
                summation = summation + displacement
                summation_sq = summation_sq + displacement*displacement
            enddo
            msd(i) = msd(i) + summation / DBLE(traj%nchains)
            msd_sq(i) = msd_sq(i) + summation_sq / DBLE(traj%nchains)
        enddo
        msd(i) = msd(i) / real(traj%nframes - tcinfo%frame_intervals(i))
        msd_sq(i) = msd_sq(i) / real(traj%nframes - tcinfo%frame_intervals(i))
    enddo

    CALL write_msd(msd_com_filename, traj, tcinfo, msd, msd_sq)
    print *, "Fineshed computing MSD of center of mass."

contains
    subroutine write_msd(filename, traj, tcinfo, msd, msd_sq)
        implicit none

        TYPE(trajectory), INTENT(IN)            :: traj
        TYPE(TimeCorrelationInfo), INTENT(IN)   :: tcinfo
        CHARACTER(LEN=*), INTENT(IN)            :: filename
        real, INTENT(IN)                        :: msd(:), msd_sq(:)

        ! local variables
        integer :: output = 17
        integer :: i

        open (output, file=filename, status="replace")
            write(output, "(A)") "# MSD data file. "
            write(output, "(A)") "# Time [tau_LJ], MSD [sigma^2], squared MSD <\delta r^4> [sigma^4]"
            do i = 1, tcinfo%npoints
                WRITE(output, "(G0, 1X, G0, 1X, G0)") tcinfo%frame_intervals(i) * traj%dt * traj%dump_freq, msd(i), msd_sq(i) 
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
