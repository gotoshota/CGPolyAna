program main
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
    double precision :: summation, summation_sq

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

    call lmp%open(params%dumpfilenames(1))
    ! 全部のフレームを読んで格納しちゃう
    do idx_frame = 1, params%nframes
        call lmp%read()
        if ( idx_frame .eq. 1) then
            ALLOCATE(coords(3, lmp%nparticles, params%nframes))
            coords(:, :, idx_frame) = lmp%coords
        end if
    enddo

contains
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
