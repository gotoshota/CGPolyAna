program main
    use global_types
    use io
    use math
    use statistics
    use coord_convert
    use time_dependent_function

    implicit none

    type(trajectory) :: traj
    type(TimeDependentFunction) :: rg2_time

    character(len=256) :: arg
    integer :: num_args
    integer :: max_args = 4
    character(LEN=256) :: param_filename = "param.nml"
    character(LEN=256) :: rg2_stat_filename = "stat.rg2"
    character(LEN=256) :: rg2_pdf_filename = "pdf.rg2"
    character(LEN=256) :: rg2_time_filename = "timetrack.rg2"

    integer :: i, j, k
    integer :: shift_chain
    integer :: outfile = 66

    real, allocatable :: com(:, :, :)
    real :: vec(3)

    double precision, allocatable :: rg2(:, :)
    double precision :: rg2_mean
    double precision :: rg2_var
    type(ProbDistFunciton) :: pdf
    integer :: num_bins = 100

    call get_command_argument(1, arg)
    if (trim(adjustl(arg)) .eq. "-h" .or. trim(adjustl(arg)) .eq. "--help") then
       call display_usage()
       stop
    end if

    num_args = command_argument_count()
    do i = 1, num_args
        call get_command_argument(1, arg)
        if (i .eq. 1) then
            param_filename = arg
        else if (i .eq. 2) then
            rg2_stat_filename = arg
        else if (i .eq. 3) then
            rg2_pdf_filename = arg
        else if (i .eq. 4) then
            rg2_time_filename = arg
        else if (i .gt. max_args) then
            exit
        end if
    end do

    call read_simulation_params(param_filename, traj)
    call read_TimeDependentFunctionInfo(param_filename, rg2_time)
    call determine_frame_intervals(rg2_time, traj)
    call read_lammpstrj(traj)

    allocate (com(3, traj%nchains, traj%nframes), source=0.0e0)
    com = center_of_mass(traj)

    allocate (rg2(traj%nchains, traj%nframes), source=0.0d0)
    do i = 1, traj%nframes
        do j = 1, traj%nchains
            shift_chain = (j - 1)*traj%nbeads
            do k = 1, traj%nbeads
                vec(:) = traj%coords(:, shift_chain + k, i) - com(:, j, i)
                rg2(j, i) = rg2(j, i) + dot_product(vec, vec)
            end do
            rg2(j, i) = rg2(j, i)/real(traj%nbeads)
        end do
    end do

    call mean_and_variance(rg2, size(rg2), rg2_mean, rg2_var)
    print *, "mean : ", rg2_mean
    print *, "variance : ", rg2_var
    print *, "standard deviation : ", dsqrt(rg2_var)

    call write_statdata(rg2_stat_filename, rg2_mean, rg2_var)

    open (outfile, file=rg2_time_filename, status="replace")
        write (outfile, *) "# Track the time variation of Rg2. Do NOT averaged over time, only space."
        write (outfile, *) "# time [\tau_LJ], Mean of Rg2 [sigma^2], standard deviation of Rg2 [sigma^2]"
        do i = 1, rg2_time%npoints
            call mean_and_variance(rg2(:, rg2_time%frame_intervals(i)), traj%nchains, rg2_mean, rg2_var)
            write (outfile, *) rg2_time%t(i), rg2_mean, dsqrt(rg2_var)
        end do
    close (outfile)
    
    pdf%n_bins = num_bins
    allocate (pdf%x(pdf%n_bins), pdf%y(pdf%n_bins))
    call calc_prob_dist(rg2, size(rg2), pdf)
    call write_prob_dist_data(rg2_pdf_filename, pdf)
    print *, sum(pdf%y*pdf%bin_width)

contains

    subroutine write_statdata(filename, mean, var)
        implicit none

        character(LEN=*), intent(IN) :: filename
        double precision, intent(IN) :: mean, var

        ! local variables
        integer :: output = 17

        open (output, file=filename, status="replace")
            write (output, "(A)") "# Statistical data of Squared Radius Gyrations. "
            write (output, "(A)") "# Mean [sigma^2], Variance [sigma^4], Standard deviation [sigma^2]"
            write (output, "(G0, 1x, G0, 1x, G0)") mean, var, dsqrt(var)
        close (output)
    end subroutine

    subroutine write_prob_dist_data(filename, pdf)
        implicit none

        character(LEN=*), intent(IN) :: filename
        type(ProbDistFunciton), intent(IN) :: pdf

        ! local variables
        integer :: output = 17
        integer :: i

        open (output, file=filename, status="replace")
            write (output, "(A)") "# Probability Distribution function of Squared Radius Gyration Rg2. "
            write (output, "(A)") "# Rg2 [sigma^2], pdf [-]"
            do i = 1, pdf%n_bins
                write (output, "(G0, 1x, G0)") pdf%x(i), pdf%y(i)
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
        print *, "2nd arg : path to output file of Radius of Gyration Rg2."
        print *, "          [default] : stat.rg2"
        print *, "3rd arg : path to output file of probability distribution function of Rg2."
        print *, "          [default] : pdf.rg2"
        print *, "4th arg : path to output file of time tracking data of Rg2."
        print *, "          [default] : timetrack.rg2"
        print *, ""
        ! 他のオプションに関する情報を追加する場合はここに追記する
    end subroutine display_usage

end program main
