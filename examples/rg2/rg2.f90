program main
    use global_types
    use io
    use math
    use statistics
    use coord_convert
    use time_dependent_function

    implicit none

    type(trajectory) :: traj
    type(trajectory) :: traj_wrap
    type(AtomHeader_Index) :: headers
    type(TimeDependentFunction) :: rg2_time

    character(len=256) :: arg
    integer :: num_args
    integer :: max_args = 5
    character(LEN=256) :: param_filename = "param.nml"
    character(LEN=256) :: rg2_stat_filename = "stat.rg2"
    character(LEN=256) :: rg2_pdf_filename = "pdf.rg2"
    character(LEN=256) :: rg2_time_filename = "timetrack.rg2"
    character(LEN=256) :: asp_stat_filename = "stat.asphericity"
    character(LEN=256) :: asp_pdf_filename = "pdf.asphericity"
    character(LEN=256) :: prl_stat_filename = "stat.prolateness"
    character(LEN=256) :: prl_pdf_filename = "pdf.prolateness"

    integer :: i, j, k
    integer :: shift_chain
    integer :: outfile = 66

    real, allocatable :: coords(:, :)
    double precision, allocatable :: rg2(:, :)
    double precision, allocatable :: asphericity(:, :)
    double precision, allocatable :: prolateness(:, :)

    double precision :: eigenval(3)
    double precision :: gyration_tensor(3,3)

    double precision :: mean
    double precision :: var
    type(ProbDistFunction) :: pdf
    integer :: num_bins = 100

    double precision, allocatable :: work(:)
    integer :: lwork
    integer :: info

    call get_command_argument(1, arg)
    if (trim(adjustl(arg)) .eq. "-h" .or. trim(adjustl(arg)) .eq. "--help") then
       call display_usage()
       stop
    else
        param_filename = trim(adjustl(arg))
    end if

    call read_simulation_params(param_filename, traj)
    call read_TimeDependentFunctionInfo(param_filename, rg2_time)
    call determine_frame_intervals(rg2_time, traj)
    call read_traj(traj)
    traj_wrap = traj

    allocate (coords(3, traj%nbeads))
    allocate (rg2(traj%nchains, traj%nframes))
    allocate (asphericity(traj%nchains, traj%nframes))
    allocate (prolateness(traj%nchains, traj%nframes))
    ! Calculate the optimal size of the work array
    lwork = -1
    allocate(work(1))
    call dsyev('V', 'U', 3, gyration_tensor, 3, eigenval, work, lwork, info)
    lwork = int(work(1))
    deallocate(work)
    allocate(work(lwork))
    ! calculate the eigenvalues of the gyration tensor
    do i = 1, traj%nframes
        do j = 1, traj%nchains
            shift_chain = (j - 1)*traj%nbeads
            coords = wrap_polymer(traj%coords(:, shift_chain + 1:shift_chain + traj%nbeads, i), traj%box_dim(:,:,i))
            traj_wrap%coords(:, shift_chain + 1:shift_chain + traj%nbeads, i) = coords
            call calc_gyration_tensor(coords, gyration_tensor)
            call dsyev('V', 'U', 3, gyration_tensor, 3, eigenval, work, lwork, info)
            call calc_radius(eigenval, rg2(j, i))
            call calc_asphericity(eigenval, asphericity(j, i))
            call calc_prolateness(eigenval, prolateness(j, i))
        end do
    end do
    headers%id = 1
    headers%mol = 2
    headers%xu = 3
    headers%yu = 4
    headers%zu = 5
    call write_lammpstrj(traj_wrap, headers, "wrapped_traj.lammpstrj")

    call mean_and_variance(rg2, size(rg2), mean, var)
    print *, "The squared radius of gyration Rg2."
    print *, "mean : ", mean
    print *, "variance : ", var
    print *, "standard deviation : ", dsqrt(var)
    call write_statdata(rg2_stat_filename, mean, var)
    open (outfile, file=rg2_time_filename, status="replace")
        write (outfile, *) "# Track the time variation of Rg2. Do NOT averaged over time, only space."
        write (outfile, *) "# time [\tau_LJ], Mean of Rg2 [sigma^2], standard deviation of Rg2 [sigma^2]"
        do i = 1, rg2_time%npoints
            call mean_and_variance(rg2(:, rg2_time%frame_intervals(i)), traj%nchains, mean, var)
            write (outfile, *) rg2_time%t(i), mean, dsqrt(var)
        end do
    close (outfile)
    pdf%n_bins = num_bins
    allocate (pdf%x(pdf%n_bins), pdf%y(pdf%n_bins))
    call calc_prob_dist(rg2, size(rg2), pdf)
    call write_prob_dist_data(rg2_pdf_filename, pdf)

    call mean_and_variance(asphericity, size(asphericity), mean, var)
    print *, "The asphericity."
    print *, "mean : ", mean
    print *, "variance : ", var
    print *, "standard deviation : ", dsqrt(var)
    call write_statdata(asp_stat_filename, mean, var)
    call calc_prob_dist(asphericity, size(asphericity), pdf)
    call write_prob_dist_data(asp_pdf_filename, pdf)

    call mean_and_variance(prolateness, size(prolateness), mean, var)
    print *, "The prolateness."
    print *, "mean : ", mean
    print *, "variance : ", var
    print *, "standard deviation : ", dsqrt(var)
    call write_statdata(prl_stat_filename, mean, var)
    call calc_prob_dist(prolateness, size(prolateness), pdf)
    call write_prob_dist_data(prl_pdf_filename, pdf)

contains

    subroutine calc_gyration_tensor(coords, gyration_tensor)
        implicit none
        
        real(4), dimension(:,:), intent(in) :: coords
        !real(4), dimension(3,3), intent(out) :: gy_tensor
        double precision, dimension(3, 3), intent(out) :: gyration_tensor
        integer :: i, j, k
        integer :: nbeads
        real, dimension(3) :: com
        
        nbeads=size(coords, 2)

        com(:) = 0.0d0
        do i = 1, nbeads
            com(:) = com(:) + coords(:, i)
        end do
        com(:) = com(:)/nbeads

        gyration_tensor(:, :) = 0.0d0
        do i = 1, nbeads
            do j = 1, 3
                do k = 1, 3
                    gyration_tensor(j, k) = gyration_tensor(j, k) &
                    + (coords(j, i) - com(j))*(coords(k, i) - com(k))
                end do
            end do
        end do

        gyration_tensor(:, :) = gyration_tensor(:, :)/dble(nbeads)

    end subroutine calc_gyration_tensor

    subroutine calc_radius(eigenval, radius)
        implicit none 
        
        double precision, dimension(3), intent(in) :: eigenval(:)
        double precision, intent(out) :: radius
        integer :: i

        radius = 0.0d0
        do i = 1, 3
            radius = radius + eigenval(i)
        end do

    end subroutine calc_radius

    subroutine calc_asphericity(eigenval, asphericity)
        implicit none

        double precision, dimension(3), intent(in) :: eigenval(:)
        double precision, intent(out) :: asphericity

        asphericity = (eigenval(1) - eigenval(2))**2
        asphericity = asphericity + (eigenval(2) - eigenval(3))**2
        asphericity = asphericity + (eigenval(3) - eigenval(1))**2
        asphericity = asphericity / (2*(eigenval(1) + eigenval(2) + eigenval(3))**2)

    end subroutine calc_asphericity

    subroutine calc_prolateness(eigenval, prolateness)
        implicit none

        double precision, dimension(3), intent(in) :: eigenval(:)
        double precision, intent(out) :: prolateness

        prolateness = 1 * (2*eigenval(1)-eigenval(2)-eigenval(3))
        prolateness = prolateness * (2*eigenval(2)-eigenval(3)-eigenval(1))
        prolateness = prolateness * (2*eigenval(3)-eigenval(1)-eigenval(2))
        prolateness = prolateness / (2*((eigenval(1)**2 + eigenval(2)**2 + eigenval(3)**2&
                      -eigenval(1)*eigenval(2)-eigenval(2)*eigenval(3)&
                      -eigenval(3)*eigenval(1))**1.5))

    end subroutine calc_prolateness

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
        type(ProbDistFunction), intent(IN) :: pdf

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
