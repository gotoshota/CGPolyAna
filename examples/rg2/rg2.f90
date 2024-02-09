program main
    use global_types
    use read_files
    use math
    use statistics
    use coord_convert

    implicit none

    type(trajectory) :: traj

    character(len=256)  :: arg
    integer             :: num_args
    INTEGER             :: max_args=3
    CHARACTER(LEN=256)  :: param_filename = "param.nml"
    CHARACTER(LEN=256)  :: rg2_stat_filename = "stat.rg2"
    CHARACTER(LEN=256)  :: rg2_pdf_filename = "pdf.rg2"

    integer             :: i, j, k
    integer             :: shift_chain, shift_frame

    real, ALLOCATABLE    :: com(:,:,:)

    DOUBLE PRECISION, ALLOCATABLE   :: rg2(:)
    DOUBLE PRECISION                :: rg2_mean
    DOUBLE PRECISION                :: rg2_var
    DOUBLE PRECISION, ALLOCATABLE   :: pdf(:,:)
    INTEGER :: num_bin = 100


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
            rg2_stat_filename = arg
        else if ( i == 3) then
            rg2_pdf_filename = arg
        else if (i > max_args) then
            exit
        end if
    enddo

    call read_simulation_params(param_filename, traj)
    call read_lammpstrj(traj)

    ALLOCATE(com(3, traj%nchains, traj%nframes))
    com = center_of_mass(traj)

    ALLOCATE(rg2(traj%nchains * traj%nframes), source = 0.0d0)
    do i = 1, traj%nframes
        shift_frame = (i-1)*traj%nchains
        do j = 1, traj%nchains
            shift_chain = (j-1)*traj%nbeads
            do k = 1, traj%nbeads
                rg2(shift_frame+j) = rg2(shift_frame+j) + norm(traj%coords(:,shift_chain+k, i) - com(:,k,i))
            enddo
        enddo
    enddo
    rg2(:) = rg2(:) / real(traj%nbeads)

    call mean_and_variance(rg2(:), rg2_mean, rg2_var)    
    print *, "mean : ", rg2_mean
    print *, "variance : ", rg2_var
    print *, "standard deviation : ", dsqrt(rg2_var)

    CALL write_statdata(rg2_stat_filename, rg2_mean, rg2_var)


    ALLOCATE(pdf(2, num_bin))
    call calc_prob_dist(rg2, pdf)
    call write_prob_dist_data(rg2_pdf_filename, pdf)
    print *, SUM(pdf, DIM=2)
contains
    subroutine write_statdata(filename, mean, var)
        implicit none

        CHARACTER(LEN=*), INTENT(IN)            :: filename
        DOUBLE PRECISION, INTENT(IN)            :: mean, var

        ! local variables
        integer :: output = 17

        open (output, file=filename, status="replace")
            write(output, "(A)") "# Statistical data of Squared Radius Gyrations. "
            write(output, "(A)") "# Mean [sigma^2], Variance [sigma^4], Standard deviation [sigma^2]"
            write(output, "(G0, 1x, G0, 1x, G0)") mean, var, dsqrt(var)
        close(output)
    end subroutine

    subroutine write_prob_dist_data(filename, pdf)
        implicit none

        CHARACTER(LEN=*), INTENT(IN)            :: filename
        DOUBLE PRECISION, INTENT(IN)            :: pdf(:,:)

        ! local variables
        integer :: output = 17
        integer :: i

        open (output, file=filename, status="replace")
            write(output, "(A)") "# Probability Distribution function of Squared Radius Gyration Rg2. "
            write(output, "(A)") "# Rg2 [sigma^2], pdf [-]"
            do i = 1, SIZE(pdf, DIM=2)
                write(output, "(G0, 1x, G0)")pdf(1,i), pdf(2,i)
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
        print *, "2nd arg : path to output file of Radius of Gyration Rg2."
        print *, "          [default] : stat.rg2"
        print *, "3rd arg : path to output file of probability distribution function of Rg2."
        print *, "          [default] : pdf.rg2"
        print *, ""
        ! 他のオプションに関する情報を追加する場合はここに追記する
    end subroutine display_usage

end program main
