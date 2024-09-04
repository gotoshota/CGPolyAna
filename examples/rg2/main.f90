program main
    use global_types
    use lammpsIO
    use math
    use statistics
    use coord_convert
    use correlation_function
    use physical_constants

    implicit none

    type(MDParams) :: params
    type(lammpstrjReader) :: reader

    integer :: idx_dump, idx_frame
    integer :: i, j, k
    integer :: outfile = 66

    type(Function1D) :: rg2_time
    type(StatValues) :: rg2
    type(StatValues) :: asphericity
    type(StatValues) :: prolateness
    double precision :: min_val, max_val
    integer :: n_bins

    real :: com(3)
    double precision :: eigenval(3)
    double precision :: gyration_tensor(3,3)
    double precision :: tmp

    double precision :: mean
    double precision :: var
    type(ProbDistFunction) :: pdf
    integer :: num_bins = 100

    ! lapack
    double precision, allocatable :: work(:)
    integer :: lwork
    integer :: info

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

    call get_command_argument(1, arg)
    if (trim(adjustl(arg)) .eq. "-h" .or. trim(adjustl(arg)) .eq. "--help") then
       call display_usage()
       stop
    else
        param_filename = trim(adjustl(arg))
    end if

    call read_MDParams(param_filename, params)
    call read_Function1DInfo(param_filename, rg2_time)
    call determine_frame_intervals(rg2_time, params%nframes)

    ! type(StatValue) の初期化(内部でallocate)
    !! Rg2
    min_val = 2.0d0 * (params%nbeads / 4.0) ** (2.0d0/3.0d0) ! ビーズのサイズと重合度から決める 2倍はテキトー
    max_val = 25.0d0*min_val ! テキトー
    n_bins = 100
    call rg2%init(min_val, max_val, n_bins)
    !! Asphericity
    min_val = 0.0d0
    max_val = 1.0d0 ! 定義より
    n_bins = 100
    call asphericity%init(min_val, max_val, n_bins)
    !! Prolateness
    min_val = -1.0d0
    max_val = 1.0d0 ! 定義より
    n_bins = 100
    call prolateness%init(min_val, max_val, n_bins)
    do idx_dump = 1, params%ndumpfiles
        call reader%open(trim(adjustl(params%dumpfilenames(idx_dump))))
        do idx_frame = 1, params%nframes
            call reader%read()
            ! もしファイルの終わりに到達したら終了
            if (reader%end_of_file) then
                call reader%close()
                exit
            end if
            do i = 1, params%nchains
                call calc_gyration_tensor(reader%coords(:, (i-1)*params%nbeads+1:i*params%nbeads), gyration_tensor)  
                call calc_radius(eigenval, tmp)
                ! update で統計量を逐次的に計算
                call rg2%update(tmp)
                call calc_asphericity(eigenval, tmp)
                call asphericity%update(tmp)
                call calc_prolateness(eigenval, tmp)
                call prolateness%update(tmp)
            enddo
        end do
        call reader%close()
    enddo

    ! 書き出し
    call write_statdata(rg2_stat_filename, rg2%mean, rg2%variance)
    call write_prob_dist_data(rg2_pdf_filename, rg2%pdf, rg2%pdf_x)
    call write_statdata(asp_stat_filename, asphericity%mean, asphericity%variance)
    call write_prob_dist_data(asp_pdf_filename, asphericity%pdf, asphericity%pdf_x)
    call write_statdata(prl_stat_filename, prolateness%mean, prolateness%variance)
    call write_prob_dist_data(prl_pdf_filename, prolateness%pdf, prolateness%pdf_x)

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
            write (output, "(A)") "# Statistical data of this physical quantity (See filename)."
            write (output, "(A)") "# Mean, Variance, Standard deviation"
            write (output, "(G0, 1x, G0, 1x, G0)") mean, var, dsqrt(var)
        close (output)
    end subroutine

    subroutine write_prob_dist_data(filename, pdf, x)
        implicit none

        character(LEN=*), intent(IN) :: filename
        double precision, intent(IN) :: x(:)
        double precision, intent(IN) :: pdf(:)


        ! local variables
        integer :: output = 17
        integer :: i

        open (output, file=filename, status="replace")
            write (output, "(A)") "# Probability Distribution function"
            write (output, "(A)") "# value, pdf"
            do i = 1, size(pdf)
                write (output, "(G0, 1x, G0)") x(:), pdf(i)
            end do
        close (output)
    end subroutine

    subroutine lapack_init(lwork, work, info)
        ! LAPACK の dsyev に必要なワークスペースのサイズを計算する
        implicit none

        integer, intent(out) :: lwork
        double precision, allocatable, intent(out) :: work(:)
        integer, intent(out) :: info

        double precision :: eigenval(3)
        double precision :: eigenvec(3, 3)
        
        lwork = -1
        allocate(work(1))
        call dsyev('V', 'U', 3, eigenvec, 3, eigenval, work, lwork, info)
        lwork = int(work(1))
        deallocate(work)
        allocate(work(lwork))

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
