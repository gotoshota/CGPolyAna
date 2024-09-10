program test
    use global_types
    use io
    use coord_convert
    use correlation_function
    use math
    use physical_constants
    use lammpsIO
    implicit none
    type(lammpstrjReader) :: reader
    type(MDParams) :: params
    type(Function1d) :: tcinfo
    type(AtomIndex) :: atomheader
    integer :: i, j
    character(LEN=5) :: i2

    character(LEN=100) :: arg

    real, dimension(3) :: a, b

    ! get arg as input 
    call get_command_argument(1, arg)
    call read_MDParams(arg, params)

    ! Display the results
    do i = 1, params%ndumpfiles
        write (i2, "(I0)") i
        print *, trim(i2), "-th dump files:", trim(params%dumpfilenames(i))
    end do
    print *, "Nchains: ", params%nchains
    print *, "Nbeads: ", params%nbeads
    print *, "Dt: ", params%dt
    print *, "Dump_freq: ", params%dump_freq
    print *, "Nframes: ", params%nframes
    print *, "Is_cubic: ", params%is_cubic

    call reader%open(params%dumpfilenames(1))
    do i = 1, params%nframes
        call reader%read()
        !do j = 1, reader%nparticles
        !    reader%coords(:,j) = reader%coords(:,j) + reader%image_flags(:,j) * (reader%box_bounds(2,:) - reader%box_bounds(1,:))
        !end do
        reader%coords(:, :) = wrap_coords(reader%coords(:, :), reader%box_bounds(:, :))
        !do j = 1, params%nchains
        !    reader%coords(:, (j-1)*params%nbeads+1:j*params%nbeads) = wrap_polymer(reader%coords(:, (j-1)*params%nbeads+1:&
        !    j*params%nbeads),reader%box_bounds(:, :))
        !    print*, gauss_linking_number(reader%coords(:, (j-1)*params%nbeads+1:j*params%nbeads), reader%coords(:, 1:params%nbeads))
        !end do
        print*, gauss_linking_number(reader%coords(:, (2-1)*params%nbeads+1:2*params%nbeads), reader%coords(:, 1:params%nbeads))
    end do
    call reader%close()
    stop

    !call read_Function1DInfo(arg, TCinfo)
    !call determine_frame_intervals(tcinfo, params)
    !print *, tcinfo%frame_intervals

    do i = 1, 3
        a(i) = i
        b(4 - i) = i
    end do
    print *, cross_product(a, b)
    print *, norm(a)
    print *, norm(b)
    print *, dot_product(a, b)

    print *, '円周率π = ', pi
    print *, 'ボルツマン定数 = ', boltzmann_constant, 'J/K'
    print *, '気体定数 = ', gas_constant, 'J/(mol・K)'


end program test
