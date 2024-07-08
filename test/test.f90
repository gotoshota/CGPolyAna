program test
    use global_types
    use io
    use correlation_function
    use math
    use physical_constants
    implicit none
    type(trajectory) :: traj
    type(Function1d) :: tcinfo
    type(AtomHeader_Index) :: atomheader
    integer :: i, j
    character(LEN=5) :: i2

    real, dimension(3) :: a, b

    call read_simulation_params('input.nml', traj)

    ! Display the results
    do i = 1, traj%ndumpfiles
        write (i2, "(I0)") i
        print *, trim(i2), "-th dump files:", trim(traj%dumpfilenames(i))
    end do
    print *, "Nparticles: ", traj%nparticles
    print *, "Nchains: ", traj%nchains
    print *, "Nbeads: ", traj%nbeads
    print *, "Dt: ", traj%dt
    print *, "Dump_freq: ", traj%dump_freq
    print *, "Nframes: ", traj%nframes
    print *, "Is_cubic: ", traj%is_cubic
    print *, "Box_dim allocated: ", allocated(traj%box_dim)
    print *, "Coord allocated: ", allocated(traj%coords)
    print *, "Molecular_IDs allocated: ", allocated(traj%mol)
    print *, "Masses allocated: ", allocated(traj%mass)
    print *, "Timesteps allocated: ", allocated(traj%timesteps)

    call read_traj(traj)
    do i = 1, traj%nframes
        do j = 1, traj%nparticles
            traj%coords(:,j, i) = traj%coords(:,j,i) + traj%image_flag(:,j,i) * (traj%box_dim(2,:,i) - traj%box_dim(1,:,i))
        end do
    end do
    print *, "Coordinates:", (traj%coords(1, 1, i), i=1, traj%nframes)
    atomheader%id = 1
    atomheader%mol = 2
    atomheader%xu = 3
    atomheader%yu = 4
    atomheader%zu = 5

    do i = 1, traj%nframes
        do j = 1, traj%nchains
            traj%coords(:, (j-1)*traj%nbeads+1:j*traj%nbeads, i) = wrap_polymer(traj%coords(:, (j-1)*traj%nbeads+1:j*traj%nbeads,&
            i),traj%box_dim(:, :, i))
        end do
    end do
    call write_lammpstrj(traj, atomheader, "test.lammpstrj")



    call read_Function1DInfo("input.nml", TCinfo)
    call determine_frame_intervals(tcinfo, traj)
    print *, tcinfo%frame_intervals

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
