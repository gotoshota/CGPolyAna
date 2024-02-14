program test
    use global_types
    use io
    use time_correlation_sampling
    use math
    use physical_constants
    implicit none
    type(trajectory) :: traj
    type(TimeCorrelationInfo) :: tcinfo
    INTEGER :: i
    CHARACTER(LEN=5) :: i2

    real, DIMENSION(3) :: a, b

    call read_simulation_params('input.nml', traj)

    ! Display the results
    do i = 1, traj%ndumpfiles
        WRITE(i2, "(I0)") i
        print *, TRIM(i2), "-th dump files:", TRIM(traj%dumpfilenames(i))
    enddo
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

    CALL read_lammpstrj(traj)
    print *, "Coordinates:", traj%coords(1, 1, :)

    CALL read_TimeCorrelationInfo("input.nml", TCinfo)
    CALL determine_frame_intervals(tcinfo, traj%nframes)
    print *, tcinfo%frame_intervals

    do i = 1, 3
        a(i) = i
        b(4-i) = i
    enddo
    print *, cross_product(a, b)
    print *, norm(a)
    print*, norm(b)
    print*, DOT_PRODUCT(a,b)


    print *, '円周率π = ', pi
    print *, 'ボルツマン定数 = ', boltzmann_constant, 'J/K'
    print *, '気体定数 = ', gas_constant, 'J/(mol・K)'
end program test
