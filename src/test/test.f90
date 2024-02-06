program test
    use global_types
    use read_files
    implicit none
    type(trajectory) :: traj
    INTEGER :: i
    CHARACTER(LEN=5) :: i2

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
    print *, "Coord allocated: ", allocated(traj%coord)
    print *, "Molecular_IDs allocated: ", allocated(traj%mol)
    print *, "Masses allocated: ", allocated(traj%mass)
    print *, "Timesteps allocated: ", allocated(traj%timesteps)

    CALL read_lammpstrj(traj)
    print *, "Coordinates:", traj%coord(1, 1, :)

end program test
