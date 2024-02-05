program test
    use global_types
    implicit none
    type(trajectory) :: sim_data

    call read_simulation_params('input.nml', sim_data)

    ! Display the results
    print *, "Nparticles: ", sim_data%nparticles
    print *, "Nchains: ", sim_data%nchains
    print *, "Nbeads: ", sim_data%nbeads
    print *, "Nsteps: ", sim_data%nsteps
    print *, "Dt: ", sim_data%dt
    print *, "Dump_freq: ", sim_data%dump_freq
    print *, "Nframes: ", sim_data%nframes
    print *, "Is_cubic: ", sim_data%is_cubic
    print *, "Box_dim allocated: ", allocated(sim_data%box_dim)
    print *, "Coord allocated: ", allocated(sim_data%coord)
    print *, "Molecular_IDs allocated: ", allocated(sim_data%mol)
    print *, "Atom_types allocated: ", allocated(sim_data%type)
    print *, "Masses allocated: ", allocated(sim_data%mass)
end program test
