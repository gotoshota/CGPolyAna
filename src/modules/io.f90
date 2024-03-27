module io
    use global_types
    implicit none

contains
    subroutine read_simulation_params(nmlfilename, traj)
        character(len=*), intent(in) :: nmlfilename
        type(trajectory), intent(out) :: traj

        ! local variables
        integer :: nparticles, nchains, nbeads, nframes
        real :: dt
        integer :: dump_freq, ndumpfiles
        logical :: is_cubic
        character(len=100), dimension(100) :: dumpfilenames

        namelist /simulation_params/ dumpfilenames, ndumpfiles, nparticles, nchains, nbeads, &
        dt, dump_freq, nframes, is_cubic

        ! read namelist
        open (unit=10, file=nmlfilename, status='old')
            read (10, simulation_params)
        close (10)

        ! ユーザー定義型のインスタンスに読み込んだデータを代入
        traj%dumpfilenames = dumpfilenames
        traj%ndumpfiles = ndumpfiles
        traj%nparticles = nparticles
        traj%nchains = nchains
        traj%nbeads = nbeads
        traj%dt = dt
        traj%nframes = nframes
        traj%dump_freq = dump_freq
        traj%is_cubic = is_cubic

        if (allocated(traj%box_dim)) deallocate (traj%box_dim)
        if (traj%is_cubic) then
            allocate (traj%box_dim(3, traj%nframes))
        else
            allocate (traj%box_dim(6, traj%nframes)) ! xx, yy, zz, tilte xy, tilte xz, tilte yz
        end if

        allocate (traj%mass(traj%nparticles), traj%type(traj%nparticles), traj%mol(traj%nparticles))
        allocate (traj%timesteps(traj%nframes))
        allocate (traj%coords(3, traj%nparticles, traj%nframes))
    end subroutine read_simulation_params

    subroutine read_lammpstrj(traj)
        implicit none

        type(trajectory), intent(INOUT) :: traj
        ! local variables
        character(LEN=256) :: ext
        integer :: i, file_id
        integer :: snapshot_id = 1
        double precision :: dummy
        integer(KIND=4), parameter :: dump = 111
        integer :: nparticles
        real :: x_min, x_max
        real :: y_min, y_max
        real :: z_min, z_max
        real :: tilte_xy, tilte_xz, tilte_yz
        character(LEN=256) :: atom_header
        character(LEN=20) :: atom_header_simple_wrap = "ITEM: ATOMS id x y z"
        character(LEN=23) :: atom_header_simple_unwrap = "ITEM: ATOMS id xu yu zu"
        character(LEN=27) :: atom_header_with_molid_unwrap = "ITEM: ATOMS id mol xu yu zu"
        character(LEN=24) :: atom_header_with_molid_wrap = "ITEM: ATOMS id mol x y z"
        
        print *, ""
        fileloop: do file_id = 1, traj%ndumpfiles
            ! Check file format
            call get_file_extention(traj%dumpfilenames(file_id), ext)
            if (ext .ne. "lammpstrj") then
                print *, "Error: dump file is NOT lammpstrj, ", trim(ext)
                stop
            else if (traj%is_cubic .eqv. .false.) then
                print *, "Error: this program does not support orthorhombic box yet."
                stop
            else
                print *, "Start reading dump file    : ", trim(traj%dumpfilenames(file_id))
            end if

            ! Define 2 types of ATOM header
            open (dump, file=traj%dumpfilenames(file_id), status='old')
                do
                    ! -- Header -- !
                    ! ITEM: TIMESTEP
                    read (dump, "()", end=999)
                    if (snapshot_id .gt. traj%nframes) exit fileloop
                    read (dump, *) traj%timesteps(snapshot_id)

                    ! ITEM: NUMBER OF ATOMS
                    read (dump, "()")
                    read (dump, *) nparticles
                    if (nparticles .ne. traj%nparticles) then
                        print *, "Error: the numbers of particles do not match between Namelist and " &
                        , trim(traj%dumpfilenames(file_id)), "at timestep = ", traj%timesteps(snapshot_id)
                    end if

                    ! ITEM: BOX BOUNDS pp pp pp
                    read (dump, "()")
                    if (traj%is_cubic) then
                        read (dump, *) x_min, x_max
                        read (dump, *) y_min, y_max
                        read (dump, *) z_min, z_max
                        traj%box_dim(1, snapshot_id) = x_max - x_min
                        traj%box_dim(2, snapshot_id) = y_max - y_min
                        traj%box_dim(3, snapshot_id) = z_max - z_min
                    else
                        read (dump, *) x_min, x_max, tilte_xy
                        read (dump, *) y_min, y_max, tilte_xz
                        read (dump, *) z_min, z_max, tilte_yz
                        traj%box_dim(1, snapshot_id) = x_max - x_min
                        traj%box_dim(2, snapshot_id) = y_max - y_min
                        traj%box_dim(3, snapshot_id) = z_max - z_min
                        traj%box_dim(4, snapshot_id) = tilte_xy
                        traj%box_dim(5, snapshot_id) = tilte_xz
                        traj%box_dim(6, snapshot_id) = tilte_yz
                    end if

                    ! -- Check dump format -- !
                    read (dump, "(A)") atom_header
                    ! ITEM: ATOMS id x y z
                    if (trim(atom_header) .eq. atom_header_simple_wrap .or. &
                        trim(atom_header) .eq. atom_header_simple_unwrap) then
                        do i = 1, traj%nparticles
                            read (dump, *) dummy, traj%coords(:, i, snapshot_id)
                        end do
                    ! ITEM: ATOMS id mol xu yu zu
                    else if (trim(atom_header) .eq. atom_header_with_molid_wrap .or.&
                        trim(atom_header) .eq. atom_header_with_molid_unwrap) then
                        do i = 1, traj%nparticles
                            read (dump, *) dummy, traj%mol(i), traj%coords(:, i, snapshot_id)
                        end do
                    else
                        print *, "ATOM header does not match."
                        stop
                    end if
                    snapshot_id = snapshot_id + 1
                end do
999         print *, "Finished reading dump file : ", trim(traj%dumpfilenames(file_id))
            print *, ""
            close (dump)
        end do fileloop
        print *, "Finished reading ALL dump file"
        print *, "=============================="
    end subroutine read_lammpstrj

    subroutine get_file_extention(filename, ext)
        implicit none

        character(LEN=*), intent(IN) :: filename
        character(LEN=*), intent(OUT) :: ext
        ! local variable
        integer(KIND=4) :: period

        period = index(filename, ".", BACK=.true.)
        ext = filename(period + 1:len(trim(filename)))

    end subroutine get_file_extention

end module

