module read_files
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
        CHARACTER(len=100), DIMENSION(100) :: dumpfilenames

        namelist /simulation_params/ dumpfilenames, ndumpfiles, nparticles, nchains, nbeads, &
        dt, dump_freq, nframes, is_cubic

        ! read namelist
        open(unit=10, file=nmlfilename, status='old')
            read(10, simulation_params)
        close(10)

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

        if (allocated(traj%box_dim)) deallocate(traj%box_dim)
        if (traj%is_cubic) then
            allocate(traj%box_dim(3, traj%nframes))
        else
            allocate(traj%box_dim(6, traj%nframes))
        endif

        ALLOCATE(traj%mass(traj%nparticles), traj%type(traj%nparticles), traj%mol(traj%nparticles))
        ALLOCATE(traj%timesteps(traj%nframes))
        ALLOCATE(traj%coords(3, traj%nparticles, traj%nframes))
    end subroutine read_simulation_params

    subroutine read_lammpstrj(traj)
        implicit none

        TYPE(trajectory), INTENT(INOUT) :: traj
        ! local variables
        CHARACTER(LEN=256)          :: ext
        integer                     :: i, file_id
        integer                     :: snapshot_id = 1
        double precision            :: dummy
        INTEGER(KIND=4), PARAMETER  :: dump=111
        integer                     :: nparticles
        real                        :: x_min, x_max
        real                        :: y_min, y_max
        real                        :: z_min, z_max
        CHARACTER(LEN=256)          :: atom_header
        CHARACTER(LEN=20)           :: atom_header_simple_wrap = "ITEM: ATOMS id x y z"
        CHARACTER(LEN=23)           :: atom_header_simple_unwrap = "ITEM: ATOMS id xu yu zu"
        CHARACTER(LEN=27)           :: atom_header_with_molid_unwrap = "ITEM: ATOMS id mol xu yu zu"
        CHARACTER(LEN=24)           :: atom_header_with_molid_wrap = "ITEM: ATOMS id mol x y z"
        

        print *, ""
        fileloop: do file_id = 1, traj%ndumpfiles
            ! Check file format
            call get_file_extention(traj%dumpfilenames(file_id), ext)
            if (ext /= "lammpstrj") then
                print *, "Error: dump file is NOT lammpstrj, ", TRIM(ext)
                stop
            else if (traj%is_cubic .eqv. .false.) then
                print *, "Error: this program does not support orthorhombic box yet."
                stop
            else 
                print *, "Start reading dump file    : ", TRIM(traj%dumpfilenames(file_id))
            endif

            ! Define 2 types of ATOM header
            open(dump, file=traj%dumpfilenames(file_id), status='old')
                do
                    ! -- Header -- !
                    ! ITEM: TIMESTEP
                    READ(dump, "()", end=999)
                    READ(dump, *) traj%timesteps(snapshot_id)

                    ! ITEM: NUMBER OF ATOMS
                    READ(dump, "()")
                    READ(dump, *) nparticles
                    if (nparticles .ne. traj%nparticles) then
                        print *, "Error: the numbers of particles do not match between Namelist and " &
                        , TRIM(traj%dumpfilenames(file_id)), "at timestep = ", traj%timesteps(snapshot_id)
                    endif

                    ! ITEM: BOX BOUNDS pp pp pp
                    READ(dump, "()")
                    if (traj%is_cubic) then
                        READ(dump, *) x_min, x_max
                        READ(dump, *) y_min, y_max
                        READ(dump, *) z_min, z_max
                        traj%box_dim(1, snapshot_id) = x_max - x_min
                        traj%box_dim(2, snapshot_id) = y_max - y_min
                        traj%box_dim(3, snapshot_id) = z_max - z_min
                    else
                        print *, "Error: this program support only cubic box."
                        stop
                    endif

                    ! -- Check dump format -- !
                    READ(dump, "(A)") atom_header
                    ! ITEM: ATOMS id x y z
                    if (TRIM(atom_header) == atom_header_simple_wrap .or. &
                        TRIM(atom_header) == atom_header_simple_unwrap) then
                        do i = 1, traj%nparticles
                            READ(dump, *) dummy, traj%coords(:, i, snapshot_id)
                        enddo
                    ! ITEM: ATOMS id mol xu yu zu
                    else if (TRIM(atom_header) == atom_header_with_molid_wrap .or. &
                        TRIM(atom_header) == atom_header_with_molid_unwrap) then
                        do i = 1, traj%nparticles
                            READ(dump, *) dummy, traj%mol(i), traj%coords(:, i, snapshot_id)
                        enddo
                    else 
                        print *, "ATOM header does not match."
                        stop
                    endif
                    snapshot_id = snapshot_id + 1
                    if (snapshot_id == traj%nframes) exit fileloop
                enddo
            999 print *, "Finished reading dump file : ", TRIM(traj%dumpfilenames(file_id))
            print *, ""
            CLOSE(dump)
        enddo fileloop
        print *, "Finished reading ALL dump file"
        print *, "=============================="
    end subroutine read_lammpstrj

    subroutine get_file_extention(filename, ext)
        implicit none

        CHARACTER(LEN=*), INTENT(IN)        :: filename
        CHARACTER(LEN=*), INTENT(OUT)     :: ext
        ! local variable
        INTEGER(KIND=4)         :: period

        period = INDEX(filename, ".", BACK=.true.)
        ext = filename(period+1 : LEN(TRIM(filename))) 

    end subroutine get_file_extention

end module

