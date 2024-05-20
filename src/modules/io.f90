module io
    use global_types
    use string_utils
    implicit none

    type :: AtomHeader_Index
        INTEGER(KIND=4) :: id   = 0
        INTEGER(KIND=4) :: mol  = 0
        INTEGER(KIND=4) :: type = 0
        INTEGER(KIND=4) :: xu   = 0
        INTEGER(KIND=4) :: yu   = 0
        INTEGER(KIND=4) :: zu   = 0
        INTEGER(KIND=4) :: x    = 0
        INTEGER(KIND=4) :: y    = 0
        INTEGER(KIND=4) :: z    = 0
        INTEGER(KIND=4) :: ix   = 0
        INTEGER(KIND=4) :: iy   = 0
        INTEGER(KIND=4) :: iz   = 0
    end type AtomHeader_Index


contains
    subroutine read_simulation_params(nmlfilename, traj)
        implicit none
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
            allocate (traj%box_dim(3, 2, traj%nframes))
        else
            allocate (traj%box_dim(3, 3, traj%nframes)) ! l_0, l_1, l_tilte
        end if

        allocate (traj%mass(traj%nparticles), traj%type(traj%nparticles), traj%mol(traj%nparticles))
        allocate (traj%timesteps(traj%nframes))
        allocate (traj%coords(3, traj%nparticles, traj%nframes))
        allocate (traj%image_flag(3, traj%nparticles, traj%nframes))
    end subroutine read_simulation_params

    ! -- interface of reading trajectories -- !
    subroutine read_traj(traj)
        implicit none

        type(trajectory), intent(INOUT) :: traj

        integer :: idx_dumpfile
        integer :: idx_frame = 1
        
        character(LEN=256) :: ext

        ! traj%ndumpfiles で指定された各ファイルに対してループ
        ! Dynamicsの解析の際には時系列に沿った順番に入力すること
        do idx_dumpfile = 1, traj%ndumpfiles
            ! Check file format
            call get_file_extention(traj%dumpfilenames(idx_dumpfile), ext)
            if (ext .ne. "lammpstrj") then
                print *, "Error: dump file is NOT lammpstrj, but ", trim(ext)
                stop

            ! -- 立法、斜方格子セルを問わず絶対座標系のみ -- !
            !else if (traj%is_cubic .eqv. .false.) then
            !    print *, "Error: this program does not support orthorhombic box yet."
            !    stop

            else
                print *, "Start reading dump file    : ", trim(traj%dumpfilenames(idx_dumpfile))
                CALL parse_lammpstrj(traj, idx_dumpfile, idx_frame)
                print *, "Finished reading dump file    : ", trim(traj%dumpfilenames(idx_dumpfile))
            end if
        end do
    end subroutine read_traj

    ! -- lammpstrjのATOMヘッダーを読み込んで座標を読む -- !
    subroutine parse_lammpstrj(traj, idx_dumpfile, idx_frame)
        implicit none

        TYPE(trajectory), INTENT(INOUT) :: traj
        INTEGER, INTENT(IN) :: idx_dumpfile
        INTEGER, INTENT(INOUT) :: idx_frame


        integer(KIND=4), parameter :: dump = 111
        integer :: i, j
        integer :: nparticles
        integer :: nColums
        INTEGER(KIND=1) :: header_flag = 0

        type(AtomHeader_Index) :: idx

        CHARACTER(len=256) :: line
        
        CHARACTER(LEN=:), ALLOCATABLE :: atom_header_parts(:), atom_parts(:)


        open (dump, file=traj%dumpfilenames(idx_dumpfile), status='old')

        header_flag = 0
        do while (.true.)
            ! Start reading header lines
            read(dump, "(A)", end=999) line
            if ( index(line, "TIMESTEP") > 0 ) then
                read(dump, *) traj%timesteps(idx_frame)
            else
                print *, "Error: Header of ", trim(traj%dumpfilenames(idx_dumpfile))
            endif

            read(dump, "(A)") line
            if ( index(line, "NUMBER OF ATOMS") > 0 ) then
                read(dump, *) nparticles
                if (nparticles /= traj%nparticles) then
                    print *, "Error: nparticles does NOT match."
                    print *, "\t From NAMELIST : ", traj%nparticles
                    print *, "\t In trajectoryfile : ", nparticles
                    stop
                endif
            else
                print *, "Error: Header of ", trim(traj%dumpfilenames(idx_dumpfile))
            endif
            

            read(dump, "(A)") line
            if ( index(line, "BOX") > 0 ) then
                read(dump, *) traj%box_dim(1, :, idx_frame)
                read(dump, *) traj%box_dim(2, :, idx_frame)
                read(dump, *) traj%box_dim(3, :, idx_frame)
            else
                print *, "Error: Header of ", trim(traj%dumpfilenames(idx_dumpfile))
            endif

            read(dump, "(A)") line
            if ( index(line, "ITEM: ATOMS") > 0 .and. header_flag == 0) then
                atom_header_parts = split_string(line)
                nColums = size(atom_header_parts) - 2 ! 最初の "ITEM:" "ATOMS" はスキップ

                do i = 1, nColums
                    select case(trim(adjustl(atom_header_parts(i+2)))) ! 最初の "ITEM:" "ATOMS" はスキップ
                        case("id")
                            idx%id = i
                        case("mol")
                            idx%mol = i
                        case("type")
                            idx%type = i
                        case("xu")
                            idx%xu = i
                        case("yu")
                            idx%yu = i
                        case("zu")
                            idx%zu = i
                        case("x")
                            idx%x = i
                        case("y")
                            idx%y = i
                        case("z")
                            idx%z = i
                        case("ix")
                            idx%ix = i
                        case("iy")
                            idx%iy = i
                        case("iz")
                            idx%iz = i
                    end select
                enddo
                header_flag = 1
            endif
            
            do i = 1, traj%nparticles
                read(dump, "(A)") line
                atom_parts = split_string(adjustl(line))
                if (idx%mol /= 0) then
                    read(atom_parts(idx%mol), *) traj%mol(i)
                end if

                if (idx%type /= 0) then
                    read(atom_parts(idx%type), *) traj%type(i)
                end if
                    
                if (idx%xu /= 0) then
                    read(atom_parts(idx%xu), *) traj%coords(1, i, idx_frame)
                end if

                if (idx%yu /= 0) then
                    read(atom_parts(idx%yu), *) traj%coords(2, i, idx_frame)
                end if

                if (idx%xu /= 0) then
                    read(atom_parts(idx%zu), *) traj%coords(3, i, idx_frame)
                end if

                if (idx%x /= 0) then
                    read(atom_parts(idx%x), *) traj%coords(1, i, idx_frame)
                end if

                if (idx%y /= 0) then
                    read(atom_parts(idx%y), *) traj%coords(2, i, idx_frame)
                end if

                if (idx%z /= 0) then
                    read(atom_parts(idx%z), *) traj%coords(3, i, idx_frame)
                end if

                if (idx%ix /= 0) then
                    read(atom_parts(idx%ix), *) traj%image_flag(1, i, idx_frame)
                end if

                if (idx%iy /= 0) then
                    read(atom_parts(idx%iy), *) traj%image_flag(2, i, idx_frame)
                end if

                if (idx%iz /= 0) then
                    read(atom_parts(idx%iz), *) traj%image_flag(3, i, idx_frame)
                end if

                ! Unwrap coordinates if image flags are present
                if (idx%ix /= 0 .and. idx%iy /= 0 .and. idx%iz /= 0) then
                    traj%coords(1, i, idx_frame) = traj%coords(1, i, idx_frame) + &
                        traj%image_flag(1, i, idx_frame) * (traj%box_dim(1, 2, idx_frame) -traj%box_dim(1, 1, idx_frame))
                    traj%coords(2, i, idx_frame) = traj%coords(2, i, idx_frame) + &
                        traj%image_flag(2, i, idx_frame) * (traj%box_dim(2, 2, idx_frame) -traj%box_dim(2, 1, idx_frame))
                    traj%coords(3, i, idx_frame) = traj%coords(3, i, idx_frame) + &
                        traj%image_flag(3, i, idx_frame) * (traj%box_dim(3, 2, idx_frame) -traj%box_dim(3, 1, idx_frame))
                end if
            enddo
            idx_frame = idx_frame + 1
            end do
        999 close(dump)
    end subroutine parse_lammpstrj

    subroutine get_file_extention(filename, ext)
        implicit none

        character(LEN=*), intent(IN) :: filename
        character(LEN=*), intent(OUT) :: ext
        ! local variable
        integer(KIND=4) :: period

        period = index(filename, ".", BACK=.true.)
        ext = filename(period + 1:len(trim(filename)))

    end subroutine get_file_extention


end module io

