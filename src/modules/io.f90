module io
    use global_types
    use coord_convert
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
        integer(KIND=4) :: xs   = 0
        integer(KIND=4) :: ys   = 0
        integer(KIND=4) :: zs   = 0
    end type AtomHeader_Index


contains
    subroutine get_file_extention(filename, ext)
        implicit none

        character(LEN=*), intent(IN) :: filename
        character(LEN=*), intent(OUT) :: ext
        ! local variable
        integer(KIND=4) :: period

        period = index(filename, ".", BACK=.true.)
        ext = filename(period + 1:len(trim(filename)))

    end subroutine get_file_extention

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
            allocate (traj%box_dim(2, 3, traj%nframes))
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
                read(dump, *) traj%box_dim(:, 1, idx_frame)
                read(dump, *) traj%box_dim(:, 2, idx_frame)
                read(dump, *) traj%box_dim(:, 3, idx_frame)
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
                        case("xs")
                            idx%xs = i
                        case("ys")
                            idx%ys = i
                        case("zs")
                            idx%zs = i
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

                if (idx%xs /= 0) then
                    read(atom_parts(idx%xs), *) traj%coords(1, i, idx_frame)
                end if

                if (idx%ys /= 0) then
                    read(atom_parts(idx%ys), *) traj%coords(2, i, idx_frame)
                end if

                if (idx%zs /= 0) then
                    read(atom_parts(idx%zs), *) traj%coords(3, i, idx_frame)
                end if

                ! Unwrap coordinates if image flags are present
                if (idx%ix /= 0 .and. idx%iy /= 0 .and. idx%iz /= 0) then
                    traj%coords(1, i, idx_frame) = traj%coords(1, i, idx_frame) + &
                        traj%image_flag(1, i, idx_frame) * (traj%box_dim(2, 1, idx_frame) -traj%box_dim(1, 1, idx_frame))
                    traj%coords(2, i, idx_frame) = traj%coords(2, i, idx_frame) + &
                        traj%image_flag(2, i, idx_frame) * (traj%box_dim(2, 2, idx_frame) -traj%box_dim(1, 2, idx_frame))
                    traj%coords(3, i, idx_frame) = traj%coords(3, i, idx_frame) + &
                        traj%image_flag(3, i, idx_frame) * (traj%box_dim(2, 3, idx_frame) -traj%box_dim(1, 3, idx_frame))
                end if

                if (idx%xs /= 0 .and. idx%ys /= 0 .and. idx%zs /= 0) then
                    print *, "Error: xs, ys, zs are not supported yet."
                    stop
                end if
            enddo
            idx_frame = idx_frame + 1
            if (idx_frame > traj%nframes) exit
        end do
        if (traj%is_cubic == .false.) then
            traj = triclinic_to_orthogonal(traj)
            print *, "Successfully converted triclinic to orthogonal."
            traj%is_cubic = .true.
        end if
        999 close(dump)
    end subroutine parse_lammpstrj

    ! =======================================================
    ! ============== Writing LAMMPS trajectory ==============
    ! =======================================================
    subroutine write_lammpstrj(traj, headers, filename)
        implicit none

        type(trajectory), intent(in) :: traj
        type(AtomHeader_Index), intent(in) :: headers
        character(len=*), intent(in) :: filename

        integer :: frame, i
        integer(KIND=4), parameter :: dump = 111

        ! Create format string based on AtomHeader_Index
        character(len=256) :: format_string
        character(len=256) :: header_string
        format_string = ""
        header_string = "ITEM: ATOMS,"

        if (headers%id /= 0) then
            format_string = trim(format_string) // "I6, "
            header_string = trim(header_string) // "id,"
        end if
        if (headers%mol /= 0) then
            format_string = trim(format_string) // "I6, "
            header_string = trim(header_string) // "mol,"
        end if
        if (headers%type /= 0) then
            format_string = trim(format_string) // "I6, "
            header_string = trim(header_string) // "type,"
        end if
        if (headers%xu /= 0) then
            format_string = trim(format_string) // "F8.3, "
            header_string = trim(header_string) // "xu,"
        end if
        if (headers%yu /= 0) then
            format_string = trim(format_string) // "F8.3, "
            header_string = trim(header_string) // "yu,"
        end if
        if (headers%zu /= 0) then
            format_string = trim(format_string) // "F8.3, "
            header_string = trim(header_string) // "zu,"
        end if
        if (headers%x /= 0) then
            format_string = trim(format_string) // "F8.3, "
            header_string = trim(header_string) // "x,"
        end if
        if (headers%y /= 0) then
            format_string = trim(format_string) // "F8.3, "
            header_string = trim(header_string) // "y,"
        end if
        if (headers%z /= 0) then
            format_string = trim(format_string) // "F8.3, "
            header_string = trim(header_string) // "z,"
        end if

        ! Remove the trailing comma and space
        header_string = trim(header_string)
        if (len_trim(header_string) > 0 .and. header_string(len_trim(header_string):len_trim(header_string)) == ",") then
            header_string = header_string(1:len_trim(header_string)-1)
        end if

        ! Replace commas with spaces
        header_string = replace(header_string, ',', ' ')

        ! Remove the trailing comma and space
        format_string = trim(adjustl(format_string(1:len_trim(format_string)-2)))

        open(unit=dump, file=filename, status='replace')
        do frame = 1, traj%nframes

            ! Write header
            write(dump, "(A)") "ITEM: TIMESTEP"
            write(dump, *) traj%timesteps(frame)
            write(dump, "(A)") "ITEM: NUMBER OF ATOMS"
            write(dump, *) traj%nparticles
            write(dump, "(A)") "ITEM: BOX BOUNDS pp pp pp"
            write(dump, *) traj%box_dim(1, 1, frame), traj%box_dim(2, 1, frame)
            write(dump, *) traj%box_dim(1, 2, frame), traj%box_dim(2, 2, frame)
            write(dump, *) traj%box_dim(1, 3, frame), traj%box_dim(2, 3, frame)
            write(dump, '(A)') trim(header_string)  ! Using '(A)' format to write the full header_string

            ! Write atom data
            do i = 1, traj%nparticles
                if (headers%id /= 0) write(dump, '(I6, x)', advance='no') i
                if (headers%mol /= 0) write(dump, '(I6, x)', advance='no') traj%mol(i)
                if (headers%type /= 0) write(dump, '(I6, x)', advance='no') traj%type(i)
                if (headers%xu /= 0) write(dump, '(F8.3, x)', advance='no') traj%coords(1, i, frame)
                if (headers%yu /= 0) write(dump, '(F8.3, x)', advance='no') traj%coords(2, i, frame)
                if (headers%zu /= 0) write(dump, '(F8.3, x)', advance='no') traj%coords(3, i, frame)
                if (headers%x /= 0) write(dump, '(F8.3, x)', advance='no') traj%coords(1, i, frame)
                if (headers%y /= 0) write(dump, '(F8.3, x)', advance='no') traj%coords(2, i, frame)
                if (headers%z /= 0) write(dump, '(F8.3, x)', advance='no') traj%coords(3, i, frame)
                write(dump, *) ! end the line
            end do

        end do
        close(dump)
    end subroutine write_lammpstrj

end module io

