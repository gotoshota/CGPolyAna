program test
    use global_types
    use lammpsIO
    use correlation_function 
    use coord_convert
    use math
    use physical_constants
    use string_utils
    implicit none

    type(lammpstrjReader) :: reader
    type(MDParams) :: params
    type(Function1d) :: tcinfo
    type(AtomIndex) :: atomheader
    integer :: i, j, idx_frame
    character(LEN=5) :: i2
    character(LEN=100) :: arg
    real, dimension(3) :: a, b
    real, allocatable :: coords(:,:,:), coms(:,:,:)
    double precision, allocatable :: box_bounds(:,:,:)

    ! get arg as input 
    call get_command_argument(1, arg)
    call read_MDParams(arg, params)
    ! check params
    call print_MDParams(params)
    call define_atomheader(atomheader)
    ! read lammps trajectory file
    allocate(coords(3, params%nbeads*params%nchains, params%nframes))
    allocate(coms(3, params%nchains, params%nframes))
    allocate(box_bounds(3, 2, params%nframes))
    do i = 1, params%ndumpfiles
        call reader%open(params%dumpfilenames(i))
        idx_frame = 0
        do while (.not. reader%end_of_file)
            idx_frame = idx_frame + 1
            call reader%read()
            if (reader%end_of_file) exit
            do j = 1, params%nchains
                coords(:, (j-1)*params%nbeads+1:j*params%nbeads, idx_frame) = &
                    wrap_polymer(reader%coords(:, (j-1)*params%nbeads+1:j*params%nbeads), reader%box_bounds)
                coms(:, j, idx_frame) = center_of_mass(coords(:, (j-1)*params%nbeads+1:j*params%nbeads, idx_frame))
            end do
            box_bounds(:, :, idx_frame) = reader%box_bounds
            print*, "box_bounds = ", box_bounds(:, :, idx_frame)
            print*, reader%box_bounds
        end do
        print*, "box_bounds = ", box_bounds(:, :, :)
        call write_lammpstrj(coords, box_bounds, atomheader, 'test.lammpstrj')
        call write_lammpstrj(coms, box_bounds, atomheader, 'test_coms.lammpstrj')
    end do
    ! Display the results
    print *, '円周率π = ', pi
    print *, 'ボルツマン定数 = ', boltzmann_constant, 'J/K'
    print *, '気体定数 = ', gas_constant, 'J/(mol・K)'
contains 
    subroutine print_MDParams(params)
        type(MDParams), intent(in) :: params
        integer :: i
        character(LEN=5) :: i2

        print *, 'MDParams:'
        do i = 1, params%ndumpfiles
            write(i2, '(I2)') i
            print *, '  dumpfilenames(', trim(i2), ') = ', trim(params%dumpfilenames(i))
        end do
        print *, '  ndumpfiles = ', params%ndumpfiles
        print *, '  nchains = ', params%nchains
        print *, '  nbeads = ', params%nbeads
        print *, '  nframes = ', params%nframes
        print *, '  dump_freq = ', params%dump_freq
        print *, '  dt = ', params%dt
        print *, '  is_cubic = ', params%is_cubic
        print *, '  is_wrap = ', params%is_wrap
        print *, ""
    end subroutine print_MDParams
    
    subroutine define_atomheader(atomheader)
        type(AtomIndex), intent(out) :: atomheader

        atomheader%id = 1
        atomheader%xu = 2
        atomheader%yu = 3
        atomheader%zu = 4
    end subroutine define_atomheader

    subroutine write_lammpstrj(coords, box_bounds, headers, filename)
        implicit none

        real, dimension(:,:,:), intent(in) :: coords
        type(AtomIndex), intent(in) :: headers
        double precision, dimension(:,:,:), intent(in) :: box_bounds
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
        do frame = 1, size(coords, 3)

            ! Write header
            write(dump, "(A)") "ITEM: TIMESTEP"
            write(dump, *) frame
            write(dump, "(A)") "ITEM: NUMBER OF ATOMS"
            write(dump, *) size(coords, 2)
            if ( size(box_bounds, 1) == 2) then
                write(dump, "(A)") "ITEM: BOX BOUNDS pp pp pp"
            else
                write(dump, "(A)") "ITEM: BOX BOUNDS xy xz yz pp pp pp"
            end if
            write(dump, *) (box_bounds(i, 1, frame), i=1, size(box_bounds, 1))
            write(dump, *) (box_bounds(i, 2, frame), i=1, size(box_bounds, 1))
            write(dump, *) (box_bounds(i, 3, frame), i=1, size(box_bounds, 1))
            write(dump, '(A)') trim(header_string)  ! Using '(A)' format to write the full header_string

            ! Write atom data
            do i = 1, size(coords, 2)
                if (headers%id /= 0) write(dump, '(I6, x)', advance='no') i
                !if (headers%mol /= 0) write(dump, '(I6, x)', advance='no') params%mol(i)
                !if (headers%type /= 0) write(dump, '(I6, x)', advance='no') params%type(i)
                if (headers%xu /= 0) write(dump, '(F8.3, x)', advance='no') coords(1, i, frame)
                if (headers%yu /= 0) write(dump, '(F8.3, x)', advance='no') coords(2, i, frame)
                if (headers%zu /= 0) write(dump, '(F8.3, x)', advance='no') coords(3, i, frame)
                if (headers%x /= 0) write(dump, '(F8.3, x)', advance='no') coords(1, i, frame)
                if (headers%y /= 0) write(dump, '(F8.3, x)', advance='no') coords(2, i, frame)
                if (headers%z /= 0) write(dump, '(F8.3, x)', advance='no') coords(3, i, frame)
                write(dump, *) ! end the line
            end do

        end do
        close(dump)
    end subroutine write_lammpstrj

end program test
