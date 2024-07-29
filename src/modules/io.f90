module io
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


    ! 修正の必要あり
    ! =======================================================
    ! ============== Writing LAMMPS paramsectory ==============
    ! =======================================================
    subroutine write_lammpstrj(coords, box_bounds, headers, filename)
        implicit none

        real, dimension(:,:,:), intent(in) :: coords
        type(AtomHeader_Index), intent(in) :: headers
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

end module io

