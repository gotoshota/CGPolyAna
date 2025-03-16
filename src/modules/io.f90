!> @file io.f90
!> @brief ファイル入出力を行うためのモジュール
!> @details LAMMPSのトラジェクトリファイルの読み書きなどの機能を提供します
!> @author CGPolyAna開発チーム
!> @date 2025-03-16
!> @version 1.0
module io
    use error_handling
    use string_utils
    implicit none

    !> @brief LAMMPSのトラジェクトリファイルのヘッダーインデックスを格納する型
    type :: AtomHeader_Index
        INTEGER(KIND=4) :: id   = 0  !< 原子ID
        INTEGER(KIND=4) :: mol  = 0  !< 分子ID
        INTEGER(KIND=4) :: type = 0  !< 原子タイプ
        INTEGER(KIND=4) :: xu   = 0  !< アンラップされたx座標
        INTEGER(KIND=4) :: yu   = 0  !< アンラップされたy座標
        INTEGER(KIND=4) :: zu   = 0  !< アンラップされたz座標
        INTEGER(KIND=4) :: x    = 0  !< x座標
        INTEGER(KIND=4) :: y    = 0  !< y座標
        INTEGER(KIND=4) :: z    = 0  !< z座標
        INTEGER(KIND=4) :: ix   = 0  !< xイメージフラグ
        INTEGER(KIND=4) :: iy   = 0  !< yイメージフラグ
        INTEGER(KIND=4) :: iz   = 0  !< zイメージフラグ
        integer(KIND=4) :: xs   = 0  !< スケールされたx座標
        integer(KIND=4) :: ys   = 0  !< スケールされたy座標
        integer(KIND=4) :: zs   = 0  !< スケールされたz座標
    end type AtomHeader_Index

contains
    !> @brief ファイルの拡張子を取得する
    !> @param[in] filename ファイル名
    !> @param[out] ext 拡張子
    !> @param[out] error エラー情報（オプション）
    !> @return エラーコード（0: 成功, 非0: エラー）
    function get_file_extension(filename, ext, error) result(ierr)
        implicit none
        character(LEN=*), intent(in) :: filename
        character(LEN=*), intent(out) :: ext
        type(ErrorInfo), intent(out), optional :: error
        integer :: ierr
        
        ! local variable
        integer(KIND=4) :: period

        ! エラー情報の初期化
        if (present(error)) call error%clear()
        ierr = 0
        
        ! 入力チェック
        if (len_trim(filename) == 0) then
            if (present(error)) then
                call error%set(ERR_INVALID_PARAMETER, "Empty filename", "get_file_extension")
            end if
            ierr = ERR_INVALID_PARAMETER
            ext = ""
            return
        end if

        period = index(filename, ".", BACK=.true.)
        if (period == 0) then
            if (present(error)) then
                call error%set(ERR_INVALID_FORMAT, "No extension found in filename: " // trim(filename), "get_file_extension")
            end if
            ierr = ERR_INVALID_FORMAT
            ext = ""
            return
        end if
        
        ext = filename(period + 1:len(trim(filename)))
    end function get_file_extension

    !> @brief LAMMPSのトラジェクトリファイルを書き込む
    !> @param[in] coords 座標配列
    !> @param[in] box_bounds ボックスの境界
    !> @param[in] headers ヘッダーインデックス
    !> @param[in] filename 出力ファイル名
    !> @param[out] error エラー情報（オプション）
    !> @return エラーコード（0: 成功, 非0: エラー）
    function write_lammpstrj(coords, box_bounds, headers, filename, error) result(ierr)
        implicit none
        real, dimension(:,:,:), intent(in) :: coords
        type(AtomHeader_Index), intent(in) :: headers
        double precision, dimension(:,:,:), intent(in) :: box_bounds
        character(len=*), intent(in) :: filename
        type(ErrorInfo), intent(out), optional :: error
        integer :: ierr
        
        ! local variables
        integer :: frame, i, ios
        integer(KIND=4), parameter :: dump = 111
        type(ErrorInfo) :: local_error

        ! Create format string based on AtomHeader_Index
        character(len=256) :: format_string
        character(len=256) :: header_string

        ! エラー情報の初期化
        if (present(error)) call error%clear()
        ierr = 0
        
        ! 入力チェック
        if (size(coords, 1) /= 3) then
            if (present(error)) then
                call error%set(ERR_INVALID_PARAMETER, "coords must be 3xNxM array", "write_lammpstrj")
            end if
            ierr = ERR_INVALID_PARAMETER
            return
        end if
        
        if (size(box_bounds, 2) /= 3) then
            if (present(error)) then
                call error%set(ERR_INVALID_PARAMETER, "box_bounds must be Nx3xM array", "write_lammpstrj")
            end if
            ierr = ERR_INVALID_PARAMETER
            return
        end if
        
        if (size(coords, 3) /= size(box_bounds, 3)) then
            if (present(error)) then
                call error%set(ERR_INVALID_PARAMETER, "coords and box_bounds must have same number of frames", "write_lammpstrj")
            end if
            ierr = ERR_INVALID_PARAMETER
            return
        end if

        ! ヘッダー文字列の作成
        format_string = ""
        header_string = "ITEM: ATOMS"

        if (headers%id /= 0) then
            format_string = trim(format_string) // "I6, "
            header_string = trim(header_string) // " id"
        end if
        if (headers%mol /= 0) then
            format_string = trim(format_string) // "I6, "
            header_string = trim(header_string) // " mol"
        end if
        if (headers%type /= 0) then
            format_string = trim(format_string) // "I6, "
            header_string = trim(header_string) // " type"
        end if
        if (headers%xu /= 0) then
            format_string = trim(format_string) // "F8.3, "
            header_string = trim(header_string) // " xu"
        end if
        if (headers%yu /= 0) then
            format_string = trim(format_string) // "F8.3, "
            header_string = trim(header_string) // " yu"
        end if
        if (headers%zu /= 0) then
            format_string = trim(format_string) // "F8.3, "
            header_string = trim(header_string) // " zu"
        end if
        if (headers%x /= 0) then
            format_string = trim(format_string) // "F8.3, "
            header_string = trim(header_string) // " x"
        end if
        if (headers%y /= 0) then
            format_string = trim(format_string) // "F8.3, "
            header_string = trim(header_string) // " y"
        end if
        if (headers%z /= 0) then
            format_string = trim(format_string) // "F8.3, "
            header_string = trim(header_string) // " z"
        end if
        if (headers%ix /= 0) then
            format_string = trim(format_string) // "I6, "
            header_string = trim(header_string) // " ix"
        end if
        if (headers%iy /= 0) then
            format_string = trim(format_string) // "I6, "
            header_string = trim(header_string) // " iy"
        end if
        if (headers%iz /= 0) then
            format_string = trim(format_string) // "I6, "
            header_string = trim(header_string) // " iz"
        end if
        if (headers%xs /= 0) then
            format_string = trim(format_string) // "F8.3, "
            header_string = trim(header_string) // " xs"
        end if
        if (headers%ys /= 0) then
            format_string = trim(format_string) // "F8.3, "
            header_string = trim(header_string) // " ys"
        end if
        if (headers%zs /= 0) then
            format_string = trim(format_string) // "F8.3, "
            header_string = trim(header_string) // " zs"
        end if

        ! Remove the trailing comma and space
        if (len_trim(format_string) > 2) then
            format_string = format_string(1:len_trim(format_string)-2)
        end if

        ! ファイルを開く
        open(unit=dump, file=filename, status='replace', iostat=ios)
        if (ios /= 0) then
            if (present(error)) then
                call error%set(ERR_FILE_NOT_FOUND, "Failed to open file for writing: " // trim(filename), "write_lammpstrj")
            end if
            ierr = ERR_FILE_NOT_FOUND
            return
        end if
        
        ! フレームごとにデータを書き込む
        do frame = 1, size(coords, 3)
            ! Write header
            write(dump, "(A)", iostat=ios) "ITEM: TIMESTEP"
            if (ios /= 0) then
                if (present(error)) then
                    call error%set(ERR_INVALID_FORMAT, "Failed to write timestep header", "write_lammpstrj")
                end if
                ierr = ERR_INVALID_FORMAT
                close(dump)
                return
            end if
            
            write(dump, *, iostat=ios) frame
            if (ios /= 0) then
                if (present(error)) then
                    call error%set(ERR_INVALID_FORMAT, "Failed to write timestep", "write_lammpstrj")
                end if
                ierr = ERR_INVALID_FORMAT
                close(dump)
                return
            end if
            
            write(dump, "(A)", iostat=ios) "ITEM: NUMBER OF ATOMS"
            if (ios /= 0) then
                if (present(error)) then
                    call error%set(ERR_INVALID_FORMAT, "Failed to write number of atoms header", "write_lammpstrj")
                end if
                ierr = ERR_INVALID_FORMAT
                close(dump)
                return
            end if
            
            write(dump, *, iostat=ios) size(coords, 2)
            if (ios /= 0) then
                if (present(error)) then
                    call error%set(ERR_INVALID_FORMAT, "Failed to write number of atoms", "write_lammpstrj")
                end if
                ierr = ERR_INVALID_FORMAT
                close(dump)
                return
            end if
            
            if (size(box_bounds, 1) == 2) then
                write(dump, "(A)", iostat=ios) "ITEM: BOX BOUNDS pp pp pp"
            else
                write(dump, "(A)", iostat=ios) "ITEM: BOX BOUNDS xy xz yz pp pp pp"
            end if
            if (ios /= 0) then
                if (present(error)) then
                    call error%set(ERR_INVALID_FORMAT, "Failed to write box bounds header", "write_lammpstrj")
                end if
                ierr = ERR_INVALID_FORMAT
                close(dump)
                return
            end if
            
            write(dump, *, iostat=ios) (box_bounds(i, 1, frame), i=1, size(box_bounds, 1))
            if (ios /= 0) then
                if (present(error)) then
                    call error%set(ERR_INVALID_FORMAT, "Failed to write box bounds (1)", "write_lammpstrj")
                end if
                ierr = ERR_INVALID_FORMAT
                close(dump)
                return
            end if
            
            write(dump, *, iostat=ios) (box_bounds(i, 2, frame), i=1, size(box_bounds, 1))
            if (ios /= 0) then
                if (present(error)) then
                    call error%set(ERR_INVALID_FORMAT, "Failed to write box bounds (2)", "write_lammpstrj")
                end if
                ierr = ERR_INVALID_FORMAT
                close(dump)
                return
            end if
            
            write(dump, *, iostat=ios) (box_bounds(i, 3, frame), i=1, size(box_bounds, 1))
            if (ios /= 0) then
                if (present(error)) then
                    call error%set(ERR_INVALID_FORMAT, "Failed to write box bounds (3)", "write_lammpstrj")
                end if
                ierr = ERR_INVALID_FORMAT
                close(dump)
                return
            end if
            
            write(dump, '(A)', iostat=ios) trim(header_string)
            if (ios /= 0) then
                if (present(error)) then
                    call error%set(ERR_INVALID_FORMAT, "Failed to write atom header", "write_lammpstrj")
                end if
                ierr = ERR_INVALID_FORMAT
                close(dump)
                return
            end if

            ! Write atom data
            do i = 1, size(coords, 2)
                if (headers%id /= 0) write(dump, '(I6, x)', advance='no', iostat=ios) i
                if (ios /= 0) then
                    if (present(error)) then
                        call error%set(ERR_INVALID_FORMAT, "Failed to write atom ID", "write_lammpstrj")
                    end if
                    ierr = ERR_INVALID_FORMAT
                    close(dump)
                    return
                end if
                
                !if (headers%mol /= 0) write(dump, '(I6, x)', advance='no') params%mol(i)
                !if (headers%type /= 0) write(dump, '(I6, x)', advance='no') params%type(i)
                
                if (headers%xu /= 0) write(dump, '(F8.3, x)', advance='no', iostat=ios) coords(1, i, frame)
                if (ios /= 0) then
                    if (present(error)) then
                        call error%set(ERR_INVALID_FORMAT, "Failed to write xu coordinate", "write_lammpstrj")
                    end if
                    ierr = ERR_INVALID_FORMAT
                    close(dump)
                    return
                end if
                
                if (headers%yu /= 0) write(dump, '(F8.3, x)', advance='no', iostat=ios) coords(2, i, frame)
                if (ios /= 0) then
                    if (present(error)) then
                        call error%set(ERR_INVALID_FORMAT, "Failed to write yu coordinate", "write_lammpstrj")
                    end if
                    ierr = ERR_INVALID_FORMAT
                    close(dump)
                    return
                end if
                
                if (headers%zu /= 0) write(dump, '(F8.3, x)', advance='no', iostat=ios) coords(3, i, frame)
                if (ios /= 0) then
                    if (present(error)) then
                        call error%set(ERR_INVALID_FORMAT, "Failed to write zu coordinate", "write_lammpstrj")
                    end if
                    ierr = ERR_INVALID_FORMAT
                    close(dump)
                    return
                end if
                
                if (headers%x /= 0) write(dump, '(F8.3, x)', advance='no', iostat=ios) coords(1, i, frame)
                if (ios /= 0) then
                    if (present(error)) then
                        call error%set(ERR_INVALID_FORMAT, "Failed to write x coordinate", "write_lammpstrj")
                    end if
                    ierr = ERR_INVALID_FORMAT
                    close(dump)
                    return
                end if
                
                if (headers%y /= 0) write(dump, '(F8.3, x)', advance='no', iostat=ios) coords(2, i, frame)
                if (ios /= 0) then
                    if (present(error)) then
                        call error%set(ERR_INVALID_FORMAT, "Failed to write y coordinate", "write_lammpstrj")
                    end if
                    ierr = ERR_INVALID_FORMAT
                    close(dump)
                    return
                end if
                
                if (headers%z /= 0) write(dump, '(F8.3, x)', advance='no', iostat=ios) coords(3, i, frame)
                if (ios /= 0) then
                    if (present(error)) then
                        call error%set(ERR_INVALID_FORMAT, "Failed to write z coordinate", "write_lammpstrj")
                    end if
                    ierr = ERR_INVALID_FORMAT
                    close(dump)
                    return
                end if
                
                write(dump, *, iostat=ios) ! end the line
                if (ios /= 0) then
                    if (present(error)) then
                        call error%set(ERR_INVALID_FORMAT, "Failed to end atom data line", "write_lammpstrj")
                    end if
                    ierr = ERR_INVALID_FORMAT
                    close(dump)
                    return
                end if
            end do
        end do
        
        close(dump, iostat=ios)
        if (ios /= 0) then
            if (present(error)) then
                call error%set(ERR_INVALID_FORMAT, "Failed to close output file", "write_lammpstrj")
            end if
            ierr = ERR_INVALID_FORMAT
            return
        end if
    end function write_lammpstrj

    !> @brief LAMMPSのトラジェクトリファイルを読み込む
    !> @param[in] filename 入力ファイル名
    !> @param[out] coords 座標配列
    !> @param[out] box_bounds ボックスの境界
    !> @param[out] headers ヘッダーインデックス
    !> @param[out] error エラー情報（オプション）
    !> @return エラーコード（0: 成功, 非0: エラー）
    function read_lammpstrj(filename, coords, box_bounds, headers, error) result(ierr)
        implicit none
        character(len=*), intent(in) :: filename
        real, allocatable, intent(out) :: coords(:,:,:)
        double precision, allocatable, intent(out) :: box_bounds(:,:,:)
        type(AtomHeader_Index), intent(out) :: headers
        type(ErrorInfo), intent(out), optional :: error
        integer :: ierr
        
        ! local variables
        integer :: ios, unit, nframes, natoms, i, j, frame
        character(len=256) :: line
        character(len=:), allocatable :: header_parts(:)
        integer :: alloc_stat
        
        ! エラー情報の初期化
        if (present(error)) call error%clear()
        ierr = 0
        
        ! ファイルを開く
        open(newunit=unit, file=filename, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            if (present(error)) then
                call error%set(ERR_FILE_NOT_FOUND, "Failed to open file for reading: " // trim(filename), "read_lammpstrj")
            end if
            ierr = ERR_FILE_NOT_FOUND
            return
        end if
        
        ! まずフレーム数と原子数を数える
        nframes = 0
        natoms = 0
        
        do
            read(unit, '(A)', iostat=ios) line
            if (ios /= 0) exit
            
            if (index(line, "ITEM: TIMESTEP") > 0) then
                nframes = nframes + 1
            end if
            
            if (nframes == 1 .and. index(line, "ITEM: NUMBER OF ATOMS") > 0) then
                read(unit, *, iostat=ios) natoms
                if (ios /= 0) then
                    if (present(error)) then
                        call error%set(ERR_INVALID_FORMAT, "Failed to read number of atoms", "read_lammpstrj")
                    end if
                    ierr = ERR_INVALID_FORMAT
                    close(unit)
                    return
                end if
            end if
            
            if (nframes == 1 .and. index(line, "ITEM: ATOMS") > 0) then
                ! ヘッダーを解析
                header_parts = split_string(line)
                
                ! ヘッダーインデックスを設定
                headers = AtomHeader_Index()
                
                do i = 3, size(header_parts) ! 最初の2つは "ITEM:" と "ATOMS"
                    select case (trim(header_parts(i)))
                        case ("id")
                            headers%id = i - 2
                        case ("mol")
                            headers%mol = i - 2
                        case ("type")
                            headers%type = i - 2
                        case ("xu")
                            headers%xu = i - 2
                        case ("yu")
                            headers%yu = i - 2
                        case ("zu")
                            headers%zu = i - 2
                        case ("x")
                            headers%x = i - 2
                        case ("y")
                            headers%y = i - 2
                        case ("z")
                            headers%z = i - 2
                        case ("ix")
                            headers%ix = i - 2
                        case ("iy")
                            headers%iy = i - 2
                        case ("iz")
                            headers%iz = i - 2
                        case ("xs")
                            headers%xs = i - 2
                        case ("ys")
                            headers%ys = i - 2
                        case ("zs")
                            headers%zs = i - 2
                    end select
                end do
                
                ! 残りのフレームをスキップ
                exit
            end if
        end do
        
        ! ファイルを先頭に戻す
        rewind(unit)
        
        ! メモリ割り当て
        allocate(coords(3, natoms, nframes), box_bounds(3, 3, nframes), stat=alloc_stat)
        if (alloc_stat /= 0) then
            if (present(error)) then
                call error%set(ERR_MEMORY_ALLOCATION, "Failed to allocate memory for trajectory data", "read_lammpstrj")
            end if
            ierr = ERR_MEMORY_ALLOCATION
            close(unit)
            return
        end if
        
        ! データを読み込む
        frame = 0
        
        do
            read(unit, '(A)', iostat=ios) line
            if (ios /= 0) exit
            
            if (index(line, "ITEM: TIMESTEP") > 0) then
                frame = frame + 1
                
                ! タイムステップをスキップ
                read(unit, *, iostat=ios)
                if (ios /= 0) then
                    if (present(error)) then
                        call error%set(ERR_INVALID_FORMAT, "Failed to read timestep", "read_lammpstrj")
                    end if
                    ierr = ERR_INVALID_FORMAT
                    close(unit)
                    return
                end if
                
                ! 原子数をスキップ
                read(unit, '(A)', iostat=ios) line
                if (ios /= 0 .or. index(line, "ITEM: NUMBER OF ATOMS") == 0) then
                    if (present(error)) then
                        call error%set(ERR_INVALID_FORMAT, "Failed to read number of atoms header", "read_lammpstrj")
                    end if
                    ierr = ERR_INVALID_FORMAT
                    close(unit)
                    return
                end if
                
                read(unit, *, iostat=ios)
                if (ios /= 0) then
                    if (present(error)) then
                        call error%set(ERR_INVALID_FORMAT, "Failed to read number of atoms", "read_lammpstrj")
                    end if
                    ierr = ERR_INVALID_FORMAT
                    close(unit)
                    return
                end if
                
                ! ボックスの境界を読み込む
                read(unit, '(A)', iostat=ios) line
                if (ios /= 0 .or. index(line, "ITEM: BOX BOUNDS") == 0) then
                    if (present(error)) then
                        call error%set(ERR_INVALID_FORMAT, "Failed to read box bounds header", "read_lammpstrj")
                    end if
                    ierr = ERR_INVALID_FORMAT
                    close(unit)
                    return
                end if
                
                do i = 1, 3
                    read(unit, *, iostat=ios) (box_bounds(j, i, frame), j=1, 3)
                    if (ios /= 0) then
                        if (present(error)) then
                            call error%set(ERR_INVALID_FORMAT, "Failed to read box bounds", "read_lammpstrj")
                        end if
                        ierr = ERR_INVALID_FORMAT
                        close(unit)
                        return
                    end if
                end do
                
                ! 原子データのヘッダーをスキップ
                read(unit, '(A)', iostat=ios) line
                if (ios /= 0 .or. index(line, "ITEM: ATOMS") == 0) then
                    if (present(error)) then
                        call error%set(ERR_INVALID_FORMAT, "Failed to read atoms header", "read_lammpstrj")
                    end if
                    ierr = ERR_INVALID_FORMAT
                    close(unit)
                    return
                end if
                
                ! 原子データを読み込む
                do i = 1, natoms
                    read(unit, '(A)', iostat=ios) line
                    if (ios /= 0) then
                        if (present(error)) then
                            call error%set(ERR_INVALID_FORMAT, "Failed to read atom data", "read_lammpstrj")
                        end if
                        ierr = ERR_INVALID_FORMAT
                        close(unit)
                        return
                    end if
                    
                    ! 行を解析
                    header_parts = split_string(line)
                    
                    ! 座標を取得
                    if (headers%x /= 0) then
                        read(header_parts(headers%x), *, iostat=ios) coords(1, i, frame)
                    elseif (headers%xu /= 0) then
                        read(header_parts(headers%xu), *, iostat=ios) coords(1, i, frame)
                    elseif (headers%xs /= 0) then
                        read(header_parts(headers%xs), *, iostat=ios) coords(1, i, frame)
                    else
                        if (present(error)) then
                            call error%set(ERR_INVALID_FORMAT, "No x coordinate found in atom data", "read_lammpstrj")
                        end if
                        ierr = ERR_INVALID_FORMAT
                        close(unit)
                        return
                    end if
                    
                    if (headers%y /= 0) then
                        read(header_parts(headers%y), *, iostat=ios) coords(2, i, frame)
                    elseif (headers%yu /= 0) then
                        read(header_parts(headers%yu), *, iostat=ios) coords(2, i, frame)
                    elseif (headers%ys /= 0) then
                        read(header_parts(headers%ys), *, iostat=ios) coords(2, i, frame)
                    else
                        if (present(error)) then
                            call error%set(ERR_INVALID_FORMAT, "No y coordinate found in atom data", "read_lammpstrj")
                        end if
                        ierr = ERR_INVALID_FORMAT
                        close(unit)
                        return
                    end if
                    
                    if (headers%z /= 0) then
                        read(header_parts(headers%z), *, iostat=ios) coords(3, i, frame)
                    elseif (headers%zu /= 0) then
                        read(header_parts(headers%zu), *, iostat=ios) coords(3, i, frame)
                    elseif (headers%zs /= 0) then
                        read(header_parts(headers%zs), *, iostat=ios) coords(3, i, frame)
                    else
                        if (present(error)) then
                            call error%set(ERR_INVALID_FORMAT, "No z coordinate found in atom data", "read_lammpstrj")
                        end if
                        ierr = ERR_INVALID_FORMAT
                        close(unit)
                        return
                    end if
                end do
            end if
        end do
        
        close(unit)
    end function read_lammpstrj

end module io
