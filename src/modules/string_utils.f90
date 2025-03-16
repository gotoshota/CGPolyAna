!> @file string_utils.f90
!> @brief 文字列操作のユーティリティを提供するモジュール
!> @details 文字列の分割や置換などの基本的な文字列操作機能を提供します
!> @author CGPolyAna開発チーム
!> @date 2025-03-16
!> @version 1.0
module string_utils
    use error_handling
    implicit none

contains
    !> @brief 文字列をスペースで区切って分割する
    !> @param[in] str 分割する文字列
    !> @param[out] error エラー情報（オプション）
    !> @return 分割された文字列の配列
    function split_string(str, error) result(substrings)
        character(len=*), intent(in) :: str
        type(ErrorInfo), intent(out), optional :: error
        character(len=:), allocatable :: substrings(:)
        integer :: i, start, word_end, nWords, strLen
        logical :: inWord
        integer :: alloc_stat

        ! エラー情報の初期化
        if (present(error)) call error%clear()

        ! 空文字列のチェック
        if (len_trim(str) == 0) then
            if (present(error)) then
                call error%set(ERR_INVALID_PARAMETER, "Empty string provided to split_string", "split_string")
            end if
            allocate(character(len=0) :: substrings(0))
            return
        end if

        strLen = len_trim(str)
        nWords = 0
        inWord = .false.
        start = 1

        ! 文字列を走査し、単語の数を数える
        do i = 1, strLen
            if (str(i:i) /= ' ' .and. .not. inWord) then
                inWord = .true.
                start = i
            elseif (str(i:i) == ' ' .and. inWord) then
                inWord = .false.
                nWords = nWords + 1
            end if
        end do
        if (inWord) nWords = nWords + 1  ! 最後の単語を数える

        ! 単語がない場合
        if (nWords == 0) then
            allocate(character(len=0) :: substrings(0))
            return
        end if

        ! 配列の割り当て
        allocate(character(len=strLen) :: substrings(nWords), stat=alloc_stat)
        if (alloc_stat /= 0) then
            if (present(error)) then
                call error%set(ERR_MEMORY_ALLOCATION, "Failed to allocate memory for substrings", "split_string")
            end if
            return
        end if

        nWords = 0
        inWord = .false.

        ! 実際に単語を抽出する
        do i = 1, strLen
            if (str(i:i) /= ' ' .and. .not. inWord) then
                inWord = .true.
                start = i
            elseif (str(i:i) == ' ' .and. inWord) then
                inWord = .false.
                word_end = i - 1
                nWords = nWords + 1
                substrings(nWords) = str(start:word_end)
            end if
        end do
        if (inWord) then
            nWords = nWords + 1
            substrings(nWords) = str(start:strLen)
        end if
    end function split_string

    !> @brief 文字列内の特定の部分文字列を別の文字列に置換する
    !> @param[in] string 元の文字列
    !> @param[in] old 置換対象の部分文字列
    !> @param[in] new 置換後の文字列
    !> @param[out] error エラー情報（オプション）
    !> @return 置換後の文字列
    function replace(string, old, new, error) result(new_string)
        implicit none
        character(len=*), intent(in) :: string, old, new
        type(ErrorInfo), intent(out), optional :: error
        character(len=len(string)) :: new_string
        integer :: pos, old_len, new_len

        ! エラー情報の初期化
        if (present(error)) call error%clear()

        ! 空文字列のチェック
        if (len_trim(string) == 0) then
            if (present(error)) then
                call error%set(ERR_INVALID_PARAMETER, "Empty string provided to replace", "replace")
            end if
            new_string = ""
            return
        end if

        ! 置換対象の文字列が空の場合
        if (len_trim(old) == 0) then
            if (present(error)) then
                call error%set(ERR_INVALID_PARAMETER, "Empty replacement target string", "replace")
            end if
            new_string = string
            return
        end if

        new_string = string
        old_len = len_trim(old)
        new_len = len_trim(new)
        
        ! 置換処理
        pos = index(new_string, old)
        do while (pos /= 0)
            ! 置換対象と置換後の文字列の長さが異なる場合の処理
            if (old_len /= new_len) then
                new_string = new_string(1:pos-1) // new // new_string(pos+old_len:)
            else
                new_string(pos:pos+old_len-1) = new
            end if
            pos = index(new_string, old)
        end do
    end function replace

    !> @brief 文字列を整数に変換する
    !> @param[in] str 変換する文字列
    !> @param[out] value 変換後の整数値
    !> @param[out] error エラー情報（オプション）
    !> @return 変換が成功したかどうか（.true.: 成功, .false.: 失敗）
    function str_to_int(str, value, error) result(success)
        implicit none
        character(len=*), intent(in) :: str
        integer, intent(out) :: value
        type(ErrorInfo), intent(out), optional :: error
        logical :: success
        integer :: ios

        ! エラー情報の初期化
        if (present(error)) call error%clear()
        success = .false.

        ! 文字列を整数に変換
        read(str, *, iostat=ios) value
        if (ios /= 0) then
            if (present(error)) then
                call error%set(ERR_INVALID_FORMAT, "Failed to convert string to integer: " // trim(str), "str_to_int")
            end if
            return
        end if

        success = .true.
    end function str_to_int

    !> @brief 文字列を実数に変換する
    !> @param[in] str 変換する文字列
    !> @param[out] value 変換後の実数値
    !> @param[out] error エラー情報（オプション）
    !> @return 変換が成功したかどうか（.true.: 成功, .false.: 失敗）
    function str_to_real(str, value, error) result(success)
        implicit none
        character(len=*), intent(in) :: str
        double precision, intent(out) :: value
        type(ErrorInfo), intent(out), optional :: error
        logical :: success
        integer :: ios

        ! エラー情報の初期化
        if (present(error)) call error%clear()
        success = .false.

        ! 文字列を実数に変換
        read(str, *, iostat=ios) value
        if (ios /= 0) then
            if (present(error)) then
                call error%set(ERR_INVALID_FORMAT, "Failed to convert string to real: " // trim(str), "str_to_real")
            end if
            return
        end if

        success = .true.
    end function str_to_real
end module string_utils
