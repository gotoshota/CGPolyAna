module string_utils
    implicit none
contains
    function split_string(str) result(substrings)
        character(len=*), intent(in) :: str
        character(len=:), allocatable :: substrings(:)
        integer :: i, start, word_end, nWords, strLen
        logical :: inWord

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

        allocate(character(len=strLen) :: substrings(nWords))
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
end module string_utils
