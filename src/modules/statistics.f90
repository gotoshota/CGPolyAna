module statistics
    implicit none

    type ProbDistFunciton
        ! should be declear before call subroutine
        INTEGER                       :: n_bins
        
        ! automatically computed
        DOUBLE PRECISION, ALLOCATABLE :: y(:)
        DOUBLE PRECISION, ALLOCATABLE :: x(:)
        DOUBLE PRECISION              :: bin_width

    end type

contains
    subroutine mean_and_variance(array, size_array, mean, variance)
        implicit none

        INTEGER, INTENT(IN)           :: size_array
        DOUBLE PRECISION, intent(in)  :: array(size_array)
        DOUBLE PRECISION, intent(out) :: mean, variance

        ! local variables
        integer :: i
        DOUBLE PRECISION :: mean_squared

        mean = 0.0d0
        mean_squared = 0.0d0
        do i = 1, size_array
            mean = mean + (array(i) - mean) / dble(i)
            mean_squared = mean_squared + (array(i)*array(i) - mean_squared) / dble(i)
            if (MOD(i, 80*501) == 0) print *, i, mean, mean_squared
        enddo

        variance    = mean_squared - mean*mean
    end subroutine

    subroutine calc_prob_dist(array, size_array, pdf)
        implicit none

        INTEGER, INTENT(IN)                   :: size_array
        DOUBLE PRECISION, INTENT(IN)          :: array(size_array)
        TYPE(ProbDistFunciton), INTENT(INOUT) :: pdf

        ! local variables 
        INTEGER :: i
        DOUBLE PRECISION :: min_val, max_val

        ! 配列の最小値と最大値の取得
        min_val = MINVAL(array)
        max_val = MAXVAL(array)

        ! bin幅の計算
        pdf%bin_width = (max_val - min_val) / DBLE(pdf%n_bins)

        ! pdfを計算する
        pdf%y = 0.0d0
        do i = 1, size_array
            ! 配列要素が最小値より小さい場合
            if (array(i) < min_val) then
                pdf%y(1) = pdf%y(1) + 1
            ! 配列要素が最大値より大きい場合
            elseif (array(i) >= max_val) then
                pdf%y(pdf%n_bins) = pdf%y(pdf%n_bins) + 1
            else
                ! 対応するbinを計算し、pdfを更新
                pdf%y(1 + INT((array(i) - min_val) / pdf%bin_width)) = &
                pdf%y(1 + INT((array(i) - min_val) / pdf%bin_width)) + 1
            endif
        end do
        pdf%y(:) = pdf%y(:) / DBLE(size_array) / pdf%bin_width

        ! binの値をpdfの2行目に格納
        do i = 1, pdf%n_bins
            pdf%x(i) = min_val + (i - 1) * pdf%bin_width
        end do
    end subroutine 


end module statistics
