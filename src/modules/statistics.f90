module statistics
    implicit none

    type ProbDistFunciton
        ! should be declear before call subroutine
        integer :: n_bins
        
        ! automatically computed
        double precision, allocatable :: y(:)
        double precision, allocatable :: x(:)
        double precision :: bin_width

    end type

contains
    subroutine mean_and_variance(array, size_array, mean, variance)
        implicit none

        integer, intent(IN) :: size_array
        double precision, intent(in) :: array(size_array)
        double precision, intent(out) :: mean, variance

        ! local variables
        integer :: i
        double precision :: mean_squared

        mean = 0.0d0
        mean_squared = 0.0d0
        do i = 1, size_array
            mean = mean + (array(i) - mean)/dble(i)
            mean_squared = mean_squared + (array(i)*array(i) - mean_squared)/dble(i)
        end do

        variance = mean_squared - mean*mean
    end subroutine

    subroutine calc_prob_dist(array, size_array, pdf)
        implicit none

        integer, intent(in) :: size_array
        double precision, intent(in) :: array(size_array)
        type(ProbDistFunciton), intent(inout) :: pdf

        ! local variables
        integer :: i
        double precision :: min_val, max_val

        ! 配列の最小値と最大値の取得
        min_val = minval(array)
        max_val = maxval(array)

        ! bin幅の計算
        pdf%bin_width = (max_val - min_val)/dble(pdf%n_bins)

        ! pdfを計算する
        pdf%y = 0.0d0
        do i = 1, size_array
            ! 配列要素が最小値より小さい場合
            if (array(i) .lt. min_val) then
                pdf%y(1) = pdf%y(1) + 1
            ! 配列要素が最大値より大きい場合
            elseif (array(i) .ge. max_val) then
                pdf%y(pdf%n_bins) = pdf%y(pdf%n_bins) + 1
            else
                ! 対応するbinを計算し、pdfを更新
                pdf%y(1 + int((array(i) - min_val)/pdf%bin_width)) = &
                pdf%y(1 + int((array(i) - min_val)/pdf%bin_width)) + 1
            end if
        end do
        pdf%y(:) = pdf%y(:)/dble(size_array)/pdf%bin_width

        ! binの値をpdfの2行目に格納
        do i = 1, pdf%n_bins
            pdf%x(i) = min_val + (i - 1)*pdf%bin_width
        end do
    end subroutine

end module statistics
