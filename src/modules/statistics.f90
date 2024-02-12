module statistics
    implicit none

contains
    subroutine mean_and_variance(array, mean, variance)
        implicit none

        DOUBLE PRECISION, intent(in) :: array(:)
        DOUBLE PRECISION, intent(out) :: mean, variance

        ! local variables
        integer :: i, n
        DOUBLE PRECISION :: mean_squared

        n = SIZE(array)

        mean = 0.0d0
        mean_squared = 0.0d0
        do i = 1, n
            mean = mean + (array(i) - mean) / dble(i)
            mean_squared = mean_squared + (array(i)*array(i) - mean_squared) / dble(i)
        enddo

        variance    = mean_squared - mean*mean
    end subroutine

    subroutine calc_prob_dist(array, pdf)
        implicit none

        DOUBLE PRECISION, INTENT(IN) :: array(:)
        DOUBLE PRECISION, intent(out) :: pdf(:,:)

        ! local variables 
        INTEGER :: i, n, n_bins
        DOUBLE PRECISION :: min_val, max_val, bin_width

        n = SIZE(array)
        n_bins = SIZE(pdf, DIM=2)

        ! 配列の最小値と最大値の取得
        min_val = MINVAL(array)
        max_val = MAXVAL(array)

        ! bin幅の計算
        bin_width = (max_val - min_val) / DBLE(n_bins)

        ! pdfを計算する
        pdf = 0.0d0
        do i = 1, n
            ! 配列要素が最小値より小さい場合
            if (array(i) < min_val) then
                pdf(2, 1) = pdf(2, 1) + 1
            ! 配列要素が最大値より大きい場合
            elseif (array(i) >= max_val) then
                pdf(2, n_bins) = pdf(2, n_bins) + 1
            else
                ! 対応するbinを計算し、pdfを更新
                pdf(2, 1 + INT((array(i) - min_val) / bin_width)) = &
                    pdf(2, 1 + INT((array(i) - min_val) / bin_width)) + 1
            endif
        end do
        pdf(2,:) = pdf(2,:) / n

        ! binの値をpdfの2行目に格納
        do i = 1, n_bins
            pdf(1, i) = min_val + (i - 1) * bin_width
        end do
    end subroutine 


end module statistics
