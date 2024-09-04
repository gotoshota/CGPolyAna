module statistics
    implicit none

    !!! old version !!!
    !!! そのうち消す !!!
    type ProbDistFunction
        ! should be declear before call subroutine
        integer :: n_bins
        ! automatically computed
        double precision, allocatable :: y(:)
        double precision, allocatable :: x(:)
        double precision :: bin_width
    end type

    type StatValues
        double precision :: mean = 0.0d0
        double precision :: variance = 0.0d0
        integer(kind=8) :: num_samples = 0
        ! probability distribution function
        double precision, allocatable :: pdf(:)
        double precision, allocatable :: pdf_x(:)
        double precision :: pdf_bin_width
        integer :: pdf_num_bins
        double precision :: pdf_min_val
        double precision :: pdf_max_val

        contains
            procedure :: update
            procedure :: init
    end type

    interface mean_and_variance
        module procedure mean_and_variance_double
        module procedure mean_and_variance_int
    end interface

contains
    
    subroutine init(self, min_val, max_val, n_bins)
        implicit none
        class(StatValues), intent(inout) :: self
        double precision, intent(in) :: min_val, max_val
        integer, intent(in) :: n_bins

        self%pdf_min_value = min_val
        self%pdf_max_value = max_val
        self%pdf_num_bins = n_bins
        self%pdf_bin_width = (max_val - min_val) / dble(n_bins)

        allocate(self%pdf(n_bins))
        allocate(self%pdf_x(n_bins))

        self%pdf = 0.0d0
        self%pdf_x = [(min_val + (i - 1) * self%pdf_bin_width, i = 1, n_bins)]
    end subroutine

    subroutine update(self, new_value)
        implicit none
        class(StatValues), intent(inout) :: self
        double precision, intent(in) :: new_value

        double precision :: delta, delta2
        integer :: bin_idx

        ! Update mean
        self%num_samples = self%num_samples + 1
        delta = new_value - self%mean
        self%mean = self%mean + delta / dble(self%num_samples)

        ! Update variance
        delta2 = new_value - self%mean
        self%variance = ((self%num_samples - 1) * self%variance + delta * delta2) / dble(self%num_samples)

        ! Update PDF
        if (new_value .lt. self%pdf_x(1)) then
            self%pdf(1) = self%pdf(1) + 1
        elseif (new_value .ge. self%pdf_x(self%pdf_num_bins)) then
            self%pdf(self%pdf_num_bins) = self%pdf(self%pdf_num_bins) + 1
        else
            bin_idx = 1 + int((new_value - self%pdf_min_value) / self%pdf_bin_width)
            self%pdf(bin_idx) = self%pdf(bin_idx) + 1
        end if
    end subroutine

    subroutine normalize_pdf(self)
        implicit none
        class(StatValues), intent(inout) :: self
        double precision :: total_count

        ! Compute the total count in the PDF
        total_count = sum(self%pdf) * self%pdf_bin_width

        ! Normalize PDF so that the integral over the bins equals 1
        if (total_count > 0.0d0) then
            self%pdf = self%pdf / (total_count * self%pdf_bin_width)
        end if
    end subroutine

    !!! Old version !!!
    subroutine mean_and_variance_double(array, size_array, mean, variance)
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
    subroutine mean_and_variance_int(array, size_array, mean, variance)
        implicit none

        integer, intent(IN) :: size_array
        integer, intent(in) :: array(size_array)
        double precision, intent(out) :: mean, variance

        ! local variables
        integer :: i
        double precision :: mean_squared

        mean = 0.0d0
        mean_squared = 0.0d0
        do i = 1, size_array
            mean = mean + (dble(array(i)) - mean)/dble(i)
            mean_squared = mean_squared + (dble(array(i))*dble(array(i)) - mean_squared)/dble(i)
        end do

        variance = mean_squared - mean*mean
    end subroutine

    !!! ここまで !!!
    subroutine calc_prob_dist(array, size_array, pdf)
        implicit none

        integer, intent(in) :: size_array
        double precision, intent(in) :: array(size_array)
        type(ProbDistFunction), intent(inout) :: pdf

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
