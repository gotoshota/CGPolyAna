module statistics
    implicit none

contains
    subroutine mean_and_variance(array, mean, variance)
        implicit none

        DOUBLE PRECISION, intent(in) :: array(:)
        DOUBLE PRECISION, intent(out) :: mean, variance

        ! local variables
        integer :: i, n
        DOUBLE PRECISION :: summation, summation_squared

        n = SIZE(array)

        summation = 0.0d0
        summation_squared = 0.0d0
        do i = 1, n
            summation           = summation + array(i)
            summation_squared   = summation_squared + array(i)*array(i)
        enddo

        mean        = summation / DBLE(n)
        variance    = summation_squared / DBLE(n-1)
    end subroutine



end module statistics
