module math
    implicit none

    interface norm
        module procedure norm_single
        module procedure norm_double
    end interface norm

contains
    function norm_double(vector) result(res)
        implicit none

        DOUBLE PRECISION, INTENT(IN) :: vector(:)
        DOUBLE PRECISION             :: res

        ! local variables
        integer :: i, n
        DOUBLE PRECISION :: summation_squred
        
        n = SIZE(vector)
        summation_squred = 0.0d0
        do i = 1, n
            summation_squred = summation_squred + vector(i)*vector(i)
        enddo

        res = DSQRT(summation_squred)
    end function

    function norm_single(vector) result(res)
        implicit none

        REAL, INTENT(IN) :: vector(:)
        REAL             :: res

        ! local variables
        integer :: i, n
        REAL :: summation_squred
        
        n = SIZE(vector)
        summation_squred = 0.0d0
        do i = 1, n
            summation_squred = summation_squred + vector(i)*vector(i)
        enddo

        res = SQRT(summation_squred)
    end function

    function cross_product(a, b) result(c)
        implicit none
        REAL, INTENT(IN) :: a(3), b(3)
        REAL :: c(3)
        
        c(1) = a(2)*b(3) - a(3)*b(2)
        c(2) = a(3)*b(1) - a(1)*b(3)
        c(3) = a(1)*b(2) - a(2)*b(1)
    end function
end module


