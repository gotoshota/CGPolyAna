module math
    implicit none

    interface norm
        module procedure norm_single
        module procedure norm_double
    end interface norm

    interface cross_product
        module procedure cross_product_single
        module procedure cross_product_double
    end interface cross_product

contains
    ! ===========================================
    ! for norm
    function norm_double(vector) result(res)
        implicit none

        double precision, intent(IN) :: vector(:)
        double precision :: res

        ! local variables
        integer :: i, n
        double precision :: summation_squred
        
        n = size(vector)
        summation_squred = 0.0d0
        do i = 1, n
            summation_squred = summation_squred + vector(i)*vector(i)
        end do

        res = DSQRT(summation_squred)
    end function

    function norm_single(vector) result(res)
        implicit none

        real, intent(IN) :: vector(:)
        real :: res

        ! local variables
        integer :: i, n
        real :: summation_squred
        
        n = size(vector)
        summation_squred = 0.0d0
        do i = 1, n
            summation_squred = summation_squred + vector(i)*vector(i)
        end do

        res = sqrt(summation_squred)
    end function
    ! ===========================================

    ! ===========================================
    ! for cross_product
    function cross_product_single(a, b) result(c)
        implicit none
        real, intent(IN) :: a(3), b(3)
        real :: c(3)
        
        c(1) = a(2)*b(3) - a(3)*b(2)
        c(2) = a(3)*b(1) - a(1)*b(3)
        c(3) = a(1)*b(2) - a(2)*b(1)
    end function
    function cross_product_double(a, b) result(c)
        implicit none
        double precision, intent(IN) :: a(3), b(3)
        double precision :: c(3)
        
        c(1) = a(2)*b(3) - a(3)*b(2)
        c(2) = a(3)*b(1) - a(1)*b(3)
        c(3) = a(1)*b(2) - a(2)*b(1)
    end function
    ! ===========================================
end module
