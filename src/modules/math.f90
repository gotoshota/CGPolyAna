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
        
        n = size(vector)
        res = 0.0d0
        do i = 1, n
            res = res + vector(i)*vector(i)
        end do
        res = sqrt(res)
    end function

    function norm_single(vector) result(res)
        implicit none

        real, intent(IN) :: vector(:)
        real :: res

        ! local variables
        integer :: i, n
        
        n = size(vector)
        res = 0.0
        do i = 1, n
            res = res + vector(i)*vector(i)
        end do
        res = sqrt(res)
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

    ! ===========================================
    ! topological calculation
    function gauss_linking_number(curve1, curve2) result(gln)
        implicit none
        real(4), intent(in) :: curve1(:, :), curve2(:, :)
        real(8) :: gln
        real(8) :: linking_number
        real(8) :: r1(3), r2(3), dr1(3), dr2(3), d_cross(3), diff(3)
        real(8) :: norm_diff
        real(8) :: eps = 1.0d-8 ! あんまり小さくすると誤差が大きくなる
        integer :: i, j
        integer :: n_points

        linking_number = 0.0d0
        n_points = size(curve1, 2)

        ! Gauss linking number の計算
        do i = 1, n_points
            r1 = dble(curve1(:,i))
            do j = 1, n_points
                r2 = dble(curve2(:,j))

                ! r1 - r2 の計算
                diff = r1 - r2
                norm_diff = sqrt(sum(diff**2)) + eps

                ! dr1 と dr2 (次の点への差分ベクトル)
                ! dr1 と dr2 の計算（iがn_pointsを超えないようにする）
                dr1 = dble(curve1(:,mod(i, n_points) + 1)) - r1
                dr2 = dble(curve2(:,mod(j, n_points) + 1)) - r2

                ! 外積を計算
                d_cross = cross_product(dr1, dr2)

                ! Gauss linking number に寄与する部分を加算
                ! ここでnorm_diff が小さすぎると誤差が大きくなるのでepsを足している
                linking_number = linking_number + dot_product(diff, d_cross) / (norm_diff**3.0d0)
            end do
        end do

        linking_number = linking_number / (4.0d0 * acos(-1.0d0))  ! 4πで割る
    end function
    ! ===========================================

end module
