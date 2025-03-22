!> @file math.f90
!> @brief 数学関数を提供するモジュール
!> @details ベクトル演算などの基本的な数学関数を提供します

!> @module math
!> @brief 数学関数モジュール
module math
    implicit none

    !> @interface norm
    !> @brief ベクトルのノルム（長さ）を計算するインターフェース
    interface norm
        module procedure norm_single
        module procedure norm_double
    end interface norm

    !> @interface cross_product
    !> @brief ベクトルの外積を計算するインターフェース
    interface cross_product
        module procedure cross_product_single
        module procedure cross_product_double
    end interface cross_product

contains
    !> @brief 倍精度ベクトルのノルムを計算する
    !> @param[in] vector 入力ベクトル
    !> @return ベクトルのノルム（長さ）
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

    !> @brief 単精度ベクトルのノルムを計算する
    !> @param[in] vector 入力ベクトル
    !> @return ベクトルのノルム（長さ）
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

    !> @brief 単精度ベクトルの外積を計算する
    !> @param[in] a 1つ目の入力ベクトル（3次元）
    !> @param[in] b 2つ目の入力ベクトル（3次元）
    !> @return 外積の結果ベクトル
    function cross_product_single(a, b) result(c)
        implicit none
        real, intent(IN) :: a(3), b(3)
        real :: c(3)
        
        c(1) = a(2)*b(3) - a(3)*b(2)
        c(2) = a(3)*b(1) - a(1)*b(3)
        c(3) = a(1)*b(2) - a(2)*b(1)
    end function

    !> @brief 倍精度ベクトルの外積を計算する
    !> @param[in] a 1つ目の入力ベクトル（3次元）
    !> @param[in] b 2つ目の入力ベクトル（3次元）
    !> @return 外積の結果ベクトル
    function cross_product_double(a, b) result(c)
        implicit none
        double precision, intent(IN) :: a(3), b(3)
        double precision :: c(3)
        
        c(1) = a(2)*b(3) - a(3)*b(2)
        c(2) = a(3)*b(1) - a(1)*b(3)
        c(3) = a(1)*b(2) - a(2)*b(1)
    end function
end module
