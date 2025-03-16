!> @file math.f90
!> @brief 基本的な数学関数を提供するモジュール
!> @details ベクトル演算や基本的な数学関数を提供します
!> @author CGPolyAna開発チーム
!> @date 2025-03-16
!> @version 1.0
module math
    use error_handling
    implicit none

    !> @brief ベクトルのノルムを計算するインターフェース
    interface norm
        module procedure norm_single
        module procedure norm_double
    end interface norm

    !> @brief ベクトルの外積を計算するインターフェース
    interface cross_product
        module procedure cross_product_single
        module procedure cross_product_double
    end interface cross_product

    !> @brief ベクトルの内積を計算するインターフェース
    interface dot_product
        module procedure dot_product_single
        module procedure dot_product_double
    end interface dot_product

    !> @brief 行列の行列式を計算するインターフェース
    interface determinant
        module procedure determinant_3x3_single
        module procedure determinant_3x3_double
    end interface determinant

contains
    !> @brief 倍精度ベクトルのノルムを計算する
    !> @param[in] vector ノルムを計算するベクトル
    !> @param[out] error エラー情報（オプション）
    !> @return ベクトルのノルム
    function norm_double(vector, error) result(res)
        implicit none
        double precision, intent(in) :: vector(:)
        type(ErrorInfo), intent(out), optional :: error
        double precision :: res

        ! local variables
        integer :: i, n
        
        ! エラー情報の初期化
        if (present(error)) call error%clear()
        
        ! ベクトルサイズのチェック
        n = size(vector)
        if (n == 0) then
            if (present(error)) then
                call error%set(ERR_INVALID_PARAMETER, "Empty vector provided to norm_double", "norm_double")
            end if
            res = 0.0d0
            return
        end if
        
        ! ノルムの計算
        res = 0.0d0
        do i = 1, n
            res = res + vector(i)*vector(i)
        end do
        res = sqrt(res)
    end function norm_double

    !> @brief 単精度ベクトルのノルムを計算する
    !> @param[in] vector ノルムを計算するベクトル
    !> @param[out] error エラー情報（オプション）
    !> @return ベクトルのノルム
    function norm_single(vector, error) result(res)
        implicit none
        real, intent(in) :: vector(:)
        type(ErrorInfo), intent(out), optional :: error
        real :: res

        ! local variables
        integer :: i, n
        
        ! エラー情報の初期化
        if (present(error)) call error%clear()
        
        ! ベクトルサイズのチェック
        n = size(vector)
        if (n == 0) then
            if (present(error)) then
                call error%set(ERR_INVALID_PARAMETER, "Empty vector provided to norm_single", "norm_single")
            end if
            res = 0.0
            return
        end if
        
        ! ノルムの計算
        res = 0.0
        do i = 1, n
            res = res + vector(i)*vector(i)
        end do
        res = sqrt(res)
    end function norm_single

    !> @brief 単精度ベクトルの外積を計算する
    !> @param[in] a 1つ目のベクトル（3次元）
    !> @param[in] b 2つ目のベクトル（3次元）
    !> @param[out] error エラー情報（オプション）
    !> @return 外積ベクトル
    function cross_product_single(a, b, error) result(c)
        implicit none
        real, intent(in) :: a(3), b(3)
        type(ErrorInfo), intent(out), optional :: error
        real :: c(3)
        
        ! エラー情報の初期化
        if (present(error)) call error%clear()
        
        ! 外積の計算
        c(1) = a(2)*b(3) - a(3)*b(2)
        c(2) = a(3)*b(1) - a(1)*b(3)
        c(3) = a(1)*b(2) - a(2)*b(1)
    end function cross_product_single

    !> @brief 倍精度ベクトルの外積を計算する
    !> @param[in] a 1つ目のベクトル（3次元）
    !> @param[in] b 2つ目のベクトル（3次元）
    !> @param[out] error エラー情報（オプション）
    !> @return 外積ベクトル
    function cross_product_double(a, b, error) result(c)
        implicit none
        double precision, intent(in) :: a(3), b(3)
        type(ErrorInfo), intent(out), optional :: error
        double precision :: c(3)
        
        ! エラー情報の初期化
        if (present(error)) call error%clear()
        
        ! 外積の計算
        c(1) = a(2)*b(3) - a(3)*b(2)
        c(2) = a(3)*b(1) - a(1)*b(3)
        c(3) = a(1)*b(2) - a(2)*b(1)
    end function cross_product_double

    !> @brief 単精度ベクトルの内積を計算する
    !> @param[in] a 1つ目のベクトル
    !> @param[in] b 2つ目のベクトル
    !> @param[out] error エラー情報（オプション）
    !> @return 内積値
    function dot_product_single(a, b, error) result(res)
        implicit none
        real, intent(in) :: a(:), b(:)
        type(ErrorInfo), intent(out), optional :: error
        real :: res
        
        ! local variables
        integer :: i, n
        
        ! エラー情報の初期化
        if (present(error)) call error%clear()
        
        ! ベクトルサイズのチェック
        n = size(a)
        if (n == 0) then
            if (present(error)) then
                call error%set(ERR_INVALID_PARAMETER, "Empty vector provided to dot_product_single", "dot_product_single")
            end if
            res = 0.0
            return
        end if
        
        if (size(b) /= n) then
            if (present(error)) then
                call error%set(ERR_INVALID_PARAMETER, "Vector sizes do not match in dot_product_single", "dot_product_single")
            end if
            res = 0.0
            return
        end if
        
        ! 内積の計算
        res = 0.0
        do i = 1, n
            res = res + a(i)*b(i)
        end do
    end function dot_product_single

    !> @brief 倍精度ベクトルの内積を計算する
    !> @param[in] a 1つ目のベクトル
    !> @param[in] b 2つ目のベクトル
    !> @param[out] error エラー情報（オプション）
    !> @return 内積値
    function dot_product_double(a, b, error) result(res)
        implicit none
        double precision, intent(in) :: a(:), b(:)
        type(ErrorInfo), intent(out), optional :: error
        double precision :: res
        
        ! local variables
        integer :: i, n
        
        ! エラー情報の初期化
        if (present(error)) call error%clear()
        
        ! ベクトルサイズのチェック
        n = size(a)
        if (n == 0) then
            if (present(error)) then
                call error%set(ERR_INVALID_PARAMETER, "Empty vector provided to dot_product_double", "dot_product_double")
            end if
            res = 0.0d0
            return
        end if
        
        if (size(b) /= n) then
            if (present(error)) then
                call error%set(ERR_INVALID_PARAMETER, "Vector sizes do not match in dot_product_double", "dot_product_double")
            end if
            res = 0.0d0
            return
        end if
        
        ! 内積の計算
        res = 0.0d0
        do i = 1, n
            res = res + a(i)*b(i)
        end do
    end function dot_product_double

    !> @brief 3x3単精度行列の行列式を計算する
    !> @param[in] matrix 行列式を計算する3x3行列
    !> @param[out] error エラー情報（オプション）
    !> @return 行列式の値
    function determinant_3x3_single(matrix, error) result(res)
        implicit none
        real, intent(in) :: matrix(3,3)
        type(ErrorInfo), intent(out), optional :: error
        real :: res
        
        ! エラー情報の初期化
        if (present(error)) call error%clear()
        
        ! 3x3行列の行列式の計算
        res = matrix(1,1) * (matrix(2,2)*matrix(3,3) - matrix(2,3)*matrix(3,2)) &
            - matrix(1,2) * (matrix(2,1)*matrix(3,3) - matrix(2,3)*matrix(3,1)) &
            + matrix(1,3) * (matrix(2,1)*matrix(3,2) - matrix(2,2)*matrix(3,1))
    end function determinant_3x3_single

    !> @brief 3x3倍精度行列の行列式を計算する
    !> @param[in] matrix 行列式を計算する3x3行列
    !> @param[out] error エラー情報（オプション）
    !> @return 行列式の値
    function determinant_3x3_double(matrix, error) result(res)
        implicit none
        double precision, intent(in) :: matrix(3,3)
        type(ErrorInfo), intent(out), optional :: error
        double precision :: res
        
        ! エラー情報の初期化
        if (present(error)) call error%clear()
        
        ! 3x3行列の行列式の計算
        res = matrix(1,1) * (matrix(2,2)*matrix(3,3) - matrix(2,3)*matrix(3,2)) &
            - matrix(1,2) * (matrix(2,1)*matrix(3,3) - matrix(2,3)*matrix(3,1)) &
            + matrix(1,3) * (matrix(2,1)*matrix(3,2) - matrix(2,2)*matrix(3,1))
    end function determinant_3x3_double
end module
