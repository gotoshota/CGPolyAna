!> @file statistics.f90
!> @brief 統計計算を行うためのモジュール
!> @details 平均、分散、確率分布関数などの統計計算機能を提供します
!> @author CGPolyAna開発チーム
!> @date 2025-03-16
!> @version 1.0
module statistics
    use error_handling
    implicit none

    !> @brief 確率分布関数を格納する型
    type ProbDistFunction
        integer :: n_bins                        !< ビンの数
        double precision, allocatable :: y(:)    !< 確率密度
        double precision, allocatable :: x(:)    !< ビンの中心値
        double precision :: bin_width            !< ビンの幅
        double precision :: min_val              !< 最小値
        double precision :: max_val              !< 最大値
        type(ErrorInfo) :: error                 !< エラー情報
    contains
        procedure :: init => init_pdf            !< 初期化
        procedure :: update => update_pdf        !< 更新
        procedure :: normalize => normalize_pdf  !< 正規化
    end type ProbDistFunction

    !> @brief 統計値を格納する型
    type StatValues
        double precision :: mean = 0.0d0         !< 平均値
        double precision :: variance = 0.0d0     !< 分散
        integer(kind=8) :: num_samples = 0       !< サンプル数
        ! probability distribution function
        double precision, allocatable :: pdf(:)  !< 確率密度関数
        double precision, allocatable :: pdf_x(:) !< ビンの中心値
        double precision :: pdf_bin_width        !< ビンの幅
        integer :: pdf_num_bins                  !< ビンの数
        double precision :: pdf_min_val          !< 最小値
        double precision :: pdf_max_val          !< 最大値
        type(ErrorInfo) :: error                 !< エラー情報
    contains
        procedure :: init                        !< 初期化
        procedure :: update                      !< 更新
        procedure :: normalize => normalize_stat !< 正規化
    end type StatValues

    !> @brief 平均と分散を計算するインターフェース
    interface mean_and_variance
        module procedure mean_and_variance_double
        module procedure mean_and_variance_int
    end interface mean_and_variance

contains
    
    !> @brief StatValues型の初期化
    !> @param[inout] self StatValuesオブジェクト
    !> @param[in] min_val 最小値
    !> @param[in] max_val 最大値
    !> @param[in] n_bins ビンの数
    !> @return エラーコード（0: 成功, 非0: エラー）
    function init(self, min_val, max_val, n_bins) result(ierr)
        implicit none
        class(StatValues), intent(inout) :: self
        double precision, intent(in) :: min_val, max_val
        integer, intent(in) :: n_bins
        integer :: ierr
        integer :: i, alloc_stat

        ! エラー情報の初期化
        call self%error%clear()
        ierr = 0

        ! パラメータの検証
        if (min_val >= max_val) then
            call self%error%set(ERR_INVALID_PARAMETER, "min_val must be less than max_val", "StatValues%init")
            ierr = ERR_INVALID_PARAMETER
            return
        end if

        if (n_bins <= 0) then
            call self%error%set(ERR_INVALID_PARAMETER, "n_bins must be positive", "StatValues%init")
            ierr = ERR_INVALID_PARAMETER
            return
        end if

        ! 初期化
        self%mean = 0.0d0
        self%variance = 0.0d0
        self%num_samples = 0
        self%pdf_min_val = min_val
        self%pdf_max_val = max_val
        self%pdf_num_bins = n_bins
        self%pdf_bin_width = (max_val - min_val) / dble(n_bins)

        ! メモリ割り当て
        if (allocated(self%pdf)) deallocate(self%pdf)
        if (allocated(self%pdf_x)) deallocate(self%pdf_x)
        
        allocate(self%pdf(n_bins), self%pdf_x(n_bins), stat=alloc_stat)
        if (alloc_stat /= 0) then
            call self%error%set(ERR_MEMORY_ALLOCATION, "Failed to allocate memory for PDF", "StatValues%init")
            ierr = ERR_MEMORY_ALLOCATION
            return
        end if

        self%pdf = 0.0d0
        self%pdf_x = [(min_val + (i - 0.5) * self%pdf_bin_width, i = 1, n_bins)]
    end function init

    !> @brief StatValues型の更新
    !> @param[inout] self StatValuesオブジェクト
    !> @param[in] new_value 新しい値
    !> @return エラーコード（0: 成功, 非0: エラー）
    function update(self, new_value) result(ierr)
        implicit none
        class(StatValues), intent(inout) :: self
        double precision, intent(in) :: new_value
        integer :: ierr

        double precision :: delta, delta2
        integer :: bin_idx

        ! エラー情報の初期化
        call self%error%clear()
        ierr = 0

        ! PDFが初期化されているか確認
        if (.not. allocated(self%pdf) .or. .not. allocated(self%pdf_x)) then
            call self%error%set(ERR_INVALID_PARAMETER, "PDF not initialized", "StatValues%update")
            ierr = ERR_INVALID_PARAMETER
            return
        end if

        ! Update mean
        self%num_samples = self%num_samples + 1
        delta = new_value - self%mean
        self%mean = self%mean + delta / dble(self%num_samples)

        ! Update variance
        delta2 = new_value - self%mean
        self%variance = ((self%num_samples - 1) * self%variance + delta * delta2) / dble(self%num_samples)

        ! Update PDF
        if (new_value < self%pdf_min_val) then
            self%pdf(1) = self%pdf(1) + 1
        elseif (new_value >= self%pdf_max_val) then
            self%pdf(self%pdf_num_bins) = self%pdf(self%pdf_num_bins) + 1
        else
            bin_idx = 1 + int((new_value - self%pdf_min_val) / self%pdf_bin_width)
            self%pdf(bin_idx) = self%pdf(bin_idx) + 1
        end if
    end function update

    !> @brief StatValues型のPDFを正規化
    !> @param[inout] self StatValuesオブジェクト
    !> @return エラーコード（0: 成功, 非0: エラー）
    function normalize_stat(self) result(ierr)
        implicit none
        class(StatValues), intent(inout) :: self
        integer :: ierr
        double precision :: total_count

        ! エラー情報の初期化
        call self%error%clear()
        ierr = 0

        ! PDFが初期化されているか確認
        if (.not. allocated(self%pdf)) then
            call self%error%set(ERR_INVALID_PARAMETER, "PDF not initialized", "StatValues%normalize")
            ierr = ERR_INVALID_PARAMETER
            return
        end if

        ! Compute the total count in the PDF
        total_count = sum(self%pdf)

        ! Normalize PDF so that the integral over the bins equals 1
        if (total_count > 0.0d0) then
            self%pdf = self%pdf / (total_count * self%pdf_bin_width)
        end if
    end function normalize_stat

    !> @brief ProbDistFunction型の初期化
    !> @param[inout] self ProbDistFunctionオブジェクト
    !> @param[in] min_val 最小値
    !> @param[in] max_val 最大値
    !> @param[in] n_bins ビンの数
    !> @return エラーコード（0: 成功, 非0: エラー）
    function init_pdf(self, min_val, max_val, n_bins) result(ierr)
        implicit none
        class(ProbDistFunction), intent(inout) :: self
        double precision, intent(in) :: min_val, max_val
        integer, intent(in) :: n_bins
        integer :: ierr
        integer :: i, alloc_stat

        ! エラー情報の初期化
        call self%error%clear()
        ierr = 0

        ! パラメータの検証
        if (min_val >= max_val) then
            call self%error%set(ERR_INVALID_PARAMETER, "min_val must be less than max_val", "ProbDistFunction%init")
            ierr = ERR_INVALID_PARAMETER
            return
        end if

        if (n_bins <= 0) then
            call self%error%set(ERR_INVALID_PARAMETER, "n_bins must be positive", "ProbDistFunction%init")
            ierr = ERR_INVALID_PARAMETER
            return
        end if

        ! 初期化
        self%n_bins = n_bins
        self%min_val = min_val
        self%max_val = max_val
        self%bin_width = (max_val - min_val) / dble(n_bins)

        ! メモリ割り当て
        if (allocated(self%y)) deallocate(self%y)
        if (allocated(self%x)) deallocate(self%x)
        
        allocate(self%y(n_bins), self%x(n_bins), stat=alloc_stat)
        if (alloc_stat /= 0) then
            call self%error%set(ERR_MEMORY_ALLOCATION, "Failed to allocate memory for PDF", "ProbDistFunction%init")
            ierr = ERR_MEMORY_ALLOCATION
            return
        end if

        self%y = 0.0d0
        self%x = [(min_val + (i - 0.5) * self%bin_width, i = 1, n_bins)]
    end function init_pdf

    !> @brief ProbDistFunction型の更新
    !> @param[inout] self ProbDistFunctionオブジェクト
    !> @param[in] new_value 新しい値
    !> @return エラーコード（0: 成功, 非0: エラー）
    function update_pdf(self, new_value) result(ierr)
        implicit none
        class(ProbDistFunction), intent(inout) :: self
        double precision, intent(in) :: new_value
        integer :: ierr
        integer :: bin_idx

        ! エラー情報の初期化
        call self%error%clear()
        ierr = 0

        ! PDFが初期化されているか確認
        if (.not. allocated(self%y) .or. .not. allocated(self%x)) then
            call self%error%set(ERR_INVALID_PARAMETER, "PDF not initialized", "ProbDistFunction%update")
            ierr = ERR_INVALID_PARAMETER
            return
        end if

        ! Update PDF
        if (new_value < self%min_val) then
            self%y(1) = self%y(1) + 1
        elseif (new_value >= self%max_val) then
            self%y(self%n_bins) = self%y(self%n_bins) + 1
        else
            bin_idx = 1 + int((new_value - self%min_val) / self%bin_width)
            self%y(bin_idx) = self%y(bin_idx) + 1
        end if
    end function update_pdf

    !> @brief ProbDistFunction型のPDFを正規化
    !> @param[inout] self ProbDistFunctionオブジェクト
    !> @return エラーコード（0: 成功, 非0: エラー）
    function normalize_pdf(self) result(ierr)
        implicit none
        class(ProbDistFunction), intent(inout) :: self
        integer :: ierr
        double precision :: total_count

        ! エラー情報の初期化
        call self%error%clear()
        ierr = 0

        ! PDFが初期化されているか確認
        if (.not. allocated(self%y)) then
            call self%error%set(ERR_INVALID_PARAMETER, "PDF not initialized", "ProbDistFunction%normalize")
            ierr = ERR_INVALID_PARAMETER
            return
        end if

        ! Compute the total count in the PDF
        total_count = sum(self%y)

        ! Normalize PDF so that the integral over the bins equals 1
        if (total_count > 0.0d0) then
            self%y = self%y / (total_count * self%bin_width)
        end if
    end function normalize_pdf

    !> @brief 倍精度配列の平均と分散を計算する
    !> @param[in] array 配列
    !> @param[in] size_array 配列のサイズ
    !> @param[out] mean 平均値
    !> @param[out] variance 分散
    !> @param[out] error エラー情報（オプション）
    !> @return エラーコード（0: 成功, 非0: エラー）
    function mean_and_variance_double(array, size_array, mean, variance, error) result(ierr)
        implicit none
        integer, intent(in) :: size_array
        double precision, intent(in) :: array(size_array)
        double precision, intent(out) :: mean, variance
        type(ErrorInfo), intent(out), optional :: error
        integer :: ierr

        ! local variables
        integer :: i
        double precision :: mean_squared

        ! エラー情報の初期化
        if (present(error)) call error%clear()
        ierr = 0

        ! 配列サイズのチェック
        if (size_array <= 0) then
            if (present(error)) then
                call error%set(ERR_INVALID_PARAMETER, "Array size must be positive", "mean_and_variance_double")
            end if
            ierr = ERR_INVALID_PARAMETER
            mean = 0.0d0
            variance = 0.0d0
            return
        end if

        ! 平均と分散の計算
        mean = 0.0d0
        mean_squared = 0.0d0
        do i = 1, size_array
            mean = mean + (array(i) - mean)/dble(i)
            mean_squared = mean_squared + (array(i)*array(i) - mean_squared)/dble(i)
        end do

        variance = mean_squared - mean*mean
    end function mean_and_variance_double

    !> @brief 整数配列の平均と分散を計算する
    !> @param[in] array 配列
    !> @param[in] size_array 配列のサイズ
    !> @param[out] mean 平均値
    !> @param[out] variance 分散
    !> @param[out] error エラー情報（オプション）
    !> @return エラーコード（0: 成功, 非0: エラー）
    function mean_and_variance_int(array, size_array, mean, variance, error) result(ierr)
        implicit none
        integer, intent(in) :: size_array
        integer, intent(in) :: array(size_array)
        double precision, intent(out) :: mean, variance
        type(ErrorInfo), intent(out), optional :: error
        integer :: ierr

        ! local variables
        integer :: i
        double precision :: mean_squared

        ! エラー情報の初期化
        if (present(error)) call error%clear()
        ierr = 0

        ! 配列サイズのチェック
        if (size_array <= 0) then
            if (present(error)) then
                call error%set(ERR_INVALID_PARAMETER, "Array size must be positive", "mean_and_variance_int")
            end if
            ierr = ERR_INVALID_PARAMETER
            mean = 0.0d0
            variance = 0.0d0
            return
        end if

        ! 平均と分散の計算
        mean = 0.0d0
        mean_squared = 0.0d0
        do i = 1, size_array
            mean = mean + (dble(array(i)) - mean)/dble(i)
            mean_squared = mean_squared + (dble(array(i))*dble(array(i)) - mean_squared)/dble(i)
        end do

        variance = mean_squared - mean*mean
    end function mean_and_variance_int

    !> @brief 配列の確率分布関数を計算する
    !> @param[in] array 配列
    !> @param[in] size_array 配列のサイズ
    !> @param[inout] pdf 確率分布関数
    !> @return エラーコード（0: 成功, 非0: エラー）
    function calc_prob_dist(array, size_array, pdf) result(ierr)
        implicit none
        integer, intent(in) :: size_array
        double precision, intent(in) :: array(size_array)
        type(ProbDistFunction), intent(inout) :: pdf
        integer :: ierr

        ! local variables
        integer :: i
        double precision :: min_val, max_val

        ! エラー情報の初期化
        call pdf%error%clear()
        ierr = 0

        ! 配列サイズのチェック
        if (size_array <= 0) then
            call pdf%error%set(ERR_INVALID_PARAMETER, "Array size must be positive", "calc_prob_dist")
            ierr = ERR_INVALID_PARAMETER
            return
        end if

        ! PDFが初期化されているか確認
        if (.not. allocated(pdf%y) .or. .not. allocated(pdf%x)) then
            ! 配列の最小値と最大値の取得
            min_val = minval(array)
            max_val = maxval(array)
            
            ! PDFの初期化
            ierr = pdf%init(min_val, max_val, pdf%n_bins)
            if (ierr /= 0) return
        end if

        ! pdfを計算する
        pdf%y = 0.0d0
        do i = 1, size_array
            ! 配列要素が最小値より小さい場合
            if (array(i) < pdf%min_val) then
                pdf%y(1) = pdf%y(1) + 1
            ! 配列要素が最大値より大きい場合
            elseif (array(i) >= pdf%max_val) then
                pdf%y(pdf%n_bins) = pdf%y(pdf%n_bins) + 1
            else
                ! 対応するbinを計算し、pdfを更新
                pdf%y(1 + int((array(i) - pdf%min_val)/pdf%bin_width)) = &
                pdf%y(1 + int((array(i) - pdf%min_val)/pdf%bin_width)) + 1
            end if
        end do
        
        ! 正規化
        pdf%y(:) = pdf%y(:)/dble(size_array)/pdf%bin_width
    end function calc_prob_dist

    !> @brief 配列の中央値を計算する
    !> @param[in] array 配列
    !> @param[in] size_array 配列のサイズ
    !> @param[out] median 中央値
    !> @param[out] error エラー情報（オプション）
    !> @return エラーコード（0: 成功, 非0: エラー）
    function median(array, size_array, median_val, error) result(ierr)
        implicit none
        integer, intent(in) :: size_array
        double precision, intent(in) :: array(size_array)
        double precision, intent(out) :: median_val
        type(ErrorInfo), intent(out), optional :: error
        integer :: ierr

        ! local variables
        double precision, allocatable :: sorted_array(:)
        integer :: alloc_stat

        ! エラー情報の初期化
        if (present(error)) call error%clear()
        ierr = 0

        ! 配列サイズのチェック
        if (size_array <= 0) then
            if (present(error)) then
                call error%set(ERR_INVALID_PARAMETER, "Array size must be positive", "median")
            end if
            ierr = ERR_INVALID_PARAMETER
            median_val = 0.0d0
            return
        end if

        ! 配列のコピーと並べ替え
        allocate(sorted_array(size_array), stat=alloc_stat)
        if (alloc_stat /= 0) then
            if (present(error)) then
                call error%set(ERR_MEMORY_ALLOCATION, "Failed to allocate memory for sorted array", "median")
            end if
            ierr = ERR_MEMORY_ALLOCATION
            median_val = 0.0d0
            return
        end if

        sorted_array = array
        call quick_sort(sorted_array, size_array)

        ! 中央値の計算
        if (mod(size_array, 2) == 0) then
            ! 偶数の場合は中央の2つの値の平均
            median_val = (sorted_array(size_array/2) + sorted_array(size_array/2 + 1)) / 2.0d0
        else
            ! 奇数の場合は中央の値
            median_val = sorted_array(size_array/2 + 1)
        end if

        deallocate(sorted_array)
    end function median

    !> @brief クイックソートアルゴリズムによる配列の並べ替え
    !> @param[inout] array 並べ替える配列
    !> @param[in] n 配列のサイズ
    subroutine quick_sort(array, n)
        implicit none
        integer, intent(in) :: n
        double precision, intent(inout) :: array(n)
        
        call quick_sort_recursive(array, 1, n)
    end subroutine quick_sort

    !> @brief クイックソートの再帰的実装
    !> @param[inout] array 並べ替える配列
    !> @param[in] left 左端のインデックス
    !> @param[in] right 右端のインデックス
    recursive subroutine quick_sort_recursive(array, left, right)
        implicit none
        double precision, intent(inout) :: array(:)
        integer, intent(in) :: left, right
        
        integer :: i, j
        double precision :: pivot, temp
        
        if (left >= right) return
        
        ! ピボットの選択（中央の要素）
        pivot = array((left + right) / 2)
        
        ! 分割
        i = left
        j = right
        do
            do while (array(i) < pivot)
                i = i + 1
            end do
            
            do while (array(j) > pivot)
                j = j - 1
            end do
            
            if (i >= j) exit
            
            ! 要素の交換
            temp = array(i)
            array(i) = array(j)
            array(j) = temp
            
            i = i + 1
            j = j - 1
        end do
        
        ! 再帰的に並べ替え
        call quick_sort_recursive(array, left, j)
        call quick_sort_recursive(array, j + 1, right)
    end subroutine quick_sort_recursive

end module statistics
