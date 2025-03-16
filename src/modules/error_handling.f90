!> @file error_handling.f90
!> @brief エラー処理を行うためのモジュール
!> @details エラーコードの定義とエラー情報を格納する型を提供します
!> @author CGPolyAna開発チーム
!> @date 2025-03-16
!> @version 1.0
module error_handling
    implicit none

    !> @brief エラーコードの定義
    integer, parameter :: ERR_SUCCESS = 0                !< 成功
    integer, parameter :: ERR_UNKNOWN = 1                !< 不明なエラー
    integer, parameter :: ERR_FILE_NOT_FOUND = 2         !< ファイルが見つからない
    integer, parameter :: ERR_INVALID_FORMAT = 3         !< 無効なフォーマット
    integer, parameter :: ERR_INVALID_PARAMETER = 4      !< 無効なパラメータ
    integer, parameter :: ERR_MEMORY_ALLOCATION = 5      !< メモリ割り当てエラー
    integer, parameter :: ERR_DIVISION_BY_ZERO = 6       !< ゼロ除算
    integer, parameter :: ERR_INDEX_OUT_OF_BOUNDS = 7    !< インデックスが範囲外
    integer, parameter :: ERR_NOT_IMPLEMENTED = 8        !< 未実装の機能
    integer, parameter :: ERR_OPERATION_FAILED = 9       !< 操作が失敗
    integer, parameter :: ERR_INVALID_STATE = 10         !< 無効な状態

    !> @brief エラー情報を格納する型
    type ErrorInfo
        integer :: code = ERR_SUCCESS                !< エラーコード
        character(len=256) :: message = ""           !< エラーメッセージ
        character(len=64) :: source = ""             !< エラー発生源
        logical :: has_error = .false.               !< エラーが発生したかどうか
    contains
        procedure :: set => set_error                !< エラー情報を設定
        procedure :: clear => clear_error            !< エラー情報をクリア
        procedure :: print => print_error            !< エラー情報を出力
    end type ErrorInfo

contains
    !> @brief エラー情報を設定する
    !> @param[inout] self ErrorInfoオブジェクト
    !> @param[in] code エラーコード
    !> @param[in] message エラーメッセージ
    !> @param[in] source エラー発生源
    subroutine set_error(self, code, message, source)
        implicit none
        class(ErrorInfo), intent(inout) :: self
        integer, intent(in) :: code
        character(len=*), intent(in) :: message
        character(len=*), intent(in), optional :: source

        self%code = code
        self%message = message
        if (present(source)) then
            self%source = source
        else
            self%source = "unknown"
        end if
        self%has_error = .true.
    end subroutine set_error

    !> @brief エラー情報をクリアする
    !> @param[inout] self ErrorInfoオブジェクト
    subroutine clear_error(self)
        implicit none
        class(ErrorInfo), intent(inout) :: self

        self%code = ERR_SUCCESS
        self%message = ""
        self%source = ""
        self%has_error = .false.
    end subroutine clear_error

    !> @brief エラー情報を出力する
    !> @param[in] self ErrorInfoオブジェクト
    !> @param[in] unit 出力ユニット（オプション、デフォルトは標準エラー出力）
    subroutine print_error(self, unit)
        implicit none
        class(ErrorInfo), intent(in) :: self
        integer, intent(in), optional :: unit
        integer :: out_unit

        if (.not. self%has_error) return

        if (present(unit)) then
            out_unit = unit
        else
            out_unit = 0 ! 標準エラー出力
        end if

        write(out_unit, '(A)') "Error occurred:"
        write(out_unit, '(A,I0)') "  Code: ", self%code
        write(out_unit, '(A,A)') "  Message: ", trim(self%message)
        write(out_unit, '(A,A)') "  Source: ", trim(self%source)
    end subroutine print_error

    !> @brief エラーコードに対応するメッセージを取得する
    !> @param[in] code エラーコード
    !> @return エラーメッセージ
    function get_error_message(code) result(message)
        implicit none
        integer, intent(in) :: code
        character(len=64) :: message

        select case (code)
            case (ERR_SUCCESS)
                message = "Success"
            case (ERR_UNKNOWN)
                message = "Unknown error"
            case (ERR_FILE_NOT_FOUND)
                message = "File not found"
            case (ERR_INVALID_FORMAT)
                message = "Invalid format"
            case (ERR_INVALID_PARAMETER)
                message = "Invalid parameter"
            case (ERR_MEMORY_ALLOCATION)
                message = "Memory allocation error"
            case (ERR_DIVISION_BY_ZERO)
                message = "Division by zero"
            case (ERR_INDEX_OUT_OF_BOUNDS)
                message = "Index out of bounds"
            case (ERR_NOT_IMPLEMENTED)
                message = "Not implemented"
            case (ERR_OPERATION_FAILED)
                message = "Operation failed"
            case (ERR_INVALID_STATE)
                message = "Invalid state"
            case default
                message = "Unknown error code: " // trim(adjustl(int_to_str(code)))
        end select
    end function get_error_message

    !> @brief 整数を文字列に変換する
    !> @param[in] i 整数
    !> @return 文字列
    function int_to_str(i) result(str)
        implicit none
        integer, intent(in) :: i
        character(len=20) :: str
        
        write(str, '(I20)') i
        str = adjustl(str)
    end function int_to_str

end module error_handling
