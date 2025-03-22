!> @file template.f90
!> @brief CGPolyAnaテンプレートプログラム
!> @details このファイルを自分の解析プログラムのベースとして使用してください
!> @author CGPolyAnaチーム
!> @date 2023年

!> @program template
!> @brief 「Hello from CGPA!」を表示するサンプルプログラム
program template
    implicit none
    
    !> @var character(len=100) :: arg
    !> @brief コマンドライン引数を格納する変数
    character(len=100) :: arg
    
    !> @var integer :: num_args
    !> @brief コマンドライン引数の数
    integer :: num_args
    
    ! コマンドライン引数の数を取得
    num_args = command_argument_count()
    
    ! 引数チェック
    if (num_args < 1) then
        call display_usage()
        stop
    end if
    
    ! 最初の引数を取得
    call get_command_argument(1, arg)
    
    ! ヘルプオプションのチェック
    if (arg == '-h' .or. arg == '--help') then
        call display_usage()
        stop
    end if
    
    ! メインメッセージを表示
    print *, "Hello from CGPA!"
    print *, "このテンプレートを自分の解析プログラムのベースとして使用してください。"
    print *, "入力ファイル: ", trim(arg)
    
contains
    !> @brief プログラムの使用方法を表示する
    !> @details コマンドライン引数が不正な場合や、ヘルプオプションが指定された場合に呼び出される
    subroutine display_usage()
        print *, ""
        print *, "使用方法: template [オプション] [引数]"
        print *, "オプション:"
        print *, "  -h, --help  : このヘルプメッセージを表示"
        print *, "引数:"
        print *, "  1番目の引数: パラメータファイルへのパス"
        print *, ""
    end subroutine display_usage
end program template 