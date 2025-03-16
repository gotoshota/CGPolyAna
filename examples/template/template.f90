!
! CGPolyAna テンプレートプログラム
! このファイルを自分の解析プログラムのベースとして使用してください
!
program template
    implicit none
    
    ! 変数宣言
    character(len=100) :: arg
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