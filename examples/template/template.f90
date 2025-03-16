!> @file template.f90
!> @brief テンプレート解析プログラム
!> @details このファイルは新しい解析プログラムを作成するためのテンプレートです。
!> 必要に応じて修正して使用してください。

program main
    !$ use omp_lib
    use lammpsio
    use global_types
    use statistics
    use io
    use coord_convert
    use physical_constants
    use error_handling

    implicit none

    type(lammpstrjReader) :: reader
    type(MDParams) :: params
    type(ErrorInfo) :: err
    character(len=256) :: nmlfilename
    integer :: i, num_frames, j
    integer :: frame
    character(len=256) :: outfilename = "output.txt"
    character(len=10) :: output_format = "txt"
    logical :: verbose = .false.
    ! 時間の計測
    real :: start, finish
    ! コマンドライン引数の解析
    integer :: arg_count, arg_idx
    character(len=256) :: arg

    ! 時間の計測開始
    call cpu_time(start)

    ! OpenMPスレッド数の表示
    !$ print *, "OMP_NUM_THREADS = ", OMP_GET_MAX_THREADS()

    ! コマンドライン引数の解析
    arg_count = command_argument_count()
    if (arg_count < 1) then
        print *, "使用方法: ./a.out input.nml [--output output.txt] [--format txt|hdf5] [--verbose]"
        stop
    end if

    ! 最初の引数はNamelistファイル
    call get_command_argument(1, nmlfilename)

    ! オプション引数の解析
    arg_idx = 2
    do while (arg_idx <= arg_count)
        call get_command_argument(arg_idx, arg)
        
        if (arg == "--output" .or. arg == "-o") then
            if (arg_idx + 1 <= arg_count) then
                arg_idx = arg_idx + 1
                call get_command_argument(arg_idx, outfilename)
            end if
        else if (arg == "--format" .or. arg == "-f") then
            if (arg_idx + 1 <= arg_count) then
                arg_idx = arg_idx + 1
                call get_command_argument(arg_idx, output_format)
            end if
        else if (arg == "--verbose" .or. arg == "-v") then
            verbose = .true.
        end if
        
        arg_idx = arg_idx + 1
    end do

    ! 詳細モードの場合、設定を表示
    if (verbose) then
        print *, "入力ファイル: ", trim(nmlfilename)
        print *, "出力ファイル: ", trim(outfilename)
        print *, "出力形式: ", trim(output_format)
    end if

    ! MDParamsの読み込み
    call read_MDParams(nmlfilename, params, err)
    if (err%code /= ERR_SUCCESS) then
        print *, "エラー: ", trim(get_error_message(err))
        stop
    end if

    ! LAMMPSトラジェクトリファイルを開く
    call reader%open(params%dumpfilenames(1), err)
    if (err%code /= ERR_SUCCESS) then
        print *, "エラー: ", trim(get_error_message(err))
        stop
    end if

    ! フレームの処理
    num_frames = 0
    do frame = 1, params%nframes
        ! フレームの読み込み
        call reader%read(err)
        if (err%code /= ERR_SUCCESS) then
            print *, "警告: フレーム読み込みエラー: ", trim(get_error_message(err))
            exit
        end if
        
        if (reader%end_of_file) exit
        
        num_frames = num_frames + 1
        
        ! 詳細モードの場合、進捗を表示
        if (verbose .and. mod(frame, 10) == 0) then
            print *, "フレーム ", frame, " を処理中..."
        end if
        
        ! ここに解析コードを追加
        ! 例: 座標の処理、統計量の計算など
        
    end do

    ! 結果の出力
    if (output_format == "txt") then
        ! テキスト形式での出力
        call write_results_txt(outfilename, err)
        if (err%code /= ERR_SUCCESS) then
            print *, "エラー: 結果の出力に失敗しました: ", trim(get_error_message(err))
        end if
    else if (output_format == "hdf5") then
        ! HDF5形式での出力（未実装）
        print *, "警告: HDF5形式の出力は現在サポートされていません。"
        print *, "テキスト形式で出力します。"
        call write_results_txt(outfilename, err)
        if (err%code /= ERR_SUCCESS) then
            print *, "エラー: 結果の出力に失敗しました: ", trim(get_error_message(err))
        end if
    else
        print *, "エラー: サポートされていない出力形式です: ", trim(output_format)
    end if

    ! LAMMPSトラジェクトリファイルを閉じる
    call reader%close()

    ! 時間の計測終了
    call cpu_time(finish)
    if (verbose) then
        print *, '処理時間: ', finish - start, ' 秒'
    end if

contains
    !> @brief テキスト形式で結果を出力する
    !> @param[in] filename 出力ファイル名
    !> @param[out] err エラー情報
    subroutine write_results_txt(filename, err)
        character(len=*), intent(in) :: filename
        type(ErrorInfo), intent(out) :: err
        integer :: unit
        integer :: ios
        
        ! エラー情報の初期化
        call clear_error(err)
        
        ! ファイルを開く
        open(newunit=unit, file=filename, status='replace', iostat=ios)
        if (ios /= 0) then
            call set_error(err, ERR_FILE_OPEN_FAILED, "ファイルを開けませんでした: " // trim(filename))
            return
        end if
        
        ! ヘッダーの出力
        write(unit, '(A)') "# テンプレート解析プログラムの出力"
        write(unit, '(A,I0)') "# 処理フレーム数: ", num_frames
        
        ! ここに結果の出力コードを追加
        ! 例: write(unit, *) x, y
        
        ! ファイルを閉じる
        close(unit)
    end subroutine write_results_txt

end program main 