# cgpa.py 改善計画

このドキュメントは、CGPolyAnaプロジェクトのインターフェースであるcgpa.pyを改善するための計画と進捗状況をまとめたものです。

## 目標

- cgpa.pyをより柔軟なインターフェースに改善
- `cgpa.py msd input.nml`のように簡単に解析プログラムを呼び出せるようにする
- ユーザーが自作のコードをexamples以下に配置して拡張できるようにする
- サブコマンドをexamples/xxx/a.outのxxxから自動的に設定する
- 拡張方法を示すためのテンプレートを提供する

## 実装計画と進捗状況

### 1. cgpa.pyの機能拡張

- [x] 計画立案
- [x] サブコマンドの自動検出機能の実装
- [x] コマンドライン引数の処理改善
- [x] エラーハンドリングの強化
- [x] ヘルプメッセージの充実

### 2. テンプレートの作成

- [x] 計画立案
- [x] examples/templateディレクトリの作成
- [x] テンプレートMakefileの作成
- [x] 'Hello from CGPA!'を表示するサンプルコードの作成
- [x] テンプレート使用方法のドキュメント作成

### 3. ドキュメント整備

- [x] 計画立案
- [ ] 拡張方法のチュートリアル作成
- [ ] サンプルコードの解説
- [ ] READMEの更新

## 実装詳細

### cgpa.py改善案（実装済み）

```python
#!/usr/bin/env python3
import argparse
import subprocess
import os
import glob

def discover_commands():
    """
    examples ディレクトリ内のサブディレクトリを検索し、
    利用可能なコマンドのリストを返す
    """
    script_path = os.path.realpath(os.path.abspath(__file__))
    cgpa_root = os.path.abspath(os.path.join(os.path.dirname(script_path), os.pardir))
    examples_dir = os.path.join(cgpa_root, 'examples')
    
    commands = []
    for item in os.listdir(examples_dir):
        item_path = os.path.join(examples_dir, item)
        if os.path.isdir(item_path) and os.path.exists(os.path.join(item_path, 'a.out')):
            commands.append(item)
    
    return commands

def main():
    # 利用可能なコマンドを検出
    available_commands = discover_commands()
    
    # コマンドライン引数のパーサーを設定
    myDescription = getDescription(available_commands)
    parser = argparse.ArgumentParser(
            description=myDescription,
            formatter_class=argparse.RawTextHelpFormatter)
    
    parser.add_argument('analysis', choices=available_commands, 
                        help='実行する解析プログラムを指定')
    parser.add_argument('-i', '--input', required=True, 
                        help='Namelist を指定してください')
    parser.add_argument('-v', '--verbose', action='store_true',
                        help='詳細な出力を表示')
    
    args = parser.parse_args()

    # cgpa.py の親ディレクトリのパスを取得
    script_path = os.path.realpath(os.path.abspath(__file__))
    cgpa_root = os.path.abspath(os.path.join(os.path.dirname(script_path), os.pardir))

    # 実行する解析プログラムのパスを設定
    analysis_program = os.path.join(cgpa_root, 'examples', args.analysis, "a.out")

    # コマンドの存在を確認
    if not os.path.isfile(analysis_program):
        print(f'エラー: 解析プログラム {analysis_program} が見つかりません。')
        print(f'ビルドするには: cd {os.path.join(cgpa_root, "examples", args.analysis)} && make')
        exit(1)

    # 実行するコマンドを構築
    cmd = [analysis_program, args.input]

    # コマンドを実行
    try:
        if args.verbose:
            print(f'実行コマンド: {" ".join(cmd)}')
        
        subprocess.run(cmd, check=True)
        print(f'{args.analysis} の解析が完了しました。')
    except subprocess.CalledProcessError as e:
        print(f'エラーが発生しました: {e}')
        exit(1)

def getDescription(available_commands):
    description = """CGPA: Coarse-Grained Particle Analysis を用いて書かれた解析プログラムを実行するためのスクリプト
解析プログラムはexamples ディレクトリに格納されている。
実行する解析プログラムを指定するための引数 analysis は以下の選択肢から選択する。
"""
    
    # 利用可能なコマンドの説明を追加
    for cmd in available_commands:
        if cmd == "rdf":
            description += f"  {cmd}: Radial Distribution Function を計算するプログラム\n"
        elif cmd == "msd":
            description += f"  {cmd}: Mean Squared Displacement を計算するプログラム\n"
        elif cmd == "rg2":
            description += f"  {cmd}: Radius of Gyration を計算するプログラム\n"
        elif cmd == "template":
            description += f"  {cmd}: テンプレートプログラム（新しい解析プログラムの作成用）\n"
        else:
            description += f"  {cmd}: ユーザー定義の解析プログラム\n"
    
    description += """
引数には、解析プログラムに渡す Namelist ファイルを指定する必要がある。
実行例：
cgpa rdf -i rdf.namelist
"""
    return description

if __name__ == '__main__':
    main()
```

### テンプレートMakefile（実装済み）

```makefile
# テンプレートMakefile for CGPolyAna
# このファイルを自作の解析プログラム用にカスタマイズしてください

# メインプログラムのファイル名（拡張子なし）
TARGET = template

# 共通Makefileをインクルード
include ../../Makefile.common

# 追加のコンパイルフラグがあれば設定
# FCFLAGS += -your-flags-here

# 追加のリンクフラグがあれば設定
# LINKFLAGS += -your-link-flags-here
```

### テンプレートFortranコード（実装済み）

```fortran
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
```

## 使用方法

1. cgpa.pyを使って解析プログラムを実行する：
   ```bash
   ./src/cgpa.py rdf -i input.nml
   ```

2. 新しい解析プログラムを作成する：
   ```bash
   # テンプレートをコピー
   cp -r examples/template examples/your_analysis_name
   
   # ファイルを編集
   cd examples/your_analysis_name
   mv template.f90 your_analysis_name.f90
   # your_analysis_name.f90を編集
   
   # Makefileを更新
   sed -i 's/TARGET = template/TARGET = your_analysis_name/' Makefile
   
   # ビルド
   make
   
   # 実行
   ../../src/cgpa.py your_analysis_name -i your_input.nml
   ```

## 今後の課題

- 複数の入力ファイルをサポート
- 出力ファイル名の指定オプション
- 解析プログラムのバージョン管理
- 並列実行のサポート

## 参考リソース

- [argparse ドキュメント](https://docs.python.org/ja/3/library/argparse.html)
- [subprocess ドキュメント](https://docs.python.org/ja/3/library/subprocess.html)
- [Fortran プログラミングガイド](https://gcc.gnu.org/onlinedocs/gfortran/) 