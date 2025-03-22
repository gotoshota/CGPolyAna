# テンプレート実装計画

このドキュメントは、CGPolyAnaプロジェクトのテンプレート機能を実装するための計画と進捗状況をまとめたものです。

## 目標

- ユーザーが簡単に新しい解析プログラムを作成できるようにするためのテンプレートを提供
- 'Hello from CGPA!'を表示するシンプルなサンプルコードを作成
- テンプレートの使用方法を明確に文書化

## 実装計画と進捗状況

### 1. テンプレートディレクトリの作成

- [x] 計画立案
- [x] examples/templateディレクトリの作成
- [x] ディレクトリ構造の設計

### 2. テンプレートファイルの作成

- [x] 計画立案
- [x] template.f90の作成
- [x] Makefileの作成
- [x] README.mdの作成

### 3. テンプレートのテスト

- [x] 計画立案
- [x] テンプレートのビルドテスト
- [x] cgpa.pyからの実行テスト
- [x] 新規プログラム作成手順のテスト

## 実装詳細

### ディレクトリ構造

```
examples/
└── template/
    ├── Makefile
    ├── README.md
    └── template.f90
```

### template.f90

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

### Makefile

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

### README.md

```markdown
# CGPolyAna テンプレート

このディレクトリには、CGPolyAnaの新しい解析プログラムを作成するためのテンプレートが含まれています。

## 使用方法

1. このディレクトリを新しい名前でコピーします：
   ```bash
   cp -r examples/template examples/your_analysis_name
   ```

2. ディレクトリ内のファイルを編集します：
   - `template.f90` を `your_analysis_name.f90` にリネームし、コードを編集
   - `Makefile` の `TARGET` 変数を `your_analysis_name` に変更

3. プログラムをビルドします：
   ```bash
   cd examples/your_analysis_name
   make
   ```

4. cgpa.pyから実行します：
   ```bash
   ./src/cgpa.py your_analysis_name -i your_input.nml
   ```

## テンプレートの構造

- `template.f90`: メインプログラムのソースコード
- `Makefile`: ビルド設定ファイル

## カスタマイズのヒント

- `template.f90` の `display_usage()` サブルーチンを編集して、プログラムの使用方法を説明
- 必要なモジュールをインポート（`use` 文を追加）
- 入力パラメータの読み込み処理を追加
- 解析ロジックを実装
- 結果の出力処理を追加

## 注意事項

- プログラム名（ファイル名とMakefileのTARGET）はディレクトリ名と一致させることを推奨
- 出力ファイルは適切な名前とフォーマットで保存
- エラー処理を適切に実装
```

## 実装手順

1. examples/templateディレクトリを作成
2. template.f90ファイルを作成
3. Makefileを作成
4. README.mdを作成
5. テンプレートをビルドしてテスト
6. cgpa.pyからの実行をテスト
7. ドキュメントを更新

## 今後の課題

- より高度なテンプレートの提供（例：RDF計算、MSD計算など）
- テンプレートのカスタマイズオプションの拡充
- テンプレートジェネレーターの作成（対話式で新しい解析プログラムを生成） 