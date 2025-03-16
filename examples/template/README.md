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