# 新しい解析プログラムの作成ガイド

このガイドでは、CGPolyAnaに新しい解析プログラムを追加する方法を説明します。

## 1. テンプレートのコピー

まず、テンプレートディレクトリを新しい解析プログラム用にコピーします。

```bash
cd path/to/CGPolyAna
cp -r examples/template examples/your_analysis_name
```

## 2. ファイル名の変更

テンプレートファイルの名前を変更します。

```bash
cd examples/your_analysis_name
mv template.f90 your_analysis_name.f90
```

## 3. Makefileの編集

Makefileを編集して、ターゲット名を変更します。

```bash
# Makefileを開く
nano Makefile

# TARGETの値を変更
# TARGET = template
TARGET = your_analysis_name
```

## 4. プログラムの実装

`your_analysis_name.f90`を編集して、解析プログラムを実装します。テンプレートには基本的な構造が含まれているので、必要な部分を修正するだけです。

主に以下の部分を実装する必要があります：

1. フレームの処理部分（コメント「ここに解析コードを追加」の部分）
2. 結果の出力部分（`write_results_txt`サブルーチン内）

## 5. Pythonインターフェースの更新

新しい解析プログラムをPythonインターフェース（cgpa.py）に追加します。

```python
# src/cgpa.py を開き、ANALYSIS_PROGRAMSに新しいプログラムを追加

ANALYSIS_PROGRAMS = {
    # 既存のプログラム
    "rdf": { ... },
    "msd": { ... },
    "rg2": { ... },
    
    # 新しいプログラム
    "your_analysis_name": {
        "description": "あなたの解析プログラムの説明",
        "output_formats": ["txt", "hdf5"],
        "default_output": "your_analysis_name.txt"
    }
}
```

## 6. ビルドとテスト

新しい解析プログラムをビルドしてテストします。

```bash
# ビルド
cd path/to/CGPolyAna
make -C examples/your_analysis_name

# テスト
./src/cgpa.py your_analysis_name -i data/template.nml -v
```

## 7. ドキュメントの更新

READMEなどのドキュメントを更新して、新しい解析プログラムについて説明を追加します。

## 8. HDF5出力のサポート（オプション）

HDF5形式での出力をサポートする場合は、`write_results_hdf5`サブルーチンを実装します。HDF5ライブラリの使用方法については、既存のプログラムを参考にしてください。

## 注意事項

- エラー処理を適切に行ってください。`ErrorInfo`型を使用して、エラー情報を伝播させます。
- OpenMPを使用して並列処理を行う場合は、適切に変数のスコープを設定してください。
- 大きなデータ構造を扱う場合は、メモリの割り当てと解放に注意してください。 