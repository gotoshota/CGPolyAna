# CGPolyAna - 粗視化高分子解析ツール

CGPolyAnaは、粗視化高分子（ビーズ・スプリングモデル）の分子動力学シミュレーションの軌跡を解析するためのFortranモジュールとPythonインターフェースを提供します。

現在、LAMMPSのテキスト形式のトラジェクトリファイル（.lammpstrj）をサポートしています。

## 機能

- **基本解析機能**:
  - 動径分布関数（RDF）
  - 平均二乗変位（MSD）
  - 回転半径（Rg2）

- **モジュール**:
  - エラー処理（error_handling）
  - グローバル型定義（global_types）
  - 文字列操作（string_utils）
  - 数学関数（math）
  - 物理定数（physical_constants）
  - 統計処理（statistics）
  - 座標変換（coord_convert）
  - 入出力（io）

## ビルド方法

### 全てのプログラムをビルド

```bash
cd path/to/CGPolyAna
make
```

### 特定のプログラムをビルド

```bash
cd path/to/CGPolyAna
make -C examples/rdf  # RDFプログラムをビルド
make -C examples/msd  # MSDプログラムをビルド
make -C examples/rg2  # Rg2プログラムをビルド
```

### クリーンアップ

```bash
cd path/to/CGPolyAna
make clean      # 一時ファイルを削除
make clean-all  # すべてのビルド成果物を削除
```

## 使用方法

### Pythonインターフェース（cgpa.py）

CGPolyAnaは、Pythonインターフェース（cgpa.py）を通じて簡単に使用できます。

```bash
# RDF解析の実行
./src/cgpa.py rdf -i input.nml

# MSD解析の実行（出力ファイル指定）
./src/cgpa.py msd -i input.nml -o msd_results.txt

# Rg2解析の実行（詳細モード）
./src/cgpa.py rg2 -i input.nml -v
```

### コマンドラインオプション

```
usage: cgpa.py [-h] [-i INPUT] [-o OUTPUT] [-f {txt,hdf5}] [-v] [--version] {rdf,msd,rg2}

CGPA: Coarse-Grained Polymer Analysis Tool

positional arguments:
  {rdf,msd,rg2}         実行する解析プログラムを指定

optional arguments:
  -h, --help            ヘルプメッセージを表示
  -i, --input INPUT     入力Namelistファイルを指定
  -o, --output OUTPUT   出力ファイルを指定（省略時はデフォルト値）
  -f, --format {txt,hdf5}
                        出力形式を指定（デフォルト: txt）
  -v, --verbose         詳細な出力を表示
  --version             バージョン情報を表示
```

## 入力ファイル

入力ファイルはNamelistフォーマットで、シミュレーションパラメータを指定します。テンプレートは`data/template.nml`にあります。

## ディレクトリ構造

- **src/**: ソースコード
  - **modules/**: Fortranモジュール
  - **LammpsDumpReader/**: LAMMPSトラジェクトリファイル読み込みライブラリ
  - **cgpa.py**: Pythonインターフェース
- **examples/**: 解析プログラム
  - **rdf/**: 動径分布関数計算
  - **msd/**: 平均二乗変位計算
  - **rg2/**: 回転半径計算
  - **template/**: テンプレートプログラム
- **test/**: テストプログラム
- **data/**: テストデータ
- **tools/**: 補助ツール

## テスト

テストプログラムは以下のようにコンパイルして実行できます：

```bash
cd path/to/CGPolyAna/test
make clean
make
./a.out パラメータファイル
```

## ライセンス

このプロジェクトはMITライセンスの下で公開されています。詳細はLICENSEファイルを参照してください。
