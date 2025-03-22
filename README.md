# Coarse-Grained Polymer Analyser
簡単な解析プログラムとそのモジュール。
粗視化高分子モデルの解析に使った。
基本的には、LAMMPSのテキスト形式のトラジェクトリファイルを読み込んで解析するため、
[LammpsDumpReader](git@github.com:gotoshota/LammpsDumpReader.git)も一緒に使うことを想定している。

## src
### gui.py
GUIで解析できる。
```
./gui.py
```
で起動する。
tkinterを使っているので、対応したpython3が必要。
macOSの場合、
```
brew install python-tk
```
でインストールできる。

各種依存ライブラリは、
```
pip install -r requirements.txt
```
でインストールできる。

### cgpa.py
解析プログラムを呼び出すインターフェース。
```
./cgpa.py rdf -i input.nml
```
といったふうに呼び出す。

## Examples
このディレクトリには、例としていくつかの解析プログラムが入っている。
インプットとして、Namelistが必要。
現在
- rdf
- rg2 
- msd 
がある。
それぞれ`cpga.py`から呼び出せる。

## data
テスト用のNamelistおよびトラジェクトリデータが入ってる。
このリポジトリは、粗視化高分子（ビーズ・スプリングモデル）の分子動力学シミュレーションの軌跡を解析するためのFortranモジュールといくつかの実行可能バイナリを提供します。

現在、LAMMPSのテキスト形式のトラジェクトリファイル（.lammpstrj）のみサポートしています。

## Examples
examplesディレクトリ内にあるコードを用いて、平均二乗変位（MSD）および平均二乗回転半径（Rg2）を簡単に計算できます。

### compile 
```
cd path/to/CGPolyAna
cd examples/xxx
make clean
make
```
xxx = ( msd rg2 rdf )

## テスト

### 依存関係
テストプログラム（test.f90）は以下のモジュールに依存しています：
- global_types（src/modules/global_types.f90）
- lammpsIO（test/lammpsIO.f90）
- correlation_function（src/modules/correlation_function.f90）
- coord_convert（src/modules/coord_convert.f90）
- math（src/modules/math.f90）
- physical_constants（src/modules/physical_constants.f90）
- string_utils（src/modules/string_utils.f90）

### コンパイル方法
テストプログラムは以下のようにコンパイルできます：
```
cd path/to/CGPolyAna
cd test
make clean
make
```

コンパイルプロセスでは、まずsrc/modulesディレクトリ内のモジュールがコンパイルされ、次にtest/lammpsIO.f90、最後にtest.f90がコンパイルされます。コンパイラはgfortranを使用し、最適化レベル2（-O2）と警告フラグ（-Wall）が設定されています。

モジュールのコンパイル順序は重要で、まずglobal_types.f90、次にstring_utils.f90、その後他のモジュールがコンパイルされます。

### テストの実行方法
コンパイル後、以下のコマンドでテストを実行できます：
```
cd path/to/CGPolyAna/test
./a.out パラメータファイル
```

ここで「パラメータファイル」はNamelistフォーマットのファイルで、シミュレーションパラメータを指定します。テンプレートはdata/template.nmlにあります。

テストプログラムはLAMMPSトラジェクトリファイルを読み込み、分子の座標を処理し、結果をtest.lammpstrjとtest_coms.lammpstrjファイルに出力します。また、いくつかの物理定数（π、ボルツマン定数、気体定数）も表示します。
