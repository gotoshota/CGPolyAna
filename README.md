# Coarse-Grained Polymer Analyser
<<<<<<< HEAD
簡単な解析プログラムとそのモジュール。
粗視化高分子モデルの解析に使った。
基本的には、LAMMPSのテキスト形式のトラジェクトリファイルを読み込んで解析するため、
[LammpsDumpReader](git@github.com:gotoshota/LammpsDumpReader.git)も一緒に使うことを想定している。

## src
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
=======
このリポジトリは、粗視化高分子（ビーズ・スプリングモデル）の分子動力学シミュレーションの軌跡を解析するためのFortranモジュールといくつかの実行可能バイナリを提供します。

現在、LAMMPSのテキスト形式のトラジェクトリファイル（.lammpstrj）のみサポートしています。

## Examples
examplesディレクトリ内にあるコードを用いて、平均二乗変位（MSD）および平均二乗回転半径（Rg2）を簡単に計算できます。
>>>>>>> main

### compile 
```
cd path/to/CGPolyAna
cd examples/xxx
make clean
make
```
xxx = ( msd rg2 rdf )
