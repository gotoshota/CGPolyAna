# Coarse-Grained Polymer Analyser
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
xxx = ( msd rg2 )
