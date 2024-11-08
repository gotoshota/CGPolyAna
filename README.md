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

### compile 
```
cd path/to/CGPolyAna
cd examples/xxx
make clean
make
```
xxx = ( msd rg2 rdf )
