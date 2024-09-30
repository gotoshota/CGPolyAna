#! /usr/bin/env python3
import argparse
import subprocess
import os

def main():
    # コマンドライン引数のパーサーを設定
    myDescription = getDescription()
    parser = argparse.ArgumentParser(
            description=myDescription,
            formatter_class=argparse.RawTextHelpFormatter)
    parser.add_argument('analysis', choices=["rdf", "msd", "rg2"], help='実行する解析プログラムを指定')
    parser.add_argument('-i', '--input', required=True, help='Namelist を指定してください')
    args = parser.parse_args()

    # cgpa.py の親ディレクトリのパスを取得
    script_path = os.path.realpath(os.path.abspath(__file__))
    cgpa_root = os.path.abspath(os.path.join(os.path.dirname(script_path), os.pardir))

    # 実行する解析プログラムのパスを設定
    analysis_program = os.path.join(cgpa_root, 'examples', args.analysis, "a.out")

    # コマンドの存在を確認
    if not os.path.isfile(analysis_program):
        print(f'解析プログラム {analysis_program} が見つかりません。ビルドされているか確認してください。')
        exit(1)

    # 実行するコマンドを構築
    cmd = [analysis_program, args.input]

    # コマンドを実行
    try:
        subprocess.run(cmd, check=True)
        print(f'{args.analysis} の解析が完了しました。出力ファイル: {args.output}')
    except subprocess.CalledProcessError as e:
        print(f'エラーが発生しました: {e}')
        exit(1)

def getDescription():
    return """CGPA: Coarse-Grained Particle Analysis を用いて書かれた解析プログラムを実行するためのスクリプト
解析プログラムはexamples ディレクトリに格納されている。
実行する解析プログラムを指定するための引数 analysis は以下の選択肢から選択する。
  rdf: Radial Distribution Function を計算するプログラム
  msd: Mean Squared Displacement を計算するプログラム
  rg2: Radius of Gyration を計算するプログラム
引数には、解析プログラムに渡す Namelist ファイルを指定する必要がある。
実行例：
cgpa rdf -i rdf.namelist
"""

if __name__ == '__main__':
    main()
