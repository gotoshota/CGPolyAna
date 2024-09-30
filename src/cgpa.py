#! /usr/bin/env python3
import argparse
import subprocess
import os

def main():
    # コマンドライン引数のパーサーを設定
    parser = argparse.ArgumentParser(description='解析プログラムを実行するスクリプト')
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

if __name__ == '__main__':
    main()
