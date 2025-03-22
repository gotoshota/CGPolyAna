#! /usr/bin/env python3
## @file cgpa.py
## @brief CGPolyAna解析プログラムのメインインターフェース
## @details 解析プログラムを実行するためのコマンドラインインターフェースを提供します
## @author CGPolyAnaチーム
## @date 2023年

import argparse
import subprocess
import os
import glob

## @brief 利用可能なコマンドを検出する関数
## @details examples ディレクトリ内のサブディレクトリを検索し、利用可能なコマンドのリストを返す
## @return 利用可能なコマンドのリスト
def discover_commands():
    """
    examples ディレクトリ内のサブディレクトリを検索し、
    利用可能なコマンドのリストを返す
    """
    script_path = os.path.realpath(os.path.abspath(__file__))
    cgpa_root = os.path.abspath(os.path.join(os.path.dirname(script_path), os.pardir))
    examples_dir = os.path.join(cgpa_root, 'examples')
    
    commands = []
    for item in os.listdir(examples_dir):
        item_path = os.path.join(examples_dir, item)
        if os.path.isdir(item_path) and os.path.exists(os.path.join(item_path, 'a.out')):
            commands.append(item)
    
    return commands

## @brief メイン関数
## @details コマンドライン引数を解析し、指定された解析プログラムを実行する
def main():
    # 利用可能なコマンドを検出
    available_commands = discover_commands()
    
    # コマンドライン引数のパーサーを設定
    myDescription = getDescription(available_commands)
    parser = argparse.ArgumentParser(
            description=myDescription,
            formatter_class=argparse.RawTextHelpFormatter)
    
    parser.add_argument('analysis', choices=available_commands, 
                        help='実行する解析プログラムを指定')
    parser.add_argument('-i', '--input', required=True, 
                        help='Namelist を指定してください')
    parser.add_argument('-v', '--verbose', action='store_true',
                        help='詳細な出力を表示')
    
    args = parser.parse_args()

    # cgpa.py の親ディレクトリのパスを取得
    script_path = os.path.realpath(os.path.abspath(__file__))
    cgpa_root = os.path.abspath(os.path.join(os.path.dirname(script_path), os.pardir))

    # 実行する解析プログラムのパスを設定
    analysis_program = os.path.join(cgpa_root, 'examples', args.analysis, "a.out")

    # コマンドの存在を確認
    if not os.path.isfile(analysis_program):
        print(f'エラー: 解析プログラム {analysis_program} が見つかりません。')
        print(f'ビルドするには: cd {os.path.join(cgpa_root, "examples", args.analysis)} && make')
        exit(1)

    # 実行するコマンドを構築
    cmd = [analysis_program, args.input]

    # コマンドを実行
    try:
        if args.verbose:
            print(f'実行コマンド: {" ".join(cmd)}')
        
        subprocess.run(cmd, check=True)
        print(f'{args.analysis} の解析が完了しました。')
    except subprocess.CalledProcessError as e:
        print(f'エラーが発生しました: {e}')
        exit(1)

## @brief ヘルプメッセージを生成する関数
## @param available_commands 利用可能なコマンドのリスト
## @return フォーマットされたヘルプメッセージ
def getDescription(available_commands):
    description = """CGPA: Coarse-Grained Particle Analysis を用いて書かれた解析プログラムを実行するためのスクリプト
解析プログラムはexamples ディレクトリに格納されている。
実行する解析プログラムを指定するための引数 analysis は以下の選択肢から選択する。
"""
    
    # 利用可能なコマンドの説明を追加
    for cmd in available_commands:
        if cmd == "rdf":
            description += f"  {cmd}: Radial Distribution Function を計算するプログラム\n"
        elif cmd == "msd":
            description += f"  {cmd}: Mean Squared Displacement を計算するプログラム\n"
        elif cmd == "rg2":
            description += f"  {cmd}: Radius of Gyration を計算するプログラム\n"
        elif cmd == "template":
            description += f"  {cmd}: テンプレートプログラム（新しい解析プログラムの作成用）\n"
        else:
            description += f"  {cmd}: ユーザー定義の解析プログラム\n"
    
    description += """
引数には、解析プログラムに渡す Namelist ファイルを指定する必要がある。
実行例：
cgpa rdf -i rdf.namelist
"""
    return description

if __name__ == '__main__':
    main()
