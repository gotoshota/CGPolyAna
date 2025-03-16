#!/usr/bin/env python3
"""
CGPolyAna: Coarse-Grained Polymer Analysis Tool

このスクリプトは、粗視化高分子のシミュレーションデータを解析するためのコマンドラインインターフェースを提供します。
Fortranで実装された解析プログラムを呼び出し、結果を出力します。
"""

import argparse
import subprocess
import os
import sys
import json
import logging
from pathlib import Path
from typing import List, Dict, Any, Optional, Union

# ロガーの設定
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    datefmt='%Y-%m-%d %H:%M:%S'
)
logger = logging.getLogger('CGPolyAna')

# 解析プログラムの定義
ANALYSIS_PROGRAMS = {
    "rdf": {
        "description": "Radial Distribution Function を計算するプログラム",
        "output_formats": ["txt", "hdf5"],
        "default_output": "rdf.txt"
    },
    "msd": {
        "description": "Mean Squared Displacement を計算するプログラム",
        "output_formats": ["txt", "hdf5"],
        "default_output": "msd.txt"
    },
    "rg2": {
        "description": "Radius of Gyration を計算するプログラム",
        "output_formats": ["txt", "hdf5"],
        "default_output": "rg2.txt"
    }
}

def get_cgpa_root() -> Path:
    """
    CGPolyAnaのルートディレクトリのパスを取得します。
    
    Returns:
        Path: CGPolyAnaのルートディレクトリのパス
    """
    script_path = Path(os.path.realpath(os.path.abspath(__file__)))
    return script_path.parent.parent

def get_analysis_program_path(analysis: str) -> Path:
    """
    解析プログラムのパスを取得します。
    
    Args:
        analysis: 解析プログラムの名前
        
    Returns:
        Path: 解析プログラムのパス
        
    Raises:
        ValueError: 解析プログラムが見つからない場合
    """
    cgpa_root = get_cgpa_root()
    program_path = cgpa_root / 'examples' / analysis / "a.out"
    
    if not program_path.exists():
        raise ValueError(f'解析プログラム {program_path} が見つかりません。ビルドされているか確認してください。')
    
    return program_path

def run_analysis(analysis: str, input_file: str, output_file: Optional[str] = None, 
                output_format: str = "txt", verbose: bool = False) -> int:
    """
    解析プログラムを実行します。
    
    Args:
        analysis: 解析プログラムの名前
        input_file: 入力ファイルのパス
        output_file: 出力ファイルのパス（省略時はデフォルト値）
        output_format: 出力形式（txt, hdf5）
        verbose: 詳細な出力を表示するかどうか
        
    Returns:
        int: 終了コード（0: 成功, 非0: エラー）
        
    Raises:
        ValueError: 入力ファイルが存在しない場合
        ValueError: 出力形式が無効な場合
    """
    # 入力ファイルの存在確認
    input_path = Path(input_file)
    if not input_path.exists():
        raise ValueError(f'入力ファイル {input_file} が見つかりません。')
    
    # 出力ファイルの設定
    if output_file is None:
        output_file = ANALYSIS_PROGRAMS[analysis]["default_output"]
    
    # 出力形式の確認
    if output_format not in ANALYSIS_PROGRAMS[analysis]["output_formats"]:
        raise ValueError(f'出力形式 {output_format} は {analysis} でサポートされていません。'
                        f'サポートされている形式: {", ".join(ANALYSIS_PROGRAMS[analysis]["output_formats"])}')
    
    # 解析プログラムのパスを取得
    try:
        program_path = get_analysis_program_path(analysis)
    except ValueError as e:
        logger.error(str(e))
        return 1
    
    # コマンドの構築
    cmd = [str(program_path), str(input_path)]
    
    # 出力ファイルと形式の指定
    cmd.extend(["--output", output_file])
    cmd.extend(["--format", output_format])
    
    # 詳細モードの設定
    if verbose:
        cmd.append("--verbose")
    
    # コマンドの実行
    logger.info(f'実行コマンド: {" ".join(cmd)}')
    try:
        if verbose:
            # 標準出力と標準エラー出力を表示
            subprocess.run(cmd, check=True)
        else:
            # 標準出力と標準エラー出力を非表示
            subprocess.run(cmd, check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        
        logger.info(f'{analysis} の解析が完了しました。出力ファイル: {output_file}')
        return 0
    except subprocess.CalledProcessError as e:
        logger.error(f'解析プログラムの実行中にエラーが発生しました: {e}')
        if e.stdout:
            logger.error(f'標準出力: {e.stdout.decode()}')
        if e.stderr:
            logger.error(f'標準エラー出力: {e.stderr.decode()}')
        return e.returncode

def get_description() -> str:
    """
    プログラムの説明文を取得します。
    
    Returns:
        str: プログラムの説明文
    """
    description = """CGPA: Coarse-Grained Polymer Analysis Tool

このプログラムは、粗視化高分子のシミュレーションデータを解析するためのコマンドラインインターフェースを提供します。
Fortranで実装された解析プログラムを呼び出し、結果を出力します。

利用可能な解析プログラム:
"""
    
    for name, info in ANALYSIS_PROGRAMS.items():
        description += f"  {name}: {info['description']}\n"
    
    description += """
使用例:
  cgpa.py rdf -i rdf.namelist
  cgpa.py msd -i msd.namelist -o msd_results.txt
  cgpa.py rg2 -i rg2.namelist -o rg2_results.hdf5 -f hdf5 -v
"""
    
    return description

def main() -> int:
    """
    メイン関数
    
    Returns:
        int: 終了コード（0: 成功, 非0: エラー）
    """
    # コマンドライン引数のパーサーを設定
    parser = argparse.ArgumentParser(
        description=get_description(),
        formatter_class=argparse.RawTextHelpFormatter
    )
    
    parser.add_argument('analysis', choices=list(ANALYSIS_PROGRAMS.keys()), 
                        help='実行する解析プログラムを指定')
    parser.add_argument('-i', '--input', required=True, 
                        help='入力Namelistファイルを指定')
    parser.add_argument('-o', '--output', 
                        help='出力ファイルを指定（省略時はデフォルト値）')
    parser.add_argument('-f', '--format', choices=['txt', 'hdf5'], default='txt',
                        help='出力形式を指定（デフォルト: txt）')
    parser.add_argument('-v', '--verbose', action='store_true',
                        help='詳細な出力を表示')
    parser.add_argument('--version', action='version', version='CGPolyAna 1.0.0',
                        help='バージョン情報を表示')
    
    args = parser.parse_args()
    
    # ログレベルの設定
    if args.verbose:
        logger.setLevel(logging.DEBUG)
    
    # 解析プログラムの実行
    try:
        return run_analysis(
            analysis=args.analysis,
            input_file=args.input,
            output_file=args.output,
            output_format=args.format,
            verbose=args.verbose
        )
    except ValueError as e:
        logger.error(str(e))
        return 1
    except Exception as e:
        logger.exception(f'予期しないエラーが発生しました: {e}')
        return 1

if __name__ == '__main__':
    sys.exit(main())
