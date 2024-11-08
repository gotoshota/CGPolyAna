#! /usr/bin/env python3
import subprocess
import os
import tkinter as tk
from tkinter import filedialog, messagebox, scrolledtext

def run_analysis():
    analysis = analysis_var.get()
    input_file = input_file_var.get()

    if not analysis:
        messagebox.showerror("エラー", "解析プログラムを選択してください")
        return

    if not input_file:
        messagebox.showerror("エラー", "Namelist ファイルを選択してください")
        return

    # スクリプトのディレクトリからcgpaのルートディレクトリを取得
    script_path = os.path.realpath(os.path.abspath(__file__))
    cgpa_root = os.path.abspath(os.path.join(os.path.dirname(script_path), os.pardir))

    # 実行する解析プログラムのパスを設定
    analysis_program = os.path.join(cgpa_root, 'examples', analysis, "a.out")

    # コマンドの存在を確認
    if not os.path.isfile(analysis_program):
        messagebox.showerror("エラー", f'解析プログラム {analysis_program} が見つかりません。ビルドされているか確認してください。')
        return

    # 実行するコマンドを構築
    cmd = [analysis_program, input_file]

    # コマンドを実行
    try:
        result = subprocess.run(cmd, check=True, capture_output=True, text=True)
        output_text.insert(tk.END, result.stdout)
        messagebox.showinfo("完了", f'{analysis} の解析が完了しました。')
    except subprocess.CalledProcessError as e:
        output_text.insert(tk.END, f'エラーが発生しました: {e}\n{e.stderr}')
        messagebox.showerror("エラー", f'エラーが発生しました: {e}')

def select_input_file():
    filename = filedialog.askopenfilename(title="Namelist ファイルを選択", filetypes=[("Namelist files", "*.namelist"), ("All files", "*.*")])
    if filename:
        input_file_var.set(filename)

def getDescription():
    return """CGPA: Coarse-Grained Particle Analysis を用いて書かれた解析プログラムを実行するためのGUIスクリプト
解析プログラムはexamples ディレクトリに格納されている。
実行する解析プログラムを選択し、Namelist ファイルを指定してください。
"""

def main():
    global analysis_var, input_file_var, output_text

    root = tk.Tk()
    root.title("CGPA GUI")

    description = getDescription()
    desc_label = tk.Label(root, text=description, justify=tk.LEFT)
    desc_label.pack(pady=10)

    # 解析プログラムの選択
    analysis_frame = tk.Frame(root)
    analysis_frame.pack(pady=5)

    tk.Label(analysis_frame, text="解析プログラムを選択:").pack(side=tk.LEFT)
    analysis_var = tk.StringVar()
    analysis_options = ["rdf", "msd", "rg2"]
    analysis_menu = tk.OptionMenu(analysis_frame, analysis_var, *analysis_options)
    analysis_menu.pack(side=tk.LEFT)

    # Namelist ファイルの選択
    input_frame = tk.Frame(root)
    input_frame.pack(pady=5)

    tk.Label(input_frame, text="Namelist ファイル:").pack(side=tk.LEFT)
    input_file_var = tk.StringVar()
    input_entry = tk.Entry(input_frame, textvariable=input_file_var, width=40)
    input_entry.pack(side=tk.LEFT)
    input_button = tk.Button(input_frame, text="参照", command=select_input_file)
    input_button.pack(side=tk.LEFT)

    # 実行ボタン
    run_button = tk.Button(root, text="実行", command=run_analysis)
    run_button.pack(pady=10)

    # 出力表示エリア
    output_text = scrolledtext.ScrolledText(root, width=80, height=20)
    output_text.pack(pady=10)

    root.mainloop()

if __name__ == '__main__':
    main()

