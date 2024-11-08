#! /usr/bin/env python3
import subprocess
import os
import tkinter as tk
from tkinter import filedialog, messagebox, scrolledtext
import matplotlib.pyplot as plt
import numpy as np
import seaborn as sns

# Namelistパラメータのデフォルト値
def get_default_params():
    return {
        "dumpfilenames": "n400m100.lammpstrj",
        "ndumpfiles": 1,
        "nchains": 100,
        "nbeads": 400,
        "dt": 0.01,
        "nframes": 10,
        "dump_freq": 10000,
        "is_cubic": True,
        "is_log": True,
        "npoints": 20
    }

# Namelist形式に変換する関数
def convert_to_namelist(params):
    md_params = "&MDparams_nml\n"
    md_params += f'    dumpfilenames = "{params["dumpfilenames"]}"\n'
    md_params += f'    ndumpfiles = {params["ndumpfiles"]},\n'
    md_params += f'    nchains = {params["nchains"]},\n'
    md_params += f'    nbeads = {params["nbeads"]},\n'
    md_params += f'    dt = {params["dt"]},\n'
    md_params += f'    nframes = {params["nframes"]},\n'
    md_params += f'    dump_freq = {params["dump_freq"]},\n'
    md_params += f'    is_cubic = {".true." if params["is_cubic"] else ".false."}\n'
    md_params += "/\n\n"

    function_1d_info = "&Function1DInfo\n"
    function_1d_info += f'    is_log = {".true." if params["is_log"] else ".false."}\n'
    function_1d_info += f'    npoints = {params["npoints"]}\n'
    function_1d_info += "/\n"

    return md_params + function_1d_info

# 解析を実行する関数
def run_analysis():
    analysis = analysis_var.get()
    if not analysis:
        messagebox.showerror("エラー", "解析プログラムを選択してください")
        return

    # パラメータを取得
    params = {
        "dumpfilenames": dumpfilenames_var.get(),
        "ndumpfiles": int(ndumpfiles_var.get()),
        "nchains": int(nchains_var.get()),
        "nbeads": int(nbeads_var.get()),
        "dt": float(dt_var.get()),
        "nframes": int(nframes_var.get()),
        "dump_freq": int(dump_freq_var.get()),
        "is_cubic": is_cubic_var.get(),
        "is_log": is_log_var.get(),
        "npoints": int(npoints_var.get())
    }

    # Namelistをファイルに書き込む
    namelist_content = convert_to_namelist(params)
    namelist_file = "generated_namelist.namelist"
    with open(namelist_file, "w") as file:
        file.write(namelist_content)

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
    cmd = [analysis_program, namelist_file]

    # コマンドを実行
    try:
        result = subprocess.run(cmd, check=True, capture_output=True, text=True)
        output_text.insert(tk.END, result.stdout)
        messagebox.showinfo("完了", f'{analysis} の解析が完了しました。')
        select_data_for_visualization(result.stdout)
    except subprocess.CalledProcessError as e:
        output_text.insert(tk.END, f'エラーが発生しました: {e}\n{e.stderr}')
        messagebox.showerror("エラー", f'エラーが発生しました: {e}')

# 結果の可視化データを選択する関数
def select_data_for_visualization(output):
    lines = output.splitlines()
    data = []
    for line in lines:
        try:
            data.append(float(line))
        except ValueError:
            continue

    if not data:
        messagebox.showwarning("可視化", "可視化するデータがありません。")
        return

    # ユーザーに可視化するデータの範囲を選ばせる
    selection_window = tk.Toplevel()
    selection_window.title("データ選択")

    tk.Label(selection_window, text="可視化するデータの範囲を選択してください。").pack(pady=10)

    start_var = tk.IntVar(value=0)
    end_var = tk.IntVar(value=len(data))

    tk.Label(selection_window, text="開始インデックス:").pack()
    start_entry = tk.Entry(selection_window, textvariable=start_var)
    start_entry.pack()

    tk.Label(selection_window, text="終了インデックス:").pack()
    end_entry = tk.Entry(selection_window, textvariable=end_var)
    end_entry.pack()

    def on_select():
        start_idx = start_var.get()
        end_idx = end_var.get()
        visualize_results(data[start_idx:end_idx])
        selection_window.destroy()

    select_button = tk.Button(selection_window, text="可視化", command=on_select)
    select_button.pack(pady=10)

# 結果を可視化する関数
def visualize_results(data):
    if not data:
        messagebox.showwarning("可視化", "可視化するデータがありません。")
        return

    # Seabornを使用してグラフを描画
    sns.set(style="whitegrid")
    plt.figure()
    sns.lineplot(data=data, marker='o')
    plt.title("解析結果の可視化")
    plt.xlabel("Index")
    plt.ylabel("Value")
    plt.show()

# 可視化するファイルを選択する関数
def select_file_for_visualization():
    filename = filedialog.askopenfilename(title="可視化するファイルを選択", filetypes=[("All files", "*.*")])
    if not filename:
        messagebox.showerror("エラー", "ファイルが選択されていません。")
        return

    try:
        data = np.loadtxt(filename)
        if data.shape[1] < 2:
            messagebox.showerror("エラー", "ファイルに2列以上のデータが必要です。")
            return
        x_data = data[:, 0]
        y_data = data[:, 1]
        visualize_xy_data(x_data, y_data, filename)
    except Exception as e:
        messagebox.showerror("エラー", f"ファイルの読み込み中にエラーが発生しました: {e}")

# 2列のデータを可視化する関数
def visualize_xy_data(x_data, y_data, filename):
    if len(x_data) == 0 or len(y_data) == 0:
        messagebox.showwarning("可視化", "可視化するデータがありません。")
        return

    # Seabornを使用してグラフを描画
    sns.set(style="whitegrid")
    plt.figure()
    sns.lineplot(x=x_data, y=y_data, marker='o')
    plt.title(f"{filename}")
    plt.show()

# 説明文を取得する関数
def getDescription():
    return "CGPA: Coarse-Grained Particle Analysis を用いて書かれた解析プログラムを実行するためのGUIスクリプト\n解析プログラムはexamples ディレクトリに格納されている。\n実行する解析プログラムを選択し、Namelist ファイルを指定してください。"

# dumpfilenamesをファイラーから選択する関数
def select_dump_file():
    filename = filedialog.askopenfilename(title="ダンプファイルを選択", filetypes=[("LAMMPS dump files", "*.lammpstrj"), ("All files", "*.*")])
    if filename:
        dumpfilenames_var.set(filename)

# メインGUI部分
def main():
    global analysis_var, dumpfilenames_var, ndumpfiles_var, nchains_var, nbeads_var, dt_var, nframes_var, dump_freq_var, is_cubic_var, is_log_var, npoints_var, output_text

    root = tk.Tk()
    root.title("CGPA GUI")

    # 説明文
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

    # 各パラメータの入力欄
    params = get_default_params()

    dumpfilenames_var = tk.StringVar(value=params["dumpfilenames"])
    ndumpfiles_var = tk.StringVar(value=str(params["ndumpfiles"]))
    nchains_var = tk.StringVar(value=str(params["nchains"]))
    nbeads_var = tk.StringVar(value=str(params["nbeads"]))
    dt_var = tk.StringVar(value=str(params["dt"]))
    nframes_var = tk.StringVar(value=str(params["nframes"]))
    dump_freq_var = tk.StringVar(value=str(params["dump_freq"]))
    is_cubic_var = tk.BooleanVar(value=params["is_cubic"])
    is_log_var = tk.BooleanVar(value=params["is_log"])
    npoints_var = tk.StringVar(value=str(params["npoints"]))

    param_frame = tk.Frame(root)
    param_frame.pack(pady=10)

    param_entries = [
        ("dumpfilenames", dumpfilenames_var),
        ("ndumpfiles", ndumpfiles_var),
        ("nchains", nchains_var),
        ("nbeads", nbeads_var),
        ("dt", dt_var),
        ("nframes", nframes_var),
        ("dump_freq", dump_freq_var),
        ("is_cubic", is_cubic_var),
        ("is_log", is_log_var),
        ("npoints", npoints_var),
    ]

    for label, var in param_entries:
        frame = tk.Frame(param_frame)
        frame.pack(pady=2, anchor='w')
        tk.Label(frame, text=label + ":").pack(side=tk.LEFT)
        if label == "dumpfilenames":
            tk.Entry(frame, textvariable=var, width=40).pack(side=tk.LEFT)
            tk.Button(frame, text="参照", command=select_dump_file).pack(side=tk.LEFT)
        elif isinstance(var, tk.BooleanVar):
            tk.Checkbutton(frame, variable=var).pack(side=tk.LEFT)
        else:
            tk.Entry(frame, textvariable=var).pack(side=tk.LEFT)

    # 実行ボタン
    run_button = tk.Button(root, text="実行", command=run_analysis)
    run_button.pack(pady=10)

    # 出力表示エリア
    output_text = scrolledtext.ScrolledText(root, width=80, height=20)
    output_text.pack(pady=10)

    # 可視化するファイルを選択するボタン
    visualize_button = tk.Button(root, text="可視化データの選択", command=select_file_for_visualization)
    visualize_button.pack(pady=10)

    root.mainloop()

if __name__ == '__main__':
    main()

