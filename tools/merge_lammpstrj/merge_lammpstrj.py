import argparse
from tqdm import tqdm

def parse_args():
    parser = argparse.ArgumentParser(description="Merge two LAMMPS trajectory files.")
    parser.add_argument("--trj1", required=True, help="Path to the first LAMMPS trajectory file.")
    parser.add_argument("--trj2", required=True, help="Path to the second LAMMPS trajectory file.")
    parser.add_argument("--output", required=True, help="Path to the output file.")
    parser.add_argument("--help", "-h", action="store_true", help="Display this help message.")
    return parser.parse_args()

def display_usage():
    print("Usage:")
    print("  python merge_lammpstrj.py --trj1 <path_to_trj1> --trj2 <path_to_trj2> --output <output_path>")
    print("Options:")
    print("  --trj1 <path_to_trj1>  Path to the first LAMMPS trajectory file.")
    print("  --trj2 <path_to_trj2>  Path to the second LAMMPS trajectory file.")
    print("  --output <output_path> Path to the output file.")
    print("  --help                 Display this help message.")

def find_timestep_position(file_path):
    timestep_positions = {}
    with open(file_path, 'r') as file:
        lines = file.readlines()
        for i in tqdm(range(len(lines)), desc=f"Reading {file_path}"):
            line = lines[i]
            if line.startswith("ITEM: TIMESTEP"):
                timestep = int(lines[i + 1].strip())
                position = i
                timestep_positions[timestep] = position
    return timestep_positions

def find_merge_timestep(timesteps1, timesteps2):
    for timestep in timesteps1:
        if timestep in timesteps2:
            return timestep
    return None

def write_merged_file(file1_path, file2_path, timesteps1, timesteps2, merge_timestep, output_path):
    with open(file1_path, 'r') as file1, open(file2_path, 'r') as file2, open(output_path, 'w') as output_file:
        lines1 = file1.readlines()
        lines2 = file2.readlines()

        # Write first part from file1
        for i in tqdm(range(timesteps1[merge_timestep]), desc="Processing trj1"):
            output_file.write(lines1[i])
        
        # Write second part from file2
        start_index = timesteps2[merge_timestep]
        for i in tqdm(range(start_index, len(lines2)), desc="Processing trj2"):
            output_file.write(lines2[i])

def main():
    args = parse_args()
    if args.help:
        display_usage()
        return
    # タイムステップの位置を見つける
    timesteps1 = find_timestep_position(args.trj1)
    timesteps2 = find_timestep_position(args.trj2)

    # 結合ポイントを見つける
    merge_timestep = find_merge_timestep(timesteps1, timesteps2)
    if merge_timestep is None:
        raise ValueError("一致するTIMESTEPが見つかりませんでした。")

    # 結合ファイルを書き出す
    write_merged_file(args.trj1, args.trj2, timesteps1, timesteps2, merge_timestep, args.output)

    print(f"結合されたファイルは '{args.output}' に保存されました。")

if __name__ == "__main__":
    main()

