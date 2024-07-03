import argparse
import math

def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument('--nbeads', required=True, type=int, help='Number of atoms')
    parser.add_argument('--output', default="output.data", type=str, help='Output file name')
    parser.add_argument('--topology', default="linear", type=str, help='Type of polymer: linear or ring')
    parser.add_argument('--style', default="bond", type=str, help='Style of polymer: bond or angle')
    return parser.parse_args()

def generate_ring_data(n, style="bond"):
    pi = math.acos(-1.0)
    r = n * 0.97 / pi / 2
    b_r = r + 1
    
    data = []
    data.append("LAMMPS data file")
    data.append(f"{n} atoms")
    data.append(f"{n} bonds")
    data.append(f"{n} angles")
    data.append("")
    data.append("1 atom types")
    data.append("1 bond types")
    data.append("1 angle types")
    data.append("")
    data.append(f"{-b_r:.3f} {b_r:.3f} xlo xhi")
    data.append(f"{-b_r:.3f} {b_r:.3f} ylo yhi")
    data.append(f"{-b_r:.3f} {b_r:.3f} zlo zhi")
    data.append("\n")
    data.append("Masses")
    data.append("")
    data.append("1 1.0")
    data.append("\n")
    data.append("Atoms")
    data.append("")
    
    for i in range(1, n + 1):
        x = r * math.cos(2 * pi * i / n)
        y = r * math.sin(2 * pi * i / n)
        data.append(f"{i} 1 1 {x:.3f} {y:.3f} 0.0")
    
    data.append("\n")
    data.append("Bonds")
    data.append("")
    for i in range(1, n):
        data.append(f"{i} 1 {i} {i + 1}")
    data.append(f"{n} 1 {n} 1")
    
    if style == "bond":
        return "\n".join(data)
    elif style == "angle":
        data.append("\n")
        data.append("Angles")
        data.append("")
        data.append(f"1 1 {n} 1 2")
        for i in range(2, n):
            data.append(f"{i} 1 {i - 1} {i} {i + 1}")
        data.append(f"{n} 1 {n - 1} {n} 1")
    
    return "\n".join(data)


def generate_linear_data(n, style="bond"):
    b_r = n * 0.97 + 1
    
    data = []
    data.append("LAMMPS data file")
    data.append("")
    data.append(f"{n} atoms")
    data.append(f"{n-1} bonds")
    data.append(f"{n-2} angles")
    data.append("")
    data.append("1 atom types")
    data.append("1 bond types")
    data.append("1 angle types")
    data.append("")
    data.append(f"0.0 {b_r:.2f} xlo xhi")
    data.append(f"0.0 {b_r:.2f} ylo yhi")
    data.append(f"0.0 {b_r:.2f} zlo zhi")
    data.append("\n")
    data.append("Masses")
    data.append("")
    data.append("1 1.0")
    data.append("\n")
    data.append("Atoms")
    data.append("")
    
    for i in range(1, n + 1):
        data.append(f"{i} 1 1 0.0 0.0 {0.96 * (i - 1):.2f}")
    
    data.append("\n")
    data.append("Bonds")
    data.append("")
    for i in range(1, n):
        data.append(f"{i} 1 {i} {i + 1}")
    
    if style == "bond":
        return "\n".join(data)
    elif style == "angle":
        data.append("\n")
        data.append("Angles")
        data.append("")
        for i in range(2, n):
            data.append(f"{i} 1 {i - 1} {i} {i + 1}")
        return "\n".join(data)


if __name__ == "__main__":
    args = parse_args()

    if args.topology == "linear":
        data = generate_linear_data(args.nbeads, args.style)
    elif args.topology == "ring":
        data = generate_ring_data(args.nbeads, args.style)
    else:
        raise ValueError("Unknown topology")
    with open(args.output, "w") as f:
        f.write(data)


