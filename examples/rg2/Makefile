# Rg2解析プログラムのMakefile

# ターゲットプログラム名
TARGET = rg2

# lammpsio.modファイルをコピー
$(shell cp ../../src/LammpsDumpReader/fortran/lammpsio.mod . 2>/dev/null || true)

# LAPACKとBLASのリンク
LINKFLAGS = -llapack -lblas

# 共通Makefileをインクルード
include ../../Makefile.common
