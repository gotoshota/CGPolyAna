# CGPolyAna モジュールMakefile

# コンパイラ設定
FC ?= gfortran
FCFLAGS ?= -O2 -fopenmp

# ソースファイル
SRCS = global_types.f90 \
       string_utils.f90 \
       math.f90 \
       physical_constants.f90 \
       statistics.f90 \
       coord_convert.f90 \
       correlation_function.f90 \
       io.f90 \
       ../LammpsDumpReader/fortran/lammpsio.f90

# オブジェクトファイル
OBJS = $(SRCS:.f90=.o)

# モジュールファイル
MODS = $(SRCS:.f90=.mod)

# デフォルトターゲット
.PHONY: all
all: $(OBJS)

# 依存関係
global_types.o: global_types.f90
	@echo "コンパイル中: $<"
	@$(FC) $(FCFLAGS) -c -o $@ $<

string_utils.o: string_utils.f90
	@echo "コンパイル中: $<"
	@$(FC) $(FCFLAGS) -c -o $@ $<

math.o: math.f90
	@echo "コンパイル中: $<"
	@$(FC) $(FCFLAGS) -c -o $@ $<

physical_constants.o: physical_constants.f90
	@echo "コンパイル中: $<"
	@$(FC) $(FCFLAGS) -c -o $@ $<

statistics.o: statistics.f90 math.o
	@echo "コンパイル中: $<"
	@$(FC) $(FCFLAGS) -c -o $@ $<

coord_convert.o: coord_convert.f90
	@echo "コンパイル中: $<"
	@$(FC) $(FCFLAGS) -c -o $@ $<

correlation_function.o: correlation_function.f90 math.o statistics.o
	@echo "コンパイル中: $<"
	@$(FC) $(FCFLAGS) -c -o $@ $<

io.o: io.f90 global_types.o
	@echo "コンパイル中: $<"
	@$(FC) $(FCFLAGS) -c -o $@ $<

# クリーンアップ
.PHONY: clean clean-all

clean:
	@echo "クリーンアップ: $(CURDIR)"
	@rm -f *.o

clean-all:
	@echo "完全クリーンアップ: $(CURDIR)"
	@rm -f *.o *.mod 
