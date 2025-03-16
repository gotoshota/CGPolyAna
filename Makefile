# CGPolyAna ルートMakefile

# 解析プログラムディレクトリ
ANALYSIS_DIRS = examples/rdf examples/msd examples/rg2

# モジュールディレクトリ
MODULES_DIR = src/modules
LDR_DIR = src/LammpsDumpReader/fortran

# コンパイラ設定
FC = gfortran
FCFLAGS = -O2 -fopenmp

# モジュールオブジェクトファイル
MOD_OBJECTS = $(MODULES_DIR)/error_handling.o \
              $(MODULES_DIR)/global_types.o \
              $(MODULES_DIR)/string_utils.o \
              $(MODULES_DIR)/math.o \
              $(MODULES_DIR)/physical_constants.o \
              $(MODULES_DIR)/statistics.o \
              $(MODULES_DIR)/coord_convert.o \
              $(MODULES_DIR)/correlation_function.o \
              $(MODULES_DIR)/io.o \
              $(LDR_DIR)/lammpsio.o

# デフォルトターゲット
.PHONY: all
all: modules copy_mods $(ANALYSIS_DIRS)

# モジュールのビルド
.PHONY: modules
modules:
	@echo "Building modules..."
	@mkdir -p $(MODULES_DIR)
	@mkdir -p $(LDR_DIR)
	
	@echo "Compiling error_handling.f90..."
	@$(FC) $(FCFLAGS) -c -o $(MODULES_DIR)/error_handling.o $(MODULES_DIR)/error_handling.f90
	
	@echo "Compiling global_types.f90..."
	@$(FC) $(FCFLAGS) -I$(MODULES_DIR) -c -o $(MODULES_DIR)/global_types.o $(MODULES_DIR)/global_types.f90
	
	@echo "Compiling string_utils.f90..."
	@$(FC) $(FCFLAGS) -I$(MODULES_DIR) -c -o $(MODULES_DIR)/string_utils.o $(MODULES_DIR)/string_utils.f90
	
	@echo "Compiling math.f90..."
	@$(FC) $(FCFLAGS) -I$(MODULES_DIR) -c -o $(MODULES_DIR)/math.o $(MODULES_DIR)/math.f90
	
	@echo "Compiling physical_constants.f90..."
	@$(FC) $(FCFLAGS) -c -o $(MODULES_DIR)/physical_constants.o $(MODULES_DIR)/physical_constants.f90
	
	@echo "Compiling statistics.f90..."
	@$(FC) $(FCFLAGS) -I$(MODULES_DIR) -c -o $(MODULES_DIR)/statistics.o $(MODULES_DIR)/statistics.f90
	
	@echo "Compiling coord_convert.f90..."
	@$(FC) $(FCFLAGS) -I$(MODULES_DIR) -c -o $(MODULES_DIR)/coord_convert.o $(MODULES_DIR)/coord_convert.f90
	
	@echo "Compiling correlation_function.f90..."
	@$(FC) $(FCFLAGS) -I$(MODULES_DIR) -c -o $(MODULES_DIR)/correlation_function.o $(MODULES_DIR)/correlation_function.f90
	
	@echo "Compiling io.f90..."
	@$(FC) $(FCFLAGS) -I$(MODULES_DIR) -c -o $(MODULES_DIR)/io.o $(MODULES_DIR)/io.f90
	
	@echo "Compiling lammpsio.f90..."
	@$(FC) $(FCFLAGS) -c -o $(LDR_DIR)/lammpsio.o $(LDR_DIR)/lammpsio.f90

# モジュールファイルをコピー
.PHONY: copy_mods
copy_mods:
	@echo "Copying module files to analysis directories..."
	@for dir in $(ANALYSIS_DIRS); do \
		echo "  Copying to $$dir..."; \
		cp -f $(MODULES_DIR)/*.o $$dir/ 2>/dev/null || true; \
		cp -f $(MODULES_DIR)/*.mod $$dir/ 2>/dev/null || true; \
		cp -f $(LDR_DIR)/*.mod $$dir/ 2>/dev/null || true; \
	done

# 各解析プログラムのビルド
.PHONY: $(ANALYSIS_DIRS)
$(ANALYSIS_DIRS): modules copy_mods
	@echo "Building $@..."
	@$(MAKE) -C $@

# クリーンアップ
.PHONY: clean clean-all

clean:
	@for dir in $(ANALYSIS_DIRS); do \
		echo "Cleaning $$dir..."; \
		$(MAKE) -C $$dir clean; \
	done

clean-all:
	@for dir in $(ANALYSIS_DIRS); do \
		echo "Cleaning all in $$dir..."; \
		$(MAKE) -C $$dir clean-all; \
	done
	@echo "Cleaning modules..."
	@rm -f $(MODULES_DIR)/*.o $(MODULES_DIR)/*.mod $(LDR_DIR)/*.o $(LDR_DIR)/*.mod

# ヘルプ
.PHONY: help
help:
	@echo "CGPolyAna ビルドシステム"
	@echo ""
	@echo "使用可能なターゲット:"
	@echo "  all       - すべての解析プログラムをビルド（デフォルト）"
	@echo "  modules   - 共通モジュールのみをビルド"
	@echo "  copy_mods - モジュールファイルを各解析ディレクトリにコピー"
	@echo "  clean     - 各解析プログラムディレクトリの一時ファイルを削除"
	@echo "  clean-all - すべてのビルド成果物を削除"
	@echo "  help      - このヘルプメッセージを表示"
	@echo ""
	@echo "個別のプログラムをビルドするには:"
	@for dir in $(ANALYSIS_DIRS); do \
		echo "  make -C $$dir"; \
	done 
