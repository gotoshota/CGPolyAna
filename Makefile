# CGPolyAna ルートMakefile

# サブディレクトリの定義
EXAMPLE_DIRS = examples/rdf examples/msd examples/rg2 examples/template
MODULE_DIRS = src/modules src/LammpsDumpReader/fortran

# デフォルトターゲット
.PHONY: all
all: modules examples

# モジュールのビルド
.PHONY: modules
modules:
	@echo "モジュールをビルドしています..."
	@for dir in $(MODULE_DIRS); do \
		$(MAKE) -C $$dir || exit 1; \
	done

# 例のビルド
.PHONY: examples
examples: modules
	@echo "解析プログラムをビルドしています..."
	@for dir in $(EXAMPLE_DIRS); do \
		$(MAKE) -C $$dir || exit 1; \
	done

# クリーンアップ
.PHONY: clean
clean:
	@echo "クリーンアップしています..."
	@for dir in $(MODULE_DIRS) $(EXAMPLE_DIRS); do \
		$(MAKE) -C $$dir clean || exit 1; \
	done
	rm -f *.mod

# 完全クリーンアップ
.PHONY: clean-all
clean-all:
	@echo "すべてのビルド成果物を削除しています..."
	@for dir in $(MODULE_DIRS) $(EXAMPLE_DIRS); do \
		$(MAKE) -C $$dir clean-all || exit 1; \
	done
	rm -f *.mod

# テスト実行
.PHONY: test
test: examples
	@echo "テストを実行しています..."
	@cd test && $(MAKE) run

# ヘルプ
.PHONY: help
help:
	@echo "使用可能なターゲット:"
	@echo "  all        - すべてのモジュールと解析プログラムをビルド (デフォルト)"
	@echo "  modules    - 共通モジュールのみをビルド"
	@echo "  examples   - 解析プログラムをビルド"
	@echo "  clean      - ビルド成果物を削除"
	@echo "  clean-all  - すべてのビルド成果物を削除"
	@echo "  test       - テストを実行"
	@echo "  help       - このヘルプメッセージを表示" 
