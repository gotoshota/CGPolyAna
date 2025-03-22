# ビルドプロセス改善計画

このドキュメントは、CGPolyAnaプロジェクトのビルドプロセスを改善するための計画と進捗状況をまとめたものです。

## 目標

- Makefileの最適化によるビルドの簡易化
- プロジェクトルートにMakefile.commonとMakefileを配置し、一括ビルドを可能に
- CI/CDによるドキュメントの自動生成
- READMEやドキュメントの充実

## 実装計画と進捗状況

### 1. ルートMakefileの作成と配置

- [x] 計画立案
- [x] プロジェクトルートにMakefileを作成
- [x] サブディレクトリのMakefileを更新して共通設定を使用
- [x] テスト実行とバグ修正

### 2. Makefile.commonの改善

- [x] 計画立案
- [x] 依存関係の自動検出機能の追加
- [x] ビルドメッセージの改善
- [x] ROOT_DIRの自動検出機能の追加
- [x] テスト実行とバグ修正

### 3. CI/CD設定

- [x] 計画立案
- [ ] GitHub Actionsの設定ファイル作成
- [ ] ドキュメント生成ワークフローの実装
- [ ] テスト実行ワークフローの追加
- [ ] デプロイ設定の確認

### 4. ドキュメント構造の整備

- [x] 計画立案
- [ ] docs/ディレクトリの作成
- [ ] モジュールごとのドキュメントテンプレート作成
- [ ] チュートリアルと使用例の追加
- [ ] API参照ドキュメントの生成設定

## 実装詳細

### ルートMakefile

```makefile
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

# 完全クリーンアップ
.PHONY: clean-all
clean-all:
	@echo "すべてのビルド成果物を削除しています..."
	@for dir in $(MODULE_DIRS) $(EXAMPLE_DIRS); do \
		$(MAKE) -C $$dir clean-all || exit 1; \
	done

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
```

### Makefile.common

```makefile
# 共通Makefile for CGPolyAna
# 各解析プログラムから include されることを想定

# コンパイラ設定（カスタマイズ可能）
FC ?= gfortran
FCFLAGS ?= -O2 -fopenmp
LINKFLAGS ?= -fopenmp

# ディレクトリ構造
ROOT_DIR ?= $(shell git rev-parse --show-toplevel 2>/dev/null || echo ../..)
SRCDIR = $(ROOT_DIR)/src/
MODDIR = $(SRCDIR)/modules
LDRDIR = $(SRCDIR)/LammpsDumpReader/fortran
BUILDDIR = .

# 出力ファイル名
OUTPUT ?= a.out

# モジュールオブジェクトファイル
MOD_OBJECTS = $(MODDIR)/global_types.o \
              $(MODDIR)/string_utils.o \
              $(MODDIR)/math.o \
              $(MODDIR)/physical_constants.o \
              $(MODDIR)/statistics.o \
              $(MODDIR)/coord_convert.o \
              $(MODDIR)/correlation_function.o \
              $(MODDIR)/io.o \
              $(LDRDIR)/lammpsio.o

# デフォルトターゲット
all: $(BUILDDIR)/$(OUTPUT)

# メインプログラムのビルド
$(BUILDDIR)/$(OUTPUT): $(TARGET).o $(MOD_OBJECTS)
	@echo "リンク中: $@"
	@$(FC) $(LINKFLAGS) -o $@ $(MOD_OBJECTS) $<

# メインプログラムのコンパイル
$(TARGET).o: $(TARGET).f90
	@echo "コンパイル中: $<"
	@$(FC) $(FCFLAGS) -I$(MODDIR) -I$(LDRDIR) -c -o $@ $<

# モジュールのビルド（必要な場合）
$(MOD_OBJECTS):
	@echo "モジュールをビルドしています..."
	@$(MAKE) -C $(MODDIR)
	@$(MAKE) -C $(LDRDIR)

# クリーンアップ
.PHONY: clean clean-all

clean:
	@echo "クリーンアップ: $(BUILDDIR)"
	@rm -f $(BUILDDIR)/*.o $(BUILDDIR)/*.mod $(BUILDDIR)/$(OUTPUT)

clean-all:
	@echo "完全クリーンアップ"
	@$(MAKE) -C $(MODDIR) clean
	@$(MAKE) -C $(LDRDIR) clean
	@rm -f $(BUILDDIR)/*.o $(BUILDDIR)/*.mod $(BUILDDIR)/$(OUTPUT)
```

## 今後の課題

- モジュール間の依存関係の自動検出
- ビルド時間の短縮
- テスト自動化の拡充
- ドキュメントのカバレッジ向上

## 注意点

- rg2プログラムはLAPACKライブラリに依存しています。ビルド時に`dsyev`関数が見つからないエラーが発生した場合は、以下のように`examples/rg2/Makefile`を修正してください：

```makefile
# Rg2解析プログラムMakefile

# メインプログラムのファイル名（拡張子なし）
TARGET = rg2

# 追加のリンクフラグ（LAPACKライブラリを追加）
LINKFLAGS += -llapack -lblas

# 共通Makefileをインクルード
include ../../Makefile.common
```

また、システムにLAPACKライブラリがインストールされていない場合は、以下のコマンドでインストールしてください：

```bash
# Ubuntuの場合
sudo apt-get install liblapack-dev libblas-dev

# CentOS/Fedoraの場合
sudo yum install lapack-devel blas-devel

# macOSの場合（Homebrewを使用）
brew install lapack
```

## 参考リソース

- [GNU Make マニュアル](https://www.gnu.org/software/make/manual/make.html)
- [GitHub Actions ドキュメント](https://docs.github.com/ja/actions)
- [MkDocs ドキュメント](https://www.mkdocs.org/)
- [FORD: Fortran ドキュメント生成ツール](https://github.com/Fortran-FOSS-Programmers/ford) 