# CGPolyAna 改善マスタープラン

このドキュメントは、CGPolyAnaプロジェクトの改善に関する全体計画と進捗状況をまとめたものです。

## 主要目標

1. ビルドプロセスの簡易化と標準化
2. インターフェース（cgpa.py）の機能拡張
3. ユーザー拡張性の向上
4. ドキュメントの充実

## 実装計画と進捗状況

### フェーズ1: ビルドシステムの改善

- [x] 計画立案
- [x] ルートMakefileの作成
- [x] Makefile.commonの改善
- [x] サブディレクトリMakefileの更新
- [x] ビルドテスト

詳細: [build_and_makefile_improvement.md](build_and_makefile_improvement.md)

### フェーズ2: cgpa.pyの機能拡張

- [x] 計画立案
- [x] サブコマンド自動検出機能の実装
- [x] エラーハンドリングの強化
- [x] ヘルプメッセージの充実
- [x] 機能テスト

詳細: [cgpa_improvement.md](cgpa_improvement.md)

### フェーズ3: テンプレート機能の実装

- [x] 計画立案
- [x] テンプレートディレクトリとファイルの作成
- [x] テンプレートのテスト
- [x] ドキュメント作成

詳細: [template_implementation.md](template_implementation.md)

### フェーズ4: CI/CDとドキュメント整備

- [x] 計画立案
- [ ] GitHub Actionsの設定
- [ ] ドキュメント生成ワークフローの実装
- [ ] READMEの更新
- [ ] チュートリアルの作成

## タイムライン

| フェーズ | 作業内容 | 予定期間 | 状態 |
|---------|---------|---------|------|
| 1 | ビルドシステムの改善 | 1週間 | 完了 ✓ |
| 2 | cgpa.pyの機能拡張 | 1週間 | 完了 ✓ |
| 3 | テンプレート機能の実装 | 3日間 | 完了 ✓ |
| 4 | CI/CDとドキュメント整備 | 1週間 | 未着手 |

## 優先順位

1. ~~ビルドシステムの改善（基盤となるため最優先）~~ ✓
2. ~~テンプレート機能の実装（ユーザー拡張性のため）~~ ✓
3. ~~cgpa.pyの機能拡張（使いやすさ向上のため）~~ ✓
4. CI/CDとドキュメント整備（保守性向上のため）

## 実装の依存関係

```
ビルドシステムの改善 ✓
    ↓
テンプレート機能の実装 ✓
    ↓
cgpa.pyの機能拡張 ✓
    ↓
CI/CDとドキュメント整備
```

## 実装済みの機能

### 1. ビルドシステムの改善
- プロジェクトルートにMakefile.commonを作成し、共通のビルド設定を定義
- プロジェクトルートにMakefileを作成し、すべてのモジュールと解析プログラムをビルド可能に
- 各モジュールディレクトリのMakefileを更新
- 各解析プログラムディレクトリのMakefileを簡素化

### 2. テンプレート機能の実装
- examples/templateディレクトリを作成
- テンプレートMakefileを作成
- 'Hello from CGPA!'を表示するサンプルコードを作成
- テンプレート使用方法のドキュメントを作成

### 3. cgpa.pyの機能拡張
- サブコマンドの自動検出機能を実装
- コマンドライン引数の処理を改善
- エラーハンドリングを強化
- ヘルプメッセージを充実

## 今後の展望

- パフォーマンス最適化
- 並列計算サポートの強化
- 可視化ツールの統合
- ユニットテストの拡充
- コード品質の向上

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

## 参考リソース

- [GNU Make マニュアル](https://www.gnu.org/software/make/manual/make.html)
- [GitHub Actions ドキュメント](https://docs.github.com/ja/actions)
- [Python argparse ドキュメント](https://docs.python.org/ja/3/library/argparse.html)
- [Fortran プログラミングガイド](https://gcc.gnu.org/onlinedocs/gfortran/) 