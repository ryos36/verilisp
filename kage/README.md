# kage

Verilisp プロジェクト用の Ninja ビルドファイル生成ツール。
`build.kage`（S 式の設定ファイル）から `build.ninja` を自動生成する。

## 依存

- [Roswell](https://github.com/roswell/roswell) (Common Lisp 処理系マネージャ)
- Quicklisp ライブラリ: `cl-fad`, `cl-ppcre`, `uiop`
- [Ninja](https://ninja-build.org/) (ビルドシステム)

## 仕組み

```
build.kage (S式の設定)
    ↓ kage.ros
    ↓  1. :build の正規表現で .hvl ファイルを検出
    ↓  2. 各ファイルの (use ...) 文を解析
    ↓  3. 他ファイルから use されているファイル → ライブラリと判定
    ↓  4. (module tb ()) または *__name__* を含む → メイン(テストベンチ)と判定
    ↓
build.ninja (Ninja設定)
    ↓ ninja
    ↓  translate:        verilisp.py で .hvl → .v
    ↓  mangle-translate: verilisp.py --mangle で .hvl → .hvlib (ライブラリ用)
    ↓  compile:          iverilog で .v → 実行形式
    ↓
*.v, 実行形式
```

## 使い方

### 1. build.kage を作成

```lisp
((:build . ".*\\.hvl$")
 (:lib . :auto))
```

### 2. kage を実行

```bash
../kage/kage.ros build.kage
```

### 3. ninja でビルド

```bash
ninja      # 全ビルド
ninja v    # .v ファイル生成のみ
ninja lib  # ライブラリのみ
ninja exe  # シミュレーション実行形式のみ
```

## build.kage の設定項目

| キー | 説明 | 例 |
|------|------|-----|
| `:build` | コンパイル対象ファイル（正規表現） | `".*\\.hvl$"` |
| `:lib` | ライブラリ指定。`:auto` で自動検出 | `:auto` |
| `:module` | インストール対象（正規表現） | `".*mod\\.hvl$"` |
| `:install-dir` | インストール先ディレクトリ | `"/tmp/install"` |
| `:rule-file` | Ninja ルールファイルのパス | `"../scripts/rule.ninja"` |

### 設定例（フル）

```lisp
((:build . ".*\\.hvl$")
 (:module . ".*mod\\.hvl$")
 (:install-dir . "/tmp/install")
 (:rule-file . "../scripts/rule.ninja")
 (:lib . :auto))
```

## コマンドラインオプション

```
kage.ros [OPTIONS] [build.kage]

  --out FILENAME   出力ファイル名 (デフォルト: build.ninja、"-" で stdout)
  --lib DIR        ライブラリ出力先 (デフォルト: lib)
  --build DIR      ビルド出力先 (デフォルト: _build)
  --v DIR / -v DIR Verilog 出力先 (デフォルト: v)
```

## ビルドルール (scripts/rule.ninja)

| ルール | 動作 |
|--------|------|
| `translate` | `verilisp.py` で `.hvl` → `.v` に変換。depfile 対応 |
| `mangle-translate` | `verilisp.py --mangle` で `.hvl` → `.hvlib` に変換（ライブラリ用） |
| `compile` | `iverilog -DTEST_BENCH -g2012` でシミュレーション実行形式を生成 |
| `do-install` | ファイルをインストール先にコピー |
