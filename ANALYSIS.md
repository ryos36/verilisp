# Verilisp コードベース分析

## 概要

Verilisp は **Common Lisp のマクロシステムを活用して Verilog HDL を生成するトランスパイラ**。
従来のコンパイラ（パース→AST→コード生成）ではなく、Lisp のマクロ展開 = コード生成という設計。

元は Google Code のプロジェクトで、現在は独自に拡張されている。

## ファイル構成

```
verilisp/
├── __verilisp__.cl    # コアトランスパイラ (2,114行, Common Lisp)
├── verilisp.py        # Python ラッパー (263行)
├── ReadMe.md          # 簡易 README
├── examples/          # 17 個の .hvl サンプル
│   ├── macro_sim_count.hvl   # マクロでモジュール生成
│   ├── if-utils.hvl          # インターフェース操作マクロ群
│   ├── vgen.hvl              # ビデオジェネレータ (複雑な FSM)
│   ├── simple-sprite.hvl     # スプライトエンジン
│   ├── psdma.hvl             # パラメタライズド DMA
│   └── ...
├── lib/               # 24 カテゴリのライブラリ
│   ├── adders/        # 加算器
│   ├── multipliers/   # 乗算器
│   ├── counters/      # カウンター
│   ├── muxes/         # マルチプレクサ
│   ├── clocks/        # クロック生成
│   ├── alus/          # ALU (エラーあり)
│   ├── dividers/      # 割り算器 (不完全)
│   ├── fpus/          # FPU (作りかけ)
│   ├── fpadders/      # FP加算器 (未実装)
│   ├── fpmultipliers/ # FP乗算器 (未実装)
│   ├── fpshifters/    # FPシフタ (未実装)
│   └── ...
├── tests/             # 12 テストケース
├── scripts/           # Ninja ビルドルール
├── kage/              # Kage (Roswell) ビルド設定
├── RISC-V/            # RISC-V 実装例
├── factory/           # ファクトリ例
└── producer-consumer/ # プロデューサ・コンシューマ例
```

## コンパイルパイプライン

```
user.hvl (Verilisp)
    │
    ▼ Python: mangle() ── 名前マングリング (if → v_if, and → v_and 等)
    │                      80+ の予約語を v_ プレフィクス付きに変換
    │                      ENABLE_SECRET_BACKQUOTE_PROGN で `,` をトップレベルで使用可能に
    │
    ▼ CLISP: eval ── マクロ展開 = Verilog コード生成 (stdout に出力)
    │
    ▼ output.v (Verilog)
```

### 名前マングリング

Python 側が Verilisp コードを前処理し、Lisp の組み込み関数と衝突する名前を変換する。

- `(if ...)` → `(v_if ...)`
- `(and ...)` → `(v_and ...)`
- `(wire ...)` → `(v_wire ...)`
- `(+ a b)` → `(v_+ a b)`
- `(| a b)` → `(v_bitwise-or a b)` (特殊マングリング)
- `(l_and ...)` → `(and ...)` (逆マングリング: Lisp 本来の関数を使いたい場合)

NAMES_TO_MANGLE に 80+ のシンボルが定義されている。

### backquote_let__main__

```python
ENABLE_SECRET_BACKQUOTE_PROGN = True
```

有効時、ユーザコード全体を `` (eval `(let ((*__name__* :__main__)) ...)) `` でラップする。
これにより **トップレベルで `,` (comma) を使える**ようになり、defmacro なしにコンパイル時計算が可能になる。

## マクロシステムの構造

### レベル 1: Verilog 構文マクロ (1070行〜)

各 Verilog 構文に対応する `v_*` マクロ群。`format`/`write-string` で直接 Verilog テキストを出力する。

| マクロ | 生成する Verilog |
|--------|-----------------|
| `v_module` | `module ... endmodule` |
| `v_wire` | `wire` 宣言 |
| `v_reg` | `reg` 宣言 (1D/2Dバス対応) |
| `v_always` | `always @(...)` ブロック |
| `v_if` | `if/else` |
| `v_cond` | `if/else if/else` チェーン |
| `v_case`, `v_casex`, `v_casez` | case 文 |
| `v_assign` | `assign` 文 |
| `v_=`, `v_n=` | ブロッキング/ノンブロッキング代入 |
| `v_<=\#` | トランスポート遅延付きノンブロッキング代入 |
| `v_for`, `v_fromto` | for ループ |
| `v_initial` | initial ブロック |
| `v_task`, `v_function` | task/function 定義 |
| `v_cat`, `v_cat*` | 連結 `{a, b}` |
| `v_ref` | ビット選択 `a[n]`, `a[n:m]` |
| `v_?` | 三項演算子 |
| `v_+`, `v_-`, `v_*` 等 | 算術/論理演算子 (一括定義) |
| `v_display` 等 | `$display` 等のシステムタスク (一括定義) |

### レベル 2: ヘルパーマクロ・関数 (41行〜)

Lisp のメタプログラミング用ユーティリティ。

| 名前 | 用途 |
|------|------|
| `foreach`, `foreach*`, `foreach**` | リストイテレーション (mapcar のラッパー) |
| `range`, `slice`, `chunk` | Python 風のシーケンス操作 |
| `filter`, `zip`, `permute`, `powerset` | リスト操作 |
| `eval-all` | 文のリストを順次 eval |
| `strcat`, `symcat` | 文字列/シンボル結合 |
| `macall`, `macro-defun` | マクロを関数的に呼ぶユーティリティ |
| `dict-insert`, `dict-lookup` | 連想リスト操作 |

### レベル 3: モジュール生成マクロ

`make-named-module` で動的にマクロを定義。モジュールをインスタンス化するマクロを自動生成する。

```lisp
(make-named-module 'my_adder 'my_adder)
;; → (defmacro my_adder (&rest args) ...) が定義される
;; → 以降 (my_adder a b c) で my_adder anon_N (a, b, c); が出力される
```

v_module の末尾で自動的に `(make-module name)` が呼ばれ、定義したモジュール名がマクロになる。

### レベル 4: ファクトリマクロ (lib/ 内, examples/ 内)

パラメータからモジュール全体を生成するマクロ。

```lisp
;; examples/macro_sim_count.hvl
(defmacro make_sim_count (module-name name &optional (w 16) (sup "0"))
  `(module ,module-name
     ((input clk) (input debug_v))
     (reg (,w count))
     ...))

(make_sim_count sim_count "test0")        ;; 16ビットカウンタ
(make_sim_count sim_count_v2 "test1" 10)  ;; 10ビットカウンタ
```

### レベル 5: インターフェース操作 (examples/if-utils.hvl)

`macroexpand-1` でモジュール定義を解析し、インターフェースを操作するメタマクロ。

- `make-if-list-from-module` : モジュールマクロからインターフェースリストを抽出
- `make-flipped-if` : input/output を反転 (マスタ/スレーブ対応)
- `make-wire-list-from-if` : インターフェースから wire 宣言を生成
- `make-assign-list-from-if` : インターフェースから assign 文を生成
- `make-reg-wire-assign` : reg + wire + assign のセットを一括生成

## マクロが使えるか: 現状の評価

### 動いているもの

1. **基本的な defmacro** : `make_sim_count` のようなパラメトリックモジュール生成は動く
2. **backquote + comma** : `` `(module ,name ...) `` パターンは `ENABLE_SECRET_BACKQUOTE_PROGN` により動作
3. **foreach によるコンパイル時展開** : ループアンロールは動く
4. **macroexpand-1 によるイントロスペクション** : if-utils.hvl で実証済み
5. **use によるライブラリ読み込み** : ファイルを読んで eval する仕組みは動作

### 中途半端な部分・問題点

#### 1. マクロ内での Verilog 構文とLisp構文の混在

マクロ定義中に Verilog 生成マクロ（`module`, `wire`, `reg` 等）を使うとき、名前マングリングとの兼ね合いが複雑になる。

- ユーザコードでは `(module ...)` と書く → Python が `(v_module ...)` に変換
- マクロ定義内の `(module ...)` も変換される → **意図しない変換**が起きる場合がある
- `l_` プレフィクスで Lisp 側の関数を使う必要があり、認知コストが高い

```lisp
;; if-utils.hvl での回避例
(l_if (l_> ,range-n 0) ...)   ;; Lisp の if, > を使うために l_ 付き
```

#### 2. eval への依存

マクロの中で `eval` を多用している。本来は backquote + comma で解決できるケースでも `eval` が使われている。

```lisp
;; __verilisp__.cl の expand マクロ内
(eval `(v_wire ,@wires))
(eval `(,gate ,@args))
```

これはデバッグを困難にし、コンパイル時/実行時の区別を曖昧にする。

#### 3. 名前マングリングの限界

- 正規表現ベースの置換なので、文字列リテラル中の偶然の一致で誤変換の可能性がある
- `(` の直後にのみ変換がかかるため、シンボルを変数として渡す場合は変換されない
- マクロで生成したコード中のシンボルは**変換されない**（マングリング済みのコードが eval される）

#### 4. エラー処理の欠如

- 構文エラーは CLISP のエラーとして表示されるが、Verilog レベルでの検証はない
- マクロ展開エラーのメッセージが分かりにくい
- `verilisp.py` の例外処理が雑（`except:` でキャッチしてるだけ）

#### 5. ライブラリの未完成

lib/ReadMe.txt より:
- `alus` : エラーになる
- `dividers` : 中途半端
- `fpadders`, `fpmultipliers`, `fpshifters` : 未実装
- `fpus` : 作りかけ
- `testers` : 不完全

#### 6. テスト基盤

- 12 テストケースが存在し、すべてパス
- しかしマクロの高度な使い方（ファクトリパターン、インターフェース操作）のテストは含まれていない
- テストは入出力の文字列比較のみで、マクロ展開の中間結果は検証できない

#### 7. ドキュメント不足

- ReadMe.md は 20 行の最小限
- `__verilisp__.cl` のコメントはごく一部
- if-utils.hvl の関数群はパラメータの意味が分かりにくい

## アーキテクチャの特徴と評価

### 強み

- **Lisp のマクロシステムを直接活用** : パーサやASTが不要で、実装がシンプル
- **パラメトリック設計** : ビット幅やインスタンス数をパラメータ化する設計が自然に書ける
- **コンパイル時計算** : Lisp の全機能がコンパイル時に利用可能
- **Verilog のほぼ全構文をカバー** : module, always, if, case, for, task, function, primitive 等

### 弱み

- **CLISP 依存** : CLISP 固有の機能（`cs-common-lisp-user`）を使っている
- **グローバル状態への依存** : `indentation`, `*in-module*`, `*module-contents*` 等がグローバル変数
- **マクロのサイドエフェクト** : マクロが `write-string` で直接出力するため、マクロ合成が難しい
- **2パス処理（Python → CLISP）** : デバッグ時に2つの言語のコンテキストを追う必要がある

## 「ちゃんとしたもの」にするための課題

### 最優先

1. **マクロ使用のドキュメント整備** : defmacro でモジュール生成マクロを定義する手順とパターンの文書化
2. **マクロ用テストの追加** : ファクトリマクロ、foreach 展開、インターフェース操作のテスト
3. **エラーメッセージの改善** : マクロ展開エラー時にユーザに有用な情報を提供

### 重要

4. **名前マングリング周りの整理** : `l_` プレフィクスが必要な場面のルール明確化
5. **if-utils.hvl のライブラリ化** : examples/ ではなく lib/ に移動し、use で利用可能にする
6. **壊れているライブラリの修正 or 削除** : alus, fpus 等

### あると良い

7. **デバッグ支援** : マングリング後のコード表示、マクロ展開ステップの表示
8. **ReadMe.md の充実** : インストール手順、基本的な使い方、マクロのチュートリアル
9. **CI/テスト自動化** : GitHub Actions 等での自動テスト
