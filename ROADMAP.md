# Verilisp ロードマップ

PROPOSAL.md の実装ロードマップを更新したもの。
各 Phase の目標・達成状況・残作業・次の方向性を整理する。

---

## 現在の状態

```
入力(.hvl) → マングリング → eval → AST(S式) → [解析パス]
                                             → [変換パス] → [emit-sv] → .sv
                                             → [emit]     → .v
```

- 旧パイプライン（stdout 直接出力）と新 AST パイプラインが並存
- CLI フラグで切り替え（`-t` vs `-tn`, `--sv`, `--analyze`）
- テスト: 旧 12 + AST 12 + SV 3 + 解析 2 = 計 29 テスト

---

## Phase 0: Python 廃止 — **完了**

CL 単体ドライバ (`verilisp.cl`) を実装し、`verilisp.py` を廃止。

## Phase 1a: AST コア — **完了**

`src/ast.cl`, `src/emit.cl`, `src/core.cl`, `src/driver.cl` を実装。
v_* マクロが AST (S式) を返す設計に移行。12/12 テスト通過。

## Phase 1b: use/expand/library — **完了**

`use` (ファイル読み込み), `expand` (ゲート展開), ライブラリファクトリマクロ対応。
12/12 テスト通過（Phase 1a と同じスイートで確認済み）。

---

## Phase 2: コンパイラパイプライン化 — **完了**

2層 IR + 複数バックエンド + 静的解析の基盤。

### 達成事項

| 機能 | ファイル | 状態 |
|------|---------|------|
| def-use 解析 | `src/analyze.cl` | 動作中 (2/2 テスト通過) |
| Verilog→SV 変換パス | `src/transform.cl` | 動作中 |
| SV エミッタ | `src/emit-sv.cl` | 動作中 (3/3 テスト通過) |
| CLIフラグ (`--sv`, `-tsv`, `-ta`, `--analyze`) | `verilisp.cl` | 動作中 |
| `translate-code-ast` の `:target`/`:analyze` 拡張 | `src/driver.cl` | 動作中 |

### 変換ルール (Verilog → SV)

| Verilog | SV | 条件 |
|---------|-----|------|
| `wire` | `logic` | 常に |
| `reg` | `logic` | 常に |
| `reg=` | `logic=` | 常に |
| `always @(posedge/negedge ...)` | `always_ff` | エッジセンシティブ |
| `always @(sig...)` / `always` | `always_comb` | レベル or 空 |

### 解析機能

- **Dead signal**: def あり use なし（output/inout 除く）
- **Undriven signal**: use あり driver なし（input/inout 除く、宣言のみは driver でない）

---

## Phase 2 残作業

Phase 2 の骨格は完成したが、PROPOSAL.md で構想されている機能がまだ残っている。

### 2a: SV 固有ノードの拡張

PROPOSAL.md で定義されている SV AST ノードのうち、未実装のもの:

| ノード | 用途 | 優先度 |
|--------|------|--------|
| `:interface` | SV interface 定義 | 中 |
| `:modport` | interface の modport | 中 |
| `:struct` | struct 型 | 低 |
| `:enum` | enum 型 | 低 |
| `:package` | SV package | 低 |
| `:assert` | assertion | 低 |

これらは examples/ で実際に必要になった段階で追加する。

### 2b: 解析パスの強化

| 機能 | 説明 | 優先度 |
|------|------|--------|
| ビット幅解析 | def の幅と use の幅を照合 | 中 |
| 組合せループ検出 | assign の依存グラフで循環を検出 | 中 |
| ポート接続チェック | inst のポート数・方向の整合性 | 中 |
| 未接続ポート検出 | モジュールインスタンスの未結線ポート | 中 |

### 2c: examples/ の SV ビルド

`kage/` に SV ビルドルール追加、`examples/` で `--sv` 出力が正しいことを確認。

---

## Phase 3: 代入記法の統一

PROPOSAL.md §1 で分析されている `<=` / `n=` / `<=#` の混乱を解消する。

### 現状の混在

```lisp
(= counter 0)       ;; ブロッキング代入  → counter = 0;
(n= counter data)    ;; ノンブロッキング  → counter <= data;       (旧記法)
(<=# data expr)      ;; ノンブロッキング  → data <= #1 expr;       (遅延付き)
(<= a b)             ;; 比較演算子        → (a <= b)               (式コンテキスト)
```

### 方針

AST 上ではすでに `:=`（ブロッキング）と `:<= `（ノンブロッキング）が明確に分離されている。
記法の統一は Phase 3 以降で検討。PROPOSAL.md の案:

- **案 B (推奨)**: `<-` をノンブロッキング代入にする。`<=` は比較演算子に固定。
- **案 C**: AST 上で文脈自動判別（文 = 代入、式 = 比較）

決定はユーザ入力の .hvl ファイルへの影響が大きいため、慎重に。

---

## Phase 4: CL パッケージによる名前分離

Python テキストマングリングを廃止し、CL パッケージで名前空間を分離する。

### 現状

```
ユーザ: (if cond ...)  →  マングラ: (v_if cond ...)  →  CLISP eval
ユーザ: (l_if cond ...)  →  マングラ: (if cond ...)  →  CL の if
```

### 目標

```lisp
(defpackage :verilisp
  (:use :cl)
  (:shadow #:if #:case #:module #:wire #:or #:and #:not #:= ...))

;; ユーザは自然な S 式を書く:
(module my_mod ((input clk)) ...)

;; マクロ定義内では cl: プレフィクスで CL にアクセス:
(defmacro if (cond then &optional else)
  `(list :if ... ,(cl:when else ...)))
```

### メリット

- `v_` プレフィクスと `l_` アンチマングリングが不要になる
- ユーザが書くコードが自然な S 式になる
- マングリングの正規表現バグ（区切り文字の判定ミス等）が消える

### リスク

- `__verilisp__.cl` 全体の書き換え（2116 行）
- 全マクロ定義で `cl:if`, `cl:and` 等が必要
- 既存 .hvl ファイルの互換性（`v_` を使っているコードがあれば動かなくなる）

### 前提条件

- Phase 1a-1b 完了（AST パイプラインが安定） ← **済**
- Phase 2 完了（複数バックエンドの検証） ← **済**
- 既存テスト・examples が AST パイプライン上で動くこと

---

## Phase 5: 高度な機能

| 機能 | 説明 | PROPOSAL.md 参照 |
|------|------|-----------------|
| テストベンチ自動生成 | モジュール AST から TB のスケルトンを生成 | §AST 操作の例 |
| ポート反転 | `flip-ports` で slave→master 変換 | §transform.cl |
| パイプラインステージ挿入 | AST 変換で FF 挿入 | §AST 操作の例 |
| REPL | AST を返す対話モード | §Phase 3 |
| 複数ファイル出力 | 1つの .hvl から複数 .v/.sv を生成 | — |
| if-utils のライブラリ化 | AST API で書き直し、lib/ に移動 | §Phase 1c |

---

## 優先順位サマリー

```
完了  Phase 0   Python 廃止
完了  Phase 1a  AST コア
完了  Phase 1b  use/expand/library
完了  Phase 2   SV バックエンド + def-use 解析
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
次    Phase 2b  解析パス強化（ビット幅・ループ検出）
      Phase 2c  examples/ の SV ビルド確認
      Phase 3   代入記法の統一
      Phase 4   CL パッケージ化（v_ 廃止）
      Phase 5   高度な機能（TB 生成・ポート反転等）
```

### 推奨する次の一手

**Phase 2b（解析パス強化）** が最もリスクが低く価値が高い。
既存コードへの影響なし（新規ファイルの拡張のみ）、
かつ解析機能の充実は SV バックエンドの品質保証にも直結する。

Phase 4（パッケージ化）は破壊的変更を伴うため、
解析パスで十分な「守り」を固めてから着手するのが安全。

---

## ファイル構成（現在）

```
verilisp/
├── __verilisp__.cl      # コア（マクロ展開・Verilog 生成、旧パイプライン）
├── verilisp.cl          # CL ドライバ（マングリング・CLI・テストランナー）
├── verilisp             # シェルラッパー
├── src/
│   ├── ast.cl           # AST ヘルパー（ast-node-p, walk-ast, transform-ast）
│   ├── emit.cl          # Verilog エミッタ
│   ├── emit-sv.cl       # SystemVerilog エミッタ
│   ├── core.cl          # v_* マクロ AST 版
│   ├── transform.cl     # Verilog→SV 変換パス
│   ├── analyze.cl       # def-use 解析
│   └── driver.cl        # AST パイプライン・テストランナー
├── lib/                 # ライブラリ（既存）
├── tests/               # テスト (12 + 3 SV + 2 解析)
├── examples/            # サンプル .hvl
├── kage/                # ビルド生成ツール
├── PROPOSAL.md          # 設計提案書（詳細な分析・方針）
├── ANALYSIS.md          # コードベース分析
└── ROADMAP.md           # ← このファイル
```

### ロード順序

```
__verilisp__.cl → src/ast.cl → src/emit.cl → src/emit-sv.cl
    → src/core.cl → src/transform.cl → src/analyze.cl → src/driver.cl
```
