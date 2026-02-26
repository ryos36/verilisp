# Verilisp 改善提案 v2

study-cps/for_verlisp のコンパイラ設計パターンと polyphony の二層 IR 設計を参考に、
具体的な実装方針をまとめる。

---

## 歴史的混乱: ブロッキング代入 vs ノンブロッキング代入

### Verilog における `<=` の二重意味

Verilog HDL では `<=` が2つの全く異なる意味を持つ:

| 文脈 | 意味 | 例 |
|------|------|-----|
| 文（statement） | ノンブロッキング代入 | `data <= data + 1;` |
| 式（expression） | 比較演算子（以下） | `if (count <= 10)` |

これは Verilog 自体の設計上の問題で、文脈で区別する必要がある。

### オリジナル verilisp（Google Code 版）の解決策

オリジナルは **`<=` を使わず `n=` という独自記法** でノンブロッキング代入を表現した:

```lisp
;; fork 元 (981fb9d) の定義
(foreach* (
    (name '(= n=))                   ;; = はブロッキング、n= はノンブロッキング
    (symbol '(" = " " <= ")))        ;; 出力する Verilog テキスト
  ...)

;; ユーザコード
(= counter 0)     ;; → counter = 0;      (ブロッキング)
(n= counter 0)    ;; → counter <= 0;     (ノンブロッキング)
```

`<=` は**比較演算子としてのみ**使える設計だった。
`n=` は Verilog を知っている人には馴染みがないが、曖昧さはなかった。

### 変更の経緯（git log + Issue.md/ReadMe.md から再構成）

```
981fb9d  fork from google code (2018-07)
         └─ n= = ノンブロッキング、<= = 比較演算子（明確な分離）
         └─ オリジナルの設計意図: Lisp の <= と衝突しないよう n= を導入

73d4825  enable to use <= instead of n= with transport-delay (2021-12)
         └─ v_<= マクロを新設。<= をノンブロッキング代入としても使えるように
         └─ *transport-delay*, *inertial-delay* パラメータ追加
         └─ 意図: (<= data expr) で data <= #1 expr; を出力
         └─ 問題: <= が代入と比較の両方に使われるようになった

2314124  Issue.md 作成 (2021-12)
         └─ 「I have overwrited <= operator to behave non blocking assignment
              on 73d4825a. The new f_<= function has a advantage to be able
              to use transport-delay, but behind the function i.e.
              less than operator.」
         └─ ← 自分でも <= の二重意味に気づいており、問題を記録している

66cbfc5  Refactoring about Non Blocking Assignment(<=) (2023-01-12)
         └─ <= を NAMES_TO_MANGLE の「math operators」セクションに移動
         └─ %<= と =< を「以下」の代替として追加
         └─ ReadMe.md 更新:「Now you can use '<=' lisp function for
              non blocking assignment with transport-delay.
              If you want to use less than operator, Please use '%<=' or '=<'.」
         └─ 方針: <= = ノンブロッキング代入、%<= or =< = 比較

5663c21  introduce <=# operator (2023-01-12、同日に方針転換)
         └─ v_<= → v_<=# にリネーム（# はトランスポート遅延の意味）
         └─ <= は比較演算子に戻す（66cbfc5 の方針を即座に撤回）
         └─ ReadMe.md 更新:「<= is a less than operator not assignment operator.」
         └─ 全 examples の (<= ...) を (<=# ...) に書き換え
         └─ 方針転換の理由: <= を代入にすると比較が書けなくなる

32cdd6b  add <=# operator into verilisp.py (2023-01-13)
         └─ Python のマングリングリストに '<=\#' を追加
         └─ v_if の yes-body に eval-all を試みる（複数文対応の試行）

e6c8a26  Reverted incorrect implementation (2023-01-21)
         └─ v_if の eval-all を revert
         └─ 「複数文は progn を使え」という結論
```

**注目**: 66cbfc5 と 5663c21 は同日（2023-01-12）のコミット。
66cbfc5 で `<=` を「ノンブロッキング代入」として整理したが、これは**勘違い**だった。
実際には `<=` は比較演算子として使われるべきで、同日中に 5663c21 で修正している。

**これが混乱の本質**: `<=` の意味が曖昧なため、**作者自身が間違えた**。
コードを書いている本人ですら `<=` がどちらの意味か混乱するのであれば、
他のユーザはもっと間違える。この問題は記法レベルで解決する必要がある。

### 現在の状態（3つの記法が混在）

```lisp
;; 1. ブロッキング代入 (=)
(= counter 0)        ;; → counter = 0;

;; 2. ノンブロッキング代入 — 旧記法 (n=)
(n= counter 0)       ;; → counter <= 0;     ※トランスポート遅延なし

;; 3. ノンブロッキング代入 — 新記法 (<=#)
(<=# counter 0)      ;; → counter <= #1 0;  ※*transport-delay* 使用

;; 4. 比較演算子（以下）— 3つの記法
(<= a b)             ;; → (a <= b)   ※「math operator」としてマングリング
(%<= a b)            ;; → (a <= b)   ※代替記法
(=< a b)             ;; → (a <= b)   ※代替記法（experimental）
```

### 本質的な問題

1. **`<=` の意味が文脈依存**: 比較演算子として NAMES_TO_MANGLE に入っているが、
   Verilog では同じ `<=` がノンブロッキング代入でもある。verilisp が「`<=` は比較」と
   決め打ちしたことで、ユーザが Verilog の慣習通り `<=` でノンブロッキング代入を
   書こうとすると混乱する。

2. **`n=` と `<=#` の併存**: オリジナルの `n=` も新しい `<=#` も両方動く。
   どちらを使うべきか不明確。

3. **`<=#` はトランスポート遅延前提**: `*transport-delay*` が nil のとき
   `<=#` は `data <= data;` を出力するが、`n=` も同じ出力。違いが分かりにくい。

4. **`v_if` の複数文問題**: `if` の then 節に複数文を書きたい場合、
   `progn` で囲む必要がある（32cdd6b で eval-all を試みたが e6c8a26 で revert）。
   これはブロッキング/ノンブロッキングとは直接関係ないが、always ブロック内で
   代入文を複数書く際に遭遇しやすい問題で、混乱に拍車をかけている。

### なぜ人間が間違えるのか

根本原因は **Verilog 自体が `<=` に2つの意味を持たせている** こと。
そしてオリジナル verilisp はその問題を回避するために `n=` を発明したが、
「Verilog と違う書き方」が別の混乱を生んだ。

人間のメンタルモデル:
- Verilog 経験者 → `<=` はノンブロッキング代入だと思っている（文脈で使い分けている）
- Lisp 経験者 → `<=` は比較演算子だと思っている
- 両方知っている人 → verilisp でどちらになるか分からない

### 解決策の候補

#### 案 A: 記号で分ける（現状の延長線）

`<=` の意味を1つに固定し、もう1つには別の記号を割り当てる。

```lisp
;; A-1: <= をノンブロッキングに（Verilog 寄り）
(= counter 0)         ;; → counter = 0;       ブロッキング
(<= counter 0)        ;; → counter <= 0;      ノンブロッキング
(=< a b)              ;; → (a <= b)           比較（以下）

;; A-2: <= を比較に（Lisp 寄り — 現状）
(= counter 0)         ;; → counter = 0;       ブロッキング
(<= a b)              ;; → (a <= b)           比較（以下）
(<=# counter 0)       ;; → counter <= #1 0;   ノンブロッキング
```

**問題**: どちらにしても「覚えないと分からない」。Verilog 知ってる人も Lisp 知ってる人も片方は裏切られる。

#### 案 B: 見た目が全く違う記号にする（曖昧さゼロ）

`<=` を**どちらの意味にも使わない**。

```lisp
(= counter 0)         ;; → counter = 0;       ブロッキング
(<- counter 0)        ;; → counter <= 0;      ノンブロッキング（矢印記法）
(<= a b)              ;; → (a <= b)           比較（Lisp 慣習通り）
```

`<-` は「左辺に値を送る」という直感的な意味。Haskell の `<-` や
シーケンス図の矢印に近い。`<=` と見間違えることがない。

```lisp
;; 遅延付き
(<- counter 0 :delay 1)   ;; → counter <= #1 0;

;; 実際のコードでの見え方
(always ((posedge clk))
  (<- data (+ data 1))        ;; ← 明らかに代入
  (if (<= count 10)           ;; ← 明らかに比較
    (= count (+ count 1))))   ;; ← 明らかに代入
```

**利点**: 3つの記号 `=`, `<-`, `<=` が完全に異なる見た目。一目で区別できる。
Verilog の `<=` 二重意味問題を Lisp 側で解消している。

**欠点**: Verilog の `<=` と対応しないので、Verilog コードから脳内変換が必要。

#### 案 C: AST で文脈自動判別（Verilog と同じ書き方）

AST があれば**構文位置で意味を自動判定**できる。Verilog と全く同じ使い方。

```lisp
(always ((posedge clk))
  (<= data (+ data 1))        ;; 文の位置 → ノンブロッキング代入
  (if (<= count 10)           ;; 式の位置 → 比較
    (= count (+ count 1))))   ;; 文の位置 → ブロッキング代入
```

判定ルール:
- **文の位置**（always/initial/if/case の直接の子）: `<=` = ノンブロッキング代入
- **式の位置**（if の条件、代入の右辺、演算子の引数）: `<=` = 比較演算子

```lisp
;; マクロ実装（AST 版）
(defmacro v_<= (&rest args)
  ;; 文脈は呼び出し元のマクロが知っている
  ;; → v_if は condition を式として、body を文として処理
  ;; → v_always は body を文として処理
  ;; AST ノードの段階で区別がつく
  ...)
```

**利点**: Verilog ユーザが何も新しく覚える必要がない。完全に Verilog 準拠。
**欠点**: 実装が複雑。文脈情報をマクロ間で伝播する仕組みが必要。
エラー時に「ここでの `<=` はどちらの意味？」が分かりにくい場合がある。

#### 案 D: 明示的な命名（Scheme スタイル）

`!` サフィックスで副作用（代入）を明示。Scheme の慣習。

```lisp
(set! counter 0)       ;; → counter = 0;       ブロッキング
(set<! counter 0)      ;; → counter <= 0;      ノンブロッキング
(<= a b)               ;; → (a <= b)           比較
```

または日本語的に:

```lisp
(block= counter 0)     ;; ブロッキング代入
(nonblock= counter 0)  ;; ノンブロッキング代入
(<= a b)               ;; 比較
```

**利点**: 完全に曖昧さがない。コードを読むだけで意味が分かる。
**欠点**: 冗長。Verilog の見た目から離れる。

### 推奨

**案 B（`<-` 記法）** を推奨する。理由:

1. **一目で区別がつく**: `=`, `<-`, `<=` は形が全く違う
2. **覚えやすい**: `<-` は「値を左辺に送る」で直感的
3. **実装がシンプル**: 文脈判定不要。マクロ1つで済む
4. **`<=` が Lisp 本来の意味（比較）を保つ**: `l_<=` 等の回避策が不要
5. **Verilog からの距離は最小限**: `=` はそのまま、`<=` だけ `<-` に変わる

```lisp
;; 最終的な代入体系
(= lvalue expr)            ;; → lvalue = expr;          ブロッキング
(<- lvalue expr)           ;; → lvalue <= expr;         ノンブロッキング
(<- lvalue expr :delay n)  ;; → lvalue <= #n expr;      遅延付き

;; 互換エイリアス（既存コード用）
(n= lvalue expr)           ;; → (<- lvalue expr) と同じ
(<=# lvalue expr)          ;; → (<- lvalue expr :delay *transport-delay*) と同じ
```

AST 上では:

```lisp
(:= counter 0)              ;; ブロッキング
(:<- counter 0)              ;; ノンブロッキング
(:<- counter 0 :delay 1)     ;; 遅延付きノンブロッキング
(:=< a b)                    ;; 比較（以下）— <=のemitを出す
```

**案 C（文脈自動判別）も将来的に併用可能。** AST があれば案 B の `<-` が文の位置に
あることを検証できるし、将来「`<=` も文脈で使えるモード」を追加することもできる。
まずは案 B でシンプルに始め、必要に応じて案 C を追加するのが低リスク。

### v_if の複数文問題の解決

AST 化すれば `progn` は不要になる:

```lisp
;; 現状: progn が必要
(if condition
  (progn
    (<- a 1)
    (<- b 2)))

;; AST 版: v_if が &rest body を取れる
(if condition
  (<- a 1)
  (<- b 2))

;; → AST:
(:if condition
  (:block
    (:<- a 1)
    (:<- b 2)))
```

`v_if` のマクロ定義で then 部の複数文を `:block` ノードに自動ラップすればよい。

ただし if/else の区切りが曖昧になる問題がある。これは Lisp の `if` と同じで:
- `(if cond a)` — then のみ
- `(if cond a b)` — then + else（各1文）
- `(if cond a b c)` — then が a b c？ then が a b で else が c？

**解決策**: `else` キーワードで明示的に区切る。

```lisp
(if condition
  (<- a 1)
  (<- b 2)
  :else
  (<- a 0))

;; → AST:
(:if condition
  (:block (:<- a 1) (:<- b 2))
  (:block (:<- a 0)))
```

または現状の `progn` を維持しつつ、`cond` を推奨する手もある:

```lisp
(cond
  (condition
    (<- a 1)
    (<- b 2))
  (else
    (<- a 0)))
```

`cond` はすでに複数文を自然に扱えるので、`if` + `progn` の問題を回避できる。

---

## 核心的な設計変更

**現状**: マクロが `write-string` で stdout に直接出力 → 戻り値は nil
**提案**: マクロが S 式の AST を返す → `emit` 関数で Verilog テキストに変換

```
現状:  (v_module ...) → stdout に直接 "module ..." → nil
提案:  (v_module ...) → (:module name params body...) → emit → "module ..."
```

## なぜ S 式 AST か

### 既に使われているパターン

axis.hvl や psdma.hvl では**関数がインターフェース定義を S 式で返す**パターンを使っている:

```lisp
;; axis.hvl — これは事実上の AST
(defun make-axis-sub-if (axi-name tdata-width)
  (copy-tree
    `((input ,(symcat axi-name 'tvalid))
      (output ,(symcat axi-name 'tready))
      (input (,tdata-width ,(symcat axi-name 'tdata))))))

;; psdma.hvl — インターフェースを合成して使っている
(defmacro make-psdma-module (module-name ...)
  `(module ,module-name
    ((input clk)
     ,@(make-psdma-read-if 'read_ data-width addr-width)   ;; ← S式を splice
     ,@(make-psdma-write-if 'write_ data-width addr-width)) ;; ← S式を splice
    ...))
```

これを**モジュール全体**に一般化するのが本提案。

### Lisp の自然な拡張

Lisp のマクロ展開 + eval がそのまま AST 構築になる。新しい構文や仕組みは不要。

```lisp
;; ユーザが書くコード（変更なし）:
(module my_mod ((input clk) (output (8 data_o)))
  (reg (8 data_r))
  (always ((posedge clk))
    (<=# data_o data_r)))

;; マクロ展開 → eval の結果が AST:
(:module my_mod ((input clk) (output (8 data_o)))
  (:reg (8 data_r))
  (:always ((posedge clk))
    (:<= data_o data_r)))
```

## 具体的な AST 設計

### AST ノード定義

キーワードシンボルをタグとする S 式リスト。
study-cps の metadata-tags パターンに倣い、先頭タグでノード種別を識別する。

```lisp
;;=== トップレベル ===
(:module name params stmt...)         ;; module 定義
(:primitive name params table-rows)   ;; primitive 定義

;;=== 宣言 ===
(:wire decl...)       ;; wire a / (8 a) / ...
(:reg decl...)        ;; reg a / (8 a) / (8 (4 mem)) / ...
(:reg= name value)    ;; reg x = 0
(:integer decl...)    ;; integer i
(:parameter name val) ;; parameter N = 8

;;=== 文 ===
(:always sensitivity stmt...)   ;; always @(posedge clk)
(:initial stmt...)              ;; initial begin...end
(:if cond then &optional else)  ;; if/else
(:cond (cond stmt...)...)       ;; if/else-if/else chain
(:case type val (match stmt...)...)  ;; case/casex/casez
(:for init cond step stmt...)   ;; for loop
(:assign lvalue expr)           ;; assign x = y
(:= lvalue expr)                ;; blocking: x = y
(:<= lvalue expr)               ;; non-blocking: x <= y
(:fork stmt...)                 ;; fork/join
(:comment word...)              ;; /* ... */

;;=== 式 ===
(:+ a b) (:- a b) (:* a b) ...  ;; 二項演算
(:! a) (:~ a)                    ;; 単項演算
(:? cond yes no)                 ;; 三項演算
(:ref name index...)             ;; ビット選択 a[n] a[n:m]
(:cat expr...)                   ;; 連結 {a, b}
(:cat* n expr)                   ;; 複製 {n{expr}}
(:d size value) (:h size value)  ;; リテラル 8'd255, 8'hFF

;;=== インスタンス ===
(:inst module-name instance-name arg...)  ;; モジュールインスタンス

;;=== システムタスク ===
(:$ name arg...)                 ;; $display, $finish, etc.
(:delay ticks)                   ;; #100

;;=== SystemVerilog 拡張（Phase 2）===
(:logic decl...)                 ;; logic 型
(:always-ff sensitivity stmt...) ;; always_ff
(:always-comb stmt...)           ;; always_comb
(:interface name port...)        ;; SV interface
(:modport name signal...)        ;; modport
(:struct name field...)          ;; struct
(:enum name value...)            ;; enum
(:package name body...)          ;; package
(:assert expr)                   ;; assertion
```

### マクロの実装方法

**原則**: アトム（シンボル・数値）はシグナル名/リテラルとして quote。リスト（S 式）はサブ式として eval。

```lisp
;; ヘルパー: マクロ引数を AST 用に変換
;; アトムは quote（シグナル名として保持）、リストは eval（サブ式を展開）
(defun ast-arg (x)
  (if (atom x) `',x x))

;;--- 宣言マクロ ---
(defmacro v_reg (&rest names)
  `(list :reg ,@(mapcar (lambda (n) `',n) names)))
;; (v_reg counter)     → (:reg counter)
;; (v_reg (8 data) a)  → (:reg (8 data) a)

(defmacro v_wire (&rest args)
  `(list :wire ,@(mapcar (lambda (a) `',a) args)))

;;--- 文マクロ ---
(defmacro v_module (name parameters &rest statements)
  `(list :module ',name ',parameters ,@statements))
;; statements 内の各フォームは eval されて AST ノードを返す

(defmacro v_always (signals &rest body)
  `(list :always ',signals ,@body))

(defmacro v_if (condition yes-body &optional no-body)
  `(list :if ,(ast-arg condition) ,yes-body
         ,@(when no-body (list no-body))))

(defmacro v_= (lvalue expr)
  `(list := ,(ast-arg lvalue) ,(ast-arg expr)))

(defmacro v_<=# (lvalue expr &optional delay)
  `(list :<= ,(ast-arg lvalue) ,(ast-arg expr)
         ,@(when delay `(:delay ,delay))))

;;--- 式マクロ ---
(defmacro v_+ (&rest args)
  `(list :+ ,@(mapcar #'ast-arg args)))
;; (v_+ counter 1)          → (:+ counter 1)
;; (v_+ counter (v_* a b))  → (:+ counter (:* a b))

(defmacro v_== (&rest args)
  `(list :== ,@(mapcar #'ast-arg args)))

(defmacro v_ref (name &rest indices)
  `(list :ref ,(ast-arg name) ,@(mapcar #'ast-arg indices)))

(defmacro v_cat (&rest args)
  `(list :cat ,@(mapcar #'ast-arg args)))

;;--- システムタスク ---
(defmacro v_display (&rest args)
  `(list :$ 'display ,@(mapcar #'ast-arg args)))

(defmacro v_finish ()
  '(list :$ 'finish))

;;--- インスタンス（make-named-module で動的生成）---
;; (my_adder clk a b sum) → (:inst my_adder anon_0 clk a b sum)
(defun make-named-module (macro-name verilog-name)
  (eval
    `(defmacro ,macro-name (&rest args)
       (let ((instance-name (gen-instance-name)))
         `(list :inst ',',verilog-name ',instance-name
                ,@(mapcar #'ast-arg args))))))
```

### 展開の具体例

```lisp
;; ユーザコード（マングリング後）:
(v_module my_mod ((input clk) (output (8 data_o)))
  (v_reg (8 data_r))
  (v_always ((posedge clk))
    (v_<=# data_o data_r)))

;; マクロ展開:
(list :module 'my_mod '((input clk) (output (8 data_o)))
  (list :reg '(8 data_r))
  (list :always '((posedge clk))
    (list :<= 'data_o 'data_r)))

;; eval 結果（= AST）:
(:module my_mod ((input clk) (output (8 data_o)))
  (:reg (8 data_r))
  (:always ((posedge clk))
    (:<= data_o data_r)))
```

## emit: AST → テキスト変換

study-cps の walk-cps パターンに倣い、`case` ディスパッチで AST を走査する。

```lisp
;;=============================================================
;; emit: AST ノードを Verilog テキストに変換
;; study-cps の visitor パターン（軽量版: 関数ベース）を採用
;;=============================================================
(defvar *emit-stream* *standard-output*)
(defvar *emit-indent* 0)

(defun emit (node &optional (stream *standard-output*))
  "AST ノードを Verilog テキストとして出力"
  (let ((*emit-stream* stream))
    (emit-node node)))

(defun emit-node (node)
  "ノード種別に応じてディスパッチ"
  (cond
    ((null node) nil)
    ((atom node) (format *emit-stream* "~a" node))  ;; シンボル/数値
    ((keywordp (car node))
     (case (car node)
       (:module    (emit-module node))
       (:wire      (emit-wire node))
       (:reg       (emit-reg node))
       (:reg=      (emit-reg= node))
       (:always    (emit-always node))
       (:initial   (emit-initial node))
       (:if        (emit-if node))
       (:cond      (emit-cond node))
       (:case      (emit-case node))
       (:assign    (emit-assign node))
       (:=         (emit-blocking node))
       (:<=        (emit-nonblocking node))
       (:inst      (emit-instance node))
       (:$         (emit-systask node))
       (:delay     (emit-delay node))
       (:ref       (emit-ref node))
       (:cat       (emit-cat node))
       (:?         (emit-ternary node))
       (:comment   (emit-comment node))
       ;; 二項演算: (:+ a b), (:- a b), etc.
       (otherwise  (emit-binop node))))
    (t (error "Unknown AST node: ~a" node))))

;; --- 各 emit 関数 ---

(defun emit-indent ()
  (dotimes (i *emit-indent*) (write-char #\Space *emit-stream*)))

(defun emit-newline ()
  (terpri *emit-stream*)
  (emit-indent))

(defun emit-module (node)
  ;; (:module name params stmt...)
  (let ((name (second node))
        (params (third node))
        (stmts (cdddr node)))
    (emit-newline)
    (format *emit-stream* "module ~a" name)
    (emit-param-list params)
    (let ((*emit-indent* (+ *emit-indent* 4)))
      (dolist (stmt stmts)
        (emit-node stmt)))
    (emit-newline)
    (write-string "endmodule" *emit-stream*)
    (terpri *emit-stream*)))

(defun emit-always (node)
  ;; (:always sensitivity stmt...)
  (let ((signals (second node))
        (body (cddr node)))
    (emit-newline)
    (write-string "always" *emit-stream*)
    (when signals
      (let ((*emit-indent* (+ *emit-indent* 4)))
        (emit-newline)
        (format *emit-stream* "@(~{~a~^ or ~})" ; sensitivity list
                (mapcar #'emit-sensitivity-item signals)))
      (let ((*emit-indent* (- *emit-indent* 0))))  ;; restore
    (emit-newline)
    (write-string "begin" *emit-stream*)
    (let ((*emit-indent* (+ *emit-indent* 4)))
      (dolist (stmt body)
        (emit-node stmt)))
    (emit-newline)
    (write-string "end" *emit-stream*)))

(defun emit-if (node)
  ;; (:if condition then &optional else)
  (emit-newline)
  (write-string "if (" *emit-stream*)
  (emit-node (second node))   ;; condition (expression)
  (write-string ")" *emit-stream*)
  (emit-newline)
  (write-string "begin" *emit-stream*)
  (let ((*emit-indent* (+ *emit-indent* 4)))
    (emit-node (third node)))
  (emit-newline)
  (write-string "end" *emit-stream*)
  (when (fourth node)
    (emit-newline)
    (write-string "else" *emit-stream*)
    (emit-newline)
    (write-string "begin" *emit-stream*)
    (let ((*emit-indent* (+ *emit-indent* 4)))
      (emit-node (fourth node)))
    (emit-newline)
    (write-string "end" *emit-stream*)))

(defun emit-binop (node)
  ;; (:+ a b), (:- a b), (:== a b), etc.
  (let ((op (string-downcase (symbol-name (car node))))
        (args (cdr node)))
    ;; キーワードから演算子文字列を取得
    (write-string "(" *emit-stream*)
    (emit-node (first args))
    (dolist (arg (rest args))
      (format *emit-stream* " ~a " op)
      (emit-node arg))
    (write-string ")" *emit-stream*)))

(defun emit-blocking (node)
  ;; (:= lvalue expr)
  (emit-newline)
  (emit-node (second node))
  (write-string " = " *emit-stream*)
  (emit-node (third node))
  (write-string ";" *emit-stream*))

(defun emit-nonblocking (node)
  ;; (:<= lvalue expr)
  (emit-newline)
  (emit-node (second node))
  (write-string " <= " *emit-stream*)
  (emit-node (third node))
  (write-string ";" *emit-stream*))
```

### SystemVerilog バックエンド

emit のバリエーションとして実装する。polyphony が IR → AHDL → Verilog と二層にしているのと同様、
**同じ AST に対して異なる emit を適用する**だけで出力形式を切り替えられる。

```lisp
(defun emit-sv (node &optional (stream *standard-output*))
  "AST ノードを SystemVerilog テキストとして出力"
  (let ((*emit-stream* stream)
        (*emit-backend* :systemverilog))  ;; バックエンド識別
    (emit-node-sv node)))

(defun emit-node-sv (node)
  (cond
    ((null node) nil)
    ((atom node) (format *emit-stream* "~a" node))
    ((keywordp (car node))
     (case (car node)
       ;; Verilog と異なる部分だけオーバーライド
       (:wire      (emit-sv-logic node))      ;; wire → logic
       (:reg       (emit-sv-logic node))      ;; reg  → logic
       (:always    (emit-sv-always node))     ;; always → always_ff/always_comb
       (:interface (emit-sv-interface node))  ;; SV 固有
       (:struct    (emit-sv-struct node))     ;; SV 固有
       (:enum      (emit-sv-enum node))       ;; SV 固有
       (:assert    (emit-sv-assert node))     ;; SV 固有
       ;; それ以外は Verilog と同じ
       (otherwise  (emit-node node))))
    (t (emit-node node))))  ;; fallback to Verilog emit

(defun emit-sv-logic (node)
  ;; :wire/:reg をまとめて logic として出力
  ;; (:wire a (8 b)) → "logic a;" "logic [7:0] b;"
  (dolist (decl (cdr node))
    (emit-newline)
    (if (atom decl)
      (format *emit-stream* "logic ~a;" decl)
      (format *emit-stream* "logic [~a:0] ~a;"
              (1- (car decl)) (cadr decl)))))

(defun emit-sv-always (node)
  ;; sensitivity list を解析して always_ff / always_comb を自動判定
  (let ((signals (second node)))
    (emit-newline)
    (cond
      ;; @(posedge ...) or @(negedge ...) → always_ff
      ((and signals (member 'posedge (flatten signals)))
       (write-string "always_ff " *emit-stream*))
      ;; @(*) or sensitivity なし → always_comb
      ((or (null signals) (equal signals '(*)))
       (write-string "always_comb " *emit-stream*))
      (t
       (write-string "always " *emit-stream*)))
    ;; 以下同様に emit
    ...))
```

### Verilog → SystemVerilog 自動変換

AST レベルで変換関数を書ける:

```lisp
(defun ast-verilog->sv (ast)
  "Verilog AST を SystemVerilog AST に変換"
  (if (atom ast) ast
    (case (car ast)
      ;; wire/reg → logic
      (:wire `(:logic ,@(cdr ast)))
      (:reg  `(:logic ,@(cdr ast)))
      ;; always の自動分類
      (:always
        (let ((sens (second ast))
              (body (cddr ast)))
          (cond
            ((has-posedge-p sens) `(:always-ff ,sens ,@(mapcar #'ast-verilog->sv body)))
            ((combinational-p sens) `(:always-comb ,@(mapcar #'ast-verilog->sv body)))
            (t `(:always ,sens ,@(mapcar #'ast-verilog->sv body))))))
      ;; 再帰的に変換
      (otherwise (mapcar #'ast-verilog->sv ast)))))
```

## AST 解析パス

study-cps のパイプラインパターンを採用。各パスは AST を受け取り AST（または解析結果）を返す。

```lisp
;;=============================================================
;; パイプライン定義
;; study-cps の pipeline.md パターン
;;=============================================================
(defparameter *verilisp-pipeline*
  '(collect-declarations    ;; 宣言を収集してシンボルテーブル構築
    check-port-connections  ;; ポート接続の整合性チェック
    check-widths            ;; ビット幅の一致検証
    ;; ... 将来: 最適化パス
    ))

(defun run-pipeline (ast &optional (passes *verilisp-pipeline*))
  "AST にパスを順次適用"
  (if (null passes) ast
    (let ((result (funcall (car passes) ast)))
      (run-pipeline result (cdr passes)))))
```

### パス例: 宣言収集

study-cps の environment-stack パターン。cons ベースのスコープチェーン。

```lisp
;; 環境: ((scope-entry...) . parent-env)
(defun make-env (&optional parent) (cons nil parent))

(defun env-add (env name &rest props)
  (push (cons name props) (car env))
  env)

(defun env-lookup (env name)
  (if (null env) nil
    (or (assoc name (car env))
        (env-lookup (cdr env) name))))

;; 宣言収集パス
(defun collect-declarations (ast)
  "AST を走査して全モジュールのシンボルテーブルを構築"
  (when (and (listp ast) (eq (car ast) :module))
    (let ((env (make-env))
          (params (third ast))
          (body (cdddr ast)))
      ;; ポート宣言を登録
      (dolist (param-set params)
        (let ((dir (car param-set)))  ;; input/output
          (dolist (p (cdr param-set))
            (if (atom p)
              (env-add env p :dir dir :width 1)
              (env-add env (cadr p) :dir dir :width (car p))))))
      ;; body の宣言を登録
      (dolist (stmt body)
        (when (listp stmt)
          (case (car stmt)
            (:wire (register-wire-decls env stmt))
            (:reg  (register-reg-decls env stmt))
            (:reg= (register-reg=-decls env stmt)))))
      ;; env を AST にメタデータとして付与
      (list :module (second ast) (third ast)
            :env env
            :body (cdddr ast)))))
```

### パス例: ポート接続チェック

```lisp
(defun check-port-connections (ast)
  "モジュールインスタンスのポート数・幅をチェック"
  (walk-ast ast
    (lambda (node)
      (when (and (listp node) (eq (car node) :inst))
        (let* ((mod-name (second node))
               (mod-def (lookup-module-def mod-name))
               (expected-ports (count-ports mod-def))
               (actual-args (length (cdddr node))))
          (unless (= expected-ports actual-args)
            (warn "~a: expected ~a ports, got ~a"
                  mod-name expected-ports actual-args))))))
  ast)  ;; AST をそのまま返す（チェックのみ）
```

### AST ウォーカー

study-cps の visitor パターン（軽量版）。

```lisp
(defun walk-ast (ast fn)
  "AST を再帰的に走査し、各ノードに fn を適用"
  (when ast
    (funcall fn ast)
    (when (listp ast)
      (dolist (child (cdr ast))
        (when (and (listp child) (keywordp (car child)))
          (walk-ast child fn))))))

(defun transform-ast (ast fn)
  "AST を再帰的に走査し、fn で変換した新しい AST を返す"
  (if (atom ast) ast
    (let ((transformed (funcall fn ast)))
      (if (not (eq transformed ast))
        transformed  ;; fn が変換した
        ;; 子ノードを再帰的に変換
        (mapcar (lambda (child) (transform-ast child fn)) ast)))))
```

## マクロ拡張の使用例

AST 化によって可能になるマクロパターンの具体例。

### 例 1: インターフェースの自動反転（if-utils の改善版）

現状の `make-flipped-if` は macroexpand-1 でパラメータだけ取れるハック。
AST があればモジュール全体を操作できる:

```lisp
(defun flip-ports (module-ast)
  "モジュールの input/output を反転した新しい AST を返す"
  (let ((name (second module-ast))
        (params (third module-ast))
        (body (cdddr module-ast)))
    `(:module ,(symcat name '_flipped)
      ,(mapcar (lambda (param-set)
                 (cons (case (car param-set)
                         (input 'output)
                         (output 'input)
                         (t (car param-set)))
                       (cdr param-set)))
               params)
      ,@body)))

;; 使い方:
(emit (flip-ports (v_module my_mod ((input clk) (output (8 data))) ...)))
```

### 例 2: パイプラインステージの自動挿入

```lisp
(defun add-pipeline-reg (module-ast &key (stages 1))
  "出力ポートにパイプラインレジスタを挿入"
  (let* ((params (third module-ast))
         (outputs (remove-if-not (lambda (p) (eq (car p) 'output)) params))
         (new-regs (mapcar (lambda (o)
                             `(:reg ,@(cdr o) ,(symcat (get-name o) '_pipe)))
                           outputs))
         (new-assigns (mapcar (lambda (o)
                                `(:assign ,(get-name o) ,(symcat (get-name o) '_pipe)))
                              outputs)))
    ;; body にレジスタと assign を追加
    `(:module ,(second module-ast) ,params
      ,@new-regs ,@new-assigns ,@(cdddr module-ast))))
```

### 例 3: テストベンチ自動生成

```lisp
(defun gen-testbench (module-ast)
  "モジュール定義からテストベンチの骨格を生成"
  (let* ((name (second module-ast))
         (params (third module-ast))
         (clk-ports (find-clk-ports params))
         (wire-decls (mapcar (lambda (p)
                               `(:wire ,@(cdr p)))
                             params)))
    `(:module ,(symcat 'tb_ name) ()
      ,@wire-decls
      (:inst ,name ,(symcat name '_dut)
             ,@(mapcar #'get-port-name params))
      ,@(when clk-ports
          (list `(:initial
                   (:$ 'dumpfile ,(format nil "~a.vcd" name))
                   (:$ 'dumpvars 0 ,(symcat 'tb_ name))
                   ;; TODO: ユーザがテストシナリオを書く
                   (:delay 1000 :pass)
                   (:$ 'finish)))))))
```

### 例 4: AXI-Stream インターフェースの合成

axis.hvl の既存パターンが自然に統合される:

```lisp
;; インターフェース定義を返す関数（既存パターンそのまま）
(defun make-axis-if (name width dir)
  `((,dir ,(symcat name 'tvalid))
    (,(if (eq dir 'output) 'input 'output) ,(symcat name 'tready))
    (,dir (,width ,(symcat name 'tdata)))
    (,dir ,(symcat name 'tuser))
    (,dir ,(symcat name 'tlast))))

;; モジュール定義に splice（既存パターンそのまま動く）
(v_module my_axis_mod
  ((input clk)
   (input rst)
   ,@(make-axis-if 's_ 8 'input)
   ,@(make-axis-if 'm_ 8 'output))
  ...)

;; さらに: 2つのモジュールを AXI-Stream で接続
(defun connect-axis (producer-ast consumer-ast axis-name width)
  "2つのモジュール AST を AXI-Stream で結線したトップモジュールを返す"
  (let ((wires (mapcar (lambda (sig)
                         `(:wire ,(symcat axis-name sig)))
                       '(tvalid tready tdata tuser tlast))))
    `(:module ,(symcat 'top_ axis-name) ((input clk) (input rst))
      ,@wires
      (:inst ,(second producer-ast) producer clk rst
             ,@(mapcar (lambda (sig) (symcat axis-name sig))
                       '(tvalid tready tdata tuser tlast)))
      (:inst ,(second consumer-ast) consumer clk rst
             ,@(mapcar (lambda (sig) (symcat axis-name sig))
                       '(tvalid tready tdata tuser tlast))))))
```

## 名前マングリング改善

### 現状の問題

Python が `(if` → `(v_if` に正規表現で変換。マクロ内で Lisp の `if` を使うには `(l_if` と書く。

### 推奨: CL パッケージによる分離

Python マングリングを不要にする。CLISP のパッケージ機能で Verilog キーワードを Lisp の同名シンボルからシャドウする。

```lisp
(defpackage :verilisp
  (:use :cl)
  (:shadow
    ;; Verilog 構文
    #:if #:case #:module #:wire #:or #:and #:not
    ;; 演算子（必要なもの）
    #:= #:+ #:- #:* #:/))

(in-package :verilisp)

;; Verilog の if
(defmacro if (condition then &optional else)
  `(list :if ,(ast-arg condition) ,then ,@(cl:when else (list else))))

;; マクロ定義内で Lisp の if が必要なとき:
(cl:if (cl:> n 0) ...)      ;; cl: プレフィクスで CL 本来の関数を使う
```

**メリット**:
- Python の正規表現マングリングが不要になる（`v_` も `l_` も不要）
- ユーザは自然な `(if ...)` `(module ...)` を書ける
- マクロ定義者は `cl:if` で Lisp 機能にアクセス

**デメリット**:
- 全マクロ定義内で `cl:if`, `cl:and` 等を使う必要がある
- 既存の `__verilisp__.cl` の大幅書き換えが必要

**移行戦略**:
- Phase 1 では `v_` プレフィクスのまま AST 化を進める
- パッケージ化は Phase 1 完了後、AST の動作確認ができてから着手

## ファイル構成案

```
verilisp/
├── __verilisp__.cl      # 既存（互換用、段階的に縮小）
├── verilisp.py          # 既存（Python ラッパー）
├── src/                 # 新規: AST ベースの実装
│   ├── ast.cl           # AST ノード定義 + ヘルパー
│   ├── emit.cl          # Verilog emit (walk パターン)
│   ├── emit-sv.cl       # SystemVerilog emit
│   ├── analyze.cl       # 解析パス（ポートチェック等）
│   ├── transform.cl     # AST 変換関数（flip-ports 等）
│   ├── pipeline.cl      # パイプライン実行エンジン
│   ├── macros.cl        # v_* マクロ定義（AST 版）
│   ├── compat.cl        # 旧 API 互換レイヤー
│   └── package.cl       # パッケージ定義（Phase 2）
├── lib/                 # ライブラリ（既存 + 整理）
│   ├── if-utils/        # if-utils を正式ライブラリ化
│   ├── axis/            # AXI-Stream ヘルパー
│   └── ...
├── tests/               # 既存テスト + AST テスト追加
└── kage/                # ビルドツール（既存）
```

## 実装ロードマップ

### Phase 0: Python ラッパーの廃止

**最初にやること。** 現状 verilisp.py が担う役割を CL 側に移し、Python 依存をなくす。

verilisp.py の現在の役割:
1. 名前マングリング（正規表現で `(if` → `(v_if` 等）
2. `backquote_let__main__` ラップ
3. CLISP プロセスの起動とパイプ
4. テストランナー
5. コマンドライン引数処理（--dir, --depfile, --mangle 等）

**移行先**:
- 1, 2 → CL 側のリーダーマクロ or パッケージ（後述）、または CL 側で同等の前処理
- 3 → 不要（CL を直接実行）
- 4 → CL 側にテスト関数を実装
- 5 → CL 側でコマンドライン処理

**具体的な手順**:

```lisp
;; verilisp のエントリポイント（シェルスクリプトまたは CL スクリプト）
;; clisp -modern __verilisp__.cl -- input.hvl -o output.v

;; __verilisp__.cl の末尾に追加:
(defun main ()
  (let ((args *args*))  ;; CLISP のコマンドライン引数
    (cond
      ((member "-t" args :test #'string=) (run-tests))
      ((member "--mangle" args :test #'string=) (mangle-mode args))
      (t (compile-files args)))))

(defun compile-files (args)
  (dolist (file (remove-if (lambda (a) (char= (char a 0) #\-)) args))
    (let ((output (make-pathname :type "v" :defaults file)))
      (with-open-file (out output :direction :output)
        (compile-verilisp file out)))))

(defun compile-verilisp (input-file output-stream)
  "ファイルを読み、マングリングし、eval し、出力する"
  (let ((code (read-file-as-string input-file)))
    ;; マングリングを CL 側で実行
    (let ((mangled (mangle-code code)))
      ;; eval + 出力
      (let ((*standard-output* output-stream)
            (*__name__* :__main__))
        (dolist (expr (read-all-from-string mangled))
          (eval expr))))))
```

**マングリングの CL 側実装**:

Python の `mangle()` と同等の処理を CL で書く。
正規表現は cl-ppcre を使うか、シンプルな文字列置換で実装する。

```lisp
(defparameter *names-to-mangle*
  '("if" "module" "always" "wire" "reg" "and" "or" "not" ...))

(defun mangle-code (code-string)
  "Python の mangle() と同等: (if → (v_if 等"
  (let ((result code-string))
    (dolist (name *names-to-mangle*)
      (setf result
        (cl-ppcre:regex-replace-all
          (format nil "\\(~a(?=[\\s\\(\\)])" (cl-ppcre:quote-meta-chars name))
          result
          (format nil "(v_~a" name))))
    result))
```

**完了条件**: `clisp -modern __verilisp__.cl -- input.hvl` で .v が生成できる。
verilisp.py と同じ出力。Python 不要。

### Phase 1a: AST コアの実装

**やること**: AST ノード定義 + emit (Verilog) + 基本マクロの AST 版

1. `src/ast.cl` — AST ヘルパー（`ast-arg`, `walk-ast`, `transform-ast`）
2. `src/emit.cl` — Verilog テキスト出力（全ノードタイプの emit 関数）
3. `src/macros.cl` — `v_module`, `v_reg`, `v_wire`, `v_always`, `v_if`, `v_=`, 演算子等の AST 版
4. テスト: 既存の 12 テストケースが AST 経由で同じ出力を生成することを確認

**完了条件**: `(emit (v_module ...))` が現在の `(v_module ...)` と同じ Verilog を出力する

### Phase 1b: 互換レイヤー + 残りのマクロ

1. `src/compat.cl` — 旧 `v_module` 等が内部で AST を生成 → emit するラッパー
2. 全マクロの AST 版実装（case, for, task, function, $-macros, primitive 等）
3. 既存の examples/ がすべて動くことを確認

**完了条件**: 既存の .hvl ファイルが変更なしで同じ .v を生成する

### Phase 1c: 代入記法の統一 + AST 操作 API

代入の混乱を解消（前述の提案を実装）:

1. `<=` をノンブロッキング代入の正式記法にする（Verilog 準拠）
2. `=<` を「以下」比較の正式記法にする
3. `n=`, `<=#` は互換エイリアスとして残す
4. AST では `:=` と `:<= ` で明確に分離されるため、emit 時に混乱なし

AST 操作:

5. `src/transform.cl` — `flip-ports`, `gen-testbench`, `connect-modules` 等
6. `src/analyze.cl` — `collect-declarations`, `check-port-connections`
7. if-utils.hvl を AST ベースで書き直し、lib/ に移動
8. ドキュメント: マクロ作成ガイド

**完了条件**: if-utils, axis, psdma が AST API で書き直せる

### Phase 2: SystemVerilog + パッケージ化

1. `src/emit-sv.cl` — SystemVerilog バックエンド
2. SV 固有の AST ノード + マクロ（logic, always_ff, interface 等）
3. `ast-verilog->sv` 変換関数
4. パッケージ分離（Python マングリング廃止、`v_` プレフィクス不要に）

### Phase 3: 高度な機能

- 静的解析（ビット幅チェック、未接続ポート検出）
- テストベンチ自動生成
- 複数ファイル出力
- REPL 改善（AST を返す対話モード）

## まとめ

| Phase | 変更 | リスク | 価値 |
|-------|------|--------|------|
| **0** | Python 廃止（CL 単体化） | 中 | 高（依存削減） |
| **1a** | AST コア + emit | 低（新規追加） | 高 |
| **1b** | 互換レイヤー | 中 | 高（移行の要） |
| **1c** | 代入統一 + AST 操作 API | 低〜中 | 高（本提案の核心） |
| **2** | SystemVerilog + パッケージ化 | 中 | 中〜高 |
| **3** | 静的解析等 | 低 | 中 |

**最初の一歩**: Phase 0（Python 廃止）→ CL 単体で .hvl → .v の変換ができる状態にする。
