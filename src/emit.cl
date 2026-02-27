;;;; src/emit.cl — AST emit dispatcher for verilisp Phase 1a
;;;; Converts AST S-expressions to Verilog text output.

;;; ============================================================
;;; Emit infrastructure
;;; ============================================================

(defvar *emit-stream* *standard-output*)
(defvar *emit-indent* 0)

(defun emit-str (s)
  "Write string S to the emit stream."
  (write-string s *emit-stream*))

(defun emit-sym (s)
  "Write symbol, number, or string S to the emit stream."
  (if (and (symbolp s) (search "'" (symbol-name s)))
      (format *emit-stream* "~a" s)
      (write s :stream *emit-stream*)))

(defun emit-nl ()
  "Write a newline to the emit stream."
  (write-char #\Newline *emit-stream*))

(defun emit-indent ()
  "Write current indentation spaces to the emit stream."
  (dotimes (i *emit-indent*)
    (write-char #\Space *emit-stream*)))

(defun emit-nli ()
  "Newline + indent."
  (emit-nl)
  (emit-indent))

(defun emit-++indent ()
  (incf *emit-indent* 4))

(defun emit---indent ()
  (decf *emit-indent* 4))

;;; ============================================================
;;; Emit dispatcher
;;; ============================================================

(defun emit-or-atom (x)
  "If X is an AST node, emit it; if atom, write it as symbol/number."
  (if (ast-node-p x)
      (emit-node x)
      (emit-sym x)))

(defun emit-node (node)
  "Dispatch on the tag of NODE and call the appropriate emitter."
  (let ((tag (ast-tag node)))
    (case tag
      ;; Top-level
      (:module      (emit-module node))
      (:primitive   (emit-primitive node))

      ;; Declarations
      (:wire        (emit-simple-decl "wire" node))
      (:wand        (emit-simple-decl "wand" node))
      (:wor         (emit-simple-decl "wor" node))
      (:trireg      (emit-simple-decl "trireg" node))
      (:reg         (emit-reg-decl node))
      (:reg=        (emit-reg=-decl node))
      (:integer     (emit-integer-decl node))
      (:parameter   (emit-parameter-decl node))

      ;; Statements
      (:always      (emit-always node))
      (:initial     (emit-initial node))
      (:if          (emit-if node))
      (:cond        (emit-cond node))
      (:case        (emit-case "case" node))
      (:casex       (emit-case "casex" node))
      (:casez       (emit-case "casez" node))
      (:for         (emit-for node))
      (:fork        (emit-fork node))
      (:function    (emit-function node))
      (:task        (emit-task node))
      (:table       (emit-table node))

      ;; Assignments
      (:assign      (emit-assign node))
      (:=           (emit-blocking node))
      (:<= (emit-nonblocking node))
      (:<=#         (emit-nb-delayed node))

      ;; Expressions
      (:ref         (emit-ref node))
      (:cat         (emit-cat node))
      (:cat*        (emit-cat* node))
      (:?           (emit-ternary node))
      (:d           (emit-num-literal "d" node))
      (:h           (emit-num-literal "h" node))
      (:b           (emit-num-literal "b" node))
      (:o           (emit-num-literal "o" node))

      ;; Instances
      (:inst        (emit-instance node))

      ;; System tasks / variables
      (:$           (emit-systask node))
      (:$var        (emit-sysvar node))
      (:delay       (emit-delay node))

      ;; Compiler directives
      (:include     (emit-include node))
      (:define      (emit-define node))
      (:timescale   (emit-timescale node))
      (:comment     (emit-comment node))

      ;; Progn (sequence of statements)
      (:progn       (dolist (stmt (cdr node)) (emit-node stmt)))

      ;; Raw text (captured stdout output)
      (:raw-text    (write-string (cadr node) *emit-stream*))

      ;; Operators
      (otherwise
        (if (emit-operator-p tag)
            (emit-operator node)
            (error "Unknown AST node tag: ~a" tag))))))

;;; ============================================================
;;; Operators
;;; ============================================================

(defparameter *keyword-op-table*
  '((:+   . "+")  (:-   . "-")  (:*   . "*")  (:/   . "/")  (:%   . "%")
    (://  . "/")
    (:<   . "<")  (:>   . ">")  (:<=_op . "<=")
    (:<< . "<<") (:>> . ">>") (:>>> . ">>>") (:<<< . "<<<")
    (:== . "==") (:!= . "!=") (:=== . "===") (:!== . "!==")
    (:^  . "^")  (:^~ . "^~") (:~^  . "~^")
    (:>= . ">=")
    (:!  . "!")  (:~  . "~")
    (:~& . "~&") (:& . "&") (:&& . "&&")
    (:\|  . "|")  (:\|\| . "||") (:~\| . "~|")))

(defun emit-operator-p (tag)
  "Return T if TAG is a known operator keyword."
  (not (null (assoc tag *keyword-op-table*))))

(defun keyword-to-op-string (tag)
  "Convert an operator keyword to its Verilog operator string."
  (let ((entry (assoc tag *keyword-op-table*)))
    (if entry
        (cdr entry)
        (error "Unknown operator: ~a" tag))))

(defun emit-operator (node)
  "Emit a binary or unary operator expression."
  (let ((op (keyword-to-op-string (ast-tag node)))
        (args (cdr node)))
    (emit-str "(")
    (if (> (length args) 1)
        ;; Binary
        (progn
          (emit-or-atom (car args))
          (dolist (arg (cdr args))
            (emit-str " ")
            (emit-str op)
            (emit-str " ")
            (emit-or-atom arg)))
        ;; Unary
        (progn
          (emit-str op)
          (emit-or-atom (car args))))
    (emit-str ")")))

;;; ============================================================
;;; Module / Primitive
;;; ============================================================

(defun emit-param-names (params)
  "Emit the port name list: (name1, name2, ...);"
  (emit-str "(")
  (let ((first t))
    (dolist (param-set params)
      (let ((direction (car param-set))
            (ports (cdr param-set)))
        (declare (ignore direction))
        (dolist (port ports)
          (if (atom port)
              (progn
                (unless first (emit-str ", "))
                (setq first nil)
                (emit-sym port))
              ;; Bus: (width name1 name2...)
              (dolist (bus-name (cdr port))
                (unless first (emit-str ", "))
                (setq first nil)
                (emit-sym bus-name)))))))
  (emit-str ");"))

(defun emit-param-declarations (params)
  "Emit port direction declarations (one per line)."
  (dolist (param-set params)
    (let ((direction (car param-set))
          (ports (cdr param-set)))
      (dolist (port ports)
        (if (atom port)
            (progn
              (emit-nli)
              (emit-str (string-downcase (string direction)))
              (emit-str " ")
              (emit-sym port)
              (emit-str ";"))
            ;; Bus port: (width name1 name2...)
            (let ((width (car port)))
              (dolist (bus-name (cdr port))
                (emit-nli)
                (emit-str (string-downcase (string direction)))
                (emit-str " [")
                (emit-width-expr width)
                (emit-str " : 0] ")
                (emit-sym bus-name)
                (emit-str ";"))))))))

(defun emit-width-expr (width)
  "Emit width-1 expression. Width can be a number or AST expression."
  (if (numberp width)
      (emit-sym (- width 1))
      ;; It's an expression: emit (expr - 1)
      (emit-node (list :- width 1))))

(defun emit-module (node)
  "(:module name params stmt...)
   params=nil → no port list (module name;)
   params=:empty → empty port list (module name();)
   params=((dir port...)...) → full port list"
  (let ((name (ast-arg node 0))
        (params (ast-arg node 1))
        (stmts (cdddr node)))
    (emit-nli)
    (emit-str "module ")
    (emit-sym name)
    (cond
      ((eq params :empty)
       ;; Empty port list: module name();
       (emit-str "();")
       (emit-++indent))
      (params
       (progn
         (emit-param-names params)
         (emit-++indent)
         (emit-param-declarations params)))
      (t
       ;; nil params → no port list at all: "module _$_;"
       (emit-str ";")
       (emit-++indent)))
    (dolist (stmt stmts)
      (emit-node stmt))
    (emit---indent)
    (emit-nli)
    (emit-str "endmodule")
    (emit-nli)))

(defun emit-primitive (node)
  "(:primitive name params stmt...)"
  (let ((name (ast-arg node 0))
        (params (ast-arg node 1))
        (stmts (cdddr node)))
    (emit-nli)
    (emit-str "primitive ")
    (emit-sym name)
    (if params
        (progn
          (emit-param-names params)
          (emit-++indent)
          (emit-param-declarations params))
        (progn
          (emit-str ";")
          (emit-++indent)))
    (dolist (stmt stmts)
      (emit-node stmt))
    (emit---indent)
    (emit-nli)
    (emit-str "endprimitive")
    (emit-nli)))

;;; ============================================================
;;; Declarations
;;; ============================================================

(defun emit-simple-decl (type-str node)
  "Emit wire/wand/wor/trireg declarations.
   (:wire decl...) where decl is atom or (width name1 name2...)"
  (dolist (decl (cdr node))
    (if (atom decl)
        (progn
          (emit-nli)
          (emit-str type-str)
          (emit-str " ")
          (emit-sym decl)
          (emit-str ";"))
        ;; Bus: (width name1 name2...)
        (let ((width (car decl)))
          (dolist (name (cdr decl))
            (emit-nli)
            (emit-str type-str)
            (emit-str " [")
            (emit-width-expr width)
            (emit-str " : 0] ")
            (emit-sym name)
            (emit-str ";"))))))

(defun emit-reg-decl (node)
  "Emit reg declarations.
   (:reg decl...) where decl is:
     atom            → reg name;
     (width name)    → reg [w-1:0] name;
     (width (depth name1...)) → reg [w-1:0] name [d-1:0];"
  (dolist (decl (cdr node))
    (if (atom decl)
        (progn
          (emit-nli)
          (emit-str "reg ")
          (emit-sym decl)
          (emit-str ";"))
        ;; (width items...)
        (let ((width (car decl)))
          (dolist (item (cdr decl))
            (if (atom item)
                ;; 1D bus
                (progn
                  (emit-nli)
                  (emit-str "reg [")
                  (emit-width-expr width)
                  (emit-str " : 0] ")
                  (emit-sym item)
                  (emit-str ";"))
                ;; 2D bus: (depth name1 name2...)
                (let ((depth (car item)))
                  (dolist (name (cdr item))
                    (emit-nli)
                    (emit-str "reg [")
                    (emit-width-expr width)
                    (emit-str " : 0] ")
                    (emit-sym name)
                    (emit-str " [")
                    (emit-width-expr depth)
                    (emit-str " : 0];")))))))))

(defun emit-reg=-decl (node)
  "Emit reg= declaration.
   (:reg= name-or-spec value)"
  (let ((name-or-spec (ast-arg node 0))
        (value (ast-arg node 1)))
    (if (atom name-or-spec)
        ;; Simple: reg name = value;
        (progn
          (emit-nli)
          (emit-str "reg ")
          (emit-sym name-or-spec)
          (emit-str " = ")
          (emit-or-atom value)
          (emit-str ";"))
        ;; Bus: (width name) or (width (depth name))
        (let ((width (car name-or-spec))
              (name-part (cadr name-or-spec)))
          (if (atom name-part)
              ;; 1D: reg [w-1:0] name = value;
              (progn
                (emit-nli)
                (emit-str "reg [")
                (emit-width-expr width)
                (emit-str " : 0] ")
                (emit-sym name-part)
                (emit-str " = ")
                (emit-or-atom value)
                (emit-str ";"))
              ;; 2D: reg [w-1:0] name [d-1:0] = '{...};
              (let ((depth (car name-part))
                    (name (cadr name-part)))
                (emit-nli)
                (emit-str "reg [")
                (emit-width-expr width)
                (emit-str " : 0] ")
                (emit-sym name)
                (emit-str " [")
                (emit-width-expr depth)
                (emit-str " : 0] = '{")
                (let ((del " "))
                  (dolist (v value)
                    (emit-str del)
                    (emit-or-atom v)
                    (setq del ", ")))
                (emit-str "};")))))))

(defun emit-integer-decl (node)
  "(:integer decl...) where decl is atom or (size name1 name2...)"
  (dolist (decl (cdr node))
    (if (atom decl)
        (progn
          (emit-nli)
          (emit-str "integer ")
          (emit-sym decl)
          (emit-str ";"))
        ;; (size name1 name2...)
        (let ((size (car decl)))
          (dolist (name (cdr decl))
            (emit-nli)
            (emit-str "integer ")
            (emit-sym name)
            (emit-str " [")
            (emit-width-expr size)
            (emit-str " : 0];"))))))

(defun emit-parameter-decl (node)
  "(:parameter name value)"
  (let ((name (ast-arg node 0))
        (value (ast-arg node 1)))
    (emit-nli)
    (emit-str "parameter ")
    (emit-sym name)
    (emit-str " = ")
    (emit-or-atom value)
    (emit-str ";")))

;;; ============================================================
;;; Control statements
;;; ============================================================

(defun emit-always (node)
  "(:always (sensitivity...) stmt...)"
  (let ((signals (ast-arg node 0))
        (stmts (cddr node)))
    (emit-nli)
    (emit-str "always")
    (when signals
      (emit-++indent)
      (emit-nli)
      (emit-str "@(")
      ;; First signal
      (let ((sig (car signals)))
        (if (atom sig)
            (emit-sym sig)
            (progn
              (emit-sym (car sig))
              (dolist (part (cdr sig))
                (emit-str " ")
                (emit-sym part)))))
      ;; Remaining signals
      (dolist (sig (cdr signals))
        (emit-str " or ")
        (if (atom sig)
            (emit-sym sig)
            (progn
              (emit-sym (car sig))
              (dolist (part (cdr sig))
                (emit-str " ")
                (emit-sym part)))))
      (emit-str ")")
      (emit---indent))
    (emit-nli)
    (emit-str "begin")
    (emit-++indent)
    (dolist (stmt stmts)
      (emit-node stmt))
    (emit---indent)
    (emit-nli)
    (emit-str "end")))

(defun emit-initial (node)
  "(:initial stmt...)"
  (let ((stmts (cdr node)))
    (emit-nli)
    (emit-str "initial")
    (emit-nli)
    (emit-str "begin")
    (emit-++indent)
    (dolist (stmt stmts)
      (emit-node stmt))
    (emit---indent)
    (emit-nli)
    (emit-str "end")))

(defun emit-if (node)
  "(:if cond then &optional else)"
  (let ((cond-expr (ast-arg node 0))
        (then-body (ast-arg node 1))
        (else-body (if (> (length node) 3) (ast-arg node 2) nil)))
    (emit-nli)
    (emit-str "if (")
    (emit-or-atom cond-expr)
    (emit-str ")")
    (emit-nli)
    (emit-str "begin")
    (emit-++indent)
    (emit-node then-body)
    (emit---indent)
    (emit-nli)
    (emit-str "end")
    (when else-body
      (emit-nli)
      (emit-str "else")
      (emit-nli)
      (emit-str "begin")
      (emit-++indent)
      (emit-node else-body)
      (emit---indent)
      (emit-nli)
      (emit-str "end"))))

(defun emit-cond (node)
  "(:cond (cond stmt...)...)"
  (let ((clauses (cdr node))
        (first t))
    (dolist (clause clauses)
      (let ((cond-expr (car clause))
            (stmts (cdr clause)))
        (if first
            (progn
              (emit-nli)
              (emit-str "if (")
              (emit-or-atom cond-expr)
              (emit-str ")")
              (emit-nli)
              (emit-str "begin")
              (emit-++indent)
              (dolist (stmt stmts) (emit-node stmt))
              (emit---indent)
              (emit-nli)
              (emit-str "end")
              (emit-nli)
              (setq first nil))
            (progn
              (emit-str "else")
              (emit-nli)
              (if (not (eq cond-expr 'else))
                  (progn
                    (emit-str "if (")
                    (emit-or-atom cond-expr)
                    (emit-str ")")
                    (emit-nli)))
              (emit-str "begin")
              (emit-++indent)
              (dolist (stmt stmts) (emit-node stmt))
              (emit---indent)
              (emit-nli)
              (emit-str "end")
              (emit-nli)))))))

(defun emit-case (type-str node)
  "Emit case/casex/casez.
   (:<case-type> value (match stmt...)...)"
  (let ((value (ast-arg node 0))
        (cases (cddr node)))
    (emit-nli)
    (emit-str type-str)
    (emit-str "(")
    (emit-or-atom value)
    (emit-str ")")
    (emit-++indent)
    (dolist (case-part cases)
      (emit-nli)
      (let ((match (car case-part))
            (stmts (cdr case-part)))
        ;; Match value(s)
        (if (and (listp match) (not (ast-node-p match)))
            ;; Multiple match values
            (let ((first t))
              (dolist (m match)
                (unless first (emit-str ", "))
                (setq first nil)
                (emit-or-atom m)))
            (emit-or-atom match))
        (emit-str ":")
        ;; If multiple statements, wrap in begin/end
        (let ((multi (> (length stmts) 1)))
          (when multi (emit-str " begin"))
          (emit-++indent)
          (dolist (stmt stmts)
            (emit-node stmt))
          (emit---indent)
          (when multi
            (emit-nli)
            (emit-str "end")))))
    (emit---indent)
    (emit-nli)
    (emit-str "endcase")))

(defun emit-for (node)
  "(:for (var init) cond (var step) stmt...)"
  (let ((init-pair (ast-arg node 0))
        (cond-expr (ast-arg node 1))
        (step-pair (ast-arg node 2))
        (stmts (cddddr node)))
    (emit-nli)
    (emit-str "for (")
    (emit-sym (car init-pair))
    (emit-str " = ")
    (emit-or-atom (cadr init-pair))
    (emit-str "; ")
    (emit-or-atom cond-expr)
    (emit-str "; ")
    (emit-sym (car step-pair))
    (emit-str " = ")
    (emit-or-atom (cadr step-pair))
    (emit-str ")")
    (emit-nli)
    (emit-str "begin")
    (emit-++indent)
    (dolist (stmt stmts)
      (emit-node stmt))
    (emit---indent)
    (emit-nli)
    (emit-str "end")))

(defun emit-fork (node)
  "(:fork stmt...)"
  (emit-nli)
  (emit-str "fork")
  (emit-++indent)
  (dolist (stmt (cdr node))
    (emit-node stmt))
  (emit---indent)
  (emit-nli)
  (emit-str "join"))

;;; ============================================================
;;; Function / Task
;;; ============================================================

(defun emit-function (node)
  "(:function range-or-nil name params stmt...)"
  (let ((range (ast-arg node 0))
        (name (ast-arg node 1))
        (params (ast-arg node 2))
        (stmts (cddddr node)))
    (emit-nli)
    (emit-str "function ")
    (when range
      (emit-str "[")
      (emit-or-atom (car range))
      (emit-str " : ")
      (emit-or-atom (cadr range))
      (emit-str "] "))
    (emit-sym name)
    (emit-str ";")
    (emit-++indent)
    ;; Emit parameter declarations (inputs only, no port list)
    (emit-param-declarations (list (cons 'input params)))
    (emit---indent)
    (emit-nli)
    (emit-str "begin")
    (emit-++indent)
    (dolist (stmt stmts)
      (emit-node stmt))
    (emit---indent)
    (emit-nli)
    (emit-str "end")
    (emit-nli)
    (emit-str "endfunction")))

(defun emit-task (node)
  "(:task name params stmt...)"
  (let ((name (ast-arg node 0))
        (params (ast-arg node 1))
        (stmts (cdddr node)))
    (emit-nli)
    (emit-str "task ")
    (emit-sym name)
    (emit-str ";")
    (emit-++indent)
    (emit-param-declarations params)
    (emit---indent)
    (emit-nli)
    (emit-str "begin")
    (emit-++indent)
    (dolist (stmt stmts)
      (emit-node stmt))
    (emit---indent)
    (emit-nli)
    (emit-str "end")
    (emit-nli)
    (emit-str "endtask")))

;;; ============================================================
;;; Table (for primitives)
;;; ============================================================

(defun emit-table (node)
  "(:table row...) where each row is (inputs... : output)"
  (emit-nli)
  (emit-str "table")
  (emit-++indent)
  (dolist (row (cdr node))
    (emit-nli)
    ;; First element: input(s)
    (let ((inputs (car row)))
      (if (atom inputs)
          (emit-sym inputs)
          (dolist (elem inputs)
            (emit-sym elem)
            (emit-str " "))))
    ;; Remaining: colon-separated outputs
    (dolist (col (cdr row))
      (emit-str ": ")
      (if (atom col)
          (emit-sym col)
          (dolist (elem col)
            (emit-sym elem)
            (emit-str " "))))
    (emit-str ";"))
  (emit---indent)
  (emit-nli)
  (emit-str "endtable"))

;;; ============================================================
;;; Assignments
;;; ============================================================

(defun emit-assign (node)
  "(:assign lvalue expr)"
  (let ((lvalue (ast-arg node 0))
        (expr (ast-arg node 1)))
    (emit-nli)
    (emit-str "assign ")
    (emit-or-atom lvalue)
    (emit-str " = ")
    (emit-or-atom expr)
    (emit-str ";")))

(defun emit-blocking (node)
  "(:= lvalue expr)"
  (let ((lvalue (ast-arg node 0))
        (expr (ast-arg node 1)))
    (emit-nli)
    (emit-sym lvalue)
    (emit-str " = ")
    (emit-or-atom expr)
    (emit-str ";")))

(defun emit-nonblocking (node)
  "(:<= lvalue expr)"
  (let ((lvalue (ast-arg node 0))
        (expr (ast-arg node 1)))
    (emit-nli)
    (emit-sym lvalue)
    (emit-str " <= ")
    (emit-or-atom expr)
    (emit-str ";")))

(defun emit-nb-delayed (node)
  "(:<=# lvalue expr transport inertial)"
  (let ((lvalue (ast-arg node 0))
        (expr (ast-arg node 1))
        (transport (ast-arg node 2))
        (inertial (if (> (length node) 4) (ast-arg node 3) nil)))
    (emit-nli)
    (when inertial
      (format *emit-stream* "#~a " inertial))
    (emit-sym lvalue)
    (emit-str " <= ")
    (when transport
      (format *emit-stream* "#~a " transport))
    (emit-or-atom expr)
    (emit-str ";")))

;;; ============================================================
;;; Expressions
;;; ============================================================

(defun emit-ref (node)
  "(:ref name index...) → name[idx] or name[hi : lo]"
  (let ((name (ast-arg node 0))
        (indices (cddr node)))
    (emit-or-atom name)
    (emit-str "[")
    (emit-or-atom (car indices))
    (dolist (idx (cdr indices))
      (emit-str " : ")
      (emit-or-atom idx))
    (emit-str "]")))

(defun emit-cat (node)
  "(:cat expr...) → {a, b, c}"
  (emit-str "{")
  (emit-or-atom (ast-arg node 0))
  (dolist (arg (cddr node))
    (emit-str ", ")
    (emit-or-atom arg))
  (emit-str "}"))

(defun emit-cat* (node)
  "(:cat* n expr) → {n{expr}}"
  (emit-str "{")
  (emit-or-atom (ast-arg node 0))
  (dolist (arg (cddr node))
    (emit-or-atom arg))
  (emit-str "}"))

(defun emit-ternary (node)
  "(:? cond yes no)"
  (emit-str " ( ( ")
  (emit-or-atom (ast-arg node 0))
  (emit-str " ) ? ( ")
  (emit-or-atom (ast-arg node 1))
  (emit-str " ) : ( ")
  (emit-or-atom (ast-arg node 2))
  (emit-str ") )"))

(defun emit-num-literal (fmt-char node)
  "(:d size num), (:h size num), etc."
  (let ((size (ast-arg node 0))
        (num (ast-arg node 1)))
    (emit-sym size)
    (emit-str "'")
    (emit-str fmt-char)
    (if (numberp num)
        (emit-formatted-num fmt-char size num)
        (format *emit-stream* "~a" num))))

(defun emit-formatted-num (fmt-str size v)
  "Write a formatted number literal value."
  (cond
    ((string= fmt-str "d") (format *emit-stream* "~d" v))
    ((string= fmt-str "b") (format *emit-stream* "~v,'0b" size v))
    ((string= fmt-str "o")
     (multiple-value-bind (a b) (floor size 3)
       (if (= b 0) (format *emit-stream* "~v,'0o" a v)
                    (format *emit-stream* "~o" v))))
    ((string= fmt-str "h")
     (multiple-value-bind (a b) (floor size 4)
       (if (= b 0) (format *emit-stream* "~(~v,'0x~)" a v)
                    (format *emit-stream* "~(~o~)" v))))))

;;; ============================================================
;;; Instances
;;; ============================================================

(defun emit-instance (node)
  "(:inst module-name instance-name arg...)
   arg can be (:named-arg name value) for .name(value) syntax"
  (let ((mod-name (ast-arg node 0))
        (inst-name (ast-arg node 1))
        (args (cdddr node)))
    (emit-nli)
    (emit-sym mod-name)
    (emit-str " ")
    (emit-sym inst-name)
    (emit-str " (")
    (let ((first t))
      (dolist (arg args)
        (unless first (emit-str ", "))
        (setq first nil)
        (if (and (consp arg) (eq (car arg) :named-arg))
            ;; .name(value)
            (progn
              (emit-str ".")
              (emit-sym (cadr arg))
              (emit-str "(")
              (emit-or-atom (caddr arg))
              (emit-str ")"))
            (emit-or-atom arg))))
    (emit-str ");")))

;;; ============================================================
;;; System tasks / variables
;;; ============================================================

(defun emit-systask (node)
  "(:$ name arg...)"
  (let ((name (ast-arg node 0))
        (args (cddr node)))
    (emit-nli)
    (emit-str "$")
    (emit-sym name)
    (when args
      (emit-str "(")
      (emit-or-atom (car args))
      (dolist (arg (cdr args))
        (emit-str ", ")
        (emit-or-atom arg))
      (emit-str ")"))
    (emit-str ";")))

(defun emit-sysvar (node)
  "(:$var name) → $name (no semicolon, used as expression)"
  (emit-str "$")
  (emit-sym (ast-arg node 0)))

;;; ============================================================
;;; Delay
;;; ============================================================

(defun emit-delay (node)
  "(:delay ticks...) — last element can be :stmt to indicate statement context"
  (let* ((args (cdr node))
         (last-arg (car (last args)))
         (is-stmt (or (null last-arg) (eq last-arg :stmt)))
         (ticks (if is-stmt (butlast args) args)))
    (when is-stmt (emit-nli))
    (if (> (length ticks) 1)
        (progn
          (emit-str "#(")
          (emit-or-atom (car ticks))
          (dolist (tick (cdr ticks))
            (emit-str ", ")
            (emit-or-atom tick))
          (emit-str ")"))
        (progn
          (emit-str "#")
          (emit-sym (car ticks))))
    (when is-stmt (emit-str ";"))))

;;; ============================================================
;;; Compiler directives
;;; ============================================================

(defun emit-include (node)
  "(:include filename...)"
  (dolist (filename (cdr node))
    (emit-nli)
    (emit-str "`include ")
    (emit-sym filename)))

(defun emit-define (node)
  "(:define name value)"
  (emit-nli)
  (emit-str "`define ")
  (emit-sym (ast-arg node 0))
  (emit-str " ")
  (emit-sym (ast-arg node 1)))

(defun emit-timescale (node)
  "(:timescale value)"
  (emit-nli)
  (emit-str "`timescale ")
  (emit-str (string (ast-arg node 0))))

(defun emit-comment (node)
  "(:comment word...)"
  (emit-nli)
  (emit-str "/*")
  (emit-++indent)
  (emit-nli)
  (dolist (word (cdr node))
    (if (stringp word)
        (emit-str word)
        (emit-sym word))
    (emit-str " "))
  (emit---indent)
  (emit-nli)
  (emit-str "*/"))

;;; ============================================================
;;; Top-level emit entry point
;;; ============================================================

(defun emit-ast (nodes &optional (stream *standard-output*))
  "Emit a list of AST nodes to STREAM."
  (let ((*emit-stream* stream)
        (*emit-indent* 0))
    (dolist (node nodes)
      (emit-node node))))

(defun emit-ast-to-string (nodes)
  "Emit a list of AST nodes, returning the result as a string."
  (with-output-to-string (s)
    (emit-ast nodes s)))
