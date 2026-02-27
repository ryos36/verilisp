;;;; src/emit-sv.cl — SystemVerilog emitter for verilisp Phase 2
;;;; Handles SV-specific AST nodes; falls back to emit.cl for shared nodes.
;;;;
;;;; Load order: __verilisp__.cl → ast.cl → emit.cl → emit-sv.cl
;;;;             → core.cl → transform.cl → analyze.cl → driver.cl

;;; ============================================================
;;; SV-specific emitters
;;; ============================================================

(defun emit-logic=-decl (node)
  "Emit logic= declaration (SV equivalent of reg=).
   (:logic= name-or-spec value)"
  (let ((name-or-spec (ast-arg node 0))
        (value (ast-arg node 1)))
    (if (atom name-or-spec)
        ;; Simple: logic name = value;
        (progn
          (emit-nli)
          (emit-str "logic ")
          (emit-sym name-or-spec)
          (emit-str " = ")
          (emit-or-atom value)
          (emit-str ";"))
        ;; Bus: (width name) or (width (depth name))
        (let ((width (car name-or-spec))
              (name-part (cadr name-or-spec)))
          (if (atom name-part)
              ;; 1D: logic [w-1:0] name = value;
              (progn
                (emit-nli)
                (emit-str "logic [")
                (emit-width-expr width)
                (emit-str " : 0] ")
                (emit-sym name-part)
                (emit-str " = ")
                (emit-or-atom value)
                (emit-str ";"))
              ;; 2D: logic [w-1:0] name [d-1:0] = '{...};
              (let ((depth (car name-part))
                    (name (cadr name-part)))
                (emit-nli)
                (emit-str "logic [")
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

(defun emit-always-ff (node)
  "(:always-ff (sensitivity...) stmt...)
   Emits: always_ff @(posedge clk) begin ... end"
  (let ((signals (cadr node))
        (stmts (cddr node)))
    (emit-nli)
    (emit-str "always_ff")
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
      (emit-node-sv stmt))
    (emit---indent)
    (emit-nli)
    (emit-str "end")))

(defun emit-always-comb (node)
  "(:always-comb nil stmt...)
   Emits: always_comb begin ... end"
  (let ((stmts (cddr node)))
    (emit-nli)
    (emit-str "always_comb")
    (emit-nli)
    (emit-str "begin")
    (emit-++indent)
    (dolist (stmt stmts)
      (emit-node-sv stmt))
    (emit---indent)
    (emit-nli)
    (emit-str "end")))

;;; ============================================================
;;; SV emit dispatcher
;;; ============================================================

(defun emit-or-atom-sv (x)
  "If X is an AST node, emit it via SV dispatcher; if atom, write it."
  (if (ast-node-p x)
      (emit-node-sv x)
      (emit-sym x)))

(defun emit-node-sv (node)
  "Dispatch on AST tag, handling SV-specific nodes first,
   falling back to the Verilog emitter for shared nodes."
  (let ((tag (ast-tag node)))
    (case tag
      ;; SV-specific nodes
      (:logic      (emit-simple-decl "logic" node))
      (:logic=     (emit-logic=-decl node))
      (:always-ff  (emit-always-ff node))
      (:always-comb (emit-always-comb node))

      ;; Module: need SV-aware recursion for body
      (:module     (emit-module-sv node))

      ;; Fall back to Verilog emitter for everything else
      (otherwise   (emit-node node)))))

(defun emit-module-sv (node)
  "Emit module using SV dispatcher for body statements."
  (let ((name (ast-arg node 0))
        (params (ast-arg node 1))
        (stmts (cdddr node)))
    (emit-nli)
    (emit-str "module ")
    (emit-sym name)
    (cond
      ((eq params :empty)
       (emit-str "();")
       (emit-++indent))
      (params
       (progn
         (emit-param-names params)
         (emit-++indent)
         (emit-param-declarations params)))
      (t
       (emit-str ";")
       (emit-++indent)))
    (dolist (stmt stmts)
      (emit-node-sv stmt))
    (emit---indent)
    (emit-nli)
    (emit-str "endmodule")
    (emit-nli)))

;;; ============================================================
;;; Top-level SV emit entry point
;;; ============================================================

(defun emit-ast-sv (nodes &optional (stream *standard-output*))
  "Emit a list of AST nodes as SystemVerilog to STREAM."
  (let ((*emit-stream* stream)
        (*emit-indent* 0))
    (dolist (node nodes)
      (emit-node-sv node))))

(defun emit-ast-sv-to-string (nodes)
  "Emit a list of AST nodes as SystemVerilog, returning the result as a string."
  (with-output-to-string (s)
    (emit-ast-sv nodes s)))
