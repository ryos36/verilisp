;;;; src/transform.cl — Verilog AST → SystemVerilog AST transformation
;;;; Uses transform-ast from ast.cl.
;;;;
;;;; Load order: __verilisp__.cl → ast.cl → emit.cl
;;;;             → core.cl → transform.cl → analyze.cl → driver.cl

;;; ============================================================
;;; Sensitivity list classification
;;; ============================================================

(defun edge-sensitive-p (signals)
  "Return T if any signal in the sensitivity list has posedge/negedge."
  (some (lambda (sig)
          (and (consp sig)
               (member (car sig) '(posedge negedge) :test #'string-equal)))
        signals))

(defun level-sensitive-p (signals)
  "Return T if signals are all plain (level-sensitive, no edge keywords)."
  (and signals
       (every (lambda (sig) (atom sig)) signals)))

;;; ============================================================
;;; Node transformation: Verilog → SystemVerilog
;;; ============================================================

(defun verilog->sv-node (node)
  "Transform a single Verilog AST node to its SV equivalent.
   Used as the callback for transform-ast."
  (let ((tag (ast-tag node)))
    (case tag
      ;; wire → logic
      (:wire
       (cons :logic (cdr node)))

      ;; reg → logic
      (:reg
       (cons :logic (cdr node)))

      ;; reg= → logic=
      (:reg=
       (cons :logic= (cdr node)))

      ;; always → always_ff / always_comb
      (:always
       (let ((signals (ast-arg node 0))
             (stmts (cddr node)))
         (cond
           ;; posedge/negedge → always_ff
           ((edge-sensitive-p signals)
            (list* :always-ff signals stmts))
           ;; Empty sensitivity or level-sensitive → always_comb
           ((or (null signals) (level-sensitive-p signals))
            (list* :always-comb nil stmts))
           ;; Mixed/unknown → keep as :always (shouldn't happen normally)
           (t node))))

      ;; Everything else: unchanged
      (otherwise node))))

(defun verilog->sv (ast-nodes)
  "Transform a list of top-level Verilog AST nodes to SystemVerilog AST nodes."
  (mapcar (lambda (node)
            (if (ast-node-p node)
                (transform-ast #'verilog->sv-node node)
                node))
          ast-nodes))
