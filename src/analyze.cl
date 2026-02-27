;;;; src/analyze.cl — def-use analysis pass for verilisp Phase 2
;;;; Walks AST modules and collects signal definition/use information.
;;;; Uses walk-ast from ast.cl.
;;;;
;;;; Load order: __verilisp__.cl → ast.cl → emit.cl → core.cl
;;;;             → transform.cl → analyze.cl → driver.cl

;;; ============================================================
;;; Signal environment
;;; ============================================================

;;; Each signal entry is a plist:
;;;   (:name name :dir dir :defs (location...) :uses (location...))
;;; where dir is one of: :input :output :inout :internal nil

(defun make-signal-env ()
  "Create an empty signal environment (hash table name→plist)."
  (make-hash-table :test #'equal))

(defun env-get (env name)
  "Get signal entry from ENV by NAME (symbol or string)."
  (gethash (string name) env))

(defun env-set (env name entry)
  "Set signal entry in ENV."
  (setf (gethash (string name) env) entry))

(defun env-ensure (env name)
  "Ensure signal NAME exists in ENV. Return the entry."
  (let ((key (string name)))
    (or (gethash key env)
        (let ((entry (list :name name :dir nil :defs nil :uses nil)))
          (setf (gethash key env) entry)
          entry))))

(defun env-add-def (env name context)
  "Record a definition of NAME in ENV."
  (let ((entry (env-ensure env name)))
    (setf (getf entry :defs)
          (cons context (getf entry :defs)))
    (env-set env name entry)))

(defun env-add-use (env name context)
  "Record a use of NAME in ENV."
  (let ((entry (env-ensure env name)))
    (setf (getf entry :uses)
          (cons context (getf entry :uses)))
    (env-set env name entry)))

(defun env-set-dir (env name dir)
  "Set the direction of NAME in ENV."
  (let ((entry (env-ensure env name)))
    (setf (getf entry :dir) dir)
    (env-set env name entry)))

(defun env-all-signals (env)
  "Return a list of all signal entries in ENV."
  (let ((result nil))
    (maphash (lambda (k v) (declare (ignore k)) (push v result)) env)
    result))

;;; ============================================================
;;; Expression use collector
;;; ============================================================

(defun collect-uses (expr env context)
  "Walk an expression and record all symbol uses in ENV.
   EXPR can be an atom (symbol/number) or an AST node."
  (cond
    ;; Symbol → use
    ((and (symbolp expr) (not (keywordp expr)) expr)
     (env-add-use env expr context))
    ;; Number / keyword / nil → skip
    ((atom expr) nil)
    ;; AST node
    ((ast-node-p expr)
     (let ((tag (ast-tag expr)))
       (case tag
         ;; (:ref name idx...) → name is used, indices may contain uses
         (:ref
          (let ((name (ast-arg expr 0)))
            (when (symbolp name)
              (env-add-use env name context))
            ;; Indices may contain expressions
            (dolist (idx (cddr expr))
              (collect-uses idx env context))))
         ;; (:cat expr...) / (:cat* n expr)
         ((:cat :cat*)
          (dolist (arg (cdr expr))
            (collect-uses arg env context)))
         ;; (:? cond yes no)
         (:?
          (dolist (arg (cdr expr))
            (collect-uses arg env context)))
         ;; (:$var name) → system variable, not a signal
         (:$var nil)
         ;; Numeric literals — no uses
         ((:d :h :b :o) nil)
         ;; (:named-arg port value) — value is used
         (:named-arg
          (collect-uses (caddr expr) env context))
         ;; Operators — recurse into all args
         (otherwise
          (dolist (arg (cdr expr))
            (collect-uses arg env context))))))
    ;; Plain list (shouldn't happen in well-formed AST, but be safe)
    (t nil)))

;;; ============================================================
;;; Statement analyzer
;;; ============================================================

(defun analyze-stmt (stmt env)
  "Analyze a single AST statement, recording defs/uses in ENV."
  (when (ast-node-p stmt)
    (let ((tag (ast-tag stmt)))
      (case tag
        ;; Declarations: define signals
        ((:wire :wand :wor :trireg :supply0 :supply1)
         (dolist (decl (cdr stmt))
           (if (atom decl)
               (env-add-def env decl :decl)
               ;; Bus: (width name1 name2...)
               (dolist (name (cdr decl))
                 (if (atom name)
                     (env-add-def env name :decl)
                     ;; Inner list (depth name1 name2...)
                     (dolist (n (cdr name))
                       (env-add-def env n :decl)))))))

        (:reg
         (dolist (decl (cdr stmt))
           (if (atom decl)
               (env-add-def env decl :decl)
               ;; (width items...)
               (dolist (item (cdr decl))
                 (if (atom item)
                     (env-add-def env item :decl)
                     ;; (depth name1 name2...)
                     (dolist (n (cdr item))
                       (env-add-def env n :decl)))))))

        (:reg=
         (let ((name-or-spec (ast-arg stmt 0))
               (value (ast-arg stmt 1)))
           (if (atom name-or-spec)
               (env-add-def env name-or-spec :decl)
               ;; (width name) or (width (depth name))
               (let ((name-part (cadr name-or-spec)))
                 (if (atom name-part)
                     (env-add-def env name-part :decl)
                     (env-add-def env (cadr name-part) :decl))))
           (collect-uses value env :reg=)))

        (:integer
         (dolist (decl (cdr stmt))
           (if (atom decl)
               (env-add-def env decl :decl)
               (dolist (name (cdr decl))
                 (env-add-def env name :decl)))))

        (:parameter
         (let ((name (ast-arg stmt 0))
               (value (ast-arg stmt 1)))
           (env-add-def env name :decl)
           (collect-uses value env :parameter)))

        ;; Assignments: lvalue is def, expr is use
        (:assign
         (let ((lvalue (ast-arg stmt 0))
               (expr (ast-arg stmt 1)))
           (if (ast-node-p lvalue)
               ;; (:ref name idx...) on LHS
               (when (eq (ast-tag lvalue) :ref)
                 (env-add-def env (ast-arg lvalue 0) :assign)
                 ;; Index expressions are uses
                 (dolist (idx (cddr lvalue))
                   (collect-uses idx env :assign)))
               (env-add-def env lvalue :assign))
           (collect-uses expr env :assign)))

        ((:= :<=)
         (let ((lvalue (ast-arg stmt 0))
               (expr (ast-arg stmt 1)))
           (if (ast-node-p lvalue)
               (when (eq (ast-tag lvalue) :ref)
                 (env-add-def env (ast-arg lvalue 0) tag)
                 (dolist (idx (cddr lvalue))
                   (collect-uses idx env tag)))
               (env-add-def env lvalue tag))
           (collect-uses expr env tag)))

        (:<=#
         (let ((lvalue (ast-arg stmt 0))
               (expr (ast-arg stmt 1)))
           (env-add-def env lvalue :<=)
           (collect-uses expr env :<=)))

        ;; Instance: output ports are defs, input args are uses
        (:inst
         (dolist (arg (cdddr stmt))
           (if (and (consp arg) (eq (car arg) :named-arg))
               ;; Named arg: value is a use
               (collect-uses (caddr arg) env :inst)
               ;; Positional arg: could be def or use depending on port direction.
               ;; Without port info, treat as use (conservative).
               (collect-uses arg env :inst))))

        ;; Control flow: recurse
        (:always
         (let ((signals (ast-arg stmt 0))
               (stmts (cddr stmt)))
           ;; Sensitivity list entries are uses
           (when signals
             (dolist (sig signals)
               (if (atom sig)
                   (env-add-use env sig :sensitivity)
                   ;; (posedge clk) etc.
                   (dolist (part (cdr sig))
                     (env-add-use env part :sensitivity)))))
           (dolist (s stmts) (analyze-stmt s env))))

        (:initial
         (dolist (s (cdr stmt)) (analyze-stmt s env)))

        (:if
         (let ((cond-expr (ast-arg stmt 0))
               (then-body (ast-arg stmt 1))
               (else-body (if (> (length stmt) 3) (ast-arg stmt 2) nil)))
           (collect-uses cond-expr env :if)
           (analyze-stmt then-body env)
           (when else-body (analyze-stmt else-body env))))

        (:cond
         (dolist (clause (cdr stmt))
           (collect-uses (car clause) env :cond)
           (dolist (s (cdr clause)) (analyze-stmt s env))))

        ((:case :casex :casez)
         (collect-uses (ast-arg stmt 0) env :case)
         (dolist (case-part (cddr stmt))
           (dolist (s (cdr case-part)) (analyze-stmt s env))))

        (:for
         (let ((init-pair (ast-arg stmt 0))
               (cond-expr (ast-arg stmt 1))
               (step-pair (ast-arg stmt 2))
               (stmts (cddddr stmt)))
           (collect-uses (cadr init-pair) env :for)
           (collect-uses cond-expr env :for)
           (collect-uses (cadr step-pair) env :for)
           (dolist (s stmts) (analyze-stmt s env))))

        (:fork
         (dolist (s (cdr stmt)) (analyze-stmt s env)))

        (:progn
         (dolist (s (cdr stmt)) (analyze-stmt s env)))

        ;; System tasks: args are uses
        (:$
         (dolist (arg (cddr stmt))
           (collect-uses arg env :systask)))

        ;; Ignore directives, comments, raw-text, etc.
        ((:include :define :timescale :comment :raw-text :delay) nil)

        (otherwise nil)))))

;;; ============================================================
;;; Module-level analysis
;;; ============================================================

(defun register-port-directions (params env)
  "Register port directions from module parameter list into ENV."
  (when (and params (not (eq params :empty)))
    (dolist (param-set params)
      (let ((direction (car param-set))
            (ports (cdr param-set)))
        (let ((dir-kw (intern (string direction) "KEYWORD")))
          (dolist (port ports)
            (if (atom port)
                (progn
                  (env-add-def env port :port)
                  (env-set-dir env port dir-kw))
                ;; Bus: (width name1 name2...)
                (dolist (name (cdr port))
                  (env-add-def env name :port)
                  (env-set-dir env name dir-kw)))))))))

(defun analyze-module (module-ast)
  "Analyze a :module AST node. Returns a signal environment (hash table)."
  (let ((params (ast-arg module-ast 1))
        (stmts (cdddr module-ast))
        (env (make-signal-env)))
    ;; Register port directions
    (register-port-directions params env)
    ;; Analyze all statements
    (dolist (stmt stmts)
      (analyze-stmt stmt env))
    env))

;;; ============================================================
;;; Warning generators
;;; ============================================================

(defun find-dead-signals (env)
  "Find signals that are defined but never used (excluding outputs).
   Returns a list of (name . entry) pairs."
  (let ((result nil))
    (dolist (entry (env-all-signals env))
      (let ((name (getf entry :name))
            (dir (getf entry :dir))
            (uses (getf entry :uses)))
        (when (and (null uses)
                   (not (eq dir :output))
                   (not (eq dir :inout)))
          (push (cons name entry) result))))
    (sort result #'string< :key (lambda (pair) (string (car pair))))))

(defun find-undriven-signals (env)
  "Find signals that are used but never driven (excluding inputs).
   A signal is 'driven' if it has an :assign, :<=, := or :inst def,
   or is an input/inout port.
   :decl and :port defs alone do not count as 'driven'.
   Returns a list of (name . entry) pairs."
  (let ((result nil))
    (dolist (entry (env-all-signals env))
      (let ((name (getf entry :name))
            (dir (getf entry :dir))
            (defs (getf entry :defs)))
        ;; Inputs and inouts are driven externally
        (when (and (not (eq dir :input))
                   (not (eq dir :inout))
                   ;; Only has declaration/port defs, no actual driver
                   (every (lambda (d) (member d '(:port :decl))) defs)
                   ;; Must have uses (otherwise it's just unused, not undriven)
                   (getf entry :uses))
          (push (cons name entry) result))))
    (sort result #'string< :key (lambda (pair) (string (car pair))))))

;;; ============================================================
;;; Top-level analysis entry point
;;; ============================================================

(defun analyze-ast (top-nodes &optional (stream *error-output*))
  "Analyze a list of top-level AST nodes. Print warnings to STREAM.
   Returns the total number of warnings."
  (let ((total-warnings 0))
    (dolist (node top-nodes)
      (when (and (ast-node-p node) (eq (ast-tag node) :module))
        (let* ((name (ast-arg node 0))
               (env (analyze-module node))
               (dead (find-dead-signals env))
               (undriven (find-undriven-signals env)))
          (dolist (pair dead)
            (format stream "Warning: ~a: signal '~a' is defined but never used~%"
                    name (car pair))
            (incf total-warnings))
          (dolist (pair undriven)
            (format stream "Warning: ~a: signal '~a' is used but never driven~%"
                    name (car pair))
            (incf total-warnings)))))
    total-warnings))
