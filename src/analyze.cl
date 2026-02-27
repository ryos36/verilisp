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
;;;   (:name name :dir dir :defs (location...) :uses (location...) :width w)
;;; where dir is one of: :input :output :inout :internal nil
;;; and width is: nil (1-bit scalar), integer (known width), or other (unknown)

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
        (let ((entry (list :name name :dir nil :defs nil :uses nil :width nil)))
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

(defun env-set-width (env name width)
  "Set the width of NAME in ENV. WIDTH is nil (scalar), integer, or other."
  (let ((entry (env-ensure env name)))
    (setf (getf entry :width) width)
    (env-set env name entry)))

(defun env-get-width (env name)
  "Get the width of NAME from ENV."
  (let ((entry (env-get env name)))
    (when entry (getf entry :width))))

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

(defun analyze-stmt (stmt env &optional port-table)
  "Analyze a single AST statement, recording defs/uses in ENV.
   PORT-TABLE, if provided, is used for inst port direction lookup."
  (when (ast-node-p stmt)
    (let ((tag (ast-tag stmt)))
      (case tag
        ;; Declarations: define signals
        ((:wire :wand :wor :trireg :supply0 :supply1)
         (dolist (decl (cdr stmt))
           (if (atom decl)
               (env-add-def env decl :decl)
               ;; Bus: (width name1 name2...)
               (let ((width (car decl)))
                 (dolist (name (cdr decl))
                   (if (atom name)
                       (progn
                         (env-add-def env name :decl)
                         (env-set-width env name width))
                       ;; Inner list (depth name1 name2...)
                       (dolist (n (cdr name))
                         (env-add-def env n :decl)
                         (env-set-width env n width))))))))

        (:reg
         (dolist (decl (cdr stmt))
           (if (atom decl)
               (env-add-def env decl :decl)
               ;; (width items...)
               (let ((width (car decl)))
                 (dolist (item (cdr decl))
                   (if (atom item)
                       (progn
                         (env-add-def env item :decl)
                         (env-set-width env item width))
                       ;; (depth name1 name2...)
                       (dolist (n (cdr item))
                         (env-add-def env n :decl)
                         (env-set-width env n width))))))))

        (:reg=
         (let ((name-or-spec (ast-arg stmt 0))
               (value (ast-arg stmt 1)))
           (if (atom name-or-spec)
               (env-add-def env name-or-spec :decl)
               ;; (width name) or (width (depth name))
               (let ((width (car name-or-spec))
                     (name-part (cadr name-or-spec)))
                 (if (atom name-part)
                     (progn
                       (env-add-def env name-part :decl)
                       (env-set-width env name-part width))
                     (progn
                       (env-add-def env (cadr name-part) :decl)
                       (env-set-width env (cadr name-part) width)))))
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
         (let* ((mod-name (ast-arg stmt 0))
                (args (cdddr stmt))
                (mod-params (when port-table
                              (gethash (string mod-name) port-table))))
           (dolist (arg args)
             (if (and (consp arg) (eq (car arg) :named-arg))
                 ;; Named arg: (:named-arg port-name value)
                 (let* ((port-name (cadr arg))
                        (value (caddr arg))
                        (dir (when mod-params
                               (get-port-direction mod-params port-name))))
                   (if (member dir '(:output :inout))
                       ;; Output/inout port: the connected signal is driven
                       (let ((sig (signal-name-from-expr value)))
                         (when sig
                           (env-add-def env sig :inst))
                         ;; Also record as use for inout
                         (when (eq dir :inout)
                           (collect-uses value env :inst)))
                       ;; Input port or unknown: value is used
                       (collect-uses value env :inst)))
                 ;; Positional arg: without port info, treat as use (conservative)
                 (collect-uses arg env :inst)))))

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
           (dolist (s stmts) (analyze-stmt s env port-table))))

        (:initial
         (dolist (s (cdr stmt)) (analyze-stmt s env port-table)))

        (:if
         (let ((cond-expr (ast-arg stmt 0))
               (then-body (ast-arg stmt 1))
               (else-body (if (> (length stmt) 3) (ast-arg stmt 2) nil)))
           (collect-uses cond-expr env :if)
           (analyze-stmt then-body env port-table)
           (when else-body (analyze-stmt else-body env port-table))))

        (:cond
         (dolist (clause (cdr stmt))
           (collect-uses (car clause) env :cond)
           (dolist (s (cdr clause)) (analyze-stmt s env port-table))))

        ((:case :casex :casez)
         (collect-uses (ast-arg stmt 0) env :case)
         (dolist (case-part (cddr stmt))
           (dolist (s (cdr case-part)) (analyze-stmt s env port-table))))

        (:for
         (let ((init-pair (ast-arg stmt 0))
               (cond-expr (ast-arg stmt 1))
               (step-pair (ast-arg stmt 2))
               (stmts (cddddr stmt)))
           (collect-uses (cadr init-pair) env :for)
           (collect-uses cond-expr env :for)
           (collect-uses (cadr step-pair) env :for)
           (dolist (s stmts) (analyze-stmt s env port-table))))

        (:fork
         (dolist (s (cdr stmt)) (analyze-stmt s env port-table)))

        (:progn
         (dolist (s (cdr stmt)) (analyze-stmt s env port-table)))

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
                (let ((width (car port)))
                  (dolist (name (cdr port))
                    (env-add-def env name :port)
                    (env-set-dir env name dir-kw)
                    (env-set-width env name width))))))))))

(defun analyze-module (module-ast &optional port-table)
  "Analyze a :module AST node. Returns a signal environment (hash table).
   PORT-TABLE, if provided, maps module names to their param lists
   for inst port direction checking."
  (let ((params (ast-arg module-ast 1))
        (stmts (cdddr module-ast))
        (env (make-signal-env)))
    ;; Register port directions
    (register-port-directions params env)
    ;; Analyze all statements (pass port-table for inst handling)
    (dolist (stmt stmts)
      (analyze-stmt stmt env port-table))
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

(defun signal-name-from-expr (expr)
  "Extract the signal name from an expression (atom or :ref node)."
  (cond
    ((and (symbolp expr) (not (keywordp expr))) expr)
    ((and (ast-node-p expr) (eq (ast-tag expr) :ref))
     (ast-arg expr 0))
    (t nil)))

(defun find-width-mismatches (env module-stmts)
  "Find width mismatches in :assign statements.
   Only compares when both LHS and RHS widths are known integers.
   Returns a list of (lhs-name lhs-width rhs-name rhs-width) tuples."
  (let ((result nil))
    (dolist (stmt module-stmts)
      (when (and (ast-node-p stmt) (eq (ast-tag stmt) :assign))
        (let* ((lvalue (ast-arg stmt 0))
               (expr (ast-arg stmt 1))
               (lhs-name (signal-name-from-expr lvalue))
               (rhs-name (signal-name-from-expr expr)))
          (when (and lhs-name rhs-name)
            (let ((lhs-w (env-get-width env lhs-name))
                  (rhs-w (env-get-width env rhs-name)))
              ;; Only compare when both are integers
              (when (and (integerp lhs-w) (integerp rhs-w)
                         (/= lhs-w rhs-w))
                (push (list lhs-name lhs-w rhs-name rhs-w) result)))))))
    (nreverse result)))

;;; ============================================================
;;; Combinational loop detection
;;; ============================================================

(defun build-assign-graph (module-stmts)
  "Build a dependency graph from :assign statements.
   Returns a hash table: lhs-signal-name → list of rhs-signal-names."
  (let ((graph (make-hash-table :test #'equal)))
    (dolist (stmt module-stmts)
      (when (and (ast-node-p stmt) (eq (ast-tag stmt) :assign))
        (let* ((lvalue (ast-arg stmt 0))
               (expr (ast-arg stmt 1))
               (lhs-name (signal-name-from-expr lvalue)))
          (when lhs-name
            (let ((rhs-names nil))
              ;; Collect all signal references from the RHS expression
              (labels ((collect-rhs (e)
                         (cond
                           ((and (symbolp e) (not (keywordp e)) e)
                            (push e rhs-names))
                           ((ast-node-p e)
                            (let ((tag (ast-tag e)))
                              (case tag
                                (:ref (let ((name (ast-arg e 0)))
                                        (when (symbolp name)
                                          (push name rhs-names))))
                                ((:d :h :b :o :$var) nil)
                                (otherwise
                                 (dolist (arg (cdr e))
                                   (collect-rhs arg)))))))))
                (collect-rhs expr))
              (let ((key (string lhs-name)))
                (setf (gethash key graph)
                      (append (gethash key graph nil)
                              (mapcar #'string rhs-names)))))))))
    graph))

(defun find-combinational-loops (graph)
  "Find combinational loops in an assign dependency graph using DFS.
   Returns a list of loop paths (each a list of signal name strings)."
  (let ((visited (make-hash-table :test #'equal))
        (on-stack (make-hash-table :test #'equal))
        (loops nil))
    (labels ((dfs (node path)
               (setf (gethash node visited) t)
               (setf (gethash node on-stack) t)
               (dolist (dep (gethash node graph))
                 (cond
                   ((gethash dep on-stack)
                    ;; Found a back edge → cycle
                    ;; Extract the cycle from path
                    (let* ((cycle-start (position dep path :test #'string=))
                           (cycle (if cycle-start
                                      (append (subseq path cycle-start) (list dep))
                                      (list dep node dep))))
                      (push cycle loops)))
                   ((not (gethash dep visited))
                    (dfs dep (append path (list dep))))))
               (setf (gethash node on-stack) nil)))
      ;; Run DFS from every node in graph (sorted for deterministic output)
      (let ((all-nodes nil))
        (maphash (lambda (node deps)
                   (declare (ignore deps))
                   (push node all-nodes))
                 graph)
        (dolist (node (sort all-nodes #'string<))
          (unless (gethash node visited)
            (dfs node (list node))))))
    (nreverse loops)))

;;; ============================================================
;;; Port connection checking
;;; ============================================================

(defun build-module-port-table (top-nodes)
  "Build a table mapping module-name → params from top-level AST nodes.
   Returns a hash table: string(module-name) → params list."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (node top-nodes)
      (when (and (ast-node-p node)
                 (member (ast-tag node) '(:module :primitive)))
        (let ((name (ast-arg node 0))
              (params (ast-arg node 1)))
          (setf (gethash (string name) table) params))))
    table))

(defun get-port-direction (params port-name)
  "Look up the direction of PORT-NAME in a module's PARAMS list.
   Returns :input, :output, :inout, or nil if not found."
  (when (and params (not (eq params :empty)))
    (dolist (param-set params)
      (let ((direction (car param-set))
            (ports (cdr param-set)))
        (dolist (port ports)
          (if (atom port)
              (when (string= (string port) (string port-name))
                (return-from get-port-direction
                  (intern (string direction) "KEYWORD")))
              ;; Bus: (width name1 name2...)
              (dolist (name (cdr port))
                (when (string= (string name) (string port-name))
                  (return-from get-port-direction
                    (intern (string direction) "KEYWORD"))))))))))

(defun find-port-mismatches (module-stmts port-table)
  "Find port count mismatches in :inst statements.
   Returns a list of (inst-name module-name expected-count actual-count)."
  (let ((result nil))
    (dolist (stmt module-stmts)
      (when (and (ast-node-p stmt) (eq (ast-tag stmt) :inst))
        (let* ((mod-name (ast-arg stmt 0))
               (inst-name (ast-arg stmt 1))
               (args (cdddr stmt))
               (params (gethash (string mod-name) port-table)))
          (when params
            (let ((expected-count 0)
                  (actual-count (length args)))
              ;; Count ports from params
              (when (and params (not (eq params :empty)))
                (dolist (param-set params)
                  (dolist (port (cdr param-set))
                    (if (atom port)
                        (incf expected-count)
                        ;; Bus: (width name1 name2...)
                        (incf expected-count (length (cdr port)))))))
              ;; Only warn if counts differ
              (when (/= expected-count actual-count)
                (push (list inst-name mod-name expected-count actual-count)
                      result)))))))
    (nreverse result)))

;;; ============================================================
;;; Top-level analysis entry point
;;; ============================================================

(defun analyze-ast (top-nodes &optional (stream *error-output*))
  "Analyze a list of top-level AST nodes. Print warnings to STREAM.
   Returns the total number of warnings."
  (let ((total-warnings 0)
        (port-table (build-module-port-table top-nodes)))
    (dolist (node top-nodes)
      (when (and (ast-node-p node) (eq (ast-tag node) :module))
        (let* ((name (ast-arg node 0))
               (stmts (cdddr node))
               (env (analyze-module node port-table))
               (dead (find-dead-signals env))
               (undriven (find-undriven-signals env))
               (width-mismatches (find-width-mismatches env stmts))
               (assign-graph (build-assign-graph stmts))
               (loops (find-combinational-loops assign-graph))
               (port-mismatches (find-port-mismatches stmts port-table)))
          ;; Width mismatch warnings
          (dolist (m width-mismatches)
            (format stream "Warning: ~a: width mismatch in assign: '~a' is ~a bits, '~a' is ~a bits~%"
                    name (first m) (second m) (third m) (fourth m))
            (incf total-warnings))
          ;; Combinational loop warnings
          (dolist (loop-path loops)
            (format stream "Warning: ~a: combinational loop: ~{~a~^ -> ~}~%"
                    name loop-path)
            (incf total-warnings))
          ;; Port mismatch warnings
          (dolist (m port-mismatches)
            (format stream "Warning: ~a: instance '~a' of '~a': expected ~a ports, got ~a~%"
                    name (first m) (second m) (third m) (fourth m))
            (incf total-warnings))
          ;; Dead signal warnings
          (dolist (pair dead)
            (format stream "Warning: ~a: signal '~a' is defined but never used~%"
                    name (car pair))
            (incf total-warnings))
          ;; Undriven signal warnings
          (dolist (pair undriven)
            (format stream "Warning: ~a: signal '~a' is used but never driven~%"
                    name (car pair))
            (incf total-warnings)))))
    total-warnings))
