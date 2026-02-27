;;;; src/core.cl — AST-returning v_* macro redefinitions for Phase 1a
;;;; Each v_* macro is redefined to return an AST S-expression (keyword-tagged list)
;;;; instead of writing Verilog text to stdout.
;;;;
;;;; Load order: __verilisp__.cl → ast.cl → emit.cl → core.cl → driver.cl

;;; ============================================================
;;; Helpers
;;; ============================================================

(defun ast-eval (form)
  "Evaluate FORM and return an AST value.
   - Atoms (symbols/numbers) are returned as-is.
   - Lists are evaluated. If the result is non-nil, return it.
   - If eval returns nil, capture any stdout output as a :raw-text node."
  (if (atom form)
      form
      (let* ((stdout-capture (make-string-output-stream))
             (result (let ((*standard-output* stdout-capture))
                       (eval form)))
             (captured (get-output-stream-string stdout-capture)))
        (cond
          (result result)
          ((> (length captured) 0) (list :raw-text captured))
          (t nil)))))

(defun make-ast-arglist (args)
  "Wrap each arg for use inside a macro expansion:
   atoms get quoted, forms get wrapped in ast-eval."
  (mapcar (lambda (arg)
            (if (atom arg)
                `',arg
                `(ast-eval ',arg)))
          args))

;;; ============================================================
;;; Pattern A — Data-only declarations (all args are atoms)
;;; ============================================================

;; Width in bus specs like ((+ 3 4) name) may contain v_* expressions
;; after mangling. Process specs to evaluate width expressions.
(defun process-decl-spec (spec)
  "Process a declarator spec: evaluate width expressions if needed.
   atom → quoted atom
   (width items...) → evaluate width if it's an expression"
  (if (atom spec)
      `',spec
      ;; It's a list: (width items...)
      ;; Width is (car spec), items are (cdr spec)
      (let ((width (car spec))
            (items (cdr spec)))
        (if (atom width)
            ;; Simple number/symbol width — quote the whole spec
            `',spec
            ;; Width is an expression — need to eval it
            `(cons (ast-eval ',width)
                   ',(mapcar (lambda (item)
                               (if (atom item) item
                                   ;; Inner list like (4 mem) — depth spec
                                   ;; depth may also be expression
                                   (if (atom (car item)) item
                                       (cons (list 'ast-eval (list 'quote (car item)))
                                             (cdr item)))))
                             items))))))

;; v_reg, v_wire, v_wand, v_wor, v_trireg, v_integer: args are literal specs
(defmacro v_reg (&rest names)
  `(list :reg ,@(mapcar #'process-decl-spec names)))

(foreach name '(wire wand wor trireg)
  (let ((kw (intern (string name) "KEYWORD")))
    (eval
      `(defmacro ,(mangle name) (&rest specs)
         `(list ,',kw ,@(mapcar #'process-decl-spec specs))))))

(defmacro v_integer (&rest specs)
  `(list :integer ,@(mapcar #'process-decl-spec specs)))

(defmacro v_parameter (name value)
  `(list :parameter ',name (ast-eval ',value)))

(defmacro v_reg= (name value)
  `(list :reg= ',name (ast-eval ',value)))

;;; v_progn — collects multiple statement results
(defmacro v_progn (&rest body)
  `(list :progn ,@(mapcar (lambda (s) `(ast-eval ',s)) body)))

;;; ============================================================
;;; Pattern B — Expression arguments (may contain v_* calls)
;;; ============================================================

;;; Operators — bulk generation (Pattern D)
(foreach name_ '(
    + - ~& & && * / % // < > << >> >>> <<< == != === !== ^~ ~^ ^ ! ~
    >= <=
    (%<= "<=")
    (=< "<=")
    (bitwise-or "|")
    (bitwise-nor "~|")
    (bitwise-and "&")
    (bitwise-nand "~&")
    (bitwise-xor "^")
    (bitwise-xnor "~^")
    (bitwise-not "~")
    (logical-or "||")
    (logical-and "&&")
    (logical-not "!")
  )
  (let* ((name (if (atom name_) name_ (car name_)))
         (op name_)
         ;; Determine keyword tag for this operator
         (kw (cond
               ;; <= as operator → :<=_op to distinguish from non-blocking assign
               ((and (atom name) (string= (string name) "<="))
                :<=_op)
               ;; %<= and =< both map to :<=_op
               ((and (listp name_) (string= (string name) "%<="))
                :<=_op)
               ((and (listp name_) (string= (string name) "=<"))
                :<=_op)
               ;; Pipe-containing operators need escaping
               ((string= (string name) "bitwise-or") :\|)
               ((string= (string name) "logical-or") :\|\|)
               ((string= (string name) "bitwise-nor") :~\|)
               (t (intern (string name) "KEYWORD")))))
    (eval
      `(defmacro ,(mangle name) (&rest args)
         `(list ,',kw ,@(make-ast-arglist args))))))

;;; Assignments
(defmacro v_assign (lvalue expr)
  `(list :assign (ast-eval ',lvalue) (ast-eval ',expr)))

(defmacro v_= (name wait-or-value &optional value-or-nil nononli)
  (declare (ignore nononli))
  (if value-or-nil
      ;; delay form: (v_= name delay value) — not used in AST typically
      `(list := ',name (ast-eval ',wait-or-value) (ast-eval ',value-or-nil))
      `(list := ',name (ast-eval ',wait-or-value))))

(defmacro v_n= (name wait-or-value &optional value-or-nil nononli)
  (declare (ignore nononli))
  (if value-or-nil
      `(list :<= ',name (ast-eval ',wait-or-value) (ast-eval ',value-or-nil))
      `(list :<= ',name (ast-eval ',wait-or-value))))

(defmacro v_<=# (lvalue expr &optional
                        (transport-delay *transport-delay*)
                        (inertial-delay *inertial-delay*))
  `(list :<=# ',lvalue (ast-eval ',expr) ,transport-delay ,inertial-delay))

;;; Compound assignments (+=, -= etc.)
(foreach name-symbol '(
    (+= +) (-= -) (*= *) (/= /) (%= %) (^= ^) (&= &)
    (bor= bitwise-or) (lor= logical-or))
  (let ((name (car name-symbol))
        (symbol (cadr name-symbol)))
    (eval
      `(defmacro ,(mangle name) (var expr)
         `(v_= ,var (,(mangle ',symbol) ,var ,expr))))))

;;; Number literals
(foreach fmt '(d b h o)
  (eval
    `(defmacro ,(mangle fmt) (size num)
       `(list ,',(intern (string fmt) "KEYWORD")
              ',size ',num))))

;;; v_ref — vector indexing
(defmacro v_ref (name &rest indices)
  `(list :ref (ast-eval ',name) ,@(make-ast-arglist indices)))

;;; v_cat — concatenation
(defmacro v_cat (&rest args)
  `(list :cat ,@(make-ast-arglist args)))

;;; v_cat* — replication concatenation
(defmacro v_cat* (&rest args)
  `(list :cat* ,@(make-ast-arglist args)))

;;; v_? — ternary operator
(defmacro v_? (condition yes no)
  `(list :? (ast-eval ',condition) (ast-eval ',yes) (ast-eval ',no)))

;;; ============================================================
;;; Pattern C — Body-collecting (control statements)
;;; ============================================================

(defmacro v_always (signals &rest body)
  `(list :always ',signals ,@(mapcar (lambda (s) `(ast-eval ',s)) body)))

(defmacro v_initial (&rest body)
  `(list :initial ,@(mapcar (lambda (s) `(ast-eval ',s)) body)))

(defmacro v_if (condition yes-body &optional no-body)
  (if no-body
      `(list :if (ast-eval ',condition)
             (ast-eval ',yes-body)
             (ast-eval ',no-body))
      `(list :if (ast-eval ',condition)
             (ast-eval ',yes-body))))

(defmacro v_cond (&rest conds)
  `(list :cond
     ,@(mapcar (lambda (c)
                 `(list* (ast-eval ',(car c))
                         (list ,@(mapcar (lambda (s) `(ast-eval ',s)) (cdr c)))))
               conds)))

(foreach case-type '(case casex casez)
  (eval
    `(defmacro ,(mangle case-type) (value &rest cases)
       `(list ,',(intern (string case-type) "KEYWORD")
              (ast-eval ',value)
              ,@(mapcar (lambda (c)
                          `(list* ',(car c)
                                  (list ,@(mapcar (lambda (s) `(ast-eval ',s)) (cdr c)))))
                        cases)))))

(defmacro v_for ((i expr0) expr1 (i_ expr2) &rest body)
  `(list :for
         (list ',i (ast-eval ',expr0))
         (ast-eval ',expr1)
         (list ',i_ (ast-eval ',expr2))
         ,@(mapcar (lambda (s) `(ast-eval ',s)) body)))

(defmacro v_fromto (i start end &rest body)
  `(v_for (,i ,start) (v_< ,i ,end) (,i (v_+ ,i 1)) ,@body))

(defmacro v_fork (&rest body)
  `(list :fork ,@(mapcar (lambda (s) `(ast-eval ',s)) body)))

;;; ============================================================
;;; Pattern E — System tasks ($display, etc.)
;;; ============================================================

(foreach name '(
    setup hold setuphold period width skew recovery
    readmemb readmemh sreadmemb sreadmemh
    display displayh displayb displayo
    fdisplay fdisplayh fdisplayb fdisplayo
    write writeh writeb writeo
    fwrite fwriteh fwriteb fwriteo
    strobe strobeh strobeb strobeo
    fstrobe fstrobeh fstrobeb fstrobeo
    monitor monitorh monitorb monitoro
    fmonitor fmonitorh fmonitorb fmonitoro
    fopen fclose fread
    printtimescale timeformat scale
    stop finish save incsave restart input log nolog key nokey
    scope showscopes showvars countdrivers list
    monitoron monitoroff dumpon dumpoff
    dumpfile dumplimit dumpflush dumpvar dumpvars dumpall
    reset reset_value reset_count
    random getpattern rtoi itor realtobits bitstoreal
  )
  (let ((qname name))
    (eval
      `(defmacro ,(mangle name) (&rest args)
         `(list :$ ',',qname ,@(make-ast-arglist args))))))

;;; $variables ($time, $stime)
(foreach name '(time stime)
  (let ((qname name))
    (eval
      `(defmacro ,(mangle name) ()
         `(list :$var ',',qname)))))

;;; v_realtime — dual-purpose: declaration or $variable
(defmacro v_realtime (&rest names)
  (if names
      ;; Declaration mode: not needed for Phase 1a tests, return raw string
      `(progn
         (check-in-module)
         (format t "~%realtime ~{~a~^, ~};" ',names)
         nil)
      ;; Variable mode: $realtime
      '(list :$var 'realtime)))

;;; v_delay / v_#
(defmacro v_delay (&rest ticks)
  (let* ((last-item (car (last ticks)))
         (is-statement (or (not last-item) (eq :pass last-item)))
         (actual-ticks (if is-statement (butlast ticks) ticks)))
    `(list :delay ,@(mapcar (lambda (t_) `',t_)  actual-ticks)
           ,@(if is-statement '(:stmt) nil))))

(defmacro v_# (&rest ticks)
  `(v_delay ,@ticks))

;;; v_dump — sugar for (initial (dumpfile fn) (dumpvars))
(defmacro v_dump (fn)
  `(v_initial (v_dumpfile ,fn) (v_dumpvars)))

;;; ============================================================
;;; Pattern F — Module/Primitive/Instance definitions
;;; ============================================================

;;; Convert parameter list from verilisp format to AST format.
;;; Input:  ((output a) (inout (2 b c)) (input d e))
;;; Output: ((output a) (inout (2 b) (2 c)) (input d e))
;;; — same as what emit expects
(defun ast-convert-params (params)
  "Convert verilisp parameter list to AST-compatible format.
   nil (i.e. empty list ()) becomes :empty sentinel.
   Use nil directly in make-dollar-module for no-port-list modules."
  (if (null params)
      :empty
      params))

;;; make-named-module-ast — creates instance macros in AST mode
(defun make-named-module-ast (macro-name verilog-name)
  (eval
    `(defmacro ,macro-name (&rest args)
       ;; Process args at macro-expansion time, return code that builds AST at eval time
       (let ((real-args args)
             (instance-name-form nil))
         ;; Handle delay as first numeric arg (skip for now — not used in tests)
         (when (and args (numberp (car args)))
           (setq real-args (cdr args)))
         ;; Handle :name keyword argument
         (let ((i (position ':name real-args)))
           (if i
               (progn
                 (setq instance-name-form `',(nth (1+ i) real-args))
                 (setq real-args
                   (let ((result nil) (skip nil))
                     (dolist (x real-args (nreverse result))
                       (cond
                         (skip (setq skip nil))
                         ((eq x ':name) (setq skip t))
                         (t (push x result)))))))
               (setq instance-name-form `',(gen-expand-variable))))
         ;; Build the form that will be evaluated
         `(progn
            (dict-insert! *current-module-contents* ',',verilog-name 1)
            (list* :inst ',',verilog-name ,instance-name-form
                   (list ,@(mapcar (lambda (arg)
                                     (if (and (listp arg) (eq (car arg) 'name))
                                         ;; Named argument: (name port expr) → (:named-arg port (ast-eval 'expr))
                                         `(list :named-arg ',(second arg) (ast-eval ',(third arg)))
                                         `(ast-eval ',arg)))
                                   real-args)))))))
  (setq all-modules (cons macro-name all-modules)))

;;; v_module
(defmacro v_module (name parameters &rest statements)
  `(progn
     (assert (null (find #\- (string-downcase (symbol-name ',name)))) nil
             "Module name must not contain '-'. name:~a" ',name)
     (let ((*in-module* t)
           (*current-module-contents* nil))
       (let ((stmts (list ,@(mapcar (lambda (s) `(ast-eval ',s)) statements)))
             (params (ast-convert-params ',parameters)))
         ;; Side effect: record module contents
         (setq *module-contents*
               (append *module-contents*
                       (list (cons ',name
                                   (foreach k-v *current-module-contents*
                                     (list (car k-v)
                                           (apply #'+ (cdr k-v))))))))
         ;; Side effect: register module as instantiable macro
         (make-named-module-ast ',name ',name)
         ;; Return the AST node
         (list* :module ',name params stmts)))))

;;; v_defmodule — alias
(defmacro v_defmodule (&rest args)
  `(v_module ,@args))

;;; v_primitive
(defmacro v_primitive (name parameters &rest statements)
  `(progn
     (let ((*in-module* t)
           (*current-module-contents* nil))
       (let ((stmts (list ,@(mapcar (lambda (s) `(ast-eval ',s)) statements)))
             (params (ast-convert-params ',parameters)))
         ;; Side effect: register
         (make-named-module-ast ',name ',name)
         (list* :primitive ',name params stmts)))))

;;; v_table
(defun unmangle-table-sym (sym)
  "Convert v_* back to * in table data."
  (if (eq sym 'v_*) '* sym))

(defun unmangle-table-element (elem)
  "Process a table element, replacing v_* with *."
  (if (atom elem)
      (unmangle-table-sym elem)
      (mapcar #'unmangle-table-sym elem)))

(defun unmangle-table-row (row)
  "Process a table row, replacing v_* with * in all positions."
  (mapcar #'unmangle-table-element row))

(defmacro v_table (&rest table)
  `(list :table ,@(mapcar (lambda (row)
                             `',(unmangle-table-row row))
                           table)))

;;; Re-register gate primitives in AST mode
(foreach primitive *primitives*
  (make-named-module-ast (mangle primitive) primitive))

;;; v_supply0, v_supply1
(foreach name '(supply0 supply1)
  (let ((kw (intern (string name) "KEYWORD")))
    (eval
      `(defmacro ,(mangle name) (&rest parameters)
         `(list ,',kw ,@(mapcar (lambda (p) `',p) parameters))))))

;;; ============================================================
;;; Directives
;;; ============================================================

(defmacro v_include (&rest filenames)
  `(list :include ,@(mapcar (lambda (f) `',f) filenames)))

(defmacro v_define (name value)
  `(list :define ',name ',value))

(defmacro v_timescale (x)
  `(list :timescale ',x))

(defmacro v_comment (&rest body)
  `(list :comment ,@(mapcar (lambda (w) (if (stringp w) w `',w)) body)))

;;; ============================================================
;;; Function / Task
;;; ============================================================

(defmacro v_function (&rest args)
  ;; Two calling patterns:
  ;; (function (msb lsb) name (inputs...) body...)
  ;; (function name (inputs...) body...)
  (let ((range nil)
        (name (first args))
        (inputs (second args))
        (statements (cddr args)))
    (when (not (atom (car args)))
      (setq range (first args))
      (setq name (second args))
      (setq inputs (third args))
      (setq statements (cdddr args)))
    `(list :function ',range ',name ',inputs
           ,@(mapcar (lambda (s) `(ast-eval ',s)) statements))))

(defmacro v_task (name parameters &rest statements)
  `(list :task ',name ',parameters
         ,@(mapcar (lambda (s) `(ast-eval ',s)) statements)))

;;; ============================================================
;;; Misc macros (not used in Phase 1a tests but needed for completeness)
;;; ============================================================

;;; v_++ / v_-- — increment/decrement sugar
(defmacro v_++ (&rest variables)
  `(list :progn ,@(mapcar (lambda (v) `(list := ',v (list :+ ',v 1))) variables)))

(defmacro v_-- (&rest variables)
  `(list :progn ,@(mapcar (lambda (v) `(list := ',v (list :- ',v 1))) variables)))

;;; v_. — dot access
(defmacro v_. (&rest names)
  ;; Just produce a symbol with dots — emit handles it as atom
  (let ((dotted (apply #'strcat
                  (cons (string (car names))
                        (mapcan (lambda (n) (list "." (string n))) (cdr names))))))
    `',dotted))

;;; v_deassign — not commonly used, return raw for now
(defmacro v_deassign (&rest lvalues)
  `(list :deassign ,@(mapcar (lambda (l) `',l) lvalues)))

;;; v_wait
(defmacro v_wait (condition)
  `(list :wait (ast-eval ',condition)))

;;; v_forever
(defmacro v_forever (&rest body)
  `(list :forever ,@(mapcar (lambda (s) `(ast-eval ',s)) body)))

;;; v_forallbits
(defmacro v_forallbits (i size &rest body)
  `(progn
     (v_reg (,size ,i))
     (v_fromto ,i 0 ,(- (<< 1 (- size 1)) 1)
       ,@body)
     ,@body))

;;; v_repeat, v_while
(foreach name '(repeat while)
  (eval
    `(defmacro ,(mangle name) (expr &rest body)
       `(list ,',(intern (string name) "KEYWORD")
              (ast-eval ',expr)
              ,@(mapcar (lambda (s) `(ast-eval ',s)) body)))))

;;; v_event, v_real — simple declarations
(foreach name '(event real)
  (let ((kw (intern (string name) "KEYWORD")))
    (eval
      `(defmacro ,(mangle name) (&rest names)
         `(list ,',kw ,@(mapcar (lambda (n) `',n) names))))))
