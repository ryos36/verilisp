;;;; src/macros.cl — AST-returning macros for verilisp Phase 1a
;;;; Each ast-* macro returns an AST S-expression (not text output).

;;; ============================================================
;;; Top-level
;;; ============================================================

(defmacro ast-module (name parameters &rest statements)
  `(list :module ',name ',parameters ,@statements))

(defmacro ast-primitive (name parameters &rest statements)
  `(list :primitive ',name ',parameters ,@statements))

;;; ============================================================
;;; Declarations
;;; ============================================================

(defmacro ast-reg (&rest specs)
  `(list :reg ,@(mapcar (lambda (s) `',s) specs)))

(defmacro ast-wire (&rest specs)
  `(list :wire ,@(mapcar (lambda (s) `',s) specs)))

(defmacro ast-wand (&rest specs)
  `(list :wand ,@(mapcar (lambda (s) `',s) specs)))

(defmacro ast-wor (&rest specs)
  `(list :wor ,@(mapcar (lambda (s) `',s) specs)))

(defmacro ast-trireg (&rest specs)
  `(list :trireg ,@(mapcar (lambda (s) `',s) specs)))

(defmacro ast-integer (&rest specs)
  `(list :integer ,@(mapcar (lambda (s) `',s) specs)))

(defmacro ast-parameter (name value)
  `(list :parameter ',name ,value))

(defmacro ast-reg= (name value)
  `(list :reg= ',name ,value))

;;; ============================================================
;;; Statements
;;; ============================================================

(defmacro ast-always (signals &rest body)
  `(list :always ',signals ,@body))

(defmacro ast-initial (&rest body)
  `(list :initial ,@body))

(defmacro ast-if (cond then &optional else)
  (if else
      `(list :if ,cond ,then ,else)
      `(list :if ,cond ,then)))

(defmacro ast-cond (&rest clauses)
  `(list :cond ,@(mapcar (lambda (c)
                           `(list ',(car c) ,@(cdr c)))
                         clauses)))

(defmacro ast-case (value &rest cases)
  `(list :case ,value ,@(mapcar (lambda (c)
                                  `(list ',(car c) ,@(cdr c)))
                                cases)))

(defmacro ast-casex (value &rest cases)
  `(list :casex ,value ,@(mapcar (lambda (c)
                                   `(list ',(car c) ,@(cdr c)))
                                 cases)))

(defmacro ast-casez (value &rest cases)
  `(list :casez ,value ,@(mapcar (lambda (c)
                                   `(list ',(car c) ,@(cdr c)))
                                 cases)))

(defmacro ast-for ((var init) cond (var2 step) &rest body)
  `(list :for (list ',var ,init) ,cond (list ',var2 ,step) ,@body))

(defmacro ast-fork (&rest body)
  `(list :fork ,@body))

(defmacro ast-function (range-or-name &rest rest)
  ;; (ast-function (1 2) myfunc (params) body...)
  ;; (ast-function myfunc (params) body...)
  (if (listp range-or-name)
      (let ((range range-or-name)
            (name (car rest))
            (params (cadr rest))
            (stmts (cddr rest)))
        `(list :function ',range ',name ',params ,@stmts))
      (let ((name range-or-name)
            (params (car rest))
            (stmts (cdr rest)))
        `(list :function nil ',name ',params ,@stmts))))

(defmacro ast-task (name parameters &rest statements)
  `(list :task ',name ',parameters ,@statements))

(defmacro ast-table (&rest rows)
  `(list :table ,@(mapcar (lambda (r) `',r) rows)))

;;; ============================================================
;;; Assignments
;;; ============================================================

(defmacro ast-assign (lvalue expr)
  `(list :assign ,lvalue ,expr))

(defmacro ast-= (lvalue expr)
  `(list := ',lvalue ,expr))

(defmacro ast-n= (lvalue expr)
  `(list :<= ',lvalue ,expr))

(defmacro ast-<=# (lvalue expr &optional transport inertial)
  `(list :<=# ',lvalue ,expr ,transport ,inertial))

;;; ============================================================
;;; Expressions — operators
;;; ============================================================

(dolist (op '(:+ :- :* :/ :% :// :< :> :<=_op :<< :>> :>>> :<<<
             :== :!= :=== :!== :^ :^~ :~^ :>= :! :~ :~& :& :&&
             :\| :\|\| :~\|))
  (let* ((op-name (symbol-name op))
         ;; Remove leading colon for macro name
         (macro-name (intern (concatenate 'string "AST-" (subseq op-name 1))
                             (find-package "CS-COMMON-LISP-USER"))))
    (eval `(defmacro ,macro-name (&rest args)
             (list* ',op args)))))

;;; ============================================================
;;; Expressions — other
;;; ============================================================

(defmacro ast-ref (name &rest indices)
  `(list :ref ',name ,@indices))

(defmacro ast-cat (&rest args)
  `(list :cat ,@args))

(defmacro ast-cat* (n expr)
  `(list :cat* ,n ,expr))

(defmacro ast-? (cond yes no)
  `(list :? ,cond ,yes ,no))

(defmacro ast-d (size num)
  `(list :d ,size ,num))

(defmacro ast-h (size num)
  `(list :h ,size ,num))

(defmacro ast-b (size num)
  `(list :b ,size ,num))

(defmacro ast-o (size num)
  `(list :o ,size ,num))

;;; ============================================================
;;; Instances
;;; ============================================================

(defmacro ast-inst (module-name instance-name &rest args)
  `(list :inst ',module-name ',instance-name ,@args))

(defun ast-named-arg (name value)
  "Create a named argument node: (:named-arg name value)"
  (list :named-arg name value))

;;; ============================================================
;;; System tasks / variables
;;; ============================================================

(defmacro ast-$ (name &rest args)
  `(list :$ ',name ,@args))

(defmacro ast-$var (name)
  `(list :$var ',name))

(defmacro ast-delay (&rest ticks)
  `(list :delay ,@ticks))

;;; ============================================================
;;; Compiler directives
;;; ============================================================

(defmacro ast-include (&rest filenames)
  `(list :include ,@(mapcar (lambda (f) `',f) filenames)))

(defmacro ast-define (name value)
  `(list :define ',name ',value))

(defmacro ast-timescale (value)
  `(list :timescale ',value))

(defmacro ast-comment (&rest words)
  `(list :comment ,@(mapcar (lambda (w) (if (stringp w) w `',w)) words)))

;;; ============================================================
;;; Sequence
;;; ============================================================

(defmacro ast-progn (&rest body)
  `(list :progn ,@body))
