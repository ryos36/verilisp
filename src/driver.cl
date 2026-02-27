;;;; src/driver.cl — AST-based translation pipeline for Phase 1a
;;;; Load order: __verilisp__.cl → ast.cl → emit.cl → core.cl → driver.cl

;;; ============================================================
;;; Post-processing: wrap-top-level
;;; ============================================================

(defun make-dollar-module (stmts)
  "Wrap statements in (:module _$_ nil stmts...)."
  (list* :module '_$_ nil stmts))

(defun needs-module-wrap-p (tag)
  "Return T if an AST node with this tag needs to be inside a module.
   In the old pipeline, check-in-module is called by declarations, system tasks, etc.
   but NOT by v_assign, v_comment, v_function, v_task."
  (not (member tag '(:assign :function :task))))

(defun process-bare-stmts (stmts)
  "Process bare statements: group module-needing stmts into _$_ modules,
   let top-level-safe stmts (assign, function, task) through directly.
   Returns a list of top-level forms in order."
  (let ((result nil)
        (module-stmts nil))
    (dolist (stmt stmts)
      (let ((tag (and (consp stmt) (car stmt))))
        (if (needs-module-wrap-p tag)
            (push stmt module-stmts)
            (progn
              ;; Flush any accumulated module-needing stmts first
              (when module-stmts
                (push (make-dollar-module (nreverse module-stmts)) result)
                (setq module-stmts nil))
              (push stmt result)))))
    ;; Flush remaining module-needing stmts
    (when module-stmts
      (push (make-dollar-module (nreverse module-stmts)) result))
    (nreverse result)))

(defun flatten-progn (node)
  "Recursively flatten nested :progn nodes into a flat list of children."
  (if (and (consp node) (eq (car node) :progn))
      (mapcan #'flatten-progn (cdr node))
      (list node)))

(defun wrap-top-level (progn-node)
  "Post-process a :progn node: extract directives, wrap bare stmts in _$_ module.
   Input: (:progn node1 node2 ...)
   Output: list of top-level AST nodes"
  (let ((directives nil)
        (top-forms nil)
        (bare-stmts nil)
        ;; Flatten nested :progn nodes first
        (flat-nodes (flatten-progn progn-node)))
    (dolist (node flat-nodes)
      (when (and node (consp node))
        (let ((tag (car node)))
          (cond
            ((member tag '(:include :define :timescale))
             (push node directives))
            ((member tag '(:module :primitive))
             ;; Flush bare stmts before module
             (when bare-stmts
               (setq top-forms
                     (append top-forms (process-bare-stmts (nreverse bare-stmts))))
               (setq bare-stmts nil))
             (setq top-forms (append top-forms (list node))))
            (t
             (push node bare-stmts))))))
    ;; Flush remaining bare stmts
    (when bare-stmts
      (setq top-forms
            (append top-forms (process-bare-stmts (nreverse bare-stmts)))))
    (append (nreverse directives) top-forms)))

;;; ============================================================
;;; Main translation entry point
;;; ============================================================

(defun translate-code-ast (code output-stream)
  "Translate verilisp CODE string via AST pipeline:
   mangle → eval (AST collect) → post-process → emit."
  (let* ((lib-path (concatenate 'string *verilisp-dir* "lib/"))
         (mangled (mangle-code code))
         (wrapped (format nil "(progn (add-verilisp-path \"~a\") (v_progn ~a))"
                          lib-path mangled))
         (ast-form (with-input-from-string (s wrapped)
                     (read s)))
         ;; Eval to collect AST
         (raw-ast (let ((*in-module* nil)
                        (*current-module-contents* nil)
                        (*ast-toplevel-nodes* (list :sentinel)))
                    (eval ast-form)))
         ;; Post-process: extract directives, wrap bare stmts
         (top-nodes (wrap-top-level raw-ast)))
    ;; Emit to output
    (emit-ast top-nodes output-stream)
    ;; Match the fresh-line behavior of the old pipeline
    (fresh-line output-stream)))

;;; ============================================================
;;; Test runner for new pipeline
;;; ============================================================

(defparameter *new-test-names*
  '("declarators" "case" "for_fromto" "function_task"
    "primitive" "primitives" "module" "assign" "dollars_backticks"
    "expand" "multiplier" "use"))

(defun run-new-tests ()
  "Run Phase 1a tests using the AST pipeline.
   Returns the number of failures."
  (let* ((test-dir (concatenate 'string *verilisp-dir* "tests/"))
         (divider (concatenate 'string
                    (string #\Newline)
                    (make-string 80 :initial-element #\=)
                    (string #\Newline)))
         (successes 0)
         (failures 0))
    (dolist (test-name (sort (copy-list *new-test-names*) #'string<))
      (let* ((test-path (concatenate 'string test-dir test-name))
             (content (read-file-to-string test-path))
             (div-pos (search divider content)))
        (if (null div-pos)
            (progn
              (format t "test ~a is malformed~%" test-name)
              (incf failures))
            (let ((vl-code (subseq content 0 div-pos))
                  (expected (subseq content (+ div-pos (length divider)))))
              (reset-verilisp-state)
              (let ((actual
                      (handler-case
                        (with-output-to-string (out)
                          (translate-code-ast vl-code out))
                        (undefined-function (e)
                          (format *error-output* "~%ERROR in ~a: undefined function ~a~%"
                                  test-name (cell-error-name e))
                          (format nil "ERROR"))
                        (error (e)
                          (format *error-output* "~%ERROR in ~a: ~a~%"
                                  test-name (princ-to-string e))
                          (format nil "ERROR")))))
                (if (string= actual expected)
                    (progn
                      (format t "Success:~c~a~%" #\Tab test-name)
                      (incf successes))
                    (progn
                      (format t "Failure:~c~a~%" #\Tab test-name)
                      (format t "    translate(~s)~%    should equal~%    ~s~%    but was~%    ~s~%"
                              vl-code expected actual)
                      (incf failures))))))))
    (format t "~a successes, ~a failures~%" successes failures)
    failures))
