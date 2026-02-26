;;;; verilisp.cl — CL-only driver for verilisp
;;;; Replaces verilisp.py: mangling + translation + test runner + CLI

(defparameter *verilisp-standalone* t)

(defparameter *verilisp-dir*
  (directory-namestring *load-truename*))

;;; Load the verilisp core
(load (concatenate 'string *verilisp-dir* "__verilisp__.cl"))

;;; Save post-init state of all-modules (contains primitives registered during load)
(defvar *initial-all-modules* (copy-list all-modules))

;;; Load AST system (Phase 1a)
(load (concatenate 'string *verilisp-dir* "src/ast.cl"))
(load (concatenate 'string *verilisp-dir* "src/emit.cl"))
(load (concatenate 'string *verilisp-dir* "src/macros.cl"))
(load (concatenate 'string *verilisp-dir* "tests/ast-tests.cl"))

;;; ============================================================
;;; Constants
;;; ============================================================

(defparameter *mangler* "v_")
(defparameter *antimangler* "l_")

(defparameter *names-to-mangle*
  '(;; special forms
    "@" "fork" "release" "assign" "deassign" "task" "function" "=" "n="
    "delay" "#" "wait" "if" "module" "always" "initial" "cat" "cat*" "."
    "primitive" "table" "for" "fromto" "forallbits" "ref" "comment"
    "b" "d" "h" "o"

    "<=#"

    ;; cases
    "case" "cond" "casex" "casez"

    ;; $macros
    "setup" "hold" "setuphold" "period" "width" "skew" "recovery"
    "readmemb" "readmemh" "sreadmemb" "sreadmemh"
    "display" "displayh" "displayb" "displayo"
    "fdisplay" "fdisplayh" "fdisplayb" "fdisplayo"
    "write" "writeh" "writeb" "writeo"
    "fwrite" "fwriteh" "fwriteb" "fwriteo"
    "strobe" "strobeh" "strobeb" "strobeo"
    "fstrobe" "fstrobeh" "fstrobeb" "fstrobeo"
    "monitor" "monitorh" "monitorb" "monitoro"
    "fmonitor" "fmonitorh" "fmonitorb" "fmonitoro"
    "fopen" "fclose" "fread"
    "time" "stime" "realtime" "scale" "printtimescale" "timeformat"
    "stop" "finish" "save" "incsave" "restart" "log" "nolog" "key" "nokey"
    "scope" "showscopes" "showvars" "countdrivers" "list"
    "monitoron" "monitoroff" "dumpon" "dumpoff" "dump"
    "dumpfile" "dumplimit" "dumpflush" "dumpvar" "dumpvars" "dumpall"
    "reset" "reset_value" "reset_count"
    "random" "getpattern" "rtoi" "itor" "realtobits" "bitstoreal"

    ;; primitives
    "not" "or" "and" "nand" "nor" "xor" "xnor"
    "buf" "bufif0" "bufif1" "notif0" "notif1"
    "pmos" "rpmos" "nmos" "rnmos" "cmos" "rcmos"
    "tran" "rtran" "tranif1" "rtranif1" "tranif0" "rtranif0"
    "pullup" "pulldown" "supply0" "supply1"

    ;; type declarators
    "wire" "wand" "wor" "reg" "trireg" "integer" "parameter"
    "reg="

    ;; backquote macros
    "include" "define" "timescale"

    ;; math operators
    "+" "-" "++" "--" "~&" "&" "?" "&&" "*" "/" "//" "%" "<<" ">>" ">" "<"
    ">>>" "<<<" "==" "!=" "===" "!==" "^" "^~" "~^" ">=" "!" "~"
    "+=" "-=" "*=" "/=" "^=" "%=" "&=" "bor=" "lor="

    ;; math operator, not assign operator
    "<="

    ;; special math operators
    "%<=" "=<"))

(defparameter *special-names-to-mangle*
  '(("|"  . "bitwise-or")
    ("||" . "logical-or")
    ("~|" . "bitwise-nor")
    ("|~" . "bitwise-nor")
    ("|=" . "bor=")
    ("||=" . "lor=")))

;;; ============================================================
;;; Utilities
;;; ============================================================

(defun read-file-to-string (path)
  (with-open-file (s path :direction :input)
    (read-stream-to-string s)))

(defun read-stream-to-string (stream)
  (let ((result (make-array 0 :element-type 'character
                              :adjustable t :fill-pointer 0)))
    (loop for c = (read-char stream nil nil)
          while c
          do (vector-push-extend c result))
    result))

;;; ============================================================
;;; Mangling
;;; ============================================================

(defun delimiter-char-p (c)
  (or (char= c #\Space)
      (char= c #\Tab)
      (char= c #\Newline)
      (char= c #\Return)
      (char= c #\()
      (char= c #\))))

(defun replace-with-delimiter (code old new)
  "Replace all occurrences of OLD in CODE with NEW,
   but only when OLD is followed by a delimiter character."
  (let ((result (make-array (length code) :element-type 'character
                                          :adjustable t :fill-pointer 0))
        (old-len (length old))
        (code-len (length code))
        (i 0))
    (loop
      (when (>= i code-len) (return))
      (if (and (<= (+ i old-len) code-len)
               (string= code old :start1 i :end1 (+ i old-len))
               (< (+ i old-len) code-len)
               (delimiter-char-p (char code (+ i old-len))))
          (progn
            (loop for c across new do (vector-push-extend c result))
            (incf i old-len))
          (progn
            (vector-push-extend (char code i) result)
            (incf i))))
    result))

(defun escape-number-quotes (code)
  "Replace digits' with digits\\' for Verilog number literals."
  (let ((result (make-array (length code) :element-type 'character
                                          :adjustable t :fill-pointer 0))
        (code-len (length code))
        (i 0))
    (loop
      (when (>= i code-len) (return))
      (if (and (char= (char code i) #\')
               (> i 0)
               (digit-char-p (char code (1- i))))
          (progn
            (vector-push-extend #\\ result)
            (vector-push-extend #\' result)
            (incf i))
          (progn
            (vector-push-extend (char code i) result)
            (incf i))))
    result))

(defun mangle-code (code)
  "Apply name mangling to CODE string, matching Python mangle() behavior."
  ;; For each name: (NAME → (v_NAME, then (l_NAME → (NAME
  (dolist (name *names-to-mangle*)
    (setf code (replace-with-delimiter
                code
                (concatenate 'string "(" name)
                (concatenate 'string "(" *mangler* name)))
    (setf code (replace-with-delimiter
                code
                (concatenate 'string "(" *antimangler* name)
                (concatenate 'string "(" name))))
  ;; Special names
  (dolist (pair *special-names-to-mangle*)
    (setf code (replace-with-delimiter
                code
                (concatenate 'string "(" (car pair))
                (concatenate 'string "(" *mangler* (cdr pair)))))
  ;; Escape number quotes
  (setf code (escape-number-quotes code))
  code)

;;; ============================================================
;;; Translation
;;; ============================================================

(defun translate-code (code output-stream &key dep-file target-file)
  "Translate verilisp CODE string, writing Verilog to OUTPUT-STREAM."
  (let* ((lib-path (concatenate 'string *verilisp-dir* "lib/"))
         (prefixed-code
           (concatenate 'string
             (if dep-file
                 (format nil "(setf *DEP-FILE* \"~a\")(setf *TARGET-FILE* \"~a\")"
                         dep-file target-file)
                 "")
             (format nil "(add-verilisp-path \"~a\")" lib-path)
             code))
         (mangled (mangle-code prefixed-code))
         (full-code
           (format nil "(eval `(let ((*__name__* :__main__)) ~a))~%(__end__)~%"
                   mangled)))
    (let ((*standard-output* output-stream))
      (with-input-from-string (s full-code)
        (let ((*standard-input* s))
          (read-eval (read))))
      (when *in-module*
        (setq indentation 0)
        (nli)
        (write-string "endmodule")
        (nli))
      ;; CLISP adds a fresh-line at process exit; replicate for in-process parity
      (fresh-line))))

(defun reset-verilisp-state ()
  "Reset global state between test runs."
  (setq indentation 0)
  (setq *in-module* nil)
  (setq *n-expand-variables* -1)
  (setq *module-contents* nil)
  (setq *current-module-contents* nil)
  (setq all-modules (copy-list *initial-all-modules*))
  (setq *used* nil)
  (setq *comment-expand* t)
  (setq *__name__* nil)
  (setq *transport-delay* nil)
  (setq *inertial-delay* nil)
  (setq *DEP-FILE* nil)
  (setq *TARGET-FILE* nil)
  (setq *timescale* '(1000 1))
  (setq *nettype-prologue* "none")
  (setq *nettype-epilogue* "wire")
  (setq *verilisp-path* (list "./lib/" "/usr/lib/verilisp/")))

;;; ============================================================
;;; Test Runner
;;; ============================================================

(defun run-tests ()
  (let* ((test-dir (concatenate 'string *verilisp-dir* "tests/"))
         (divider (concatenate 'string
                    (string #\Newline)
                    (make-string 80 :initial-element #\=)
                    (string #\Newline)))
         (test-files (directory (concatenate 'string test-dir "*")))
         (successes 0)
         (failures 0))
    (dolist (test-path (sort test-files #'string< :key #'namestring))
      (let* ((fn (file-namestring test-path))
             (content (read-file-to-string test-path))
             (div-pos (search divider content)))
        (if (null div-pos)
            (format t "test ~a is malformed~%" fn)
            (let ((vl-code (subseq content 0 div-pos))
                  (expected (subseq content (+ div-pos (length divider)))))
              (reset-verilisp-state)
              (let ((actual (with-output-to-string (out)
                              (translate-code vl-code out))))
                (if (string= actual expected)
                    (progn
                      (format t "Success:~c~a~%" #\Tab fn)
                      (incf successes))
                    (progn
                      (format t "Failure:~c~a~%" #\Tab fn)
                      (format t "    translate(~s)~%    should equal~%    ~s~%    but was~%    ~s~%"
                              vl-code expected actual)
                      (incf failures))))))))
    (format t "~a successes, ~a failures~%" successes failures)
    failures))

;;; ============================================================
;;; File Translation
;;; ============================================================

(defun translate-file (in-path out-path &key dep-file)
  (let ((code (read-file-to-string in-path)))
    (reset-verilisp-state)
    (with-open-file (out out-path :direction :output :if-exists :supersede)
      (translate-code code out :dep-file dep-file :target-file out-path))))

;;; ============================================================
;;; CLI
;;; ============================================================

(defun main ()
  (let ((args ext:*args*)
        (only-mangle nil)
        (out-dir nil)
        (dep-file nil)
        (next-is-dir nil)
        (next-is-dep nil))
    (if (null args)
        ;; stdin → stdout
        (let ((code (read-stream-to-string *standard-input*)))
          (reset-verilisp-state)
          (translate-code code *standard-output*))
        ;; Process args
        (dolist (arg args)
          (cond
            (next-is-dir
             (setq out-dir arg)
             (setq next-is-dir nil))
            (next-is-dep
             (setq dep-file arg)
             (setq next-is-dep nil)
             ;; Touch the depfile
             (with-open-file (f dep-file :direction :output :if-exists :supersede)))
            ((string= arg "--") nil) ; skip separator
            ((or (string= arg "-h") (string= arg "--help") (string= arg "-H"))
             (format t "Usage: verilisp [OPTIONS] [FILES...]~%")
             (format t "  --mangle       Mangle only (.hvlib output)~%")
             (format t "  --dir DIR      Output directory~%")
             (format t "  --depfile FILE Generate depfile~%")
             (format t "  -t             Run tests~%")
             (format t "  -ta            Run AST emit tests~%")
             (format t "  -h / --help    Show this help~%")
             (format t "  (no args)      stdin -> stdout~%"))
            ((string= arg "--mangle")
             (setq only-mangle t))
            ((string= arg "-t")
             (let ((failures (run-tests)))
               (when (> failures 0) (ext:exit 1))))
            ((string= arg "-ta")
             (let ((failures (run-ast-tests)))
               (when (> failures 0) (ext:exit 1))))
            ((string= arg "--dir")
             (setq next-is-dir t))
            ((string= arg "--depfile")
             (setq next-is-dep t))
            ((probe-file arg)
             (let* ((in-pathname (pathname arg))
                    (base (pathname-name in-pathname))
                    (ext (if only-mangle ".hvlib" ".v"))
                    (out-name (concatenate 'string base ext))
                    (out-path (if out-dir
                                  (concatenate 'string out-dir "/" out-name)
                                  (concatenate 'string
                                    (directory-namestring in-pathname)
                                    out-name))))
               (if only-mangle
                   (let ((code (read-file-to-string arg)))
                     (with-open-file (out out-path :direction :output
                                                   :if-exists :supersede)
                       (write-string (mangle-code code) out)))
                   (progn
                     (reset-verilisp-state)
                     (handler-case
                       (translate-file arg out-path :dep-file dep-file)
                       (error (e)
                         (declare (ignore e))
                         (when (probe-file out-path)
                           (delete-file out-path))))))))
            (t
             (format *error-output* "File not found: ~a~%" arg)))))))

(main)
(ext:exit 0)
