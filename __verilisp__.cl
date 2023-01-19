#!/usr/bin/clisp

#|
    verilisp is a collection of Common Lisp macros which generate verilog code.
    do not run this script directly unless you know what you're doing -- 	
    compile your verilisp files into verilog with verilisp.py. it munges the names so you call my v_and instead of clisp's builtin and.
|#

(use-package 'cs-common-lisp-user)
(setq *print-base* 10)

(defparameter *__name__* nil)
(defparameter *timescale* '(1000 1))
(defparameter *nettype-prologue* "none")
(defparameter *nettype-epilogue* "wire")
(defparameter *transport-delay* nil)
(defparameter *inertial-delay* nil)

(defun debug-write (x) (princ x *error-output*))
(defun DEBUG (x)       (princ x *error-output*))

(defun make-prologue ()
  (when *__name__*
      (if *timescale*
        (format t "`timescale ~ans / ~aps~%" 
                (/ (car *timescale*) 1000)
                (cadr *timescale*)))
      (if *nettype-prologue*
        (format t "`default_nettype ~a~%" *nettype-prologue*))))

(defun make-epilogue ()
  (when *__name__* 
    (if *nettype-epilogue*
      (format t "~%`default_nettype ~a~%" *nettype-epilogue*))))

;;; BEGIN HELPER macros, functions, and variables

(defmacro foreach (itemname L &rest body)
    `(mapcar
        (lambda (,itemname)
            ,@body
        )
        ,L
    )
)
(defmacro foreach* (names-Ls &rest body)
    `(mapcar
        (lambda (
                ,@(mapcar #'first names-Ls)
            )
            ,@body
        )
        ,@(mapcar #'second names-Ls)
    )
)
(defmacro foreach** (names Ls &rest body)
    `(mapcar
        (lambda (,@names)
            ,@body
        )
        ,@Ls
    )
)

(defun zip (&rest Ls)
    (let (
            (lengths (mapcar #'length Ls))
            (range-n-Ls (range (length Ls)))
        )
        (foreach i (range (apply #'max lengths))
            (foreach j range-n-Ls
                (nth i (nth j Ls))
            )
        )
    )
)

(defun eval-all (statements)
    (mapcar #'eval statements)
)

(defun nonnull (x) (not (null x)))

(defun dec2hex (x)
    (tobase x 16)
)

(defun frombase (s base)
    (flet (
            (foo (s)
                (position
                    (string-downcase s)
                    (mapcar
                        #'(lambda (x) (string-downcase (write-to-string x)))
                        (append (range) '(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z))
                    )
                    :test #'string=
                )
            )
            (first-substr (s)
                (string (char s 0))
            )
            (cdr-substr (s)
                (substring s 1 (string-width s))
            )
        )
        (labels (
                (re (s result iter)
                    (if (> (string-width s) 0)
                        (re
                            (cdr-substr s)
                            (+ (foo (first-substr s)) (* base result))
                            (+ iter 1)
                        )
                        result
                    )
                )
            )
            (re
                (cdr-substr s)
                (foo (first-substr s))
                0
            )
        )
    )
)

(defun tobase (x base)
    (flet (
            (foo (x)
                (write-to-string
                    (nth
                        (mod x base)
                        (append (range) '(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z))
                    )
                )
            )
        )
        (labels (
                (re (x result)
                    (if (>= x base)
                        (re
                            (floor (/ x base))
                            (strcat (foo x) result)
                        )
                        (if (>= x 1)
                            (strcat (foo x) result)
                            result
                        )
                    )
                )
            )
            (assert (<= base 36))
            (assert (>= x 0))
            (re
                (floor (/ x base))
                (foo x)
            )
        )
    )
)

(defun rebase (x frombase tobase)
    (tobase (frombase x frombase) tobase)
)

(defun macall (macro-name &rest args)
    ; call a macro as if it were a function -- syntactic sugar.
    (eval `(,macro-name ,@args))
)

(defmacro macro-defun (macro-name function-name)
    `(defun ,function-name (&rest args)
        (eval (cons ',macro-name args))
    )
)

(defun permute (x)
    (if (not x)
        (list x)
        (apply #'append
            (foreach elem x
                (foreach sub (permute (remove elem x))
                    (cons elem sub)
                )
            )
        )
    )
)

(defun powerset (x)
    ; (equal (powerset '(0 1 2)) '(() (2) (1) (1 2) (0) (0 2) (0 1) (0 1 2)))
    (if (not x)
        (list x)
        (append
            (powerset (cdr x))
            (foreach subpow (powerset (cdr x))
                (cons (car x) subpow)
            )
        )
    )
)

(defun chunk (seq n)
    ; (equal (chunk (range) 2) '((0 1) (2 3) (4 5) (6 7) (8 9)))
    (foreach i (range 0 (length seq) n)
        (slice seq i (+ i n))
    )
)

(defun range (&rest L)
    (defun range3 (start stop step &optional result)
        (if (funcall
                (if (< 0 step)
                    #'>
                    #'<
                )
                (- stop step)
                start
            )
            (range3
                (+ start step)
                stop
                step
                (append result (list start))
            )
            (append result (list start))
        )
    )
    (if (> (length L) 0)
        (if (> (length L) 1)
            (if (> (length L) 2)
                (range3 (car L) (cadr L) (caddr L))
                (range3 (car L) (cadr L) 1)
            )
            (range3 0 (car L) 1)
        )
        (range3 0 10 1)
    )
)

(defun slice (L &rest start-stop-step)
    #|  python list slicing
        (slice list start stop step)
        (assert (and
            (equal (slice (range) 4) '(0 1 2 3))
            (equal (slice (range) 3 5) '(3 4))
            (equal (slice (range) 1 7 2) '(1 3 5))
            (equal (slice (range) 4 nil) '(4 5 6 7 8 9))
            (equal (slice (range) 5 nil 2) '(5 7 9))
            (equal (slice (range) nil 5 -1) '(9 8 7 6))
            (equal (slice (range) nil nil -1) '(9 8 7 6 5 4 3 2 1 0))
            (equal (slice (range) 0) nil)
            (equal (slice (range) 4 4) nil)
            (equal (slice (range) 4 -4) '(4 5))
        ))
        negative step is allowable so long as (< stop start)
        errors [where X is anything]:
           (slice (range) nil X)
           (slice (range) X X nil)
    |#
    (defun slice3 (start stop step &optional result)
        ; closes over L, result defaults to nil.
        (if (funcall
                (if (< 0 step)
                    #'>
                    #'<
                )
                (- stop step)
                start
            )
            (slice3
                (+ start step)
                stop
                step
                (append result (list (nth start L)))
            )
            (if (eq start stop)
                result
                (append result (list (nth start L)))
            )
        )
    )
    (let (
            (len-L (length L))
            (len-sss (length start-stop-step))
        )
        (cond
            ((= 0 len-sss) L)
            ((= 1 len-sss)
                (slice3
                    0
                    (car start-stop-step)
                    1
                )
            )
            ((= 2 len-sss)
                (let (
                        (cadr-sss (cadr start-stop-step))
                    )
                    (slice3
                        (car start-stop-step)
                        (if (not cadr-sss)
                            len-L
                            (if (< cadr-sss 0)
                                (+ len-L cadr-sss)
                                cadr-sss
                            )
                        )
                        1
                    )
                )
            )
            (t
                (let (
                        (car-sss (car start-stop-step))
                        (cadr-sss (cadr start-stop-step))
                        (caddr-sss (caddr start-stop-step))
                    )
                    (slice3
                        (if (not car-sss)
                            (if (> caddr-sss 0)
                                0
                                (- len-L 1)
                            )
                            car-sss
                        )
                        (if (not cadr-sss)
                            (if (> caddr-sss 0)
                                len-L
                                -1
                            )
                            cadr-sss
                        )
                        caddr-sss
                    )
                )
            )
        )
    )
)

(defun filter (fun L)
    ; what the hell kind of lisp is common lisp if filter isn't builtin?
    (if (cdr L)
        (append
            (if (funcall fun (car L))
                (list (car L))
                nil
            )
            (filter fun (cdr L))
        )
        (if (funcall fun (car L))
            L
            nil
        )
    )
)

(defun pow (a b)
    ; i can't believe that i actually have to define this.
    (cond
        ((eq b 0) 1)
        ((eq b 1) a)
        (t (*
            a
            (pow a (- b 1))
        ))
    )
)
(defun << (num shift)
    (* num (pow 2 shift))
)
(defun 1<< (shift)
    (<< 1 shift)
)
(defun 2** (shift)
    (pow 2 shift)
)

(defun replace-all (string part replacement &key (test #'char=))
    ; (eq (replace-all "aba" "a" "z") "zbz")
    (with-output-to-string (out)
        (loop
            with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search
                part string
                :start2 old-pos
                :test test
            ) do (
                write-string string out
                :start old-pos
                :end (or pos (length string))
            )
            when pos do (write-string replacement out)
            while pos
        )
    )
)

(defun write-all (&rest strings)
    (foreach s strings
        (write-string (string s))
    )
)

(defun nl () (write-char #\Newline) nil)

(defun read-eval (expr)
    ; eval expressions until '(__end__)
    (if (and
            (eq (car expr) '__end__)
            (not (cdr expr))
        )
        nil    ; end loop
        (progn
            (eval expr)
            (ignore-errors
                (read-eval (read))
            )
        )
    )
)

(defun strcat (&rest strings)
    (eval
        `(concatenate 'string
            ,@(foreach str strings
                (if (null str) ""
                  (if (or (numberp str) (listp str))
                    (write-to-string str)
                    (string str)
                  )
                )
            )
        )
    )
)
(defun symcat (&rest symbols)
    (read-from-string (apply #'strcat symbols))
)

(defun dict-insert (dict key value)
    ; symbol-list-map is a list of lists.
    ; (caar symbol-list-map) is a key.
    ; (cdar symbol-list-map) is every value that has ever been inserted for that key.
    ; this is a bit like Allen Downey's mdict in the python aspn cookbook.
    (if dict
        (if (and
                (not
                    (atom (car dict))
                )
                (eq
                    key
                    (caar dict)
                )
            )
            (cons
                (append
                    (car dict)
                    (list value)
                )
                (cdr dict)
            )
            (cons
                (car dict)
                (dict-insert (cdr dict) key value)
            )
        )
        ; if we get to the end of the map, map a new key-value pair
        (list
            (list key value)
        )
    )
)
(defmacro dict-insert! (dict-name key value)
    `(setq ,dict-name
        (dict-insert ,dict-name ,key ,value)
    )
)
(defun dict-lookup (dict key)
    ; (write (dict-lookup '((a b c) (d e f)) 'd))
    (if dict
        (if (eq (caar dict) key)
            (cdar dict)
            (dict-lookup (cdr dict) key)
        )
        nil
    )
)

(defun parse-parameters (params &optional (write-all-args t) (write-declarations t))
    (if write-all-args
        (progn
            (let (
                    (started nil)    ; don't print a comma before the first argument name
                )
                (write-string "(")
                (foreach param-set params
                    (foreach param (cdr param-set)
                        (if (atom param)
                            (progn
                                (if started
                                    (write-string ", ")
                                    (setq started t)
                                )
                                (write param)
                            )
                            (foreach bus-name (cdr param)
                                (progn
                                    (if started
                                        (write-string ", ")
                                        (setq started t)
                                    )
                                    (write bus-name)
                                )
                            )
                        )
                    )
                )
            )
            (write-string ");")
            (++indent)
        )
    )
    (if write-declarations
        (foreach param-set params
            (foreach param (cdr param-set)
                (if (atom param)
                    (progn
                        (nli)
                        (write-all
                            (car param-set)
                            " "
                            param
                            ";"
                        )
                    )
                    (foreach bus-name (cdr param)
                        (progn
                            (nli)
                            (write-all
                                (car param-set)
                                " ["
                                (let (
                                        (bus-size (car param))
                                    )
                                    (write-to-string
                                        (if (numberp bus-size)
                                            (- bus-size 1)
                                            (eval bus-size)
                                        )
                                    )
                                )
                                " : 0] "
                                bus-name
                                ";"
                            )
                        )
                    )
                )
            )
        )
    )
)

(defvar *in-module* nil)
(defun check-in-module ()
    ; most "v_" macros call this to see if they need to make a module
    (if (not *in-module*)
        (progn
            (nli)
            (write-string "module _$_;")
            (++indent)
            (setq *in-module* t)
        )
    )
)

(defvar indentation 0)    ; current indentation level (multiple of 4)

(defun symbol-to-v_const (symbol)
  (if (and (symbolp symbol)
           (search "'" (symbol-name symbol)))
    (format t "~a" symbol)
    (write symbol)))

(defun write-or-eval (symbol)
    (if (atom symbol)
        (symbol-to-v_const symbol)
        (eval symbol)
    )
)

(defun eval-or-write (symbol) (write-or-eval symbol))

(defun indent (&optional (n-spaces indentation))
    (if (> n-spaces 0)
        (progn
            (write-string " ")
            (indent (- n-spaces 1))
        )
    )
)

(defun nli ()
    (nl)
    (indent)
    nil
)

(defun ++indent ()
    (setq indentation (+ indentation 4))
)

(defun --indent ()
    (setq indentation (- indentation 4))
)

(defvar verilog-name-mangle "v_")    ; prepended to some macro names [like 'if' and '+'], so they don't make lisp sad

(defun mangle (name)
    (read-from-string (strcat verilog-name-mangle (string name)))
)

(defun unmangle (name)
    (coerce (cddr (coerce (string name) 'list)) 'string)
)

(defun write-poss-named-arg (arg)
    ; write a (possibly named) argument to a module
    (if (atom arg)
        (write arg)
        (if (eq (first arg) 'name)
            (progn
                (write-all "." (second arg) "(")
                (write-or-eval (third arg))
                (write-string ")")
            )
            (eval arg)
        )
    )
)

(defvar all-modules nil)	; used by expand
(defvar *module-contents* nil)    ; ((module-name (module-name number) (module-name number) ...) (module-name (module-name number) (module-name number) ...) ...)
(defvar *current-module-contents* nil) ; use by v_module
(defun make-named-module (macro-name verilog-name)
    (eval
        `(defmacro ,macro-name (&rest args)
            (check-in-module)
            (nli)
            (dict-insert! *current-module-contents* ',verilog-name 1)
            (write-all ',verilog-name " ")
            (if (and
                    args
                    (numberp (car args))
                )
                (progn
                    (eval `(v_delay ,(car args)))
                    (setq args (cdr args))
                    (write-string " ")
                )
            )
            (let (
                    (instance-name nil)
                    (i (position ':name args)) ; pseudo-keyword argument to let you name an instance
                )
                (if i
                    (progn
                        (setq instance-name
                            (nth
                                (1+ i)
                                args
                            )
                        )
                        (delete instance-name args)
                        (setq args (remove ':name args))
                    )
                    (setq instance-name (gen-expand-variable))
                )
                (write-all instance-name " (")
                (if args
                    (progn
                        (write-poss-named-arg (car args))
                        (foreach arg (cdr args)
                            (write-string ", ")
                            (write-poss-named-arg arg)
                        )
                    )
                )
                (write-string ");")
                nil
            )
        )
    )
    (setq all-modules (cons macro-name all-modules))
    nil
)
(defun make-module (name)
    (make-named-module name name)
)
(defmacro make-delayed-module (old-name delay-amt &key new-name)
    (if (not new-name)
        (setq new-name old-name)
    )
    (nli)
    (write-string "`define ")
    (write new-name)
    (write-string " ")
    (write old-name)
    (write-string " #")
    (write delay-amt)
    `(defmacro ,(if (contains *primitives* new-name) (mangle new-name) new-name) (&rest args)
        (check-in-module)
        (nli)
        (dict-insert! *current-module-contents* ',new-name 1)
        (write-all "`" ',new-name " ")
        (if (and
                args
                (numberp (car args))
            )
            (progn
                (eval `(v_delay ,@(cdar args)))
                (setq args (cdr args))
                (write-string " ")
            )
        )
        (write-all (gen-expand-variable) " (")
        (if args
            (progn
                (write-poss-named-arg (car args))
                (foreach arg (cdr args)
                    (write-string ", ")
                    (write-poss-named-arg arg)
                )
            )
        )
        (write-string ");")
        nil
    )
)
(defmacro delay-primitives (delay-amt &rest names)
    (if (not names)
        (setq names *primitives*)
    )
    `(progn
        ,@(foreach name names
           `(make-delayed-module ,name ,delay-amt)
        )
    )
)
(defun count-primitives (name)
    ; use *module-contents* recursively to find a histogram of the numbers of each kind of primitive used by the module named name
    ; eg:
    ;     (use adders)
    ;     (make-adder foo 16)
    ;     (assert (equal
    ;         (count-primitives 'foo)
    ;         '()
    ;     ))
    ; 
    ; *module-contents* looks like
    ; ((module-name (module-name number) (module-name number) ...) (module-name (module-name number) (module-name number) ...) ...)
    (let (
            (result nil)    ; ((name number) (name number) ...)
        )
        (foreach kvp (dict-lookup *module-contents* name)
            (if (contains *primitives* (car kvp))
                (dict-insert! result
                    (car kvp)
                    (cadr kvp)
                )
                (foreach subkvp (count-primitives (car kvp))
                    (dict-insert! result
                        (car subkvp)
                        (*
                            (cadr subkvp)
                            (cadr kvp)
                        )
                    )
                )
            )
        )
        (foreach kvp result
            (list
                (car kvp)
                (apply #'+ (cdr kvp))
            )
        )
    )
)

(defun contains (L e &optional (comparator #'eq))
    (if L
        (if (funcall comparator e (car L))
            t
            (contains (cdr L) e)
        )
        nil
    )
)

(defun read-all (filename)
    (let (
            (body nil)
        )
        (ignore-errors
            (with-open-file
                (stream filename)
                (defun repeated ()
                    (setq body
                        (append body (list (read stream)))
                    )
                    (repeated)
                )
                (repeated)
            )
        )
        body
    )
)

(defvar *used* nil)
(defvar *verilisp-path* (list "./lib/" "/usr/lib/verilisp/"))
(defun add-verilisp-path (&rest new-paths)
    (setq *verilisp-path*
        (append *verilisp-path* new-paths)
    )
)
(defun find-verilisp-package (package-name)
    (let (
            (result nil)
        )
        (foreach path-dir *verilisp-path*
            (foreach path (directory (make-pathname :name :wild :defaults path-dir))
                ; (write (pathname-name path)
                (if (string= (pathname-name path) package-name)
                    (setq result path)
                )
            )
        )
        result
    )
)
(defmacro use (package-name)
    (if (contains *used* package-name)
        nil
        (let ((*__name__* nil))
            (setq *used* (cons package-name *used*))
            `(let ((*__name__* nil))
                ,@(read-all
                    (find-verilisp-package
                        (string package-name)
                    )
                )
            )
        )
    )
)

(defvar *n-expand-variables* -1)
(defvar *comment-expand* t)
(defun gen-expand-variable ()
    (setq *n-expand-variables* (+ *n-expand-variables* 1))
    (read-from-string (strcat "anon_" (write-to-string *n-expand-variables*)))
)
(defun expandf (&rest args)
    (eval `(expand ,@args))
)
(defmacro expand (&rest args)
    (check-in-module)
    (let (
            (expr (car (last args)))
            (wires nil)
            (module-args nil)
            (orig-comment-expand *comment-expand*)
            (current-unnamed-arg-in-all-modules nil)
            (current-unnamed-arg-mangled-in-all-modules nil)
        )
        (if *comment-expand*
            (progn
                (commentf args)
                (setq *comment-expand* nil)
            )
        )
        (foreach unnamed-arg (cdr expr)
            ; build wires, module-args
            (if (or
                    (atom unnamed-arg)
                    (not (or
                        (contains all-modules (car unnamed-arg))
                        (contains all-modules (mangle (car unnamed-arg)))
                    ))
                )
                (setq module-args
                    (append module-args (list (list unnamed-arg)))
                )
                (progn
                    (setq throwaway (gen-expand-variable))
                    (setq wires
                        (append wires (list throwaway))
                    )
                    (setq module-args
                        (append
                            module-args
                            (list
                                (list
                                    throwaway
                                    (if (contains all-modules (car unnamed-arg))
                                        unnamed-arg
                                        (cons
                                            (mangle (car unnamed-arg))
                                            (cdr unnamed-arg)
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
        (if wires
            (eval `(v_wire ,@wires))
        )
        (eval
            `(  ,(if (contains
                        all-modules
                        (mangle (car expr))
                    )
                    (mangle (car expr))    ; use the module
                    (car expr)    ; hope to god you were smart enough to specify an actual module
                )
                ,@(slice
                    args
                    (length (cdr args))
                )
                ; module-args:
                ;     each element of length 1 is to be written-or-evald here
                ;     each element of length 2 is to expandf'd later
                ,@(foreach module-arg module-args
                    (car module-arg)
                )
            )
        )
        (foreach inner-expr module-args
            (if (cdr inner-expr)
                (expandf (car inner-expr) (cadr inner-expr))
            )
        )
        (if orig-comment-expand
            (setq *comment-expand* orig-comment-expand)
        )
    )
    nil
)

(defmacro gate-busses (gate &rest bus-specs)
    (let (
            (gate
                (if (contains all-modules gate)
                    gate
                    (mangle gate)
                )
            )
            (max-bus-size
                (apply #'max
                    (foreach bus-spec bus-specs
                        (if (or
                                (atom bus-spec)
                                (not (numberp (first bus-spec)))
                            )
                            1
                            (first bus-spec)
                        )
                    )
                )
            )
        )
        (foreach n (range max-bus-size)
            (eval
                `(
                    ,gate
                    ,@(foreach bus-spec bus-specs
                        (if (or
                                (atom bus-spec)
                                (not (numberp (first bus-spec)))
                            )
                            bus-spec
                            `(v_ref
                                ,(second bus-spec)
                                ,(mod n (first bus-spec))
                            )
                        )
                    )
                )
            )
        )
        nil
    )
)

(defmacro gate-busses* (gate &rest bus-specs)
    (let (
            (gate
                (if (contains all-modules gate)
                    gate
                    (mangle gate)
                )
            )
            (max-bus-size
                (apply #'max
                    (foreach bus-spec bus-specs
                        (if (or
                                (atom bus-spec)
                                (not (numberp (first bus-spec)))
                            )
                            1
                            (first bus-spec)
                        )
                    )
                )
            )
        )
        (foreach n (range max-bus-size)
            (eval
                `(
                    ,gate
                    ,@(foreach bus-spec bus-specs
                        (if (or
                                (atom bus-spec)
                                (not (numberp (first bus-spec)))
                            )
                            bus-spec
                            `(v_ref
                                ,(second bus-spec)
                                ,(if (third bus-spec)
                                    (eval
                                        (third bus-spec)
                                    )
                                    (mod
                                        n
                                        (first bus-spec)
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
        nil
    )
)

;;; END HELPERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BEGIN VERILISP LANGUAGE
; all keywords which traditionally begin on a newline must call nli themselves.

(defmacro v_module (name parameters &rest statements)
    (setq *in-module* t)
    (setq *current-module-contents* nil)
    (nli)
    (write-all "module " name)
    (parse-parameters parameters)
    ; done processing parameters, 
    (eval-all statements)
    (--indent)
    (nli)
    (write-string "endmodule")
    (nli)
    (setq *in-module* nil)
    (setq *module-contents*
        (append
            *module-contents*
            (list 
                (cons
                    name
                    (foreach k-v *current-module-contents*
                        (list
                            (car k-v)
                            (apply #'+
                                (cdr k-v)
                            )
                        )
                    )
                )
            )
        )
    )
    (make-module name)
)

(defmacro v_defmodule (&rest args)
    `(v_module ,@args)
)

(defmacro v_primitive (name parameters &rest statements)
    (setq *in-module* t)
    (nli)
    (write-all "primitive " name)
    (parse-parameters parameters)
    (eval-all statements)
    (--indent)
    (nli)
    (write-string "endprimitive")
    (nli)
    (setq *in-module* nil)
    (make-module name)
)

(defmacro v_table (&rest table)
    (defun *v_table-write* (sym)
        (if (atom sym)
            (if (eq sym 'v_*)
                (write '*)
                (write sym)
            )
            (foreach elem sym
                (if (eq elem 'v_*)
                    (write '*)
                    (write elem)
                )
                (write-string " ")
            )
        )
    )
    (nli)
    (write-string "table")
    (++indent)
    (foreach row table
        (nli)
        (*v_table-write* (car row))
        (foreach col (cdr row)
            (write-string ": ")
            (*v_table-write* col)
        )
        (write-string ";")
    )
    (--indent)
    (nli)
    (write-string "endtable")
)

(defmacro v_comment (&rest body)
    (nli)
    (write-string "/*")
    (++indent)
    (nli)
    (foreach word body
        (if (stringp word)
            (write-string word)
            (write word)
        )
        (write-string " ")
    )
    (--indent)
    (nli)
    (write-string "*/")
    nil
)
(defun commentf (body)
    (eval `(v_comment ,@body))
)

(defmacro v_wait (condition)
    (check-in-module)
    (nli)
    (write-string "wait")
    (eval-or-write condition)
    (write-string ";")
    (nli)
    nil)
    
;(v_delay num [nil :pass])
(defmacro v_delay (&rest ticks)
    (check-in-module)
    (let* (
            (last-item (car (last ticks)))
            (is-statement (or (not last-item) (eq :pass last-item)))
            (ticks
                (if is-statement
                    (slice ticks 0 (- (length ticks) 1))
                    ticks
                )
            )
        )
        (if is-statement
            (nli)
        )
        (if (> (length ticks) 1)
            (progn
                (write-string "#(")
                (write-or-eval (car ticks))
                (foreach tick (cdr ticks)
                    (write-string ", ")
                    (write-or-eval tick)
                )
                (write-string ")")
            )
            (progn
                (write-string "#")
                (write (car ticks))
            )
        )
        (if is-statement
            (write-string ";")
        )
    )
    nil
)

(defmacro v_# (&rest ticks)
    `(v_delay ,@ticks)
)

(defmacro v_if (condition yes-body &optional no-body)
    (check-in-module)
    (nli)
    (write-string "if (")
    (eval-or-write condition)
    (write-string ")")
    (nli)
    (write-string "begin")
    (++indent)
    (if (and yes-body (listp yes-body) (atom (car yes-body)))
        (eval yes-body)
        (eval-all yes-body))
    (--indent)
    (nli)
    (write-string "end")

    (when no-body
        (nli)
        (write-string "else")
        (nli)
        (write-string "begin")
        (++indent)
        (if (and no-body (listp no-body) (atom (car no-body)))
            (eval no-body)
            (eval-all no-body))
        (--indent)
        (nli)
        (write-string "end")
        nil)
)

(defmacro v_cond (&rest conds)
    (check-in-module)
    (nli)
    (write-string "if (")
    (eval-or-write (caar conds))
    (write-string ")")
    (nli)
    (write-string "begin")
    (++indent)
    (foreach stmt (cdar conds)
        (eval stmt)
    )
    (--indent)
    (nli)
    (write-string "end")
    (nli)
    (foreach condi (cdr conds)
        (write-string "else")
        (nli)
        (if (not (eq 'else (car condi)))
            (progn
                (write-string "if (")
                (eval-or-write (car condi))
                (write-string ")")
                (nli)
            )
        )
        (write-string "begin")
        (++indent)
        (foreach stmt (cdr condi)
            (eval stmt)
        )
        (--indent)
        (nli)
        (write-string "end")
        (nli)
    )
    nil
)

(defmacro v_ref (name &rest indices)
    ; reference a vector:
    ; (ref name index index) -> name[index : index]
    ; (ref name index) -> name[index]
    (write-or-eval name)
    (write-string "[")
    (write-or-eval (car indices))
    (foreach index (cdr indices)
        (write-string " : ")
        (write-or-eval index)
    )
    (write-string "]")
)

(defmacro v_cat (&rest args)
    ; concatenation:
    ; (cat fred george) -> {fred, george}
    (write-string "{")
    (write-or-eval (car args))
    (foreach arg (cdr args)
        (write-string ", ")
        (write-or-eval arg)
    )
    (write-string "}")
    nil
)

(defmacro v_assign (lvalue expr)
    (nli)
    (write-string "assign ")
    (write-or-eval lvalue)
    (write-string " = ")
    (write-or-eval expr)
    (write-string ";")
)

(defmacro v_<=# (lvalue expr &optional 
                       (transport-delay *transport-delay*)
                       (inertial-delay *inertial-delay*))
    (nli)
    (if inertial-delay 
      (format t "#~a " inertial-delay))
    (write-or-eval lvalue)
    (write-string " <= ")
    (if transport-delay 
      (format t "#~a " transport-delay))
    (write-or-eval expr)
    (write-string ";")
)


(defmacro v_deassign (&rest lvalues)
    (nli)
    (write-string "deassign ")
    (write (car lvalues))
    (foreach lvalue (cdr lvalues)
        (write-string ", ")
        (write-or-eval lvalue)
    )
    (write-string ";")
)

(defmacro v_fork (&rest body)
    (nli)
    (write-string "fork")
    (++indent)
    (foreach bodystmt body
        (eval bodystmt)
    )
    (--indent)
    (nli)
    (write-string "join")
    nil
)

(defmacro v_? (condition yes no)
    (write-string " ( ( ")
    (write-or-eval condition)
    (write-string " ) ? ( ")
    (write-or-eval yes)
    (write-string " ) : ( ")
    (write-or-eval no)
    (write-string ") )")
    nil
)

(defmacro v_++ (&rest variables)
    (foreach varname variables
        (nli)
        (write-all varname " = " varname " + 1;")
        (nli)
    )
    nil
)

(defmacro v_-- (&rest variables)
    (foreach varname variables
        (nli)
        (write-all varname " = " varname " - 1;")
    )
    nil
)

(defmacro v_task (name parameters &rest statements)
    ; (task myfunc ((output (wire a b)) (input (reg c d))) (assign (cat a b) (cat c d)))
    (nli)
    (write-string "task ")
    (write name)
    (write-string ";")
    (++indent)
    (parse-parameters parameters nil)
    (--indent)
    (nli)
    (write-string "begin")
    (++indent)
    (eval-all statements)
    (--indent)
    (nli)
    (write-string "end")
    (nli)
    (write-string "endtask")
    nil
)

(defmacro v_function (&rest args)
    ; (function (1 2) myfunc (a (2 b) c) (assign (cat a b) (cat c d)))
    ; (function myfunc (a (2 b) c) (assign (cat a b) (cat c d)))
    ; # pretend (a (2 b) c) is ((input a (2 b) c))
    (let (
            (range nil)
            (name (first args))
            (inputs (second args))
            (statements (cddr args))
        )
        (if (not (atom (car args)))
            (progn
                (setq range (first args))
                (setq name (second args))
                (setq inputs (third args))
                (setq statements (cdr statements))
            )
        )
        (nli)
        (write-string "function ")
        (if range
            (progn
                (write-string "[")
                (write-or-eval (first range))
                (write-string " : ")
                (write-or-eval (second range))
                (write-string "] ")
            )
        )
        (write name)
        (write-string ";")
        (++indent)
        (parse-parameters (list (cons 'input inputs)) nil)
        (--indent)
        (nli)
        (write-string "begin")
        (++indent)
        (eval-all statements)
        (--indent)
        (nli)
        (write-string "end")
        (nli)
        (write-string "endfunction")
        nil
    )
)

(defmacro v_for ((i expr0) expr1 (i_ expr2) &rest body)
    (check-in-module)
    (nli)
    (write-string "for (")
    (write i)
    (write-string " = ")
    (write-or-eval expr0)
    (write-string "; ")
    (eval expr1)
    (write-string "; ")
    (write i_)
    (write-string " = ")
    (write-or-eval expr2)
    (write-string ")")
    (nli)
    (write-string "begin")
    (++indent)
    (eval-all body)
    (--indent)
    (nli)
    (write-string "end")
)

(defmacro v_fromto (i start end &rest body)
    ; (fromto i 0 64 (v_display i)) := (for (i 0) (< i 64) (i (+ i 1)) (v_display i))
    `(v_for
        (,i ,start)
        (v_< ,i ,end)
        (,i (v_+ ,i 1))
        ,@body
    )
)

(defmacro v_forallbits (i size &rest body)
    `(progn
        (v_reg (,size ,i))
        (v_fromto ,i 0 ,(- (<< 1 (- size 1)) 1)
            ,@body
        )
        ,@body
    )
)

(defmacro v_forever (&rest body)
    (nli)
    (write-string "forever")
    (nli)
    (write-string "begin")
    (++indent)
    (eval-all body)
    (--indent)
    (nli)
    (write-string "end")
)

(defmacro v_dump (fn)
    `(v_initial (v_dumpfile ,fn) (v_dumpvars))
)

(defmacro v_integer (&rest args)
    (check-in-module)
    (foreach arg args
        (if (atom arg)
            (progn
                (nli)
                (write-all "integer " arg ";")
            )
            (foreach inner-arg (cdr arg)
                (nli)
                (write-all "integer " inner-arg " [")
                (if (numberp (car arg))
                    (write (- (car arg) 1))
                    (eval `(v_- ,(car arg) 1))
                )
                (write-all " : 0];")
            )
        )
    )
    nil
)

(defmacro v_parameter (name value)
    (check-in-module)
    (nli)
    (write-all "parameter " name " = ")
    (write-or-eval value)
    (write-string ";")
    nil
)

(defmacro v_reg (&rest names)
    (check-in-module)
    (foreach param names
        (if (atom param)
            (progn
                (nli)
                (write-all "reg " param ";")
            )
            (foreach bus-name (cdr param)
                (if (atom bus-name)
                    (progn
                        ; 1d busses
                        (nli)
                        (write-string "reg [")
                        (if (numberp (car param))
                            (write (- (car param) 1))
                            (eval `(v_- ,(car param) 1))
                        )
                        (write-all " : 0] " bus-name ";")
                    )
                    (foreach bus2-name (cdr bus-name)
                        ; 2d busses
                        (nli)
                        (write-string "reg [")
                        (if (numberp (car param))
                            (write (- (car param) 1))
                            (eval `(v_- ,(car param) 1))
                        )
                        (write-all " : 0] " bus2-name " [")
                        (if (numberp (car bus-name))
                            (write (- (car bus-name) 1))
                            (eval `(v_- ,(car bus-name) 1))
                        )
                        (write-string " : 0];")
                    )
                )
            )
        )
    )
    nil
)

(defmacro v_reg= (name value)
    (check-in-module)
    (if (atom name)
        (progn
            (nli)
            (write-all "reg " name " = ")
            (write-or-eval value)
            (write-string ";"))

        (let ((bus-width (car name))
              (bus-name (cadr name)))

          (if (atom bus-name)
            (progn
              ; 1d busses
              (nli)
              (write-string "reg [")

              (if (numberp bus-width)
                (write (- bus-width 1))

                (eval `(v_- ,bus-width 1)))
              (write-all " : 0] " bus-name " = ")
              (write-or-eval value)
              (write-string ";"))

            (let ((bus-array-n (car bus-name))
                  (bus-name (cadr bus-name))) ; // tricky?

              ; 2d busses
              (nli)
              (write-string "reg [")
              (if (numberp bus-width)
                (write (- bus-width 1))
                (eval `(v_- ,bus-width 1)))

              (write-all " : 0] " bus-name " [")
              (if (numberp bus-array-n)
                (write (- bus-array-n 1))
                (eval `(v_- ,bus-array-n 1)))
              (write-string " : 0] = '{")
              (let ((del " "))
                (dolist (v value)
                  (write-string del)
                  (write-or-eval v)
                  (setf del ", ")))

              (write-string "};")))))
    nil
)

(defmacro v_always (signals &rest body)
    (check-in-module)
    (nli)
    (write-string "always")
    (if signals
        (progn
            (++indent)
            (nli)
            (write-string "@(")
            (if (car signals)
                (if (atom (car signals))
                    (write (car signals))
                    (progn
                        (write (caar signals))
                        (foreach sig-part (cdar signals)
                            (write-string " ")
                            (write sig-part)
                        )
                    )
                )
            )
            (foreach sig (cdr signals)
                (write-string " or ")
                (if (atom sig)
                    (write sig)
                    (progn
                        (write (car sig))
                        (foreach sig-part (cdr sig)
                            (write-string " ")
                            (write sig-part)
                        )
                    )
                )
            )
            (write-string ")")
            (--indent)
        )
    )
    (nli)
    (write-string "begin")
    (++indent)
    (eval-all body)
    (--indent)
    (nli)
    (write-string "end")
    nil
)

(defmacro v_initial (&rest body)
    (check-in-module)
    (nli)
    (write-string "initial")
    (nli)
    (write-string "begin")
    (++indent)
    (eval-all body)
    (--indent)
    (nli)
    (write-string "end")
    nil
)

(foreach name '(repeat while)
    (eval
        `(defmacro ,(mangle name) (expr &rest body)
            (nli)
            (write ',name)
            (write-string " (")
            (write-or-eval expr)
            (write-string ")")
            (nli)
            (write-string "begin")
            (++indent)
            (eval-all body)
            (--indent)
            (nli)
            (write-string "end")
            (nli)
        )
    )
)

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
    (eval
        (let
            (
                (name name_)
                (op name_)
            )
            (if (not (atom name))
                (progn
                    (setq name (car name_))
                    (setq op (cadr name_))
                )
            )
            `(defmacro ,(mangle name) (&rest args)
                (write-string "(") ; )
                (if (> (length args) 1)
                    (cons
                        (write-or-eval (car args))
                        (foreach arg (cdr args)
                            (write-all " " ',op " ")
                            (write-or-eval arg)
                        )
                    )
                    (write-string
                        (strcat
                            (string ',op)
                            (string (car args))
                        )
                    )
                )
                ; (
                (write-string ")")
                nil
            )
        )
    )
)


(foreach name '(
        ; $ macros
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
    (eval
        `(defmacro ,(mangle name) (&rest args)
            (check-in-module)
            (nli)
            (write-string (strcat "$" (string ',name)))
            (if args
                (progn
                    (write-string "(")
                    (write-or-eval (car args))
                    (foreach arg (cdr args)
                        (write-string ", ")
                        (write-or-eval arg)
                    )
                    (write-string ")")
                )
            )
            (write-string ";")
            nil
        )
    )
)

(foreach name '(
        ; $ variables
        time stime
    )
    (eval
        `(defmacro ,(mangle name) ()
            (write-string "$")
            (write ',name)
            nil
        )
    )
)

(defmacro v_timescale (x)
    (nli)
    (write-string "`timescale ")
    (write-string x)
    nil
)

(defmacro v_include (&rest filenames)
    (foreach filename filenames
        (nli)
        (write-string "`include ")
        (write filename)
    )
    nil
)
(defmacro v_define (name value)
    (nli)
    (write-string "`define ")
    (write name)
    (write-string " ")
    (write value)
    nil
)

(defmacro v_. (&rest names)
    (write (car names))
    (foreach name (cdr names)
        (write-string ".")
        (write name)
    )
    nil
)

(foreach name '(wire wand wor trireg)
    ; simple declarators with 1 possible array dimension
    (eval
        `(defmacro ,(mangle name) (&rest args)
            (check-in-module)
            (foreach arg args
                (if (atom arg)
                    (progn
                        (nli)
                        (write-all ',name " " arg ";")
                    )
                    (foreach inner-arg (cdr arg)
                        (nli)
                        (write-all ',name " [")
                        (if (numberp (car arg))
                            (write (- (car arg) 1))
                            (eval `(v_- ,(car arg) 1))
                        )
                        (write-all " : 0] " inner-arg ";")
                    )
                )
            )
            nil
        )
    )
)

(defmacro v_realtime (&rest names)
    ; with args, declare those args to be of type realtime
    ; without args, invoke the $-variable "$realtime"
    (if names
        (progn
            (check-in-module)
            (nli)
            (write-string "realtime ")
            (write (car names))
            (foreach name (cdr names)
                (write-string ", ")
                (write name)
            )
            (write-string ";")
            nil
        )
        (progn
            (write-string "$realtime")
            nil
        )
    )
)

(foreach name '(event real)
    (eval
        `(defmacro ,(mangle name) (&rest names)
            (check-in-module)
            (nli)
            (write-all ',name " ")
            (write (car names))
            (foreach name (cdr names)
                (write-string ", ")
                (write name)
            )
            (write-string ";")
            nil
        )
    )
)

(defvar *primitives* '(
        not or and nand nor xor xnor
        buf bufif0 bufif1 notif0 notif1
        pmos rpmos nmos rnmos cmos rcmos
        tran rtran tranif1 rtranif1 tranif0 rtranif0
        pullup pulldown
    )
)
(foreach primitive *primitives*
    (make-named-module (mangle primitive) primitive)
)

(foreach name '(supply0 supply1)
    (eval
        `(defmacro ,(mangle name) (&rest parameters)
            (parse-parameters (list (cons ',name parameters)) nil)
            nil
        )
    )
)

(foreach case-type '(case casex casez)
    (eval
        `(defmacro ,(mangle case-type) (value &rest cases)
            (check-in-module)
            (nli)
            (write-string (strcat ',case-type "("))
            (write-or-eval value)
            (write-string ")")
            (++indent)
            (foreach case-part cases
                (nli)
                (write-or-eval (car case-part))
                (write-string ":")
                (let* ((remain (cdr case-part))
                       (flag-n (> (length remain) 1)))
                  (if flag-n
                    (write-string " begin"))

                  (++indent)
                  (eval-all (cdr case-part))
                  (--indent)

                  (when flag-n
                      (nli)
                      (write-string "end"))
                )
            )
            (--indent)
            (nli)
            (write-string "endcase")
            nil
        )
    )
)

(foreach* (
        (name '(= n=))
        (symbol '(" = " " <= "))
    )
    (eval
        `(defmacro ,(mangle name) (name wait-or-value &optional value-or-nil nononli)
           (if (not nononli)
            (nli))
            (write-or-eval name)
            (write-string ,symbol)
            (write-or-eval wait-or-value)
            (if value-or-nil
                (write-or-eval value-or-nil)
            )
            (if (or (atom wait-or-value) (not (find (car wait-or-value) '(v_fopen v_fread v_fwrite))))
              (write-string ";"))
            nil
        )
    )
)

(defun write-formatted-num (format size v)
    (assert (<= v (expt 2 size)) nil "~a must be smaller than 2^~a" v size)
    (ccase format
        ('d (format t "~d" v))
        ('b (format t "~v,'0b" size v))
        ('o (multiple-value-bind (a b) (floor size 3)
            (if (= b 0) (format t "~v,'0o" a v)
                         (format t "~o" v))))
        ('h (multiple-value-bind (a b) (floor size 4)
            (if (= b 0) (format t "~v,'0x" a v)
                         (format t "~o" v))))))

(foreach format '(d b h o)
    (eval
        `(defmacro ,(mangle format) (size num)
            (write size)
            (write-string "'")
            (write ',format)
            (if (numberp num)
              (write-formatted-num ',format size num)
              (format t "~a" num))
            nil
        )
    )
)

(foreach name-symbol '(
        (+= +)
        (-= -)
        (*= *)
        (/= /)
        (%= %)
        (^= ^)
        (&= &)
        (bor= bitwise-or)
        (lor= logical-or)
    )
    (let (
            (name (car name-symbol))
            (symbol (cadr name-symbol))
        )
        (eval
            `(defmacro ,(mangle name) (var expr)
                `(v_= ,var
                    (,(mangle ',symbol)
                        ,var
                        ,expr
                    )
                )
            )
        )
    )
)




;;; END VERILISP LANGUAGE
;;; BEGIN PROCESSING OF STDIN

(read-eval (read))

(if *in-module*
    (progn
        (setq indentation 0)
        (nli)
        (write-string "endmodule")
        (nli)
    )
)
