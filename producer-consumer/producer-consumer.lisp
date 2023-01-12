(defparameter *base-clock* (* 27 1000 1000 4))
(defparameter *converter* nil)
(defparameter *unit-second* '((:second . 1)
                              (:sec . 1)
                              (:milli-second . 1000)
                              (:ms . 1)
                              (:micro-second . 1000)
                              (:us . 1)
                              (:nano-second . 1000)
                              (:ns . 1)
                              (:pico-second . 1000)
                              (:ps . 1)))

;----------------------------------------------------------------
; use defmacro for place of assert
(defmacro assert-number>0 (space)
  `(let ((value ,space)
         (sym ',space))
     (assert (and (numberp value)
               (> value 0)) nil "~s is a digit higher than 0. value:'~a(~a)'" 
                (if (symbolp sym) (string-downcase (symbol-name sym)) sym) value (if (numberp value) "number" (string-downcase (type-of value))))))

(defmacro assert-number>=0 (space)
  `(let ((value ,space)
         (sym ',space))
     (assert (and (numberp value)
               (>= value 0)) nil "~s is a number greater than or equal to 0. value:'~a(~a)'" 
                (if (symbolp sym) (string-downcase (symbol-name sym)) sym) value (if (numberp value) "number" (string-downcase (type-of value))))))

(defmacro assert-time-unit (time-unit)
  `(assert (and (listp ,time-unit) (not (null ,time-unit)) (atom (cdr ,time-unit))) nil "time-unit error: ~a~a" ,time-unit
    (if (listp (cdr ,time-unit)) (format nil "~%Please use cons to make time-unit.") "")))

;----------------------------------------------------------------
(defmacro multiple-time-unit (time-unit-lst time-unit &rest body)
  `(let ((,(car time-unit-lst) (car ,time-unit))
         (,(cadr time-unit-lst) (cdr ,time-unit)))
     (assert-time-unit ,time-unit)
     ,@body))

;----------------------------------------------------------------
(defun find-convert-scale (unit0 unit1 scaler &optional rev)
    (let* ((unit0-list (member unit0 scaler :key #'car))
           (unit1r0-list (member unit1 (reverse unit0-list) :key #'car)))
        (if unit1r0-list
            (let ((rv 1))
              (dolist (unit-scale unit1r0-list)
                (let ((unit (car unit-scale))
                      (scale (cdr unit-scale)))
                  (setf rv (* rv scale))))
              (if rev (/ 1 rv) rv))
            (if (null rev)
              (find-convert-scale unit1 unit0 scaler t)))))

;----------------------------------------------------------------
#|
(print `(:sec-to-ms 
          ,(find-convert-scale :sec :ms *unit-second*)))
(print `(:ms-to-sec 
          ,(find-convert-scale :ms :sec *unit-second*)))
(print `(:ms-to-ms 
          ,(find-convert-scale :ms :ms *unit-second*)))
(print `(:ms-to-no 
          ,(find-convert-scale :ms :no *unit-second*)))
(quit)
|#

;----------------------------------------------------------------
(defun register-converter (units converter)
    (push (cons units converter) *converter*))

(defclass converter ()
    ((units :initarg :units :initform nil :reader units)))

(defmethod initialize-instance :after ((obj converter) &rest initargs)
    (let ((units (units obj)))
        (assert (and (not (null units))
                     (keywordp (car units))
                     (keywordp (cdr units)))
                    nil "units must be keyword cons. ~a~%~a" units
                    (if (listp (cdr units))
                        "Please use cons to make a unit pair."
                        ""))
        ;(register-converter units obj)
        ))

(defclass second-clock-converter (converter)
    ((units :initarg :units :initform `(:second . :clock) )))

(defmethod convert ((obj second-clock-converter) time-unit)
    (multiple-time-unit (time unit) time-unit
        (let* ((from-unit (car (units obj)))
               (to-unit (cdr (units obj)))
               (scale (if (eq from-unit unit) 1 (find-convert-scale unit from-unit *unit-second*))))
          (print `(:scale ,scale ,from-unit ,unit ,(find-convert-scale unit from-unit *unit-second* ) ,(* scale time *base-clock*)))
          (assert (numberp scale) nil "I cannot convert from ~a to ~a" from-unit to-unit)
          (let ((new-time (* time scale *base-clock*)))
            (cons new-time to-unit)))))

(defmethod ~convert ((obj second-clock-converter) time-unit)
  (multiple-time-unit (time unit) time-unit
        (let ((from-unit (car (units obj)))
              (to-unit (cdr (units obj))))
            (assert (eq from-unit unit))
            (let ((new-time (/ time *base-clock*)))
              (cons new-time to-unit)))))

(defun convert-time-unit (time-unit unit)
)

;----------------------------------------------------------------
#|
(setf conv (make-instance 'second-clock-converter))
(print `(:donv ,(convert conv '(1 . :us))))
(quit)
|#
    
;----------------------------------------------------------------
;(:nano-sec :MHz :count)
(defclass consumer-base ()
  ((hard-real-time :initarg :hard-real-time :initform nil :accessor hard-real-time)
   ;(burst-n :initarg :burst-n :initform 1 :accessor burst-n)
   )
  (:documentation "consumer のベースになるクラス"))

(defmethod has-child ((obj consumer-base))
    nil)

(defun unit* (time-unit scala)
  (assert-time-unit time-unit)
  (assert (numberp scala) nil "second argument must be number. ~a(~a)" scala (string-downcase (type-of scala)))

  (multiple-time-unit (time unit) time-unit (cons (* time scala) unit)))

(defun unit+ (time-unit0 time-unit1)
  (assert-time-unit time-unit1)

  (if (and (numberp time-unit0) (= time-unit0 0))
    time-unit1

    (progn
      (assert-time-unit time-unit0)

      (multiple-time-unit (time0 unit0) time-unit0
        (multiple-time-unit (time1 unit1) time-unit1
          (assert (eq unit0 unit1) ((cdr time-unit0) (cdr time-unit1)) "must be same unit. ~a ~a" unit0 unit1)
          (cons (+ time0 time1) unit0))))))

;----------------------------------------------------------------
(defclass consumer(consumer-base)
  ((consume-bytes :initarg :bytes :initform nil :accessor bytes)
   (time-unit :initarg :time-unit :initform nil :accessor time-unit)))

(defmethod initialize-instance :after ((obj consumer) &rest initargs)
    (let ((time-unit (time-unit obj)))
      (assert (and (listp time-unit) (not (null time-unit)) (atom (cdr time-unit))) (time-unit obj) "error ~a" time-unit)
      (let ((time (car time-unit))
            (unit (cdr time-unit)))
        (assert-number>0 time)))
    (assert-number>=0 (bytes obj)))

(defmethod total-time ((obj consumer))
    (time-unit obj))
            
;----------------------------------------------------------------
(defclass meta-consumer (consumer-base)
    ((consumers :initarg :consumers :initform nil)))

(defmethod has-child ((obj meta-consumer))
    (length (slot-value obj 'consumers)))

;----------------------------------------------------------------
(defclass counter-consumer (meta-consumer) 
  ((count :initarg :count :initform nil)))

(defmethod initialize-instance :after ((obj counter-consumer) &rest initargs)
    (let ((count (slot-value obj 'count)))
      (assert (and (numberp count)
                   (> count 0)) (count) "count is a digit higher than 0:'~a'" count)))

(defmethod total-time ((obj counter-consumer))
    (let ((total-time (total-time (slot-value obj 'consumers))))
        (unit* total-time (slot-value obj 'count))))

;----------------------------------------------------------------
(defclass list-consumer (meta-consumer)
    ())

(defmethod initialize-instance :after ((obj list-consumer) &rest initargs)
    (let ((consumers (slot-value obj 'consumers)))
        (dolist (c consumers)
            (assert (subtypep (class-of c) 'consumer-base)))))

(defmethod total-time ((obj list-consumer))
    (let ((consumers (slot-value obj 'consumers))
          (rv 0))
        (dolist (c consumers)
            (setf rv (unit+ rv (total-time c))))
        rv))
            

;----------------------------------------------------------------
#|
(setf c0 (make-instance 'consumer :time-unit '(20 . :clock) :bytes '100))

(print `(:consumer ,(hard-real-time c0)))
(print `(:consumer ,(total-time c0)))
(setf (hard-real-time c0) nil)
(print `(:consumer ,c0 ,(has-child c0)))

(setf xc0 (make-instance 'counter-consumer :consumers c0 :count 5))
(print `(:xc ,(total-time xc0) ,(has-child xc0)))

(print `(:aa ,(unit* (total-time xc0) 4)))
(print `(:aa ,(unit* (total-time xc0) 4)))

(multiple-time-unit (a b) '(20 . :clock) (print `(:ab ,a ,b)))

(print `(:unit+ ,(unit+ '(20 . :clock) '(30 . :clocks))))
|#
