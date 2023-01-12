(load "producer-consumer.lisp")

(setf vga-h-fp (make-instance 'consumer :time-unit '(16 . :clock) :bytes 0))
(setf vga-h-sp (make-instance 'consumer :time-unit '(96 . :clock) :bytes 0))
(setf vga-h-data (make-instance 'consumer :time-unit '(640 . :clock) :bytes (* 480 4)))
(setf vga-h-bp (make-instance 'consumer :time-unit '(48 . :clock) :bytes 0))

(setf vga-h-line (make-instance 'list-consumer :consumers (list vga-h-fp vga-h-sp vga-h-bp vga-h-data)))
(print `(:tt ,(total-time vga-h-line)))

(setf vga-v-data (make-instance 'counter-consumer :consumers vga-h-line :count 480))
(print `(:tt ,(total-time vga-v-data)))

(setf vga-v-fp (make-instance 'consumer :time-unit (cons (* 10 800) :clock) :bytes 0))
(setf vga-v-sp (make-instance 'consumer :time-unit (cons (* 2 800) :clock) :bytes 0))
(setf vga-v-bp (make-instance 'consumer :time-unit (cons (* 33 800) :clock) :bytes 0))

(setf vga-screen (make-instance 'list-consumer :consumers (list vga-v-fp vga-v-sp vga-v-bp vga-v-data)))
(print `(:tt ,(total-time vga-screen)))


