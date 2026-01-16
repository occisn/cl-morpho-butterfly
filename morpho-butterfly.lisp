(eval-when (:compile-toplevel)
  (ql:quickload :zpng)
  (ql:quickload :lparallel))

(defpackage butterfly1
  (:use :cl :lparallel))

(in-package :butterfly1)

(declaim (optimize (speed 3) (debug 0) (safety 0)))

(defconstant +width+ 2000
  "width of the picture, in pixels")
(defconstant +height+ 1100
  "height of the picture, in pixels")
(defconstant +nb-cores+ 18
  "Number of cores")
(declaim (type fixnum +width+ +height+ +nb-cores+))

(defmacro exp-exp (x)
  "Expand into (exp (- (exp X))), or 0 if X or Y is above 85 (in order to avoid overflow)."
  `(if (> ,x 85.0d0)
       0.0d0
       (exp (- (exp ,x)))))

(defmacro exp-exp+exp (x y)
  "Expand into (exp (- (+ (exp X) (exp Y))), or 0 if X or Y is above 85 (in order to avoid overflow)."
  `(if (or (> ,x 85.0d0) (> ,y 85.0d0))
       0.0d0
       (exp (- (+ (exp ,x) (exp ,y))))))

(declaim (ftype (function (double-float double-float) double-float) C))
(declaim (inline C))
(defun C (x y)
  "Return C(x,y)."
  (declare (type double-float x y))
  (expt
   (sin (+ (* 14 (atan (/ (* 100 (+ y
                                    (/ x 4.0d0)
                                    (- (/ 1.0d0 25.0d0))))
                          (+ (abs (- (* 100.0d0 x)
                                     (* 25.0d0 y)
                                     (* 3.0d0 (atan (- (* 100.0d0 x)
                                                       (* 25.0d0 y))))))
                             1.0d0))))
           (* 14 (abs (- (/ x 2.0d0)
                         (/ y 8.0d0))))))
   4))

(declaim (ftype (function (fixnum double-float double-float) double-float) K))
(declaim (inline K))
(defun K (v x y)
  "Return K(v,x,y)."
  (declare (type fixnum v)
           (type double-float x y))
  (let ((sum1 0.0d0))
    (declare (type double-float sum1))
    (loop for s of-type fixnum from 1 to 60
          do (let ((exp0
                     (- (* 25
                           (- (* (expt
                                  (sin
                                   (+ (sin (* 2.0d0 s))
                                      (* (+ 6 (sin (* 1.0d0 s s)))
                                         (+ (* (sin (* 7.0d0 s)) (/ x 2.0d0))
                                            (* (cos (* 7.0d0 s)) (/ (- y 8) 2.0d0))))))
                                  10)
                                 (expt
                                  (sin
                                   (+ (sin (* 3.0d0 s))
                                      (* (+ 6 (* 2 (sin (* 1.0d0 s s))))
                                         (- (* (sin (* 7.0d0 s)) (/ (- y 8) 2.0d0))
                                            (* (cos (* 7.0d0 s)) (/ x 2.0d0))))))
                                  10.0d0))
                              0.1d0)))))
               (declare (type double-float exp0))
               (incf sum1
                     (* (/ 5.0d0 2.0d0)
                        (+ (/ 2 25.0d0)
                           (* (/ 3 50.0d0)
                              (cos (* s (+ 4.0d0 (* 4.0d0 v))))))
                        (* (+ (sin (* 5.0d0 s))
                              (sin (* 2.0d0 s))
                              3.0d0)
                           (/ 1.0d0 5.0d0))
                        (exp-exp exp0)))))
    sum1))

(declaim (ftype (function (fixnum double-float double-float double-float) double-float) A))
(declaim (inline A))
(defun A (v x y Cxy)
  "Return A(v,x,y)."
  (declare (type fixnum v))
  (declare (type double-float x y Cxy))
  (exp
   (+
    (- (exp
        ;; first exponential:
        (* 200.0d0
           (+ y
              (/ x 4.0d0)
              (* v (/ 1.0d0 50.0d0)) 
              (- (* .25d0
                    (abs
                     (sin
                      (* (/ 12.0d0 5.0d0)
                         (+ (* .7d0 (abs (- x (/ y 4.0d0))))
                            (* .3d0 (sqrt (abs (- x (/ y 4.0d0)))))))))))))))
    (- (exp
        ;; second exponential:
        (- (* 200.0d0
              (+ y
                 (/ x 4.0d0)
                 (/ 7.0d0 20.0d0)
                 (- (* (* v 7.0d0) (/ 1.0d0 50.0d0)))
                 (* 0.2d0 (atan (* 6.0d0 (abs (- x (/ y 4.0d0))))))
                 (* 0.2d0 (atan (* 40.0d0 (abs (- x (/ y 4.0d0))))))
                 (- (* (/ 23.0d0 20.0d0)
                       (+ 1.5d0
                          (* (/ 1.0d0 25.0d0)
                             (cos
                              (* 10.0d0
                                 (+ y
                                    (/ x 4.0d0)
                                    (/ 6.0d0 25.0d0)))))
                          (* 0.03d0 Cxy)
                          (* 0.3d0
                             (atan (* 30.0d0
                                      (+ y (/ x 4.0d0) (- 0.25d0))))))
                       (abs (- x (/ y 4.0d0)))))))))))))

(declaim (ftype (function (double-float double-float) double-float) E))
(declaim (inline E))
(defun E (x y)
  "Return E(x,y)."
  (declare (type double-float x y))
  (- 1.0d0
     (* (exp-exp
         (+ (- (* 100.0d0 (expt (+ (* 3 y) (* 0.75d0 x) 0.27d0) 4)))
            (- (* 100.0d0 (expt
                           (abs
                            (* 7
                               (+ 1.0d0
                                  (/ 1.0d0
                                     (+ (sqrt
                                         (the (double-float 0.0d0)
                                              (abs
                                               (+ (* 100 y)
                                                  (* 25 x)
                                                  (- 6)))))
                                        0.3d0)))
                               (- x (/ y 4.0d0))))
                           (+ (* 3 y) (* 0.75d0 x) 2.27d0))))
            10.0d0))
        (- 1.0d0
           (exp-exp+exp
            (+ (* 200
                  (abs
                   (+ y
                      (/ x 4.0d0)
                      (- 0.2d0)
                      (* 3 (- x (/ y 4.0d0)) (- x (/ y 4.0d0))))))
               -32.0d0)
            (+ (* 500
                  (abs
                   (+ y
                      (/ x 4.0d0)
                      (- (/ 1.0d0 20.0d0))
                      (- (* 0.7d0 (sqrt (abs (- x (/ y 4.0d0)))))))))
               -2.5d0))))))

(declaim (ftype (function (double-float double-float) double-float) L))
(declaim (inline L))
(defun L (x y)
  "Return L(x,y)."
  (declare (type double-float x y))
  (let ((sum1 0.0d0))
    (declare (type double-float sum1))
    (loop for s of-type fixnum from 1 to 25
          do
             (incf sum1
                   (expt
                    (sin (+ (* (+ 80 (* 30 (sin (* 1.0d0 s s))))
                               (atan (/ (- (+ (* 100.0d0 y) (* 25.0d0 x))
                                           (* 4.0d0 (sin (* 1.0d0 s))))
                                        (+ (abs (- (* 100.0d0 x)
                                                   (* 25.0d0 y)
                                                   (* 3.0d0 (atan (- (* 100.0d0 x)
                                                                     (* 25.0d0 y))))))
                                           1.0d0))))
                            (abs (- (/ x 2.0d0) (/ y 8.0d0)))
                            (* 4.0d0 (sin (* 5.0d0 s)))))
                    6)))
    sum1))

(declaim (ftype (function (double-float double-float double-float) double-float) W))
(declaim (inline W))
(defun W (x y Cxy)
  "Return W(x,y)."
  (declare (type double-float x y Cxy))
  
  (+ (* (- (exp-exp+exp
            (+ (- (* 40 Cxy))
               (/ 196.0d0 5.0d0)
               (* (/ 4.0d0 5.0d0)
                  (sqrt (the (double-float 0.0d0)
                             (+ (* (- x (/ y 4.0d0))
                                   (- x (/ y 4.0d0)))
                                (* (+ y (/ x 4.0d0))
                                   (+ y (/ x 4.0d0))))))))
            (- (* 40
                  (+ (* 5 (abs
                           (+ y
                              (/ x 4.0d0)
                              (- (/ 3 50.0d0))
                              (* (/ 1.0d0 3.0d0)
                                 (- x (/ y 4.0d0))
                                 (- x (/ y 4.0d0))))))
                     (expt (abs (- (* 2 x) (/ y 2.0d0))) 3)
                     (- (/ 2.0d0 5.0d0)))))))
        (- 1 (exp-exp+exp
              (+
               (- (* 1000
                     (+ (abs (- x (/ y 4.0d0))))))
               100.0d0
               (- (* 90.0d0 (atan (+ (* 8 y)
                                     (* 2 x)
                                     (/ 8 5.0d0))))))
              (* 1000 (+ (abs (- x (/ y 4.0d0)))
                         (- (/ 7 50.0d0))
                         (* (/ 9.0d0 20.0d0)
                            (+ y (/ x 4.0d0) 0.2d0)))))))
     (- (exp-exp (* 70 (+ (abs
                           (+
                            (* 5
                               (abs
                                (+ y
                                   (/ x 4.0d0)
                                   (- (/ 3 50.0d0))
                                   (* (/ 1.0d0 3.0d0)
                                      (- x (/ y 4.0d0))
                                      (- x (/ y 4.0d0))))))
                            (expt (abs (- (* 2 x) (/ y 2.0d0))) 3)
                            (- (/ 2 5.0d0))))
                          (- (/ 1.0d0 200.0d0))))))
     (- (exp-exp (+ (* 700
                       (abs
                        (+
                         (abs (- x (/ y 4.0d0)))
                         (- 0.1d0)
                         (* 0.09d0 (atan (* 8
                                            (+ y
                                               (/ x 4.0d0)
                                               (/ 1 5.0d0))))))))
                    (- (/ 21 20.0d0)))))
     1.0d0))

(declaim (ftype (function (fixnum double-float double-float) double-float) H))
(declaim (inline H))
(defun H (v x y)
  "Return H(v,x,y)."
  (declare (type fixnum v)
           (type double-float x y))

  (let* ((Cxy (C x y))
         (Exy (E x y))
         (A0xy (A 0 x y Cxy))
         (Lxy (L x y))
         (Kxy (K v x y))
         (Wxy (W x y Cxy)))
    (declare (type double-float Cxy Exy Lxy Wxy Kxy A0xy))

    (+
     ;; first term:
     (* (* (+ 18 (- (* 9.0d0 v)) (* 1.0d0 v v)) (/ 1.0d0 20.0d0))
        (- 1.0d0 A0xy)
        (- 1.0d0 Exy)
        Kxy) 
     ;; second term:
     (* (* (+ 2 (* 3.0d0 v)) (/ 1.0d0 5.0d0))
        (* A0xy
           (A 1 x y Cxy)
           (- 1 Exy)
           (* (+ 50.0d0 Lxy) (/ 1.0d0 50.0d0))
           (exp-exp+exp
            (+ (* 2 y)
               (* 0.5d0 x)
               (/ 2.0d0 5.0d0)
               (- (* 2.0d0 (abs (- x (/ y 4.0d0))))))
            (+ (* 8 y)
               (* 2 x)
               (/ 2.0d0 5.0d0)
               (- (abs (- (* 8 x) (* 2 y))))))
           Wxy))
     ;; third term:
     (exp-exp (- (* 50.0d0
                    (- (* (expt (cos (+ (* 2 y) 
                                        (* x 0.5d0)
                                        (/ 7 5.0d0)
                                        (- (abs (- (* 2 x)
                                                   (* y 0.5d0))))))
                                80)
                          (expt (sin (+ (* 20 y)
                                        (* 5 x)
                                        (abs (- (* 20 x) (* 5 y)))))
                                2))
                       (expt (+ (* 2.7d0 y)
                                (* (* 27 x) (/ 1.0d0 40.0d0))
                                (/ 81.0d0 250.0d0)
                                )
                             10)
                       (/ 49.0d0 50.0d0)))))
     ;; fourth term:
     (* 0.1d0 Exy (* (- v 1.0d0) (- v 1.0d0))))))

(declaim (ftype (function (double-float) fixnum) F))
(declaim (inline F))
(defun F (x)
  "Return F(x)."
  (declare (type double-float x))
  (let ((a (- (* 1000.0 x)))
        (b (* 1000.0 (- x 1.0))))
    (declare (type double-float a b))
    (floor
     (* 255.0
        (exp-exp a)
        (expt (abs x) (exp-exp b))))))

(defun draw-pic-from-rgb-arrays (height width r-array g-array b-array export-file)
  "Export picture to file from RGB arrays."
  (declare (type fixnum height width)
           (type (simple-array fixnum) r-array g-array b-array))
  (let* ((alpha 255)
         (png (make-instance 'zpng:pixel-streamed-png
                             :color-type :truecolor-alpha
                             :width width
                             :height height)))
    (declare (type fixnum alpha))
    (with-open-file (stream export-file
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create
			    :element-type '(unsigned-byte 8))
      (zpng:start-png png stream)
      (loop for n of-type fixnum from 1 to height do
        (loop for m of-type fixnum from 1 to width do
          (let ((r (aref r-array (- n 1) (- m 1)))
                (g (aref g-array (- n 1) (- m 1)))
                (b (aref b-array (- n 1) (- m 1))))
            (declare (type fixnum r g b))
            (zpng:write-pixel (list r g b alpha) png))))
      (zpng:finish-png png))))

(defun run1 ()
  "Main function. Create butterfly.png"
  
  (format t "~%1) Create RGB arrays...~%")
  (let* ((export-file "pics/butterfly1.png")
         (r-array (make-array `(,+height+ ,+width+) :element-type 'fixnum :initial-element 0))
         (g-array (make-array `(,+height+ ,+width+) :element-type 'fixnum :initial-element 0))
         (b-array (make-array `(,+height+ ,+width+) :element-type 'fixnum :initial-element 0))
         (nb-cores +nb-cores+)
         (nb-chunks (* nb-cores 4))                  
         (chunk-size (ceiling +height+ nb-chunks))
         (real-base (get-internal-real-time))
         (duration 0))
    (declare (type fixnum nb-cores nb-chunks chunk-size)
             (type (simple-array fixnum) r-array g-array b-array))

    (format t "~%2) Calculate RGB components...~%")
    (setq lparallel:*kernel* (lparallel:make-kernel nb-cores))
    (lparallel:pmap
     nil
     (lambda (chunk-idx)
       (declare (type fixnum chunk-idx))
       (let* ((chunk-start (* chunk-idx chunk-size))
              (chunk-end  (min (the fixnum (- +height+ 1))
                               (the fixnum (- (the fixnum (+ chunk-start chunk-size)) 1)))))
         (declare (type fixnum chunk-start chunk-end))
         (format t "[thread #~A] computes task #~A [~D .. ~D]~%"
                 (lparallel.kernel:kernel-worker-index)
                 chunk-idx
                 chunk-start chunk-end)
         (dotimes (n0 (the fixnum (+ (the fixnum (- chunk-end chunk-start)) 1)))
           (declare (type fixnum n0))
           (let ((n (+ n0 chunk-start 1)))
             (declare (type fixnum n))
             (dotimes (m0 +width+)
               (declare (type fixnum m0))
               (let* ((m (1+ m0))
                      (x (* (- m 1000.0d0) (/ 1.0d0 960.0d0)))
                      (y (* (- 451.0d0 n) (/ 1.0d0 960.0d0)))
                      (r (F (H 0 x y)))
                      (g (F (H 1 x y)))
                      (b (F (H 2 x y))))
                 (declare (type double-float x y)
                          (type fixnum m r g b))
                 (setf (aref r-array (- n 1) (- m 1)) r)
                 (setf (aref g-array (- n 1) (- m 1)) g)
                 (setf (aref b-array (- n 1) (- m 1)) b)))))))
     (loop for i from 0 below nb-chunks collect i))
    
    (locally
        (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
      (setq duration (/ (- (get-internal-real-time) real-base) internal-time-units-per-second 1.0)))
    
    (format t "~%3) Export pic...~%")
    (draw-pic-from-rgb-arrays +height+ +width+ r-array g-array b-array export-file)
    
    (format t "Done in ~f seconds.~%" duration)
    duration))

(defun benchmark-5-times ()
  ""
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  
  (let ((nb-runs 1)
        (durations '()))
    (dotimes (i nb-runs)
      (format t "~%~%Run ~D / ~D:~%" (1+ i) nb-runs)
      (let ((duration (run1)))
        (format t "Run ~D / ~D: ~F seconds~%" (1+ i) nb-runs duration)
        (push duration durations)))
    (setq durations (sort durations #'<))
    (let ((quickest (car durations))
          (second-best (cadr durations))
          (slowest (car (last durations))))
      (locally
          (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
        (format t "~%RESULTS:~%")
        (dotimes (i nb-runs)
          (format t "Run ~D / ~D: ~F seconds~%" (1+ i) nb-runs (nth (- nb-runs 1 i) durations)))
        (format t "=> quickest duration: ~,4F seconds~%" quickest)
        (format t "=> second best:       ~,4F seconds~%" second-best)
        (format t "=> slowest duration:  ~,4F seconds = quickest + ~a %~%" slowest (truncate (* 100 (/ (- slowest quickest) quickest)))))
      nil)))

(defun draw-heatmap-from-values (height width value-array export-file)
  "Create png file with heatmap from value-array.
Algorithm to convert value to HSL then RGB is inspired by: https://stackoverflow.com/questions/17525215/calculate-color-values-from-green-to-red/30612603#30612603"
  (declare (type fixnum height width)
           (type (simple-array double-float) value-array))

  (labels ((hue-to-rgb (p q tt)
             (declare (type double-float p q tt))
             (when (< tt 0.0d0) (setq tt (+ tt 1.0d0)))
             (when (> tt 1.0d0) (setq tt (- tt 1.0d0)))
             (cond ((< tt (/ 1.0d0 6.0d0))
                    (+ p (* (- q p) 6.0d0 tt)))
                   ((< tt 0.5d0) q)
                   ((< tt (/ 2.0d0 3.0d0))
                    (+ p (* (- q p) (- (/ 2.0d0 3.0d0) tt) 6.0d0)))
                   (t p))))

    (declare (ftype (function (double-float double-float double-float) double-float) hue-to-rgb))
    
    (let* ((r-array (make-array (list height width) :element-type 'fixnum :initial-element 0))
           (g-array (make-array (list height width) :element-type 'fixnum :initial-element 0))
           (b-array (make-array (list height width) :element-type 'fixnum :initial-element 0))
           (value-min 0.0d0)
           (value-max 0.0d0)
           (value-range 0.0d0))
      (declare (type (simple-array fixnum) r-array g-array b-array)
               (type double-float value-min value-max value-range))
      
      ;; (1) calculate values min, max and range
      (setq value-min (aref value-array 0 0))
      (setq value-max (aref value-array 0 0))
      (loop for n of-type fixnum from 1 to height do
        (loop for m of-type fixnum from 1 to width do
          (let ((z (aref value-array (- n 1) (- m 1))))
            (declare (type double-float z))
            (when (> z value-max) (setq value-max z))
            (when (< z value-min) (setq value-min z)))))
      (setq value-range (- value-max value-min))

      ;; (2) calculate RGB arrays
      (loop for n of-type fixnum from 1 to height do
        (loop for m of-type fixnum from 1 to width do
          (let* ((z (aref value-array (- n 1) (- m 1)))
                 (w (/ (- z value-min) value-range)) ; between 0 and 1
                 (h (* (* w 1.2d0) (/ 1.0d0 3.60d0))) ; hie between 0° and 120°/360°
                 (s 1.0d0)                  ; saturation
                 (l 0.5d0)                  ; lightness
                 (q (- (+ l s) (* l s)))
                 (p (- (* 2 l) q))
                 (r (truncate (* 255 (hue-to-rgb p q (+ h (/ 1.0d0 3.0d0))))))
                 (g (truncate (* 255 (hue-to-rgb p q h))))
                 (b (truncate (* 255 (hue-to-rgb p q (- h (/ 1.0d0 3.0d0)))))))
            (declare (type double-float z w h s l p q)
                     (type fixnum r g b))
            (setf (aref r-array (- n 1) (- m 1)) r)
            (setf (aref g-array (- n 1) (- m 1)) g)
            (setf (aref b-array (- n 1) (- m 1)) b))))

      ;; (3) create heatmap picture
      (draw-pic-from-rgb-arrays height width r-array g-array b-array export-file))))

(defun run2 ()
  "Main function. Create butterfly.png"

  (format t "~%1) Create arrays...~%")
  (let* ((r-array (make-array `(,+height+ ,+width+) :element-type 'fixnum :initial-element 0))
         (g-array (make-array `(,+height+ ,+width+) :element-type 'fixnum :initial-element 0))
         (b-array (make-array `(,+height+ ,+width+) :element-type 'fixnum :initial-element 0))
         (C-array (make-array `(,+height+ ,+width+) :element-type 'double-float :initial-element 0.0d0))
         (E-array (make-array `(,+height+ ,+width+) :element-type 'double-float :initial-element 0.0d0))
         (L-array (make-array `(,+height+ ,+width+) :element-type 'double-float :initial-element 0.0d0))
         (W-array (make-array `(,+height+ ,+width+) :element-type 'double-float :initial-element 0.0d0))
         (A0-array (make-array `(,+height+ ,+width+) :element-type 'double-float :initial-element 0.0d0))
         (A1-array (make-array `(,+height+ ,+width+) :element-type 'double-float :initial-element 0.0d0))
         (K0-array (make-array `(,+height+ ,+width+) :element-type 'double-float :initial-element 0.0d0))
         (K1-array (make-array `(,+height+ ,+width+) :element-type 'double-float :initial-element 0.0d0))
         (K2-array (make-array `(,+height+ ,+width+) :element-type 'double-float :initial-element 0.0d0))
         (H0-array (make-array `(,+height+ ,+width+) :element-type 'double-float :initial-element 0.0d0))
         (H1-array (make-array `(,+height+ ,+width+) :element-type 'double-float :initial-element 0.0d0))
         (H2-array (make-array `(,+height+ ,+width+) :element-type 'double-float :initial-element 0.0d0))
         (nb-cores +nb-cores+)
         (nb-chunks (* nb-cores 4))                  
         (chunk-size (ceiling +height+ nb-chunks))
         (real-base (get-internal-real-time))
         (duration 0))
    (declare (type fixnum nb-cores nb-chunks chunk-size))
    (declare (type (simple-array fixnum) r-array g-array b-array)
             (type (simple-array double-float) C-array E-array L-array W-array))

    (format t "~%2) Calculate...~%")
    (setq lparallel:*kernel* (lparallel:make-kernel nb-cores))
    (lparallel:pmap
     nil
     (lambda (chunk-idx)
       (declare (type fixnum chunk-idx))
       (let* ((chunk-start (* chunk-idx chunk-size))
              (chunk-end  (min (the fixnum (- +height+ 1))
                               (the fixnum (- (+ chunk-start chunk-size) 1)))))
         (declare (type fixnum chunk-start chunk-end))
         (format t "[thread #~A] computes task #~A [~D .. ~D]~%"
                 (lparallel.kernel:kernel-worker-index)
                 chunk-idx
                 chunk-start chunk-end)
         (dotimes (n0 (the fixnum (+ (the fixnum (- chunk-end chunk-start)) 1)))
           (declare (type fixnum n0))
           (let ((n (+ n0 chunk-start 1)))
             (declare (type fixnum n))
             (dotimes (m0 +width+)
               (declare (type fixnum m0))
               (let* ((m (1+ m0))
                      (x (* (- m 1000.0d0) (/ 1.0d0 960.0d0)))
                      (y (* (- 451.0d0 n) (/ 1.0d0 960.0d0)))
                      (Cxy (expt
                            (sin (+ (* 14 (atan (/ (* 100 (+ y
                                                             (/ x 4.0d0)
                                                             (- (/ 1.0d0 25.0d0))))
                                                   (+ (abs (- (* 100.0d0 x)
                                                              (* 25.0d0 y)
                                                              (* 3.0d0 (atan (- (* 100.0d0 x)
                                                                                (* 25.0d0 y))))))
                                                      1.0d0))))
                                    (* 14 (abs (- (/ x 2.0d0)
                                                  (/ y 8.0d0))))))
                            4))
                      (Exy (- 1.0d0
                              (* (exp-exp (+ (- (* 100.0d0 (expt (+ (* 3 y) (* 0.75d0 x) 0.27d0) 4)))
                                             (- (* 100.0d0 (expt
                                                            (abs
                                                             (* 7
                                                                (+ 1.0d0
                                                                   (/ 1.0d0
                                                                      (+ (sqrt
                                                                          (the (double-float 0.0d0)
                                                                               (abs
                                                                                (+ (* 100 y)
                                                                                   (* 25 x)
                                                                                   (- 6)))))
                                                                         0.3d0)))
                                                                (- x (/ y 4.0d0))))
                                                            (+ (* 3 y) (* 0.75d0 x) 2.27d0))))
                                             10.0d0))
                                 (- 1.0d0
                                    (exp-exp+exp
                                     (+ (* 200
                                           (abs
                                            (+ y
                                               (/ x 4.0d0)
                                               (- 0.2d0)
                                               (* 3 (- x (/ y 4.0d0)) (- x (/ y 4.0d0))))))
                                        -32.0d0)
                                     (+ (* 500
                                           (abs
                                            (+ y
                                               (/ x 4.0d0)
                                               (- (/ 1.0d0 20.0d0))
                                               (- (* 0.7d0 (sqrt (abs (- x (/ y 4.0d0)))))))))
                                        -2.5d0))))))
                      (v 0)
                      (A0xy (exp
                             (+
                              (- (exp
                                  (* 200.0d0
                                     (+ y
                                        (/ x 4.0d0)
                                        (* v (/ 1.0d0 50.0d0)) 
                                        (- (* .25d0
                                              (abs
                                               (sin
                                                (* (/ 12.0d0 5.0d0)
                                                   (+ (* .7d0 (abs (- x (/ y 4.0d0))))
                                                      (* .3d0 (sqrt (abs (- x (/ y 4.0d0)))))))))))))))
                              (- (exp
                                  (- (* 200.0d0
                                        (+ y
                                           (/ x 4.0d0)
                                           (/ 7.0d0 20.0d0)
                                           (- (* (* v 7.0d0) (/ 1.0d0 50.0d0)))
                                           (* 0.2d0 (atan (* 6.0d0 (abs (- x (/ y 4.0d0))))))
                                           (* 0.2d0 (atan (* 40.0d0 (abs (- x (/ y 4.0d0))))))
                                           (- (* (/ 23.0d0 20.0d0)
                                                 (+ 1.5d0
                                                    (* (/ 1.0d0 25.0d0)
                                                       (cos
                                                        (* 10.0d0
                                                           (+ y
                                                              (/ x 4.0d0)
                                                              (/ 6.0d0 25.0d0)))))
                                                    (* 0.03d0 Cxy)
                                                    (* 0.3d0
                                                       (atan (* 30.0d0
                                                                (+ y (/ x 4.0d0) (- 0.25d0))))))
                                                 (abs (- x (/ y 4.0d0)))))))))))))
                      (v 1)
                      (A1xy (exp
                             (+
                              (- (exp
                                  (* 200.0d0
                                     (+ y
                                        (/ x 4.0d0)
                                        (* v (/ 1.0d0 50.0d0)) 
                                        (- (* .25d0
                                              (abs
                                               (sin
                                                (* (/ 12.0d0 5.0d0)
                                                   (+ (* .7d0 (abs (- x (/ y 4.0d0))))
                                                      (* .3d0 (sqrt (abs (- x (/ y 4.0d0)))))))))))))))
                              (- (exp
                                  (- (* 200.0d0
                                        (+ y
                                           (/ x 4.0d0)
                                           (/ 7.0d0 20.0d0)
                                           (- (* (* v 7.0d0) (/ 1.0d0 50.0d0)))
                                           (* 0.2d0 (atan (* 6.0d0 (abs (- x (/ y 4.0d0))))))
                                           (* 0.2d0 (atan (* 40.0d0 (abs (- x (/ y 4.0d0))))))
                                           (- (* (/ 23.0d0 20.0d0)
                                                 (+ 1.5d0
                                                    (* (/ 1.0d0 25.0d0)
                                                       (cos
                                                        (* 10.0d0
                                                           (+ y
                                                              (/ x 4.0d0)
                                                              (/ 6.0d0 25.0d0)))))
                                                    (* 0.03d0 Cxy)
                                                    (* 0.3d0
                                                       (atan (* 30.0d0
                                                                (+ y (/ x 4.0d0) (- 0.25d0))))))
                                                 (abs (- x (/ y 4.0d0)))))))))))))
                      (Lxy (let ((sum1 0.0d0))
                             (declare (type double-float sum1))
                             (dotimes (s0 25) ; from 0 to 24
                               (declare (type fixnum s0))
                               (let ((s (1+ s0))) ; from 1 to 25
                                 (declare (type fixnum s))
                                 (incf sum1
                                       (expt
                                        (sin (+ (* (+ 80 (* 30 (sin (* 1.0d0 s s))))
                                                   (atan (/ (- (+ (* 100.0d0 y) (* 25.0d0 x))
                                                               (* 4.0d0 (sin (* 1.0d0 s))))
                                                            (+ (abs (- (* 100.0d0 x)
                                                                       (* 25.0d0 y)
                                                                       (* 3.0d0 (atan (- (* 100.0d0 x)
                                                                                         (* 25.0d0 y))))))
                                                               1.0d0))))
                                                (abs (- (/ x 2.0d0) (/ y 8.0d0)))
                                                (* 4.0d0 (sin (* 5.0d0 s)))))
                                        6))))        
                             sum1))
                      (v 0)
                      (K0xy (let ((sum1 0.0d0))
                              (declare (type double-float sum1))
                              (dotimes (s0 60)
                                (declare (type fixnum s0))
                                (let* ((s (1+ s0))
                                       (exp0
                                         (- (* 25
                                               (- (* (expt
                                                      (sin
                                                       (+ (sin (* 2.0d0 s))
                                                          (* (+ 6 (sin (* 1.0d0 s s)))
                                                             (+ (* (sin (* 7.0d0 s)) (/ x 2.0d0))
                                                                (* (cos (* 7.0d0 s)) (/ (- y 8) 2.0d0))))))
                                                      10)
                                                     (expt
                                                      (sin
                                                       (+ (sin (* 3.0d0 s))
                                                          (* (+ 6 (* 2 (sin (* 1.0d0 s s))))
                                                             (- (* (sin (* 7.0d0 s)) (/ (- y 8) 2.0d0))
                                                                (* (cos (* 7.0d0 s)) (/ x 2.0d0))))))
                                                      10.0d0))
                                                  0.1d0)))))
                                  (declare (type fixnum s)
                                           (type double-float exp0))
                                  (incf sum1
                                        (* (/ 5.0d0 2.0d0)
                                           (+ (/ 2 25.0d0)
                                              (* (/ 3 50.0d0)
                                                 (cos (* s (+ 4.0d0 (* 4.0d0 v))))))
                                           (* (+ (sin (* 5.0d0 s))
                                                 (sin (* 2.0d0 s))
                                                 3.0d0)
                                              (/ 1.0d0 5.0d0))
                                           (exp-exp exp0)))))
                              sum1))
                      (v 1)
                      (K1xy (let ((sum1 0.0d0))
                              (declare (type double-float sum1))
                              (dotimes (s0 60)
                                (declare (type fixnum s0))
                                (let* ((s (1+ s0))
                                       (exp0
                                         (- (* 25
                                               (- (* (expt
                                                      (sin
                                                       (+ (sin (* 2.0d0 s))
                                                          (* (+ 6 (sin (* 1.0d0 s s)))
                                                             (+ (* (sin (* 7.0d0 s)) (/ x 2.0d0))
                                                                (* (cos (* 7.0d0 s)) (/ (- y 8) 2.0d0))))))
                                                      10)
                                                     (expt
                                                      (sin
                                                       (+ (sin (* 3.0d0 s))
                                                          (* (+ 6 (* 2 (sin (* 1.0d0 s s))))
                                                             (- (* (sin (* 7.0d0 s)) (/ (- y 8) 2.0d0))
                                                                (* (cos (* 7.0d0 s)) (/ x 2.0d0))))))
                                                      10.0d0))
                                                  0.1d0)))))
                                  (declare (type fixnum s)
                                           (type double-float exp0))
                                  (incf sum1
                                        (* (/ 5.0d0 2.0d0)
                                           (+ (/ 2 25.0d0)
                                              (* (/ 3 50.0d0)
                                                 (cos (* s (+ 4.0d0 (* 4.0d0 v))))))
                                           (* (+ (sin (* 5.0d0 s))
                                                 (sin (* 2.0d0 s))
                                                 3.0d0)
                                              (/ 1.0d0 5.0d0))
                                           (exp-exp exp0)))))
                              sum1))
                      (v 2)
                      (K2xy (let ((sum1 0.0d0))
                              (declare (type double-float sum1))
                              (dotimes (s0 60)
                                (declare (type fixnum s0))
                                (let* ((s (1+ s0))
                                       (exp0
                                         (- (* 25
                                               (- (* (expt
                                                      (sin
                                                       (+ (sin (* 2.0d0 s))
                                                          (* (+ 6 (sin (* 1.0d0 s s)))
                                                             (+ (* (sin (* 7.0d0 s)) (/ x 2.0d0))
                                                                (* (cos (* 7.0d0 s)) (/ (- y 8) 2.0d0))))))
                                                      10)
                                                     (expt
                                                      (sin
                                                       (+ (sin (* 3.0d0 s))
                                                          (* (+ 6 (* 2 (sin (* 1.0d0 s s))))
                                                             (- (* (sin (* 7.0d0 s)) (/ (- y 8) 2.0d0))
                                                                (* (cos (* 7.0d0 s)) (/ x 2.0d0))))))
                                                      10.0d0))
                                                  0.1d0)))))
                                  (declare (type fixnum s)
                                           (type double-float exp0))
                                  (incf sum1
                                        (* (/ 5.0d0 2.0d0)
                                           (+ (/ 2 25.0d0)
                                              (* (/ 3 50.0d0)
                                                 (cos (* s (+ 4.0d0 (* 4.0d0 v))))))
                                           (* (+ (sin (* 5.0d0 s))
                                                 (sin (* 2.0d0 s))
                                                 3.0d0)
                                              (/ 1.0d0 5.0d0))
                                           (exp-exp exp0)))))
                              sum1))
                      (Wxy (+ (* (- (exp-exp+exp
                                     (+ (- (* 40 Cxy))
                                        (/ 196.0d0 5.0d0)
                                        (* (/ 4.0d0 5.0d0)
                                           (sqrt (the (double-float 0.0d0)
                                                      (+ (* (- x (/ y 4.0d0))
                                                            (- x (/ y 4.0d0)))
                                                         (* (+ y (/ x 4.0d0))
                                                            (+ y (/ x 4.0d0))))))))
                                     (- (* 40
                                           (+ (* 5 (abs
                                                    (+ y
                                                       (/ x 4.0d0)
                                                       (- (/ 3 50.0d0))
                                                       (* (/ 1.0d0 3.0d0)
                                                          (- x (/ y 4.0d0))
                                                          (- x (/ y 4.0d0))))))
                                              (expt (abs (- (* 2 x) (/ y 2.0d0))) 3)
                                              (- (/ 2.0d0 5.0d0)))))))
                                 (- 1 (exp-exp+exp
                                       (+
                                        (- (* 1000
                                              (+ (abs (- x (/ y 4.0d0))))))
                                        100.0d0
                                        (- (* 90.0d0 (atan (+ (* 8 y)
                                                              (* 2 x)
                                                              (/ 8 5.0d0))))))
                                       (* 1000 (+ (abs (- x (/ y 4.0d0)))
                                                  (- (/ 7 50.0d0))
                                                  (* (/ 9.0d0 20.0d0)
                                                     (+ y (/ x 4.0d0) 0.2d0)))))))
                              (- (exp-exp (* 70 (+ (abs
                                                    (+
                                                     (* 5
                                                        (abs
                                                         (+ y
                                                            (/ x 4.0d0)
                                                            (- (/ 3 50.0d0))
                                                            (* (/ 1.0d0 3.0d0)
                                                               (- x (/ y 4.0d0))
                                                               (- x (/ y 4.0d0))))))
                                                     (expt (abs (- (* 2 x) (/ y 2.0d0))) 3)
                                                     (- (/ 2 5.0d0))))
                                                   (- (/ 1.0d0 200.0d0))))))
                              (- (exp-exp (+ (* 700
                                                (abs
                                                 (+
                                                  (abs (- x (/ y 4.0d0)))
                                                  (- 0.1d0)
                                                  (* 0.09d0 (atan (* 8
                                                                     (+ y
                                                                        (/ x 4.0d0)
                                                                        (/ 1 5.0d0))))))))
                                             (- (/ 21 20.0d0)))))
                              1.0d0))
                      (v 0)
                      (H0xy (+
                             ;; first term:
                             (* (* (+ 18 (- (* 9.0d0 v)) (* 1.0d0 v v)) (/ 1.0d0 20.0d0))
                                (- 1.0d0 A0xy)
                                (- 1.0d0 Exy)
                                K0xy) 
                             ;; second term:
                             (* (* (+ 2 (* 3.0d0 v)) (/ 1.0d0 5.0d0))
                                (* A0xy
                                   A1xy
                                   (- 1 Exy)
                                   (* (+ 50.0d0 Lxy) (/ 1.0d0 50.0d0))
                                   (exp-exp+exp (+ (* 2 y)
                                                   (* 0.5d0 x)
                                                   (/ 2.0d0 5.0d0)
                                                   (- (* 2.0d0 (abs (- x (/ y 4.0d0))))))
                                                (+ (* 8 y)
                                                   (* 2 x)
                                                   (/ 2.0d0 5.0d0)
                                                   (- (abs (- (* 8 x) (* 2 y))))))
                                   Wxy))
                             ;; third term:
                             (exp-exp (- (* 50.0d0
                                            (- (* (expt (cos (+ (* 2 y) 
                                                                (* x 0.5d0)
                                                                (/ 7 5.0d0)
                                                                (- (abs (- (* 2 x)
                                                                           (* y 0.5d0))))))
                                                        80)
                                                  (expt (sin (+ (* 20 y)
                                                                (* 5 x)
                                                                (abs (- (* 20 x) (* 5 y)))))
                                                        2))
                                               (expt (+ (* 2.7d0 y)
                                                        (* (* 27 x) (/ 1.0d0 40.0d0))
                                                        (/ 81.0d0 250.0d0)
                                                        )
                                                     10)
                                               (/ 49.0d0 50.0d0)))))
                             ;; fourth term:
                             (* 0.1d0 Exy (* (- v 1.0d0) (- v 1)))))
                      (v 1)
                      (H1xy (+
                             ;; first term:
                             (* (* (+ 18 (- (* 9.0d0 v)) (* 1.0d0 v v)) (/ 1.0d0 20.0d0))
                                (- 1.0d0 A0xy)
                                (- 1.0d0 Exy)
                                K1xy) 
                             ;; second term:
                             (* (* (+ 2 (* 3.0d0 v)) (/ 1.0d0 5.0d0))
                                (* A0xy
                                   A1xy
                                   (- 1 Exy)
                                   (* (+ 50.0d0 Lxy) (/ 1.0d0 50.0d0))
                                   (exp-exp+exp (+ (* 2 y)
                                                   (* 0.5d0 x)
                                                   (/ 2.0d0 5.0d0)
                                                   (- (* 2.0d0 (abs (- x (/ y 4.0d0))))))
                                                (+ (* 8 y)
                                                   (* 2 x)
                                                   (/ 2.0d0 5.0d0)
                                                   (- (abs (- (* 8 x) (* 2 y))))))
                                   Wxy))
                             ;; third term:
                             (exp-exp (- (* 50.0d0
                                            (- (* (expt (cos (+ (* 2 y) 
                                                                (* x 0.5d0)
                                                                (/ 7 5.0d0)
                                                                (- (abs (- (* 2 x)
                                                                           (* y 0.5d0))))))
                                                        80)
                                                  (expt (sin (+ (* 20 y)
                                                                (* 5 x)
                                                                (abs (- (* 20 x) (* 5 y)))))
                                                        2))
                                               (expt (+ (* 2.7d0 y)
                                                        (* (* 27 x) (/ 1.0d0 40.0d0))
                                                        (/ 81.0d0 250.0d0)
                                                        )
                                                     10)
                                               (/ 49.0d0 50.0d0)))))
                             ;; fourth term:
                             (* 0.1d0 Exy (* (- v 1.0d0) (- v 1)))))

                      (v 2)
                      (H2xy (+
                             ;; first term:
                             (* (* (+ 18 (- (* 9.0d0 v)) (* 1.0d0 v v)) (/ 1.0d0 20.0d0))
                                (- 1.0d0 A0xy)
                                (- 1.0d0 Exy)
                                K2xy) 
                             ;; second term:
                             (* (* (+ 2 (* 3.0d0 v)) (/ 1.0d0 5.0d0))
                                (* A0xy
                                   A1xy
                                   (- 1 Exy)
                                   (* (+ 50.0d0 Lxy) (/ 1.0d0 50.0d0))
                                   (exp-exp+exp (+ (* 2 y)
                                                   (* 0.5d0 x)
                                                   (/ 2.0d0 5.0d0)
                                                   (- (* 2.0d0 (abs (- x (/ y 4.0d0))))))
                                                (+ (* 8 y)
                                                   (* 2 x)
                                                   (/ 2.0d0 5.0d0)
                                                   (- (abs (- (* 8 x) (* 2 y))))))
                                   Wxy))
                             ;; third term:
                             (exp-exp (- (* 50.0d0
                                            (- (* (expt (cos (+ (* 2 y) 
                                                                (* x 0.5d0)
                                                                (/ 7 5.0d0)
                                                                (- (abs (- (* 2 x)
                                                                           (* y 0.5d0))))))
                                                        80)
                                                  (expt (sin (+ (* 20 y)
                                                                (* 5 x)
                                                                (abs (- (* 20 x) (* 5 y)))))
                                                        2))
                                               (expt (+ (* 2.7d0 y)
                                                        (* (* 27 x) (/ 1.0d0 40.0d0))
                                                        (/ 81.0d0 250.0d0)
                                                        )
                                                     10)
                                               (/ 49.0d0 50.0d0)))))
                             ;; fourth term:
                             (* 0.1d0 Exy (* (- v 1.0d0) (- v 1)))))
                      (r (floor
                          (* 255.0
                             (exp-exp (- (* 1000.0 H0xy)))
                             (expt (abs H0xy) (exp-exp (* 1000.0 (- H0xy 1.0)))))))
                      (g (floor
                          (* 255.0
                             (exp-exp (- (* 1000.0 H1xy)))
                             (expt (abs H1xy) (exp-exp (* 1000.0 (- H1xy 1.0)))))))
                      (b (floor
                          (* 255.0
                             (exp-exp (- (* 1000.0 H2xy)))
                             (expt (abs H2xy) (exp-exp (* 1000.0 (- H2xy 1.0)))))))) ; end of let bindings
                 (declare (type double-float x y Cxy Exy Lxy Wxy K0xy K1xy K2xy A0xy A1xy H0xy H1xy H2xy)
                          (type fixnum m v r g b))

                 (setf (aref C-array (- n 1) (- m 1)) Cxy)
                 (setf (aref E-array (- n 1) (- m 1)) Exy)
                 (setf (aref L-array (- n 1) (- m 1)) Lxy)
                 (setf (aref W-array (- n 1) (- m 1)) Wxy)
                 (setf (aref A0-array (- n 1) (- m 1)) A0xy)
                 (setf (aref A1-array (- n 1) (- m 1)) A1xy)
                 (setf (aref K0-array (- n 1) (- m 1)) K0xy)
                 (setf (aref K1-array (- n 1) (- m 1)) K1xy)
                 (setf (aref K2-array (- n 1) (- m 1)) K2xy)
                 (setf (aref H0-array (- n 1) (- m 1)) H0xy)
                 (setf (aref H1-array (- n 1) (- m 1)) H1xy)
                 (setf (aref H2-array (- n 1) (- m 1)) H2xy)
                 (setf (aref r-array (- n 1) (- m 1)) r)
                 (setf (aref g-array (- n 1) (- m 1)) g)
                 (setf (aref b-array (- n 1) (- m 1)) b)))))))                                        
     (loop for i from 0 below nb-chunks collect i)) 

    (locally
        (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
      (setq duration (/ (- (get-internal-real-time) real-base) internal-time-units-per-second 1.0)))
    
    (format t "~%3) Export pic...~%")
    (draw-pic-from-rgb-arrays +height+ +width+ r-array g-array b-array "pics/butterfly2.png")

    (format t "~%4) Export heatmaps...~%")
    (format t "C") 
    (draw-heatmap-from-values +height+ +width+ C-array "pics/C-heatmap.png")
    (format t ", E") 
    (draw-heatmap-from-values +height+ +width+ E-array "pics/E-heatmap.png")
    (format t ", L") 
    (draw-heatmap-from-values +height+ +width+ L-array "pics/L-heatmap.png")
    (format t ", W") 
    (draw-heatmap-from-values +height+ +width+ W-array "pics/W-heatmap.png")
    (format t ", A0") 
    (draw-heatmap-from-values +height+ +width+ A0-array "pics/A0-heatmap.png")
    (format t ", A1") 
    (draw-heatmap-from-values +height+ +width+ A1-array "pics/A1-heatmap.png")
    (format t ", K0") 
    (draw-heatmap-from-values +height+ +width+ K0-array "pics/K0-heatmap.png")
    (format t ", K1") 
    (draw-heatmap-from-values +height+ +width+ K1-array "pics/K1-heatmap.png")
    (format t ", K2") 
    (draw-heatmap-from-values +height+ +width+ K2-array "pics/K2-heatmap.png")
    (format t ", H0") 
    (draw-heatmap-from-values +height+ +width+ H0-array "pics/H0-heatmap.png")
    (format t ", H1") 
    (draw-heatmap-from-values +height+ +width+ H1-array "pics/H1-heatmap.png")
    (format t ", H2") 
    (draw-heatmap-from-values +height+ +width+ H2-array "pics/H2-heatmap.png")
    (format t "~%~%")
    
    (format t "Done in ~f seconds.~%" duration)
    duration))

(defun main ()
  (format t "~%(1) GENERATE BUTTERFLY~%")
  (format t "----------------------~%")
  (run1)
  (format t "~%(2) GENERATE HEATMAPS~%")
  (format t "---------------------~%~%")
  (run2))

;;; end
