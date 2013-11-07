;;;; Copyright (C) 2013, Christopher Mark Gore,
;;;; Soli Deo Gloria,
;;;; All rights reserved.
;;;;
;;;; 2317 South River Road, Saint Charles, Missouri 63303 USA.
;;;; Web: http://cgore.com
;;;; Email: cgore@cgore.com
;;;;
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions are met:
;;;;
;;;;     * Redistributions of source code must retain the above copyright
;;;;       notice, this list of conditions and the following disclaimer.
;;;;
;;;;     * Redistributions in binary form must reproduce the above copyright
;;;;       notice, this list of conditions and the following disclaimer in the
;;;;       documentation and/or other materials provided with the distribution.
;;;;
;;;;     * Neither the name of Christopher Mark Gore nor the names of other
;;;;       contributors may be used to endorse or promote products derived from
;;;;       this software without specific prior written permission.
;;;;
;;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;;; POSSIBILITY OF SUCH DAMAGE.


(require :asdf)
(require :sigma)
(require :cffi)
(load "~/quicklisp/setup.lisp")
(require :cl-opengl)
(require :cl-glu)
(require :cl-glut)

(defclass movement-window (glut:window)
  ((red :accessor red :initform 1)
   (green :accessor green :initform 1)
   (blue :accessor blue :initform 1)
   (eye-x :accessor eye-x :initform 0)
   (eye-y :accessor eye-y :initform 0)
   (eye-z :accessor eye-z :initform 5))
  (:default-initargs :width 700 :height 700 :title "movement.lisp"
                     :mode '(:single :rgb)))

(defmethod glut:display-window :before ((w movement-window))
  (gl:clear-color 0 0 0 0)
  (gl:shade-model :flat))

(defmethod glut:display ((w movement-window))
  (gl:clear :color-buffer)
  (gl:color (red w) (green w) (blue w))
  (gl:load-identity) ; clear the matrix
  ;; viewing transformation
  (glu:look-at (eye-x w) (eye-y w) (eye-z w)
               0 0 0  ; look pos
               0 1 0) ; up vector
  ;; modeling transformation
  (gl:scale 1 1 1)
  (glut:wire-cube 1)
  (gl:flush))

(defmethod glut:reshape ((w movement-window) width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:frustum -1 1 -1 1 1.5 20)
  (gl:matrix-mode :modelview))


(defmethod glut:keyboard ((w movement-window) key x y)
  (declare (ignore x y))
  (flet ((update (slot n)
           (setf (slot-value w slot) n)
           (glut:post-redisplay))
         (update+ (slot n)
           (incf (slot-value w slot) n)
           (glut:post-redisplay)))
    (case key
      (#\r (update 'red 1))
      (#\R (update 'red 0))
      (#\g (update 'green 1))
      (#\G (update 'green 0))
      (#\b (update 'blue 1))
      (#\B (update 'blue 0))
      (#\w (update+ 'eye-y 1))
      (#\s (update+ 'eye-y -1))
      (#\a (update+ 'eye-x -1))
      (#\d (update+ 'eye-x 1))
      (#\q (update+ 'eye-z -1))
      (#\e (update+ 'eye-z 1))
      (#\Esc (glut:destroy-current-window)))))

(defun movement ()
  (glut:display-window (make-instance 'movement-window)))
