(in-package #:os-scheduler)

(declaim (optimize safety)) ; necesary to check the type of the slot "state"

(deftype process-state () '(member :running :ready :done :waiting))

(defclass process ()
  ((pid :initarg :pid
        :initform nil
        :accessor :pid)
   (state :initarg :state
          :type process-state
          :initform :ready
          :accessor :state)
   (run-time :initarg :run-time
             :initform nil
             :accessor :run-time)
   (arrival-time :initarg :arrival-time
                 :initform 0
                 :accessor :arrival-time)))

(defun make-process (&key pid (state :ready) run-time (arrival-time 0))
  (make-instance 'process :pid pid :state state :run-time run-time :arrival-time arrival-time))
