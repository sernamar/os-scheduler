(in-package #:os-scheduler)

(declaim (optimize safety)) ; necesary to check the type of the slot "state"

;;; ------------- ;;;
;;; Process class ;;;
;;; ------------- ;;;

(defvar *pid* 0)
(defconstant +max-run-time+ 10)

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
                 :accessor :arrival-time)
   (start-time :initarg :start-time
                    :initform nil
                    :accessor :start-time)
   (completion-time :initarg :completion-time
                    :initform nil
                    :accessor :completion-time)))

(defun make-process (&key pid (state :ready) run-time (arrival-time 0) completion-time)
  (make-instance 'process :pid pid
                          :state state
                          :run-time run-time
                          :arrival-time arrival-time
                          :completion-time completion-time))

(defmethod process-turnaround-time (process)
  "Compute the turnaround time of a process, which is a scheduling metric defined as the time at which the process completes minus the time at which the process arrived in the system."
  (let ((completion-time (:completion-time process))
        (arrival-time (:arrival-time process)))
    (when completion-time
      (- completion-time arrival-time))))

;;; ------------------- ;;;
;;; Auxiliary functions ;;;
;;; ------------------- ;;;

(defun create-workload (&key number-of-processes run-time (arrival-time 0))
  (loop :repeat number-of-processes
        :do (incf *pid*)
        :collect (make-process :pid *pid* :run-time run-time :arrival-time arrival-time)))

(defun print-workload (workload)
  (dolist (process workload)
    (format t "pid: ~d, state: ~a, run-time: ~d, arrival time: ~d~%"
            (:pid process) (:state process) (:run-time process) (:arrival-time process))))

(defun print-process-run-time (process)
  (loop :for i :from (:start-time process) :below (+ (:start-time process) (:run-time process))
        :do (format t "Process ~d: ~d~%" (:pid process) (1+ i))))

(defun turnaround-time (workload)
  (/ (reduce #'+ (mapcar #'process-turnaround-time workload))
     (length workload)))

;;; ------------------- ;;;
;;; Scheduling policies ;;;
;;; ------------------- ;;;

;; Initial workload assumptions (chapter 7, page 1, "Operating Systems: Three Easy Pieces")
(defparameter *run-time* 3)
(defparameter *arrival-time* 0)

;; "First In, First Out" policy
(defun fifo (workload)
  (let ((time 0))
    (dolist (process workload)
      (setf (:start-time process) time)
      (setf (:state process) :running)
      (print-process-run-time process)
      (setf (:completion-time process) (+ (:start-time process) (:run-time process)))
      (setf (:state process) :done)
      (setf time (:completion-time process)))))

;;; ---------- ;;;
;;; Simulation ;;;
;;; ---------- ;;;

(defun simulate (&key number-of-processes (run-time *run-time*) (arrival-time *arrival-time*))
  (let ((workload (create-workload :number-of-processes number-of-processes
                                   :run-time run-time
                                   :arrival-time arrival-time)))
    (print-workload workload)
    (fifo workload)))

(simulate :number-of-processes 3)
