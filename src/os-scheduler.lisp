(in-package #:os-scheduler)

(declaim (optimize safety)) ; necesary to check the type of the slot "state"

;;; ------------- ;;;
;;; Process class ;;;
;;; ------------- ;;;

(defvar *pid* 0)

(deftype process-state () '(member :running :ready :done :waiting))

(defclass process ()
  ((pid :initarg :pid
        :accessor :pid
        :documentation "Process identifier.")
   (state :initarg :state
          :type process-state
          :initform :ready
          :accessor :state
          :documentation "Process state (:running, :ready, :done, or :waiting).")
   (arrival-time :initarg :arrival-time
                 :accessor :arrival-time
                 :documentation "The time that the process arrives to the system.")
   (start-time :initarg :start-time
               :accessor :start-time
               :documentation "The time that the system starts executing the process.")
   (run-time :initarg :run-time
             :accessor :run-time
             :documentation "The time (duration) that the process should be run by the system to be considered completed.")
   (completion-time :initarg :completion-time
                    :accessor :completion-time
                    :documentation "The time when the process was completed.")))

(defun make-process (&key pid (state :ready) run-time (arrival-time 0) completion-time)
  (make-instance 'process
                 :pid pid
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
  "Create a number of processes (the workload) with the same arrival-time values, but different run-time values (especified in a list)."
  (when (and (listp run-time) (= number-of-processes (length run-time)))
    (loop :for process-run-time :in run-time
       :do (incf *pid*)
       :collect (make-process :pid *pid* :run-time process-run-time :arrival-time arrival-time))))

(defun create-workload-with-same-run-time (&key number-of-processes run-time (arrival-time 0))
  "Create a number of processes (the workload) with the same arrival-time values and the same run-time values."
  (create-workload :number-of-processes number-of-processes
                   :run-time (make-list number-of-processes :initial-element run-time)
                   :arrival-time arrival-time))

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

;; FIFO same run-time (10) example
(defun simulate-fifo-same-run-time ()
  (let ((workload (create-workload-with-same-run-time :number-of-processes 3
                                                      :run-time 10
                                                      :arrival-time 0)))
    (print-workload workload)
    (fifo workload)
    (format t "Turnaround time: ~d~%" (turnaround-time workload))))

(simulate-fifo-same-run-time)

;; FIFO different run-time (100, 10, 10) example
(defun simulate-fifo-differet-run-time ()
  (let ((workload (create-workload :number-of-processes 3
                                   :run-time '(100 10 10)
                                   :arrival-time 0)))
    (print-workload workload)
    (fifo workload)
    (format t "Turnaround time: ~d~%" (turnaround-time workload))))

(simulate-fifo-differet-run-time)
