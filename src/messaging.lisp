(defpackage #:qah-utils/messaging
  (:use :cl)
  (:export :message
           :message-from-bytes
           :message-from-file
           :message-from-stream
           :make-message-from-bytes
           :make-message-from-string
           :make-message-from-lisp
           :channel
           :channel-in
           :channel-out
           :stream-channel
           :make-stream-channel
           :binary-channel
           :make-binary-channel
           :lisp-channel
           :make-lisp-channel
           :get-bytes
           :send
           :recv
           :actor
           :make-actor
           :actor-name
           :actor-channel
           :actor-log
           :actor-stop
           :actor-start))

(in-package #:qah-utils/messaging)

;;
;; Message
;;
(defclass message () ()
  (:documentation "The base class for all messages"))

(defgeneric get-bytes (message)
  (:documentation "Returns the MESSAGE as a STREAM of bytes"))

(defclass message-from-bytes (message)
  ((bytes :initarg :bytes
          :type unsigned-byte
          :initform (error "Please supply a byte vector")))
  (:documentation "A message based on a byte vector"))

(defmethod get-bytes ((message message-from-bytes))
  (flex:make-in-memory-input-stream
   (slot-value message 'bytes)))

(defun make-message-from-bytes (bytes)
  (make-instance 'message-from-bytes :bytes bytes))

(defun make-message-from-string (string)
  (make-instance 'message-from-bytes
                 :bytes (flex:string-to-octets string)))

(defun make-message-from-lisp (lisp)
  (make-message-from-string
   (format nil "~a" lisp)))

(defclass message-from-file (message)
  ((file :initarg :file
         :initform (error "Please supply a path to a file")))
  (:documentation "A message based on a file"))

(defmethod get-bytes ((message message-from-file))
  (open (slot-value message 'file)
        :element-type 'unsigned-byte))

(defclass message-from-stream (message)
  ((stream :initarg :stream
           :initform (error "Please supply a STREAM")))
  (:documentation "A message based on a stream"))

;;
;; Channel
;;
(defclass channel () ())

(defgeneric send (channel message))

(defgeneric recv (channel))

(defclass stream-channel (channel)
  ((in  :initarg :in
        :initform (error "Input stream must be supplied")
        :reader  channel-in)
   (out :initarg :out
        :initform (error "Output stream must be supplied")
        :reader  channel-out)))

(defun make-stream-channel (in out)
  (make-instance 'stream-channel :in in :out out))

(defclass binary-channel (stream-channel)
  ((receive-buffer-size :initarg :receive-buffer-size
                        :initform 1024)))

(defun make-binary-channel (in out)
  (make-instance 'binary-channel :in in :out out))

(defmethod send ((channel binary-channel) (message message))
  (uiop:copy-stream-to-stream (get-bytes message)
                              (slot-value channel 'out)
                              :element-type '(unsigned-byte 8)
                              :linewise nil
                              :buffer-size (expt 2 20)))

(defmethod recv ((channel binary-channel))
  (let ((receive-buffer (make-array (slot-value channel 'receive-buffer-size))))
    (do
     ((len (handler-case (read-sequence receive-buffer
                                             (channel-in channel))
                  (end-of-file () (sleep 1) 0))))
     ((> len 0) (vec:slice receive-buffer 0 len)))))

(defclass lisp-channel (binary-channel) ())

(defun make-lisp-channel (in out)
  (make-instance 'lisp-channel :in in :out out))

(defmethod send ((channel lisp-channel) data)
  (let ((message (make-message-from-lisp data)))
    (call-next-method channel message)))

(defmethod recv ((channel lisp-channel))
  (do ((data (handler-case (read (channel-in channel))
                         (end-of-file () (sleep 0.1) nil))))
      (data data)))

;;
;; Actors
;;
(defclass actor ()
  ((name          :initarg :name
                  :initform (error "Please supply a NAME for the actor")
                  :reader  actor-name)
   (channel       :initarg :channel
                  :initform (error "Please supply a CHANNEL")
                  :reader  actor-channel)
   (worker        :initarg :worker
                  :initform (error "Please supply a worker function"))
   (worker-thread :initform nil)
   (log-stream    :initarg :log-stream
                  :initform nil)))

(defun make-actor (name channel worker &optional log-stream)
  (make-instance 'actor
                 :name name
                 :channel channel
                 :worker worker
                 :log-stream log-stream))

(defmethod display ((actor actor))
  (slot-value actor 'name))

(defmethod actor-log ((actor actor) (msg string))
  (with-slots (log-stream channel name) actor
    (when log-stream
      (format (if (eq t log-stream)
                  (slot-value channel 'out)
                  log-stream)
              "~a: ~a~%"
              name msg))))

(defmethod actor-stop ((actor actor))
  (with-slots (worker-thread) actor
    (if worker-thread
        (progn
          (if (bt:thread-alive-p worker-thread)
              (progn
                (actor-log actor "Stopping")
                (bt:destroy-thread worker-thread))
              (actor-log actor "stopping zombie"))
          (setf (slot-value actor 'worker-thread) nil))
        (actor-log actor "Already stopped"))
    :stopped))

(defmethod actor-start ((actor actor))
  (with-slots (worker-thread worker name) actor
    (when worker-thread
      (actor-stop actor))
    (actor-log actor "Starting" )
    (setf (slot-value actor 'worker-thread)
          (bt:make-thread
           (lambda ()
             (funcall worker actor))
           :name (format nil "Worker for ~a" name)))
    :started))

(defun test-actor ()
  (let ((input (make-string-input-stream "hello hello"))
        (output *standard-output*))
    (make-actor
     "Test Actor"
     (make-lisp-channel input output)
     #'(lambda (ac)
         (loop
           for got = (recv (actor-channel ac))
           do (send (actor-channel ac)
                    (make-message-from-lisp got)))))))
