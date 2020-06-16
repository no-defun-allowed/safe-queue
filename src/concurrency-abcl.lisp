(in-package #:safe-queue)
(declaim (optimize (speed 3)))

;;; Stuff to make Java FFI go fast.
(defmacro method (class name &rest arguments)
  `(load-time-value (java:jmethod ,class ,name ,@arguments)))
(defmacro call (class name subject &rest arguments)
  `(java:jcall (method ,class ,name ,@(mapcar #'second arguments))
               ,subject ,@(mapcar #'first arguments)))
(defmacro call-raw (class name subject &rest arguments)
  `(java:jcall-raw (method ,class ,name ,@(mapcar #'second arguments))
                   ,subject ,@(mapcar #'first arguments)))

;;; Queues

;;; Mailboxes

(deftype mailbox ()
  '(satisfies mailbox-p))

(defun make-mailbox (&key name initial-contents)
  "Returns a new MAILBOX with messages in INITIAL-CONTENTS enqueued."
  (declare (ignore name))
  (let ((mailbox
          (java:jnew "java.util.concurrent.LinkedBlockingQueue")))
    (map 'nil (lambda (item)
                (mailbox-send-message item mailbox))
         initial-contents)
    mailbox))

(defun mailbox-p (mailbox)
  (java:jinstance-of-p mailbox "java.util.concurrent.LinkedBlockingQueue"))

;;; This is perfectly not-O(n); the count is stored in the queue.
(defun mailbox-empty-p (mailbox)
  "Returns true if MAILBOX is currently empty, NIL otherwise."
  (zerop (mailbox-count mailbox)))

(defun mailbox-send-message (mailbox message)
  (call "java.util.concurrent.LinkedBlockingQueue" "put"
        mailbox (message "java.lang.Object")))

(defun mailbox-receive-message (mailbox &key timeout)
  "Removes the oldest message from MAILBOX and returns it as the
primary value. If MAILBOX is empty waits until a message arrives."
  (if (null timeout)
      (values (call "java.util.concurrent.LinkedBlockingQueue" "take"
                    mailbox)
              t)
      (let ((value (call-raw "java.util.concurrent.LinkedBlockingQueue" "poll"
                             mailbox
                             ((floor timeout 0.001) "long")
                             ((load-time-value #"TimeUnit.MILLISECONDS")
                              "java.util.concurrent.TimeUnit"))))
        (if (and (java:java-object-p value)
                 (java:jnull-ref-p value))
            (values nil nil)
            (values value t)))))

(defun mailbox-receive-message-no-hang (mailbox)
  "The non-blocking variant of RECEIVE-MESSAGE. Returns two values,
the message removed from MAILBOX, and a flag specifying whether a
message could be received."
  (let ((value (call-raw "java.util.concurrent.LinkedBlockingQueue" "poll"
                         mailbox)))
    (if (and (java:java-object-p value)
             (java:jnull-ref-p value))
        (values nil nil)
        (values value t))))

(defun mailbox-count (mailbox)
  "Returns the number of messages currently in the MAILBOX."
  (call "java.util.concurrent.LinkedBlockingQueue" "size" mailbox))

(defun mailbox-list-messages (mailbox)
  "Returns a fresh list containing all the messages in the
mailbox. Does not remove messages from the mailbox."
  (java:jobject-lisp-value
   (call-raw "java.util.concurrent.LinkedBlockingQueue" "toArray" mailbox)))

(defun mailbox-receive-pending-messages (mailbox &optional n)
  "Removes and returns all (or at most N) currently pending messages
from MAILBOX, or returns NIL if no messages are pending.

Note: Concurrent threads may be snarfing messages during the run of
this function, so even though X,Y appear right next to each other in
the result, does not necessarily mean that Y was the message sent
right after X."
  (loop with msg = nil
        with found = nil
        for i from 0
        while (or (null n) (< i n))
        do (setf (values msg found) (mailbox-receive-message-no-hang mailbox))
        while found
        collect msg))
