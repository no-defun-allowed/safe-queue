(in-package #:safe-queue)

(declaim (optimize (speed 3)))
(deftype mailbox ()
  '(satisfies mailbox-p))

(defmacro method (class name &rest arguments)
  `(load-time-value (java:jmethod ,class ,name ,@arguments)))

(defun mailbox-p (mailbox)
  (java:jinstance-of-p mailbox "java.util.concurrent.BlockingQueue"))

(defun mailbox-send-message (mailbox message)
  (java:jcall (method "java.util.concurrent.LinkedBlockingQueue" "put" "java.lang.Object")
              mailbox message))

(defun mailbox-receive-message (mailbox &key timeout)
  (if (null timeout)
      (values (java:jcall (method "java.util.concurrent.LinkedBlockingQueue" "take")
                          mailbox)
              t)
      (let ((value (java:jcall-raw (method "java.util.concurrent.LinkedBlockingQueue" "poll"
                                           "long" "java.util.concurrent.TimeUnit")
                                   mailbox
                                   (floor timeout 0.001)
                                   (load-time-value #"TimeUnit.MILLISECONDS"))))
        (if (and (java:java-object-p value)
                 (java:jnull-ref-p value))
            (values nil nil)
            (values value t)))))

(defun make-mailbox (&key name initial-contents)
  (declare (ignore name))
  (let ((mailbox
          (java:jnew "java.util.concurrent.LinkedBlockingQueue")))
    (map 'nil
         (lambda (item)
           (mailbox-send-message item mailbox))
         initial-contents)
    mailbox))
