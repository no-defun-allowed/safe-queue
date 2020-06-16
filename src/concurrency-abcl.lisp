(in-package #:safe-queue)

(deftype mailbox ()
  '(satisfies mailbox-p))

(defun make-mailbox (&key name initial-contents)
  (let ((mailbox
          (java:jnew "java.util.concurrent.LinkedBlockingQueue")))
    (map 'nil
         (lambda (item)
           (mailbox-send-message item mailbox))
         initial-contents)
    mailbox))

(defvar *put* (java:jmethod "java.util.concurrent.LinkedBlockingQueue" "put" "java.lang.Object"))
(defun mailbox-send-message (mailbox message)
  (java:jcall *put* mailbox message))

(defvar *milliseconds* #"TimeUnit.MILLISECONDS")
(defvar *take* (java:jmethod "java.util.concurrent.LinkedBlockingQueue" "take"))
(defvar *poll* (java:jmethod "java.util.concurrent.LinkedBlockingQueue" "poll"
                             "long" "java.util.concurrent.TimeUnit"))
(defun mailbox-receive-message (mailbox &key timeout)
  (if (null timeout)
      (values (java:jcall *take* mailbox) t)
      (let ((value (java:jcall-raw *poll* mailbox
                                   (floor timeout 0.001)
                                   *milliseconds*)))
        (if (and (java:java-object-p value)
                 (java:jnull-ref-p value))
            (values nil nil)
            (values value t)))))
