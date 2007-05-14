;; -*- mode: scheme; coding: utf-8 -*-
;; history plugin.

(use gauche.parameter)
(use wiliki.log)

(define-plugin "history"
  (version "0.1")
  (export write-log history-file)
  (depend #f))

(define history-file (make-parameter #f))

(define (write-log name old new logmsg user)
  (let ((content (wiliki-log-create
                  name new old
                  :message logmsg
                  :remote-user user)))
    (or (and-let* ((h (force (history-file))))
	  (call-with-output-file h (lambda (p) (display content p) (flush p))
				 :if-exists :append))
	(error "please set history-file"))))

