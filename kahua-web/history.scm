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
                  pagename new old
                  :message logmsg
                  :remote-user user)))
    (if (history-file)
        (call-with-output-file (history-file)
          (lambda (p) (display content p) (flush p))
          :if-exists :append)
      (error "please set history-file"))))

