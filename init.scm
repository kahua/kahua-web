;; kahua-web initialization
;;
;;  Copyright (c) 2004 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2004 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: init.scm,v 1.3 2007/04/18 09:25:19 bizenn Exp $

(use kahua)
(use file.util)
(use gauche.parseopt)
(use gauche.termios)

(define (main args)
  (let-args (cdr args)
      ((conf-file "c=s")
       (site-dir "S=s")
       (dbpath "D=s"))
    (kahua-common-init site-dir conf-file)
    (let1 dbname (kahua-dbpath (or dbpath (kahua-default-database-name)))
      (with-db (db dbname)
        (unless (find-kahua-instance <kahua-user> "admin")
          (let* ((pass (get-password "Type initial admin password: "))
                 (u (kahua-add-user "admin" pass)))
            (slot-set! u 'role-alist '(admin))))
        ))
    0))

(define (get-password prompt)
  (let* ((port (current-input-port))
         (attr (sys-tcgetattr port))
         (lflag (slot-ref attr 'lflag)))
    ;; Show prompt
    (display prompt)
    (flush)
    ;; Turn off echo during reading.
    (dynamic-wind
        (lambda ()
          (slot-set! attr 'lflag (logand lflag (lognot (logior ECHO ECHOE ECHOK ECHONL))))
          (sys-tcsetattr port TCSAFLUSH attr))
        (lambda ()
          (read-line port))
        (lambda ()
          (slot-set! attr 'lflag lflag)
          (sys-tcsetattr port TCSANOW attr)
          (display "\n")))))
