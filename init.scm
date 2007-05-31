;; kahua-web initialization
;;
;;  Copyright (c) 2004-2007 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2004-2007 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: init.scm,v 1.5 2007/05/31 08:10:35 bizenn Exp $

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
    (set! (port-buffering (current-output-port)) :none)
    (let1 dbname (kahua-dbpath (or dbpath (kahua-default-database-name)))
      (with-db (db dbname)
        (unless (find-kahua-instance <kahua-user> "admin")
          (let1 pass (get-password)
	    (kahua-add-user "admin" pass :role-alist '(admin))
	    (display "\ndone.\n"))))))
  0)

(define (abort)
  (display "\naborted.\n")
  (exit 70))

(define (with-noecho port proc)
  (let* ((attr (sys-tcgetattr port))
         (lflag (slot-ref attr 'lflag)))
    (dynamic-wind
        (lambda ()
          (slot-set! attr 'lflag (logand lflag (lognot (logior ECHO ECHOE ECHOK ECHONL))))
          (sys-tcsetattr port TCSAFLUSH attr))
	(cut proc port)
        (lambda ()
          (slot-set! attr 'lflag lflag)
          (sys-tcsetattr port TCSANOW attr)))))

(define (get-password)
  (define (prompt&read-line port prompt)
    (format #t "~a: " prompt)
    (read-line port))
  (with-noecho (current-input-port)
    (lambda (port)
      (let loop ()
	(let1 pw (prompt&read-line port "Type \"admin\" initial password")
	  (unless (string? pw) (abort))
	  (let1 pw2 (prompt&read-line port "\nType \"admin\" initial password (again)")
	    (unless (string? pw2) (abort))
	    (cond ((equal? pw pw2) pw)
		  (else
		   (display "\nDon't match, try again\n")
		   (loop)))))))))
