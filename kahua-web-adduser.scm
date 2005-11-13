;; kahua-web user management
;;
;;  Copyright (c) 2004 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2004 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: kahua-web-adduser.scm,v 1.1 2005/11/13 08:48:11 nobsun Exp $

(use kahua)
(use file.util)
(use gauche.parseopt)

(define (main args)
  (let-args (cdr args) ((conf-file "c=s") . args)
    (unless (= (length args) 2)
      (print "Usage: gosh kahua-web-adduser.scm [-c conf-file] username password")
      (exit 70))
    (kahua-init conf-file)
    (let ((dbname (build-path (ref (kahua-config) 'working-directory)
                              "kahua-web"))
          (user (car args))
          (pass (cadr args)))
      (with-db (db dbname)
        (let1 u (or (kahua-add-user user pass)
                    (begin
                      (print #`"User ,user is already registered.")
                      (exit 70)))
          ;; for now, all users are given developer priviledge.
          (set! (ref u 'role-alist) '(developer))))
      )
    0))
