;; kahua-web initialization
;;
;;  Copyright (c) 2004 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2004 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: init.scm,v 1.1 2004/01/25 14:40:47 shiro Exp $

(use kahua)
(use file.util)
(use gauche.parseopt)

(define (main args)
  (let-args (cdr args) ((conf-file "c=s"))
    (kahua-init conf-file)
    (let ((dbname (build-path (ref (kahua-config) 'working-directory)
                              "kahua-web")))
      (with-db (db dbname)
        (unless (find-kahua-instance <kahua-user> "admin")
          (let* ((pass (get-pass))
                 (u (kahua-add-user "admin" pass)))
            (slot-set! u 'role-alist '(admin))))
        ))
    0))

(define (get-pass)
  (display "Type initial admin password (it is echoed!): ")
  (flush)
  (read-line))




