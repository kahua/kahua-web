;; kahua-web user management
;;
;;  Copyright (c) 2004 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2004 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: lsuser.scm,v 1.1 2004/01/25 14:51:32 shiro Exp $

(use kahua)
(use file.util)
(use gauche.collection)
(use gauche.parseopt)

(define (main args)
  (let-args (cdr args) ((conf-file "c=s"))
    (kahua-init conf-file)
    (let ((dbname (build-path (ref (kahua-config) 'working-directory)
                              "kahua-web")))
      (with-db (db dbname)
        (format #t "~10a ~a\n" "username" "roles")
        (for-each (lambda (u)
                    (format #t "~10a ~s\n"
                            (ref u 'login-name)
                            (ref u 'role-alist)))
                  (make-kahua-collection <kahua-user>))))
    0))
