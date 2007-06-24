;; -*- mode: scheme; coding: utf-8 -*-
;;
;;  Copyright (c) 2005-2007 Kahua Project, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: rss-reader.scm,v 1.11 2007/06/24 03:26:20 bizenn Exp $

(use srfi-11)
(use rfc.uri)
(use rfc.http)
(use sxml.ssax)
(use sxml.sxpath)
(use util.match)
(use util.list)
(use gauche.charconv)
(use gauche.version)
(use kahua.util)

(define-plugin "rss-reader"
  (version "0.1")
  (export rss->sxml)
  (depend #f))

;; Because http-get/http-post of Gauche 0.8.10 or prior cannot handle :sink and :flusher
;; arguments, so we patch to original rfc.http module.
(define-macro (define-transitional-patch)
  (if (version<=? (gauche-version) "0.8.10")
      '(with-module rfc.http
	 ;; This definition is taken from Gauche's lib/rfc/http.scm 1.12.
	 (define (request-response request server request-uri options)
	   (define (%send-request request server host request-uri has-content? options)
	     (with-server
	      server
	      (lambda (in out)
		(send-request out request host request-uri options)
		(receive (code headers) (receive-header in)
		  (values code
			  headers
			  (and has-content?
			       (let-keywords options
				   ((sink    (open-output-string))
				    (flusher (lambda (sink _) (get-output-string sink)))
				    . #f)
				 (receive-body in headers sink flusher))))))))

	   (let-keywords options
	       ((host    (server->host server))
		(no-redirect #f)
		. restopts)
	     (let1 has-content? (not (eq? request 'HEAD))
	       (if no-redirect
		   (%send-request request server host request-uri has-content? restopts)
		   (let loop ((history (list (values-ref (canonical-uri request-uri host) 0)))
			      (server server)
			      (host host)
			      (request-uri request-uri))
		     (receive (code headers body)
			 (%send-request request server host request-uri has-content? restopts)
		       (cond ((and (string-prefix? "3" code)
				   (assoc "location" headers))
			      => (lambda (loc)
				   (receive (uri server path*)
				       (canonical-uri (cadr loc) server)
				     (when (or (member uri history)
					       (> (length history) 20))
				       (errorf <http-error> "redirection is looping via ~a" uri))
				     (loop (cons uri history)
					   server
					   (server->host server)
					   path*))))
			     (else
			      (values code headers body))))))))))
      '(begin)))			; Do nothing
(define-transitional-patch)

(define (rss->sxml uri tmpbase)
  (define (open-rss-input-file-encoding fname)
    (let* ((in (open-input-file fname))
	   (encoding (match (ssax:read-markup-token in)
		       ('(PI . xml)
			(let1 attrs (call-with-input-string
					(ssax:read-pi-body-as-string in)
				      (cut ssax:read-attributes <> '()))
			  (assq-ref attrs 'encoding 'utf-8)))
		       (else                        'utf-8))))
      (port-seek in 0)
      (wrap-with-input-conversion in encoding)))
  (let*-values (((schema user-info hostname port path query fragment) (uri-parse uri))
		((sink fname) (sys-mkstemp (or tmpbase "/tmp/rss-"))))
    (receive (status header body)
	(unwind-protect
	 (http-get hostname path
		   :sink sink
		   :flusher (lambda (sink _)
			      (flush sink)
			      (open-rss-input-file-encoding fname)))
	 (begin
	   (close-output-port sink)
	   (sys-remove fname)))
      (unwind-protect
       (values status header (ssax:xml->sxml body '()))
       (close-input-port body)))))
