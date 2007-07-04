;; -*- mode: scheme; coding: utf-8 -*-
;;
;;  Copyright (c) 2005-2007 Kahua Project, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: rss-reader.scm,v 1.12 2007/07/04 05:19:51 bizenn Exp $

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
  (version "0.2")
  (export rss->sxml)
  (depend #f))


(define rss->sxml
  (if (version>=? (gauche-version) "0.8.10")
      ;; Use temporary file.
      (lambda (uri tmpbase)
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
      ;; Only on-memory - transitional implementation.
      (lambda (uri _)
	(define (rss-encoding str)
	  (call-with-input-string str
	    (lambda (in)
	      (match (ssax:read-markup-token in)
		('(PI . xml)
		 (let1 attrs (call-with-input-string
				 (ssax:read-pi-body-as-string in)
			       (cut ssax:read-attributes <> '()))
		   (assq-ref attrs 'encoding 'utf-8)))
		(else                        'utf-8)))))
	(let*-values (((schema user-info hostname port path query fragment) (uri-parse uri))
		      ((status header body) (http-get hostname path))
		      ((encoding) (rss-encoding body)))
	  (call-with-input-string body
	    (lambda (in)
	      (let1 in (wrap-with-input-conversion in encoding)
		(values status header (ssax:xml->sxml in '())))))))
      ))
