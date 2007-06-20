;; -*- mode: scheme; coding: utf-8 -*-
;;
;;  Copyright (c) 2005-2007 Kahua Project, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: rss-reader.scm,v 1.7 2007/06/20 03:26:13 bizenn Exp $

(use srfi-11)
(use rfc.uri)
(use rfc.http)
(use rfc.822)
(use sxml.ssax)
(use sxml.sxpath)
(use util.match)
(use util.list)
(use gauche.charconv)
(use kahua.util)

(define-plugin "rss-reader"
  (version "0.1")
  (export rss->sxml
	  rss-include
	  )
  (depend #f))

;; Because http-get/http-post of Gauche 0.8.10 cannot handle :sink and :flusher
;; arguments, so we ommit the code below until this problem is fixed.
;;
;; (define (rss->sxml uri tmpbase)
;;   (define (open-rss-input-file-encoding fname)
;;     (let* ((in (open-input-file fname))
;; 	   (encoding (match (ssax:read-markup-token in)
;; 		       ('(PI . xml)
;; 			(let1 attrs (call-with-input-string
;; 					(ssax:read-pi-body-as-string in)
;; 				      (cut ssax:read-attributes <> '()))
;; 			  (assq-ref attrs 'encoding 'utf-8)))
;; 		       (else                        'utf-8))))
;;       (port-seek in 0)
;;       (wrap-with-input-conversion in encoding)))
;;   (let*-values (((schema user-info hostname port path query fragment) (uri-parse uri))
;; 		((sink fname) (sys-mkstemp (or tmpbase "/tmp/rss-"))))
;;     (receive (status header body)
;; 	(unwind-protect
;; 	 (http-get hostname path
;; 		   :sink sink
;; 		   :flusher (lambda (sink _)
;; 			      (flush sink)
;; 			      (open-rss-input-file-encoding fname)))
;; 	 (begin
;; 	   (close-output-port sink)
;; 	   (sys-remove fname)))
;;       (unwind-protect
;;        (values status header (ssax:xml->sxml body '()))
;;        (close-input-port body)))))

;; This is very transitional implementation.
(define (rss->sxml uri _)
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

(define (rss-include uri . kargs)
  (define (default-formatter item)
    (let ((date (car item))
	  (title (cadr item))
	  (link (caddr item)))
      `(div  (a (@ (href ,link)) ,(date->string date "~Y-~m-~d") " " ,title))))

  (let-keywords* kargs ((tmpbase #f)
			(count   #f)
			(formatter default-formatter))
    (define finish?
      (if count
	  (lambda (items cnt) (or (null? items) (>= 0 cnt)))
	  (lambda (items cnt) (null? items))))
    (define dec
      (if count
	  (cut - <> 1)
	  identity))
    (receive (st hd sx) (rss->sxml uri tmpbase)
      (let loop ((items ((sxpath '(rss channel item)) sx))
		 (count count)
		 (accum '()))
	(if (finish? items count)
	    (reverse! accum)
	    (loop (cdr items) (dec count)
		  (cons (let1 els (cdar items)
			  (formatter
			   (list (rfc822-date->date (assq-ref-car els 'pubDate))
				 (assq-ref-car els 'title)
				 (assq-ref-car els 'link)
				 (assq-ref-car els 'description)
				 (assq-ref-car els 'author)
				 (assq-ref-car els 'comments))))
			accum)))))
    ))
