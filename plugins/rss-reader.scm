;; -*- mode: scheme coding: utf-8 -*-
;;
;;  Copyright (c) 2005-2007 Kahua Project, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: rss-reader.scm,v 1.4 2007/06/06 05:45:12 bizenn Exp $

(use srfi-11)
(use rfc.uri)
(use rfc.http)
(use rfc.822)
(use sxml.ssax)
(use sxml.sxpath)
(use util.match)
(use util.list)
(use gauche.charconv)

(define-plugin "rss-reader"
  (version "0.1")
  (export rss->sxml
	  rss-include
	  )
  (depend #f))

(define (rss->sxml uri . maybe-tmpbase)
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
		((sink fname) (sys-mkstemp (get-optional maybe-tmpbase "/tmp/rss-"))))
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

(define (rss-include uri . count&format)
  (let-optionals* count&format
      ((count #f)
       (format (lambda (item)
		 (let ((date (car item))
		       (title (cadr item))
		       (link (caddr item)))
		   `(div  (a (@ (href ,link)) ,(date->string date "~Y-~m-~d") " " ,title))))))
    (receive (st hd sx)
	(rss->sxml uri)
      (let ((item-list (map
			(lambda (item)
			  (match item
				   (('item ('title t)
					   ('link l)
					   ('pubDate d)
					   ('description c)
					   ('author a)
					   ('comments m))
				    (list (rfc822-date->date d) t l c a m))
				   (('item ('title t)
					   ('link l)
					   ('pubDate d)
					   ('description c)
					   ('author a))
				    (list (rfc822-date->date d) t l c a #f))
				   (('item ('title t)
					   ('link l)
					   ('pubDate d)
					   ('description c))
				    (list (rfc822-date->date d) t l c #f #f))
				   (('item ('title t)
					   ('link l)
					   ('pubDate d))
				    (list (rfc822-date->date d) t l #f #f #f))))
			((sxpath '(rss channel item)) sx))))
	(let ((l (if count
		     (take* item-list count)
		     item-list)))
	  (map format l))))))
