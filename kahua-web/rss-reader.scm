;; -*- coding: euc-jp; mode: scheme -*-
;;
;;  Copyright (c) 2005 Kahua.Org, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: rss-reader.scm,v 1.2 2006/10/25 02:34:18 yasuyuki Exp $

(use rfc.uri)
(use rfc.http)
(use rfc.822)
(use sxml.ssax)
(use sxml.sxpath)
(use util.match)
(use util.list)

(define-plugin "rss-reader"
  (version "0.1")
  (export rss->sxml
	  rss-include
	  )
  (depend #f))

(define (rss->sxml uri)
  (receive (schema user-info hostname port path query fragment)
      (uri-parse uri)
    (receive (status header body)
	(http-get hostname path)
      (call-with-input-string body
	(lambda (in)
	  (let ((sxml-data (ssax:xml->sxml in '())))
	    (values status header sxml-data)))))))

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
